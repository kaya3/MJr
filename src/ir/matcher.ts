///<reference path="names.ts"/>

namespace IR {
    // TODO: more kinds of match handler (e.g. conditional on another pattern not being
    // present, conditional on a boolean expression)
    export type MatchHandler = Readonly<
        | {kind: 'sampler', pattern: Pattern, sampler: AbstractSampler, i: number}
        | {kind: 'counter', pattern: Pattern, counter: NameExpr}
        | {kind: 'convolution', pattern: Pattern, buffer: ConvBuffer, i: number}
    >
    
    const {
        START_X, START_Y, END_X, END_Y, EFFECTIVE_WIDTH, EFFECTIVE_HEIGHT,
        S, OLD_S, AT, AT_X, AT_Y,
    } = NAMES;
    
    export class Matcher {
        private readonly matchHandlers: MatchHandler[] = [];
        
        public readonly updateFuncName: NameExpr;
        
        public constructor(
            public readonly g: Grid,
            private readonly id: number,
        ) {
            this.updateFuncName = NAMES.matcherVar(g, id, 'update');
        }
        
        public addMatchHandler(handler: MatchHandler): void {
            this.matchHandlers.push(handler);
        }
        
        private makeMatchHandler(h: MatchHandler, f: 'add' | 'del'): Stmt {
            switch(h.kind) {
                case 'sampler':
                    return h.sampler.handleMatch(f, h.i);
                
                case 'counter':
                    return assign(h.counter, f === 'add' ? '+=' : '-=', ONE);
                
                case 'convolution':
                    return h.buffer.update(h.i, AT_X, AT_Y, f === 'add' ? '+=' : '-=');
            }
        }
        
        public declareUpdateFunc(): Stmt[] {
            const {g, id, updateFuncName} = this;
            const {alphabet} = g.grid;
            
            const rowDFA = NAMES.matcherVar(g, id, 'rowDFA');
            const colDFA = NAMES.matcherVar(g, id, 'colDFA');
            const rowAcceptSets = NAMES.matcherVar(g, id, 'rowAcceptSets');
            const colAcceptSets = NAMES.matcherVar(g, id, 'colAcceptSets');
            const rowStates = NAMES.matcherVar(g, id, 'rowStates');
            const colStates = NAMES.matcherVar(g, id, 'colStates');
            
            const patternMap = IDMap.ofWithKey(this.matchHandlers.map(h => h.pattern), Pattern.key);
            const [_rowDFA, _colDFA] = makePatternMatcherDFAs(alphabet.key.length, patternMap);
            
            const handlersByPattern = new Map<string, MatchHandler[]>();
            for(const handler of this.matchHandlers) {
                const key = Pattern.key(handler.pattern);
                getOrCompute(handlersByPattern, key, () => []).push(handler);
            }
            
            // would be possible to compute a table of deltas for each pair of states, but this results in quadratic size code output
            const makeStateChangeHandlers = (dfa: DFA<Pattern>, f: 'add' | 'del'): Stmt[] => {
                return dfa.acceptSetMap.map(accepts => {
                    const out: Stmt[] = [];
                    for(const pattern of accepts) {
                        const handlers = handlersByPattern.get(Pattern.key(pattern));
                        if(handlers !== undefined) {
                            for(const h of handlers) {
                                out.push(this.makeMatchHandler(h, f));
                            }
                        }
                    }
                    return block(out);
                });
            };
            
            const rowDFASize = _rowDFA.size(),
                colDFASize = _colDFA.size(),
                rowAcceptSetsCount = _rowDFA.acceptSetMap.size(),
                colAcceptSetsCount = _colDFA.acceptSetMap.size();
            
            return [
                declVars([
                    {name: rowDFA, type: constArrayType(rowDFASize), initialiser: constArray(_rowDFA.toFlatArray(), rowDFASize, _rowDFA.alphabetSize)},
                    {name: rowAcceptSets, type: constArrayType(rowAcceptSetsCount), initialiser: constArray(_rowDFA.getAcceptSetIDs(), rowAcceptSetsCount)},
                    {name: colDFA, type: constArrayType(colDFASize), initialiser: constArray(_colDFA.toFlatArray(), colDFASize, _colDFA.alphabetSize)},
                    {name: colAcceptSets, type: constArrayType(colAcceptSetsCount), initialiser: constArray(_colDFA.getAcceptSetIDs(), colAcceptSetsCount)},
                    {name: rowStates, type: mutableArrayType(rowDFASize), initialiser: newArray(g.n, rowDFASize)},
                    {name: colStates, type: mutableArrayType(colDFASize), initialiser: newArray(g.n, colDFASize)},
                ]),
                declFunc(
                    updateFuncName,
                    undefined,
                    [START_X, START_Y, EFFECTIVE_WIDTH, EFFECTIVE_HEIGHT],
                    [INT_TYPE, INT_TYPE, INT_TYPE, INT_TYPE],
                    VOID_TYPE,
                    block([
                        declVars([
                            {name: END_X, type: INT_TYPE, initialiser: OP.add(START_X, EFFECTIVE_WIDTH)},
                            {name: END_Y, type: INT_TYPE, initialiser: OP.add(START_Y, EFFECTIVE_HEIGHT)},
                        ]),
                        BLANK_LINE,
                        
                        comment('recompute row states'),
                        forRange(AT_Y, START_Y, END_Y, [
                            declVar(S, INT_TYPE, ternary(
                                OP.lt(END_X, g.width),
                                access(rowStates, g.index(END_X, AT_Y)),
                                ZERO,
                            ), true),
                            forRangeReverse(AT_X, ZERO, END_X, [
                                g.declareAtXY(AT_X, AT_Y),
                                declVar(OLD_S, INT_TYPE, access(rowStates, AT)),
                                assign(S, '=', access(
                                    rowDFA,
                                    OP.multAddConstant(S, _rowDFA.alphabetSize, g.access(AT))
                                )),
                                if_(
                                    OP.ne(S, OLD_S),
                                    block([
                                        assign(access(rowStates, AT), '=', S),
                                        
                                        // update samplers
                                        switch_(access(rowAcceptSets, OLD_S), makeStateChangeHandlers(_rowDFA, 'del')),
                                        switch_(access(rowAcceptSets, S), makeStateChangeHandlers(_rowDFA, 'add')),
                                        
                                        if_(OP.lt(AT_X, START_X), assign(START_X, '=', AT_X)),
                                    ]),
                                    if_(OP.lt(AT_X, START_X), BREAK),
                                ),
                            ]),
                        ]),
                        BLANK_LINE,
                        
                        comment('recompute col states'),
                        forRange(AT_X, START_X, END_X, [
                            declVar(S, INT_TYPE, ternary(
                                OP.lt(END_Y, g.height),
                                access(colStates, g.index(AT_X, END_Y)),
                                ZERO,
                            ), true),
                            forRangeReverse(AT_Y, ZERO, END_Y, [
                                g.declareAtXY(AT_X, AT_Y),
                                declVar(OLD_S, INT_TYPE, access(colStates, AT)),
                                assign(S, '=', access(
                                    colDFA,
                                    OP.multAddConstant(S, _colDFA.alphabetSize, access(rowAcceptSets, access(rowStates, AT))),
                                )),
                                if_(
                                    OP.ne(S, OLD_S),
                                    block([
                                        assign(access(colStates, AT), '=', S),
                                        
                                        // update samplers
                                        switch_(access(colAcceptSets, OLD_S), makeStateChangeHandlers(_colDFA, 'del')),
                                        switch_(access(colAcceptSets, S), makeStateChangeHandlers(_colDFA, 'add')),
                                    ]),
                                    if_(OP.lt(AT_Y, START_Y), BREAK),
                                ),
                            ]),
                        ]),
                    ]),
                ),
                localCallStmt(updateFuncName, [ZERO, ZERO, g.width, g.height]),
                BLANK_LINE,
            ];
        }
    }
    
    /**
     * Builds a pair of DFAs which can be used to match 2D patterns. The `rowDFA`
     * recognises pattern rows, and the `colDFA` recognises sequences of pattern
     * rows matched by the `rowDFA`.
     * 
     * The DFAs recognise the patterns in reverse order, for convenience so that
     * matches are reported where the patterns start rather than where they end.
     */
    function makePatternMatcherDFAs(alphabetSize: number, patterns: ReadonlyIDMap<Pattern>): [DFA<Pattern>, DFA<Pattern>] {
        const rowPatterns = IDMap.ofWithKey(patterns.map(Pattern.rowsOf).flat(), Pattern.key);
        const rowRegex = _makeRegex(rowPatterns.map(pattern => ({
            pattern,
            seq: pattern.masks.map(ISet.toArray),
        })));
        const rowDFA = Regex.compile(alphabetSize, rowRegex, Pattern.key);
        
        const acceptingSets: number[][] = makeArray(rowPatterns.size(), () => []);
        rowDFA.acceptSetMap.forEach((patterns, id) => {
            for(const pattern of patterns) {
                const patternID = rowPatterns.getID(pattern);
                acceptingSets[patternID].push(id);
            }
        });
        
        // patterns with only one row can be matched by the rowDFA directly
        const colRegex = _makeRegex(patterns.filter(
            pattern => pattern.height > 1
        ).map(pattern => ({
            pattern,
            seq: Pattern.rowsOf(pattern).map(row => acceptingSets[rowPatterns.getID(row)]),
        })));
        const colDFA = Regex.compile(rowDFA.acceptSetMap.size(), colRegex, Pattern.key);
        
        return [rowDFA, colDFA];
    }
    
    function _makeRegex(patterns: {pattern: Pattern, seq: readonly number[][]}[]): Regex.Node<Pattern> {
        return Regex.concat([
            Regex.DOT_STAR,
            Regex.union(
                patterns.map(p => Regex.concat([
                    ...p.seq.map(Regex.letters).reverse(),
                    Regex.accept(p.pattern),
                ]))
            ),
        ]);
    }
}
