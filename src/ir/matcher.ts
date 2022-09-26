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
            
            const handlersByPattern = makeArray<MatchHandler[]>(patternMap.size(), () => []);
            for(const handler of this.matchHandlers) {
                const patternID = patternMap.getID(handler.pattern);
                handlersByPattern[patternID].push(handler);
            }
            
            // would be possible to compute a table of deltas for each pair of states, but this results in quadratic size code output
            const makeStateChangeHandlers = (f: 'add' | 'del'): Stmt[] => {
                return _colDFA.acceptSetMap.map(patternIDs => {
                    const out: Stmt[] = [];
                    for(const patternID of patternIDs) {
                        for(const h of handlersByPattern[patternID]) {
                            out.push(this.makeMatchHandler(h, f));
                        }
                    }
                    return block(out);
                });
            };
            
            const rowDFAType = constArrayType(_rowDFA.size()),
                rowAcceptSetsType = constArrayType(_rowDFA.acceptSetMap.size()),
                colDFAType = constArrayType(_colDFA.size()),
                colAcceptSetsType = constArrayType(_colDFA.acceptSetMap.size()),
                rowStatesType = mutableArrayType(rowDFAType.domainSize),
                colStatesType = mutableArrayType(colDFAType.domainSize);
            
            return [
                declVars([
                    {name: rowDFA, type: rowDFAType, initialiser: constArray(_rowDFA.toFlatArray(), rowDFAType.domainSize, _rowDFA.alphabetSize)},
                    {name: rowAcceptSets, type: rowAcceptSetsType, initialiser: constArray(_rowDFA.getAcceptSetIDs(), rowAcceptSetsType.domainSize)},
                    {name: colDFA, type: colDFAType, initialiser: constArray(_colDFA.toFlatArray(), colDFAType.domainSize, _colDFA.alphabetSize)},
                    {name: colAcceptSets, type: colAcceptSetsType, initialiser: constArray(_colDFA.getAcceptSetIDs(), colAcceptSetsType.domainSize)},
                    {name: rowStates, type: rowStatesType, initialiser: newArray(g.n, rowDFAType.domainSize)},
                    {name: colStates, type: colStatesType, initialiser: newArray(g.n, colDFAType.domainSize)},
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
                                        switch_(access(colAcceptSets, OLD_S), makeStateChangeHandlers('del')),
                                        switch_(access(colAcceptSets, S), makeStateChangeHandlers('add')),
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
    function makePatternMatcherDFAs(alphabetSize: number, patterns: ReadonlyIDMap<Pattern>): [DFA, DFA] {
        const numPatterns = patterns.size();
        
        const rowPatterns = IDMap.ofWithKey(patterns.map(Pattern.rowsOf).flat(), Pattern.key);
        const rowRegex = _makeRegex(rowPatterns.map(row => row.masks.map(ISet.toArray)));
        const rowDFA = Regex.compile(alphabetSize, rowPatterns.size(), rowRegex);
        
        const acceptingSets: number[][] = makeArray(rowPatterns.size(), () => []);
        rowDFA.acceptSetMap.forEach((xs, id) => {
            for(const x of xs) {
                acceptingSets[x].push(id);
            }
        });
        
        const colRegex = _makeRegex(patterns.map(pattern => Pattern.rowsOf(pattern).map(row => acceptingSets[rowPatterns.getID(row)])));
        const colDFA = Regex.compile(rowDFA.acceptSetMap.size(), numPatterns, colRegex);
        
        return [rowDFA, colDFA];
    }
    
    function _makeRegex(sequences: readonly number[][][]): Regex.Node {
        return Regex.concat([
            Regex.kleeneStar(Regex.WILDCARD),
            Regex.union(
                sequences.map((seq, seqID) => Regex.concat([
                    Regex.concat(seq.map(Regex.letters).reverse()),
                    Regex.accept(seqID),
                ]))
            ),
        ]);
    }
}
