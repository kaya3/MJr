///<reference path="names.ts"/>

namespace IR {
    // TODO: more kinds of match handler (e.g. conditional on a boolean expression)
    export type MatchHandler = Readonly<
        | {kind: 'sampler', pattern: PatternTree, sampler: AbstractSampler, i: number}
        | {kind: 'counter', pattern: PatternTree, counter: NameExpr}
        | {kind: 'convolution', pattern: Pattern, buffer: ConvBuffer, i: number}
    >
    
    const {
        START_X, START_Y, END_X, END_Y, EFFECTIVE_WIDTH, EFFECTIVE_HEIGHT,
        AT, AT_X, AT_Y,
        S, OLD_S, T, OLD_T,
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
            const rowsToCols = NAMES.matcherVar(g, id, 'rowsToCols');
            const rowStates = NAMES.matcherVar(g, id, 'rowStates');
            const colStates = NAMES.matcherVar(g, id, 'colStates');
            
            const patternMap = IDMap.ofWithKey(this.matchHandlers.map(h => h.pattern), PatternTree.key);
            const dfas = makePatternMatcherDFAs(alphabet.key.length, patternMap);
            
            const handlersByPattern = new Map<string, MatchHandler[]>();
            for(const handler of this.matchHandlers) {
                const key = PatternTree.key(handler.pattern);
                getOrCompute(handlersByPattern, key, () => []).push(handler);
            }
            
            // would be possible to compute a table of deltas for each pair of states, but this results in quadratic size code output
            const makeStateChangeHandlers = (acceptSetMap: ReadonlyIDMap<readonly PatternTree[]>, f: 'add' | 'del'): Stmt[] => acceptSetMap.map(accepts => {
                const out: Stmt[] = [];
                for(const pattern of accepts) {
                    const handlers = handlersByPattern.get(PatternTree.key(pattern));
                    if(handlers !== undefined) {
                        for(const h of handlers) {
                            out.push(this.makeMatchHandler(h, f));
                        }
                    }
                }
                return block(out);
            });
            const makeUpdateSamplers = (acceptSets: Expr, acceptSetMap: ReadonlyIDMap<readonly PatternTree[]>): Stmt[] => acceptSetMap.size() > 1 ? [
                declVars([
                    {name: T, type: INT_TYPE, initialiser: access(acceptSets, S)},
                    {name: OLD_T, type: INT_TYPE, initialiser: access(acceptSets, OLD_S)},
                ]),
                if_(OP.eq(T, OLD_T), CONTINUE),
                
                switch_(OLD_T, makeStateChangeHandlers(acceptSetMap, 'del')),
                switch_(T, makeStateChangeHandlers(acceptSetMap, 'add')),
            ] : [];
            
            const rowDFASize = dfas.rowDFA.size(),
                colDFASize = dfas.colDFA.size();
            
            return [
                declVars([
                    constArrayDecl(rowDFA, dfas.rowDFA.toFlatArray(), rowDFASize, dfas.rowDFA.alphabetSize),
                    constArrayDecl(rowAcceptSets, dfas.rowsAcceptSetIDs, dfas.rowsAcceptSetMap.size()),
                    constArrayDecl(rowsToCols, dfas.rowsToCols, dfas.colDFA.alphabetSize),
                    constArrayDecl(colDFA, dfas.colDFA.toFlatArray(), colDFASize, dfas.colDFA.alphabetSize),
                    constArrayDecl(colAcceptSets, dfas.colsAcceptSetIDs, dfas.colsAcceptSetMap.size()),
                    newArrayDecl(rowStates, g.n, rowDFASize),
                    newArrayDecl(colStates, g.n, colDFASize),
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
                                    OP.multAddConstant(S, dfas.rowDFA.alphabetSize, g.access(AT))
                                )),
                                if_(OP.eq(S, OLD_S), if_(OP.lt(AT_X, START_X), BREAK, CONTINUE)),
                                assign(access(rowStates, AT), '=', S),
                                if_(OP.lt(AT_X, START_X), assign(START_X, '=', AT_X)),
                                
                                // must occur last, since it uses `continue`
                                ...makeUpdateSamplers(rowAcceptSets, dfas.rowsAcceptSetMap),
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
                                    OP.multAddConstant(S, dfas.colDFA.alphabetSize, access(rowsToCols, access(rowStates, AT))),
                                )),
                                if_(OP.eq(S, OLD_S), if_(OP.lt(AT_Y, START_Y), BREAK, CONTINUE)),
                                
                                assign(access(colStates, AT), '=', S),
                                
                                // must occur last, since it uses `continue`
                                ...makeUpdateSamplers(colAcceptSets, dfas.colsAcceptSetMap),
                            ]),
                        ]),
                    ]),
                ),
                localCallStmt(updateFuncName, [ZERO, ZERO, g.width, g.height]),
                BLANK_LINE,
            ];
        }
    }
    
    type MatcherDFAs = Readonly<{
        rowDFA: DFA<PatternTree>,
        rowsAcceptSetIDs: readonly number[],
        rowsAcceptSetMap: ReadonlyIDMap<readonly PatternTree[]>,
        rowsToCols: readonly number[],
        colDFA: DFA<PatternTree>,
        colsAcceptSetIDs: readonly number[],
        colsAcceptSetMap: ReadonlyIDMap<readonly PatternTree[]>,
    }>
    
    /**
     * Builds a pair of DFAs which can be used to match 2D patterns. The `rowDFA`
     * recognises pattern rows, and the `colDFA` recognises sequences of pattern
     * rows matched by the `rowDFA`.
     * 
     * The DFAs recognise the patterns in reverse order, for convenience so that
     * matches are reported where the patterns start rather than where they end.
     */
    function makePatternMatcherDFAs(alphabetSize: number, patterns: ReadonlyIDMap<PatternTree>): MatcherDFAs {
        const allLeaves = IDMap.ofWithKey(patterns.flatMap(PatternTree.getLeaves), PatternTree.key);
        const allLeafRows = IDMap.withKey(Pattern.key);
        const leafRowMap = allLeaves.map(pattern => Pattern.rowsOf(pattern).map(row => allLeafRows.getOrCreateID(row)));
        const rowAcceptMap = IDMap.withKey(PatternTree.key);
        
        // the IDs of the leaf rows which must be distinguished by `colDFA`
        const keepRows = ISet.empty(allLeafRows.size());
        
        for(const rowIDs of leafRowMap) {
            if(rowIDs.length === 1) { continue; }
            for(const rowID of rowIDs) {
                ISet.add(keepRows, rowID);
                rowAcceptMap.getOrCreateID(allLeafRows.getByID(rowID));
            }
        }
        // single-row patterns will be recognised by the row DFA
        rowAcceptMap.addAll(patterns.filter(p => p.height === 1));
        
        const rowDFA = Regex.compile(
                alphabetSize,
                _makeRegex(allLeafRows.map(pattern => ({
                    pattern,
                    seq: pattern.kind === 'top'
                        ? emptyArray(pattern.width, Regex.WILDCARD)
                        : pattern.masks.map(ISet.toArray).map(Regex.letters),
                }))),
            )
            .map(literalRows => {
                const acceptSet = new Set(literalRows.map(Pattern.key));
                return rowAcceptMap.filter(pattern => PatternTree.matches(pattern, p => acceptSet.has(Pattern.key(p))));
            })
            .minimise(PatternTree.key);
        
        const [rowsAcceptSetIDs, rowsAcceptSetMap] = rowDFA.getAcceptSetMap(rowAcceptMap, row => patterns.has(row));
        
        // reduce alphabet size of colDFA by not distinguishing rows which aren't part of taller patterns
        const [rowsToCols, rowsToColsMap] = rowDFA.getAcceptSetMap(
            rowAcceptMap,
            (row: PatternTree): row is PatternTree.LeafOrTop => (row.kind === 'leaf' || row.kind === 'top') && ISet.has(keepRows, allLeafRows.getID(row)),
        );
        
        const acceptingSetIDs: number[][] = makeArray(allLeafRows.size(), () => []);
        rowsToColsMap.forEach((rowSet, rowSetID) => {
            for(const row of rowSet) {
                const rowID = allLeafRows.getID(row);
                acceptingSetIDs[rowID].push(rowSetID);
            }
        });
        const colRegexLetters = acceptingSetIDs.map(Regex.letters);
        
        const colRegexPatterns: PatternSeq[] = [];
        allLeaves.forEach((pattern, i) => {
            // patterns with only one row will be matched by the rowDFA directly
            if(pattern.height === 1) { return; }
            colRegexPatterns.push({
                pattern,
                seq: leafRowMap[i].map(rowID => colRegexLetters[rowID]),
            });
        });
        const colDFA = Regex.compile(
                rowsToColsMap.size(),
                _makeRegex(colRegexPatterns),
            )
            .map(literals => {
                const acceptSet = new Set(literals.map(Pattern.key));
                return patterns.filter(pattern => PatternTree.matches(pattern, p => acceptSet.has(Pattern.key(p))));
            })
            .minimise(PatternTree.key);
        
        const [colsAcceptSetIDs, colsAcceptSetMap] = colDFA.getAcceptSetMap(patterns);
        
        return {
            rowDFA,
            rowsAcceptSetIDs,
            rowsAcceptSetMap,
            rowsToCols,
            colDFA,
            colsAcceptSetIDs,
            colsAcceptSetMap,
        };
    }
    
    type PatternSeq = Readonly<{pattern: PatternTree.LeafOrTop, seq: readonly Regex.Node<PatternTree.LeafOrTop>[]}>
    function _makeRegex(patterns: PatternSeq[]): Regex.Node<PatternTree.LeafOrTop> {
        return Regex.concat([
            Regex.DOT_STAR,
            Regex.union(
                patterns.map(p => Regex.concat([
                    Regex.accept(p.pattern),
                    ...p.seq,
                ].reverse()))
            ),
        ]);
    }
}
