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
        S, OLD_S, T, OLD_T, U,
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
            const rowAcceptSetIDs = NAMES.matcherVar(g, id, 'rowAcceptSetIDs');
            const colAcceptSetIDs = NAMES.matcherVar(g, id, 'colAcceptSetIDs');
            const rowAcceptSets = NAMES.matcherVar(g, id, 'rowAcceptSets');
            const colAcceptSets = NAMES.matcherVar(g, id, 'colAcceptSets');
            const rowsToCols = NAMES.matcherVar(g, id, 'rowsToCols');
            const rowStates = NAMES.matcherVar(g, id, 'rowStates');
            const colStates = NAMES.matcherVar(g, id, 'colStates');
            
            const dfas = makePatternMatcherDFAs(alphabet.key.length, this.matchHandlers.map(h => h.pattern));
            
            const handlersByPattern = new Map<string, MatchHandler[]>();
            for(const handler of this.matchHandlers) {
                const key = PatternTree.key(handler.pattern);
                getOrCompute(handlersByPattern, key, () => []).push(handler);
            }
            
            const maskDiff = (acceptSets: NameExpr, maskSize: number, t1: NameExpr, t2: NameExpr, index: number): Expr => {
                const indexExpr = IR.int(index);
                return OP.bitwiseAnd(
                    access(acceptSets, OP.multAddConstant(t1, maskSize, indexExpr)),
                    OP.bitwiseNot(access(acceptSets, OP.multAddConstant(t2, maskSize, indexExpr))),
                );
            };
            
            const makeStateChangeHandlers = (acceptSets: NameExpr, acceptMap: ReadonlyIDMap<PatternTree>, f: 'add' | 'del'): Stmt[] => {
                const t1 = f === 'add' ? T : OLD_T;
                const t2 = f === 'add' ? OLD_T : T;
                const maskSize = (acceptMap.size() + 31) >> 5;
                
                const out: Stmt[] = [];
                for(let index = 0; index < maskSize; ++index) {
                    const cases: Stmt[] = [];
                    const minAcceptID = index << 5,
                        maxAcceptID = Math.min((index + 1) << 5, acceptMap.size());
                    for(let acceptID = minAcceptID; acceptID < maxAcceptID; ++acceptID) {
                        const key = PatternTree.key(acceptMap.getByID(acceptID));
                        const handlers = handlersByPattern.get(key) ?? fail();
                        cases.push(block(handlers.map(h => this.makeMatchHandler(h, f))));
                    }
                    
                    out.push(
                        assign(U, '=', maskDiff(acceptSets, maskSize, t1, t2, index)),
                        cases.length === 1 ? if_(OP.ne(U, ZERO), cases[0])
                        : while_(OP.ne(U, ZERO), block([
                            // special case, when there is no need for `countTrailingZeros`
                            cases.length === 2 ? switch_(OP.bitwiseAnd(U, ONE), cases.reverse())
                            : switch_(OP.countTrailingZeros(U), cases),
                            assign(U, '&=', OP.minusOne(U)),
                        ])),
                    );
                }
                
                return out;
            };
            
            const makeUpdateSamplers = (acceptSetIDs: Expr, acceptSets: NameExpr, acceptMap: ReadonlyIDMap<PatternTree>): Stmt[] => {
                return acceptMap.size() === 0 ? [] : [
                    declVars([
                        {name: T, type: INT_TYPE, initialiser: access(acceptSetIDs, S)},
                        {name: OLD_T, type: INT_TYPE, initialiser: access(acceptSetIDs, OLD_S)},
                    ]),
                    if_(OP.eq(T, OLD_T), CONTINUE),
                    
                    declVar(U, INT_TYPE, undefined, true),
                    ...makeStateChangeHandlers(acceptSets, acceptMap, 'del'),
                    ...makeStateChangeHandlers(acceptSets, acceptMap, 'add'),
                ];
            };
            
            const rowDFASize = dfas.rowDFA.size(),
                colDFASize = dfas.colDFA.size(),
                numRowPatterns = dfas.rowsAcceptMap.size(),
                numColPatterns = dfas.colsAcceptMap.size();
            
            const arrays: VarDeclWithInitialiser[] = [];
            if(numRowPatterns > 0 || numColPatterns > 0) {
                arrays.push(
                    constArrayDecl(rowDFA, dfas.rowDFA.toFlatArray(), rowDFASize, dfas.rowDFA.alphabetSize),
                    newArrayDecl(rowStates, g.n, rowDFASize),
                );
            }
            if(numRowPatterns > 0) {
                const rowAcceptSetMasks = dfas.rowsAcceptSetMap.flatMap(entry => [...dfas.rowsAcceptMap.getIDSet(entry)]);
                arrays.push(
                    constArrayDecl(rowAcceptSetIDs, dfas.rowsAcceptSetIDs, dfas.rowsAcceptSetMap.size()),
                    constArrayDecl(rowAcceptSets, rowAcceptSetMasks, numRowPatterns <= 16 ? 1 << numRowPatterns : INT32_ARRAY_TYPE.domainSize, (numRowPatterns + 31) >> 5),
                );
            }
            if(numColPatterns > 0) {
                const colAcceptSetMasks = dfas.colsAcceptSetMap.flatMap(entry => [...dfas.colsAcceptMap.getIDSet(entry)]);
                arrays.push(
                    constArrayDecl(rowsToCols, dfas.rowsToCols, dfas.colDFA.alphabetSize),
                    constArrayDecl(colDFA, dfas.colDFA.toFlatArray(), colDFASize, dfas.colDFA.alphabetSize),
                    constArrayDecl(colAcceptSetIDs, dfas.colsAcceptSetIDs, dfas.colsAcceptSetMap.size()),
                    constArrayDecl(colAcceptSets, colAcceptSetMasks, numColPatterns <= 16 ? 1 << numColPatterns : INT32_ARRAY_TYPE.domainSize, (numColPatterns + 31) >> 5),
                    newArrayDecl(colStates, g.n, colDFASize),
                );
            }
            
            const recomputeRowStates = [
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
                        numColPatterns > 0 ? if_(OP.lt(AT_X, START_X), assign(START_X, '=', AT_X)) : PASS,
                        
                        // must occur last, since it uses `continue`
                        ...makeUpdateSamplers(rowAcceptSetIDs, rowAcceptSets, dfas.rowsAcceptMap),
                    ]),
                ]),
            ];
            const recomputeColStates = numColPatterns === 0 ? [] : [
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
                        ...makeUpdateSamplers(colAcceptSetIDs, colAcceptSets, dfas.colsAcceptMap),
                    ]),
                ]),
            ];
            
            return [
                declVars(arrays),
                declFunc(
                    updateFuncName,
                    undefined,
                    [START_X, START_Y, EFFECTIVE_WIDTH, EFFECTIVE_HEIGHT],
                    [INT_TYPE, INT_TYPE, INT_TYPE, INT_TYPE],
                    VOID_TYPE,
                    numRowPatterns === 0 && numColPatterns === 0 ? PASS : block([
                        declVars([
                            {name: END_X, type: INT_TYPE, initialiser: OP.add(START_X, EFFECTIVE_WIDTH)},
                            {name: END_Y, type: INT_TYPE, initialiser: OP.add(START_Y, EFFECTIVE_HEIGHT)},
                        ]),
                        ...recomputeRowStates,
                        ...recomputeColStates,
                    ]),
                ),
                localCallStmt(updateFuncName, [ZERO, ZERO, g.width, g.height]),
                BLANK_LINE,
            ];
        }
    }
    
    type MatcherDFAs = Readonly<{
        rowDFA: DFA<PatternTree>,
        rowsAcceptMap: ReadonlyIDMap<PatternTree>,
        rowsAcceptSetIDs: readonly number[],
        rowsAcceptSetMap: ReadonlyIDMap<readonly PatternTree[]>,
        rowsToCols: readonly number[],
        colDFA: DFA<PatternTree>,
        colsAcceptMap: ReadonlyIDMap<PatternTree>,
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
    function makePatternMatcherDFAs(alphabetSize: number, allPatterns: readonly PatternTree[]): MatcherDFAs {
        const rowPatterns = allPatterns.filter(p => p.height === 1);
        const colPatterns = allPatterns.filter(p => p.height > 1);
        
        const rowsAcceptMap = IDMap.ofWithKey(rowPatterns, PatternTree.key);
        const rowsKeepMap = IDMap.withKey(Pattern.key);
        const rowsAcceptOrKeepMap = IDMap.ofWithKey(rowPatterns, PatternTree.key);
        const colsAcceptMap = IDMap.ofWithKey(colPatterns, PatternTree.key);
        
        const allLeaves = IDMap.ofWithKey(allPatterns.flatMap(PatternTree.getLeaves), PatternTree.key);
        const allLeafRows = IDMap.withKey(Pattern.key);
        const leafRowMap: number[][] = [];
        
        allLeaves.forEach(pattern => {
            const rows = Pattern.rowsOf(pattern);
            // determine which rows to keep when mapping from rowDFA to colDFA
            if(rows.length > 1) {
                rowsAcceptOrKeepMap.addAll(rows);
                rowsKeepMap.addAll(rows);
            }
            // associate this pattern with the IDs of its rows in `allLeafRows`
            leafRowMap.push(rows.map(row => allLeafRows.getOrCreateID(row)));
        });
        
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
                return rowsAcceptOrKeepMap.filter(pattern => PatternTree.matches(pattern, p => acceptSet.has(Pattern.key(p))));
            })
            .minimise(PatternTree.key);
        
        const [rowsAcceptSetIDs, rowsAcceptSetMap] = rowDFA.getAcceptSetMap(rowsAcceptOrKeepMap, row => rowsAcceptMap.has(row));
        
        // reduce alphabet size of colDFA by not distinguishing rows which aren't part of taller patterns
        const [rowsToCols, rowsToColsMap] = rowDFA.getAcceptSetMap(
            rowsAcceptOrKeepMap,
            (row: PatternTree): row is PatternTree.LeafOrTop => rowsKeepMap.has(row as PatternTree.LeafOrTop),
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
                return allPatterns.filter(pattern => PatternTree.matches(pattern, p => acceptSet.has(Pattern.key(p))));
            })
            .minimise(PatternTree.key);
        
        const [colsAcceptSetIDs, colsAcceptSetMap] = colDFA.getAcceptSetMap(colsAcceptMap);
        
        return {
            rowDFA,
            rowsAcceptMap,
            rowsAcceptSetIDs,
            rowsAcceptSetMap,
            rowsToCols,
            colDFA,
            colsAcceptMap,
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
