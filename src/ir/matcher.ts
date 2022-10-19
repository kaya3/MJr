///<reference path="names.ts"/>

namespace IR {
    // TODO: more kinds of match handler (e.g. conditional on a boolean expression)
    export type MatchHandler = Readonly<
        | {kind: 'sampler', pattern: PatternTree, sampler: AbstractSampler, i: number}
        | {kind: 'counter', pattern: PatternTree, counter: NameExpr, weight: Expr}
        | {kind: 'convolution', pattern: Pattern, buffer: ConvBuffer, i: number}
    >
    
    const {
        START_X, START_Y, END_X, END_Y, EFFECTIVE_WIDTH, EFFECTIVE_HEIGHT,
        AT, AT_X, AT_Y,
        S, OLD_S, T, OLD_T, U, I,
    } = NAMES;
    
    export class Matcher {
        private readonly matchHandlers: MatchHandler[] = [];
        private readonly updateFuncName: NameExpr;
        
        public constructor(
            public readonly g: Grid,
            private readonly id: number,
        ) {
            this.updateFuncName = NAMES.matcherVar(g, id, 'update');
        }
        
        public addMatchHandler(handler: MatchHandler): void {
            this.matchHandlers.push(handler);
        }
        
        private makeMatchHandler(h: MatchHandler, acceptID: number, f: 'add' | 'del'): Stmt {
            switch(h.kind) {
                case 'sampler':
                    // `I - constant` allows code to be shared among consecutive match handlers
                    return h.sampler.handleMatch(f, OP.addConstant(I, h.i - acceptID));
                
                case 'counter':
                    return assign(h.counter, f === 'add' ? '+=' : '-=', h.weight);
                
                case 'convolution':
                    return h.buffer.update(h.i, AT_X, AT_Y, f === 'add' ? '+=' : '-=');
            }
        }
        
        public update(x: Expr, y: Expr, w: Expr, h: Expr): Stmt {
            return localCallStmt(this.updateFuncName, [x, y, w, h]);
        }
        
        public declareUpdateFunc(): Stmt[] {
            const {g, id} = this;
            
            const gridAlphabetSize = g.grid.alphabet.key.length;
            const dfas = makePatternMatcherDFAs(gridAlphabetSize, this.matchHandlers.map(h => h.pattern));
            const colsAlphabetSize = dfas.colDFA.alphabetSize,
                rowDFASize = dfas.rowDFA.size(),
                colDFASize = dfas.colDFA.size(),
                numRowPatterns = dfas.rowsAcceptMap.size(),
                numColPatterns = dfas.colsAcceptMap.size(),
                rowAcceptSetMasks = dfas.rowsAcceptSetMap.flatMap(entry => [...dfas.rowsAcceptMap.getIDSet(entry)]),
                colAcceptSetMasks = dfas.colsAcceptSetMap.flatMap(entry => [...dfas.colsAcceptMap.getIDSet(entry)]);
            
            const rowDFA = makeConstArray2D(NAMES.matcherVar(g, id, 'rowDFA'), dfas.rowDFA.toFlatArray(), gridAlphabetSize, rowDFASize);
            const rowAcceptSetIDs = makeConstArray(NAMES.matcherVar(g, id, 'rowAcceptSetIDs'), dfas.rowsAcceptSetIDs, dfas.rowsAcceptSetMap.size());
            const rowAcceptSets = makeConstArray2D(NAMES.matcherVar(g, id, 'rowAcceptSets'), rowAcceptSetMasks, (numRowPatterns + 31) >> 5, numRowPatterns <= 31 ? 1 << numRowPatterns : INT32_ARRAY_TYPE.domainSize);
            const rowsToCols = makeConstArray(NAMES.matcherVar(g, id, 'rowsToCols'), dfas.rowsToCols, colsAlphabetSize);
            const colDFA = makeConstArray2D(NAMES.matcherVar(g, id, 'colDFA'), dfas.colDFA.toFlatArray(), colsAlphabetSize, colDFASize);
            const colAcceptSetIDs = makeConstArray(NAMES.matcherVar(g, id, 'colAcceptSetIDs'), dfas.colsAcceptSetIDs, dfas.colsAcceptSetMap.size());
            const colAcceptSets = makeConstArray2D(NAMES.matcherVar(g, id, 'colAcceptSets'), colAcceptSetMasks, (numColPatterns + 31) >> 5, numColPatterns <= 31 ? 1 << numColPatterns : INT32_ARRAY_TYPE.domainSize);
            const rowStates = makeMutableArray(NAMES.matcherVar(g, id, 'rowStates'), g.n, rowDFASize);
            const colStates = makeMutableArray(NAMES.matcherVar(g, id, 'colStates'), g.n, colDFASize);
            
            const handlersByPattern = new Map<string, MatchHandler[]>();
            for(const handler of this.matchHandlers) {
                const key = PatternTree.key(handler.pattern);
                getOrCompute(handlersByPattern, key, () => []).push(handler);
            }
            
            const maskDiff = (acceptSets: ConstArray2D, t1: Expr, t2: Expr, index: number): Expr => {
                const indexExpr = int(index);
                return OP.bitwiseAnd(
                    acceptSets.get(t1, indexExpr),
                    OP.bitwiseNot(acceptSets.get(t2, indexExpr)),
                );
            };
            
            const makeStateChangeHandlers = (t1: Expr, t2: Expr, acceptSets: ConstArray2D, acceptMap: ReadonlyIDMap<PatternTree>, f: 'add' | 'del'): Stmt[] => {
                const maskSize = (acceptMap.size() + 31) >> 5;
                
                const out: Stmt[] = [];
                for(let index = 0; index < maskSize; ++index) {
                    const cases: Stmt[] = [];
                    const minAcceptID = index << 5,
                        maxAcceptID = Math.min(minAcceptID + 32, acceptMap.size());
                    for(let acceptID = minAcceptID; acceptID < maxAcceptID; ++acceptID) {
                        const key = PatternTree.key(acceptMap.getByID(acceptID));
                        const handlers = handlersByPattern.get(key) ?? fail();
                        cases.push(block(handlers.map(h => this.makeMatchHandler(h, acceptID & 31, f))));
                    }
                    
                    out.push(
                        assign(U, '=', maskDiff(acceptSets, t1, t2, index)),
                        // special cases for short switches, when there is no need for `countTrailingZeros`
                        cases.length === 1 ? if_(OP.ne(U, ZERO), replace(cases[0], I, ZERO))
                        : cases.length === 2 ? block(cases.map((c, i) => if_(OP.ne(OP.bitwiseAnd(U, int(1 << i)), ZERO), replace(c, I, int(i)))))
                        : while_(OP.ne(U, ZERO), block([
                            declVar(I, INT_TYPE, OP.countTrailingZeros(U)),
                            switch_(I, cases, true),
                            assign(U, '&=', OP.minus(U, ONE)),
                        ])),
                    );
                }
                
                return out;
            };
            
            const makeUpdateSamplers = (acceptSetIDs: ConstArray, acceptSets: ConstArray2D, acceptMap: ReadonlyIDMap<PatternTree>): Stmt[] => {
                let t = acceptSetIDs.get(S), oldT = acceptSetIDs.get(OLD_S);
                if(isInt(t) && isInt(oldT)) {
                    return t.value === oldT.value ? [] : fail();
                }
                
                const out: Stmt[] = [];
                if(t !== S || oldT !== OLD_S) {
                    out.push(
                        declVars([
                            {name: T, type: INT_TYPE, initialiser: t},
                            {name: OLD_T, type: INT_TYPE, initialiser: oldT},
                        ]),
                        if_(OP.eq(T, OLD_T), CONTINUE),
                    );
                    t = T;
                    oldT = OLD_T;
                }
                
                out.push(
                    declVar(U, INT_TYPE, undefined, true),
                    ...makeStateChangeHandlers(oldT, t, acceptSets, acceptMap, 'del'),
                    ...makeStateChangeHandlers(t, oldT, acceptSets, acceptMap, 'add'),
                );
                return out;
            };
            
            const recomputeRowStates = [
                BLANK_LINE,
                comment('recompute row states'),
                forRange(AT_Y, START_Y, END_Y, [
                    declVar(S, INT_TYPE, ternary(
                        OP.lt(END_X, g.width),
                        rowStates.get(g.index(END_X, AT_Y)),
                        ZERO,
                    ), true),
                    forRangeReverse(AT_X, ZERO, END_X, [
                        g.declareAtXY(AT_X, AT_Y),
                        declVar(OLD_S, INT_TYPE, rowStates.get(AT)),
                        assign(S, '=', rowDFA.get(S, g.data.get(AT))),
                        if_(OP.eq(S, OLD_S), if_(OP.lt(AT_X, START_X), BREAK, CONTINUE)),
                        
                        rowStates.set(AT, '=', S),
                        numColPatterns === 0 ? PASS : if_(OP.lt(AT_X, START_X), assign(START_X, '=', AT_X)),
                        
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
                        colStates.get(g.index(AT_X, END_Y)),
                        ZERO,
                    ), true),
                    forRangeReverse(AT_Y, ZERO, END_Y, [
                        g.declareAtXY(AT_X, AT_Y),
                        declVar(OLD_S, INT_TYPE, colStates.get(AT)),
                        assign(S, '=', colDFA.get(S, rowsToCols.get(rowStates.get(AT)))),
                        if_(OP.eq(S, OLD_S), if_(OP.lt(AT_Y, START_Y), BREAK, CONTINUE)),
                        
                        colStates.set(AT, '=', S),
                        
                        // must occur last, since it uses `continue`
                        ...makeUpdateSamplers(colAcceptSetIDs, colAcceptSets, dfas.colsAcceptMap),
                    ]),
                ]),
            ];
            
            return [
                declVars([
                    rowDFA,
                    rowAcceptSetIDs,
                    rowAcceptSets,
                    rowsToCols,
                    colDFA,
                    colAcceptSetIDs,
                    colAcceptSets,
                    rowStates,
                    colStates
                ].flatMap(arr => arr.decl)),
                declFunc(
                    this.updateFuncName,
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
                this.update(ZERO, ZERO, g.width, g.height),
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
        
        const [rowsAcceptSetIDs, rowsAcceptSetMap] = rowDFA.getAcceptSetMap(rowsAcceptOrKeepMap, rowsAcceptMap.predicate());
        
        // reduce alphabet size of colDFA by not distinguishing rows which aren't part of taller patterns
        const [rowsToCols, rowsToColsMap] = rowDFA.getAcceptSetMap(rowsAcceptOrKeepMap, rowsKeepMap.predicate());
        
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
            if(pattern.height > 1) {
                const seq = leafRowMap[i].map(rowID => colRegexLetters[rowID]);
                colRegexPatterns.push({pattern, seq});
            }
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
    
    type PatternSeq = Readonly<{pattern: Pattern, seq: readonly Regex.Node<Pattern>[]}>
    function _makeRegex(patterns: PatternSeq[]): Regex.Node<Pattern> {
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
