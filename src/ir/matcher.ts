///<reference path="factory.ts"/>

namespace IR {
    // TODO: more kinds of match handler (e.g. conditional on a boolean expression)
    export type MatchHandler = Readonly<
        | {kind: 'sampler', pattern: PatternTree, sampler: AbstractSampler, i: number}
        | {kind: 'counter', pattern: PatternTree, counter: MutNameExpr, weight: Expr}
        | {kind: 'convolution', pattern: Pattern, buffer: ConvBuffer, i: number}
    >
    
    export class Matcher {
        private readonly matchHandlers: MatchHandler[] = [];
        private readonly updateFuncName: ConstNameExpr;
        
        public constructor(
            private readonly ir: Factory,
            public readonly g: Grid,
        ) {
            this.updateFuncName = ir.func('matcherUpdate');
        }
        
        public addMatchHandler(handler: MatchHandler): void {
            this.matchHandlers.push(handler);
        }
        
        private makeMatchHandler(h: MatchHandler, at: Location, bitIndex: Expr, acceptID: number, f: 'add' | 'del'): Stmt {
            switch(h.kind) {
                case 'sampler':
                    // `I - constant` allows code to be shared among consecutive match handlers
                    return h.sampler.handleMatch(f, OP.addConstant(bitIndex, h.i - acceptID), at);
                
                case 'counter':
                    return assign(h.counter, f === 'add' ? '+=' : '-=', h.weight);
                
                case 'convolution':
                    return h.buffer.update(h.i, at, f === 'add' ? '+=' : '-=');
            }
        }
        
        public update(x: Expr, y: Expr, w: Expr, h: Expr): Stmt {
            return localCallStmt(this.updateFuncName, [x, y, w, h]);
        }
        
        public declare(): StmtLevelDecl {
            const {ir, g} = this;
            
            const gridAlphabetSize = g.grid.alphabet.key.length,
                dfas = makePatternMatcherDFAs(gridAlphabetSize, this.matchHandlers.map(h => h.pattern)),
                colsAlphabetSize = dfas.colDFA.alphabetSize,
                rowDFASize = dfas.rowDFA.size(),
                colDFASize = dfas.colDFA.size(),
                numRowPatterns = dfas.rowsAcceptMap.size(),
                numColPatterns = dfas.colsAcceptMap.size(),
                rowAcceptSetMasks = dfas.rowsAcceptSetMap.flatMap(entry => [...dfas.rowsAcceptMap.getIDSet(entry)]),
                colAcceptSetMasks = dfas.colsAcceptSetMap.flatMap(entry => [...dfas.colsAcceptMap.getIDSet(entry)]),
                rowDFA = makeConstArray2D(ir, 'rowDFA', dfas.rowDFA.toFlatArray(), gridAlphabetSize, rowDFASize),
                rowAcceptSetIDs = makeConstArray(ir, 'rowAcceptSetIDs', dfas.rowsAcceptSetIDs, dfas.rowsAcceptSetMap.size()),
                rowAcceptSets = makeConstArray2D(ir, 'rowAcceptSets', rowAcceptSetMasks, (numRowPatterns + 31) >> 5, numRowPatterns <= 31 ? 1 << numRowPatterns : INT32_ARRAY_TYPE.domainSize),
                rowsToCols = makeConstArray(ir, 'rowsToCols', dfas.rowsToCols, colsAlphabetSize),
                colDFA = makeConstArray2D(ir, 'colDFA', dfas.colDFA.toFlatArray(), colsAlphabetSize, colDFASize),
                colAcceptSetIDs = makeConstArray(ir, 'colAcceptSetIDs', dfas.colsAcceptSetIDs, dfas.colsAcceptSetMap.size()),
                colAcceptSets = makeConstArray2D(ir, 'colAcceptSets', colAcceptSetMasks, (numColPatterns + 31) >> 5, numColPatterns <= 31 ? 1 << numColPatterns : INT32_ARRAY_TYPE.domainSize),
                rowStates = makeMutableArray(ir, 'rowStates', g.n, rowDFASize),
                colStates = makeMutableArray(ir, 'colStates', g.n, colDFASize);
            
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
            
            const startX = ir.paramDecl('startX', INT_TYPE),
                startY = ir.paramDecl('startY', INT_TYPE),
                effectiveWidth = ir.paramDecl('w', INT_TYPE),
                effectiveHeight = ir.paramDecl('h', INT_TYPE),
                minX = ir.varDecl('minX', INT_TYPE, startX.name),
                minY = ir.varDecl('minY', INT_TYPE, startY.name),
                maxX = ir.constDecl('maxX', INT_TYPE, OP.add(startX.name, effectiveWidth.name)),
                maxY = ir.constDecl('maxY', INT_TYPE, OP.add(startY.name, effectiveHeight.name));
            
            const makeStateChangeHandlers = (at: Location, t1: Expr, t2: Expr, mask: MutNameExpr, acceptSets: ConstArray2D, acceptMap: ReadonlyIDMap<PatternTree>, f: 'add' | 'del'): Stmt[] => {
                const maskSize = (acceptMap.size() + 31) >> 5;
                
                const out: Stmt[] = [];
                for(let index = 0; index < maskSize; ++index) {
                    const bitIndex = ir.constDecl('i', INT_TYPE, OP.countTrailingZeros(mask));
                    
                    const cases: Stmt[] = [];
                    const minAcceptID = index << 5,
                        maxAcceptID = Math.min(minAcceptID + 32, acceptMap.size());
                    for(let acceptID = minAcceptID; acceptID < maxAcceptID; ++acceptID) {
                        const key = PatternTree.key(acceptMap.getByID(acceptID));
                        const handlers = handlersByPattern.get(key) ?? fail();
                        cases.push(seq(handlers.map(h => this.makeMatchHandler(h, at, bitIndex.name, acceptID & 31, f))));
                    }
                    
                    out.push(
                        assign(mask, '=', maskDiff(acceptSets, t1, t2, index)),
                        // special cases for short switches, when there is no need for `countTrailingZeros`
                        cases.length === 1 ? if_(OP.ne(mask, ZERO), replace(cases[0], bitIndex.name, ZERO))
                        : cases.length <= 4 ? seq(
                            cases.map((c, i) => if_(OP.ne(OP.bitwiseAnd(mask, int(1 << i)), ZERO), replace(c, bitIndex.name, int(i))))
                        )
                        : while_(OP.ne(mask, ZERO), seq([
                            withDecls([bitIndex], switch_(bitIndex.name, cases, true)),
                            assign(mask, '&=', OP.minus(mask, ONE)),
                        ])),
                    );
                }
                
                return out;
            };
            
            const makeUpdateSamplers = (acceptSetIDs: ConstArray, acceptSets: ConstArray2D, acceptMap: ReadonlyIDMap<PatternTree>, at: Location, state: MutNameExpr, oldState: ConstExpr): Stmt => {
                function _update(oldT: Expr, t: Expr): Stmt {
                    return ir.withVar('u', INT_TYPE, undefined, mask => seq([
                        ...makeStateChangeHandlers(at, oldT, t, mask, acceptSets, acceptMap, 'del'),
                        ...makeStateChangeHandlers(at, t, oldT, mask, acceptSets, acceptMap, 'add'),
                    ]));
                }
                
                const t = acceptSetIDs.get(state), oldT = acceptSetIDs.get(oldState);
                if(t === state && oldT === oldState) {
                    return _update(oldT, t);
                } else {
                    return ir.withConst('t', INT_TYPE, t, t => 
                        ir.withConst('oldT', INT_TYPE, oldT, oldT =>
                            if_(OP.ne(t, oldT), _update(oldT, t)),
                        ),
                    );
                }
            };
            
            const recomputeRowStates = [
                BLANK_LINE,
                comment('recompute row states'),
                ir.forRange('y', minY.name, maxY.name, y =>
                    ir.withVar('s', INT_TYPE, ternary(
                        OP.lt(maxX.name, g.width),
                        rowStates.get(g.index(maxX.name, y)),
                        ZERO,
                    ), rowState => ir.forRangeReverse('x', ZERO, maxX.name, x =>
                        g.declareAtXY(x, y, at =>
                            ir.withConst('oldS', INT_TYPE, rowStates.get(at.index), oldRowState => IR.seq([
                                assign(rowState, '=', rowDFA.get(rowState, g.data.get(at.index))),
                                if_(OP.eq(rowState, oldRowState), if_(OP.lt(x, startX.name), BREAK, CONTINUE)),
                                
                                rowStates.set(at.index, '=', rowState),
                                numColPatterns === 0 ? PASS : if_(OP.lt(x, minX.name), assign(minX.name, '=', x)),
                                
                                // must occur last, since it uses `continue`
                                makeUpdateSamplers(rowAcceptSetIDs, rowAcceptSets, dfas.rowsAcceptMap, at, rowState, oldRowState),
                            ])),
                        ),
                    )),
                ),
            ];
            const recomputeColStates = numColPatterns === 0 ? [] : [
                BLANK_LINE,
                comment('recompute col states'),
                ir.forRange('x', minX.name, maxX.name, x =>
                    ir.withVar('s', INT_TYPE, ternary(
                        OP.lt(maxY.name, g.height),
                        colStates.get(g.index(x, maxY.name)),
                        ZERO,
                    ), state => ir.forRangeReverse('y', ZERO, maxY.name, y =>
                        g.declareAtXY(x, y, at =>
                            ir.withConst('oldS', INT_TYPE, colStates.get(at.index), oldState =>
                                IR.seq([
                                    assign(state, '=', colDFA.get(state, rowsToCols.get(rowStates.get(at.index)))),
                                    if_(OP.eq(state, oldState), if_(OP.lt(y, startY.name), BREAK, CONTINUE)),
                                    
                                    colStates.set(at.index, '=', state),
                                    
                                    // must occur last, since it uses `continue`
                                    makeUpdateSamplers(colAcceptSetIDs, colAcceptSets, dfas.colsAcceptMap, at, state, oldState),
                                ]),
                            ),
                        ),
                    )),
                ),
            ];
            
            return initDecl(
                multiDecl([
                    rowDFA.decl,
                    rowAcceptSetIDs.decl,
                    rowAcceptSets.decl,
                    rowsToCols.decl,
                    colDFA.decl,
                    colAcceptSetIDs.decl,
                    colAcceptSets.decl,
                    rowStates.decl,
                    colStates.decl,
                    funcDecl(
                        this.updateFuncName,
                        undefined,
                        [startX, startY, effectiveWidth, effectiveHeight],
                        VOID_TYPE,
                        numRowPatterns === 0 && numColPatterns === 0 ? PASS : withDecls([minX, minY, maxX, maxY], seq([
                            ...recomputeRowStates,
                            ...recomputeColStates,
                        ])),
                    ),
                ]),
                this.update(ZERO, ZERO, g.width, g.height),
            );
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
