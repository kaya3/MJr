namespace IR {
    export interface SamplerTarget extends Omit<MutableArray, 'decl'> {}
    
    export interface AbstractSampler {
        readonly count: Expr;
        readonly isNotEmpty: Expr;
        declare(): StmtLevelDecl,
        handleMatch(f: 'add' | 'del', patternIndex: Expr, at: Location): Stmt;
        sampleWithReplacement(
            prng: PRNG,
            cases: (at: Location) => readonly Stmt[],
        ): Stmt;
        sampleWithoutReplacement(
            prng: PRNG,
            cases: (at: Location, ifT: Stmt, ifF: Stmt) => readonly Stmt[],
            ifTrue: Stmt,
            ifFalse: Stmt,
        ): Stmt;
        copyInto(matches: SamplerTarget): Stmt;
        copyIntoOffset(matches: SamplerTarget, offset: Expr, m: number, c: number): Stmt;
        shuffleInto(prng: PRNG, matches: TempArray): Stmt;
        shuffleIntoOffset(prng: PRNG, matches: TempArray, m: number, c: number): Stmt;
        forEach(then: (at: Location) => Stmt): Stmt;
    }
    
    export class Sampler implements AbstractSampler {
        private readonly decl: ConstDecl;
        public readonly count: Expr;
        public readonly isNotEmpty: Expr;
        private readonly arr: Expr;
        
        public constructor(
            private readonly ir: Factory,
            public readonly g: Grid,
            public readonly numPatterns: number,
        ) {
            const capacity = OP.multConstant(g.n, numPatterns);
            
            this.decl = ir.constDecl(`sampler`, SAMPLER_TYPE, libConstructorCall('Sampler', [capacity]));
            this.count = attr(this.decl.name, 'count');
            this.isNotEmpty = OP.gt(this.count, ZERO);
            this.arr = attr(this.decl.name, 'arr');
        }
        
        public declare(): StmtLevelDecl {
            return this.decl;
        }
        
        public handleMatch(f: 'add' | 'del', patternIndex: Expr, at: Location): Stmt {
            const match = OP.multAddConstant(at.index, this.numPatterns, patternIndex);
            return libMethodCallStmt('Sampler', f, this.decl.name, [match]);
        }
        
        private matchSwitch(match: ConstExpr, cases: (at: Location) => readonly Stmt[]): Stmt {
            const k = this.numPatterns;
            return this.g.declareAtIndex(OP.divConstant(match, k), at =>
                switch_(OP.modConstant(match, k), cases(at)),
            );
        }
        
        public sampleWithReplacement(prng: PRNG, cases: (at: Location) => readonly Stmt[]): Stmt {
            return this.ir.withConst('match', INT_TYPE, access(this.arr, prng.nextInt(this.count)), match =>
                this.matchSwitch(match, cases),
            );
        }
        
        public sampleWithoutReplacement(
            prng: PRNG,
            cases: (at: Location, ifT: Stmt, ifF: Stmt) => readonly Stmt[],
            ifTrue: Stmt,
            ifFalse: Stmt,
        ): Stmt {
            const ir = this.ir;
            const flag = ir.flag();
            return ir.withVar('count', INT_TYPE, this.count, count =>
                withDecl(flag.decl, IR.seq([
                    IR.while_(
                        OP.and(OP.not(flag.check), OP.gt(count, ZERO)),
                        ir.withConst('match', INT_TYPE, libMethodCall('Sampler', 'sample', this.decl.name, [count, prng.name]), match =>
                            seq([
                                this.matchSwitch(match, at => cases(at, flag.set, PASS)),
                                assign(count, '-=', ONE),
                            ]),
                        ),
                    ),
                    IR.if_(flag.check, ifTrue, ifFalse),
                ])),
            );
        }
        
        public copyInto(matchesArray: SamplerTarget): Stmt {
            return libMethodCallStmt('Sampler', 'copyInto', this.decl.name, [matchesArray.array]);
        }
        
        public copyIntoOffset(matches: SamplerTarget, offset: Expr, m: number, c: number): Stmt {
            return libMethodCallStmt('Sampler', 'copyIntoOffset', this.decl.name, [matches.array, offset, int(m), int(c)]);
        }
        
        public shuffleInto(prng: PRNG, matches: TempArray): Stmt {
            return libMethodCallStmt('Sampler', 'shuffleInto', this.decl.name, [matches.array, prng.name]);
        }
        
        public shuffleIntoOffset(prng: PRNG, matches: TempArray, m: number, c: number): Stmt {
            return seq([
                libMethodCallStmt('Sampler', 'shuffleIntoOffset', this.decl.name, [matches.array, matches.count, int(m), int(c), prng.name]),
                assign(matches.count, '+=', this.count),
            ]);
        }
        
        public forEach(then: (at: Location) => Stmt): Stmt {
            if(this.numPatterns !== 1) { fail(); }
            
            return this.ir.forRange('i', ZERO, this.count, i =>
                this.g.declareAtIndex(access(this.arr, i), then),
            );
        }
    }
    
    export class TrivialSampler implements AbstractSampler {
        public readonly count: Expr;
        public readonly isNotEmpty: Expr = TRUE;
        private readonly width: Expr;
        private readonly height: Expr;
        private readonly is1x1: boolean;
        
        public constructor(
            private readonly ir: Factory,
            private readonly g: Grid,
            patternWidth: number,
            patternHeight: number,
        ) {
            this.width = OP.minusConstant(g.width, patternWidth - 1);
            this.height = OP.minusConstant(g.height, patternHeight - 1);
            // TODO: optimisations also apply for 1-by-N patterns
            this.is1x1 = patternWidth === 1 && patternHeight === 1;
            this.count = this.is1x1 ? g.n : OP.mult(this.width, this.height);
        }
        
        public declare(): StmtLevelDecl {
            return NO_DECL;
        }
        
        public handleMatch(f: "add" | "del", patternIndex: Expr, at: Location): Stmt {
            fail();
        }
        
        public sampleWithReplacement(prng: PRNG, cases: (at: Location) => readonly Stmt[]): Stmt {
            const then = (at: Location) => {
                const cs = cases(at);
                if(cs.length !== 1) { fail(); }
                return cs[0];
            };
            
            return this.is1x1
                ? this.g.declareAtIndex(prng.nextInt(this.count), then)
                : this.g.declareAtXY(prng.nextInt(this.width), prng.nextInt(this.height), then);
        }
        
        public sampleWithoutReplacement(
            prng: PRNG,
            cases: (at: Location, ifT: Stmt, ifF: Stmt) => readonly Stmt[],
            ifTrue: Stmt,
            ifFalse: Stmt,
        ): Stmt {
            const ir = this.ir;
            const flag = ir.flag();
            return ir.withVar('match', INT_TYPE, OP.add(prng.nextInt(this.count), ONE), match => {
                const nextMatch = while_(
                    TRUE,
                    seq([
                        assign(match, '=', ternary(
                            OP.ne(OP.bitwiseAnd(match, ONE), ZERO),
                            OP.bitwiseXor(OP.rshift(match, ONE), this.g.lfsrFeedbackTerm),
                            OP.rshift(match, ONE),
                        )),
                        if_(OP.le(match, this.count), BREAK),
                    ]),
                );
                
                const then = (at: Location) => {
                    const cs = cases(at, seq([flag.set, BREAK]), PASS);
                    if(cs.length !== 1) { fail(); }
                    return cs[0];
                };
                
                const declAt = this.is1x1
                    ? this.g.declareAtIndex(OP.minus(match, ONE), then)
                    : this.g.declareAtXY(OP.mod(match, this.width), OP.floordiv(OP.minus(match, ONE), this.width), then);
                
                return ir.withVar('count', INT_TYPE, this.count, count =>
                    withDecl(flag.decl, seq([
                        while_(
                            OP.and(OP.not(flag.check), OP.gt(count, ZERO)),
                            seq([
                                declAt,
                                assign(count, '-=', ONE),
                                nextMatch,
                            ]),
                        ),
                        IR.if_(flag.check, ifTrue, ifFalse),
                    ])),
                );
            });
        }
        
        public copyInto(matches: SamplerTarget): Stmt {
            return this.copyIntoOffset(matches, ZERO, 1, 0);
        }
        
        public copyIntoOffset(matches: SamplerTarget, offset: Expr, m: number, c: number): Stmt {
            const {ir} = this;
            return this.is1x1
                ? this.forEach(at =>
                    matches.set(OP.add(at.index, offset), '=', OP.multAddConstant(at.index, m, int(c))),
                )
                : ir.withVar('j', INT_TYPE, offset, j =>
                    this.forEach(at => seq([
                        matches.set(j, '=', OP.multAddConstant(at.index, m, int(c))),
                        assign(j, '+=', ONE),
                    ])),
                );
        }
        
        public shuffleInto(prng: PRNG, matches: TempArray): Stmt {
            return this.shuffleIntoOffset(prng, matches, 1, 0);
        }
        
        public shuffleIntoOffset(prng: PRNG, matches: TempArray, m: number, c: number): Stmt {
            return this.forEach(at =>
                matches.insertShuffled(prng, OP.multAddConstant(at.index, m, int(c)))
            );
        }
        
        public forEach(then: (at: Location) => Stmt): Stmt {
            const {ir, g} = this;
            return this.is1x1
                ? ir.forRange('i', ZERO, this.count, i =>
                    g.declareAtIndex(i, then),
                )
                : ir.forRange('y', ZERO, this.height, y =>
                    ir.forRange('x', ZERO, this.width, x =>
                        g.declareAtXY(x, y, then),
                    )
                );
        }
    }
    
    export class EmptySampler implements AbstractSampler {
        public readonly count: Expr = ZERO;
        public readonly isNotEmpty: Expr = FALSE;
        declare(): StmtLevelDecl {
            return NO_DECL;
        }
        handleMatch(): Stmt {
            fail();
        }
        sampleWithReplacement(): Stmt {
            return exprStmt(unusedExpr(`empty sampler`));
        }
        sampleWithoutReplacement(prng: PRNG, cases: (at: Location, ifT: Stmt, ifF: Stmt) => readonly Stmt[], ifTrue: Stmt, ifFalse: Stmt): Stmt {
            return exprStmt(unusedExpr(`empty sampler`));
        }
        copyInto(matches: SamplerTarget): Stmt {
            return PASS;
        }
        copyIntoOffset(matches: SamplerTarget, offset: Expr, m: number, c: number): Stmt {
            return PASS;
        }
        shuffleInto(prng: PRNG, matches: TempArray): Stmt {
            return matches.declareCount(ZERO);
        }
        shuffleIntoOffset(prng: PRNG, matches: TempArray, m: number, c: number): Stmt {
            return PASS;
        }
        forEach(then: (at: Location) => Stmt): Stmt {
            return PASS;
        }
    }
}
