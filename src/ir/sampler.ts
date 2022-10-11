///<reference path="names.ts"/>

namespace IR {
    export interface AbstractSampler {
        readonly count: Expr;
        declare(): Stmt,
        handleMatch(f: 'add' | 'del', patternIndex: Expr): Stmt;
        sampleWithReplacement(cases: readonly Stmt[]): Stmt;
        beginSamplingWithoutReplacement(): Stmt;
        sampleWithoutReplacement(cases: readonly Stmt[], count: Expr): Stmt;
        copyInto(matches: MatchesArray, shuffle: boolean): Stmt[];
        forEach(then: readonly Stmt[]): Stmt;
    }
    
    const {RNG, I, S, MATCH, AT, AT_X, AT_Y} = NAMES;
    
    export class Sampler implements AbstractSampler {
        private readonly name: NameExpr;
        public readonly count: Expr;
        private readonly arr: Expr;
        private readonly capacity: Expr;
        private readonly declareMatchAt: Stmt;
        private readonly matchPatternIndex: Expr;
        
        public constructor(
            id: number,
            public readonly g: Grid,
            public readonly numPatterns: number,
        ) {
            this.name = NAMES.sampler(g, id);
            this.count = attr(this.name, 'count');
            this.arr = attr(this.name, 'arr');
            this.capacity = OP.multConstant(g.n, numPatterns);
            
            this.declareMatchAt = this.g.declareAtIndex(OP.divConstant(MATCH, this.numPatterns));
            this.matchPatternIndex = OP.modConstant(MATCH, this.numPatterns);
        }
        
        public declare(): Stmt {
            return declVar(this.name, SAMPLER_TYPE, libConstructorCall('Sampler', [this.capacity]));
        }
        
        public handleMatch(f: 'add' | 'del', patternIndex: Expr): Stmt {
            const match = OP.multAddConstant(AT, this.numPatterns, patternIndex);
            return libMethodCallStmt('Sampler', f, this.name, [match]);
        }
        
        public sampleWithReplacement(cases: readonly Stmt[]): Stmt {
            return block([
                IR.declVar(
                    MATCH,
                    IR.INT_TYPE,
                    access(this.arr, libMethodCall('PRNG', 'nextInt', RNG, [this.count])),
                ),
                this.declareMatchAt,
                switch_(this.matchPatternIndex, cases),
            ]);
        }
        
        public beginSamplingWithoutReplacement(): Stmt {
            return PASS;
        }
        
        public sampleWithoutReplacement(cases: readonly Stmt[], count: Expr): Stmt {
            return block([
                declVar(
                    MATCH,
                    INT_TYPE,
                    libMethodCall('Sampler', 'sample', this.name, [count, RNG]),
                ),
                this.declareMatchAt,
                switch_(this.matchPatternIndex, cases),
            ]);
        }
        
        public copyInto(matches: MatchesArray, shuffle: boolean): Stmt[] {
            return [
                shuffle ? libMethodCallStmt('Sampler', 'shuffleInto', this.name, [matches.array, RNG])
                    : libMethodCallStmt('Sampler', 'copyInto', this.name, [matches.array]),
                declVar(matches.count, INT_TYPE, this.count),
            ];
        }
        
        public forEach(then: readonly Stmt[]): Stmt {
            return this.numPatterns === 1
                ? forRange(I, ZERO, this.count, [
                    this.g.declareAtIndex(access(this.arr, I)),
                    ...then,
                ])
                : fail();
        }
    }
    
    export class TrivialSampler implements AbstractSampler {
        public readonly count: Expr;
        private readonly width: Expr;
        private readonly height: Expr;
        private readonly is1x1: boolean;
        
        public constructor(
            private readonly g: Grid,
            patternWidth: number,
            patternHeight: number,
        ) {
            this.width = OP.minusConstant(g.width, patternWidth - 1);
            this.height = OP.minusConstant(g.height, patternHeight - 1);
            this.is1x1 = patternWidth === 1 && patternHeight === 1;
            this.count = this.is1x1 ? g.n : OP.mult(this.width, this.height);
        }
        
        public declare(): Stmt {
            return PASS;
        }
        
        public handleMatch(f: "add" | "del", patternIndex: Expr): Stmt {
            fail();
        }
        
        public sampleWithReplacement(cases: readonly Stmt[]): Stmt {
            return block([
                this.is1x1 ? this.g.declareAtIndex(libMethodCall('PRNG', 'nextInt', RNG, [this.count]))
                : this.g.declareAtXY(libMethodCall('PRNG', 'nextInt', RNG, [this.width]), libMethodCall('PRNG', 'nextInt', RNG, [this.height])),
                cases.length === 1 ? cases[0] : fail(),
            ]);
        }
        
        public beginSamplingWithoutReplacement(): Stmt {
            return IR.declVar(S, INT_TYPE, OP.addOne(libMethodCall('PRNG', 'nextInt', RNG, [this.count])), true);
        }
        
        public sampleWithoutReplacement(cases: readonly Stmt[], count: Expr): Stmt {
            const declAt = this.is1x1
                ? this.g.declareAtIndex(OP.minusOne(S))
                : this.g.declareAtXY(OP.mod(S, this.width), OP.floordiv(OP.minusOne(S), this.width));
            return IR.block([
                IR.while_(
                    TRUE,
                    IR.block([
                        assign(S, '=', ternary(
                            OP.ne(OP.bitwiseAnd(S, ONE), ZERO),
                            OP.bitwiseXor(OP.rshift(S, ONE), this.g.useLsfrFeedbackTerm()),
                            OP.rshift(S, ONE),
                        )),
                        if_(OP.le(S, this.count), BREAK),
                    ]),
                ),
                declAt,
                cases.length === 1 ? cases[0] : fail(),
            ]);
        }
        
        public copyInto(matches: MatchesArray, shuffle: boolean): Stmt[] {
            return [
                declVar(matches.count, INT_TYPE, ZERO, true),
                this.is1x1 ? forRange(AT, ZERO, this.count, matches.add(AT, shuffle))
                : forRange(AT_Y, ZERO, this.height, [
                    forRange(AT_X, ZERO, this.width, matches.add(this.g.index(AT_X, AT_Y), shuffle)),
                ]),
            ];
        }
        
        public forEach(then: readonly Stmt[]): Stmt {
            return this.is1x1
                ? forRange(AT, ZERO, this.count, [
                    this.g.declareAtIndex(AT),
                    ...then,
                ])
                : forRange(AT_Y, ZERO, this.height, [
                    forRange(AT_X, ZERO, this.width, [
                        this.g.declareAtXY(AT_X, AT_Y),
                        ...then,
                    ])
                ]);
        }
    }
}
