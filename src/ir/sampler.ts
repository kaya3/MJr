///<reference path="names.ts"/>

namespace IR {
    export interface AbstractSampler {
        readonly count: Expr;
        readonly isNotEmpty: Expr;
        declare(): Stmt,
        handleMatch(f: 'add' | 'del', patternIndex: Expr): Stmt;
        sampleWithReplacement(cases: readonly Stmt[]): Stmt;
        beginSamplingWithoutReplacement(): Stmt;
        sampleWithoutReplacement(cases: readonly Stmt[], count: NameExpr): Stmt;
        copyInto(matchesArray: NameExpr): Stmt;
        copyIntoOffset(matchesArray: NameExpr, offset: Expr, m: number, c: number): Stmt;
        shuffleInto(matches: MatchesArray): Stmt;
        shuffleIntoOffset(matches: MatchesArray, m: number, c: number): Stmt;
        forEach(then: readonly Stmt[]): Stmt;
    }
    
    const {
        RNG,
        I, J,
        S, MATCH,
        AT, AT_X, AT_Y,
    } = NAMES;
    
    export class Sampler implements AbstractSampler {
        private readonly name: NameExpr;
        public readonly count: Expr;
        public readonly isNotEmpty: Expr;
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
            this.isNotEmpty = OP.gt(this.count, ZERO);
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
                    access(this.arr, PRNG.nextInt(this.count)),
                ),
                this.declareMatchAt,
                switch_(this.matchPatternIndex, cases),
            ]);
        }
        
        public beginSamplingWithoutReplacement(): Stmt {
            return PASS;
        }
        
        public sampleWithoutReplacement(cases: readonly Stmt[], count: NameExpr): Stmt {
            return block([
                declVar(
                    MATCH,
                    INT_TYPE,
                    libMethodCall('Sampler', 'sample', this.name, [count, RNG]),
                ),
                this.declareMatchAt,
                switch_(this.matchPatternIndex, cases),
                assign(count, '-=', ONE),
            ]);
        }
        
        public copyInto(matchesArray: NameExpr): Stmt {
            return libMethodCallStmt('Sampler', 'copyInto', this.name, [matchesArray]);
        }
        
        public copyIntoOffset(matchesArray: NameExpr, offset: Expr, m: number, c: number): Stmt {
            return libMethodCallStmt('Sampler', 'copyIntoOffset', this.name, [matchesArray, offset, int(m), int(c)]);
        }
        
        public shuffleInto(matchesArray: MatchesArray): Stmt {
            return block([
                libMethodCallStmt('Sampler', 'shuffleInto', this.name, [matchesArray.array, RNG]),
                IR.assign(matchesArray.count, '+=', this.count),
            ]);
        }
        
        public shuffleIntoOffset(matchesArray: MatchesArray, m: number, c: number): Stmt {
            return block([
                libMethodCallStmt('Sampler', 'shuffleIntoOffset', this.name, [matchesArray.array, matchesArray.count, int(m), int(c), RNG]),
                IR.assign(matchesArray.count, '+=', this.count),
            ]);
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
        public readonly isNotEmpty: Expr = TRUE;
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
                this.is1x1 ? this.g.declareAtIndex(PRNG.nextInt(this.count))
                : this.g.declareAtXY(PRNG.nextInt(this.width), PRNG.nextInt(this.height)),
                cases.length === 1 ? cases[0] : fail(),
            ]);
        }
        
        public beginSamplingWithoutReplacement(): Stmt {
            return declVar(S, INT_TYPE, OP.add(PRNG.nextInt(this.count), ONE), true);
        }
        
        public sampleWithoutReplacement(cases: readonly Stmt[], count: NameExpr): Stmt {
            const declAt = this.is1x1
                ? this.g.declareAtIndex(OP.minus(S, ONE))
                : this.g.declareAtXY(OP.mod(S, this.width), OP.floordiv(OP.minus(S, ONE), this.width));
            return block([
                while_(
                    TRUE,
                    block([
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
                assign(count, '-=', ONE),
            ]);
        }
        
        public copyInto(matchesArray: NameExpr): Stmt {
            return this.copyIntoOffset(matchesArray, ZERO, 1, 0);
        }
        
        public copyIntoOffset(matchesArray: NameExpr, offset: Expr, m: number, c: number): Stmt {
            return this.is1x1
                ? forRange(AT, ZERO, this.count, [
                    IR.assign(IR.access(matchesArray, OP.add(AT, offset)), '=', OP.multAddConstant(AT, m, int(c))),
                ])
                : block([
                    IR.declVar(J, INT_TYPE, offset, true),
                    forRange(AT_Y, ZERO, this.height, [
                        forRange(AT_X, ZERO, this.width, [
                            IR.assign(IR.access(matchesArray, J), '=', OP.multAddConstant(this.g.index(AT_X, AT_Y), m, int(c))),
                            IR.assign(J, '+=', ONE),
                        ]),
                    ]),
                ]);
        }
        
        public shuffleInto(matches: MatchesArray): Stmt {
            return this.shuffleIntoOffset(matches, 1, 0);
        }
        
        public shuffleIntoOffset(matches: MatchesArray, m: number, c: number): Stmt {
            return this.is1x1
                ? forRange(AT, ZERO, this.count, matches.insertShuffled(OP.multAddConstant(AT, m, int(c))))
                : forRange(AT_Y, ZERO, this.height, [
                    forRange(AT_X, ZERO, this.width, matches.insertShuffled(OP.multAddConstant(this.g.index(AT_X, AT_Y), m, int(c)))),
                ]);
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
    
    export class EmptySampler implements AbstractSampler {
        public readonly count: Expr = ZERO;
        public readonly isNotEmpty: Expr = FALSE;
        declare(): Stmt {
            return PASS;
        }
        handleMatch(f: "add" | "del", patternIndex: Expr): Stmt {
            fail();
        }
        sampleWithReplacement(cases: readonly Stmt[]): Stmt {
            return throw_(`empty sampler`);
        }
        beginSamplingWithoutReplacement(): Stmt {
            return PASS;
        }
        sampleWithoutReplacement(cases: readonly Stmt[], count: NameExpr): Stmt {
            return throw_(`empty sampler`);
        }
        copyInto(matchesArray: NameExpr): Stmt {
            return PASS;
        }
        copyIntoOffset(matchesArray: NameExpr, offset: Expr, m: number, c: number): Stmt {
            return PASS;
        }
        shuffleInto(matches: MatchesArray): Stmt {
            return PASS;
        }
        shuffleIntoOffset(matches: MatchesArray, m: number, c: number): Stmt {
            return PASS;
        }
        forEach(then: readonly Stmt[]): Stmt {
            return PASS;
        }
        
    }
}
