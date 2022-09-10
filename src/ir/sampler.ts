///<reference path="names.ts"/>

namespace IR {
    export interface AbstractSampler {
        readonly count: Expr;
        declare(): VarDeclWithInitialiser[],
        declareAt(i: Expr): Stmt;
        sampleWithReplacement(): Expr;
        beginSamplingWithoutReplacement(matchVar: NameExpr): Stmt;
        sampleWithoutReplacement(matchVar: NameExpr, count: Expr): Stmt;
        copyInto(indexVar: NameExpr, matches: MatchesArray, shuffle: boolean): Stmt[];
    }
    
    const {RNG, S} = NAMES;
    
    export class Sampler implements AbstractSampler {
        public readonly name: NameExpr;
        public readonly count: Expr;
        public readonly arr: Expr;
        
        public constructor(
            id: number,
            public readonly g: Grid,
            public readonly numPatterns: number,
        ) {
            this.name = NAMES.sampler(g, id);
            this.count = attr(this.name, 'count');
            this.arr = attr(this.name, 'arr');
        }
        
        public declare(): VarDeclWithInitialiser[] {
            return [{
                name: this.name,
                type: SAMPLER_TYPE,
                initialiser: libConstructorCall('Sampler', [OP.multConstant(this.g.n, this.numPatterns)]),
            }];
        }
        
        public declareAt(i: Expr): Stmt {
            return this.g.declareAtIndex(access(this.arr, i));
        }
        
        public sampleWithReplacement(): Expr {
            return access(this.arr, libMethodCall('PRNG', 'nextInt', RNG, [this.count]));
        }
        
        public beginSamplingWithoutReplacement(): Stmt {
            return PASS;
        }
        
        public sampleWithoutReplacement(matchVar: NameExpr, count: Expr): Stmt {
            return declVar(
                matchVar,
                INT_TYPE,
                libMethodCall('Sampler', 'sample', this.name, [count, RNG]),
            );
        }
        
        public copyInto(indexVar: NameExpr, matches: MatchesArray, shuffle: boolean): Stmt[] {
            return [
                shuffle ? libMethodCallStmt('Sampler', 'shuffleInto', this.name, [matches.array, RNG])
                    : libMethodCallStmt('Sampler', 'copyInto', this.name, [matches.array]),
                declVar(matches.count, INT_TYPE, this.count),
            ];
        }
    }
    
    export class TrivialSampler implements AbstractSampler {
        public readonly count: Expr;
        
        public constructor(private readonly g: Grid) {
            this.count = g.n;
        }
        
        public declare(): VarDeclWithInitialiser[] {
            return [];
        }
        
        public declareAt(i: Expr): Stmt {
            return this.g.declareAtIndex(i);
        }
        
        public sampleWithReplacement(): Expr {
            return libMethodCall('PRNG', 'nextInt', RNG, [this.count]);
        }
        
        public beginSamplingWithoutReplacement(matchVar: NameExpr): Stmt {
            return IR.declVar(S, INT_TYPE, OP.add(libMethodCall('PRNG', 'nextInt', RNG, [this.g.n]), ONE), true);
        }
        
        public sampleWithoutReplacement(matchVar: NameExpr, count: Expr): Stmt {
            const {g} = this;
            return IR.block([
                IR.while_(
                    TRUE,
                    IR.block([
                        assign(S, '=', ternary(OP.ne(OP.bitwiseAnd(S, ONE), ZERO), OP.bitwiseXor(OP.rshift(S, ONE), g.useLsfrFeedbackTerm()), OP.rshift(S, ONE))),
                        if_(OP.le(S, g.n), BREAK),
                    ]),
                ),
                IR.declVar(matchVar, INT_TYPE, OP.minusOne(S)),
            ]);
        }
        
        public copyInto(indexVar: NameExpr, matches: MatchesArray, shuffle: boolean): Stmt[] {
            return [
                declVar(matches.count, INT_TYPE, ZERO, true),
                forRange(indexVar, ZERO, this.count, matches.add(indexVar, shuffle)),
            ];
        }
    }
}
