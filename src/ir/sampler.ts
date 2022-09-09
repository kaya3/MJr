namespace IR {
    export class Sampler {
        public readonly name: NameExpr;
        public readonly count: Expr;
        public readonly arr: Expr;
        public readonly isNotEmpty: Expr;
        
        public constructor(
            id: number,
            public readonly inGrid: Grid,
            public readonly numPatterns: number,
        ) {
            this.name = NAMES.sampler(inGrid, id);
            this.count = attr(this.name, 'count');
            this.arr = attr(this.name, 'arr');
            this.isNotEmpty = OP.gt(this.count, ZERO);
        }
        
        public declare(): VarDeclWithInitialiser {
            return {
                name: this.name,
                type: SAMPLER_TYPE,
                initialiser: libConstructorCall('Sampler', [OP.multConstant(this.inGrid.n, this.numPatterns)]),
            };
        }
        
        public get(index: Expr): Expr {
            return access(this.arr, index);
        }
        
        public sampleWithReplacement(): Expr {
            return this.get(libMethodCall('PRNG', 'nextInt', NAMES.RNG, [this.count]));
        }
        
        public sampleWithoutReplacement(count: Expr): Expr {
            return libMethodCall('Sampler', 'sample', this.name, [this.count, NAMES.RNG]);
        }
        
        public forEach(indexVar: NameExpr, then: readonly Stmt[]): Stmt {
            return forRange(indexVar, ZERO, this.count, block(then));
        }
    }
}
