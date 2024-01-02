///<reference path="factory.ts"/>

namespace IR {
    export class TempArray implements SamplerTarget {
        private readonly capacity: DeferredExpr;
        private readonly decl: ConstDecl;
        private readonly countDecl: MutVarDecl;
        
        public readonly array: ConstNameExpr;
        public readonly count: MutNameExpr;
        public readonly isNotEmpty: Expr;
        public readonly incrementCount: Stmt;
        
        public constructor(
            private readonly ir: Factory,
        ) {
            const capacity = this.capacity = ir.deferredExpr('TempArray.capacity');
            const decl = this.decl = ir.mutArrayDecl('matches', capacity, INT32_ARRAY_TYPE.domainSize);
            const countDecl = this.countDecl = ir.varDecl('count', INT_TYPE);
            
            this.array = decl.name;
            const count = this.count = countDecl.name;
            
            this.isNotEmpty = OP.gt(count, ZERO);
            this.incrementCount = assign(count, '+=', ONE);
        }
        
        public declare(capacity: Expr): StmtLevelDecl {
            return multiDecl([
                replaceInDecl(this.decl, this.capacity, capacity),
                this.countDecl, // TODO
            ]);
        }
        
        public declareCount(initial: Expr): Stmt {
            return assign(this.count, '=', initial);
        }
        
        public get(index: Expr): ArrayAccessExpr {
            return access(this.array, index);
        }
        
        public set(index: Expr, op: AssignOp, value: Expr): Stmt {
            return assign(this.get(index), op, value);
        }
        
        public push(value: Expr): Stmt {
            return seq([
                assign(this.get(this.count), '=', value),
                this.incrementCount,
            ]);
        }
        
        public insertShuffled(prng: PRNG, value: Expr): Stmt {
            return this.ir.withConst('j', INT_TYPE, prng.nextInt(OP.add(this.count, ONE)), j => seq([
                assign(this.get(this.count), '=', this.get(j)),
                assign(this.get(j), '=', value),
                this.incrementCount,
            ]));
        }
    }
}
