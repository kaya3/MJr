///<reference path="names.ts"/>

namespace IR {
    const {
        J,
    } = NAMES;
    
    export class TempArray {
        private readonly getAtCount: ArrayAccessExpr;
        public readonly isNotEmpty: Expr;
        public readonly incrementCount: Stmt;
        
        public constructor(
            public readonly array: NameExpr,
            public readonly count: NameExpr,
        ) {
            this.getAtCount = this.get(count);
            this.isNotEmpty = OP.gt(count, ZERO);
            this.incrementCount = assign(count, '+=', ONE);
        }
        
        public declareCount(initial: Expr): Stmt {
            return assign(this.count, '=', initial);
        }
        
        public get(index: Expr): ArrayAccessExpr {
            return access(this.array, index);
        }
        
        public push(match: Expr): Stmt[] {
            return [
                assign(this.getAtCount, '=', match),
                this.incrementCount,
            ];
        }
        
        public insertShuffled(match: Expr): Stmt[] {
            return [
                declVar(J, INT_TYPE, PRNG.nextInt(OP.add(this.count, ONE))),
                assign(this.getAtCount, '=', this.get(J)),
                assign(this.get(J), '=', match),
                this.incrementCount,
            ];
        }
    }
}
