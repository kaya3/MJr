namespace IR {
    export class PRNG {
        public constructor(public readonly name: ConstNameExpr) {}
        
        public nextInt(n: Expr): Expr {
            return n === ONE ? ZERO : libMethodCall('PRNG', 'nextInt', this.name, [n]);
        }
        public nextIntChecked(n: Expr): Expr {
            return libFunctionCall('nextIntChecked', [this.name, n]);
        }
        
        private _nextDouble?: Expr;
        public nextDouble(): Expr {
            return this._nextDouble ??= libMethodCall('PRNG', 'nextDouble', this.name, []);
        }
    }
}
