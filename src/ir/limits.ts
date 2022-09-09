namespace IR {
    export class Limits {
        private readonly vars: readonly NameExpr[];
        private readonly checks: readonly Expr[];
        private readonly decrements: readonly Stmt[];
        
        public constructor(private readonly limits: readonly ASG.FormalLimit[]) {
            const vars = this.vars = makeArray(limits.length, NAMES.limit);
            this.checks = vars.map(v => OP.gt(v, ZERO));
            this.decrements = vars.map(v => assign(v, '-=', ONE));
        }
        
        public declare(c: IRCompiler): Stmt {
            const {vars} = this;
            return declVars(this.limits.map((limit, i) => ({
                name: vars[i],
                type: INT_TYPE,
                initialiser: limit.canReset ? undefined : c.expr(limit.initialiser),
            })), true);
        }
        public reset(limitID: number, c: IRCompiler): Stmt {
            const limit = this.limits[limitID];
            if(!limit.canReset) { throw new Error(); }
            return assign(this.vars[limitID], '=', c.expr(limit.initialiser));
        }
        public check(limitID: number): Expr {
            return this.checks[limitID];
        }
        public decrement(limitID: number): Stmt {
            return this.decrements[limitID];
        }
    }
}
