namespace IR {
    export class Variables {
        private readonly names: readonly NameExpr[];
        
        public constructor(private readonly variables: readonly ASG.FormalVariable[]) {
            const counts = new Map<string, number>();
            this.names = variables.map(v => {
                const count = counts.get(v.name) ?? 0;
                counts.set(v.name, count + 1);
                return IR.NAMES.variable(v.name, count);
            });
        }
        
        public declare(c: IRCompiler): readonly Stmt[] {
            const {names} = this;
            // this also filters out compile-time constants, since the resolver folds them instead of referencing them
            return this.variables
                .filter(v => v.references > 0)
                .map(v => declVar(
                    names[v.id],
                    c.type(v.type),
                    v.initialiser && c.expr(v.initialiser),
                    v.initialiser === undefined,
                ));
        }
        
        public name(variableID: number): NameExpr {
            return this.names[variableID];
        }
        
        public type(variableID: number, c: IRCompiler): IRType {
            return c.type(this.variables[variableID].type);
        }
    }
}
