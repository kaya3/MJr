///<reference path="../ir/names.ts"/>

namespace Compiler {
    const {NAMES} = IR;
    
    export function declareASGVariables(c: Compiler, variables: readonly ASG.FormalVariable[]) {
        // this also filters out compile-time constants, since the resolver folds them instead of referencing them
        return variables
            .filter(v => v.references > 0)
            .map(v => IR.declVar(
                NAMES.variable(v),
                c.type(v.type),
                v.initialiser && c.expr(v.initialiser),
                v.initialiser === undefined,
            ));
    }
}
