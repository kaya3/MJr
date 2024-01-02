///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
///<reference path="../ir/stmt.ts"/>

namespace Compiler {
    const {
        OP,
    } = IR;
    
    export class Stmt_Put extends StmtCompiler<ASG.PutStmt> {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const {stmt} = this;
            const c = ctx.c;
            const g = c.grids[stmt.inGrid];
            const {pattern} = this.stmt;
            
            // check whether pattern is effective
            const isEffective = (at: IR.Location): IR.Expr =>
                pattern.kind === 'expr.constant' ? OP.some(
                    pattern.constant.value,
                    (dx, dy, c) => OP.ne(g.data.get(g.relativeIndex(at, dx, dy)), IR.int(c)),
                )
                : IR.TRUE;
            
            const r = g.declareAtIndex(ctx.expr(stmt.at), at => {
                const ruleCtx = ctx.withPosition(at);
                return IR.seq([
                    // check bounds, given size of pattern
                    g.grid.periodic ? IR.PASS : IR.if_(
                        OP.or(
                            pattern.type.width > 1 ? OP.ge(OP.addConstant(at.x, pattern.type.width - 1), g.width) : IR.FALSE,
                            pattern.type.height > 1 ? OP.ge(OP.addConstant(at.y, pattern.type.height - 1), g.height) : IR.FALSE,
                        ),
                        IR.throw_('pattern would be out of bounds'),
                    ),
                    
                    ruleCtx.usePattern(pattern, p =>
                        IR.if_(
                            OP.and(isEffective(at), writeCondition(ruleCtx, g, undefined, p.constant !== undefined ? undefined : p, stmt.condition)),
                            doWrite(ruleCtx, g, undefined, p, undefined, true, true),
                        ),
                    ),
                    
                    ifFalse,
                ]);
            });
            
            return [r, ctx];
        }
    }
}
