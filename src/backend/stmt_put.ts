///<reference path="../ir/names.ts"/>
///<reference path="../ir/expr.ts"/>
///<reference path="../ir/stmt.ts"/>

namespace Compiler {
    const {
        NAMES: {
            AT_X, AT_Y,
            P,
        },
        OP,
    } = IR;
    
    export class Stmt_Put extends StmtCompiler<ASG.PutStmt> {
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt): IR.Stmt {
            const {stmt} = this;
            const g = c.grids[stmt.inGrid];
            const {pattern} = this.stmt;
            
            // check whether pattern is effective
            let isEffective: IR.Expr;
            if(pattern.kind === 'expr.constant') {
                const {value} = pattern.constant;
                const checks = value.map((dx, dy, c) => OP.ne(g.data.get(g.relativeIndex(dx, dy)), IR.int(c)));
                isEffective = checks.length > 0 ? checks.reduce(OP.or) : fail();
            } else {
                // non-constant pattern will be checked in _writeCondition
                isEffective = IR.TRUE;
            }
            
            return IR.block([
                g.declareAtIndex(c.expr(stmt.at)),
                
                // check bounds, given size of pattern
                g.grid.periodic ? IR.PASS : IR.if_(
                    OP.or(
                        pattern.type.width > 1 ? OP.ge(OP.addConstant(AT_X, pattern.type.width - 1), g.width) : IR.FALSE,
                        pattern.type.height > 1 ? OP.ge(OP.addConstant(AT_Y, pattern.type.height - 1), g.height) : IR.FALSE,
                    ),
                    IR.throw_('pattern would be out of bounds'),
                ),
                
                pattern.kind !== 'expr.constant' ? IR.declVar(P, IR.PATTERN_TYPE, c.expr(pattern)) : IR.PASS,
                
                IR.if_(
                    OP.and(isEffective, writeCondition(c, g, pattern, P, stmt.condition)),
                    doWrite(c, g, undefined, pattern, false, undefined, true, c.config.animate),
                ),
                ifFalse,
            ]);
        }
    }
}
