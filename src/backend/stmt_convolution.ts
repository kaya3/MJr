///<reference path="../ir/factory.ts"/>

namespace Compiler {
    export class Stmt_Convolution extends StmtCompiler<ASG.ConvolutionStmt> {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const {stmt} = this;
            const c = ctx.c;
            const g = c.grids[stmt.inGrid];
            const buffer = g.makeConvBuffer(stmt.kernel);
            
            const flag = c.ir.flag();
            
            function buildCases(at: IR.Location, atConv: IR.ConstExpr): readonly IR.Stmt[] {
                const convCtx = ctx.withConvPosition(at, atConv);
                
                const cases: IR.Stmt[] = emptyArray(g.grid.alphabet.key.length, IR.PASS);
                for(const rule of stmt.rewrites) {
                    const mask = PatternTree.isLeafOrTop(rule.from) ? rule.from.masks[0] : fail();
                    
                    const caseHandler = convCtx.usePattern(rule.to, p =>
                        IR.if_(
                            writeCondition(convCtx, g, rule.from, p, rule.condition),
                            IR.seq([
                                doWrite(convCtx, g, rule.from, p, undefined, false, false),
                                flag.set,
                            ]),
                        ),
                    );
                    ISet.forEach(mask, i => {
                        if(cases[i] !== IR.PASS) { fail(); }
                        cases[i] = caseHandler;
                    });
                }
                return cases;
            }
            
            // TODO: different strategy if rules don't cover the whole alphabet?
            const r = IR.withDecl(flag.decl, IR.seq([
                c.ir.forRange('y', IR.ZERO, g.height, y =>
                    c.ir.forRange('x', IR.ZERO, g.width, x =>
                        g.declareAtXY(x, y, at =>
                            c.ir.withConst('atConv', IR.INT_TYPE, buffer.index(x, y), atConv =>
                                IR.switch_(g.data.get(at.index), buildCases(at, atConv)),
                            ),
                        ),
                    ),
                ),
                IR.if_(
                    flag.check,
                    IR.seq([
                        // TODO: this is suboptimal, but need to defer update until all `sum` expressions are evaluated
                        g.update(IR.ZERO, IR.ZERO, g.width, g.height),
                        c.config.animate ? g.yield_() : IR.PASS,
                        ifTrue,
                    ]),
                    ifFalse,
                ),
            ]));
            
            return [r, ctx];
        }
    }
}
