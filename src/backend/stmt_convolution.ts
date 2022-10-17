///<reference path="../ir/names.ts"/>

namespace Compiler {
    const {
        NAMES: {
            AT, AT_X, AT_Y, AT_CONV,
            P,
            ANY,
        },
    } = IR;
    
    export class Stmt_Convolution implements StmtCompiler {
        constructor(
            readonly stmt: ASG.ConvolutionStmt,
            readonly ifChanged: IR.Stmt,
            readonly then: IR.Stmt,
        ) {}
        
        compile(c: Compiler): IR.Stmt {
            const {stmt} = this;
            const g = c.grids[stmt.inGrid];
            const buffer = g.makeConvBuffer(stmt.kernel);
            
            const cases: IR.Stmt[] = emptyArray(g.grid.alphabet.key.length, IR.PASS);
            for(const rule of stmt.rewrites) {
                const mask = PatternTree.isLeafOrTop(rule.from) ? rule.from.masks[0] : fail();
                
                const caseHandler = IR.block([
                    rule.to.kind === 'expr.constant' ? IR.PASS : IR.declVar(P, IR.PATTERN_TYPE, c.expr(rule.to)),
                    IR.if_(
                        writeCondition(c, g, rule.to, P, rule.condition),
                        doWrite(c, g, rule.from, rule.to, false, ANY, false, false),
                    ),
                ]);
                ISet.forEach(mask, i => {
                    if(cases[i] !== IR.PASS) { fail(); }
                    cases[i] = caseHandler;
                });
            }
            
            // TODO: different strategy if rules don't cover the whole alphabet?
            return IR.block([
                IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true),
                IR.forRange(AT_Y, IR.ZERO, g.height, [IR.forRange(AT_X, IR.ZERO, g.width, [
                    g.declareAtXY(AT_X, AT_Y),
                    IR.declVar(AT_CONV, IR.INT_TYPE, buffer.index(AT_X, AT_Y)),
                    IR.switch_(g.data.get(AT), cases),
                ])]),
                IR.if_(
                    ANY,
                    IR.block([
                        // TODO: this is suboptimal, but need to defer update until all `sum` expressions are evaluated
                        g.update(IR.ZERO, IR.ZERO, g.width, g.height),
                        c.config.animate ? g.yield_() : IR.PASS,
                        this.ifChanged,
                    ]),
                    this.then,
                ),
            ]);
        }
    }
}
