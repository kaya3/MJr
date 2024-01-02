///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>

namespace Compiler {
    const {
        OP,
    } = IR;
    
    export class Stmt_BasicOne extends StmtCompiler<ASG.BasicRulesStmt> {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const {rewrites} = this.stmt;
            const c = ctx.c;
            const g = c.grids[this.stmt.inGrid];
            const sampler = g.makeSampler(rewrites.map(rule => rule.from));
            
            // optimisation for common case: all rewrites are unconditional and definitely effective
            const allUnconditionalAndEffective = rewrites.every(rule =>
                rule.to.kind === 'expr.constant'
                && PatternTree.isDisjoint(rule.from, rule.to.constant.value) && ctx.isTautology(rule.condition),
            );
            
            const buildCases = (at: IR.Location, ifT: IR.Stmt, ifF: IR.Stmt) => {
                const ruleCtx = ctx.withPosition(at);
                return rewrites.map(rule => ruleCtx.usePattern(rule.to, p =>
                    IR.if_(
                        writeCondition(ruleCtx, g, rule.from, p, rule.condition),
                        IR.seq([
                            doWrite(ruleCtx, g, rule.from, p, undefined, true, true),
                            ifT,
                        ]),
                        ifF,
                    ),
                ));
            };
            
            const r = allUnconditionalAndEffective
                ? IR.if_(
                    sampler.isNotEmpty,
                    IR.seq([
                        sampler.sampleWithReplacement(c.prng, at => buildCases(at, IR.PASS, IR.PASS)),
                        ifTrue,
                    ]),
                    ifFalse,
                )
                : sampler.sampleWithoutReplacement(c.prng, buildCases, ifTrue, ifFalse);
            
            return [r, ctx];
        }
    }
}
