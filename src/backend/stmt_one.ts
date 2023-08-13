///<reference path="../ir/names.ts"/>
///<reference path="../ir/expr.ts"/>

namespace Compiler {
    const {
        NAMES: {
            P,
            ANY,
        },
        OP,
    } = IR;
    
    export class Stmt_BasicOne extends StmtCompiler<ASG.BasicRulesStmt> {
        compile(c: Compiler, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
            const {rewrites} = this.stmt;
            const g = c.grids[this.stmt.inGrid];
            const sampler = g.makeSampler(rewrites.map(rule => rule.from));
            
            const writeConditions = rewrites.map(rule => writeCondition(c, g, rule.to, P, rule.condition));
            
            // optimisation for common case: all rewrites are unconditional and definitely effective
            const allUnconditionalAndEffective = writeConditions.every(c => c === IR.TRUE);
            
            const cases = rewrites.map((rule, i) => IR.block([
                rule.to.kind !== 'expr.constant' ? IR.declVar(P, IR.PATTERN_TYPE, c.expr(rule.to)) : IR.PASS,
                IR.if_(
                    writeConditions[i],
                    doWrite(c, g, rule.from, rule.to, false, allUnconditionalAndEffective ? undefined : ANY, true, c.config.animate),
                ),
            ]));
            
            if(allUnconditionalAndEffective) {
                return IR.if_(
                    sampler.isNotEmpty,
                    IR.block([
                        sampler.sampleWithReplacement(cases),
                        ifChanged,
                    ]),
                    then,
                );
            } else {
                const matches = c.useTempArray(rewrites.length);
                return IR.block([
                    sampler.beginSamplingWithoutReplacement(),
                    matches.declareCount(sampler.count),
                    IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true),
                    IR.while_(
                        OP.and(matches.isNotEmpty, OP.not(ANY)),
                        sampler.sampleWithoutReplacement(cases, matches.count),
                    ),
                    IR.if_(ANY, ifChanged, then),
                ]);
            }
        }
    }
}
