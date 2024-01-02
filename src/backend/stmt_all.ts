///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
///<reference path="stmt.ts"/>

namespace Compiler {
    const {
        OP,
    } = IR;
    
    export class Stmt_BasicAllPrl extends StmtCompiler<ASG.BasicRulesStmt> {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const {stmt} = this;
            const {rewrites} = stmt;
            const c = ctx.c;
            const g = c.grids[stmt.inGrid];
            const k = rewrites.length;
            
            if(c.config.animate) {
                ifTrue = IR.seq([g.yield_(), ifTrue]);
            }
            
            const outPatternIsSameEverywhere = rewrites.map(rule => ASG.exprIsSameEverywhere(rule.to));
            
            // TODO: if rule.to is grid-dependent and not same-everywhere, need to do rewrites on a temporary buffer then copy over afterwards
            const patternIsGridDependent = rewrites.map(rule => !ASG.exprIsGridIndependent(rule.to));
            
            rewrites.forEach((rule, i) => {
                if(patternIsGridDependent[i] && !outPatternIsSameEverywhere[i]) {
                    c.notSupported('output pattern dependent on both grid state and match position', rule.pos);
                }
            });
            
            const matches = c.useTempArray(g.grid.scaleX * g.grid.scaleY * k);
            
            const mask = stmt.kind === 'stmt.rules.basic.all' && (!stmt.commutative || rewrites.some(rule => {
                // conservative estimate of whether rewrites can overlap; TODO: check this more accurately
                if(rule.to.kind === 'expr.constant') {
                    const {value} = rule.to.constant;
                    return value.effectiveWidth > 1 || value.effectiveHeight > 1;
                } else {
                    const {type} = rule.to;
                    return type.width > 1 || type.height > 1;
                }
            })) ? c.useMask(g) : undefined;
            
            const shuffle = mask !== undefined || !stmt.commutative;
            
            // tmpPatterns only used when
            const unusedPositionCtx = ctx.withPosition({
                index: IR.unusedExpr('at.index'),
                x: IR.unusedExpr('at.x'),
                y: IR.unusedExpr('at.y'),
            });
            const tmpPatterns = rewrites.map(rule =>
                c.ir.constDecl('p', IR.PATTERN_TYPE, unusedPositionCtx.expr(rule.to))
            );
            
            // conditions which must be checked during writing
            const duringWritesCondition = (ctx: RuleContext, rule: ASG.RewriteRule, p: IR.PatternResult, i: number) =>
                patternHasEffect(ctx, g, rule.from, p);
            
            // if any duringWritesCondition do more than just check the mask, use a flag for whether any rewrites were done
            // but no flag needed if this statement isn't branching anyway
            const flag = (ifTrue !== IR.PASS || ifFalse !== IR.PASS) && rewrites.some(rule => rule.to.kind !== 'expr.constant')
                ? c.ir.flag()
                : undefined;
            
            const out: IR.Stmt[] = [];
            
            // optimisation for common case: all rewrites are unconditional
            const allUnconditional = rewrites.every(rule => ctx.isTautology(rule.condition));
            
            if(allUnconditional) {
                const sampler = g.makeSampler(rewrites.map(rule => rule.from));
                out.push(
                    shuffle ? sampler.shuffleInto(c.prng, matches) : sampler.copyInto(matches),
                    matches.declareCount(sampler.count),
                );
            } else {
                out.push(matches.declareCount(IR.ZERO));
                for(let i = 0; i < k; ++i) {
                    const rule = rewrites[i],
                        sampler = g.makeSampler([rule.from]);
                    
                    out.push(
                        // if condition is same-everywhere, then we only need to check it once for all matches of this rule
                        ASG.exprIsSameEverywhere(rule.condition)
                        ? IR.if_(
                            ctx.expr(rule.condition),
                            shuffle ? sampler.shuffleIntoOffset(c.prng, matches, k, i) : sampler.copyIntoOffset(matches, matches.count, k, i)
                        )
                        // otherwise, need to check the condition separately for each match
                        : sampler.forEach(at => {
                            const match = OP.multAddConstant(at.index, k, IR.int(i));
                            return IR.if_(
                                ctx.withPosition(at).expr(rule.condition),
                                shuffle ? matches.insertShuffled(c.prng, match) : matches.push(match),
                            );
                        })
                    );
                }
            }
            
            const buildCases = (ctx: RuleContext) => rewrites.map((rule, i) => {
                const buildCase = (p: IR.PatternResult) => IR.if_(
                    OP.and(
                        duringWritesCondition(ctx, rule, p, i),
                        mask !== undefined ? mask.patternFits(g, p, ctx.at) : IR.TRUE,
                    ),
                    IR.seq([
                        doWrite(ctx, g, rule.from, p, mask, true, false),
                        flag !== undefined ? flag.set : IR.PASS,
                    ]),
                );
                
                return rule.to.kind === 'expr.constant'
                    ? buildCase({expr: IR.unusedExpr('constant out pattern'), constant: rule.to.constant.value})
                    : outPatternIsSameEverywhere[i]
                    ? buildCase({expr: tmpPatterns[i].name, constant: undefined})
                    : ctx.usePattern(rule.to, buildCase);
            });
            
            const doMain = IR.if_(
                matches.isNotEmpty,
                IR.seq([
                    mask !== undefined ? mask.clear(g) : IR.PASS,
                    c.ir.forRange('i', IR.ZERO, matches.count, i =>
                        c.ir.withConst('match', IR.INT_TYPE, matches.get(i), match =>
                            g.declareAtIndex(OP.divConstant(match, k), at =>
                                IR.switch_(OP.modConstant(match, k), buildCases(ctx.withPosition(at))),
                            ),
                        ),
                    ),
                    flag !== undefined ? IR.PASS : ifTrue,
                ]),
                flag !== undefined ? undefined : ifFalse,
            );
            out.push(flag !== undefined
                ? IR.withDecl(flag.decl, IR.seq([
                    doMain,
                    IR.if_(flag.check, ifTrue, ifFalse),
                ]))
                : doMain,
            );
            return [
                IR.withDecls(tmpPatterns, IR.seq(out)),
                ctx,
            ];
        }
    }
}
