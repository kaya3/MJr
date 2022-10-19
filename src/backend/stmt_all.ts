///<reference path="../ir/names.ts"/>
///<reference path="../ir/ops.ts"/>

namespace Compiler {
    const {
        NAMES: {
            AT,
            I, P,
            MATCH, ANY,
        },
        OP,
    } = IR;
    
    function _exprIs(expr: ASG.Expression, flags: ExprFlags): boolean {
        return (expr.flags & flags) === flags;
    }
    
    export class Stmt_BasicAllPrl implements StmtCompiler {
        constructor(readonly stmt: ASG.BasicRulesStmt) {}
        
        compile(c: Compiler, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
            const {stmt} = this;
            const {rewrites} = stmt;
            const g = c.grids[stmt.inGrid];
            const k = rewrites.length;
            const out: IR.Stmt[] = [];
            
            const conditionIsSameEverywhere = rewrites.map(rule =>
                _exprIs(rule.condition, ExprFlags.SAME_EVERYWHERE)
                && rule.to.kind === 'expr.constant'
            );
            const outPatternIsConstant = rewrites.map(rule => rule.to.kind === 'expr.constant');
            const outPatternIsSameEverywhere = rewrites.map(rule => _exprIs(rule.to, ExprFlags.SAME_EVERYWHERE));
            
            // TODO: if rule.to is grid-dependent and not same-everywhere, need to do rewrites on a temporary buffer then copy over afterwards
            const patternIsGridDependent = rewrites.map(rule => !_exprIs(rule.to, ExprFlags.GRID_INDEPENDENT));
            rewrites.forEach((rule, i) => {
                if(patternIsGridDependent[i] && !outPatternIsSameEverywhere[i]) {
                    c.notSupported('output pattern dependent on both grid state and match position', rule.pos);
                }
            });
            
            c.matches.use(g, k);
            const useMask = stmt.kind === 'stmt.rules.basic.all' && (!stmt.commutative || rewrites.some(rule => {
                if(rule.to.kind === 'expr.constant') {
                    const {value} = rule.to.constant;
                    return value.effectiveWidth > 1 || value.effectiveHeight > 1;
                } else {
                    const {type} = rule.to;
                    return type.width > 1 || type.height > 1;
                }
            }));
            if(useMask) { c.mask.use(g); }
            const shuffle = useMask || !stmt.commutative;
            
            out.push(IR.declVars(rewrites.flatMap((rule, i) =>
                !outPatternIsConstant[i] && outPatternIsSameEverywhere[i]
                ? [{name: IR.NAMES.tmpPattern(i), type: IR.PATTERN_TYPE, initialiser: c.expr(rule.to)}]
                : []
            )));
            
            const firstPassConditions = rewrites.map((rule, i) => writeCondition(
                c,
                g,
                outPatternIsSameEverywhere[i] ? rule.to : undefined,
                IR.NAMES.tmpPattern(i),
                rule.condition,
            ));
            const secondPassConditions = rewrites.map((rule, i) => writeCondition(
                c,
                g,
                outPatternIsSameEverywhere[i] ? undefined : rule.to,
                P,
                undefined,
            ));
            
            // if any second-pass conditions do more than just check the mask, use a flag for whether any rewrites were done
            // but no flag needed if this statement isn't branching anyway
            const useFlag = (ifChanged !== IR.PASS || then !== IR.PASS) && outPatternIsSameEverywhere.includes(false);
            
            // optimisation for common case: all rewrites are unconditional and definitely effective
            if(firstPassConditions.every(c => c === IR.TRUE)) {
                const sampler = g.makeSampler(rewrites.map(rule => rule.from));
                if(shuffle) {
                    out.push(
                        c.matches.declareCount(IR.ZERO, true),
                        sampler.shuffleInto(c.matches),
                    );
                } else {
                    out.push(
                        sampler.copyInto(c.matches.array),
                        c.matches.declareCount(sampler.count, true),
                    );
                }
            } else {
                out.push(c.matches.declareCount(IR.ZERO, true));
                for(let i = 0; i < k; ++i) {
                    const rule = rewrites[i];
                    const sampler = g.makeSampler([rule.from]);
                    const condition = firstPassConditions[i];
                    const match = OP.multAddConstant(AT, k, IR.int(i));
                    
                    out.push(
                        // if condition is same-everywhere, then we only need to check it once for all matches of this rule
                        conditionIsSameEverywhere[i]
                        ? IR.if_(condition, shuffle
                            ? sampler.shuffleIntoOffset(c.matches, k, i)
                            : sampler.copyIntoOffset(c.matches.array, c.matches.count, k, i)
                        )
                        // otherwise, need to check the condition separately for each match
                        : sampler.forEach([IR.if_(condition, IR.block(shuffle ? c.matches.insertShuffled(match) : c.matches.push(match)))])
                    );
                }
            }
            
            const doWrites = rewrites.map((rule, i) => IR.block([
                outPatternIsConstant[i] ? IR.PASS : IR.declVar(
                    P,
                    IR.PATTERN_TYPE,
                    // TODO: `c.expr(rule.to)` is only correct when `rule.to` is grid-independent (see above)
                    outPatternIsSameEverywhere[i] ? IR.NAMES.tmpPattern(i) : c.expr(rule.to),
                ),
                IR.if_(
                    OP.and(
                        secondPassConditions[i],
                        useMask ? c.mask.patternFits(g, rule.to) : IR.TRUE,
                    ),
                    doWrite(c, g, rule.from, rule.to, useMask, useFlag ? ANY : undefined, true, false),
                ),
            ]));
            
            if(c.config.animate) {
                ifChanged = IR.block([g.yield_(), ifChanged]);
            }
            
            out.push(
                useFlag ? IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true) : IR.PASS,
                IR.if_(
                    c.matches.isNotEmpty,
                    IR.block([
                        useMask ? c.mask.clear(g) : IR.PASS,
                        IR.forRange(I, IR.ZERO, c.matches.count, [
                            IR.declVar(MATCH, IR.INT_TYPE, c.matches.get(I)),
                            g.declareAtIndex(OP.divConstant(MATCH, k)),
                            IR.switch_(OP.modConstant(MATCH, k), doWrites),
                        ]),
                        useFlag ? IR.PASS : ifChanged,
                    ]),
                    useFlag ? undefined : then,
                ),
                useFlag ? IR.if_(ANY, ifChanged, then) : IR.PASS,
            );
            return IR.block(out);
        }
    }
}
