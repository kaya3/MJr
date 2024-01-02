///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>

namespace Compiler {
    const {
        OP,
    } = IR;
    
    export function doWrite(ctx: RuleContext, outGrid: IR.Grid, from: PatternTree | undefined, to: IR.PatternResult, mask: IR.Mask | undefined, doUpdate: boolean, doYield: boolean): IR.Stmt {
        const out: IR.Stmt[] = [];
        let mX: IR.Expr, mY: IR.Expr, eW: IR.Expr, eH: IR.Expr;
        
        if(to.constant !== undefined) {
            // constant output pattern
            const value = to.constant;
            
            value.forEach((dx, dy, colour) => {
                // TODO: `[ABC] or [ADE] -> [A..]` isn't maybe effective in the first position
                const maybeEffective = from === undefined || from.kind !== 'leaf' || from.pattern[dx + from.width * dy] !== colour;
                if(mask !== undefined || maybeEffective) {
                    out.push(outGrid.write(
                        outGrid.relativeIndex(ctx.at, dx, dy),
                        IR.int(colour),
                        mask,
                    ));
                }
            });
            
            mX = IR.int(value.minX);
            mY = IR.int(value.minY);
            eW = IR.int(value.effectiveWidth);
            eH = IR.int(value.effectiveHeight);
        } else {
            // output pattern determined at runtime
            out.push(IR.libMethodCallStmt(
                'Pattern', 'put', to.expr,
                [outGrid.obj, mask !== undefined ? mask.arr.array : IR.NULL, ctx.at.x, ctx.at.y],
            ));
            
            mX = IR.attr(to.expr, 'minX');
            mY = IR.attr(to.expr, 'minY');
            eW = IR.attr(to.expr, 'effectiveWidth');
            eH = IR.attr(to.expr, 'effectiveHeight');
        }
        
        if(doUpdate) {
            const x = OP.add(ctx.at.x, mX),
                y = OP.add(ctx.at.y, mY);
            out.push(outGrid.update(x, y, eW, eH));
            if(doYield && ctx.c.config.animate) {
                out.push(outGrid.yieldRewriteInfo(x, y, eW, eH));
            }
        }
        return IR.seq(out);
    }
    
    export function patternHasEffect(ctx: RuleContext, g: IR.Grid, from: PatternTree | undefined, to: IR.PatternResult): IR.Expr {
        return to.constant === undefined
            ? IR.libMethodCall('Pattern', 'hasEffect', to.expr, [g.obj, ctx.at.x, ctx.at.y])
            : from !== undefined && PatternTree.isDisjoint(from, to.constant)
            ? IR.TRUE
            : OP.some(to.constant, (dx, dy, c) =>
                from === undefined || !PatternTree.matches(from, p => !p.canBe(dx, dy, c))
                    ? OP.ne(g.data.get(g.relativeIndex(ctx.at, dx, dy)), IR.int(c))
                    : IR.TRUE
            );
    }
    
    export function writeCondition(ctx: RuleContext, g: IR.Grid, from: PatternTree | undefined, to: IR.PatternResult | undefined, conditionExpr: ASG.Prop<'bool?'>): IR.Expr {
        return OP.and(
            to !== undefined ? patternHasEffect(ctx, g, from, to) : IR.TRUE,
            conditionExpr !== undefined ? ctx.expr(conditionExpr) : IR.TRUE,
        );
    }
}
