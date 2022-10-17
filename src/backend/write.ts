///<reference path="../ir/names.ts"/>
///<reference path="../ir/ops.ts"/>

namespace Compiler {
    const {
        NAMES: {
            AT_X, AT_Y,
            P,
        },
        OP,
    } = IR;
    
    export function doWrite(c: Compiler, outGrid: IR.Grid, from: PatternTree | undefined, to: ASG.Prop<'pattern.out'>, useMask: boolean, flagVar: IR.NameExpr | undefined, doUpdate: boolean, doYield: boolean): IR.Stmt {
        const out: IR.Stmt[] = [];
        let mX: IR.Expr, mY: IR.Expr, eW: IR.Expr, eH: IR.Expr;
        
        if(to.kind === 'expr.constant') {
            // constant output pattern
            const {value} = to.constant;
            
            let minX = value.width, maxX = 0, minY = value.height, maxY = 0;
            value.forEach((dx, dy, colour) => {
                const maybeEffective = from === undefined || from.kind !== 'leaf' || from.pattern[dx + from.width * dy] !== colour;
                if(useMask || maybeEffective) {
                    out.push(outGrid.write(
                        outGrid.relativeIndex(dx, dy),
                        IR.int(colour),
                        useMask ? c.mask : undefined,
                    ));
                }
                if(maybeEffective) {
                    minX = Math.min(minX, dx);
                    maxX = Math.max(maxX, dx + 1);
                    minY = Math.min(minY, dy);
                    maxY = Math.max(maxY, dy + 1);
                }
            });
            
            if(minX > maxX || minY > maxY) { fail(); }
            
            mX = IR.int(minX);
            mY = IR.int(minY);
            eW = IR.int(maxX - minX);
            eH = IR.int(maxY - minY);
        } else {
            // output pattern determined at runtime
            // this requires that `p` is already declared
            out.push(IR.libMethodCallStmt(
                'Pattern', 'put', P,
                [outGrid.useObj(), useMask ? c.mask.name : IR.NULL, AT_X, AT_Y],
            ));
            
            mX = IR.attr(P, 'minX');
            mY = IR.attr(P, 'minY');
            eW = IR.attr(P, 'effectiveWidth');
            eH = IR.attr(P, 'effectiveHeight');
        }
        
        if(doUpdate) {
            const x = OP.add(AT_X, mX), y = OP.add(AT_Y, mY);
            out.push(outGrid.update(x, y, eW, eH));
            if(doYield) { out.push(outGrid.yieldRewriteInfo(x, y, eW, eH)); }
        }
        if(flagVar !== undefined) {
            out.push(IR.assign(flagVar, '=', IR.TRUE));
        }
        return IR.block(out);
    }
    
    export function writeCondition(c: Compiler, g: IR.Grid, patternExpr: ASG.Prop<'pattern.out'> | undefined, patternVar: IR.NameExpr | undefined, conditionExpr: ASG.Prop<'bool?'>): IR.Expr {
        const hasEffect
            = patternExpr === undefined || patternExpr.kind === 'expr.constant'
            ? IR.TRUE
            : IR.libMethodCall('Pattern', 'hasEffect', patternVar ?? fail(), [g.useObj(), AT_X, AT_Y]);
        return conditionExpr === undefined ? hasEffect : OP.and(hasEffect, c.expr(conditionExpr));
    }
}
