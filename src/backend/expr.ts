///<reference path="../ir/names.ts"/>
///<reference path="../ir/expr.ts"/>
///<reference path="../ir/prng.ts"/>

namespace Compiler {
    const {
        NAMES: {
            AT, AT_X, AT_Y,
        },
        OP,
        PRNG,
    } = IR;
    
    export function compileExpr(c: Compiler, expr: ASG.Expression): IR.Expr {
        switch(expr.kind) {
            case 'expr.attr.dict':
                return IR.attr(c.expr(expr.left), expr.attr);
            case 'expr.attr.grid':
                return c.grids[expr.grid].attr(expr.attr);
            case 'expr.attr.position': {
                const g = c.grids[expr.left.type.inGrid];
                
                // optimise common cases
                if(expr.left.kind === 'expr.name.keyword') {
                    switch(expr.left.name) {
                        case 'at':
                            return expr.attr === 'x' ? AT_X
                                : expr.attr === 'y' ? AT_Y
                                : fail();
                        case 'origin':
                            return expr.attr === 'x' ? g.originX
                                : expr.attr === 'y' ? g.originY
                                : fail();
                    }
                }
                
                const pos = c.expr(expr.left);
                return expr.attr === 'x' ? OP.mod(pos, g.width)
                    : expr.attr === 'y' ? OP.floordiv(pos, g.width)
                    : fail();
            }
            case 'expr.constant':
                return compileLiteral(c, expr.constant, expr.pos);
            case 'expr.count':
                return c.grids[expr.inGrid].makeCounter(expr.patterns);
            case 'expr.decl': {
                // TODO: get rid of `expr.decl` in ASG and `expr.letin` in IR, by hoisting assign statements?
                const decls: IR.VarDeclWithInitialiser[] = [];
                let cur: ASG.Expression = expr;
                while(cur.kind === 'expr.decl') {
                    const {variable, rhs} = cur.decl;
                    if(variable.references > 0) {
                        decls.push({
                            name: IR.NAMES.variable(variable),
                            // need to pass the type, in case the code generator wants to use a lambda requiring a type annotation
                            type: c.type(variable.type),
                            initialiser: c.expr(rhs),
                        });
                    }
                    cur = cur.child;
                }
                return IR.letIn(decls, c.expr(cur));
            }
            case 'expr.dict': {
                const type = c.dictType(expr.type.entryTypes);
                return IR.dict(type, type.keys.map(k => c.expr(expr.entryExprs.get(k)!)));
            }
            case 'expr.name.keyword': {
                switch(expr.name) {
                    case 'at':
                        return AT;
                    case 'origin':
                        return expr.type.kind === 'position'
                            ? c.grids[expr.type.inGrid].useOrigin()
                            : fail();
                    case 'random':
                        return PRNG.NEXT_DOUBLE;
                    default:
                        fail();
                }
            }
            case 'expr.name.simple':
                return IR.NAMES.variable(expr.variable);
            case 'expr.op.binary': {
                const r = IR.binaryOp(expr.op, c.expr(expr.left), c.expr(expr.right));
                // `IR.binaryOp` may optimise e.g. `0 - x` to `-x`
                if(r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') { c.opsUsed.add(r.op); }
                return r;
            }
            case 'expr.op.ternary':
                return IR.ternary(c.expr(expr.condition), c.expr(expr.then), c.expr(expr.otherwise));
            case 'expr.op.unary': {
                const r = IR.unaryOp(expr.op, c.expr(expr.child));
                // `IR.unaryOp` may optimise e.g. `not x == y` to `x != y`
                if(r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') { c.opsUsed.add(r.op); }
                return r;
            }
            case 'expr.param':
                return IR.param(expr.name, c.expr(expr.otherwise));
            case 'expr.randint': {
                const max = c.expr(expr.max);
                return !IR.isInt(max) ? PRNG.nextIntChecked(max)
                    : max.value > 0 ? PRNG.nextInt(max)
                    : fail();
            }
            case 'expr.sum': {
                const g = c.grids[expr.inGrid];
                const p = g.grid.convPatterns.getByID(expr.patternID);
                return g.makeConvBuffer(p.kernel).get(p);
            }
        }
    }
    
    function compileLiteral(c: Compiler, constant: Type.ConstantValue, pos: SourcePosition): IR.Expr {
        switch(constant.kind) {
            case 'bool': return constant.value ? IR.TRUE : IR.FALSE;
            case 'float': return IR.float(constant.value);
            case 'int': return IR.int(constant.value);
            case 'str': return IR.str(constant.value);
            
            case 'dict': {
                const type = c.dictType(constant.type.entryTypes);
                const values = Array.from(type.keys, k => compileLiteral(c, constant.value.get(k)!, pos));
                return c.constants.intern(IR.dict(type, values), type);
            }
            case 'fraction': {
                const expr = OP.fraction(IR.int(constant.value.p), IR.int(constant.value.q));
                c.opsUsed.add(expr.op);
                return c.constants.intern(expr, IR.FRACTION_TYPE);
            }
            case 'grid': {
                return c.grids[constant.value].useObj();
            }
            case 'pattern.in': {
                c.notSupported('input pattern in non-constant expression', pos);
                return IR.NULL;
            }
            case 'pattern.out': {
                const {width, height, pattern} = constant.value;
                const patternExpr = IR.constArray(pattern, Math.max(...pattern) + 1, width);
                return c.constants.intern(IR.libConstructorCall('Pattern', [IR.int(width), IR.int(height), patternExpr]), IR.PATTERN_TYPE);
            }
            case 'position': {
                const {x, y, inGrid} = constant.value;
                const g = c.grids[inGrid];
                return c.constants.intern(g.checkedIndex(IR.int(x), IR.int(y)), IR.INT_TYPE);
            }
        }
    }
}
