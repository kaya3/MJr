///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
///<reference path="../ir/prng.ts"/>
///<reference path="compiler.ts"/>

namespace Compiler {
    const {
        OP,
    } = IR;
    
    /**
     * A "rule context" is one where the keyword expression `at` may be used.
     */
    export type RuleContext = CompilerContext & {readonly at: IR.Location}
    
    const UNUSED_POSITION: IR.Location = {
        index: IR.unusedExpr('at.index'),
        x: IR.unusedExpr('at.x'),
        y: IR.unusedExpr('at.y'),
    };
    const UNUSED_AT_CONV = IR.unusedExpr('at.conv');
    
    export class CompilerContext {
        public static root(c: Compiler): CompilerContext {
            return new CompilerContext(c, new Map(), undefined, undefined);
        }
        
        private constructor(
            public readonly c: Compiler,
            private readonly variables: ReadonlyMap<number, IR.NameExpr>,
            public readonly at: IR.Location | undefined,
            public readonly atConv: IR.ConstExpr | undefined,
        ) {}
        
        public withVariable(id: number, name: IR.NameExpr): CompilerContext {
            const variables = new Map(this.variables);
            variables.set(id, name);
            return new CompilerContext(this.c, variables, this.at, this.atConv);
        }
        public withPosition(at: IR.Location): RuleContext {
            return new CompilerContext(this.c, this.variables, at, this.atConv) as RuleContext;
        }
        public withConvPosition(at: IR.Location, atConv: IR.ConstExpr): RuleContext {
            return new CompilerContext(this.c, this.variables, at, atConv) as RuleContext;
        }
        
        public expr(expr: ASG.Expression): IR.Expr {
            switch(expr.kind) {
                case 'expr.attr.dict': {
                    return IR.attr(this.expr(expr.left), expr.attr);
                }
                case 'expr.attr.grid': {
                    return this.c.grids[expr.grid].attr(expr.attr);
                }
                case 'expr.attr.position': {
                    const g = this.c.grids[expr.left.type.inGrid];
                    
                    // optimise common cases
                    if(expr.left.kind === 'expr.name.keyword') {
                        switch(expr.left.name) {
                            case 'at':
                                const at = this.at ?? fail();
                                return expr.attr === 'x' ? at.x
                                    : expr.attr === 'y' ? at.y
                                    : fail();
                            case 'origin':
                                return expr.attr === 'x' ? g.originX
                                    : expr.attr === 'y' ? g.originY
                                    : fail();
                        }
                    }
                    
                    const pos = this.expr(expr.left);
                    return expr.attr === 'x' ? OP.mod(pos, g.width)
                        : expr.attr === 'y' ? OP.floordiv(pos, g.width)
                        : fail();
                }
                case 'expr.constant': {
                    return this.compileLiteral(expr.constant, expr.pos);
                }
                case 'expr.count': {
                    return this.c.grids[expr.inGrid].makeCounter(expr.patterns);
                }
                case 'expr.decl': {
                    // TODO: get rid of `expr.decl` in ASG and `expr.letin` in IR, by hoisting assign statements?
                    const {variable, rhs} = expr.decl;
                    if(variable.references === 0) { return this.expr(rhs); }
                    
                    const decl: IR.ConstDecl = this.c.ir.constDecl(
                        variable.name,
                        // need to pass the type, in case the code generator wants to use a lambda requiring a type annotation
                        this.c.type(variable.type),
                        this.expr(rhs),
                    );
                    const ctx = this.withVariable(variable.id, decl.name);
                    return IR.letIn(decl, ctx.expr(expr.child));
                }
                case 'expr.dict': {
                    const type = this.c.dictType(expr.type.entryTypes);
                    return IR.dict(type, type.keys.map(k => this.expr(expr.entryExprs.get(k)!)));
                }
                case 'expr.name.keyword': {
                    switch(expr.name) {
                        case 'at':
                            return (this.at ?? fail()).index;
                        case 'origin':
                            return expr.type.kind === 'position'
                                ? this.c.grids[expr.type.inGrid].origin
                                : fail();
                        case 'random':
                            return this.c.prng.nextDouble();
                    }
                    // exhaustivity check
                    fail(expr satisfies never);
                }
                case 'expr.name.simple': {
                    return this.variables.get(expr.variable.id) ?? fail(`variable not defined in this context`, expr.variable, this);
                }
                case 'expr.op.binary': {
                    const r = IR.binaryOp(expr.op, this.expr(expr.left), this.expr(expr.right));
                    // `IR.binaryOp` may optimise e.g. `0 - x` to `-x`
                    if(r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') { this.c.opsUsed.add(r.op); }
                    return r;
                }
                case 'expr.op.ternary': {
                    return IR.ternary(this.expr(expr.condition), this.expr(expr.then), this.expr(expr.otherwise));
                }
                case 'expr.op.unary': {
                    const r = IR.unaryOp(expr.op, this.expr(expr.child));
                    // `IR.unaryOp` may optimise e.g. `not x == y` to `x != y`
                    if(r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') { this.c.opsUsed.add(r.op); }
                    return r;
                }
                case 'expr.param': {
                    return IR.param(expr.name, this.expr(expr.otherwise));
                }
                case 'expr.randint': {
                    const prng = this.c.prng,
                        max = this.expr(expr.max);
                    return !IR.isInt(max) ? prng.nextIntChecked(max)
                        : max.value > 0 ? prng.nextInt(max)
                        : fail();
                }
                case 'expr.sum': {
                    const g = this.c.grids[expr.inGrid];
                    const p = g.grid.convPatterns.getByID(expr.patternID);
                    return g.makeConvBuffer(p.kernel).get(p, this.atConv ?? fail());
                }
            }
        }
        
        private compileLiteral(constant: Type.ConstantValue, pos: SourcePosition): IR.ConstExpr {
            switch(constant.kind) {
                case 'bool': return constant.value ? IR.TRUE : IR.FALSE;
                case 'float': return IR.float(constant.value);
                case 'int': return IR.int(constant.value);
                case 'str': return IR.str(constant.value);
                
                case 'dict': {
                    const type = this.c.dictType(constant.type.entryTypes);
                    const values = Array.from(type.keys, k => this.compileLiteral(constant.value.get(k)!, pos));
                    return this.c.internConstant(IR.dict(type, values), type);
                }
                case 'fraction': {
                    const expr = OP.fraction(IR.int(constant.value.p), IR.int(constant.value.q));
                    this.c.opsUsed.add(expr.op);
                    return this.c.internConstant(expr, IR.FRACTION_TYPE);
                }
                case 'grid': {
                    return this.c.grids[constant.value].obj;
                }
                case 'pattern.in': {
                    this.c.notSupported('input pattern in non-constant expression', pos);
                    return IR.NULL;
                }
                case 'pattern.out': {
                    const {width, height, pattern} = constant.value;
                    const patternExpr = IR.constArray(pattern, Math.max(...pattern) + 1, width);
                    return this.c.internConstant(IR.libConstructorCall('Pattern', [IR.int(width), IR.int(height), patternExpr]), IR.PATTERN_TYPE);
                }
                case 'position': {
                    const {x, y, inGrid} = constant.value;
                    const g = this.c.grids[inGrid];
                    return this.c.internConstant(g.checkedIndex(IR.int(x), IR.int(y)), IR.INT_TYPE);
                }
            }
        }
        
        public usePattern(expr: ASG.Prop<'pattern.out'>, child: (pattern: IR.PatternResult) => IR.Stmt): IR.Stmt {
            return this.c.ir.withConst('pattern', IR.PATTERN_TYPE, this.expr(expr), pattern => child({
                expr: pattern,
                constant: expr.kind === 'expr.constant' ? expr.constant.value : undefined,
            }));
        }
        
        public isTautology(expr: ASG.Expression): boolean {
            return this.withConvPosition(UNUSED_POSITION, UNUSED_AT_CONV).expr(expr) === IR.TRUE;
        }
    }
}
