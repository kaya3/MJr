///<reference path="../ir/ir.ts"/>
///<reference path="../ir/names.ts"/>
///<reference path="../ir/ops.ts"/>
///<reference path="../ir/prng.ts"/>

/**
 * Compiles MJr source code to a specified target language.
 */
namespace Compiler {
    export const COMPILER_VERSION = '0.1 (unstable)';
    export const REQUIRED_RUNTIME_LIB_VERSION = 0;
    
    export type Config = typeof DEFAULT_CONFIG
    const DEFAULT_CONFIG = {
        indentSpaces: 4,
        emitComments: true,
        emitChecks: true,
        entryPointName: 'main',
        maxIterations: 0,
        animate: false,
    };
    
    const ALWAYS_USED_OPS: readonly IR.Op[] = [
        'bool_and', 'bool_or', 'bool_not',
        'int_eq', 'int_ne', 'int_lt', 'int_le', 'int_gt', 'int_ge',
        'int_and', 'int_or', 'int_xor', 'int_not', 'int_lshift', 'int_rshift', 'int_ctz',
        'loose_int_plus', 'loose_int_mult', 'loose_int_floordiv', 'loose_int_mod',
    ];
    
    // helpers
    const {
        NAMES: {
            WIDTH, HEIGHT, PARAMS, RNG,
            STATE, ITERATIONS,
            AT, AT_X, AT_Y, AT_CONV,
            I, P,
            S, OLD_S, T, OLD_T,
            ANY, MATCH,
        },
        OP,
        PRNG,
    } = IR;
    
    /**
     * Compiles MJr source code to a high-level intermediate representation.
     */
    class Compiler implements IR.IRCompiler {
        readonly diagnostics = new Diagnostics();
        
        private readonly cfg: CFG.CFG;
        private readonly switchNodes: ReadonlyIDMap<CFG.SwitchNode>;
        readonly opsUsed = new Set(ALWAYS_USED_OPS);
        private readonly internedLiterals = IDMap.withKey<{expr: IR.Expr, type: IR.IRType}>(entry => IR.key(entry.expr));
        
        readonly grids: readonly IR.Grid[];
        readonly mask = new IR.Mask();
        private readonly flags: IR.Flags;
        private readonly limits: IR.Limits;
        readonly matches = new IR.MatchesArray();
        readonly variables: IR.Variables;
        
        constructor(
            readonly asg: ASG.ASG,
            readonly config: Readonly<Config>,
        ) {
            this.cfg = CFG.build(asg.root, config.animate);
            this.switchNodes = IDMap.ofWithKey(this.cfg.nodes.filter(CFG.isSwitchNode), node => node.id);
            this.grids = asg.grids.map(g => new IR.Grid(g));
            this.flags = new IR.Flags(this.cfg.numFlags);
            this.limits = new IR.Limits(asg.limits);
            this.variables = new IR.Variables(asg.variables);
        }
        
        private internLiteral(expr: IR.Expr, type: IR.IRType): IR.Expr {
            const id = this.internedLiterals.getOrCreateID({expr, type});
            return IR.NAMES.constant(id);
        }
        
        notSupported(s: string, pos: SourcePosition): void {
            this.diagnostics.compilationError(`${s} is not currently supported`, pos);
        }
        
        compile(): IR.Stmt {
            // YYYY-MM-DD HH:mm:ss GMT+offset
            const date = new Date().toLocaleString('en-SE', {timeZoneName: 'short'});
            
            const {params, endGridID} = this.asg;
            
            const switchCases = this.switchNodes.map(node =>
                node.kind === 'stmt.branching' ? this.compileBranchingStmtNode(node)
                : node.kind === 'stmt.nonbranching' ? this.compileNonBranchingStmtNode(node)
                : node.kind === 'reset' ? this.compileResetNode(node)
                : fail()
            );
            
            // TODO: potentials
            
            const gridDecls: IR.Stmt[] = [];
            const gridUpdateDecls: IR.Stmt[] = [];
            const endGridObj = this.grids[endGridID].useObj();
            for(const g of this.grids) {
                if(g.grid.periodic) { this.notSupported('periodic grid', g.grid.pos); }
                
                gridDecls.push(...g.declareVars());
                gridUpdateDecls.push(...g.matcher.declareUpdateFunc());
            }
            
            const mainParams: IR.NameExpr[] = [WIDTH, HEIGHT];
            const mainParamTypes: IR.IRType[] = [IR.INT_TYPE, IR.INT_TYPE];
            if(params.size > 0) {
                mainParams.push(PARAMS);
                mainParamTypes.push(IR.nullableType({
                    kind: 'dict',
                    keys: Array.from(params.keys()),
                    values: Array.from(params.values(), t => IR.nullableType(this.type(t))),
                }));
            }
            mainParams.push(RNG);
            mainParamTypes.push(IR.nullableType(IR.PRNG_TYPE));
            
            // need to compile everything before preamble, so that `maxScale` and `this.opsUsed` are correct
            const matchesDecl = this.matches.declare(),
                maskDecl = this.mask.declare(),
                constDecls = IR.declVars(this.internedLiterals.map((s, i) => ({name: IR.NAMES.constant(i), type: s.type, initialiser: s.expr}))),
                varDecls = this.variables.declare(this),
                flagDecls = this.flags.declare(),
                limitDecls = this.limits.declare(this);
            
            // compute maximum grid dimensions, to ensure that arrays aren't over-allocated and loose int operations don't overflow
            // 0x3FFFFFFE is the magic number for the largest allowed array length for a LFSR
            // don't need to include mask.scale here; mask array length is at most 1/32 of any grid array length
            const maxScale = Math.max(
                this.matches.scale,
                ...this.grids.map(g => g.getScale()),
            );
            const maxDim = IR.int(Math.sqrt(0x3FFFFFFE / maxScale) | 0);
            const {maxIterations} = this.config;
            
            return IR.declFunc(IR.nameExpr(this.config.entryPointName), this.config.animate ? IR.REWRITE_INFO_TYPE : undefined, mainParams, mainParamTypes, IR.GRID_TYPE, IR.block([
                IR.comment(`compiled by mjrc-${COMPILER_VERSION} on ${date}`),
                this.config.emitChecks ? IR.if_(
                    OP.or(OP.le(WIDTH, IR.ZERO), OP.le(HEIGHT, IR.ZERO)),
                    IR.throw_('Grid dimensions must be positive'),
                    IR.if_(
                        OP.or(OP.gt(WIDTH, maxDim), OP.gt(HEIGHT, maxDim)),
                        IR.throw_(`Grid dimensions cannot exceed ${maxDim.value}`),
                    ),
                ) : IR.PASS,
                
                IR.preamble(this.dictType(params), this.config.emitChecks, REQUIRED_RUNTIME_LIB_VERSION, Array.from(this.opsUsed)),
                IR.BLANK_LINE,
                
                ...gridDecls,
                IR.BLANK_LINE,
                
                ...gridUpdateDecls,
                
                matchesDecl,
                ...maskDecl,
                constDecls,
                ...varDecls,
                flagDecls,
                limitDecls,
                maxIterations !== 0 ? IR.declVar(ITERATIONS, IR.INT_TYPE, IR.ZERO, true) : IR.PASS,
                ...this.goto(-1, 0),
                IR.while_(
                    OP.ge(STATE, IR.ZERO),
                    IR.block([
                        maxIterations !== 0 ? IR.if_(
                            OP.ge(ITERATIONS, IR.int(maxIterations)),
                            IR.throw_(`Exceeded maximum of ${maxIterations} iterations`),
                        ) : IR.PASS,
                        IR.switch_(STATE, switchCases),
                        maxIterations !== 0 ? IR.assign(ITERATIONS, '+=', IR.ONE) : IR.PASS,
                    ]),
                ),
                IR.return_(endGridObj),
            ]));
        }
        
        private compileBranchingStmtNode(node: CFG.BranchingStmtNode): IR.Stmt {
            const {stmt} = node;
            
            const ifTrue = this.goto(node.id, node.ifChanged.nodeID),
                ifFalse = this.goto(node.id, node.then.nodeID),
                eitherWay: IR.Stmt[] = [];
            while(ifTrue.length > 0 && ifFalse.length > 0 && IR.equals(ifTrue[ifTrue.length - 1], ifFalse[ifFalse.length - 1])) {
                const s = ifTrue.pop()!;
                ifFalse.pop();
                eitherWay.push(s);
            }
            
            const f = STMT_COMPILE_FUNCS[stmt.kind];
            return IR.block([
                IR.comment(`${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                f(this, stmt as never, IR.block(ifTrue), IR.block(ifFalse)),
                ...eitherWay,
            ]);
        }
        
        private compileNonBranchingStmtNode(node: CFG.NonBranchingStmtNode): IR.Stmt {
            const {stmt} = node;
            const f = STMT_COMPILE_FUNCS[stmt.kind];
            return IR.block([
                IR.comment(`${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                f(this, stmt as never),
                ...this.goto(node.id, node.then.nodeID),
            ]);
        }
        
        private compileResetNode(node: CFG.ResetNode): IR.Stmt {
            const out: IR.Stmt[] = [IR.comment(`reset ${node.stmt.kind} at line ${node.stmt.pos.line}, col ${node.stmt.pos.col}`)];
            if(node.reset !== undefined) {
                const {limitIDs} = node.reset;
                out.push(...limitIDs.map(limitID => this.limits.reset(limitID, this)));
            }
            out.push(
                this.flags.clear(node.flagID),
                ...this.goto(node.id, node.then.nodeID),
            );
            return IR.block(out);
        }
        
        expr(expr: ASG.Expression): IR.Expr {
            const f = EXPR_COMPILE_FUNCS[expr.kind] as ExprCompileFunc<typeof expr['kind']>
            return f(this, expr);
        }
        
        literal(c: Type.ConstantValue, pos: SourcePosition): IR.Expr {
            switch(c.kind) {
                case 'bool': return c.value ? IR.TRUE : IR.FALSE;
                case 'float': return IR.float(c.value);
                case 'int': return IR.int(c.value);
                case 'str': return IR.str(c.value);
                
                case 'dict': {
                    const type = this.dictType(c.type.entryTypes);
                    const values = Array.from(type.keys, k => this.literal(c.value.get(k)!, pos));
                    return this.internLiteral(IR.dict(type, values), type);
                }
                case 'fraction': {
                    const expr = OP.fraction(IR.int(c.value.p), IR.int(c.value.q));
                    this.opsUsed.add(expr.op);
                    return this.internLiteral(expr, IR.FRACTION_TYPE);
                }
                case 'grid': {
                    return this.grids[c.value].useObj();
                }
                case 'pattern.in': {
                    this.notSupported('input pattern in non-constant expression', pos);
                    return IR.NULL;
                }
                case 'pattern.out': {
                    const {width, height, pattern} = c.value;
                    const patternExpr = IR.constArray(pattern, Math.max(...pattern) + 1, width);
                    return this.internLiteral(IR.libConstructorCall('Pattern', [IR.int(width), IR.int(height), patternExpr]), IR.PATTERN_TYPE);
                }
                case 'position': {
                    const {x, y, inGrid} = c.value;
                    const g = this.grids[inGrid];
                    return this.internLiteral(g.checkedIndex(IR.int(x), IR.int(y)), IR.INT_TYPE);
                }
            }
        }
        
        type(type: Type.Type): IR.IRType {
            return type.kind === 'dict' ? this.dictType(type.entryTypes)
                : type.kind !== 'pattern.in' ? TYPES_TO_IR[type.kind]
                : fail();
        }
        dictType(entryTypes: ReadonlyMap<string, Type.Type>): IR.DictType {
            const keys = Array.from(entryTypes.keys()).sort();
            // TODO: declare type aliases for dict types
            return {kind: 'dict', keys, values: keys.map(k => this.type(entryTypes.get(k)!))};
        }
        
        goto(fromNodeID: number, toNodeID: number): IR.Stmt[] {
            const initial = fromNodeID < 0;
            
            const out: IR.Stmt[] = [];
            const {nodes} = this.cfg;
            let cur: CFG.Node | undefined;
            while(true) {
                cur = nodes[toNodeID];
                if(cur.kind === 'stmt.branching' || cur.kind === 'stmt.nonbranching' || (cur.kind === 'reset' && cur.reset !== undefined) || cur.kind === 'stop') {
                    break;
                } else if(cur.kind === 'checklimit') {
                    if(initial) { toNodeID = cur.ifTrue.nodeID; continue; }
                    break;
                } else if(cur.kind === 'checkflag' || cur.kind === 'reset') {
                    if(initial) { toNodeID = cur.then.nodeID; continue; }
                    break;
                } else if(cur.kind === 'decrementlimit') {
                    if(initial) { fail(); }
                    out.push(this.limits.decrement(cur.limitID));
                } else if(cur.kind === 'setflag') {
                    if(initial) { fail(); }
                    out.push(this.flags.set(cur.flagID));
                }
                toNodeID = cur.then.nodeID;
            }
            
            if(cur.kind === 'checkflag' || cur.kind === 'checklimit') {
                if(initial) { fail(); }
                
                out.push(IR.if_(
                    cur.kind === 'checkflag' ? this.flags.check(cur.flagID) : this.limits.check(cur.limitID),
                    IR.block(this.goto(fromNodeID, cur.ifTrue.nodeID)),
                    IR.block(this.goto(fromNodeID, cur.then.nodeID)),
                ));
            } else {
                const nextState = IR.int(cur.kind === 'stop' ? -1 : this.switchNodes.getID(cur));
                if(initial) {
                    out.push(IR.declVar(STATE, IR.INT_TYPE, nextState, true));
                } else if(fromNodeID !== toNodeID) {
                    out.push(IR.assign(STATE, '=', nextState));
                }
            }
            return out;
        }
    }
    
    const TYPES_TO_IR: IRecord<Exclude<Type.Kind, 'dict' | 'pattern.in'>, IR.IRType> = {
        bool: IR.BOOL_TYPE,
        float: IR.FLOAT_TYPE,
        fraction: IR.FRACTION_TYPE,
        grid: IR.GRID_TYPE,
        int: IR.INT_TYPE,
        'pattern.out': IR.PATTERN_TYPE,
        position: IR.INT_TYPE,
        str: IR.STR_TYPE,
    };
    
    type ExprCompileFunc<K extends ASG.Expression['kind']> = (c: Compiler, expr: Extract<ASG.Expression, {readonly kind: K}>) => IR.Expr
    const EXPR_COMPILE_FUNCS: {readonly [K in ASG.Expression['kind']]: ExprCompileFunc<K>} = {
        'expr.attr.dict': (c, expr) => IR.attr(c.expr(expr.left), expr.attr),
        'expr.attr.grid': (c, expr) => c.grids[expr.grid].attr(expr.attr),
        'expr.attr.position': (c, expr) => {
            const g = c.grids[expr.left.type.inGrid];
            
            // optimise common cases
            if(expr.left.kind === 'expr.name.keyword') {
                switch(expr.left.name) {
                    case 'at':
                        return {x: AT_X, y: AT_Y}[expr.attr];
                    case 'origin':
                        return {x: g.originX, y: g.originY}[expr.attr];
                }
            }
            
            const pos = c.expr(expr.left);
            switch(expr.attr) {
                case 'x': return OP.mod(pos, g.width);
                case 'y': return OP.floordiv(pos, g.width);
            }
            // exhaustivity check
            const _: never = expr.attr;
        },
        'expr.constant': (c, expr) => c.literal(expr.constant, expr.pos),
        'expr.count': (c, expr) => c.grids[expr.inGrid].makeCounter(expr.patterns),
        'expr.decl': (c, expr) => {
            // TODO: get rid of `expr.decl` in ASG and `expr.letin` in IR, by hoisting assign statements?
            const decls: IR.VarDeclWithInitialiser[] = [];
            let cur: ASG.Expression = expr;
            while(cur.kind === 'expr.decl') {
                const {variable, rhs} = cur.decl;
                if(variable.references > 0) {
                    decls.push({
                        name: c.variables.name(variable.id),
                        // need to pass the type, in case the code generator wants to use a lambda requiring a type annotation
                        type: c.variables.type(variable.id, c),
                        initialiser: c.expr(rhs),
                    });
                }
                cur = cur.child;
            }
            return IR.letIn(decls, c.expr(cur));
        },
        'expr.dict': (c, expr) => {
            const type = c.dictType(expr.type.entryTypes);
            return IR.dict(type, type.keys.map(k => c.expr(expr.entryExprs.get(k)!)));
        },
        'expr.name.keyword': (c, expr) => {
            switch(expr.name) {
                case 'at':
                    return AT;
                case 'origin':
                    return expr.type.kind === 'position'
                        ? c.grids[expr.type.inGrid].useOrigin()
                        : fail();
                case 'random':
                    return PRNG.NEXT_DOUBLE;
            }
        },
        'expr.name.simple': (c, expr) => c.variables.name(expr.variableID),
        'expr.op.binary': (c, expr) => {
            const r = IR.binaryOp(expr.op, c.expr(expr.left), c.expr(expr.right));
            // `IR.binaryOp` may optimise e.g. `0 - x` to `-x`
            if(r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') { c.opsUsed.add(r.op); }
            return r;
        },
        'expr.op.ternary': (c, expr) => IR.ternary(c.expr(expr.condition), c.expr(expr.then), c.expr(expr.otherwise)),
        'expr.op.unary': (c, expr) => {
            const r = IR.unaryOp(expr.op, c.expr(expr.child));
            // `IR.unaryOp` may optimise e.g. `not x == y` to `x != y`
            if(r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') { c.opsUsed.add(r.op); }
            return r;
        },
        'expr.param': (c, expr) => IR.param(expr.name, c.expr(expr.otherwise)),
        'expr.randint': (c, expr) => {
            const max = c.expr(expr.max);
            return !IR.isInt(max) ? PRNG.nextIntChecked(max)
                : max.value > 0 ? PRNG.nextInt(max)
                : fail();
        },
        'expr.sum': (c, expr) => {
            const g = c.grids[expr.inGrid];
            const p = g.grid.convPatterns.getByID(expr.patternID);
            return g.makeConvBuffer(p.kernel).get(p);
        },
    };
    
    type StmtCompileFunc<K extends ASG.Statement['kind']>
        = K extends ASG.BranchingStmt['kind']
        ? (c: Compiler, stmt: Extract<ASG.Statement, {readonly kind: K}>, ifChanged: IR.Stmt, then: IR.Stmt) => IR.Stmt
        : (c: Compiler, stmt: Extract<ASG.Statement, {readonly kind: K}>) => IR.Stmt
    const STMT_COMPILE_FUNCS: {readonly [K in (ASG.BranchingStmt | ASG.NonBranchingStmt)['kind']]: StmtCompileFunc<K>} = {
        // non-branching
        'stmt.assign': (c, stmt) => IR.assign(c.variables.name(stmt.variable.id), '=', c.expr(stmt.rhs)),
        'stmt.log': (c, stmt) => IR.log(c.expr(stmt.expr)),
        'stmt.rules.map': _stmtNotSupported,
        'stmt.put': (c, stmt) => {
            const g = c.grids[stmt.inGrid];
            const {pattern} = stmt;
            
            // check whether pattern is effective
            let isEffective: IR.Expr;
            if(pattern.kind === 'expr.constant') {
                const {value} = pattern.constant;
                const checks = value.map((dx, dy, c) => OP.ne(g.data.get(g.relativeIndex(dx, dy)), IR.int(c)));
                isEffective = checks.length > 0 ? checks.reduce(OP.or) : fail();
            } else {
                // non-constant pattern will be checked in _writeCondition
                isEffective = IR.TRUE;
            }
            
            return IR.block([
                g.declareAtIndex(c.expr(stmt.at)),
                
                // check bounds, given size of pattern
                g.grid.periodic ? IR.PASS : IR.if_(
                    OP.or(
                        pattern.type.width > 1 ? OP.ge(OP.addConstant(AT_X, pattern.type.width - 1), g.width) : IR.FALSE,
                        pattern.type.height > 1 ? OP.ge(OP.addConstant(AT_Y, pattern.type.height - 1), g.height) : IR.FALSE,
                    ),
                    IR.throw_('pattern would be out of bounds'),
                ),
                
                pattern.kind !== 'expr.constant' ? IR.declVar(P, IR.PATTERN_TYPE, c.expr(pattern)) : IR.PASS,
                
                IR.if_(
                    OP.and(isEffective, _writeCondition(c, g, pattern, P, stmt.condition)),
                    _doWrite(c, g, undefined, pattern, false, undefined, true, c.config.animate),
                ),
            ]);
        },
        'stmt.use': (c, stmt) => {
            return c.config.animate ? c.grids[stmt.grid].yield_() : fail();
        },
        
        // branching
        'stmt.convchain': _stmtNotSupported,
        'stmt.path': _stmtNotSupported,
        'stmt.rules.basic.all': _basicAllPrl,
        'stmt.rules.basic.one': _basicOne,
        'stmt.rules.basic.prl': _basicAllPrl,
        'stmt.rules.biased.all': _stmtNotSupported,
        'stmt.rules.biased.one': _stmtNotSupported,
        'stmt.rules.convolution': _stmtConvolution,
        'stmt.rules.search.all': _stmtNotSupported,
        'stmt.rules.search.one': _stmtNotSupported,
    };
    
    function _stmtNotSupported(c: Compiler, stmt: ASG.Statement): IR.Stmt {
        c.notSupported(`'${stmt.kind}'`, stmt.pos);
        return IR.PASS;
    }
    
    function _doWrite(c: Compiler, outGrid: IR.Grid, from: PatternTree | undefined, to: ASG.Prop<'pattern.out'>, useMask: boolean, flagVar: IR.NameExpr | undefined, doUpdate: boolean, doYield: boolean): IR.Stmt {
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
    
    function _writeCondition(c: Compiler, g: IR.Grid, patternExpr: ASG.Prop<'pattern.out'> | undefined, patternVar: IR.NameExpr | undefined, conditionExpr: ASG.Prop<'bool?'>): IR.Expr {
        const hasEffect
            = patternExpr === undefined || patternExpr.kind === 'expr.constant'
            ? IR.TRUE
            : IR.libMethodCall('Pattern', 'hasEffect', patternVar ?? fail(), [g.useObj(), AT_X, AT_Y]);
        return conditionExpr === undefined ? hasEffect : OP.and(hasEffect, c.expr(conditionExpr));
    }
    
    function _basicOne(c: Compiler, stmt: ASG.BasicRulesStmt, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
        const {rewrites} = stmt;
        const g = c.grids[stmt.inGrid];
        const sampler = g.makeSampler(rewrites.map(rule => rule.from));
        
        const writeConditions = rewrites.map(rule => _writeCondition(c, g, rule.to, P, rule.condition));
        
        // optimisation for common case: all rewrites are unconditional and definitely effective
        const allUnconditionalAndEffective = writeConditions.every(c => c === IR.TRUE);
        
        const cases = rewrites.map((rule, i) => IR.block([
            rule.to.kind !== 'expr.constant' ? IR.declVar(P, IR.PATTERN_TYPE, c.expr(rule.to)) : IR.PASS,
            IR.if_(
                writeConditions[i],
                _doWrite(c, g, rule.from, rule.to, false, allUnconditionalAndEffective ? undefined : ANY, true, c.config.animate),
            ),
        ]));
        
        return allUnconditionalAndEffective
            ? IR.if_(
                OP.gt(sampler.count, IR.ZERO),
                IR.block([
                    sampler.sampleWithReplacement(cases),
                    ifChanged,
                ]),
                then,
            )
            : IR.block([
                sampler.beginSamplingWithoutReplacement(),
                c.matches.declareCount(sampler.count, true),
                IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true),
                IR.while_(
                    OP.and(c.matches.isNotEmpty, OP.not(ANY)),
                    sampler.sampleWithoutReplacement(cases, c.matches.count),
                ),
                IR.if_(ANY, ifChanged, then),
            ]);
    }
    
    function _exprIs(expr: ASG.Expression, flags: ExprFlags): boolean {
        return (expr.flags & flags) === flags;
    }
    
    function _basicAllPrl(c: Compiler, stmt: ASG.BasicRulesStmt, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
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
        
        const firstPassConditions = rewrites.map((rule, i) => _writeCondition(
            c,
            g,
            outPatternIsSameEverywhere[i] ? rule.to : undefined,
            IR.NAMES.tmpPattern(i),
            rule.condition,
        ));
        const secondPassConditions = rewrites.map((rule, i) => _writeCondition(
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
                _doWrite(c, g, rule.from, rule.to, useMask, useFlag ? ANY : undefined, true, false),
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
    
    function _stmtConvolution(c: Compiler, stmt: ASG.ConvolutionStmt, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
        const {kernel} = stmt;
        const g = c.grids[stmt.inGrid];
        const buffer = g.makeConvBuffer(kernel);
        
        const cases: IR.Stmt[] = emptyArray(g.grid.alphabet.key.length, IR.PASS);
        for(const rule of stmt.rewrites) {
            const mask = PatternTree.isLeafOrTop(rule.from) ? rule.from.masks[0] : fail();
            
            const caseHandler = IR.block([
                rule.to.kind === 'expr.constant' ? IR.PASS : IR.declVar(P, IR.PATTERN_TYPE, c.expr(rule.to)),
                IR.if_(
                    _writeCondition(c, g, rule.to, P, rule.condition),
                    _doWrite(c, g, rule.from, rule.to, false, ANY, false, false),
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
                    ifChanged,
                ]),
                then,
            ),
        ]);
    }

    function _stmtConvChain(c: Compiler, stmt: ASG.ConvChainStmt, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
        const g = c.grids[stmt.inGrid];
        
        const sampler = g.makeSampler([stmt.on]);
        const counter = g.makeWeightedCounter(stmt.weights);
        const output = stmt.output;
        const temperature = stmt.temperature !== undefined ? c.expr(stmt.temperature) : IR.FLOAT_ONE;
        
        // TODO
        const colourArrayName = null!;
        const matchesArrayName = null!;
        const matchesCount = null!;
        const doReset = null!;
        const setFlagToNotDoResetNextTime = null!;
        const keepProbability = null!;
        
        const colourArray = IR.makeConstArray(colourArrayName, output, g.grid.alphabet.key.length);
        const matchesArray = IR.makeMutableArray(matchesArrayName, g.n, IR.INT32_ARRAY_TYPE.domainSize);
        
        const getRandomColour = colourArray.get(PRNG.nextInt(IR.int(output.length)));
        const update1x1 = g.update(AT_X, AT_Y, IR.ONE, IR.ONE);
        
        return IR.if_(
            doReset,
            IR.if_(
                OP.gt(matchesCount, IR.ZERO),
                IR.block([
                    sampler.copyInto(matchesArrayName),
                    IR.assign(matchesCount, '=', sampler.count),
                    IR.forRange(I, IR.ZERO, matchesCount, [
                        g.write(matchesArray.get(I), getRandomColour),
                    ]),
                    setFlagToNotDoResetNextTime,
                    ifChanged,
                ]),
                then,
            ),
            IR.block([
                IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true),
                IR.forRange(I, IR.ZERO, matchesCount, [
                    g.declareAtIndex(matchesArray.get(PRNG.nextInt(matchesCount))),
                    IR.declVar(OLD_S, IR.BYTE_TYPE, g.data.get(AT)),
                    IR.declVar(OLD_T, IR.INT_TYPE, counter),
                    g.write(AT, getRandomColour),
                    update1x1,
                    IR.if_(
                        OP.and(OP.lt(counter, OLD_T), OP.ge(PRNG.NEXT_DOUBLE, keepProbability)),
                        IR.block([
                            g.write(AT, OLD_S),
                            update1x1,
                            IR.assign(ANY, '=', IR.TRUE),
                        ]),
                    ),
                ]),
                IR.if_(
                    ANY,
                    IR.block([c.config.animate ? g.yield_() : IR.PASS, ifChanged]),
                    then,
                ),
            ]),
        );
    }
    
    type CodeGenClass = new (config: Config) => {diagnostics: Diagnostics, writeStmt(stmt: IR.Stmt): void, render(): string}
    export type CodeGenName = KeysMatching<typeof CodeGen, CodeGenClass>
    
    export function compile(src: string, targetLanguage: CodeGenName, config?: Partial<Config>): string {
        const ast = Parser.parse(src);
        const asg = Resolver.resolve(ast);
        
        const compiler = new Compiler(asg, config !== undefined ? {...DEFAULT_CONFIG, ...config} : DEFAULT_CONFIG);
        const result = compiler.compile();
        compiler.diagnostics.throwIfAnyErrors();
        
        const cls: CodeGenClass = CodeGen[targetLanguage];
        const gen = new cls(compiler.config);
        gen.writeStmt(result);
        
        gen.diagnostics.throwIfAnyErrors();
        return gen.render();
    }
}
