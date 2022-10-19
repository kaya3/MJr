///<reference path="../ir/names.ts"/>
///<reference path="../ir/ops.ts"/>
///<reference path="../ir/types.ts"/>
///<reference path="./expr.ts"/>
///<reference path="./stmt_all.ts"/>
///<reference path="./stmt_convchain.ts"/>
///<reference path="./stmt_convolution.ts"/>
///<reference path="./stmt_one.ts"/>
///<reference path="./stmt_put.ts"/>

/**
 * Compiles MJr source code to a specified target language.
 */
namespace Compiler {
    export const COMPILER_VERSION = '0.1 (unstable)';
    export const REQUIRED_RUNTIME_LIB_VERSION = 0;
    
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
        },
        OP,
    } = IR;
    
    export interface Compiler extends IR.IRCompiler {
        readonly config: Config;
        readonly diagnostics: Diagnostics;
        readonly nodeCompilers: ReadonlyMap<number, StmtCompiler>;
        readonly opsUsed: Set<IR.Op>;
        
        readonly constants: IR.Constants;
        readonly flags: IR.Flags;
        readonly grids: readonly IR.Grid[];
        readonly limits: IR.Limits;
        readonly mask: IR.Mask;
        readonly matches: IR.MatchesArray;
        
        notSupported(msg: string, pos: SourcePosition): void;
        dictType(entryTypes: ReadonlyMap<string, Type.Type>): IR.DictType;
    }
    
    /**
     * Compiles MJr source code to a high-level intermediate representation.
     */
    class CompilerImpl implements Compiler {
        readonly diagnostics = new Diagnostics();
        readonly opsUsed = new Set(ALWAYS_USED_OPS);
        
        private readonly cfg: CFG.CFG;
        private readonly switchNodes: ReadonlyIDMap<CFG.SwitchNode>;
        readonly nodeCompilers = new Map<number, StmtCompiler>();
        
        readonly constants = new IR.Constants();
        readonly flags: IR.Flags;
        readonly grids: readonly IR.Grid[];
        readonly limits: IR.Limits;
        readonly mask = new IR.Mask();
        readonly matches = new IR.MatchesArray();
        
        constructor(
            private readonly asg: ASG.ASG,
            readonly config: Readonly<Config>,
        ) {
            this.cfg = CFG.build(asg.root, config.animate);
            this.switchNodes = IDMap.ofWithKey(this.cfg.nodes.filter(CFG.isSwitchNode), node => node.id);
            this.grids = asg.grids.map(g => new IR.Grid(g));
            this.flags = new IR.Flags(this.cfg.numFlags);
            this.limits = new IR.Limits(asg.limits);
            
            for(const g of asg.grids) {
                if(g.periodic) { this.notSupported('periodic grid', g.pos); }
            }
        }
        
        notSupported(s: string, pos: SourcePosition): void {
            this.diagnostics.compilationError(`${s} is not currently supported`, pos);
        }
        
        compile(): IR.Stmt {
            // YYYY-MM-DD HH:mm:ss GMT+offset
            const date = new Date().toLocaleString('en-SE', {timeZoneName: 'short'});
            
            const {params, endGridID} = this.asg;
            
            const nodeCompilers = this.switchNodes.map(node => this.getStmtCompiler(node));
            const switchCases = this.switchNodes.map((node, i) => {
                const {before, ifTrue, ifFalse, after} = this.getNodeTransitions(node);
                const nc = nodeCompilers[i];
                return IR.block([
                    before,
                    nc.compile(this, ifTrue ?? IR.PASS, ifFalse ?? IR.PASS),
                    ...after,
                ]);
            });
            
            // TODO: potentials
            
            const endGridObj = this.grids[endGridID].useObj();
            const gridDecls = this.grids.flatMap(g => g.declare());
            
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
                constDecls = this.constants.declare(),
                varDecls = declareASGVariables(this, this.asg.variables),
                flagDecls = this.flags.declare(),
                limitDecls = this.limits.declare(this),
                otherDecls: IR.Stmt[] = [];
            
            for(const nc of nodeCompilers) {
                const r = nc.declare?.();
                if(r !== undefined) { otherDecls.push(r); }
            }
            
            // compute maximum grid dimensions, to ensure that arrays aren't over-allocated and loose int operations don't overflow
            // 0x3FFFFFFE is the magic number for the largest allowed array length for a LFSR
            // don't need to include mask.scale here; mask array length is at most 1/32 of any grid array length
            const maxScale = Math.max(
                this.matches.scale,
                ...this.grids.map(g => g.getScale()),
            );
            const maxDim = IR.int(Math.sqrt(0x3FFFFFFE / maxScale) | 0);
            const checkDims = IR.if_(
                OP.or(OP.le(WIDTH, IR.ZERO), OP.le(HEIGHT, IR.ZERO)),
                IR.throw_('Grid dimensions must be positive'),
                IR.if_(
                    OP.or(OP.gt(WIDTH, maxDim), OP.gt(HEIGHT, maxDim)),
                    IR.throw_(`Grid dimensions cannot exceed ${maxDim.value}`),
                ),
            );
            
            const {animate, emitChecks, entryPointName, maxIterations} = this.config;
            return IR.declFunc(IR.nameExpr(entryPointName), animate ? IR.REWRITE_INFO_TYPE : undefined, mainParams, mainParamTypes, IR.GRID_TYPE, IR.block([
                IR.comment(`compiled by mjrc-${COMPILER_VERSION} on ${date}`),
                emitChecks ? checkDims : IR.PASS,
                
                IR.preamble(this.dictType(params), emitChecks, REQUIRED_RUNTIME_LIB_VERSION, Array.from(this.opsUsed)),
                IR.BLANK_LINE,
                
                ...gridDecls,
                matchesDecl,
                ...maskDecl,
                constDecls,
                ...varDecls,
                ...otherDecls,
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
        
        getStmtCompiler(node: CFG.SwitchNode): StmtCompiler {
            return getOrCompute(this.nodeCompilers, node.id, () =>
                node.kind === 'reset' ? new Stmt_Reset(node)
                : new STMT_COMPILERS[node.stmt.kind](node.stmt as never, node.id, this)
            );
        }
        
        private getNodeTransitions(node: CFG.SwitchNode): {before: IR.Stmt, ifTrue?: IR.Stmt, ifFalse?: IR.Stmt, after: readonly IR.Stmt[]} {
            switch(node.kind) {
                case 'reset': {
                    const {stmt} = node;
                    return {
                        before: IR.comment(`reset ${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                        after: this.goto(node.id, node.then.nodeID),
                    };
                }
                
                case 'stmt.branching': {
                    const ifTrue = this.goto(node.id, node.ifChanged.nodeID),
                        ifFalse = this.goto(node.id, node.then.nodeID),
                        eitherWay: IR.Stmt[] = [];
                    while(ifTrue.length > 0 && ifFalse.length > 0 && IR.equals(ifTrue[ifTrue.length - 1], ifFalse[ifFalse.length - 1])) {
                        const s = ifTrue.pop()!;
                        ifFalse.pop();
                        eitherWay.push(s);
                    }
                    
                    const {stmt} = node;
                    return {
                        before: IR.comment(`${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                        ifTrue: IR.block(ifTrue),
                        ifFalse: IR.block(ifFalse),
                        after: eitherWay,
                    };
                }
                
                case 'stmt.nonbranching': {
                    const {stmt} = node;
                    return {
                        before: IR.comment(`${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                        after: this.goto(node.id, node.then.nodeID),
                    };
                }
            }
        }
        
        expr(expr: ASG.Expression): IR.Expr {
            return compileExpr(this, expr);
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
                if(cur.kind === 'stmt.branching' || cur.kind === 'stmt.nonbranching' || (cur.kind === 'reset' && cur.stmt.anyResets) || cur.kind === 'stop') {
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
    
    type StmtKind = (ASG.BranchingStmt | ASG.NonBranchingStmt)['kind']
    
    export interface StmtCompiler {
        declare?(): IR.Stmt;
        compileReset?(c: Compiler): IR.Stmt;
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt): IR.Stmt;
    }
    
    class Stmt_Reset implements StmtCompiler {
        constructor(readonly node: CFG.ResetNode) {}
        compile(c: Compiler): IR.Stmt {
            const {node} = this;
            const out: IR.Stmt[] = [c.flags.clear(node.flagID)];
            for(const childID of node.childIDs) {
                const comp = c.nodeCompilers.get(childID);
                const r = comp?.compileReset?.(c);
                if(r !== undefined) { out.push(r); }
            }
            for(const limitID of node.limitIDs) {
                out.push(c.limits.reset(limitID, c));
            }
            return IR.block(out);
        }
    }
    
    class Stmt_NotSupported implements StmtCompiler {
        constructor(readonly stmt: ASG.Statement) {}
        compile(c: Compiler): IR.Stmt {
            const {kind, pos} = this.stmt;
            c.notSupported(`'${kind}'`, pos);
            return IR.PASS;
        }
    }
    
    class Stmt_Assign implements StmtCompiler {
        constructor(readonly stmt: ASG.AssignStmt) {}
        compile(c: Compiler) {
            const {variable, rhs} = this.stmt;
            return IR.assign(IR.NAMES.variable(variable), '=', c.expr(rhs));
        }
    }
    
    class Stmt_Log implements StmtCompiler {
        constructor(readonly stmt: ASG.LogStmt) {}
        compile(c: Compiler) {
            const {expr} = this.stmt;
            return IR.log(c.expr(expr));
        }
    }
    
    class Stmt_Use implements StmtCompiler {
        constructor(readonly stmt: ASG.UseStmt) {}
        compile(c: Compiler) {
            const {grid} = this.stmt;
            return c.config.animate ? c.grids[grid].yield_() : fail();
        }
    }
    
    type StmtCompileClass<K extends StmtKind> = new (stmt: Extract<ASG.Statement, {readonly kind: K}>, stmtID: number, c: Compiler) => StmtCompiler
    
    const STMT_COMPILERS: {readonly [K in StmtKind]: StmtCompileClass<K>} = {
        // non-branching
        'stmt.assign': Stmt_Assign,
        'stmt.log': Stmt_Log,
        'stmt.rules.map': Stmt_NotSupported,
        'stmt.put': Stmt_Put,
        'stmt.use': Stmt_Use,
        
        // branching
        'stmt.convchain': Stmt_ConvChain,
        'stmt.path': Stmt_NotSupported,
        'stmt.rules.basic.all': Stmt_BasicAllPrl,
        'stmt.rules.basic.one': Stmt_BasicOne,
        'stmt.rules.basic.prl': Stmt_BasicAllPrl,
        'stmt.rules.biased.all': Stmt_NotSupported,
        'stmt.rules.biased.one': Stmt_NotSupported,
        'stmt.rules.convolution': Stmt_Convolution,
        'stmt.rules.search.all': Stmt_NotSupported,
        'stmt.rules.search.one': Stmt_NotSupported,
    };
    
    type CodeGenClass = new (config: Config) => {diagnostics: Diagnostics, writeStmt(stmt: IR.Stmt): void, render(): string}
    export type CodeGenName = KeysMatching<typeof CodeGen, CodeGenClass>
    
    export function compile(src: string, targetLanguage: CodeGenName, config?: Partial<Config>): string {
        const ast = Parser.parse(src);
        const asg = Resolver.resolve(ast);
        
        const compiler = new CompilerImpl(asg, config !== undefined ? {...DEFAULT_CONFIG, ...config} : DEFAULT_CONFIG);
        const result = compiler.compile();
        compiler.diagnostics.throwIfAnyErrors();
        
        const cls: CodeGenClass = CodeGen[targetLanguage];
        const gen = new cls(compiler.config);
        gen.writeStmt(result);
        
        gen.diagnostics.throwIfAnyErrors();
        return gen.render();
    }
}
