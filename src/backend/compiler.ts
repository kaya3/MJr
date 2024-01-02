///<reference path="../ir/types.ts"/>
///<reference path="./ctx.ts"/>
///<reference path="./stmt.ts"/>
///<reference path="./stmt_all.ts"/>
///<reference path="./stmt_convchain.ts"/>
///<reference path="./stmt_convolution.ts"/>
///<reference path="./stmt_one.ts"/>
///<reference path="./stmt_put.ts"/>

/**
 * Compiles MJr source code to a specified target language.
 */
namespace Compiler {
    export const COMPILER_VERSION = '0.2 (unstable)';
    export const REQUIRED_RUNTIME_LIB_VERSION = 0;
    
    const ALWAYS_USED_OPS: readonly IR.Op[] = [
        'bool_and', 'bool_or', 'bool_not',
        'int_eq', 'int_ne', 'int_lt', 'int_le', 'int_gt', 'int_ge',
        'int_and', 'int_or', 'int_xor', 'int_not', 'int_lshift', 'int_rshift', 'int_ctz',
        'loose_int_plus', 'loose_int_mult', 'loose_int_floordiv', 'loose_int_mod',
    ];
    
    // helpers
    const {OP} = IR;
    
    export interface Compiler {
        readonly config: Config;
        readonly diagnostics: Diagnostics;
        readonly opsUsed: Set<IR.Op>;
        
        readonly ir: IR.Factory;
        readonly grids: readonly IR.Grid[];
        readonly prng: IR.PRNG;
        
        readonly checkMaxIterations: IR.Stmt;
        
        type(type: Type.Type): IR.IRType;
        dictType(entryTypes: ReadonlyMap<string, Type.Type>): IR.DictType;
        getStmtCompiler(stmt: ASG.Statement): StmtCompiler;
        
        internConstant(expr: IR.Expr, type: IR.IRType): IR.ConstNameExpr;
        useMask(grid: IR.Grid): IR.Mask;
        useTempArray(scale: number): IR.TempArray;
        
        notSupported(msg: string, pos: SourcePosition): void;
    }
    
    /**
     * Compiles MJr source code to a high-level intermediate representation.
     */
    class CompilerImpl implements Compiler {
        public readonly diagnostics = new Diagnostics();
        public readonly opsUsed = new Set(ALWAYS_USED_OPS);
        
        public readonly ir: IR.Factory;
        public readonly grids: readonly IR.Grid[];
        public readonly prng: IR.PRNG;
        
        private readonly entryPointName: IR.ConstNameExpr;
        private readonly width: IR.ConstNameExpr;
        private readonly height: IR.ConstNameExpr;
        private readonly mainDecls: readonly IR.StmtLevelDecl[];
        private readonly mainParams: readonly IR.ParamDecl[];
        readonly checkMaxIterations: IR.Stmt;
        
        private readonly mask: IR.Mask;
        private maskScale = 0;
        
        private readonly tempArray: IR.TempArray;
        private tempArrayScale = 0;
        
        constructor(
            private readonly asg: ASG.ASG,
            readonly config: Readonly<Config>,
        ) {
            for(const g of asg.grids) {
                if(g.periodic) { this.notSupported('periodic grid', g.pos); }
            }
            
            const ir = this.ir = new IR.Factory();
            this.entryPointName = ir.func(config.entryPointName);
            
            const width = ir.paramDecl('width', IR.INT_TYPE),
                height = ir.paramDecl('height', IR.INT_TYPE),
                rng = ir.paramDecl('rng', IR.PRNG_TYPE, true);
            
            this.width = width.name;
            this.height = height.name;
            this.grids = asg.grids.map(g => new IR.Grid(ir, g, width.name, height.name));
            this.prng = new IR.PRNG(rng.name);
            this.mask = new IR.Mask(ir);
            this.tempArray = new IR.TempArray(this.ir);
            
            const mainDecls: IR.StmtLevelDecl[] = [];
            
            const mainParams = [width, height];
            if(asg.params.size > 0) {
                const p = ir.paramDecl('params', IR.nullableType({
                    kind: 'dict',
                    keys: Array.from(asg.params.keys()),
                    values: Array.from(asg.params.values(), t => IR.nullableType(this.type(t))),
                }));
                mainParams.push(p);
            }
            mainParams.push(rng);
            
            const maxIterations = config.maxIterations;
            if(maxIterations !== 0) {
                const iterations = ir.varDecl('iterations', IR.INT_TYPE, IR.ZERO);
                mainDecls.push(iterations);
                
                this.checkMaxIterations = IR.seq([
                    IR.assign(iterations.name, '+=', IR.ONE),
                    IR.if_(
                        OP.gt(iterations.name, IR.int(maxIterations)),
                        IR.throw_(`Exceeded maximum of ${maxIterations} iterations`),
                    ),
                ]);
            } else {
                this.checkMaxIterations = IR.PASS;
            }
            
            this.mainDecls = mainDecls;
            this.mainParams = mainParams;
        }
        
        notSupported(s: string, pos: SourcePosition): void {
            this.diagnostics.compilationError(`${s} is not currently supported`, pos);
        }
        
        public getStmtCompiler(stmt: ASG.Statement): StmtCompiler {
            const cls = STMT_COMPILERS[stmt.kind];
            return new cls(stmt as never, this);
        }
        
        compile(): IR.Stmt {
            // YYYY-MM-DD HH:mm:ss GMT+offset
            const date = new Date().toLocaleString('en-SE', {timeZoneName: 'short'});
            
            const {asg, config, grids} = this;
            const ctx = CompilerContext.root(this);
            // TODO: potentials
            
            // need to compile everything before preamble, so that `maxScale` and `this.opsUsed` are correct
            const rootCompiler = this.getStmtCompiler(asg.root),
                declRoot = rootCompiler.declareRoot(ctx),
                declRootMut = rootCompiler.declareParent(ctx),
                compiledRoot = rootCompiler.compile(ctx, IR.PASS, IR.PASS)[0],
                gridDecls = grids.map(g => g.declare()),
                tmpArraysDecl = this.declareTempArrays();
            
            // compute maximum grid dimensions, to ensure that loose int operations on array lengths/indices don't overflow
            // 0x3FFFFFFE is the magic number for the largest allowed array length for an LFSR
            const maxScale = Math.max(
                this.maskScale,
                this.tempArrayScale,
                ...grids.map(g => g.getScale()),
            );
            const maxDim = IR.int(Math.sqrt(0x3FFFFFFE / maxScale) | 0);
            const checkDims = config.emitChecks ? IR.if_(
                OP.or(OP.le(this.width, IR.ZERO), OP.le(this.height, IR.ZERO)),
                IR.throw_('Grid dimensions must be positive'),
                IR.if_(
                    OP.or(OP.gt(this.width, maxDim), OP.gt(this.height, maxDim)),
                    IR.throw_(`Grid dimensions cannot exceed ${maxDim.value}`),
                ),
            ) : IR.PASS;
            
            return IR.exportDecl(IR.funcDecl(this.entryPointName, config.animate ? IR.REWRITE_INFO_TYPE : undefined, this.mainParams, IR.GRID_TYPE, IR.seq([
                IR.comment(`compiled by mjrc-${COMPILER_VERSION} on ${date}`),
                checkDims,
                
                IR.preamble(this.dictType(asg.params), config.emitChecks, REQUIRED_RUNTIME_LIB_VERSION, Array.from(this.opsUsed)),
                IR.BLANK_LINE,
                
                IR.withDecls([
                    ...this.mainDecls,
                    ...gridDecls,
                    ...tmpArraysDecl,
                    ...this.internedConstants.values(),
                    declRoot,
                    declRootMut,
                ], IR.seq([
                    IR.BLANK_LINE,
                    compiledRoot,
                    IR.return_(grids[asg.endGridID].obj),
                ])),
            ])));
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
        
        private readonly internedConstants = new Map<string, IR.ConstDecl>();
        internConstant(expr: IR.Expr, type: IR.IRType): IR.ConstNameExpr {
            return getOrCompute(
                this.internedConstants,
                IR.key(expr),
                () => this.ir.constDecl('constant', type, expr),
            ).name;
        }
        
        useMask(g: IR.Grid): IR.Mask {
            this.maskScale = Math.max(this.maskScale, g.grid.scaleX * g.grid.scaleY);
            return this.mask;
        }
        useTempArray(scale: number): IR.TempArray {
            this.tempArrayScale = Math.max(this.tempArrayScale, scale);
            return this.tempArray;
        }
        private declareTempArrays(): IR.StmtLevelDecl[] {
            const decls: IR.StmtLevelDecl[] = [];
            if(this.maskScale > 0) {
                const maskScale = OP.multConstant(OP.mult(this.width, this.height), this.maskScale);
                decls.push(this.mask.declare(maskScale));
            }
            if(this.tempArrayScale > 0) {
                // TODO: in principle, we can get a better estimate for the maximum number of matches if we know some patterns cannot overlap
                const tempArrayScale = OP.multConstant(OP.mult(this.width, this.height), this.tempArrayScale);
                decls.push(this.tempArray.declare(tempArrayScale));
            }
            return decls;
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
    
    type StmtKind = ASG.Statement['kind']
    export type StmtCompileClass<K extends StmtKind> = new (stmt: Extract<ASG.Statement, {readonly kind: K}>, c: Compiler) => StmtCompiler
    
    export const STMT_COMPILERS: {readonly [K in StmtKind]: StmtCompileClass<K>} = {
        // control-flow
        'stmt.block.markov': Stmt_Markov,
        'stmt.block.sequence': Stmt_Sequence,
        'stmt.modified.limit': Stmt_Limit,
        
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
    
    type CodeGenClass = new (config: Config) => {diagnostics: Diagnostics, beginGenerating(): void, writeStmt(stmt: IR.Stmt): void, render(): string}
    export type CodeGenName = KeysMatching<typeof CodeGen, CodeGenClass>
    
    export function compile(src: string, targetLanguage: CodeGenName, config?: Partial<Config>): string {
        const ast = Parser.parse(src);
        const asg = Resolver.resolve(ast);
        
        const compiler = new CompilerImpl(asg, config !== undefined ? {...DEFAULT_CONFIG, ...config} : DEFAULT_CONFIG);
        const result = compiler.compile();
        compiler.diagnostics.throwIfAnyErrors();
        
        if(result.info.hasFreeVars()) { fail(`compilation result had free variables`, result); }
        
        const cls: CodeGenClass = CodeGen[targetLanguage];
        const gen = new cls(compiler.config);
        gen.beginGenerating();
        gen.writeStmt(result);
        
        gen.diagnostics.throwIfAnyErrors();
        return gen.render();
    }
}
