///<reference path="../ir/types.ts"/>
///<reference path="./expr.ts"/>
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
    const {
        NAMES: {
            WIDTH, HEIGHT, PARAMS, RNG,
            ITERATIONS,
        },
        OP,
    } = IR;
    
    export interface Compiler {
        readonly config: Config;
        readonly diagnostics: Diagnostics;
        readonly opsUsed: Set<IR.Op>;
        
        readonly constants: IR.Constants;
        readonly grids: readonly IR.Grid[];
        readonly mask: IR.Mask;
        
        readonly checkMaxIterations: IR.Stmt;
        
        getStmtCompiler(stmt: ASG.Statement): StmtCompiler;
        expr(expr: ASG.Expression): IR.Expr;
        type(type: Type.Type): IR.IRType;
        
        useTempArray(scale: number): IR.TempArray;
        
        notSupported(msg: string, pos: SourcePosition): void;
        dictType(entryTypes: ReadonlyMap<string, Type.Type>): IR.DictType;
    }
    
    /**
     * Compiles MJr source code to a high-level intermediate representation.
     */
    class CompilerImpl implements Compiler {
        readonly diagnostics = new Diagnostics();
        readonly opsUsed = new Set(ALWAYS_USED_OPS);
        
        readonly constants = new IR.Constants();
        readonly grids: readonly IR.Grid[];
        readonly mask = new IR.Mask();
        
        readonly declareMaxIterations: IR.Stmt;
        readonly checkMaxIterations: IR.Stmt;
        
        private tempArrayScale = 0;
        
        constructor(
            private readonly asg: ASG.ASG,
            readonly config: Readonly<Config>,
        ) {
            this.grids = asg.grids.map(g => new IR.Grid(g));
            
            for(const g of asg.grids) {
                if(g.periodic) { this.notSupported('periodic grid', g.pos); }
            }
            
            const maxIterations = config.maxIterations;
            if(maxIterations !== 0) {
                this.declareMaxIterations = IR.declVar(ITERATIONS, IR.INT_TYPE, IR.ZERO, true);
                this.checkMaxIterations = IR.block([
                    IR.assign(ITERATIONS, '+=', IR.ONE),
                    IR.if_(
                        OP.gt(ITERATIONS, IR.int(maxIterations)),
                        IR.throw_(`Exceeded maximum of ${maxIterations} iterations`),
                    ),
                ]);
            } else {
                this.declareMaxIterations = IR.PASS;
                this.checkMaxIterations = IR.PASS;
            }
        }
        
        notSupported(s: string, pos: SourcePosition): void {
            this.diagnostics.compilationError(`${s} is not currently supported`, pos);
        }
        
        compile(): IR.Stmt {
            // YYYY-MM-DD HH:mm:ss GMT+offset
            const date = new Date().toLocaleString('en-SE', {timeZoneName: 'short'});
            
            const {params, endGridID, root} = this.asg;
            
            // TODO: potentials
            
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
            const rootCompiler = this.getStmtCompiler(root),
                declRoot = rootCompiler.declare(),
                compiledRoot = rootCompiler.compile(this, IR.PASS, IR.PASS),
                endGridObj = this.grids[endGridID].useObj(),
                gridDecls = this.grids.flatMap(g => g.declare()),
                matchesDecl = this.declareTempArray(),
                maskDecl = this.mask.declare(),
                constDecls = this.constants.declare(),
                varDecls = declareASGVariables(this, this.asg.variables),
                otherDecls: IR.Stmt[] = [];
            
            // compute maximum grid dimensions, to ensure that arrays aren't over-allocated and loose int operations don't overflow
            // 0x3FFFFFFE is the magic number for the largest allowed array length for a LFSR
            // don't need to include mask.scale here; mask array length is at most 1/32 of any grid array length
            const maxScale = Math.max(
                this.tempArrayScale,
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
            
            const {animate, emitChecks, entryPointName} = this.config;
            
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
                this.declareMaxIterations,
                declRoot,
                compiledRoot,
                IR.return_(endGridObj),
            ]));
        }
        
        private lastStmtID = -1;
        readonly getStmtCompiler = (stmt: ASG.Statement): StmtCompiler => {
            const id = ++this.lastStmtID;
            return new STMT_COMPILERS[stmt.kind](stmt as never, id, this);
        };
        
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
        
        private readonly tempArray = new IR.TempArray(IR.NAMES.MATCHES, IR.NAMES.MATCH_COUNT);
        useTempArray(scale: number): IR.TempArray {
            this.tempArrayScale = Math.max(this.tempArrayScale, scale);
            return this.tempArray;
        }
        private declareTempArray(): IR.Stmt {
            if(this.tempArrayScale === 0) { return IR.PASS; }
            
            // TODO: in principle, we can get a better estimate for the maximum number of matches if we know some patterns cannot overlap
            const n = OP.multConstant(OP.mult(WIDTH, HEIGHT), this.tempArrayScale);
            return IR.block([
                IR.declVar(IR.NAMES.MATCHES, IR.INT32_ARRAY_TYPE, IR.newInt32Array(n)),
                IR.declVar(IR.NAMES.MATCH_COUNT, IR.INT_TYPE, undefined, true),
            ]);
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
    type StmtCompileClass<K extends StmtKind> = new (stmt: Extract<ASG.Statement, {readonly kind: K}>, stmtID: number, c: Compiler) => StmtCompiler
    
    const STMT_COMPILERS: {readonly [K in StmtKind]: StmtCompileClass<K>} = {
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
