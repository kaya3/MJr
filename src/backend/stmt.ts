namespace Compiler {
    export type StmtCompileResult = [IR.Stmt, CompilerContext]
    
    export abstract class StmtCompiler<T extends ASG.Statement = ASG.Statement> {
        constructor(
            protected readonly stmt: T,
        ) {}
        
        declareRoot(ctx: CompilerContext): IR.StmtLevelDecl {
            return IR.NO_DECL;
        }
        declareParent(ctx: CompilerContext): IR.StmtLevelDecl {
            return IR.NO_DECL;
        }
        compileReset(ctx: CompilerContext): IR.Stmt {
            return IR.PASS;
        }
        
        abstract compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult;
    }
    
    export class Stmt_NotSupported extends StmtCompiler {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const {kind, pos} = this.stmt;
            ctx.c.notSupported(`'${kind}'`, pos);
            return [ifFalse, ctx];
        }
    }
    
    export class Stmt_Assign extends StmtCompiler<ASG.AssignStmt> {
        public readonly decl: IR.MutVarDecl;
        readonly initLate: boolean;
        
        constructor(stmt: ASG.AssignStmt, c: Compiler) {
            super(stmt);
            this.initLate = !ASG.exprIsGridIndependent(stmt.rhs);
            this.decl = c.ir.varDecl(stmt.variable.name, c.type(stmt.variable.type));
        }
        
        private _init(ctx: CompilerContext): IR.Stmt {
            return IR.assign(this.decl.name, '=', ctx.expr(this.stmt.rhs));
        }
        
        declareParent(ctx: CompilerContext): IR.StmtLevelDecl {
            return this.initLate ? this.decl : IR.initDecl(this.decl, this._init(ctx));
        }
        
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            return [
                this.initLate ? IR.seq([this._init(ctx), ifFalse]) : ifFalse,
                ctx.withVariable(this.stmt.variable.id, this.decl.name),
            ];
        }
    }
    
    export class Stmt_Log extends StmtCompiler<ASG.LogStmt> {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const {expr} = this.stmt;
            return [
                IR.seq([IR.log(ctx.expr(expr)), ifFalse]),
                ctx,
            ];
        }
    }
    
    export class Stmt_Use extends StmtCompiler<ASG.UseStmt> {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const c = ctx.c, grid = this.stmt.grid;
            return [
                IR.seq([c.config.animate ? c.grids[grid].yield_() : IR.PASS, ifFalse]),
                ctx,
            ];
        }
    }
    
    export abstract class Stmt_Block<T extends ASG.BlockStmt> extends StmtCompiler<T> {
        protected readonly children: readonly StmtCompiler[];
        
        constructor(stmt: T, c: Compiler) {
            super(stmt);
            this.children = stmt.children.map(child => c.getStmtCompiler(child));
        }
        
        declareRoot(ctx: CompilerContext): IR.StmtLevelDecl {
            return IR.multiDecl(this.children.map(child => child.declareRoot(ctx)));
        }
        
        protected declareChildren(ctx: CompilerContext): IR.StmtLevelDecl {
            return IR.multiDecl(this.children.map(child => child.declareParent(ctx)));
        }
        
        protected resetChildren(ctx: CompilerContext): IR.Stmt {
            return IR.seq(this.children.map(child => child.compileReset(ctx)));
        }
    }
    
    export abstract class Stmt_Modified<T extends ASG.ModifiedStmt> extends StmtCompiler<T> {
        protected readonly child: StmtCompiler;
        
        constructor(stmt: T, c: Compiler) {
            super(stmt);
            this.child = c.getStmtCompiler(stmt.child);
        }
        
        declareRoot(ctx: CompilerContext): IR.StmtLevelDecl {
            return this.child.declareRoot(ctx);
        }
        declareParent(ctx: CompilerContext): IR.StmtLevelDecl {
            return this.child.declareParent(ctx);
        }
    }
    
    export class Stmt_Markov extends Stmt_Block<ASG.MarkovStmt> {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const isConsequential = ifTrue !== IR.PASS || ifFalse !== IR.PASS;
            
            const flag = ctx.c.ir.flag(),
                setFlagAndContinue = isConsequential ? IR.seq([flag.set, IR.CONTINUE]) : IR.CONTINUE,
                childDecls = this.declareChildren(ctx),
                resetChildren = this.resetChildren(ctx);
            
            const body: IR.Stmt[] = [ctx.c.checkMaxIterations];
            for(const child of this.children) {
                const [r, newCtx] = child.compile(ctx, setFlagAndContinue, IR.PASS);
                body.push(r);
                ctx = newCtx;
            }
            body.push(IR.BREAK);
            
            return [IR.withDecl(childDecls, IR.seq([
                // TODO: this is not strictly correct, since it resets out of order
                resetChildren,
                IR.withDecl(
                    isConsequential ? flag.decl : IR.NO_DECL,
                    IR.seq([
                        IR.while_(IR.TRUE, IR.seq(body)),
                        IR.if_(flag.check, ifTrue, ifFalse),
                    ]),
                ),
            ])), ctx];
        }
        
        compileTransparent(ctx: CompilerContext, ifTrue: IR.Stmt): StmtCompileResult {
            const ifTrueContinue = IR.seq([ifTrue, IR.CONTINUE]),
                childDecls = this.declareChildren(ctx),
                resetChildren = this.resetChildren(ctx);
            
            const body: IR.Stmt[] = [ctx.c.checkMaxIterations];
            for(const child of this.children) {
                const [r, newCtx] = child.compile(ctx, ifTrueContinue, IR.PASS);
                body.push(r);
                ctx = newCtx;
            }
            body.push(IR.BREAK);
            
            return [
                IR.withDecl(childDecls, IR.seq([
                    resetChildren,
                    IR.while_(IR.TRUE, IR.seq(body)),
                ])),
                ctx,
            ];
        }
    }
    
    export class Stmt_Sequence extends Stmt_Block<ASG.SequenceStmt> {
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const isConsequential = ifTrue !== IR.PASS || ifFalse !== IR.PASS;
            
            const flag = ctx.c.ir.flag(),
                setFlag = isConsequential ? flag.set : IR.PASS;
            
            const out: IR.Stmt[] = [],
                execChildren: IR.Stmt[] = [];
            
            for(const child of this.children) {
                out.push(
                    child instanceof Stmt_Limit
                    ? child.compileResetTransparent(ctx)
                    : child.compileReset(ctx)
                );
            }
            
            for(const child of this.children) {
                if(child instanceof Stmt_Markov || child instanceof Stmt_Limit) {
                    const [r, newCtx] = child.compileTransparent(ctx, setFlag);
                    execChildren.push(r);
                    ctx = newCtx;
                } else {
                    const [r, newCtx] = child.compile(ctx, setFlag, IR.BREAK);
                    execChildren.push(IR.while_(IR.TRUE, IR.seq([ctx.c.checkMaxIterations, r])));
                    ctx = newCtx;
                }
            }
            if(isConsequential) {
                execChildren.push(IR.if_(flag.check, ifTrue, ifFalse));
            }
            
            out.push(IR.withDecl(
                isConsequential ? flag.decl : IR.NO_DECL,
                IR.seq(execChildren),
            ));
            
            return [
                IR.withDecl(this.declareChildren(ctx), IR.seq(out)),
                ctx,
            ];
        }
    }
    
    export class Stmt_Limit extends Stmt_Modified<ASG.LimitStmt> {
        readonly limitVar: IR.MutVarDecl;
        readonly initLate: boolean;
        
        constructor(stmt: ASG.LimitStmt, c: Compiler) {
            super(stmt, c);
            this.initLate = !ASG.exprIsGridIndependent(stmt.limit);
            this.limitVar = c.ir.varDecl('limit', IR.INT_TYPE);
        }
        
        private _init(ctx: CompilerContext): IR.Stmt {
            return IR.assign(this.limitVar.name, '=', ctx.expr(this.stmt.limit));
        }
        
        declareParent(ctx: CompilerContext): IR.StmtLevelDecl {
            return IR.multiDecl([
                this.initLate ? this.limitVar : IR.initDecl(this.limitVar, this._init(ctx)),
                this.child.declareParent(ctx),
            ]);
        }
        
        compileReset(ctx: CompilerContext): IR.Stmt {
            const child = this.child.compileReset(ctx);
            
            return this.initLate ? IR.seq([this._init(ctx), child]) : child;
        }
        
        compileResetTransparent(ctx: CompilerContext): IR.Stmt {
            return this.child.compileReset(ctx);
        }
        
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const limit = this.limitVar.name;
            const [r, newCtx] = this.child.compile(ctx, IR.seq([
                IR.assign(limit, '-=', IR.ONE),
                ifTrue,
            ]), ifFalse);
            
            return [
                IR.if_(IR.OP.gt(limit, IR.ZERO), r, ifFalse),
                newCtx,
            ];
        }
        
        compileTransparent(ctx: CompilerContext, ifTrue: IR.Stmt): StmtCompileResult {
            const init = ctx.expr(this.stmt.limit);
            if(init === IR.ONE) {
                return this.child.compile(ctx, ifTrue, IR.PASS);
            }
            
            const c = ctx.c;
            const [r, newCtx] = this.child.compile(ctx, ifTrue, IR.BREAK);
            
            return [
                c.ir.withConst('max', IR.INT_TYPE, init, max =>
                    c.ir.forRange('i', IR.ZERO, max, () => IR.seq([
                        ctx.c.checkMaxIterations,
                        r,
                    ])),
                ),
                newCtx,
            ];
        }
    }
}
