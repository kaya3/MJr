namespace Compiler {
    export abstract class StmtCompiler<T extends ASG.Statement = ASG.Statement> {
        constructor(
            protected readonly stmt: T,
        ) {}
        
        declare(): IR.Stmt {
            return IR.PASS;
        }
        compileReset(c: Compiler): IR.Stmt {
            return IR.PASS;
        }
        
        abstract compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt): IR.Stmt;
    }
    
    export class Stmt_NotSupported extends StmtCompiler {
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt): IR.Stmt {
            const {kind, pos} = this.stmt;
            c.notSupported(`'${kind}'`, pos);
            return ifFalse;
        }
    }
    
    export class Stmt_Assign extends StmtCompiler<ASG.AssignStmt> {
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt) {
            const {variable, rhs} = this.stmt;
            return IR.block([
                IR.assign(IR.NAMES.variable(variable), '=', c.expr(rhs)),
                ifFalse,
            ]);
        }
    }
    
    export class Stmt_Log extends StmtCompiler<ASG.LogStmt> {
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt) {
            const {expr} = this.stmt;
            return IR.block([
                IR.log(c.expr(expr)),
                ifFalse,
            ]);
        }
    }
    
    export class Stmt_Use extends StmtCompiler<ASG.UseStmt> {
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt) {
            const {grid} = this.stmt;
            return IR.block([
                c.config.animate ? c.grids[grid].yield_() : IR.PASS,
                ifFalse,
            ]);
        }
    }
    
    export abstract class Stmt_Block<T extends ASG.BlockStmt> extends StmtCompiler<T> {
        protected readonly children: readonly StmtCompiler[];
        
        constructor(stmt: T, protected stmtID: number, c: Compiler) {
            super(stmt);
            this.children = stmt.children.map(c.getStmtCompiler);
        }
        
        declare(): IR.Stmt {
            return IR.block(this.children.map(child => child.declare()));
        }
    }
    
    export abstract class Stmt_Modified<T extends ASG.ModifiedStmt> extends StmtCompiler<T> {
        protected readonly child: StmtCompiler;
        
        constructor(stmt: T, protected stmtID: number, c: Compiler) {
            super(stmt);
            this.child = c.getStmtCompiler(stmt.child);
        }
        
        declare(): IR.Stmt {
            return this.child.declare();
        }
    }
    
    export class Stmt_Markov extends Stmt_Block<ASG.MarkovStmt> {
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt): IR.Stmt {
            const isConsequential = ifTrue !== IR.PASS || ifFalse !== IR.PASS;
            
            const flag = IR.NAMES.otherVar(this.stmtID, 'flag'),
                declFlag = isConsequential ? IR.declVar(flag, IR.BOOL_TYPE, IR.FALSE, true) : IR.PASS,
                setFlagAndContinue = IR.block([
                    isConsequential ? IR.assign(flag, '=', IR.TRUE) : IR.PASS,
                    IR.CONTINUE,
                ]);
            
            return IR.block([
                ...this.children.map(child => child.compileReset(c)),
                declFlag,
                IR.while_(IR.TRUE, IR.block([
                    c.checkMaxIterations,
                    ...this.children.map(child => child.compile(c, setFlagAndContinue, IR.PASS)),
                    IR.BREAK,
                ])),
                isConsequential ? IR.if_(flag, ifTrue, ifFalse) : IR.PASS,
            ]);
        }
    }
    
    export class Stmt_Sequence extends Stmt_Block<ASG.SequenceStmt> {
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt): IR.Stmt {
            const isConsequential = ifTrue !== IR.PASS || ifFalse !== IR.PASS;
            
            const flag = IR.NAMES.otherVar(this.stmtID, 'flag'),
                declFlag = isConsequential ? IR.declVar(flag, IR.BOOL_TYPE, IR.FALSE, true) : IR.PASS,
                setFlag = isConsequential ? IR.assign(flag, '=', IR.TRUE) : IR.PASS;
            
            function _resetChild(child: StmtCompiler): IR.Stmt {
                return child instanceof Stmt_Limit
                    ? child.compileResetTransparent(c)
                    : child.compileReset(c);
            }
            
            function _compileChild(child: StmtCompiler): IR.Stmt {
                return child instanceof Stmt_Limit
                    ? child.compileTransparent(c, setFlag)
                    : IR.while_(IR.TRUE, IR.block([
                        c.checkMaxIterations,
                        child.compile(c, setFlag, IR.BREAK),
                    ]));
            }
            
            return IR.block([
                ...this.children.map(_resetChild),
                declFlag,
                ...this.children.map(_compileChild),
                isConsequential ? IR.if_(flag, ifTrue, ifFalse) : IR.PASS,
            ]);
        }
    }
    
    export class Stmt_Limit extends Stmt_Modified<ASG.LimitStmt> {
        readonly limitVar: IR.NameExpr;
        
        constructor(stmt: ASG.LimitStmt, stmtID: number, c: Compiler) {
            super(stmt, stmtID, c);
            this.limitVar = IR.NAMES.limit(stmtID);
        }
        
        compileReset(c: Compiler): IR.Stmt {
            const init = c.expr(this.stmt.limit);
            return IR.block([
                IR.declVar(this.limitVar, IR.INT_TYPE, init, true),
                this.child.compileReset(c),
            ]);
        }
        
        compileResetTransparent(c: Compiler): IR.Stmt {
            return this.child.compileReset(c);
        }
        
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt): IR.Stmt {
            return IR.if_(
                IR.OP.gt(this.limitVar, IR.ZERO),
                this.child.compile(c, IR.block([
                    IR.assign(this.limitVar, '-=', IR.ONE),
                    ifTrue,
                ]), ifFalse),
                ifFalse,
            );
        }
        
        compileTransparent(c: Compiler, ifTrue: IR.Stmt): IR.Stmt {
            const init = c.expr(this.stmt.limit);
            if(init === IR.ONE) {
                return this.child.compile(c, ifTrue, IR.PASS);
            }
            
            const loopBody = [c.checkMaxIterations, this.child.compile(c, ifTrue, IR.BREAK)],
                i = IR.NAMES.otherVar(this.stmtID, 'i');
            return IR.isInt(init)
                ? IR.forRange(i, IR.ZERO, init, loopBody)
                : IR.block([
                    IR.declVar(this.limitVar, IR.INT_TYPE, init),
                    IR.forRange(i, IR.ZERO, this.limitVar, loopBody),
                ]);
        }
    }
}
