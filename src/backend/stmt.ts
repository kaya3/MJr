///<reference path="./stmt_all.ts"/>
///<reference path="./stmt_convchain.ts"/>
///<reference path="./stmt_convolution.ts"/>
///<reference path="./stmt_one.ts"/>
///<reference path="./stmt_put.ts"/>

namespace Compiler {
    export interface StmtCompiler {
        declare?(): IR.Stmt;
        compileReset?(c: Compiler): IR.Stmt;
        compile(c: Compiler, ifTrue: IR.Stmt, ifFalse: IR.Stmt): IR.Stmt;
    }
    
    export class Stmt_Reset implements StmtCompiler {
        constructor(
            private readonly flagID: number,
            private readonly childCompilers: readonly StmtCompiler[],
            private readonly limitIDs: readonly number[],
        ) {}
        
        compile(c: Compiler): IR.Stmt {
            const out: IR.Stmt[] = [c.flags.clear(this.flagID)];
            for(const comp of this.childCompilers) {
                const r = comp?.compileReset?.(c);
                if(r !== undefined) { out.push(r); }
            }
            for(const limitID of this.limitIDs) {
                out.push(c.limits.reset(limitID, c));
            }
            return IR.block(out);
        }
    }
    
    class Stmt_NotSupported implements StmtCompiler {
        constructor(private readonly stmt: ASG.Statement) {}
        
        compile(c: Compiler): IR.Stmt {
            const {kind, pos} = this.stmt;
            c.notSupported(`'${kind}'`, pos);
            return IR.PASS;
        }
    }
    
    class Stmt_Assign implements StmtCompiler {
        constructor(private readonly stmt: ASG.AssignStmt) {}
        
        compile(c: Compiler) {
            const {variable, rhs} = this.stmt;
            return IR.assign(IR.NAMES.variable(variable), '=', c.expr(rhs));
        }
    }
    
    class Stmt_Log implements StmtCompiler {
        constructor(private readonly stmt: ASG.LogStmt) {}
        
        compile(c: Compiler) {
            const {expr} = this.stmt;
            return IR.log(c.expr(expr));
        }
    }
    
    class Stmt_Use implements StmtCompiler {
        constructor(private readonly stmt: ASG.UseStmt) {}
        
        compile(c: Compiler) {
            const {grid} = this.stmt;
            return c.config.animate ? c.grids[grid].yield_() : fail();
        }
    }
    
    type StmtKind = (ASG.BranchingStmt | ASG.NonBranchingStmt)['kind']
    type StmtCompileClass<K extends StmtKind> = new (stmt: Extract<ASG.Statement, {readonly kind: K}>, stmtID: number, c: Compiler) => StmtCompiler
    
    export const STMT_COMPILERS: {readonly [K in StmtKind]: StmtCompileClass<K>} = {
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
}
