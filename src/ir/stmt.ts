/**
 * Abstract syntax tree for a high-level intermediate representation (IR) which
 * is output by the compiler. The IR is converted into output source code by a
 * class in the `CodeGen` namespace.
 * 
 * The IR should be convertible to most imperative languages, so long as they
 * allow local function declarations to access constants from the outer scope.
 */
namespace IR {
    export const DEFAULT_ROW_LENGTH = 16;
    
    export function constArrayDecl(name: NameExpr, from: readonly number[], domainSize: number, rowLength: number = DEFAULT_ROW_LENGTH): VarDeclWithInitialiser {
        return {
            name,
            type: constArrayType(domainSize),
            initialiser: constArray(from, domainSize, rowLength),
        };
    }
    export function newArrayDecl(name: NameExpr, length: Expr, domainSize: number): VarDeclWithInitialiser {
        return {
            name,
            type: mutableArrayType(domainSize),
            initialiser: newArray(length, domainSize),
        };
    }
    
    // singletons
    export const BLANK_LINE: BlankLineStmt = {kind: 'stmt.blankline'};
    export const BREAK: BreakStmt = {kind: 'stmt.break'};
    export const CONTINUE: ContinueStmt = {kind: 'stmt.continue'};
    export const PASS: PassStmt = {kind: 'stmt.pass'};
    
    export function assign(left: NameExpr | AttrExpr | ArrayAccessExpr, op: AssignOp, right: Expr): Stmt {
        if(isInt(right)) {
            if(right.value === 0) {
                if(op === '+=' || op === '-=' || op === '|=') {
                    return PASS;
                } else if(op === '&=') {
                    op = '=';
                }
            } else if(right.value < 0 && (op === '+=' || op === '-=')) {
                op = op === '+=' ? '-=' : '+=';
                right = int(-right.value);
            }
        } else if(right.kind === 'expr.op.unary' && right.op === 'int_uminus' && (op === '+=' || op === '-=')) {
            op = op === '+=' ? '-=' : '+=';
            right = right.child;
        }
        return {kind: 'stmt.assign', op, left, right};
    }
    export function block(children: readonly Stmt[]): Stmt {
        children = children.flatMap(c =>
            c.kind === 'stmt.block' ? c.children
            : c === PASS ? []
            : [c]
        );
        return children.length === 0 ? PASS
            : children.length === 1 ? children[0]
            : {kind: 'stmt.block', children};
    }
    export function comment(comment: string): CommentStmt {
        return {kind: 'stmt.comment', comment};
    }
    export function declFunc(name: NameExpr, yields: IRType | undefined, params: readonly NameExpr[], paramTypes: readonly IRType[], returnType: IRType, body: Stmt): DeclFuncStmt {
        return {kind: 'stmt.decl.func', name, yields, params, paramTypes, returnType, body};
    }
    export function declVar(name: NameExpr, type: IRType, initialiser?: Expr, mutable: boolean = false): Stmt {
        return declVars([{name, type, initialiser}], mutable);
    }
    export function declVars(decls: readonly VarDecl[], mutable: boolean = false): Stmt {
        return decls.length > 0 ? {kind: 'stmt.decl.vars', decls, mutable} : PASS;
    }
    export function forRange(index: NameExpr, low: Expr, high: Expr, stmts: readonly Stmt[], reverse: boolean = false): Stmt {
        const body = block(stmts);
        return body === PASS ? PASS : {kind: 'stmt.for.range', index, low, high, reverse, body};
    }
    export function forRangeReverse(index: NameExpr, low: Expr, high: Expr, stmts: readonly Stmt[]): Stmt {
        return forRange(index, low, high, stmts, true);
    }
    export function if_(condition: Expr, then: Stmt, otherwise?: Stmt): Stmt {
        if(otherwise === PASS) { otherwise = undefined; }
        
        // assumes condition is pure
        if(condition === TRUE) {
            return then;
        } else if(condition === FALSE) {
            return otherwise ?? PASS;
        } else if(equals(then, otherwise)) {
            return then;
        } else if(then === PASS) {
            return otherwise === undefined ? PASS : if_(OP.not(condition), otherwise);
        } else if(then.kind === 'stmt.assign' && otherwise !== undefined && otherwise.kind === 'stmt.assign' && equals(then.left, otherwise.left) && then.op === otherwise.op) {
            // replace `if(c) { x = a; } else { x = b; }` with `x = c ? a : b;`
            return assign(then.left, then.op, ternary(condition, then.right, otherwise.right));
        } else if(then.kind === 'stmt.for.range' && then.low === ZERO && (equals(condition, OP.gt(then.high, ZERO)) || equals(condition, OP.lt(ZERO, then.high))) && otherwise === undefined) {
            // omit redundant `if` statement guarding a `for` loop
            return then;
        } else if(then.kind === 'stmt.if' && equals(otherwise, then.otherwise)) {
            // replace `if(c1) { if(c2) A else B } else B` with `if(c1 && c2) A else B`
            return if_(OP.and(condition, then.condition), then.then, otherwise);
        } else {
            return {kind: 'stmt.if', condition, then, otherwise};
        }
    }
    export function libFunctionCallStmt(f: LibFunction, args: readonly Expr[]): ExprStmt {
        return {kind: 'stmt.expr', expr: libFunctionCall(f, args)};
    }
    export function libMethodCallStmt<K extends LibClass>(className: K, name: LibMethod<K>, obj: Expr, args: readonly Expr[]): ExprStmt {
        return {kind: 'stmt.expr', expr: libMethodCall(className, name, obj, args)};
    }
    export function localCallStmt(f: NameExpr, args: readonly Expr[]): ExprStmt {
        return {kind: 'stmt.expr', expr: localCall(f, args)};
    }
    export function log(expr: Expr): LogStmt {
        return {kind: 'stmt.log', expr};
    }
    export function preamble(paramTypes: DictType, emitChecks: boolean, libVersion: number, opsUsed: readonly Op[]): PreambleStmt {
        return {kind: 'stmt.preamble', paramTypes, emitChecks, libVersion, opsUsed};
    }
    export function return_(expr?: Expr): ReturnStmt {
        return {kind: 'stmt.return', expr};
    }
    export function switch_(expr: Expr, casesByIndex: readonly Stmt[], optimise: boolean = false): Stmt {
        if(casesByIndex.length === 0) {
            return PASS;
        } else if(casesByIndex.length === 1) {
            return casesByIndex[0];
        } else if(casesByIndex.length === 2) {
            return if_(OP.eq(expr, ZERO), casesByIndex[0], casesByIndex[1]);
        }
        
        const firstCase = casesByIndex[0];
        if(firstCase.kind === 'stmt.if' && casesByIndex.every((c: Stmt): c is IfStmt => c.kind === 'stmt.if' && equals(c.condition, firstCase.condition))) {
            // factor out common condition; the `otherwise` part will generally be trivial
            return if_(
                firstCase.condition,
                switch_(expr, casesByIndex.map(c => c.then)),
                switch_(expr, casesByIndex.map(c => c.otherwise ?? PASS)),
            );
        }
        
        // de-duplicate cases
        const map = new Map<string, {values: number[], then: Stmt}>();
        let exhaustive = true;
        for(let i = 0; i < casesByIndex.length; ++i) {
            const c = casesByIndex[i];
            if(c === PASS) { exhaustive = false; continue; }
            
            const k = key(c);
            getOrCompute(map, k, () => ({values: [], then: c})).values.push(i);
        }
        
        if(optimise) {
            for(const c of map.values()) {
                if(c.values.length === 1) {
                    c.then = replace(c.then, expr, int(c.values[0]));
                }
            }
        }
        
        const cases = Array.from(map.values());
        return cases.length === 0 ? PASS
            : cases.length === 1 && exhaustive ? firstCase
            : cases.length === 1 && cases[0].values.length === 1 ? if_(OP.eq(expr, int(cases[0].values[0])), cases[0].then)
            : {kind: 'stmt.switch', expr, cases};
    }
    export function throw_(message: string): ThrowStmt {
        return {kind: 'stmt.throw', message};
    }
    export function while_(condition: Expr, then: Stmt): Stmt {
        if(then.kind === 'stmt.if') {
            if(then.then === BREAK) {
                condition = OP.and(condition, OP.not(then.condition));
                then = then.otherwise ?? PASS;
            } else if(then.otherwise === BREAK) {
                condition = OP.and(condition, then.condition);
                then = then.then;
            }
        }
        
        // the compiler won't ever output an infinite loop that does nothing; if the loop body is empty, then the condition must be false
        return then === PASS || then === BREAK ? PASS
            : {kind: 'stmt.while', condition, then};
    }
    export function yield_(expr?: Expr): YieldStmt {
        return {kind: 'stmt.yield', expr};
    }
}
