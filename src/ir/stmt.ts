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
    export const BLANK_LINE: BlankLineStmt = {kind: 'stmt.blankline', flags: NodeFlags.DO_NOTHING};
    export const BREAK: BreakStmt = {kind: 'stmt.break', flags: NodeFlags.DO_NOTHING & ~NodeFlags.NO_BREAKS};
    export const CONTINUE: ContinueStmt = {kind: 'stmt.continue', flags: NodeFlags.DO_NOTHING & ~NodeFlags.NO_BREAKS};
    export const PASS: PassStmt = {kind: 'stmt.pass', flags: NodeFlags.DO_NOTHING};
    
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
        
        const flags = left.flags & right.flags & ~NodeFlags.NO_STATE_CHANGES;
        return {kind: 'stmt.assign', op, left, right, flags};
    }
    export function block(children: readonly Stmt[]): Stmt {
        children = children.flatMap(c =>
            c.kind === 'stmt.block' ? c.children
            : c === PASS ? []
            : [c]
        );
        return children.length === 0 ? PASS
            : children.length === 1 ? children[0]
            : {kind: 'stmt.block', children, flags: _reduceFlags(NodeFlags.DO_NOTHING, children)};
    }
    export function comment(comment: string): CommentStmt {
        return {kind: 'stmt.comment', comment, flags: NodeFlags.DO_NOTHING};
    }
    export function declFunc(name: NameExpr, yields: IRType | undefined, params: readonly NameExpr[], paramTypes: readonly IRType[], returnType: IRType, body: Stmt): DeclFuncStmt {
        return {kind: 'stmt.decl.func', name, yields, params, paramTypes, returnType, body, flags: NodeFlags.DO_NOTHING};
    }
    export function declVar(name: NameExpr, type: IRType, initialiser?: Expr, mutable: boolean = false): Stmt {
        return declVars([{name, type, initialiser}], mutable);
    }
    export function declVars(decls: readonly VarDecl[], mutable: boolean = false): Stmt {
        let flags = NodeFlags.DO_NOTHING & ~NodeFlags.NO_STATE_CHANGES;
        for(const decl of decls) {
            if(decl.initialiser !== undefined) {
                flags &= decl.initialiser.flags;
            }
        }
        return decls.length > 0 ? {kind: 'stmt.decl.vars', decls, mutable, flags} : PASS;
    }
    export function forRange(index: NameExpr, low: Expr, high: Expr, stmts: readonly Stmt[], reverse: boolean = false): Stmt {
        const body = block(stmts);
        const flags = body.flags | NodeFlags.NO_BREAKS;
        return body === PASS ? PASS : {kind: 'stmt.for.range', index, low, high, reverse, body, flags};
    }
    export function forRangeReverse(index: NameExpr, low: Expr, high: Expr, stmts: readonly Stmt[]): Stmt {
        return forRange(index, low, high, stmts, true);
    }
    export function if_(condition: Expr, then: Stmt, otherwise?: Stmt): Stmt {
        if(exprHasSideEffects(condition)) { fail(condition); }
        
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
        } else if(then.kind === 'stmt.if' && equals(then.otherwise, otherwise)) {
            // replace `if(c1) { if(c2) A else B } else B` with `if(c1 && c2) A else B`
            return if_(OP.and(condition, then.condition), then.then, otherwise);
        } else if(then.kind === 'stmt.if' && equals(then.then, otherwise)) {
            // replace `if(c1) { if(c2) B else A } else B` with `if(c1 && !c2) A else B`
            return if_(OP.and(condition, OP.not(then.condition)), then.otherwise ?? PASS, otherwise);
        } else if(then.kind === 'stmt.block' && otherwise !== undefined && otherwise.kind === 'stmt.block') {
            // replace `if(c) { ...; A } else { ...; A }` with `if(c) { ... } else { ... } A`
            // moving from the start of each block instead of the end would be unsound, since it could change the condition's value
            const thenC = then.children,
                otherC = otherwise.children;
            let i = thenC.length,
                j = otherC.length;
            while(i > 0 && j > 0 && equals(thenC[i - 1], otherC[j - 1])) {
                --i; --j;
            }
            if(i < thenC.length) {
                return block([
                    if_(condition, block(thenC.slice(0, i)), block(otherC.slice(0, j))),
                    ...thenC.slice(i),
                ]);
            }
        }
        
        let flags = condition.flags & then.flags;
        if(otherwise !== undefined) { flags &= otherwise.flags; }
        return {kind: 'stmt.if', condition, then, otherwise, flags};
    }
    
    export function exprStmt(expr: Expr): Stmt {
        return exprHasSideEffects(expr) ? {kind: 'stmt.expr', expr, flags: expr.flags} : PASS;
    }
    
    export function libFunctionCallStmt(f: LibFunction, args: readonly Expr[]): Stmt {
        return exprStmt(libFunctionCall(f, args));
    }
    export function libMethodCallStmt<K extends LibClass>(className: K, name: LibMethod<K>, obj: Expr, args: readonly Expr[]): Stmt {
        return exprStmt(libMethodCall(className, name, obj, args));
    }
    export function localCallStmt(f: NameExpr, args: readonly Expr[]): Stmt {
        return exprStmt(localCall(f, args));
    }
    export function log(expr: Expr): LogStmt {
        const flags = expr.flags & ~NodeFlags.NO_OUTPUT;
        return {kind: 'stmt.log', expr, flags};
    }
    export function preamble(paramTypes: DictType, emitChecks: boolean, libVersion: number, opsUsed: readonly Op[]): PreambleStmt {
        return {kind: 'stmt.preamble', paramTypes, emitChecks, libVersion, opsUsed, flags: NodeFlags.LOCALLY_DETERMINISTIC};
    }
    export function return_(expr?: Expr): ReturnStmt {
        const flags = (expr !== undefined ? expr.flags : NodeFlags.DO_NOTHING) & ~NodeFlags.NO_RETURNS;
        return {kind: 'stmt.return', expr, flags};
    }
    export function switch_(expr: Expr, casesByIndex: readonly Stmt[], optimise: boolean = false): Stmt {
        // TODO: optimisation depends on referential transparency
        if(exprHasSideEffects(expr)) { fail(expr); }
        
        if(casesByIndex.length === 0) {
            return PASS;
        } else if(casesByIndex.length === 1) {
            return casesByIndex[0];
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
            
            if((c.flags & NodeFlags.NO_BREAKS) === 0) { fail(c); }
            
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
        
        return switchCases(expr, Array.from(map.values()), exhaustive);
    }
    
    function _eqAny(expr: Expr, values: readonly number[]): Expr {
        return values.length > 1 && values.every(v => v < values.length)
            ? OP.lt(expr, int(values.length))
            : values.map(v => OP.eq(expr, int(v))).reduce(OP.or, FALSE);
    }
    export function switchCases(expr: Expr, cases: readonly Case[], exhaustive: boolean): Stmt {
        if(exprHasSideEffects(expr)) { fail(expr); }
        
        if(cases.length === 0) {
            return PASS;
        } else if(cases.length === 1) {
            const a = cases[0];
            return exhaustive ? a.then : if_(_eqAny(expr, a.values), a.then);
        } else if(cases.length === 2 && exhaustive) {
            const a = cases[0], b = cases[1];
            return if_(_eqAny(expr, a.values), a.then, b.then);
        }
        
        let flags = expr.flags;
        for(const c of cases) { flags &= c.then.flags; }
        
        return cases.length === 0 ? PASS
            : cases.length === 1 && exhaustive ? cases[0].then
            : cases.length <= 2 && cases[0].values.length === 1 ? if_(OP.eq(expr, int(cases[0].values[0])), cases[0].then, cases.length === 2 ? cases[1].then : undefined)
            : {kind: 'stmt.switch', expr, cases, exhaustive, flags};
    }
    export function throw_(message: string): ThrowStmt {
        return {kind: 'stmt.throw', message, flags: NodeFlags.DO_NOTHING & ~NodeFlags.NO_THROWS};
    }
    export function while_(condition: Expr, then: Stmt): Stmt {
        if(exprHasSideEffects(condition)) { fail(condition); }
        
        if(then === BREAK) {
            return PASS;
        }
        if(then.kind === 'stmt.if') {
            if(then.then === BREAK) {
                condition = OP.and(condition, OP.not(then.condition));
                then = then.otherwise ?? PASS;
            } else if(then.otherwise === BREAK) {
                condition = OP.and(condition, then.condition);
                then = then.then;
            }
        }
        if(then.kind === 'stmt.block' && then.children[then.children.length - 1] === BREAK) {
            const r = block(then.children.slice(0, then.children.length - 1));
            if((r.flags & NodeFlags.NO_BREAKS) !== 0) {
                return r;
            }
        }
        
        // the compiler won't ever output an infinite loop that does nothing; if the loop body is empty, then the condition must be false
        const flags = condition.flags & (then.flags | NodeFlags.NO_BREAKS);
        return then === PASS || then === BREAK ? PASS
            : {kind: 'stmt.while', condition, then, flags};
    }
    export function yield_(expr?: Expr): YieldStmt {
        const flags = (expr !== undefined ? expr.flags : NodeFlags.DO_NOTHING) & ~NodeFlags.NO_OUTPUT;
        return {kind: 'stmt.yield', expr, flags};
    }
}
