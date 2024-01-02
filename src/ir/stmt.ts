///<reference path="./ir.ts"/>
///<reference path="./info.ts"/>

namespace IR {
    export const DEFAULT_ROW_LENGTH = 16;
    
    // singletons
    export const BLANK_LINE: BlankLineStmt = {kind: 'stmt.blankline', info: Info.DO_NOTHING};
    export const BREAK: BreakStmt = {kind: 'stmt.break', info: Info.BREAK};
    export const CONTINUE: ContinueStmt = {kind: 'stmt.continue', info: Info.BREAK};
    export const PASS: PassStmt = {kind: 'stmt.pass', info: Info.DO_NOTHING};
    
    export function assign(left: MutNameExpr | AttrExpr | ArrayAccessExpr, op: AssignOp, right: Expr): Stmt {
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
        
        let info = left.kind === 'expr.name' && op === '='
            ? Info.DO_NOTHING
            : left.info;
        info = info.then(right.info)
            .then(left.kind === 'expr.name' ? Info.varSet(left.id) : Info.STATE_UPDATE);
        return {kind: 'stmt.assign', op, left, right, info};
    }
    export function seq(stmts: readonly Stmt[]): Stmt {
        const children: Stmt[] = [];
        for(const stmt of stmts) {
            if(stmt.kind === 'stmt.sequence') {
                children.push(...stmt.children);
            } else if(stmt !== PASS) {
                children.push(stmt);
            }
            if(!stmt.info.canCompleteNormally()) { break; }
        }
        
        return children.length === 0 ? PASS
            : children.length === 1 ? children[0]
            : {kind: 'stmt.sequence', children, info: Info.seq(children)};
    }
    export function comment(comment: string): CommentStmt {
        return {kind: 'stmt.comment', comment, info: Info.DO_NOTHING};
    }
    export function exportDecl(decl: ConstDecl | FuncDecl): ExportStmt {
        return {kind: 'stmt.export', decl, info: Info.DO_NOTHING};
    }
    export function forRange(index: LoopVarDecl, low: Expr, high: Expr, body: Stmt, reverse: boolean = false): Stmt {
        if(body === PASS) {
            return PASS;
        } else if(body.kind === 'stmt.if' && !body.condition.info.hasSideEffects() && body.condition.info.isLocallyDeterministic() && !body.condition.info.canUseVar(index.name)) {
            // replace `for if(cond) A else B` with `if(cond) { for A } else { for B }`
            // only allowed if cond is safe to repeatedly evaluate and doesn't involve loop variable
            return if_(
                body.condition,
                forRange(index, low, high, body.then, reverse),
                forRange(index, low, high, body.otherwise ?? PASS, reverse), // otherwise will usually be trivial
            );
        } else if(body.kind === 'stmt.decl' && !body.decl.info.hasSideEffects() && body.decl.info.isLocallyDeterministic() && !body.decl.info.canUseVar(index.name)) {
            // replace `for decl in A` with `decl in for A`
            // only allowed if decl is safe to repeatedly evaluate and doesn't involve loop variable
            return withDecl(body.decl, forRange(index, low, high, body.child, reverse));
        }
        
        let info = body.info.asLoopBody(false);
        if(!isInt(low) || !isInt(high)) {
            // possible that body executes 0 times
            info = info.or(Info.DO_NOTHING);
        } else if(low.value === high.value) {
            return PASS;
        } else if(low.value + 1 === high.value && !body.info.canBreakOrContinue()) {
            return withConst(index.name, INT_TYPE, low, body);
        }
        
        if(!body.info.canCompleteNormally() && !body.info.canBreakOrContinue()) {
            const first = reverse ? OP.minusConstant(high, 1) : low;
            return if_(OP.lt(low, high), withConst(index.name, INT_TYPE, first, body));
        }
        
        info = low.info
            .then(high.info)
            .then(info.asVarDecl(index.name));
        return {kind: 'stmt.for.range', index, low, high, reverse, body, info};
    }
    export function forRangeReverse(index: LoopVarDecl, low: Expr, high: Expr, body: Stmt): Stmt {
        return forRange(index, low, high, body, true);
    }
    export function if_(condition: Expr, then: Stmt, otherwise?: Stmt): Stmt {
        if(condition.info.hasSideEffects()) { fail('if statement condition must not have side-effects', condition); }
        
        if(otherwise === undefined) { otherwise = PASS; }
        
        // assumes condition is pure
        if(condition === TRUE) {
            return then;
        } else if(condition === FALSE) {
            return otherwise;
        } else if(equals(then, otherwise)) {
            return then;
        } else if(then === PASS) {
            // otherwise !== PASS by above
            return if_(OP.not(condition), otherwise);
        } else if(then.kind === 'stmt.assign' && otherwise.kind === 'stmt.assign' && equals(then.left, otherwise.left) && then.op === otherwise.op) {
            // replace `if(c) { x = a; } else { x = b; }` with `x = c ? a : b;`
            return assign(then.left, then.op, ternary(condition, then.right, otherwise.right));
        } else if(then.kind === 'stmt.for.range' && (equals(condition, OP.lt(then.low, then.high)) || equals(condition, OP.gt(then.high, then.low))) && otherwise === PASS) {
            // omit redundant `if` statement guarding a `for` loop
            return then;
        } else if(then.kind === 'stmt.if' && equals(then.otherwise, otherwise)) {
            // replace `if(c1) { if(c2) A else B } else B` with `if(c1 && c2) A else B`
            return if_(OP.and(condition, then.condition), then.then, otherwise);
        } else if(then.kind === 'stmt.if' && equals(then.then, otherwise)) {
            // replace `if(c1) { if(c2) B else A } else B` with `if(c1 && !c2) A else B`
            return if_(OP.and(condition, OP.not(then.condition)), then.otherwise ?? PASS, otherwise);
        } else if(then.kind === 'stmt.sequence' && otherwise.kind === 'stmt.sequence') {
            // replace `if(c) { ...; A } else { ...; A }` with `if(c) { ... } else { ... } A`
            const thenC = then.children,
                otherC = otherwise.children;
            let i = 0,
                j = thenC.length,
                k = otherC.length;
            // moving the condition forwards is only sound if it is time-independent
            while(i < thenC.length && i < otherC.length && condition.info.commutesWith(thenC[i]) && equals(thenC[i], otherC[i])) {
                ++i;
            }
            while(j > i && k > i && equals(thenC[j - 1], otherC[k - 1])) {
                --j; --k;
            }
            if(i > 0 || j < thenC.length) {
                return seq([
                    ...thenC.slice(0, i),
                    if_(condition, seq(thenC.slice(i, j)), seq(otherC.slice(i, k))),
                    ...thenC.slice(j),
                ]);
            }
        }
        
        if(!then.info.canCompleteNormally() && otherwise !== PASS) {
            return seq([if_(condition, then), otherwise]);
        } else if(!otherwise.info.canCompleteNormally()) {
            return seq([if_(OP.not(condition), otherwise), then]);
        }
        
        const info = condition.info
            .then(then.info.or(otherwise.info));
        if(otherwise === PASS) { otherwise = undefined; }
        return {kind: 'stmt.if', condition, then, otherwise, info};
    }
    
    export function exprStmt(expr: Expr): Stmt {
        return expr.info.hasSideEffects() ? {kind: 'stmt.expr', expr, info: expr.info} : PASS;
    }
    
    export function libFunctionCallStmt(f: LibFunction, args: readonly Expr[]): Stmt {
        return exprStmt(libFunctionCall(f, args));
    }
    export function libMethodCallStmt<K extends LibClass>(className: K, name: LibMethod<K>, obj: Expr, args: readonly Expr[]): Stmt {
        return exprStmt(libMethodCall(className, name, obj, args));
    }
    export function localCallStmt(f: ConstNameExpr, args: readonly Expr[]): Stmt {
        return exprStmt(localCall(f, args));
    }
    export function log(expr: Expr): LogStmt {
        return {kind: 'stmt.log', expr, info: Info.OUTPUT};
    }
    export function preamble(paramTypes: DictType, emitChecks: boolean, libVersion: number, opsUsed: readonly Op[]): PreambleStmt {
        return {kind: 'stmt.preamble', paramTypes, emitChecks, libVersion, opsUsed, info: Info.PREAMBLE};
    }
    export function return_(expr?: Expr): ReturnStmt {
        const info = expr !== undefined ? expr.info.then(Info.RETURN) : Info.RETURN;
        return {kind: 'stmt.return', expr, info};
    }
    export function switch_(expr: Expr, casesByIndex: readonly Stmt[], optimise: boolean = false): Stmt {
        // TODO: some transformations depend on expr being locally constant, since we repeat it in some conditions, or replace it with its value when optimising
        if(expr.info.hasSideEffects()) { fail('switch statement scrutinee must not have side-effects', expr); }
        
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
            
            if(c.info.canBreakOrContinue()) { fail('switch statement cases cannot contain break/continue', c); }
            
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
            : OP.some(values, v => OP.eq(expr, int(v)));
    }
    export function switchCases(expr: Expr, cases: readonly Case[], exhaustive: boolean): Stmt {
        if(expr.info.hasSideEffects()) { fail('switch statement scrutinee must not have side-effects', expr); }
        
        if(cases.length === 0) {
            return PASS;
        } else if(cases.length === 1) {
            const a = cases[0];
            return exhaustive ? a.then : if_(_eqAny(expr, a.values), a.then);
        } else if(cases.length === 2 && exhaustive) {
            const a = cases[0], b = cases[1];
            return if_(_eqAny(expr, a.values), a.then, b.then);
        }
        
        let info = exhaustive ? Info.UNREACHABLE : Info.DO_NOTHING;
        for(const c of cases) { info = info.or(c.then.info); }
        info = expr.info.then(info);
        return {kind: 'stmt.switch', expr, cases, exhaustive, info};
    }
    export function throw_(message: string): ThrowStmt {
        return {kind: 'stmt.throw', message, info: Info.THROW};
    }
    export function while_(condition: Expr, then: Stmt): Stmt {
        if(condition.info.hasSideEffects()) { fail('while statement condition must not have side-effects', condition); }
        
        if(then.kind === 'stmt.if') {
            if(then.then === BREAK) {
                return while_(
                    OP.and(condition, OP.not(then.condition)),
                    then.otherwise ?? PASS,
                );
            } else if(then.otherwise === BREAK) {
                return while_(
                    OP.and(condition, then.condition),
                    then.then,
                );
            }
        } else if(then.kind === 'stmt.sequence') {
            const firstChild = then.children[0],
                lastChild = then.children[then.children.length - 1];
            
            if(firstChild.kind === 'stmt.if') {
                if(firstChild.then === BREAK) {
                    return while_(
                        OP.and(condition, OP.not(firstChild.condition)),
                        seq([firstChild.otherwise ?? PASS, ...then.children.slice(1)]),
                    );
                } else if(firstChild.otherwise === BREAK) {
                    return while_(
                        OP.and(condition, firstChild.condition),
                        seq([firstChild.then, ...then.children.slice(1)]),
                    );
                }
            }
            
            if(lastChild === BREAK) {
                const r = seq(then.children.slice(0, then.children.length - 1));
                if(!r.info.canBreakOrContinue()) {
                    return r;
                }
            } else if(lastChild === CONTINUE) {
                const r = seq(then.children.slice(0, then.children.length - 1));
                return while_(condition, r);
            }
        }
        
        if(condition === FALSE || then == BREAK) {
            return PASS;
        } else if(then === PASS || then === CONTINUE) {
            // the compiler shouldn't ever output an infinite loop that does nothing
            fail(`empty while loop should have impossible condition`, condition);
        } else if(!then.info.canBreakOrContinue() && !then.info.canCompleteNormally()) {
            // if the body won't finish then the loop won't iterate more than once
            return if_(condition, then);
        }
        
        const info = condition.info
            .then(then.info.asLoopBody(condition === TRUE));
        return {kind: 'stmt.while', condition, then, info};
    }
    export function withDecls(decls: readonly StmtLevelDecl[], child: Stmt): Stmt {
        for(let i = decls.length - 1; i >= 0; --i) {
            child = withDecl(decls[i], child);
        }
        return child;
    }
    export function withDecl(decl: StmtLevelDecl, child: Stmt): Stmt {
        switch(decl.kind) {
            case 'decl.none':
                return child;
            case 'decl.multi':
                return withDecls(decl.children, child);
            case 'decl.init':
                return withDecl(decl.child, seq([decl.stmt, child]));
            case 'decl.var.const':
                return withConst(decl.name, decl.type, decl.initialiser, child);
            case 'decl.var.mut':
                return withVar(decl.name, decl.type, decl.initialiser, child);
            case 'decl.func':
                return withFunc(decl, child);
        }
    }
    export function withConst(name: ConstNameExpr, type: IRType, initialiser: Expr, child: Stmt): Stmt {
        if(!child.info.canGetVar(name)) {
            return seq([exprStmt(initialiser), child]);
        } else if(isSimpleConstant(initialiser)) {
            return replace(child, name, initialiser);
        } else if(!initialiser.info.hasSideEffects() && child.kind === 'stmt.if' && child.otherwise === undefined && !child.condition.info.canUseVar(name) && child.condition.info.commutesWith(initialiser)) {
            // replace `const x = A; if(B) C` with `if(B) { const x = A; C }
            return if_(child.condition, withConst(name, type, initialiser, child.then));
        }
        
        const info = initialiser.info
            .then(child.info.asVarDecl(name));
        return {kind: 'stmt.decl', decl: {kind: 'decl.var.const', name, type, initialiser, info: initialiser.info}, child, info};
    }
    export function withVar(name: MutNameExpr, type: IRType, initialiser: Expr | undefined, child: Stmt): Stmt {
        if(!child.info.canGetVar(name)) {
            // TODO: remove assignment statements from child
            if(!child.info.canSetVar(name)) {
                return initialiser === undefined ? child : seq([exprStmt(initialiser), child])
            }
        } else if(!child.info.canSetVar(name)) {
            const constName: ConstNameExpr = {kind: 'expr.name', id: name.id, namePart: name.namePart, isMutable: false, info: Info.constGet(name.id)};
            return withConst(constName, type, initialiser ?? fail(), replace(child, name, constName));
        }
        
        if(initialiser === undefined && child.kind === 'stmt.sequence') {
            const firstChild = child.children[0];
            if(firstChild.kind === 'stmt.assign' && firstChild.op === '=' && equals(firstChild.left, name)) {
                return withVar(name, type, firstChild.right, seq(child.children.slice(1)));
            }
        }
        
        let info = child.info.asVarDecl(name);
        if(initialiser !== undefined) { info = initialiser.info.then(info); }
        return {kind: 'stmt.decl', decl: {kind: 'decl.var.mut', name, type, initialiser, info: initialiser !== undefined ? initialiser.info : Info.DO_NOTHING}, child, info};
    }
    export function withFunc(decl: FuncDecl, child: Stmt): Stmt {
        if(!child.info.canGetVar(decl.name)) {
            return child;
        }
        return {kind: 'stmt.decl', decl, child, info: decl.info.then(child.info)};
    }
    export function yield_(expr?: Expr): YieldStmt {
        const info = expr !== undefined ? expr.info.then(Info.OUTPUT) : Info.OUTPUT;
        return {kind: 'stmt.yield', expr, info};
    }
}
