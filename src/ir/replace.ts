namespace IR {
    export function replaceInDecl<T extends VarDecl | ExportableDecl>(decl: T, from: Expr, to: Expr): T;
    export function replaceInDecl(decl: StmtLevelDecl, from: Expr, to: Expr): StmtLevelDecl;
    export function replaceInDecl(decl: Decl, from: Expr, to: Expr): Decl {
        switch(decl.kind) {
            case 'decl.none':
            case 'decl.var.loop':
            case 'decl.var.param':
                return decl;
            
            case 'decl.func':
                return funcDecl(decl.name, decl.yields, decl.params, decl.returnType, replace(decl.body, from, to));
            case 'decl.init':
                return initDecl(replaceInDecl(decl.child, from, to), replace(decl.stmt, from, to));
            case 'decl.multi':
                return multiDecl(decl.children.map(d => replaceInDecl(d, from, to)));
            
            case 'decl.var.const': {
                const initialiser = replace(decl.initialiser, from, to);
                return {kind: 'decl.var.const', name: decl.name, type: decl.type, initialiser, info: initialiser.info};
            }
            
            case 'decl.var.mut': {
                const initialiser = decl.initialiser !== undefined ? replace(decl.initialiser, from, to) : undefined;
                const info = initialiser !== undefined ? initialiser.info : Info.DO_NOTHING;
                return {kind: 'decl.var.mut', name: decl.name, type: decl.type, initialiser, info};
            }
        }
    }
    
    export function replace(node: Expr, from: Expr, to: Expr): Expr;
    export function replace(node: Stmt, from: Expr, to: Expr): Stmt;
    export function replace(node: Stmt | Expr, from: Expr, to: Expr): Stmt | Expr {
        if(equals(node, from)) { return to; }
        
        switch(node.kind) {
            case 'stmt.blankline':
            case 'stmt.break':
            case 'stmt.comment':
            case 'stmt.continue':
            case 'stmt.pass':
            case 'stmt.preamble':
            case 'stmt.throw':
            case 'expr.array.const':
            case 'expr.literal.bool':
            case 'expr.literal.float':
            case 'expr.literal.int':
            case 'expr.literal.null':
            case 'expr.literal.str':
            case 'expr.name':
            case 'expr.unused.deferred':
            case 'expr.unused.error':
                return node;
            
            case 'expr.array.new':
                return newArray(replace(node.length, from, to), node.domainSize);
            case 'expr.attr':
                return attr(replace(node.left, from, to), node.attr);
            case 'expr.dict':
                return dict(node.type, node.values.map(v => replace(v, from, to)));
            case 'expr.letin':
                return letIn(replaceInDecl(node.decl, from, to), replace(node.child, from, to));
            case 'expr.op.binary':
                const left = replace(node.left, from, to),
                    right = replace(node.right, from, to);
                switch(node.op) {
                    case 'int_and':
                        return OP.bitwiseAnd(left, right);
                    case 'int_or':
                        return OP.bitwiseOr(left, right);
                    case 'int_xor':
                        return OP.bitwiseXor(left, right);
                    case 'int_lshift':
                        return OP.lshift(left, right);
                    case 'int_rshift':
                        return OP.rshift(left, right);
                    case 'loose_int_plus':
                        return OP.add(left, right);
                    case 'loose_int_minus':
                        return OP.minus(left, right);
                    case 'loose_int_mult':
                        return OP.mult(left, right);
                    case 'loose_int_floordiv':
                        return OP.floordiv(left, right);
                    case 'loose_int_mod':
                        return OP.mod(left, right);
                    default:
                        return binaryOp(node.op, left, right);
                }
            case 'expr.op.call.lib.constructor':
                return libConstructorCall(node.className, node.args.map(v => replace(v, from, to)));
            case 'expr.op.call.lib.function':
                return libFunctionCall(node.name, node.args.map(v => replace(v, from, to)));
            case 'expr.op.call.lib.method':
                return libMethodCall(node.className, node.name as never, replace(node.obj, from, to), node.args.map(v => replace(v, from, to)));
            case 'expr.op.call.local':
                return localCall(node.name, node.args.map(v => replace(v, from, to)), !node.info.hasSideEffects());
            case 'expr.op.ternary':
                return ternary(replace(node.condition, from, to), replace(node.then, from, to), replace(node.otherwise, from, to));
            case 'expr.op.unary':
                return unaryOp(node.op, replace(node.child, from, to));
            case 'expr.param':
                return param(node.name, replace(node.otherwise, from, to));
            
            case 'stmt.assign':
                return assign(replace(node.left, from, to) as never, node.op, replace(node.right, from, to));
            case 'stmt.decl':
                return withDecl(replaceInDecl(node.decl, from, to), replace(node.child, from, to));
            case 'stmt.expr':
                return exprStmt(replace(node.expr, from, to));
            case 'stmt.export':
                return exportDecl(replaceInDecl(node.decl, from, to));
            case 'stmt.for.range':
                return forRange(node.index, replace(node.low, from, to), replace(node.high, from, to), replace(node.body, from, to), node.reverse);
            case 'stmt.if':
                return if_(replace(node.condition, from, to), replace(node.then, from, to), node.otherwise && replace(node.otherwise, from, to));
            case 'stmt.log':
                return log(replace(node.expr, from, to));
            case 'stmt.return':
                return return_(node.expr !== undefined ? replace(node.expr, from, to) : undefined);
            case 'stmt.sequence':
                return seq(node.children.map(c => replace(c, from, to)));
            case 'stmt.switch':
                return switchCases(
                    replace(node.expr, from, to),
                    node.cases.map(c => ({values: c.values, then: replace(c.then, from, to)})),
                    node.exhaustive,
                );
            case 'stmt.while':
                return while_(replace(node.condition, from, to), replace(node.then, from, to));
            case 'stmt.yield':
                return yield_(node.expr !== undefined ? replace(node.expr, from, to) : undefined);
        }
    }
}
