namespace IR {
    function _replaceVarDecl(decl: VarDeclWithInitialiser, from: Expr, to: Expr): VarDeclWithInitialiser;
    function _replaceVarDecl(decl: VarDecl, from: Expr, to: Expr): VarDecl;
    function _replaceVarDecl(decl: VarDecl, from: Expr, to: Expr): VarDecl {
        const {name, type} = decl;
        return {name, type, initialiser: decl.initialiser && replace(decl.initialiser, from, to)};
    }
    
    export function replace(node: Stmt, from: Expr, to: Expr): Stmt;
    export function replace(node: Expr, from: Expr, to: Expr): Expr;
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
            case 'expr.array.new':
            case 'expr.literal.bool':
            case 'expr.literal.float':
            case 'expr.literal.int':
            case 'expr.literal.null':
            case 'expr.literal.str':
            case 'expr.name':
            case 'expr.param':
                return node;
            
            case 'expr.attr':
                return attr(replace(node.left, from, to), node.attr);
            case 'expr.dict':
                return dict(node.type, node.values.map(v => replace(v, from, to)));
            case 'expr.letin':
                return letIn(
                    node.decls.map(d => _replaceVarDecl(d, from, to)),
                    replace(node.child, from, to),
                );
            case 'expr.op.access':
                return access(replace(node.left, from, to), replace(node.right, from, to));
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
                return localCall(node.name, node.args.map(v => replace(v, from, to)));
            case 'expr.op.ternary':
                return ternary(replace(node.condition, from, to), replace(node.then, from, to), replace(node.otherwise, from, to));
            case 'expr.op.unary':
                const child = replace(node.child, from, to);
                switch(node.op) {
                    case 'int_not':
                        return OP.bitwiseNot(child);
                    case 'int_ctz':
                        return OP.countTrailingZeros(child);
                    case 'float_log2':
                        return OP.log2(child);
                    default:
                        return unaryOp(node.op, child);
                }
            case 'stmt.assign':
                return assign(replace(node.left, from, to) as never, node.op, replace(node.right, from, to));
            case 'stmt.block':
                return block(node.children.map(c => replace(c, from, to)));
            case 'stmt.decl.func':
                return declFunc(node.name, node.yields, node.params, node.paramTypes, node.returnType, replace(node.body, from, to));
            case 'stmt.decl.vars':
                return declVars(node.decls.map(d => _replaceVarDecl(d, from, to)), node.mutable);
            case 'stmt.expr':
                return {kind: 'stmt.expr', expr: replace(node.expr, from, to) as never};
            case 'stmt.for.range':
                return forRange(node.index, replace(node.low, from, to), replace(node.high, from, to), [replace(node.body, from, to)], node.reverse);
            case 'stmt.if':
                return if_(replace(node.condition, from, to), replace(node.then, from, to), node.otherwise && replace(node.otherwise, from, to));
            case 'stmt.log':
                return log(replace(node.expr, from, to));
            case 'stmt.return':
                return return_(node.expr && replace(node.expr, from, to));
            case 'stmt.switch':
                throw new Error();
            case 'stmt.while':
                return while_(replace(node.condition, from, to), replace(node.then, from, to));
            case 'stmt.yield':
                return yield_(node.expr && replace(node.expr, from, to));
        }
    }
}
