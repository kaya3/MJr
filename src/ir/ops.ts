namespace IR {
    export function binaryOp(op: Op.BinaryOp, left: Expr, right: Expr): Expr {
        switch(op) {
            case 'int_plus':
                if(left.kind === 'expr.literal.int' && left.value === 0) { return right; }
                if(left.kind === 'expr.op.unary' && left.op === 'int_uminus') { return binaryOp('int_minus', right, left.child); }
                if(right.kind === 'expr.literal.int' && right.value === 0) { return left; }
                if(right.kind === 'expr.op.unary' && right.op === 'int_uminus') { return binaryOp('int_minus', left, right.child); }
                break;
            case 'int_minus':
                if(left.kind === 'expr.literal.int' && left.value === 0) { return unaryOp('int_uminus', right); }
                if(right.kind === 'expr.literal.int' && right.value === 0) { return left; }
                if(right.kind === 'expr.op.unary' && right.op === 'int_uminus') { return binaryOp('int_plus', left, right.child); }
                break;
            
            case 'float_plus':
                if(left.kind === 'expr.literal.float' && left.value === 0) { return right; }
                if(left.kind === 'expr.op.unary' && left.op === 'float_uminus') { return binaryOp('float_minus', right, left.child); }
                if(right.kind === 'expr.literal.float' && right.value === 0) { return left; }
                if(right.kind === 'expr.op.unary' && right.op === 'float_uminus') { return binaryOp('float_minus', left, right.child); }
                break;
            case 'float_minus':
                if(left.kind === 'expr.literal.float' && left.value === 0) { return unaryOp('float_uminus', right); }
                if(right.kind === 'expr.literal.float' && right.value === 0) { return left; }
                if(right.kind === 'expr.op.unary' && right.op === 'float_uminus') { return binaryOp('float_plus', left, right.child); }
                break;
            
            case 'int_mult':
                if(left.kind === 'expr.literal.int') {
                    if(left.value === -1) { return unaryOp('int_uminus', right); }
                    if(left.value === 0) { return ZERO; }
                    if(left.value === 1) { return right; }
                }
                if(right.kind === 'expr.literal.int') {
                    if(right.value === -1) { return unaryOp('int_uminus', left); }
                    if(right.value === 0) { return ZERO; }
                    if(right.value === 1) { return left; }
                }
                break;
            case 'int_truediv':
                if(right.kind === 'expr.literal.int' && right.value === 1) { return _unOp('int_to_fraction', left); }
                break;
            case 'int_floordiv':
                if(right.kind === 'expr.literal.int') {
                    if(right.value === -1) { return unaryOp('int_uminus', left); }
                    if(right.value === 1) { return left; }
                }
                break;
            
            case 'float_mult':
                if(left.kind === 'expr.literal.float') {
                    if(left.value === -1) { return unaryOp('float_uminus', right); }
                    // `0 * x === 0` is not strictly correct, due to infinity and NaN
                    //if(left.value === 0) { return ZERO; }
                    if(left.value === 1) { return right; }
                }
                if(right.kind === 'expr.literal.float') {
                    if(right.value === -1) { return unaryOp('float_uminus', left); }
                    //if(right.value === 0) { return ZERO; }
                    if(right.value === 1) { return left; }
                }
                break;
        }
        return _binOp(op, left, right);
    }
    
    export function unaryOp(op: Op.UnaryOp, child: Expr): Expr {
        switch(op) {
            case 'bool_not':
                return OP.not(child);
            
            case 'float_uminus':
            case 'fraction_uminus':
            case 'int_uminus':
                // optimisation for self-inverse ops
                if(child.kind === 'expr.op.unary' && child.op === op) { return child.child; }
                break;
        }
        return _unOp(op, child);
    }
    
    const OP_NEGATIONS = (<K extends BinaryOp>(x: IRecord<K, BinaryOp>) => x)({
        bool_eq: 'bool_ne',
        bool_ne: 'bool_eq',
        
        float_eq: 'float_ne',
        float_ne: 'float_eq',
        // float comparison ops cannot be swapped like this, due to NaN
        // https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN
        // TODO: disallow Infinity/NaN completely?
        /*
        float_lt: 'float_ge',
        float_le: 'float_gt',
        float_gt: 'float_le',
        float_ge: 'float_lt',
        */
        
        fraction_eq: 'fraction_ne',
        fraction_ne: 'fraction_eq',
        fraction_lt: 'fraction_ge',
        fraction_le: 'fraction_gt',
        fraction_gt: 'fraction_le',
        fraction_ge: 'fraction_lt',
        
        int_eq: 'int_ne',
        int_ne: 'int_eq',
        int_lt: 'int_ge',
        int_le: 'int_gt',
        int_gt: 'int_le',
        int_ge: 'int_lt',
        
        str_eq: 'str_ne',
        str_ne: 'str_eq',
    });
    
    function _binOp(op: BinaryOp, left: Expr, right: Expr): BinaryOpExpr {
        return {kind: 'expr.op.binary', op, left, right};
    }
    
    function _unOp(op: UnaryOp, child: Expr): UnaryOpExpr {
        return {kind: 'expr.op.unary', op, child};
    }
    
    function _isPowerOfTwo(x: number): boolean {
        return x !== 0 && (x & (x - 1)) === 0;
    }
    
    function _log2(x: number): Expr {
        return int(31 - Math.clz32(x));
    }
    
    /**
     * Functions for constructing `uint` and `bool` operations in the IR.
     */
     export const OP = {
        and(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.bool' ? (left.value ? right : FALSE)
                : right.kind === 'expr.literal.bool' ? (right.value ? left : FALSE)
                : _binOp('bool_and', left, right);
        },
        or(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.bool' ? (left.value ? TRUE : right)
                : right.kind === 'expr.literal.bool' ? (right.value ? TRUE : left)
                : _binOp('bool_or', left, right);
        },
        not(expr: Expr): Expr {
            if(expr.kind === 'expr.op.binary') {
                const {op} = expr;
                if(op === 'bool_and' || op === 'bool_or') {
                    // https://en.wikipedia.org/wiki/De_Morgan's_laws
                    return _binOp(
                        op === 'bool_and' ? 'bool_or' : 'bool_and',
                        OP.not(expr.left),
                        OP.not(expr.right),
                    );
                } else if(objHasKey(OP_NEGATIONS, op)) {
                    return _binOp(OP_NEGATIONS[op], expr.left, expr.right);
                }
            } else if(expr.kind === 'expr.op.unary' && expr.op === 'bool_not') {
                return expr.child;
            }
            return _unOp('bool_not', expr);
        },
        
        add(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.int' && left.value === 0 ? right
                : right.kind === 'expr.literal.int' && right.value === 0 ? left
                : _binOp('loose_int_plus', left, right);
        },
        minusOne(expr: Expr): Expr {
            return expr.kind === 'expr.literal.int' ? int(expr.value - 1) : _binOp('loose_int_minus', expr, ONE)
        },
        mult(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.int' ? OP.multConstant(right, left.value)
                : right.kind === 'expr.literal.int' ? OP.multConstant(left, right.value)
                : _binOp('loose_int_mult', left, right);
        },
        fraction(left: Expr, right: Expr): BinaryOpExpr | UnaryOpExpr {
            return right.kind === 'expr.literal.int' && right.value === 1 ? _unOp('int_to_fraction', left)
                : _binOp('int_truediv', left, right);
        },
        floordiv(left: Expr, right: Expr): Expr {
            return right.kind === 'expr.literal.int' && right.value > 0 ? OP.divConstant(left, right.value)
                : _binOp('loose_int_floordiv', left, right);
        },
        mod(left: Expr, right: Expr): Expr {
            return right.kind === 'expr.literal.int' && right.value > 0 ? OP.modConstant(left, right.value)
                : _binOp('loose_int_mod', left, right);
        },
        
        multConstant(left: Expr, right: number): Expr {
            if(right < 0) { throw new Error(); }
            // special case for power of 2
            return right === 1 ? left
                : right === 0 ? ZERO
                : _isPowerOfTwo(right) ? OP.lshift(left, _log2(right))
                : _binOp('loose_int_mult', int(right), left);
        },
        /**
         * For packing two numbers into one int; requires `0 <= y < scale`.
         */
        multAddConstant(x: Expr, scale: number, y: Expr): Expr {
            if(scale <= 0) { throw new Error(); }
            // special case for power of 2
            return _isPowerOfTwo(scale) ? OP.bitwiseOr(OP.lshift(x, _log2(scale)), y)
                : OP.add(OP.multConstant(x, scale), y)
        },
        divConstant(left: Expr, right: number): Expr {
            if(right <= 0) { throw new Error(); }
            // special case for power of 2
            return right === 1 ? left
                : _isPowerOfTwo(right) ? OP.rshift(left, _log2(right))
                : _binOp('loose_int_floordiv', left, int(right));
        },
        modConstant(left: Expr, right: number): Expr {
            if(right <= 0) { throw new Error(); }
            // special case for power of 2
            return right === 1 ? ZERO
                : _isPowerOfTwo(right) ? OP.bitwiseAnd(left, int(right - 1))
                : _binOp('loose_int_mod', left, int(right));
        },
        
        lshift(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.int' && left.value === 0 ? ZERO
                : right.kind === 'expr.literal.int' && right.value === 0 ? left
                : _binOp('int_lshift', left, right);
        },
        rshift(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.int' && left.value === 0 ? ZERO
                : right.kind === 'expr.literal.int' && right.value === 0 ? left
                : _binOp('int_rshift', left, right);
        },
        bitwiseAnd(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.int' && left.value === 0 ? ZERO
                : right.kind === 'expr.literal.int' && right.value === 0 ? ZERO
                : _binOp('int_and', left, right);
        },
        bitwiseOr(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.int' && left.value === 0 ? right
                : right.kind === 'expr.literal.int' && right.value === 0 ? left
                : _binOp('int_or', left, right);
        },
        bitwiseNot(expr: Expr): Expr {
            return expr.kind === 'expr.op.unary' && expr.op === 'int_not' ? expr.child
                : _unOp('int_not', expr);
        },
        
        eq(left: Expr, right: Expr): Expr {
            if(left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value === right.value ? TRUE : FALSE;
            }
            return _binOp('int_eq', left, right);
        },
        ne(left: Expr, right: Expr): Expr {
            if(left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value !== right.value ? TRUE : FALSE;
            }
            return _binOp('int_ne', left, right);
        },
        lt(left: Expr, right: Expr): Expr {
            if(left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value < right.value ? TRUE : FALSE;
            }
            return _binOp('int_lt', left, right);
        },
        le(left: Expr, right: Expr): Expr {
            if(left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value <= right.value ? TRUE : FALSE;
            }
            return _binOp('int_le', left, right);
        },
        gt(left: Expr, right: Expr): Expr {
            if(left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value > right.value ? TRUE : FALSE;
            }
            return _binOp('int_gt', left, right);
        },
        ge(left: Expr, right: Expr): Expr {
            if(left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value >= right.value ? TRUE : FALSE;
            }
            return _binOp('int_ge', left, right);
        },
    };
}
