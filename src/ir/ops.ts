namespace IR {
    export function binaryOp(op: Op.BinaryOp, left: Expr, right: Expr): Expr {
        switch(op) {
            case 'int_plus':
                if(left === ZERO) { return right; }
                if(left.kind === 'expr.op.unary' && left.op === 'int_uminus') { return binaryOp('int_minus', right, left.child); }
                if(right === ZERO) { return left; }
                if(right.kind === 'expr.op.unary' && right.op === 'int_uminus') { return binaryOp('int_minus', left, right.child); }
                break;
            case 'int_minus':
                if(left === ZERO) { return unaryOp('int_uminus', right); }
                if(right === ZERO) { return left; }
                if(right.kind === 'expr.op.unary' && right.op === 'int_uminus') { return binaryOp('int_plus', left, right.child); }
                break;
            
            case 'float_plus':
                if(left === FLOAT_ZERO) { return right; }
                if(left.kind === 'expr.op.unary' && left.op === 'float_uminus') { return binaryOp('float_minus', right, left.child); }
                if(right === FLOAT_ZERO) { return left; }
                if(right.kind === 'expr.op.unary' && right.op === 'float_uminus') { return binaryOp('float_minus', left, right.child); }
                break;
            case 'float_minus':
                if(left === FLOAT_ZERO) { return unaryOp('float_uminus', right); }
                if(right === FLOAT_ZERO) { return left; }
                if(right.kind === 'expr.op.unary' && right.op === 'float_uminus') { return binaryOp('float_plus', left, right.child); }
                break;
            
            case 'int_mult':
                if(left === ZERO || right === ZERO) { return ZERO; }
                if(left === ONE) { return right; }
                if(right === ONE) { return left; }
                if(left === MINUS_ONE) { return unaryOp('int_uminus', right); }
                if(right === MINUS_ONE) { return unaryOp('int_uminus', left); }
                break;
            case 'int_truediv':
                if(right === ONE) { return _unOp('int_to_fraction', left); }
                break;
            case 'int_floordiv':
                if(right === ONE) { return left; }
                if(right === MINUS_ONE) { return unaryOp('int_uminus', left); }
               break;
            
            case 'float_mult':
                // `0 * x === 0` is not strictly correct, due to infinity and NaN
                if(left === FLOAT_ONE) { return right; }
                if(right === FLOAT_ONE) { return left; }
                if(left === FLOAT_MINUS_ONE) { return unaryOp('float_uminus', right); }
                if(right === FLOAT_MINUS_ONE) { return unaryOp('float_uminus', left); }
                break;
            case 'float_truediv':
                // `0 / y === 0` is not strictly correct, due to NaN
                if(right === FLOAT_ONE) { return left; }
                if(right === FLOAT_MINUS_ONE) { return unaryOp('float_uminus', left); }
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
        return x > 0 && (x & (x - 1)) === 0;
    }
    
    function _log2(x: number): Expr {
        return int(31 - Math.clz32(x));
    }
    
    /**
     * Functions for constructing `uint` and `bool` operations in the IR.
     */
    export const OP = {
        and(left: Expr, right: Expr): Expr {
            return left === TRUE || right === FALSE ? right
                : left === FALSE || right === TRUE ? left
                : _binOp('bool_and', left, right);
        },
        or(left: Expr, right: Expr): Expr {
            return left === FALSE || right === TRUE ? right
                : left === TRUE || right === FALSE ? left
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
            return left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int' ? int(left.value + right.value)
                : left === ZERO ? right
                : right === ZERO ? left
                : _binOp('loose_int_plus', left, right);
        },
        addOne(expr: Expr): Expr {
            return OP.addConstant(expr, 1);
        },
        addConstant(left: Expr, right: number): Expr {
            return right === 0 ? left
                : left.kind === 'expr.literal.int' ? int(left.value + right)
                : right > 0 ? OP.add(left, int(right))
                : OP.minus(left, int(-right));
        },
        minus(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int' ? int(left.value - right.value)
                : right === ZERO ? left
                : _binOp('loose_int_minus', left, right);
        },
        minusOne(expr: Expr): Expr {
            return OP.addConstant(expr, -1);
        },
        minusConstant(left: Expr, right: number): Expr {
            return OP.addConstant(left, -right);
        },
        mult(left: Expr, right: Expr): Expr {
            return left.kind === 'expr.literal.int' ? OP.multConstant(right, left.value)
                : right.kind === 'expr.literal.int' ? OP.multConstant(left, right.value)
                : _binOp('loose_int_mult', left, right);
        },
        fraction(left: Expr, right: Expr): BinaryOpExpr | UnaryOpExpr {
            return right === ONE ? _unOp('int_to_fraction', left)
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
            return right === 1 ? left
                : right === 0 ? ZERO
                : _isPowerOfTwo(right) ? OP.lshift(left, _log2(right))
                : right > 0 ? _binOp('loose_int_mult', int(right), left)
                : fail();
        },
        /**
         * For packing two numbers into one int; requires `0 <= y < scale`.
         */
        multAddConstant(x: Expr, scale: number, y: Expr): Expr {
            return _isPowerOfTwo(scale) ? OP.bitwiseXor(OP.lshift(x, _log2(scale)), y)
                : scale > 0 ? OP.add(OP.multConstant(x, scale), y)
                : fail();
        },
        divConstant(left: Expr, right: number): Expr {
            return right === 1 ? left
                : _isPowerOfTwo(right) ? OP.rshift(left, _log2(right))
                : right > 0 ? _binOp('loose_int_floordiv', left, int(right))
                : fail();
        },
        modConstant(left: Expr, right: number): Expr {
            return right === 1 ? ZERO
                : _isPowerOfTwo(right) ? OP.bitwiseAnd(left, int(right - 1))
                : right > 0 ? _binOp('loose_int_mod', left, int(right))
                : fail();
        },
        
        lshift(left: Expr, right: Expr): Expr {
            return left === ZERO || right === ZERO ? left
                : _binOp('int_lshift', left, right);
        },
        rshift(left: Expr, right: Expr): Expr {
            return left === ZERO || right === ZERO ? left
                : _binOp('int_rshift', left, right);
        },
        bitwiseAnd(left: Expr, right: Expr): Expr {
            return left === ZERO || right === ZERO ? ZERO
                : _binOp('int_and', left, right);
        },
        bitwiseOr(left: Expr, right: Expr): Expr {
            return left === ZERO ? right
                : right === ZERO ? left
                : _binOp('int_or', left, right);
        },
        bitwiseXor(left: Expr, right: Expr): Expr {
            return left === ZERO ? right
                : right === ZERO ? left
                : _binOp('int_xor', left, right);
        },
        bitwiseNot(expr: Expr): Expr {
            return expr.kind === 'expr.op.unary' && expr.op === 'int_not' ? expr.child
                : _unOp('int_not', expr);
        },
        countTrailingZeros(expr: Expr): Expr {
            return _unOp('int_ctz', expr);
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
