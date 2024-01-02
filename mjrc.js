"use strict";
var IR;
(function (IR) {
    IR.BOOL_TYPE = { kind: 'bool' };
    IR.BYTE_TYPE = { kind: 'byte' };
    IR.FLOAT_TYPE = { kind: 'float' };
    IR.FRACTION_TYPE = { kind: 'fraction' };
    IR.GRID_TYPE = { kind: 'grid' };
    IR.INT_TYPE = { kind: 'int' };
    IR.PATTERN_TYPE = { kind: 'pattern' };
    IR.PRNG_TYPE = { kind: 'prng' };
    IR.REWRITE_INFO_TYPE = { kind: 'rewriteinfo' };
    IR.SAMPLER_TYPE = { kind: 'sampler' };
    IR.STR_TYPE = { kind: 'str' };
    IR.VOID_TYPE = { kind: 'void' };
    IR.GRID_DATA_ARRAY_TYPE = mutArrayType(128);
    IR.INT32_ARRAY_TYPE = mutArrayType(2 ** 32);
    function mutArrayType(domainSize) {
        return { kind: 'array.mutable', domainSize };
    }
    IR.mutArrayType = mutArrayType;
    function constArrayType(domainSize) {
        return { kind: 'array.const', domainSize };
    }
    IR.constArrayType = constArrayType;
    function nullableType(componentType) {
        return componentType.kind === 'nullable' ? componentType : { kind: 'nullable', componentType };
    }
    IR.nullableType = nullableType;
})(IR || (IR = {}));
/**
 * Abstract syntax tree for a high-level intermediate representation (IR) which
 * is output by the compiler. The IR is converted into output source code by a
 * class in the `CodeGen` namespace.
 *
 * The IR should be convertible to most imperative languages, so long as they
 * allow local function declarations to access constants from the outer scope.
 */
var IR;
(function (IR) {
    // all type declarations here must be JSON serialisable, since `JSON.stringify` is used to detect repeated IR code and simplify it
    const JSON_KEY = Symbol();
    function key(a) {
        return a[JSON_KEY] ??= JSON.stringify(a);
    }
    IR.key = key;
    function equals(a, b) {
        if (a === b) {
            return true;
        }
        if (a === undefined || b === undefined || a.kind !== b.kind) {
            return false;
        }
        return key(a) === key(b);
    }
    IR.equals = equals;
})(IR || (IR = {}));
var IR;
(function (IR) {
    function _addAll(a, b) {
        if (b !== undefined) {
            for (const x of b) {
                a.add(x);
            }
        }
    }
    class Info {
        flags;
        freeVarsGet;
        freeVarsSet;
        static DO_NOTHING = new Info(511 /* Flags.DO_NOTHING */);
        static BREAK = new Info(511 /* Flags.DO_NOTHING */ & ~2 /* Flags.NO_BREAKS */ & ~256 /* Flags.CAN_COMPLETE_NORMALLY */);
        static CONTINUE = new Info(511 /* Flags.DO_NOTHING */ & ~4 /* Flags.NO_CONTINUES */ & ~256 /* Flags.CAN_COMPLETE_NORMALLY */);
        static RETURN = new Info(511 /* Flags.DO_NOTHING */ & ~8 /* Flags.NO_RETURNS */ & ~256 /* Flags.CAN_COMPLETE_NORMALLY */);
        static THROW = new Info(511 /* Flags.DO_NOTHING */ & ~16 /* Flags.NO_THROWS */ & ~256 /* Flags.CAN_COMPLETE_NORMALLY */);
        static OUTPUT = new Info(511 /* Flags.DO_NOTHING */ & ~32 /* Flags.NO_OUTPUT */);
        static DEFERRED = new Info(256 /* Flags.CAN_COMPLETE_NORMALLY */, new Set([-1]), new Set([-1]));
        static UNREACHABLE = new Info(511 /* Flags.DO_NOTHING */ & ~256 /* Flags.CAN_COMPLETE_NORMALLY */);
        static FRESH_VALUE = new Info(319 /* Flags.NO_SIDE_EFFECTS */ | 128 /* Flags.CONTEXT_INDEPENDENT */);
        static STATE_GET = new Info(383 /* Flags.STATE_GET */);
        static STATE_UPDATE = new Info(318 /* Flags.STATE_UPDATE */);
        static PREAMBLE = new Info(319 /* Flags.NO_SIDE_EFFECTS */ & ~16 /* Flags.NO_THROWS */);
        static constGet(id) {
            return new Info(511 /* Flags.DO_NOTHING */, new Set([id]));
        }
        static varGet(id) {
            return new Info(383 /* Flags.STATE_GET */, new Set([id]));
        }
        static varSet(id) {
            return new Info(318 /* Flags.STATE_UPDATE */, undefined, new Set([id]));
        }
        static seq(nodes) {
            return Info.DO_NOTHING.thenSeq(nodes);
        }
        constructor(flags, freeVarsGet, freeVarsSet) {
            this.flags = flags;
            this.freeVarsGet = freeVarsGet;
            this.freeVarsSet = freeVarsSet;
            if (freeVarsGet?.size === 0) {
                this.freeVarsGet = undefined;
            }
            if (freeVarsSet?.size === 0) {
                this.freeVarsSet = undefined;
            }
        }
        toJSON() { }
        withFlags(flags) {
            return new Info(flags, this.freeVarsGet, this.freeVarsSet);
        }
        withUnionVars(flags, other) {
            const freeVarsGet = new Set(this.freeVarsGet), freeVarsSet = new Set(this.freeVarsSet);
            _addAll(freeVarsGet, other.freeVarsGet);
            _addAll(freeVarsSet, other.freeVarsSet);
            return new Info(flags, freeVarsGet, freeVarsSet);
        }
        then(other) {
            return this.withUnionVars(this.flags & other.flags, other);
        }
        thenSeq(others) {
            let flags = this.flags;
            const freeVarsGet = new Set(this.freeVarsGet), freeVarsSet = new Set(this.freeVarsSet);
            for (const other of others) {
                flags &= other.info.flags;
                _addAll(freeVarsGet, other.info.freeVarsGet);
                _addAll(freeVarsSet, other.info.freeVarsSet);
            }
            return new Info(flags, freeVarsGet, freeVarsSet);
        }
        or(other) {
            const a = this.flags, b = other.flags, c = (a | b) & 256 /* Flags.CAN_COMPLETE_NORMALLY */;
            return this.withUnionVars((a & b) | c, other);
        }
        asLoopBody(infinite) {
            let flags = this.flags | 6 /* Flags.NO_BREAKS_OR_CONTINUES */;
            if (this.canBreak()) {
                flags |= 256 /* Flags.CAN_COMPLETE_NORMALLY */;
            }
            else if (infinite) {
                flags &= ~256 /* Flags.CAN_COMPLETE_NORMALLY */;
            }
            return this.withFlags(flags);
        }
        asVarDecl(name) {
            const freeVarsGet = new Set(this.freeVarsGet);
            const freeVarsSet = new Set(this.freeVarsSet);
            freeVarsGet.delete(name.id);
            freeVarsSet.delete(name.id);
            return new Info(this.flags, freeVarsGet, freeVarsSet);
        }
        asFuncDecl(name) {
            return this.asVarDecl(name)
                .withFlags(511 /* Flags.DO_NOTHING */);
        }
        canGetVar(name) {
            const s = this.freeVarsGet;
            return s !== undefined
                && (s.has(name.id) || s.has(-1));
        }
        canSetVar(name) {
            const s = this.freeVarsSet;
            return s !== undefined
                && (s.has(name.id) || s.has(-1));
        }
        canUseVar(name) {
            return this.canGetVar(name) || this.canSetVar(name);
        }
        hasFreeVars() {
            return this.freeVarsGet !== undefined || this.freeVarsSet !== undefined;
        }
        hasSideEffects() {
            return (this.flags & 319 /* Flags.NO_SIDE_EFFECTS */) !== 319 /* Flags.NO_SIDE_EFFECTS */;
        }
        isTimeIndependent() {
            return (this.flags & 447 /* Flags.TIME_INDEPENDENT */) === 447 /* Flags.TIME_INDEPENDENT */;
        }
        isLocallyDeterministic() {
            return (this.flags & 64 /* Flags.LOCALLY_DETERMINISTIC */) !== 0;
        }
        canCompleteNormally() {
            return (this.flags & 256 /* Flags.CAN_COMPLETE_NORMALLY */) !== 0;
        }
        canBreakOrContinue() {
            return (this.flags & 6 /* Flags.NO_BREAKS_OR_CONTINUES */) !== 6 /* Flags.NO_BREAKS_OR_CONTINUES */;
        }
        canBreak() {
            return (this.flags & 2 /* Flags.NO_BREAKS */) === 0;
        }
        commutesWith(other) {
            return (!this.hasSideEffects() || other.info.isTimeIndependent())
                && (!other.info.hasSideEffects() || this.isTimeIndependent());
        }
    }
    IR.Info = Info;
    IR.LOCAL_FUNCTION_INFO = {
        mask_clear: Info.STATE_UPDATE,
        mask_set: Info.STATE_UPDATE,
        mask_hasnt: Info.STATE_GET,
    };
    IR.LIB_FUNCTION_INFO = {
        lfsrFeedbackTerm: Info.DO_NOTHING,
        nextIntChecked: Info.FRESH_VALUE.or(Info.THROW),
    };
    IR.LIB_METHOD_INFO = {
        Grid: {
            index: Info.DO_NOTHING.or(Info.THROW),
            toString: Info.STATE_GET,
            wrapIndex: Info.DO_NOTHING,
        },
        PRNG: {
            nextDouble: Info.FRESH_VALUE,
            nextInt: Info.FRESH_VALUE,
        },
        Pattern: {
            fitsMask: Info.STATE_GET,
            hasEffect: Info.STATE_GET,
            put: Info.STATE_UPDATE,
        },
        RewriteInfo: {},
        Sampler: {
            add: Info.STATE_UPDATE,
            copyInto: Info.STATE_UPDATE,
            copyIntoOffset: Info.STATE_UPDATE,
            del: Info.STATE_UPDATE,
            has: Info.STATE_GET,
            sample: Info.STATE_GET,
            shuffleInto: Info.STATE_UPDATE,
            shuffleIntoOffset: Info.STATE_UPDATE,
        },
    };
    IR.LIB_CONSTRUCTOR_INFO = {
        Grid: Info.FRESH_VALUE,
        Pattern: Info.DO_NOTHING,
        RewriteInfo: Info.DO_NOTHING,
        Sampler: Info.FRESH_VALUE,
    };
})(IR || (IR = {}));
///<reference path="./ir.ts"/>
///<reference path="./info.ts"/>
var IR;
(function (IR) {
    function float(value) {
        return value === 0 ? IR.FLOAT_ZERO
            : value === 1 ? IR.FLOAT_ONE
                : value === -1 ? IR.FLOAT_MINUS_ONE
                    : { kind: 'expr.literal.float', value, info: IR.Info.DO_NOTHING };
    }
    IR.float = float;
    function int(value) {
        return value === 0 ? IR.ZERO
            : value === 1 ? IR.ONE
                : value === -1 ? IR.MINUS_ONE
                    : { kind: 'expr.literal.int', value, info: IR.Info.DO_NOTHING };
    }
    IR.int = int;
    function str(value) {
        return { kind: 'expr.literal.str', value, info: IR.Info.DO_NOTHING };
    }
    IR.str = str;
    function isInt(expr) {
        return expr.kind === 'expr.literal.int';
    }
    IR.isInt = isInt;
    function isSimpleConstant(expr) {
        return expr.kind.startsWith('expr.literal.')
            || (expr.kind === 'expr.name' && !expr.isMutable)
            || expr.kind === 'expr.unused.error';
    }
    IR.isSimpleConstant = isSimpleConstant;
    // singleton objects for common values
    IR.TRUE = { kind: 'expr.literal.bool', value: true, info: IR.Info.DO_NOTHING };
    IR.FALSE = { kind: 'expr.literal.bool', value: false, info: IR.Info.DO_NOTHING };
    IR.NULL = { kind: 'expr.literal.null', info: IR.Info.DO_NOTHING };
    IR.ZERO = { kind: 'expr.literal.int', value: 0, info: IR.Info.DO_NOTHING };
    IR.ONE = { kind: 'expr.literal.int', value: 1, info: IR.Info.DO_NOTHING };
    IR.MINUS_ONE = { kind: 'expr.literal.int', value: -1, info: IR.Info.DO_NOTHING };
    IR.FLOAT_ZERO = { kind: 'expr.literal.float', value: 0, info: IR.Info.DO_NOTHING };
    IR.FLOAT_ONE = { kind: 'expr.literal.float', value: 1, info: IR.Info.DO_NOTHING };
    IR.FLOAT_MINUS_ONE = { kind: 'expr.literal.float', value: -1, info: IR.Info.DO_NOTHING };
    function unusedExpr(error) {
        return { kind: 'expr.unused.error', error, info: IR.Info.DO_NOTHING };
    }
    IR.unusedExpr = unusedExpr;
    function attr(left, attr) {
        return { kind: 'expr.attr', left, attr, info: left.info };
    }
    IR.attr = attr;
    function letIn(decl, child) {
        if (isSimpleConstant(decl.initialiser)) {
            return IR.replace(child, decl.name, decl.initialiser);
        }
        const info = decl.initialiser.info
            .then(child.info);
        return { kind: 'expr.letin', decl, child, info };
    }
    IR.letIn = letIn;
    // TODO: replace with null-coalescing operator in IR
    function param(name, otherwise) {
        return { kind: 'expr.param', name, otherwise, info: otherwise.info };
    }
    IR.param = param;
    function dict(type, values) {
        return { kind: 'expr.dict', type, values, info: IR.Info.seq(values) };
    }
    IR.dict = dict;
    function newArray(length, domainSize) {
        return { kind: 'expr.array.new', length, domainSize, info: length.info.then(IR.Info.FRESH_VALUE) };
    }
    IR.newArray = newArray;
    function newGridDataArray(length) {
        return newArray(length, IR.GRID_DATA_ARRAY_TYPE.domainSize);
    }
    IR.newGridDataArray = newGridDataArray;
    function newInt32Array(length) {
        return newArray(length, IR.INT32_ARRAY_TYPE.domainSize);
    }
    IR.newInt32Array = newInt32Array;
    function constArray(from, domainSize, rowLength = IR.DEFAULT_ROW_LENGTH) {
        return { kind: 'expr.array.const', from, domainSize, rowLength, info: IR.Info.DO_NOTHING };
    }
    IR.constArray = constArray;
    function access(left, right) {
        const info = left.info
            .then(right.info);
        return { kind: 'expr.op.binary', op: 'array_access', left, right, info };
    }
    IR.access = access;
    function libConstructorCall(className, args) {
        const info = IR.LIB_CONSTRUCTOR_INFO[className].thenSeq(args);
        return { kind: 'expr.op.call.lib.constructor', className, args, info };
    }
    IR.libConstructorCall = libConstructorCall;
    function libFunctionCall(name, args) {
        const info = IR.LIB_FUNCTION_INFO[name].thenSeq(args);
        return { kind: 'expr.op.call.lib.function', name, args, info };
    }
    IR.libFunctionCall = libFunctionCall;
    function libMethodCall(className, name, obj, args) {
        const info = obj.info
            .then(IR.LIB_METHOD_INFO[className][name].thenSeq(args));
        return { kind: 'expr.op.call.lib.method', className, name, obj, args, info };
    }
    IR.libMethodCall = libMethodCall;
    function localCall(name, args, isGetter = false) {
        // TODO: replace isGetter with inference
        const info = name.info
            .then(isGetter ? IR.Info.STATE_GET : IR.Info.STATE_UPDATE)
            .thenSeq(args);
        return { kind: 'expr.op.call.local', name, args, info };
    }
    IR.localCall = localCall;
    function ternary(condition, then, otherwise) {
        if (condition.info.hasSideEffects()) {
            fail(condition);
        }
        if (condition === IR.TRUE) {
            return then;
        }
        else if (condition === IR.FALSE) {
            return otherwise;
        }
        else if (IR.equals(then, otherwise)) {
            return then;
        }
        else if (condition.kind === 'expr.op.unary' && condition.op === 'bool_not') {
            condition = condition.child;
            const tmp = then;
            then = otherwise;
            otherwise = tmp;
        }
        const info = condition.info
            .then(then.info.or(otherwise.info));
        return { kind: 'expr.op.ternary', condition, then, otherwise, info };
    }
    IR.ternary = ternary;
    function binaryOp(op, left, right) {
        switch (op) {
            case 'int_plus':
                if (left === IR.ZERO) {
                    return right;
                }
                if (left.kind === 'expr.op.unary' && left.op === 'int_uminus') {
                    return binaryOp('int_minus', right, left.child);
                }
                if (right === IR.ZERO) {
                    return left;
                }
                if (right.kind === 'expr.op.unary' && right.op === 'int_uminus') {
                    return binaryOp('int_minus', left, right.child);
                }
                break;
            case 'int_minus':
                if (left === IR.ZERO) {
                    return unaryOp('int_uminus', right);
                }
                if (right === IR.ZERO) {
                    return left;
                }
                if (right.kind === 'expr.op.unary' && right.op === 'int_uminus') {
                    return binaryOp('int_plus', left, right.child);
                }
                if (IR.equals(left, right)) {
                    return IR.ZERO;
                }
                break;
            case 'float_plus':
                if (left === IR.FLOAT_ZERO) {
                    return right;
                }
                if (left.kind === 'expr.op.unary' && left.op === 'float_uminus') {
                    return binaryOp('float_minus', right, left.child);
                }
                if (right === IR.FLOAT_ZERO) {
                    return left;
                }
                if (right.kind === 'expr.op.unary' && right.op === 'float_uminus') {
                    return binaryOp('float_minus', left, right.child);
                }
                break;
            case 'float_minus':
                if (left === IR.FLOAT_ZERO) {
                    return unaryOp('float_uminus', right);
                }
                if (right === IR.FLOAT_ZERO) {
                    return left;
                }
                if (right.kind === 'expr.op.unary' && right.op === 'float_uminus') {
                    return binaryOp('float_plus', left, right.child);
                }
                break;
            case 'int_mult':
                if (left === IR.ZERO || right === IR.ZERO) {
                    return IR.ZERO;
                }
                if (left === IR.ONE) {
                    return right;
                }
                if (right === IR.ONE) {
                    return left;
                }
                if (left === IR.MINUS_ONE) {
                    return unaryOp('int_uminus', right);
                }
                if (right === IR.MINUS_ONE) {
                    return unaryOp('int_uminus', left);
                }
                if (isInt(left) && isInt(right)) {
                    return int(Math.imul(left.value, right.value));
                }
                break;
            case 'int_truediv':
                if (right === IR.ONE) {
                    return _unOp('int_to_fraction', left);
                }
                break;
            case 'int_floordiv':
                if (right === IR.ONE) {
                    return left;
                }
                if (right === IR.MINUS_ONE) {
                    return unaryOp('int_uminus', left);
                }
                break;
            case 'float_mult':
                // `0 * x === 0` is not strictly correct, due to infinity and NaN
                if (left === IR.FLOAT_ONE) {
                    return right;
                }
                if (right === IR.FLOAT_ONE) {
                    return left;
                }
                if (left === IR.FLOAT_MINUS_ONE) {
                    return unaryOp('float_uminus', right);
                }
                if (right === IR.FLOAT_MINUS_ONE) {
                    return unaryOp('float_uminus', left);
                }
                break;
            case 'float_truediv':
                // `0 / y === 0` is not strictly correct, due to NaN
                if (right === IR.FLOAT_ONE) {
                    return left;
                }
                if (right === IR.FLOAT_MINUS_ONE) {
                    return unaryOp('float_uminus', left);
                }
                break;
        }
        return _binOp(op, left, right);
    }
    IR.binaryOp = binaryOp;
    function unaryOp(op, child) {
        switch (op) {
            case 'bool_not':
                return IR.OP.not(child);
            case 'int_ctz':
                return IR.OP.countTrailingZeros(child);
            case 'int_not':
                return IR.OP.bitwiseNot(child);
            case 'float_log2':
                return IR.OP.log2(child);
            case 'float_uminus':
            case 'fraction_uminus':
            case 'int_uminus':
                // optimisation for self-inverse ops
                if (child.kind === 'expr.op.unary' && child.op === op) {
                    return child.child;
                }
                break;
        }
        return _unOp(op, child);
    }
    IR.unaryOp = unaryOp;
    const OP_NEGATIONS = ((x) => x)({
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
    function _binOp(op, left, right) {
        const info = left.info
            .then(right.info);
        return { kind: 'expr.op.binary', op, left, right, info };
    }
    function _unOp(op, child) {
        let info = child.info;
        if (op === 'int_checkzero' || op === 'float_checkzero') {
            info = info.or(IR.Info.THROW);
        }
        return { kind: 'expr.op.unary', op, child, info };
    }
    function _isPowerOfTwo(x) {
        return x > 0 && (x & (x - 1)) === 0;
    }
    function _log2(x) {
        return int(31 - Math.clz32(x));
    }
    /**
     * Functions for constructing `uint` and `bool` operations in the IR.
     */
    IR.OP = {
        and(left, right) {
            return left === IR.TRUE || right === IR.FALSE ? right
                : left === IR.FALSE || right === IR.TRUE ? left
                    : _binOp('bool_and', left, right);
        },
        or(left, right) {
            return left === IR.FALSE || right === IR.TRUE ? right
                : left === IR.TRUE || right === IR.FALSE ? left
                    : _binOp('bool_or', left, right);
        },
        not(expr) {
            if (expr.kind === 'expr.op.binary') {
                const { op } = expr;
                if (op === 'bool_and' || op === 'bool_or') {
                    // https://en.wikipedia.org/wiki/De_Morgan's_laws
                    return _binOp(op === 'bool_and' ? 'bool_or' : 'bool_and', IR.OP.not(expr.left), IR.OP.not(expr.right));
                }
                else if (objHasKey(OP_NEGATIONS, op)) {
                    return _binOp(OP_NEGATIONS[op], expr.left, expr.right);
                }
            }
            else if (expr.kind === 'expr.op.unary' && expr.op === 'bool_not') {
                return expr.child;
            }
            return _unOp('bool_not', expr);
        },
        all(xs, f) {
            let r = IR.TRUE;
            xs.forEach((...args) => {
                r = IR.OP.and(r, f(...args));
            });
            return r;
        },
        some(xs, f) {
            let r = IR.FALSE;
            xs.forEach((...args) => {
                r = IR.OP.or(r, f(...args));
            });
            return r;
        },
        add(left, right) {
            if (left.kind === 'expr.op.binary' && (left.op === 'loose_int_plus' || left.op === 'loose_int_minus') && isInt(right)) {
                if (isInt(left.right)) {
                    const lrSign = left.op === 'loose_int_plus' ? 1 : -1;
                    right = int(lrSign * left.right.value + right.value);
                    left = left.left;
                }
                else if (isInt(left.left)) {
                    const llSign = left.op === 'loose_int_plus' ? 1 : -1;
                    right = int(llSign * left.left.value + right.value);
                    left = left.right;
                }
            }
            return isInt(left) && isInt(right) ? int(left.value + right.value)
                : left === IR.ZERO ? right
                    : right === IR.ZERO ? left
                        : isInt(left) && left.value < 0 ? IR.OP.minusConstant(right, -left.value)
                            : isInt(right) && right.value < 0 ? IR.OP.minusConstant(left, -right.value)
                                : _binOp('loose_int_plus', left, right);
        },
        addConstant(left, right) {
            return right === 0 ? left
                : isInt(left) ? int(left.value + right)
                    : right > 0 ? IR.OP.add(left, int(right))
                        : IR.OP.minus(left, int(-right));
        },
        minus(left, right) {
            if (left.kind === 'expr.op.binary' && (left.op === 'loose_int_plus' || left.op === 'loose_int_minus') && isInt(right)) {
                if (isInt(left.right)) {
                    const lrSign = left.op === 'loose_int_plus' ? -1 : 1;
                    right = int(lrSign * left.right.value - right.value);
                    left = left.left;
                }
            }
            return isInt(left) && isInt(right) ? int(left.value - right.value)
                : right === IR.ZERO ? left
                    : IR.equals(left, right) ? IR.ZERO
                        : isInt(left) && left.value < 0 ? IR.OP.addConstant(right, -left.value)
                            : isInt(right) && right.value < 0 ? IR.OP.addConstant(left, -right.value)
                                : _binOp('loose_int_minus', left, right);
        },
        minusConstant(left, right) {
            return IR.OP.addConstant(left, -right);
        },
        mult(left, right) {
            return isInt(left) ? IR.OP.multConstant(right, left.value)
                : isInt(right) ? IR.OP.multConstant(left, right.value)
                    : _binOp('loose_int_mult', left, right);
        },
        fraction(left, right) {
            return right === IR.ONE ? _unOp('int_to_fraction', left)
                : _binOp('int_truediv', left, right);
        },
        floordiv(left, right) {
            return isInt(right) && right.value > 0 ? IR.OP.divConstant(left, right.value)
                : _binOp('loose_int_floordiv', left, right);
        },
        mod(left, right) {
            return isInt(right) && right.value > 0 ? IR.OP.modConstant(left, right.value)
                : _binOp('loose_int_mod', left, right);
        },
        multConstant(left, right) {
            return right === 1 ? left
                : right === 0 ? IR.ZERO
                    : isInt(left) ? int(left.value * right)
                        : _isPowerOfTwo(right) ? IR.OP.lshift(left, _log2(right))
                            : right > 0 ? _binOp('loose_int_mult', int(right), left)
                                : fail();
        },
        /**
         * For packing two numbers into one int; requires `0 <= y < scale`.
         */
        multAddConstant(x, scale, y) {
            return _isPowerOfTwo(scale) ? IR.OP.bitwiseXor(IR.OP.lshift(x, _log2(scale)), y)
                : scale > 0 ? IR.OP.add(IR.OP.multConstant(x, scale), y)
                    : fail();
        },
        divConstant(left, right) {
            return right === 1 ? left
                : isInt(left) ? int((left.value / right) | 0)
                    : _isPowerOfTwo(right) ? IR.OP.rshift(left, _log2(right))
                        : right > 0 ? _binOp('loose_int_floordiv', left, int(right))
                            : fail();
        },
        modConstant(left, right) {
            return right === 1 ? IR.ZERO
                : isInt(left) ? int(left.value % right)
                    : _isPowerOfTwo(right) ? IR.OP.bitwiseAnd(left, int(right - 1))
                        : right > 0 ? _binOp('loose_int_mod', left, int(right))
                            : fail();
        },
        lshift(left, right) {
            return left === IR.ZERO || right === IR.ZERO ? left
                : isInt(left) && isInt(right) ? int(left.value << right.value)
                    : _binOp('int_lshift', left, right);
        },
        rshift(left, right) {
            return left === IR.ZERO || right === IR.ZERO ? left
                : isInt(left) && isInt(right) ? int(left.value >> right.value)
                    : _binOp('int_rshift', left, right);
        },
        bitwiseAnd(left, right) {
            return left === IR.ZERO || right === IR.ZERO ? IR.ZERO
                : isInt(left) && isInt(right) ? int(left.value & right.value)
                    : _binOp('int_and', left, right);
        },
        bitwiseOr(left, right) {
            return left === IR.ZERO ? right
                : right === IR.ZERO ? left
                    : isInt(left) && isInt(right) ? int(left.value | right.value)
                        : _binOp('int_or', left, right);
        },
        bitwiseXor(left, right) {
            return left === IR.ZERO ? right
                : right === IR.ZERO ? left
                    : isInt(left) && isInt(right) ? int(left.value ^ right.value)
                        : _binOp('int_xor', left, right);
        },
        bitwiseNot(expr) {
            // folding e.g. ~1 into -2 doesn't reduce code size
            return isInt(expr) && expr.value < 0 ? int(~expr.value)
                : expr.kind === 'expr.op.unary' && expr.op === 'int_not' ? expr.child
                    : _unOp('int_not', expr);
        },
        countTrailingZeros(expr) {
            return isInt(expr) ? int(MJr.OPS.int_ctz(expr.value))
                : _unOp('int_ctz', expr);
        },
        log2(expr) {
            return expr.kind === 'expr.literal.float' ? float(Math.log2(expr.value))
                : _unOp('float_log2', expr);
        },
        eq(left, right) {
            if (isInt(left) && isInt(right)) {
                return left.value === right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_eq', left, right);
        },
        ne(left, right) {
            if (isInt(left) && isInt(right)) {
                return left.value !== right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_ne', left, right);
        },
        lt(left, right) {
            if (isInt(left) && isInt(right)) {
                return left.value < right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_lt', left, right);
        },
        le(left, right) {
            if (isInt(left) && isInt(right)) {
                return left.value <= right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_le', left, right);
        },
        gt(left, right) {
            if (isInt(left) && isInt(right)) {
                return left.value > right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_gt', left, right);
        },
        ge(left, right) {
            if (isInt(left) && isInt(right)) {
                return left.value >= right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_ge', left, right);
        },
    };
})(IR || (IR = {}));
///<reference path="./ir.ts"/>
///<reference path="./info.ts"/>
var IR;
(function (IR) {
    IR.DEFAULT_ROW_LENGTH = 16;
    // singletons
    IR.BLANK_LINE = { kind: 'stmt.blankline', info: IR.Info.DO_NOTHING };
    IR.BREAK = { kind: 'stmt.break', info: IR.Info.BREAK };
    IR.CONTINUE = { kind: 'stmt.continue', info: IR.Info.BREAK };
    IR.PASS = { kind: 'stmt.pass', info: IR.Info.DO_NOTHING };
    function assign(left, op, right) {
        if (IR.isInt(right)) {
            if (right.value === 0) {
                if (op === '+=' || op === '-=' || op === '|=') {
                    return IR.PASS;
                }
                else if (op === '&=') {
                    op = '=';
                }
            }
            else if (right.value < 0 && (op === '+=' || op === '-=')) {
                op = op === '+=' ? '-=' : '+=';
                right = IR.int(-right.value);
            }
        }
        else if (right.kind === 'expr.op.unary' && right.op === 'int_uminus' && (op === '+=' || op === '-=')) {
            op = op === '+=' ? '-=' : '+=';
            right = right.child;
        }
        let info = left.kind === 'expr.name' && op === '='
            ? IR.Info.DO_NOTHING
            : left.info;
        info = info.then(right.info)
            .then(left.kind === 'expr.name' ? IR.Info.varSet(left.id) : IR.Info.STATE_UPDATE);
        return { kind: 'stmt.assign', op, left, right, info };
    }
    IR.assign = assign;
    function seq(stmts) {
        const children = [];
        for (const stmt of stmts) {
            if (stmt.kind === 'stmt.sequence') {
                children.push(...stmt.children);
            }
            else if (stmt !== IR.PASS) {
                children.push(stmt);
            }
            if (!stmt.info.canCompleteNormally()) {
                break;
            }
        }
        return children.length === 0 ? IR.PASS
            : children.length === 1 ? children[0]
                : { kind: 'stmt.sequence', children, info: IR.Info.seq(children) };
    }
    IR.seq = seq;
    function comment(comment) {
        return { kind: 'stmt.comment', comment, info: IR.Info.DO_NOTHING };
    }
    IR.comment = comment;
    function exportDecl(decl) {
        return { kind: 'stmt.export', decl, info: IR.Info.DO_NOTHING };
    }
    IR.exportDecl = exportDecl;
    function forRange(index, low, high, body, reverse = false) {
        if (body === IR.PASS) {
            return IR.PASS;
        }
        else if (body.kind === 'stmt.if' && !body.condition.info.hasSideEffects() && body.condition.info.isLocallyDeterministic() && !body.condition.info.canUseVar(index.name)) {
            // replace `for if(cond) A else B` with `if(cond) { for A } else { for B }`
            // only allowed if cond is safe to repeatedly evaluate and doesn't involve loop variable
            return if_(body.condition, forRange(index, low, high, body.then, reverse), forRange(index, low, high, body.otherwise ?? IR.PASS, reverse));
        }
        else if (body.kind === 'stmt.decl' && !body.decl.info.hasSideEffects() && body.decl.info.isLocallyDeterministic() && !body.decl.info.canUseVar(index.name)) {
            // replace `for decl in A` with `decl in for A`
            // only allowed if decl is safe to repeatedly evaluate and doesn't involve loop variable
            return withDecl(body.decl, forRange(index, low, high, body.child, reverse));
        }
        let info = body.info.asLoopBody(false);
        if (!IR.isInt(low) || !IR.isInt(high)) {
            // possible that body executes 0 times
            info = info.or(IR.Info.DO_NOTHING);
        }
        else if (low.value === high.value) {
            return IR.PASS;
        }
        else if (low.value + 1 === high.value && !body.info.canBreakOrContinue()) {
            return withConst(index.name, IR.INT_TYPE, low, body);
        }
        if (!body.info.canCompleteNormally() && !body.info.canBreakOrContinue()) {
            const first = reverse ? IR.OP.minusConstant(high, 1) : low;
            return if_(IR.OP.lt(low, high), withConst(index.name, IR.INT_TYPE, first, body));
        }
        info = low.info
            .then(high.info)
            .then(info.asVarDecl(index.name));
        return { kind: 'stmt.for.range', index, low, high, reverse, body, info };
    }
    IR.forRange = forRange;
    function forRangeReverse(index, low, high, body) {
        return forRange(index, low, high, body, true);
    }
    IR.forRangeReverse = forRangeReverse;
    function if_(condition, then, otherwise) {
        if (condition.info.hasSideEffects()) {
            fail('if statement condition must not have side-effects', condition);
        }
        if (otherwise === undefined) {
            otherwise = IR.PASS;
        }
        // assumes condition is pure
        if (condition === IR.TRUE) {
            return then;
        }
        else if (condition === IR.FALSE) {
            return otherwise;
        }
        else if (IR.equals(then, otherwise)) {
            return then;
        }
        else if (then === IR.PASS) {
            // otherwise !== PASS by above
            return if_(IR.OP.not(condition), otherwise);
        }
        else if (then.kind === 'stmt.assign' && otherwise.kind === 'stmt.assign' && IR.equals(then.left, otherwise.left) && then.op === otherwise.op) {
            // replace `if(c) { x = a; } else { x = b; }` with `x = c ? a : b;`
            return assign(then.left, then.op, IR.ternary(condition, then.right, otherwise.right));
        }
        else if (then.kind === 'stmt.for.range' && (IR.equals(condition, IR.OP.lt(then.low, then.high)) || IR.equals(condition, IR.OP.gt(then.high, then.low))) && otherwise === IR.PASS) {
            // omit redundant `if` statement guarding a `for` loop
            return then;
        }
        else if (then.kind === 'stmt.if' && IR.equals(then.otherwise, otherwise)) {
            // replace `if(c1) { if(c2) A else B } else B` with `if(c1 && c2) A else B`
            return if_(IR.OP.and(condition, then.condition), then.then, otherwise);
        }
        else if (then.kind === 'stmt.if' && IR.equals(then.then, otherwise)) {
            // replace `if(c1) { if(c2) B else A } else B` with `if(c1 && !c2) A else B`
            return if_(IR.OP.and(condition, IR.OP.not(then.condition)), then.otherwise ?? IR.PASS, otherwise);
        }
        else if (then.kind === 'stmt.sequence' && otherwise.kind === 'stmt.sequence') {
            // replace `if(c) { ...; A } else { ...; A }` with `if(c) { ... } else { ... } A`
            const thenC = then.children, otherC = otherwise.children;
            let i = 0, j = thenC.length, k = otherC.length;
            // moving the condition forwards is only sound if it is time-independent
            while (i < thenC.length && i < otherC.length && condition.info.commutesWith(thenC[i]) && IR.equals(thenC[i], otherC[i])) {
                ++i;
            }
            while (j > i && k > i && IR.equals(thenC[j - 1], otherC[k - 1])) {
                --j;
                --k;
            }
            if (i > 0 || j < thenC.length) {
                return seq([
                    ...thenC.slice(0, i),
                    if_(condition, seq(thenC.slice(i, j)), seq(otherC.slice(i, k))),
                    ...thenC.slice(j),
                ]);
            }
        }
        if (!then.info.canCompleteNormally() && otherwise !== IR.PASS) {
            return seq([if_(condition, then), otherwise]);
        }
        else if (!otherwise.info.canCompleteNormally()) {
            return seq([if_(IR.OP.not(condition), otherwise), then]);
        }
        const info = condition.info
            .then(then.info.or(otherwise.info));
        if (otherwise === IR.PASS) {
            otherwise = undefined;
        }
        return { kind: 'stmt.if', condition, then, otherwise, info };
    }
    IR.if_ = if_;
    function exprStmt(expr) {
        return expr.info.hasSideEffects() ? { kind: 'stmt.expr', expr, info: expr.info } : IR.PASS;
    }
    IR.exprStmt = exprStmt;
    function libFunctionCallStmt(f, args) {
        return exprStmt(IR.libFunctionCall(f, args));
    }
    IR.libFunctionCallStmt = libFunctionCallStmt;
    function libMethodCallStmt(className, name, obj, args) {
        return exprStmt(IR.libMethodCall(className, name, obj, args));
    }
    IR.libMethodCallStmt = libMethodCallStmt;
    function localCallStmt(f, args) {
        return exprStmt(IR.localCall(f, args));
    }
    IR.localCallStmt = localCallStmt;
    function log(expr) {
        return { kind: 'stmt.log', expr, info: IR.Info.OUTPUT };
    }
    IR.log = log;
    function preamble(paramTypes, emitChecks, libVersion, opsUsed) {
        return { kind: 'stmt.preamble', paramTypes, emitChecks, libVersion, opsUsed, info: IR.Info.PREAMBLE };
    }
    IR.preamble = preamble;
    function return_(expr) {
        const info = expr !== undefined ? expr.info.then(IR.Info.RETURN) : IR.Info.RETURN;
        return { kind: 'stmt.return', expr, info };
    }
    IR.return_ = return_;
    function switch_(expr, casesByIndex, optimise = false) {
        // TODO: some transformations depend on expr being locally constant, since we repeat it in some conditions, or replace it with its value when optimising
        if (expr.info.hasSideEffects()) {
            fail('switch statement scrutinee must not have side-effects', expr);
        }
        if (casesByIndex.length === 0) {
            return IR.PASS;
        }
        else if (casesByIndex.length === 1) {
            return casesByIndex[0];
        }
        const firstCase = casesByIndex[0];
        if (firstCase.kind === 'stmt.if' && casesByIndex.every((c) => c.kind === 'stmt.if' && IR.equals(c.condition, firstCase.condition))) {
            // factor out common condition; the `otherwise` part will generally be trivial
            return if_(firstCase.condition, switch_(expr, casesByIndex.map(c => c.then)), switch_(expr, casesByIndex.map(c => c.otherwise ?? IR.PASS)));
        }
        // de-duplicate cases
        const map = new Map();
        let exhaustive = true;
        for (let i = 0; i < casesByIndex.length; ++i) {
            const c = casesByIndex[i];
            if (c === IR.PASS) {
                exhaustive = false;
                continue;
            }
            if (c.info.canBreakOrContinue()) {
                fail('switch statement cases cannot contain break/continue', c);
            }
            const k = IR.key(c);
            getOrCompute(map, k, () => ({ values: [], then: c })).values.push(i);
        }
        if (optimise) {
            for (const c of map.values()) {
                if (c.values.length === 1) {
                    c.then = IR.replace(c.then, expr, IR.int(c.values[0]));
                }
            }
        }
        return switchCases(expr, Array.from(map.values()), exhaustive);
    }
    IR.switch_ = switch_;
    function _eqAny(expr, values) {
        return values.length > 1 && values.every(v => v < values.length)
            ? IR.OP.lt(expr, IR.int(values.length))
            : IR.OP.some(values, v => IR.OP.eq(expr, IR.int(v)));
    }
    function switchCases(expr, cases, exhaustive) {
        if (expr.info.hasSideEffects()) {
            fail('switch statement scrutinee must not have side-effects', expr);
        }
        if (cases.length === 0) {
            return IR.PASS;
        }
        else if (cases.length === 1) {
            const a = cases[0];
            return exhaustive ? a.then : if_(_eqAny(expr, a.values), a.then);
        }
        else if (cases.length === 2 && exhaustive) {
            const a = cases[0], b = cases[1];
            return if_(_eqAny(expr, a.values), a.then, b.then);
        }
        let info = exhaustive ? IR.Info.UNREACHABLE : IR.Info.DO_NOTHING;
        for (const c of cases) {
            info = info.or(c.then.info);
        }
        info = expr.info.then(info);
        return { kind: 'stmt.switch', expr, cases, exhaustive, info };
    }
    IR.switchCases = switchCases;
    function throw_(message) {
        return { kind: 'stmt.throw', message, info: IR.Info.THROW };
    }
    IR.throw_ = throw_;
    function while_(condition, then) {
        if (condition.info.hasSideEffects()) {
            fail('while statement condition must not have side-effects', condition);
        }
        if (then.kind === 'stmt.if') {
            if (then.then === IR.BREAK) {
                return while_(IR.OP.and(condition, IR.OP.not(then.condition)), then.otherwise ?? IR.PASS);
            }
            else if (then.otherwise === IR.BREAK) {
                return while_(IR.OP.and(condition, then.condition), then.then);
            }
        }
        else if (then.kind === 'stmt.sequence') {
            const firstChild = then.children[0], lastChild = then.children[then.children.length - 1];
            if (firstChild.kind === 'stmt.if') {
                if (firstChild.then === IR.BREAK) {
                    return while_(IR.OP.and(condition, IR.OP.not(firstChild.condition)), seq([firstChild.otherwise ?? IR.PASS, ...then.children.slice(1)]));
                }
                else if (firstChild.otherwise === IR.BREAK) {
                    return while_(IR.OP.and(condition, firstChild.condition), seq([firstChild.then, ...then.children.slice(1)]));
                }
            }
            if (lastChild === IR.BREAK) {
                const r = seq(then.children.slice(0, then.children.length - 1));
                if (!r.info.canBreakOrContinue()) {
                    return r;
                }
            }
            else if (lastChild === IR.CONTINUE) {
                const r = seq(then.children.slice(0, then.children.length - 1));
                return while_(condition, r);
            }
        }
        if (condition === IR.FALSE || then == IR.BREAK) {
            return IR.PASS;
        }
        else if (then === IR.PASS || then === IR.CONTINUE) {
            // the compiler shouldn't ever output an infinite loop that does nothing
            fail(`empty while loop should have impossible condition`, condition);
        }
        else if (!then.info.canBreakOrContinue() && !then.info.canCompleteNormally()) {
            // if the body won't finish then the loop won't iterate more than once
            return if_(condition, then);
        }
        const info = condition.info
            .then(then.info.asLoopBody(condition === IR.TRUE));
        return { kind: 'stmt.while', condition, then, info };
    }
    IR.while_ = while_;
    function withDecls(decls, child) {
        for (let i = decls.length - 1; i >= 0; --i) {
            child = withDecl(decls[i], child);
        }
        return child;
    }
    IR.withDecls = withDecls;
    function withDecl(decl, child) {
        switch (decl.kind) {
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
    IR.withDecl = withDecl;
    function withConst(name, type, initialiser, child) {
        if (!child.info.canGetVar(name)) {
            return seq([exprStmt(initialiser), child]);
        }
        else if (IR.isSimpleConstant(initialiser)) {
            return IR.replace(child, name, initialiser);
        }
        else if (!initialiser.info.hasSideEffects() && child.kind === 'stmt.if' && child.otherwise === undefined && !child.condition.info.canUseVar(name) && child.condition.info.commutesWith(initialiser)) {
            // replace `const x = A; if(B) C` with `if(B) { const x = A; C }
            return if_(child.condition, withConst(name, type, initialiser, child.then));
        }
        const info = initialiser.info
            .then(child.info.asVarDecl(name));
        return { kind: 'stmt.decl', decl: { kind: 'decl.var.const', name, type, initialiser, info: initialiser.info }, child, info };
    }
    IR.withConst = withConst;
    function withVar(name, type, initialiser, child) {
        if (!child.info.canGetVar(name)) {
            // TODO: remove assignment statements from child
            if (!child.info.canSetVar(name)) {
                return initialiser === undefined ? child : seq([exprStmt(initialiser), child]);
            }
        }
        else if (!child.info.canSetVar(name)) {
            const constName = { kind: 'expr.name', id: name.id, namePart: name.namePart, isMutable: false, info: IR.Info.constGet(name.id) };
            return withConst(constName, type, initialiser ?? fail(), IR.replace(child, name, constName));
        }
        if (initialiser === undefined && child.kind === 'stmt.sequence') {
            const firstChild = child.children[0];
            if (firstChild.kind === 'stmt.assign' && firstChild.op === '=' && IR.equals(firstChild.left, name)) {
                return withVar(name, type, firstChild.right, seq(child.children.slice(1)));
            }
        }
        let info = child.info.asVarDecl(name);
        if (initialiser !== undefined) {
            info = initialiser.info.then(info);
        }
        return { kind: 'stmt.decl', decl: { kind: 'decl.var.mut', name, type, initialiser, info: initialiser !== undefined ? initialiser.info : IR.Info.DO_NOTHING }, child, info };
    }
    IR.withVar = withVar;
    function withFunc(decl, child) {
        if (!child.info.canGetVar(decl.name)) {
            return child;
        }
        return { kind: 'stmt.decl', decl, child, info: decl.info.then(child.info) };
    }
    IR.withFunc = withFunc;
    function yield_(expr) {
        const info = expr !== undefined ? expr.info.then(IR.Info.OUTPUT) : IR.Info.OUTPUT;
        return { kind: 'stmt.yield', expr, info };
    }
    IR.yield_ = yield_;
})(IR || (IR = {}));
///<reference path="./expr.ts"/>
///<reference path="./stmt.ts"/>
///<reference path="./types.ts"/>
var IR;
(function (IR) {
    class Factory {
        lastID = -1;
        nextName(namePart, isMutable) {
            const id = ++this.lastID;
            return { kind: 'expr.name', id, namePart, isMutable, info: isMutable ? IR.Info.varGet(id) : IR.Info.constGet(id) };
        }
        deferredExpr(purpose) {
            return { kind: 'expr.unused.deferred', id: ++this.lastID, purpose, info: IR.Info.DEFERRED };
        }
        func(namePart) {
            return this.nextName(namePart, false);
        }
        constDecl(namePart, type, initialiser) {
            const name = this.nextName(namePart, false);
            return { kind: 'decl.var.const', name, type, initialiser, info: initialiser.info };
        }
        loopVarDecl(namePart = 'i', type = IR.INT_TYPE) {
            const name = this.nextName(namePart, false);
            return { kind: 'decl.var.loop', name, type, info: IR.Info.DO_NOTHING };
        }
        paramDecl(namePart, type, isOptional = false) {
            const name = this.nextName(namePart, false);
            let initialiser = undefined;
            if (isOptional) {
                type = IR.nullableType(type);
                initialiser = IR.NULL;
            }
            return { kind: 'decl.var.param', name, type, isOptional, initialiser, info: IR.Info.DO_NOTHING };
        }
        varDecl(namePart, type, initialiser) {
            const name = this.nextName(namePart, true);
            const info = initialiser !== undefined ? initialiser.info : IR.Info.DO_NOTHING;
            return { kind: 'decl.var.mut', name, type, initialiser, info };
        }
        flag() {
            const decl = this.varDecl('flag', IR.BOOL_TYPE, IR.FALSE);
            return {
                decl,
                set: IR.assign(decl.name, '=', IR.TRUE),
                unset: IR.assign(decl.name, '=', IR.FALSE),
                check: decl.name,
            };
        }
        constArrayDecl(namePart, from, domainSize, rowLength = IR.DEFAULT_ROW_LENGTH) {
            return this.constDecl(namePart, IR.constArrayType(domainSize), IR.constArray(from, domainSize, rowLength));
        }
        mutArrayDecl(namePart, length, domainSize) {
            return this.constDecl(namePart, IR.mutArrayType(domainSize), IR.newArray(length, domainSize));
        }
        withConst(namePart, type, initialiser, child) {
            if (IR.isSimpleConstant(initialiser)) {
                return child(initialiser);
            }
            const name = this.nextName(namePart, false);
            return IR.withConst(name, type, initialiser, child(name));
        }
        withVar(namePart, type, initialiser, child) {
            const name = this.nextName(namePart, true);
            return IR.withVar(name, type, initialiser, child(name));
        }
        forRange(namePart, low, high, body, reverse = false) {
            const decl = this.loopVarDecl(namePart);
            return IR.forRange(decl, low, high, body(decl.name), reverse);
        }
        forRangeReverse(namePart, low, high, body) {
            return this.forRange(namePart, low, high, body, true);
        }
    }
    IR.Factory = Factory;
})(IR || (IR = {}));
var IR;
(function (IR) {
    class PRNG {
        name;
        constructor(name) {
            this.name = name;
        }
        nextInt(n) {
            return n === IR.ONE ? IR.ZERO : IR.libMethodCall('PRNG', 'nextInt', this.name, [n]);
        }
        nextIntChecked(n) {
            return IR.libFunctionCall('nextIntChecked', [this.name, n]);
        }
        _nextDouble;
        nextDouble() {
            return this._nextDouble ??= IR.libMethodCall('PRNG', 'nextDouble', this.name, []);
        }
    }
    IR.PRNG = PRNG;
})(IR || (IR = {}));
///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
///<reference path="../ir/prng.ts"/>
///<reference path="compiler.ts"/>
var Compiler;
(function (Compiler) {
    const { OP, } = IR;
    const UNUSED_POSITION = {
        index: IR.unusedExpr('at.index'),
        x: IR.unusedExpr('at.x'),
        y: IR.unusedExpr('at.y'),
    };
    const UNUSED_AT_CONV = IR.unusedExpr('at.conv');
    class CompilerContext {
        c;
        variables;
        at;
        atConv;
        static root(c) {
            return new CompilerContext(c, new Map(), undefined, undefined);
        }
        constructor(c, variables, at, atConv) {
            this.c = c;
            this.variables = variables;
            this.at = at;
            this.atConv = atConv;
        }
        withVariable(id, name) {
            const variables = new Map(this.variables);
            variables.set(id, name);
            return new CompilerContext(this.c, variables, this.at, this.atConv);
        }
        withPosition(at) {
            return new CompilerContext(this.c, this.variables, at, this.atConv);
        }
        withConvPosition(at, atConv) {
            return new CompilerContext(this.c, this.variables, at, atConv);
        }
        expr(expr) {
            switch (expr.kind) {
                case 'expr.attr.dict': {
                    return IR.attr(this.expr(expr.left), expr.attr);
                }
                case 'expr.attr.grid': {
                    return this.c.grids[expr.grid].attr(expr.attr);
                }
                case 'expr.attr.position': {
                    const g = this.c.grids[expr.left.type.inGrid];
                    // optimise common cases
                    if (expr.left.kind === 'expr.name.keyword') {
                        switch (expr.left.name) {
                            case 'at':
                                const at = this.at ?? fail();
                                return expr.attr === 'x' ? at.x
                                    : expr.attr === 'y' ? at.y
                                        : fail();
                            case 'origin':
                                return expr.attr === 'x' ? g.originX
                                    : expr.attr === 'y' ? g.originY
                                        : fail();
                        }
                    }
                    const pos = this.expr(expr.left);
                    return expr.attr === 'x' ? OP.mod(pos, g.width)
                        : expr.attr === 'y' ? OP.floordiv(pos, g.width)
                            : fail();
                }
                case 'expr.constant': {
                    return this.compileLiteral(expr.constant, expr.pos);
                }
                case 'expr.count': {
                    return this.c.grids[expr.inGrid].makeCounter(expr.patterns);
                }
                case 'expr.decl': {
                    // TODO: get rid of `expr.decl` in ASG and `expr.letin` in IR, by hoisting assign statements?
                    const { variable, rhs } = expr.decl;
                    if (variable.references === 0) {
                        return this.expr(rhs);
                    }
                    const decl = this.c.ir.constDecl(variable.name, 
                    // need to pass the type, in case the code generator wants to use a lambda requiring a type annotation
                    this.c.type(variable.type), this.expr(rhs));
                    const ctx = this.withVariable(variable.id, decl.name);
                    return IR.letIn(decl, ctx.expr(expr.child));
                }
                case 'expr.dict': {
                    const type = this.c.dictType(expr.type.entryTypes);
                    return IR.dict(type, type.keys.map(k => this.expr(expr.entryExprs.get(k))));
                }
                case 'expr.name.keyword': {
                    switch (expr.name) {
                        case 'at':
                            return (this.at ?? fail()).index;
                        case 'origin':
                            return expr.type.kind === 'position'
                                ? this.c.grids[expr.type.inGrid].origin
                                : fail();
                        case 'random':
                            return this.c.prng.nextDouble();
                    }
                    // exhaustivity check
                    fail(expr);
                }
                case 'expr.name.simple': {
                    return this.variables.get(expr.variable.id) ?? fail(`variable not defined in this context`, expr.variable, this);
                }
                case 'expr.op.binary': {
                    const r = IR.binaryOp(expr.op, this.expr(expr.left), this.expr(expr.right));
                    // `IR.binaryOp` may optimise e.g. `0 - x` to `-x`
                    if (r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') {
                        this.c.opsUsed.add(r.op);
                    }
                    return r;
                }
                case 'expr.op.ternary': {
                    return IR.ternary(this.expr(expr.condition), this.expr(expr.then), this.expr(expr.otherwise));
                }
                case 'expr.op.unary': {
                    const r = IR.unaryOp(expr.op, this.expr(expr.child));
                    // `IR.unaryOp` may optimise e.g. `not x == y` to `x != y`
                    if (r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') {
                        this.c.opsUsed.add(r.op);
                    }
                    return r;
                }
                case 'expr.param': {
                    return IR.param(expr.name, this.expr(expr.otherwise));
                }
                case 'expr.randint': {
                    const prng = this.c.prng, max = this.expr(expr.max);
                    return !IR.isInt(max) ? prng.nextIntChecked(max)
                        : max.value > 0 ? prng.nextInt(max)
                            : fail();
                }
                case 'expr.sum': {
                    const g = this.c.grids[expr.inGrid];
                    const p = g.grid.convPatterns.getByID(expr.patternID);
                    return g.makeConvBuffer(p.kernel).get(p, this.atConv ?? fail());
                }
            }
        }
        compileLiteral(constant, pos) {
            switch (constant.kind) {
                case 'bool': return constant.value ? IR.TRUE : IR.FALSE;
                case 'float': return IR.float(constant.value);
                case 'int': return IR.int(constant.value);
                case 'str': return IR.str(constant.value);
                case 'dict': {
                    const type = this.c.dictType(constant.type.entryTypes);
                    const values = Array.from(type.keys, k => this.compileLiteral(constant.value.get(k), pos));
                    return this.c.internConstant(IR.dict(type, values), type);
                }
                case 'fraction': {
                    const expr = OP.fraction(IR.int(constant.value.p), IR.int(constant.value.q));
                    this.c.opsUsed.add(expr.op);
                    return this.c.internConstant(expr, IR.FRACTION_TYPE);
                }
                case 'grid': {
                    return this.c.grids[constant.value].obj;
                }
                case 'pattern.in': {
                    this.c.notSupported('input pattern in non-constant expression', pos);
                    return IR.NULL;
                }
                case 'pattern.out': {
                    const { width, height, pattern } = constant.value;
                    const patternExpr = IR.constArray(pattern, Math.max(...pattern) + 1, width);
                    return this.c.internConstant(IR.libConstructorCall('Pattern', [IR.int(width), IR.int(height), patternExpr]), IR.PATTERN_TYPE);
                }
                case 'position': {
                    const { x, y, inGrid } = constant.value;
                    const g = this.c.grids[inGrid];
                    return this.c.internConstant(g.checkedIndex(IR.int(x), IR.int(y)), IR.INT_TYPE);
                }
            }
        }
        usePattern(expr, child) {
            return this.c.ir.withConst('pattern', IR.PATTERN_TYPE, this.expr(expr), pattern => child({
                expr: pattern,
                constant: expr.kind === 'expr.constant' ? expr.constant.value : undefined,
            }));
        }
        isTautology(expr) {
            return this.withConvPosition(UNUSED_POSITION, UNUSED_AT_CONV).expr(expr) === IR.TRUE;
        }
    }
    Compiler.CompilerContext = CompilerContext;
})(Compiler || (Compiler = {}));
var Compiler;
(function (Compiler) {
    class StmtCompiler {
        stmt;
        constructor(stmt) {
            this.stmt = stmt;
        }
        declareRoot(ctx) {
            return IR.NO_DECL;
        }
        declareParent(ctx) {
            return IR.NO_DECL;
        }
        compileReset(ctx) {
            return IR.PASS;
        }
    }
    Compiler.StmtCompiler = StmtCompiler;
    class Stmt_NotSupported extends StmtCompiler {
        compile(ctx, ifTrue, ifFalse) {
            const { kind, pos } = this.stmt;
            ctx.c.notSupported(`'${kind}'`, pos);
            return [ifFalse, ctx];
        }
    }
    Compiler.Stmt_NotSupported = Stmt_NotSupported;
    class Stmt_Assign extends StmtCompiler {
        decl;
        initLate;
        constructor(stmt, c) {
            super(stmt);
            this.initLate = !ASG.exprIsGridIndependent(stmt.rhs);
            this.decl = c.ir.varDecl(stmt.variable.name, c.type(stmt.variable.type));
        }
        _init(ctx) {
            return IR.assign(this.decl.name, '=', ctx.expr(this.stmt.rhs));
        }
        declareParent(ctx) {
            return this.initLate ? this.decl : IR.initDecl(this.decl, this._init(ctx));
        }
        compile(ctx, ifTrue, ifFalse) {
            return [
                this.initLate ? IR.seq([this._init(ctx), ifFalse]) : ifFalse,
                ctx.withVariable(this.stmt.variable.id, this.decl.name),
            ];
        }
    }
    Compiler.Stmt_Assign = Stmt_Assign;
    class Stmt_Log extends StmtCompiler {
        compile(ctx, ifTrue, ifFalse) {
            const { expr } = this.stmt;
            return [
                IR.seq([IR.log(ctx.expr(expr)), ifFalse]),
                ctx,
            ];
        }
    }
    Compiler.Stmt_Log = Stmt_Log;
    class Stmt_Use extends StmtCompiler {
        compile(ctx, ifTrue, ifFalse) {
            const c = ctx.c, grid = this.stmt.grid;
            return [
                IR.seq([c.config.animate ? c.grids[grid].yield_() : IR.PASS, ifFalse]),
                ctx,
            ];
        }
    }
    Compiler.Stmt_Use = Stmt_Use;
    class Stmt_Block extends StmtCompiler {
        children;
        constructor(stmt, c) {
            super(stmt);
            this.children = stmt.children.map(child => c.getStmtCompiler(child));
        }
        declareRoot(ctx) {
            return IR.multiDecl(this.children.map(child => child.declareRoot(ctx)));
        }
        declareChildren(ctx) {
            return IR.multiDecl(this.children.map(child => child.declareParent(ctx)));
        }
        resetChildren(ctx) {
            return IR.seq(this.children.map(child => child.compileReset(ctx)));
        }
    }
    Compiler.Stmt_Block = Stmt_Block;
    class Stmt_Modified extends StmtCompiler {
        child;
        constructor(stmt, c) {
            super(stmt);
            this.child = c.getStmtCompiler(stmt.child);
        }
        declareRoot(ctx) {
            return this.child.declareRoot(ctx);
        }
        declareParent(ctx) {
            return this.child.declareParent(ctx);
        }
    }
    Compiler.Stmt_Modified = Stmt_Modified;
    class Stmt_Markov extends Stmt_Block {
        compile(ctx, ifTrue, ifFalse) {
            const isConsequential = ifTrue !== IR.PASS || ifFalse !== IR.PASS;
            const flag = ctx.c.ir.flag(), setFlagAndContinue = isConsequential ? IR.seq([flag.set, IR.CONTINUE]) : IR.CONTINUE, childDecls = this.declareChildren(ctx), resetChildren = this.resetChildren(ctx);
            const body = [ctx.c.checkMaxIterations];
            for (const child of this.children) {
                const [r, newCtx] = child.compile(ctx, setFlagAndContinue, IR.PASS);
                body.push(r);
                ctx = newCtx;
            }
            body.push(IR.BREAK);
            return [IR.withDecl(childDecls, IR.seq([
                    // TODO: this is not strictly correct, since it resets out of order
                    resetChildren,
                    IR.withDecl(isConsequential ? flag.decl : IR.NO_DECL, IR.seq([
                        IR.while_(IR.TRUE, IR.seq(body)),
                        IR.if_(flag.check, ifTrue, ifFalse),
                    ])),
                ])), ctx];
        }
        compileTransparent(ctx, ifTrue) {
            const ifTrueContinue = IR.seq([ifTrue, IR.CONTINUE]), childDecls = this.declareChildren(ctx), resetChildren = this.resetChildren(ctx);
            const body = [ctx.c.checkMaxIterations];
            for (const child of this.children) {
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
    Compiler.Stmt_Markov = Stmt_Markov;
    class Stmt_Sequence extends Stmt_Block {
        compile(ctx, ifTrue, ifFalse) {
            const isConsequential = ifTrue !== IR.PASS || ifFalse !== IR.PASS;
            const flag = ctx.c.ir.flag(), setFlag = isConsequential ? flag.set : IR.PASS;
            const out = [], execChildren = [];
            for (const child of this.children) {
                out.push(child instanceof Stmt_Limit
                    ? child.compileResetTransparent(ctx)
                    : child.compileReset(ctx));
            }
            for (const child of this.children) {
                if (child instanceof Stmt_Markov || child instanceof Stmt_Limit) {
                    const [r, newCtx] = child.compileTransparent(ctx, setFlag);
                    execChildren.push(r);
                    ctx = newCtx;
                }
                else {
                    const [r, newCtx] = child.compile(ctx, setFlag, IR.BREAK);
                    execChildren.push(IR.while_(IR.TRUE, IR.seq([ctx.c.checkMaxIterations, r])));
                    ctx = newCtx;
                }
            }
            if (isConsequential) {
                execChildren.push(IR.if_(flag.check, ifTrue, ifFalse));
            }
            out.push(IR.withDecl(isConsequential ? flag.decl : IR.NO_DECL, IR.seq(execChildren)));
            return [
                IR.withDecl(this.declareChildren(ctx), IR.seq(out)),
                ctx,
            ];
        }
    }
    Compiler.Stmt_Sequence = Stmt_Sequence;
    class Stmt_Limit extends Stmt_Modified {
        limitVar;
        initLate;
        constructor(stmt, c) {
            super(stmt, c);
            this.initLate = !ASG.exprIsGridIndependent(stmt.limit);
            this.limitVar = c.ir.varDecl('limit', IR.INT_TYPE);
        }
        _init(ctx) {
            return IR.assign(this.limitVar.name, '=', ctx.expr(this.stmt.limit));
        }
        declareParent(ctx) {
            return IR.multiDecl([
                this.initLate ? this.limitVar : IR.initDecl(this.limitVar, this._init(ctx)),
                this.child.declareParent(ctx),
            ]);
        }
        compileReset(ctx) {
            const child = this.child.compileReset(ctx);
            return this.initLate ? IR.seq([this._init(ctx), child]) : child;
        }
        compileResetTransparent(ctx) {
            return this.child.compileReset(ctx);
        }
        compile(ctx, ifTrue, ifFalse) {
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
        compileTransparent(ctx, ifTrue) {
            const init = ctx.expr(this.stmt.limit);
            if (init === IR.ONE) {
                return this.child.compile(ctx, ifTrue, IR.PASS);
            }
            const c = ctx.c;
            const [r, newCtx] = this.child.compile(ctx, ifTrue, IR.BREAK);
            return [
                c.ir.withConst('max', IR.INT_TYPE, init, max => c.ir.forRange('i', IR.ZERO, max, () => IR.seq([
                    ctx.c.checkMaxIterations,
                    r,
                ]))),
                newCtx,
            ];
        }
    }
    Compiler.Stmt_Limit = Stmt_Limit;
})(Compiler || (Compiler = {}));
///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
///<reference path="stmt.ts"/>
var Compiler;
(function (Compiler) {
    const { OP, } = IR;
    class Stmt_BasicAllPrl extends Compiler.StmtCompiler {
        compile(ctx, ifTrue, ifFalse) {
            const { stmt } = this;
            const { rewrites } = stmt;
            const c = ctx.c;
            const g = c.grids[stmt.inGrid];
            const k = rewrites.length;
            if (c.config.animate) {
                ifTrue = IR.seq([g.yield_(), ifTrue]);
            }
            const outPatternIsSameEverywhere = rewrites.map(rule => ASG.exprIsSameEverywhere(rule.to));
            // TODO: if rule.to is grid-dependent and not same-everywhere, need to do rewrites on a temporary buffer then copy over afterwards
            const patternIsGridDependent = rewrites.map(rule => !ASG.exprIsGridIndependent(rule.to));
            rewrites.forEach((rule, i) => {
                if (patternIsGridDependent[i] && !outPatternIsSameEverywhere[i]) {
                    c.notSupported('output pattern dependent on both grid state and match position', rule.pos);
                }
            });
            const matches = c.useTempArray(g.grid.scaleX * g.grid.scaleY * k);
            const mask = stmt.kind === 'stmt.rules.basic.all' && (!stmt.commutative || rewrites.some(rule => {
                // conservative estimate of whether rewrites can overlap; TODO: check this more accurately
                if (rule.to.kind === 'expr.constant') {
                    const { value } = rule.to.constant;
                    return value.effectiveWidth > 1 || value.effectiveHeight > 1;
                }
                else {
                    const { type } = rule.to;
                    return type.width > 1 || type.height > 1;
                }
            })) ? c.useMask(g) : undefined;
            const shuffle = mask !== undefined || !stmt.commutative;
            // tmpPatterns only used when
            const unusedPositionCtx = ctx.withPosition({
                index: IR.unusedExpr('at.index'),
                x: IR.unusedExpr('at.x'),
                y: IR.unusedExpr('at.y'),
            });
            const tmpPatterns = rewrites.map(rule => c.ir.constDecl('p', IR.PATTERN_TYPE, unusedPositionCtx.expr(rule.to)));
            // conditions which must be checked during writing
            const duringWritesCondition = (ctx, rule, p, i) => Compiler.patternHasEffect(ctx, g, rule.from, p);
            // if any duringWritesCondition do more than just check the mask, use a flag for whether any rewrites were done
            // but no flag needed if this statement isn't branching anyway
            const flag = (ifTrue !== IR.PASS || ifFalse !== IR.PASS) && rewrites.some(rule => rule.to.kind !== 'expr.constant')
                ? c.ir.flag()
                : undefined;
            const out = [];
            // optimisation for common case: all rewrites are unconditional
            const allUnconditional = rewrites.every(rule => ctx.isTautology(rule.condition));
            if (allUnconditional) {
                const sampler = g.makeSampler(rewrites.map(rule => rule.from));
                out.push(shuffle ? sampler.shuffleInto(c.prng, matches) : sampler.copyInto(matches), matches.declareCount(sampler.count));
            }
            else {
                out.push(matches.declareCount(IR.ZERO));
                for (let i = 0; i < k; ++i) {
                    const rule = rewrites[i], sampler = g.makeSampler([rule.from]);
                    out.push(
                    // if condition is same-everywhere, then we only need to check it once for all matches of this rule
                    ASG.exprIsSameEverywhere(rule.condition)
                        ? IR.if_(ctx.expr(rule.condition), shuffle ? sampler.shuffleIntoOffset(c.prng, matches, k, i) : sampler.copyIntoOffset(matches, matches.count, k, i))
                        // otherwise, need to check the condition separately for each match
                        : sampler.forEach(at => {
                            const match = OP.multAddConstant(at.index, k, IR.int(i));
                            return IR.if_(ctx.withPosition(at).expr(rule.condition), shuffle ? matches.insertShuffled(c.prng, match) : matches.push(match));
                        }));
                }
            }
            const buildCases = (ctx) => rewrites.map((rule, i) => {
                const buildCase = (p) => IR.if_(OP.and(duringWritesCondition(ctx, rule, p, i), mask !== undefined ? mask.patternFits(g, p, ctx.at) : IR.TRUE), IR.seq([
                    Compiler.doWrite(ctx, g, rule.from, p, mask, true, false),
                    flag !== undefined ? flag.set : IR.PASS,
                ]));
                return rule.to.kind === 'expr.constant'
                    ? buildCase({ expr: IR.unusedExpr('constant out pattern'), constant: rule.to.constant.value })
                    : outPatternIsSameEverywhere[i]
                        ? buildCase({ expr: tmpPatterns[i].name, constant: undefined })
                        : ctx.usePattern(rule.to, buildCase);
            });
            const doMain = IR.if_(matches.isNotEmpty, IR.seq([
                mask !== undefined ? mask.clear(g) : IR.PASS,
                c.ir.forRange('i', IR.ZERO, matches.count, i => c.ir.withConst('match', IR.INT_TYPE, matches.get(i), match => g.declareAtIndex(OP.divConstant(match, k), at => IR.switch_(OP.modConstant(match, k), buildCases(ctx.withPosition(at)))))),
                flag !== undefined ? IR.PASS : ifTrue,
            ]), flag !== undefined ? undefined : ifFalse);
            out.push(flag !== undefined
                ? IR.withDecl(flag.decl, IR.seq([
                    doMain,
                    IR.if_(flag.check, ifTrue, ifFalse),
                ]))
                : doMain);
            return [
                IR.withDecls(tmpPatterns, IR.seq(out)),
                ctx,
            ];
        }
    }
    Compiler.Stmt_BasicAllPrl = Stmt_BasicAllPrl;
})(Compiler || (Compiler = {}));
///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
var Compiler;
(function (Compiler) {
    const { OP, } = IR;
    class Stmt_ConvChain extends Compiler.StmtCompiler {
        colourArray;
        c2iArray;
        c2iOffset;
        matchesCount;
        matchesArray;
        score;
        temperature;
        anneal;
        rootDecl;
        parentDecl;
        constructor(stmt, c) {
            super(stmt);
            const g = c.grids[stmt.inGrid];
            const alphabetSize = g.grid.alphabet.key.length;
            const colours = stmt.output;
            this.c2iOffset = colours[0];
            const colourRange = colours[colours.length - 1] + 1 - colours[0];
            const colourToIndex = emptyArray(colourRange, 0);
            for (let i = 0; i < colours.length; ++i) {
                const d = colours[i] - colours[0];
                colourToIndex[d] = i + 1;
            }
            this.colourArray = IR.makeConstArray(c.ir, 'colours', colours, alphabetSize);
            this.c2iArray = IR.makeConstArray(c.ir, 'c2i', colourToIndex, colours.length + 1);
            const matchesCount = c.ir.varDecl('convchainCount', IR.INT_TYPE);
            this.matchesCount = matchesCount.name;
            const constDecls = [
                this.colourArray.decl,
                this.c2iArray.decl,
                (this.matchesArray?.decl ?? IR.NO_DECL)
            ];
            const varDecls = [
                matchesCount,
            ];
            if (stmt.on.kind !== 'top') {
                const matches = this.matchesArray = IR.makeMutableArray(c.ir, 'convchainMatches', g.n, IR.INT32_ARRAY_TYPE.domainSize);
                constDecls.push(matches.decl);
            }
            else {
                this.matchesArray = undefined;
            }
            const logEpsilon = Math.log2(stmt.epsilon);
            this.score = g.makeWeightedCounter(stmt.weights.map(w => ({
                pattern: w.pattern,
                weight: Math.log2(w.weight) - logEpsilon,
            })), true);
            if ((stmt.temperature !== undefined && stmt.temperature.kind !== 'expr.constant') || stmt.anneal !== undefined) {
                const temperature = c.ir.varDecl('temperature', IR.FLOAT_TYPE);
                this.temperature = temperature.name;
                varDecls.push(temperature);
            }
            else {
                this.temperature = undefined;
            }
            if (stmt.anneal !== undefined && stmt.anneal.kind !== 'expr.constant') {
                const anneal = c.ir.varDecl('anneal', IR.FLOAT_TYPE);
                this.anneal = anneal.name;
                varDecls.push(anneal);
            }
            else {
                this.anneal = undefined;
            }
            this.rootDecl = IR.multiDecl(constDecls);
            this.parentDecl = IR.multiDecl(varDecls);
            for (const op of [
                'float_plus', 'float_minus', 'float_mult', 'float_log2',
                'float_eq', 'float_ne', 'float_lt', 'float_le', 'float_gt', 'float_ge',
            ]) {
                c.opsUsed.add(op);
            }
        }
        getFromMatchesArray(index) {
            const { matchesArray } = this;
            return matchesArray !== undefined ? matchesArray.get(index) : index;
        }
        declareRoot(ctx) {
            return this.rootDecl;
        }
        declareParent(ctx) {
            return this.parentDecl;
        }
        compileReset(ctx) {
            return IR.assign(this.matchesCount, '=', IR.MINUS_ONE);
        }
        compile(ctx, ifTrue, ifFalse) {
            const { stmt } = this;
            const c = ctx.c;
            const g = c.grids[stmt.inGrid];
            const alphabetKey = g.grid.alphabet.key;
            const on = stmt.on.masks[0];
            const output = stmt.output;
            const outputMask = ISet.of(alphabetKey.length, output);
            const initIsDefinitelyEffective = ISet.isDisjoint(on, outputMask);
            const initIsDefinitelyIneffective = ISet.isSubset(on, outputMask);
            const sampler = g.makeSampler([stmt.on]);
            const initSampler = initIsDefinitelyIneffective ? new IR.EmptySampler() : g.makeSampler([
                new Pattern(1, 1, alphabetKey, [-2], [ISet.difference(on, outputMask)], true),
            ]);
            const temperatureInit = stmt.temperature !== undefined ? ctx.expr(stmt.temperature) : IR.FLOAT_ONE;
            const annealInit = stmt.anneal !== undefined ? ctx.expr(stmt.anneal) : IR.FLOAT_ZERO;
            const { colourArray, c2iArray, c2iOffset, matchesArray, matchesCount, score, temperature, anneal } = this;
            // TODO: can this use integer arithmetic?
            const keepWithProbability = temperatureInit === IR.FLOAT_ZERO ? IR.FALSE
                : IR.binaryOp('float_lt', IR.binaryOp('float_mult', temperature ?? temperatureInit, OP.log2(IR.binaryOp('float_minus', IR.FLOAT_ONE, c.prng.nextDouble()))), score);
            const getRandomColour = colourArray.get(c.prng.nextInt(IR.int(output.length)));
            const getRandomDifferentColour = (oldS) => colourArray.get(OP.modConstant(OP.add(c2iArray.get(OP.minusConstant(oldS, c2iOffset)), c.prng.nextInt(IR.int(output.length - 1))), output.length));
            const update1x1 = (at) => g.update(at.x, at.y, IR.ONE, IR.ONE);
            if (c.config.animate) {
                ifTrue = IR.seq([g.yield_(), ifTrue]);
            }
            const flag = c.ir.flag();
            const branchOnFlag = IR.if_(flag.check, ifTrue, ifFalse);
            const shouldInit = OP.lt(matchesCount, IR.ZERO);
            const doInit = IR.seq([
                matchesArray !== undefined ? sampler.copyInto(matchesArray) : IR.PASS,
                IR.assign(matchesCount, '=', sampler.count),
                temperature !== undefined ? IR.seq([
                    IR.assign(temperature, '=', temperatureInit),
                    temperatureInit.kind !== 'expr.literal.float' ? IR.if_(IR.binaryOp('float_lt', temperature, IR.FLOAT_ZERO), IR.throw_(`'temperature' must be non-negative`)) : IR.PASS,
                ]) : IR.PASS,
                anneal !== undefined ? IR.seq([
                    IR.assign(anneal, '=', annealInit),
                    IR.if_(IR.binaryOp('float_lt', anneal, IR.FLOAT_ZERO), IR.throw_(`'anneal' must be non-negative`)),
                ]) : IR.PASS,
                initIsDefinitelyIneffective ? IR.PASS : IR.if_(initSampler === sampler ? OP.gt(matchesCount, IR.ZERO) : initSampler.isNotEmpty, IR.seq([
                    initSampler === sampler
                        ? c.ir.forRange('i', IR.ZERO, matchesCount, i => g.write(this.getFromMatchesArray(i), getRandomColour))
                        : initSampler.forEach(at => g.write(at.index, getRandomColour)),
                    g.update(IR.ZERO, IR.ZERO, g.width, g.height),
                    flag.set,
                ])),
            ]);
            const doMain = IR.seq([
                c.ir.forRange('i', IR.ZERO, matchesCount, () => g.declareAtIndex(this.getFromMatchesArray(c.prng.nextInt(matchesCount)), at => c.ir.withConst('oldS', IR.BYTE_TYPE, g.data.get(at.index), oldS => IR.seq([
                    IR.assign(score, '=', IR.FLOAT_ZERO),
                    g.write(at.index, getRandomDifferentColour(oldS)),
                    update1x1(at),
                    IR.if_(OP.or(IR.binaryOp('float_ge', score, IR.FLOAT_ZERO), keepWithProbability), flag.set, IR.seq([
                        g.write(at.index, oldS),
                        update1x1(at),
                    ])),
                ])))),
                temperature !== undefined && annealInit !== IR.FLOAT_ZERO ? IR.assign(temperature, '=', IR.ternary(IR.binaryOp('float_gt', temperature, anneal ?? annealInit), IR.binaryOp('float_minus', temperature, anneal ?? annealInit), IR.FLOAT_ZERO)) : IR.PASS,
            ]);
            const r = initIsDefinitelyIneffective ? IR.seq([
                IR.if_(shouldInit, doInit),
                IR.withDecl(flag.decl, IR.seq([
                    doMain,
                    branchOnFlag,
                ])),
            ])
                : IR.withDecl(flag.decl, initIsDefinitelyEffective ? IR.seq([
                    IR.if_(shouldInit, doInit, doMain),
                    branchOnFlag,
                ])
                    : IR.seq([
                        IR.if_(shouldInit, doInit),
                        IR.if_(OP.not(flag.check), doMain),
                        branchOnFlag,
                    ]));
            return [r, ctx];
        }
    }
    Compiler.Stmt_ConvChain = Stmt_ConvChain;
})(Compiler || (Compiler = {}));
///<reference path="../ir/factory.ts"/>
var Compiler;
(function (Compiler) {
    class Stmt_Convolution extends Compiler.StmtCompiler {
        compile(ctx, ifTrue, ifFalse) {
            const { stmt } = this;
            const c = ctx.c;
            const g = c.grids[stmt.inGrid];
            const buffer = g.makeConvBuffer(stmt.kernel);
            const flag = c.ir.flag();
            function buildCases(at, atConv) {
                const convCtx = ctx.withConvPosition(at, atConv);
                const cases = emptyArray(g.grid.alphabet.key.length, IR.PASS);
                for (const rule of stmt.rewrites) {
                    const mask = PatternTree.isLeafOrTop(rule.from) ? rule.from.masks[0] : fail();
                    const caseHandler = convCtx.usePattern(rule.to, p => IR.if_(Compiler.writeCondition(convCtx, g, rule.from, p, rule.condition), IR.seq([
                        Compiler.doWrite(convCtx, g, rule.from, p, undefined, false, false),
                        flag.set,
                    ])));
                    ISet.forEach(mask, i => {
                        if (cases[i] !== IR.PASS) {
                            fail();
                        }
                        cases[i] = caseHandler;
                    });
                }
                return cases;
            }
            // TODO: different strategy if rules don't cover the whole alphabet?
            const r = IR.withDecl(flag.decl, IR.seq([
                c.ir.forRange('y', IR.ZERO, g.height, y => c.ir.forRange('x', IR.ZERO, g.width, x => g.declareAtXY(x, y, at => c.ir.withConst('atConv', IR.INT_TYPE, buffer.index(x, y), atConv => IR.switch_(g.data.get(at.index), buildCases(at, atConv)))))),
                IR.if_(flag.check, IR.seq([
                    // TODO: this is suboptimal, but need to defer update until all `sum` expressions are evaluated
                    g.update(IR.ZERO, IR.ZERO, g.width, g.height),
                    c.config.animate ? g.yield_() : IR.PASS,
                    ifTrue,
                ]), ifFalse),
            ]));
            return [r, ctx];
        }
    }
    Compiler.Stmt_Convolution = Stmt_Convolution;
})(Compiler || (Compiler = {}));
///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
var Compiler;
(function (Compiler) {
    const { OP, } = IR;
    class Stmt_BasicOne extends Compiler.StmtCompiler {
        compile(ctx, ifTrue, ifFalse) {
            const { rewrites } = this.stmt;
            const c = ctx.c;
            const g = c.grids[this.stmt.inGrid];
            const sampler = g.makeSampler(rewrites.map(rule => rule.from));
            // optimisation for common case: all rewrites are unconditional and definitely effective
            const allUnconditionalAndEffective = rewrites.every(rule => rule.to.kind === 'expr.constant'
                && PatternTree.isDisjoint(rule.from, rule.to.constant.value) && ctx.isTautology(rule.condition));
            const buildCases = (at, ifT, ifF) => {
                const ruleCtx = ctx.withPosition(at);
                return rewrites.map(rule => ruleCtx.usePattern(rule.to, p => IR.if_(Compiler.writeCondition(ruleCtx, g, rule.from, p, rule.condition), IR.seq([
                    Compiler.doWrite(ruleCtx, g, rule.from, p, undefined, true, true),
                    ifT,
                ]), ifF)));
            };
            const r = allUnconditionalAndEffective
                ? IR.if_(sampler.isNotEmpty, IR.seq([
                    sampler.sampleWithReplacement(c.prng, at => buildCases(at, IR.PASS, IR.PASS)),
                    ifTrue,
                ]), ifFalse)
                : sampler.sampleWithoutReplacement(c.prng, buildCases, ifTrue, ifFalse);
            return [r, ctx];
        }
    }
    Compiler.Stmt_BasicOne = Stmt_BasicOne;
})(Compiler || (Compiler = {}));
///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
///<reference path="../ir/stmt.ts"/>
var Compiler;
(function (Compiler) {
    const { OP, } = IR;
    class Stmt_Put extends Compiler.StmtCompiler {
        compile(ctx, ifTrue, ifFalse) {
            const { stmt } = this;
            const c = ctx.c;
            const g = c.grids[stmt.inGrid];
            const { pattern } = this.stmt;
            // check whether pattern is effective
            const isEffective = (at) => pattern.kind === 'expr.constant' ? OP.some(pattern.constant.value, (dx, dy, c) => OP.ne(g.data.get(g.relativeIndex(at, dx, dy)), IR.int(c)))
                : IR.TRUE;
            const r = g.declareAtIndex(ctx.expr(stmt.at), at => {
                const ruleCtx = ctx.withPosition(at);
                return IR.seq([
                    // check bounds, given size of pattern
                    g.grid.periodic ? IR.PASS : IR.if_(OP.or(pattern.type.width > 1 ? OP.ge(OP.addConstant(at.x, pattern.type.width - 1), g.width) : IR.FALSE, pattern.type.height > 1 ? OP.ge(OP.addConstant(at.y, pattern.type.height - 1), g.height) : IR.FALSE), IR.throw_('pattern would be out of bounds')),
                    ruleCtx.usePattern(pattern, p => IR.if_(OP.and(isEffective(at), Compiler.writeCondition(ruleCtx, g, undefined, p.constant !== undefined ? undefined : p, stmt.condition)), Compiler.doWrite(ruleCtx, g, undefined, p, undefined, true, true))),
                    ifFalse,
                ]);
            });
            return [r, ctx];
        }
    }
    Compiler.Stmt_Put = Stmt_Put;
})(Compiler || (Compiler = {}));
///<reference path="../ir/types.ts"/>
///<reference path="./ctx.ts"/>
///<reference path="./stmt.ts"/>
///<reference path="./stmt_all.ts"/>
///<reference path="./stmt_convchain.ts"/>
///<reference path="./stmt_convolution.ts"/>
///<reference path="./stmt_one.ts"/>
///<reference path="./stmt_put.ts"/>
/**
 * Compiles MJr source code to a specified target language.
 */
var Compiler;
(function (Compiler) {
    Compiler.COMPILER_VERSION = '0.2 (unstable)';
    Compiler.REQUIRED_RUNTIME_LIB_VERSION = 0;
    const ALWAYS_USED_OPS = [
        'bool_and', 'bool_or', 'bool_not',
        'int_eq', 'int_ne', 'int_lt', 'int_le', 'int_gt', 'int_ge',
        'int_and', 'int_or', 'int_xor', 'int_not', 'int_lshift', 'int_rshift', 'int_ctz',
        'loose_int_plus', 'loose_int_mult', 'loose_int_floordiv', 'loose_int_mod',
    ];
    // helpers
    const { OP } = IR;
    /**
     * Compiles MJr source code to a high-level intermediate representation.
     */
    class CompilerImpl {
        asg;
        config;
        diagnostics = new Diagnostics();
        opsUsed = new Set(ALWAYS_USED_OPS);
        ir;
        grids;
        prng;
        entryPointName;
        width;
        height;
        mainDecls;
        mainParams;
        checkMaxIterations;
        mask;
        maskScale = 0;
        tempArray;
        tempArrayScale = 0;
        constructor(asg, config) {
            this.asg = asg;
            this.config = config;
            for (const g of asg.grids) {
                if (g.periodic) {
                    this.notSupported('periodic grid', g.pos);
                }
            }
            const ir = this.ir = new IR.Factory();
            this.entryPointName = ir.func(config.entryPointName);
            const width = ir.paramDecl('width', IR.INT_TYPE), height = ir.paramDecl('height', IR.INT_TYPE), rng = ir.paramDecl('rng', IR.PRNG_TYPE, true);
            this.width = width.name;
            this.height = height.name;
            this.grids = asg.grids.map(g => new IR.Grid(ir, g, width.name, height.name));
            this.prng = new IR.PRNG(rng.name);
            this.mask = new IR.Mask(ir);
            this.tempArray = new IR.TempArray(this.ir);
            const mainDecls = [];
            const mainParams = [width, height];
            if (asg.params.size > 0) {
                const p = ir.paramDecl('params', IR.nullableType({
                    kind: 'dict',
                    keys: Array.from(asg.params.keys()),
                    values: Array.from(asg.params.values(), t => IR.nullableType(this.type(t))),
                }));
                mainParams.push(p);
            }
            mainParams.push(rng);
            const maxIterations = config.maxIterations;
            if (maxIterations !== 0) {
                const iterations = ir.varDecl('iterations', IR.INT_TYPE, IR.ZERO);
                mainDecls.push(iterations);
                this.checkMaxIterations = IR.seq([
                    IR.assign(iterations.name, '+=', IR.ONE),
                    IR.if_(OP.gt(iterations.name, IR.int(maxIterations)), IR.throw_(`Exceeded maximum of ${maxIterations} iterations`)),
                ]);
            }
            else {
                this.checkMaxIterations = IR.PASS;
            }
            this.mainDecls = mainDecls;
            this.mainParams = mainParams;
        }
        notSupported(s, pos) {
            this.diagnostics.compilationError(`${s} is not currently supported`, pos);
        }
        getStmtCompiler(stmt) {
            const cls = Compiler.STMT_COMPILERS[stmt.kind];
            return new cls(stmt, this);
        }
        compile() {
            // YYYY-MM-DD HH:mm:ss GMT+offset
            const date = new Date().toLocaleString('en-SE', { timeZoneName: 'short' });
            const { asg, config, grids } = this;
            const ctx = Compiler.CompilerContext.root(this);
            // TODO: potentials
            // need to compile everything before preamble, so that `maxScale` and `this.opsUsed` are correct
            const rootCompiler = this.getStmtCompiler(asg.root), declRoot = rootCompiler.declareRoot(ctx), declRootMut = rootCompiler.declareParent(ctx), compiledRoot = rootCompiler.compile(ctx, IR.PASS, IR.PASS)[0], gridDecls = grids.map(g => g.declare()), tmpArraysDecl = this.declareTempArrays();
            // compute maximum grid dimensions, to ensure that loose int operations on array lengths/indices don't overflow
            // 0x3FFFFFFE is the magic number for the largest allowed array length for an LFSR
            const maxScale = Math.max(this.maskScale, this.tempArrayScale, ...grids.map(g => g.getScale()));
            const maxDim = IR.int(Math.sqrt(0x3FFFFFFE / maxScale) | 0);
            const checkDims = config.emitChecks ? IR.if_(OP.or(OP.le(this.width, IR.ZERO), OP.le(this.height, IR.ZERO)), IR.throw_('Grid dimensions must be positive'), IR.if_(OP.or(OP.gt(this.width, maxDim), OP.gt(this.height, maxDim)), IR.throw_(`Grid dimensions cannot exceed ${maxDim.value}`))) : IR.PASS;
            return IR.exportDecl(IR.funcDecl(this.entryPointName, config.animate ? IR.REWRITE_INFO_TYPE : undefined, this.mainParams, IR.GRID_TYPE, IR.seq([
                IR.comment(`compiled by mjrc-${Compiler.COMPILER_VERSION} on ${date}`),
                checkDims,
                IR.preamble(this.dictType(asg.params), config.emitChecks, Compiler.REQUIRED_RUNTIME_LIB_VERSION, Array.from(this.opsUsed)),
                IR.BLANK_LINE,
                IR.withDecls([
                    ...this.mainDecls,
                    ...gridDecls,
                    ...tmpArraysDecl,
                    ...this.internedConstants.values(),
                    declRoot,
                    declRootMut,
                ], IR.seq([
                    IR.BLANK_LINE,
                    compiledRoot,
                    IR.return_(grids[asg.endGridID].obj),
                ])),
            ])));
        }
        type(type) {
            return type.kind === 'dict' ? this.dictType(type.entryTypes)
                : type.kind !== 'pattern.in' ? TYPES_TO_IR[type.kind]
                    : fail();
        }
        dictType(entryTypes) {
            const keys = Array.from(entryTypes.keys()).sort();
            // TODO: declare type aliases for dict types
            return { kind: 'dict', keys, values: keys.map(k => this.type(entryTypes.get(k))) };
        }
        internedConstants = new Map();
        internConstant(expr, type) {
            return getOrCompute(this.internedConstants, IR.key(expr), () => this.ir.constDecl('constant', type, expr)).name;
        }
        useMask(g) {
            this.maskScale = Math.max(this.maskScale, g.grid.scaleX * g.grid.scaleY);
            return this.mask;
        }
        useTempArray(scale) {
            this.tempArrayScale = Math.max(this.tempArrayScale, scale);
            return this.tempArray;
        }
        declareTempArrays() {
            const decls = [];
            if (this.maskScale > 0) {
                const maskScale = OP.multConstant(OP.mult(this.width, this.height), this.maskScale);
                decls.push(this.mask.declare(maskScale));
            }
            if (this.tempArrayScale > 0) {
                // TODO: in principle, we can get a better estimate for the maximum number of matches if we know some patterns cannot overlap
                const tempArrayScale = OP.multConstant(OP.mult(this.width, this.height), this.tempArrayScale);
                decls.push(this.tempArray.declare(tempArrayScale));
            }
            return decls;
        }
    }
    const TYPES_TO_IR = {
        bool: IR.BOOL_TYPE,
        float: IR.FLOAT_TYPE,
        fraction: IR.FRACTION_TYPE,
        grid: IR.GRID_TYPE,
        int: IR.INT_TYPE,
        'pattern.out': IR.PATTERN_TYPE,
        position: IR.INT_TYPE,
        str: IR.STR_TYPE,
    };
    Compiler.STMT_COMPILERS = {
        // control-flow
        'stmt.block.markov': Compiler.Stmt_Markov,
        'stmt.block.sequence': Compiler.Stmt_Sequence,
        'stmt.modified.limit': Compiler.Stmt_Limit,
        // non-branching
        'stmt.assign': Compiler.Stmt_Assign,
        'stmt.log': Compiler.Stmt_Log,
        'stmt.rules.map': Compiler.Stmt_NotSupported,
        'stmt.put': Compiler.Stmt_Put,
        'stmt.use': Compiler.Stmt_Use,
        // branching
        'stmt.convchain': Compiler.Stmt_ConvChain,
        'stmt.path': Compiler.Stmt_NotSupported,
        'stmt.rules.basic.all': Compiler.Stmt_BasicAllPrl,
        'stmt.rules.basic.one': Compiler.Stmt_BasicOne,
        'stmt.rules.basic.prl': Compiler.Stmt_BasicAllPrl,
        'stmt.rules.biased.all': Compiler.Stmt_NotSupported,
        'stmt.rules.biased.one': Compiler.Stmt_NotSupported,
        'stmt.rules.convolution': Compiler.Stmt_Convolution,
        'stmt.rules.search.all': Compiler.Stmt_NotSupported,
        'stmt.rules.search.one': Compiler.Stmt_NotSupported,
    };
    function compile(src, targetLanguage, config) {
        const ast = Parser.parse(src);
        const asg = Resolver.resolve(ast);
        const compiler = new CompilerImpl(asg, config !== undefined ? { ...Compiler.DEFAULT_CONFIG, ...config } : Compiler.DEFAULT_CONFIG);
        const result = compiler.compile();
        compiler.diagnostics.throwIfAnyErrors();
        if (result.info.hasFreeVars()) {
            fail(`compilation result had free variables`, result);
        }
        const cls = CodeGen[targetLanguage];
        const gen = new cls(compiler.config);
        gen.beginGenerating();
        gen.writeStmt(result);
        gen.diagnostics.throwIfAnyErrors();
        return gen.render();
    }
    Compiler.compile = compile;
})(Compiler || (Compiler = {}));
var Compiler;
(function (Compiler) {
    Compiler.DEFAULT_CONFIG = {
        indentSpaces: 4,
        emitComments: true,
        emitChecks: true,
        entryPointName: 'main',
        maxIterations: 0,
        animate: false,
    };
})(Compiler || (Compiler = {}));
///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>
var Compiler;
(function (Compiler) {
    const { OP, } = IR;
    function doWrite(ctx, outGrid, from, to, mask, doUpdate, doYield) {
        const out = [];
        let mX, mY, eW, eH;
        if (to.constant !== undefined) {
            // constant output pattern
            const value = to.constant;
            value.forEach((dx, dy, colour) => {
                // TODO: `[ABC] or [ADE] -> [A..]` isn't maybe effective in the first position
                const maybeEffective = from === undefined || from.kind !== 'leaf' || from.pattern[dx + from.width * dy] !== colour;
                if (mask !== undefined || maybeEffective) {
                    out.push(outGrid.write(outGrid.relativeIndex(ctx.at, dx, dy), IR.int(colour), mask));
                }
            });
            mX = IR.int(value.minX);
            mY = IR.int(value.minY);
            eW = IR.int(value.effectiveWidth);
            eH = IR.int(value.effectiveHeight);
        }
        else {
            // output pattern determined at runtime
            out.push(IR.libMethodCallStmt('Pattern', 'put', to.expr, [outGrid.obj, mask !== undefined ? mask.arr.array : IR.NULL, ctx.at.x, ctx.at.y]));
            mX = IR.attr(to.expr, 'minX');
            mY = IR.attr(to.expr, 'minY');
            eW = IR.attr(to.expr, 'effectiveWidth');
            eH = IR.attr(to.expr, 'effectiveHeight');
        }
        if (doUpdate) {
            const x = OP.add(ctx.at.x, mX), y = OP.add(ctx.at.y, mY);
            out.push(outGrid.update(x, y, eW, eH));
            if (doYield && ctx.c.config.animate) {
                out.push(outGrid.yieldRewriteInfo(x, y, eW, eH));
            }
        }
        return IR.seq(out);
    }
    Compiler.doWrite = doWrite;
    function patternHasEffect(ctx, g, from, to) {
        return to.constant === undefined
            ? IR.libMethodCall('Pattern', 'hasEffect', to.expr, [g.obj, ctx.at.x, ctx.at.y])
            : from !== undefined && PatternTree.isDisjoint(from, to.constant)
                ? IR.TRUE
                : OP.some(to.constant, (dx, dy, c) => from === undefined || !PatternTree.matches(from, p => !p.canBe(dx, dy, c))
                    ? OP.ne(g.data.get(g.relativeIndex(ctx.at, dx, dy)), IR.int(c))
                    : IR.TRUE);
    }
    Compiler.patternHasEffect = patternHasEffect;
    function writeCondition(ctx, g, from, to, conditionExpr) {
        return OP.and(to !== undefined ? patternHasEffect(ctx, g, from, to) : IR.TRUE, conditionExpr !== undefined ? ctx.expr(conditionExpr) : IR.TRUE);
    }
    Compiler.writeCondition = writeCondition;
})(Compiler || (Compiler = {}));
var CodeGen;
(function (CodeGen) {
    CodeGen.NOOP = Symbol();
    function binaryOp(p, before, lMinP, between, rMinP, after) {
        return { p, before, lMinP, between, rMinP, after };
    }
    CodeGen.binaryOp = binaryOp;
    function unaryOp(p, before, cMinP, after) {
        return { p, before, cMinP, after };
    }
    CodeGen.unaryOp = unaryOp;
    function infixOp(p, op, associativity = 1 /* Associativity.LEFT */) {
        const lMinP = (associativity & 1 /* Associativity.LEFT */) !== 0 ? p : p + 1;
        const rMinP = (associativity & 2 /* Associativity.RIGHT */) !== 0 ? p : p + 1;
        return binaryOp(p, '', lMinP, ` ${op} `, rMinP, '');
    }
    CodeGen.infixOp = infixOp;
    function prefixOp(p, op) {
        return unaryOp(p, op, p, '');
    }
    CodeGen.prefixOp = prefixOp;
    function statements(stmt) {
        return stmt.kind === 'stmt.sequence' ? stmt.children : [stmt];
    }
    CodeGen.statements = statements;
    function _getImmediatelyDeclaredNames(node) {
        switch (node.kind) {
            case 'decl.func':
                return [node.name, ...node.params.map(p => p.name)];
            case 'decl.var.const':
            case 'decl.var.loop':
            case 'decl.var.mut':
            case 'decl.var.param':
                return [node.name];
            case 'stmt.for.range':
                return [node.index.name];
            case 'expr.letin':
                return [node.decl.name];
            default:
                return [];
        }
    }
    class Base {
        config;
        diagnostics = new Diagnostics();
        _out = [];
        _indentationLevel = 0;
        _indent = '';
        _toAssign = new Map();
        LPAREN = '(';
        RPAREN = ')';
        LBLOCK = '{';
        RBLOCK = '}';
        _reserved = new Set();
        _namesInUse = [];
        _namesMap = new Map();
        constructor(config) {
            this.config = config;
        }
        beginGenerating() {
            const { _reserved } = this;
            for (const word of this.RESERVED_WORDS.split(/\s+/)) {
                _reserved.add(word);
            }
        }
        beginLine() {
            const { _out } = this;
            if (_out.length > 0 && !_out[_out.length - 1].endsWith('\n')) {
                _out.push('\n');
            }
            _out.push(this._indent);
        }
        write(s) {
            this._out.push(s);
        }
        indent() {
            const i = ++this._indentationLevel;
            this._indent = ' '.repeat(this.config.indentSpaces * i);
        }
        dedent() {
            const i = --this._indentationLevel;
            this._indent = ' '.repeat(this.config.indentSpaces * i);
        }
        getName(expr) {
            return this._namesMap.get(expr.id) ?? fail(`use of name (${expr.namePart}) outside of its declared scope`, expr);
        }
        getLibName(name) {
            while (this._reserved.has(name)) {
                name += '_';
            }
            return name;
        }
        declareNames(node) {
            const { _reserved, _namesInUse, _namesMap } = this;
            const nameExprs = _getImmediatelyDeclaredNames(node);
            for (const expr of nameExprs) {
                //if(_namesMap.has(expr.id)) { fail(`redeclared name`, expr); }
                let name = expr.namePart, suffix = 1;
                while (_reserved.has(name) || _namesInUse.includes(name)) {
                    ++suffix;
                    name = `${expr.namePart}${suffix}`;
                }
                _namesInUse.push(name);
                _namesMap.set(expr.id, name);
            }
        }
        withScope(f) {
            const { _namesInUse } = this;
            const n = _namesInUse.length;
            f();
            _namesInUse.length = n;
        }
        writeBlockScope(stmt) {
            this.withScope(() => this.writeIndentedBlock(stmt));
        }
        writeIndentedBlock(stmt) {
            this.write(this.LBLOCK);
            this.indent();
            this.writeStmt(stmt);
            this.dedent();
            this.beginLine();
            this.write(this.RBLOCK);
        }
        writeStmt(stmt) {
            while (stmt.kind === 'stmt.decl') {
                this.beginLine();
                this.writeDecl(stmt.decl);
                stmt = stmt.child;
            }
            switch (stmt.kind) {
                case 'stmt.blankline': {
                    this.beginLine();
                    return;
                }
                case 'stmt.comment': {
                    if (!this.config.emitComments) {
                        return;
                    }
                    break;
                }
                case 'stmt.sequence': {
                    for (const child of stmt.children) {
                        this.writeStmt(child);
                    }
                    return;
                }
            }
            this.declareNames(stmt);
            const f = this.STMT_WRITE_FUNCS[stmt.kind];
            f(this, stmt);
        }
        writeDecl(decl) {
            switch (decl.kind) {
                case 'decl.var.loop':
                case 'decl.init': {
                    fail(`'${decl.kind}' was not replaced`);
                }
                case 'decl.none': {
                    return;
                }
                case 'decl.multi': {
                    for (const d of decl.children) {
                        this.writeDecl(d);
                    }
                    return;
                }
            }
            this.declareNames(decl);
            const f = this.DECL_WRITE_FUNCS[decl.kind];
            f(this, decl);
        }
        writeExpr(expr, minPrecedence = 0) {
            switch (expr.kind) {
                case 'expr.unused.deferred': {
                    fail('deferred expression was not substituted', expr.purpose);
                }
                case 'expr.unused.error': {
                    fail('unused expression was not eliminated', expr.error);
                }
                case 'expr.letin': {
                    // moving initialiser forwards is only sound if it commutes
                    if (this.writeAssignExpr !== undefined && expr.decl.initialiser.info.commutesWith(expr.child)) {
                        this.declareNames(expr.decl);
                        this._toAssign.set(expr.decl.name.id, expr.decl.initialiser);
                        this.writeExpr(expr.child, minPrecedence);
                        return;
                    }
                    break;
                }
                case 'expr.name': {
                    const rhs = this._toAssign.get(expr.id);
                    if (this.writeAssignExpr !== undefined && rhs !== undefined) {
                        this._toAssign.delete(expr.id);
                        if (minPrecedence > 0) {
                            this.write(this.LPAREN);
                        }
                        this.writeAssignExpr(expr, rhs);
                        if (minPrecedence > 0) {
                            this.write(this.RPAREN);
                        }
                        return;
                    }
                    break;
                }
                case 'expr.op.unary': {
                    const spec = this.UNARY_OPS[expr.op];
                    if (spec === CodeGen.NOOP) {
                        this.writeExpr(expr.child, minPrecedence);
                    }
                    else {
                        const { p, before, cMinP, after } = spec;
                        if (p < minPrecedence) {
                            this.write(this.LPAREN);
                        }
                        this.write(before);
                        this.writeExpr(expr.child, cMinP);
                        this.write(after);
                        if (p < minPrecedence) {
                            this.write(this.RPAREN);
                        }
                    }
                    return;
                }
                case 'expr.op.binary': {
                    const { p, before, lMinP, between, rMinP, after } = this.BINARY_OPS[expr.op];
                    if (p < minPrecedence) {
                        this.write(this.LPAREN);
                    }
                    this.write(before);
                    this.writeExpr(expr.left, lMinP);
                    this.write(between);
                    this.writeExpr(expr.right, rMinP);
                    this.write(after);
                    if (p < minPrecedence) {
                        this.write(this.RPAREN);
                    }
                    return;
                }
            }
            this.declareNames(expr);
            const [p, f] = this.EXPR_WRITE_FUNCS[expr.kind];
            if (p < minPrecedence) {
                this.write(this.LPAREN);
            }
            f(this, expr);
            if (p < minPrecedence) {
                this.write(this.RPAREN);
            }
        }
        writeList(writer, n, rowLength = IR.DEFAULT_ROW_LENGTH, compact = false, sep = ',') {
            const multiline = rowLength < n;
            if (multiline) {
                this.indent();
                if (!compact) {
                    this.beginLine();
                }
            }
            for (let i = 0; i < n; ++i) {
                writer(i);
                if (i < n - 1) {
                    this.write(sep);
                    if (i % rowLength < rowLength - 1) {
                        this.write(' ');
                    }
                    else {
                        this.beginLine();
                    }
                }
            }
            if (multiline) {
                this.dedent();
                if (!compact) {
                    this.beginLine();
                }
            }
        }
        writeExprList(exprs, rowLength = IR.DEFAULT_ROW_LENGTH, compact = false, sep = ',') {
            this.writeList(i => this.writeExpr(exprs[i]), exprs.length, rowLength, compact, sep);
        }
        writeLongStringLiteral(s, rowLength, concatOp = ' +') {
            if (s.length === 0) {
                this.write(`""`);
                return;
            }
            rowLength *= Math.ceil(4 * IR.DEFAULT_ROW_LENGTH / rowLength);
            this.writeList(i => {
                const start = i * rowLength;
                this.write(JSON.stringify(s.slice(start, start + rowLength)));
            }, Math.ceil(s.length / rowLength), 1, false, concatOp);
        }
        render() {
            return this._out.join('');
        }
    }
    CodeGen.Base = Base;
    function signedIntBits(domainSize) {
        return domainSize <= (1 << 7) ? 8
            : domainSize <= (1 << 15) ? 16
                : 32;
    }
    CodeGen.signedIntBits = signedIntBits;
    function uintBits(domainSize) {
        return domainSize <= (1 << 8) ? 8
            : domainSize <= (1 << 16) ? 16
                : 32;
    }
    CodeGen.uintBits = uintBits;
    function uintBitsFours(domainSize) {
        return domainSize <= (1 << 4) ? 4
            : domainSize <= (1 << 8) ? 8
                : domainSize <= (1 << 12) ? 12
                    : domainSize <= (1 << 16) ? 16
                        : domainSize <= (1 << 20) ? 20
                            : domainSize <= (1 << 24) ? 24
                                : domainSize <= (1 << 28) ? 28
                                    : 32;
    }
    CodeGen.uintBitsFours = uintBitsFours;
    function arrayToHex(arr, bitsPerElement) {
        const digitsPerElement = bitsPerElement >> 2;
        return arr.map(x => x.toString(16).padStart(digitsPerElement, '0')).join('');
    }
    CodeGen.arrayToHex = arrayToHex;
})(CodeGen || (CodeGen = {}));
///<reference path="base.ts"/>
var CodeGen;
(function (CodeGen) {
    const RUNTIME_LIB_NAME = 'MJr';
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#keywords
    const JAVASCRIPT_KEYWORDS = `abstract arguments as async await boolean break byte case catch char class const continue debugger default delete do double else enum eval export extends false final finally float for from function get goto if implements import in instanceof int interface let long native new null of package private protected public return set short static super switch synchronized this throw throws transient true try typeof undefined var void volatile while with yield`;
    const _literal = [16 /* Precedence.LITERAL */, (out, expr) => {
            out.write(JSON.stringify(expr.value));
        }];
    class JavaScript extends CodeGen.Base {
        RESERVED_WORDS = `${RUNTIME_LIB_NAME} ${JAVASCRIPT_KEYWORDS} console`;
        STMT_WRITE_FUNCS = {
            'stmt.assign': (out, stmt) => {
                const { left, op, right } = stmt;
                out.beginLine();
                if ((op === '+=' || op === '-=') && right === IR.ONE) {
                    out.write(op === '+=' ? '++' : '--');
                    out.writeExpr(left);
                }
                else {
                    out.writeExpr(left);
                    out.write(` ${op} `);
                    out.writeExpr(right);
                }
                out.write(';');
            },
            'stmt.break': (out, stmt) => {
                out.beginLine();
                out.write('break;');
            },
            'stmt.comment': (out, stmt) => {
                out.beginLine();
                out.write(`// ${stmt.comment}`);
            },
            'stmt.continue': (out, stmt) => {
                out.beginLine();
                out.write('continue;');
            },
            'stmt.expr': (out, stmt) => {
                out.beginLine();
                out.writeExpr(stmt.expr);
                out.write(';');
            },
            'stmt.export': (out, stmt) => {
                // TODO: JS module config option
                out.writeDecl(stmt.decl);
            },
            'stmt.for.range': (out, stmt) => {
                const { index, low, high, reverse, body } = stmt;
                const i = out.getName(index.name);
                out.beginLine();
                out.write(`for(let ${i} = `);
                if (reverse) {
                    out.writeExpr(IR.OP.minus(high, IR.ONE));
                    out.write(`; ${i} >= `);
                    out.writeExpr(low, 9 /* Precedence.CMP */);
                    out.write(`; --${i}) `);
                }
                else {
                    out.writeExpr(low);
                    out.write(`; ${i} < `);
                    out.writeExpr(high, 9 /* Precedence.CMP */);
                    out.write(`; ++${i}) `);
                }
                out.writeBlockScope(body);
            },
            'stmt.if': (out, stmt) => {
                let cur = stmt;
                out.beginLine();
                while (true) {
                    out.write('if(');
                    out.writeExpr(cur.condition);
                    out.write(') ');
                    // intented block has braces, avoiding parsing hazard of `if(...) if(...) ... else ...`
                    out.writeBlockScope(cur.then);
                    cur = cur.otherwise;
                    if (cur === undefined) {
                        break;
                    }
                    out.write(' else ');
                    if (cur.kind !== 'stmt.if') {
                        out.writeBlockScope(cur);
                        break;
                    }
                }
            },
            'stmt.log': (out, stmt) => {
                out.beginLine();
                out.write('console.log(');
                out.writeExpr(stmt.expr);
                out.write(')');
            },
            'stmt.pass': (out, stmt) => {
                out.beginLine();
                out.write(';');
            },
            'stmt.preamble': (out, stmt) => {
                out.beginLine();
                out.write('width = width | 0;');
                out.beginLine();
                out.write('height = height | 0;');
                if (stmt.emitChecks) {
                    // TODO: add code to check params are valid at runtime
                    const { libVersion } = stmt;
                    out.beginLine();
                    out.beginLine();
                    out.write(`if(typeof ${RUNTIME_LIB_NAME} !== "object" || typeof ${RUNTIME_LIB_NAME}.VERSION !== "number") throw new Error("${RUNTIME_LIB_NAME} runtime library not found");`);
                    out.beginLine();
                    out.write(`if(${RUNTIME_LIB_NAME}.VERSION !== ${libVersion}) throw new Error("Requires ${RUNTIME_LIB_NAME} runtime library version ${libVersion}");`);
                }
                out.beginLine();
                out.write(`rng ??= ${RUNTIME_LIB_NAME}.DEFAULT_PRNG;`);
                for (const op of stmt.opsUsed) {
                    if (objHasKey(MJr.OPS, op)) {
                        out.beginLine();
                        out.write(`const ${op} = ${RUNTIME_LIB_NAME}.OPS.${op};`);
                    }
                }
            },
            'stmt.return': (out, stmt) => {
                out.beginLine();
                out.write('return');
                if (stmt.expr !== undefined) {
                    out.write(' ');
                    out.writeExpr(stmt.expr);
                }
                out.write(';');
            },
            'stmt.switch': (out, stmt) => {
                out.beginLine();
                out.writeSwitch(stmt);
            },
            'stmt.throw': (out, stmt) => {
                out.beginLine();
                out.write(`throw new Error(${JSON.stringify(stmt.message)});`);
            },
            'stmt.while': (out, stmt) => {
                const { then } = stmt;
                out.beginLine();
                out.write('while(');
                out.writeExpr(stmt.condition);
                out.write(') ');
                out.writeBlockScope(then);
            },
            'stmt.yield': (out, stmt) => {
                out.beginLine();
                out.write('yield');
                if (stmt.expr !== undefined) {
                    out.write(' ');
                    out.writeExpr(stmt.expr);
                }
                out.write(';');
            },
        };
        DECL_WRITE_FUNCS = {
            'decl.func': (out, decl) => {
                const { params } = decl;
                out.write('function');
                if (decl.yields !== undefined) {
                    out.write('*');
                }
                out.write(` ${this.getName(decl.name)}(`);
                out.withScope(() => {
                    out.writeList(i => out.writeVarDecl(params[i]), params.length);
                    out.write(')');
                    out.writeReturnType(decl.returnType, decl.yields);
                    out.write(' ');
                    out.writeIndentedBlock(decl.body);
                });
            },
            'decl.var.const': (out, decl) => {
                out.write('const ');
                out.writeVarDecl(decl);
                out.write(';');
            },
            'decl.var.mut': (out, decl) => {
                out.write('let ');
                out.writeVarDecl(decl);
                out.write(';');
            },
            'decl.var.param': (out, decl) => {
                out.writeVarDecl(decl);
            },
        };
        EXPR_WRITE_FUNCS = {
            'expr.array.const': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    const { from } = expr;
                    const bits = CodeGen.uintBitsFours(expr.domainSize);
                    const s = CodeGen.arrayToHex(from, bits);
                    out.write(`${RUNTIME_LIB_NAME}.HEX.u${bits}(`);
                    out.writeLongStringLiteral(s, expr.rowLength * s.length / from.length);
                    out.write(')');
                }],
            'expr.array.new': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    const bits = CodeGen.uintBits(expr.domainSize);
                    out.write(`new Uint${bits}Array(`);
                    out.writeExpr(expr.length);
                    out.write(')');
                }],
            'expr.attr': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.left, 17 /* Precedence.ATTR_ACCESS_CALL */);
                    out.write(`.${expr.attr}`);
                }],
            'expr.dict': [18 /* Precedence.MAX */, (out, expr) => {
                    const { type: { keys }, values } = expr;
                    out.write('{');
                    out.writeList(i => {
                        out.write(`${keys[i]}: `);
                        out.writeExpr(values[i]);
                    }, keys.length, 1);
                    out.write('}');
                }],
            'expr.letin': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    // TODO: eliminate 'expr.letin' before codegen
                    out.write('(function(');
                    let e = expr;
                    while (true) {
                        out.writeAssignExpr(e.decl.name, e.decl.initialiser);
                        e = e.child;
                        if (e.kind !== 'expr.letin') {
                            break;
                        }
                        out.write(', ');
                    }
                    out.write(') { return ');
                    out.writeExpr(e);
                    out.write('; })()');
                }],
            'expr.literal.bool': _literal,
            'expr.literal.float': _literal,
            'expr.literal.int': _literal,
            'expr.literal.str': _literal,
            'expr.literal.null': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write('undefined');
                }],
            'expr.name': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write(this.getName(expr));
                }],
            'expr.op.call.lib.constructor': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write(`new ${RUNTIME_LIB_NAME}.${expr.className}`);
                    out.write(`(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.lib.function': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write(`${RUNTIME_LIB_NAME}.${expr.name}`);
                    out.write(`(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.lib.method': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.obj);
                    out.write(`.${this.getLibName(expr.name)}(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.local': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write(`${out.getName(expr.name)}(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.param': [3 /* Precedence.NULL_COALESCE */, (out, expr) => {
                    out.write(`params?['${expr.name}'] ?? `);
                    out.writeExpr(expr.otherwise, 3 /* Precedence.NULL_COALESCE */);
                }],
            'expr.op.ternary': [2 /* Precedence.TERNARY */, (out, expr) => {
                    out.writeExpr(expr.condition, 2 /* Precedence.TERNARY */ + 1);
                    out.write(' ? ');
                    out.writeExpr(expr.then, 2 /* Precedence.TERNARY */);
                    out.write(' : ');
                    out.writeExpr(expr.otherwise, 2 /* Precedence.TERNARY */);
                }],
        };
        BINARY_OPS = (function () {
            function _intOp(p, op) {
                // all of these ops are left-associative
                return CodeGen.binaryOp(14 /* Precedence.BITWISE_NOT */, '~~(', p, ` ${op} `, p + 1, ')');
            }
            function _func(name) {
                return CodeGen.binaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `${name}(`, 0 /* Precedence.MIN */, ', ', 0 /* Precedence.MIN */, ')');
            }
            const PLUS = CodeGen.infixOp(11 /* Precedence.PLUS_MINUS */, '+', 3 /* Associativity.BOTH */), MINUS = CodeGen.infixOp(11 /* Precedence.PLUS_MINUS */, '-'), EQ = CodeGen.infixOp(8 /* Precedence.EQ */, '==='), NE = CodeGen.infixOp(8 /* Precedence.EQ */, '!=='), LT = CodeGen.infixOp(9 /* Precedence.CMP */, '<'), LE = CodeGen.infixOp(9 /* Precedence.CMP */, '<='), GT = CodeGen.infixOp(9 /* Precedence.CMP */, '>'), GE = CodeGen.infixOp(9 /* Precedence.CMP */, '>=');
            return {
                array_access: CodeGen.binaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, '', 17 /* Precedence.ATTR_ACCESS_CALL */, '[', 0 /* Precedence.MIN */, ']'),
                bool_and: CodeGen.infixOp(4 /* Precedence.BOOL_AND */, '&&', 3 /* Associativity.BOTH */),
                bool_or: CodeGen.infixOp(3 /* Precedence.BOOL_OR */, '||', 3 /* Associativity.BOTH */),
                bool_eq: EQ,
                bool_ne: NE,
                // + and * are not strictly right-associative for floats
                float_plus: CodeGen.infixOp(11 /* Precedence.PLUS_MINUS */, '+'),
                float_minus: MINUS,
                float_mult: CodeGen.infixOp(12 /* Precedence.MULT_DIV_MOD */, '*'),
                float_truediv: CodeGen.infixOp(12 /* Precedence.MULT_DIV_MOD */, '/'),
                float_mod: _func('float_mod'),
                float_eq: EQ,
                float_ne: NE,
                float_lt: LT,
                float_le: LE,
                float_gt: GT,
                float_ge: GE,
                fraction_plus: _func('fraction_plus'),
                fraction_minus: _func('fraction_minus'),
                fraction_mult: _func('fraction_mult'),
                fraction_truediv: _func('fraction_truediv'),
                fraction_eq: _func('fraction_eq'),
                fraction_ne: _func('fraction_ne'),
                fraction_lt: _func('fraction_lt'),
                fraction_le: _func('fraction_le'),
                fraction_gt: _func('fraction_gt'),
                fraction_ge: _func('fraction_ge'),
                int_plus: _intOp(11 /* Precedence.PLUS_MINUS */, '+'),
                int_minus: _intOp(11 /* Precedence.PLUS_MINUS */, '-'),
                int_mult: _func('Math.imul'),
                int_truediv: _func('int_truediv'),
                int_floordiv: _func('int_floordiv'),
                int_mod: _func('int_mod'),
                int_eq: EQ,
                int_ne: NE,
                int_lt: LT,
                int_le: LE,
                int_gt: GT,
                int_ge: GE,
                int_and: CodeGen.infixOp(7 /* Precedence.BITWISE_AND */, '&', 3 /* Associativity.BOTH */),
                int_or: CodeGen.infixOp(5 /* Precedence.BITWISE_OR */, '|', 3 /* Associativity.BOTH */),
                int_xor: CodeGen.infixOp(6 /* Precedence.BITWISE_XOR */, '^', 3 /* Associativity.BOTH */),
                int_lshift: CodeGen.infixOp(10 /* Precedence.BITWISE_SHIFT */, '<<'),
                int_rshift: CodeGen.infixOp(10 /* Precedence.BITWISE_SHIFT */, '>>'),
                str_concat: PLUS,
                str_eq: EQ,
                str_ne: NE,
                loose_int_plus: PLUS,
                loose_int_minus: MINUS,
                loose_int_mult: CodeGen.infixOp(12 /* Precedence.MULT_DIV_MOD */, '*', 3 /* Associativity.BOTH */),
                loose_int_floordiv: _intOp(12 /* Precedence.MULT_DIV_MOD */, '/'),
                loose_int_mod: CodeGen.infixOp(12 /* Precedence.MULT_DIV_MOD */, '%'),
            };
        })();
        UNARY_OPS = (function () {
            function _intOp(p, op) {
                return CodeGen.unaryOp(5 /* Precedence.BITWISE_OR */, `(${op}`, p, ') | 0');
            }
            function _func(name) {
                return CodeGen.unaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `${name}(`, 0 /* Precedence.MIN */, ')');
            }
            const TO_STR = CodeGen.unaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, '', 17 /* Precedence.ATTR_ACCESS_CALL */, '.toString()');
            return {
                bool_not: CodeGen.prefixOp(14 /* Precedence.BOOL_NOT */, '!'),
                // need space to avoid incorrect parse of `- - x`
                float_uminus: CodeGen.prefixOp(14 /* Precedence.UPLUS_UMINUS */, '- '),
                float_checkzero: _func('float_checkzero'),
                float_log2: _func('Math.log2'),
                fraction_uminus: _func('fraction_uminus'),
                fraction_checkzero: CodeGen.NOOP,
                // need space to avoid incorrect parse of `- - x`
                int_uminus: _intOp(14 /* Precedence.UPLUS_UMINUS */, '- '),
                int_checkzero: _func('int_checkzero'),
                int_not: CodeGen.prefixOp(14 /* Precedence.BITWISE_NOT */, '~'),
                int_ctz: _func('int_ctz'),
                int_to_float: CodeGen.NOOP,
                int_to_fraction: _func('int_to_fraction'),
                bool_to_str: TO_STR,
                float_to_str: TO_STR,
                fraction_to_str: _func('fraction_to_str'),
                grid_to_str: TO_STR,
                int_to_str: TO_STR,
            };
        })();
        writeAssignExpr(left, right) {
            // TODO: this omits `let`, making a global variable; need to hoist declaration
            this.writeExpr(left);
            this.write(' = ');
            this.writeExpr(right, 2 /* Precedence.ASSIGN */);
        }
        writeSwitch(stmt) {
            this.write('switch(');
            this.writeExpr(stmt.expr);
            this.write(') {');
            this.indent();
            for (const c of stmt.cases) {
                this.beginLine();
                for (const value of c.values) {
                    this.write(`case ${value}: `);
                }
                this.writeBlockScope(IR.seq([c.then, IR.BREAK]));
            }
            this.dedent();
            this.beginLine();
            this.write('}');
        }
        writeVarDecl(decl) {
            this.write(this.getName(decl.name));
            if (decl.initialiser !== undefined) {
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type, yields) { }
    }
    CodeGen.JavaScript = JavaScript;
    class TypeScript extends JavaScript {
        writeParamAnnotation(type) {
            if (type.kind === 'nullable') {
                this.write('?');
                type = type.componentType;
            }
            this.write(': ');
            this.writeType(type);
        }
        writeVarDecl(decl) {
            this.write(this.getName(decl.name));
            if (decl.kind === 'decl.var.param') {
                this.writeParamAnnotation(decl.type);
            }
            else if (decl.initialiser === undefined) {
                this.write(': ');
                this.writeType(decl.type);
            }
            else {
                // otherwise, type should be inferred from initialiser
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type, yields) {
            this.write(': ');
            if (yields === undefined) {
                this.writeType(type);
            }
            else {
                this.write(`Generator<`);
                this.writeType(yields);
                this.write(`, `);
                this.writeType(type);
                this.write(`>`);
            }
        }
        writeType(type) {
            switch (type.kind) {
                case 'dict': {
                    const { keys, values } = type;
                    this.write('{');
                    this.writeList(i => {
                        this.write(keys[i]);
                        this.writeParamAnnotation(values[i]);
                    }, keys.length);
                    this.write('}');
                    return;
                }
                case 'array.const':
                case 'array.mutable': {
                    this.write(`Uint${CodeGen.uintBits(type.domainSize)}Array`);
                    return;
                }
                case 'nullable': {
                    this.writeType(type.componentType);
                    this.write(' | undefined');
                    return;
                }
                default: {
                    this.write(TYPES_TO_TS[type.kind]);
                    return;
                }
            }
        }
    }
    CodeGen.TypeScript = TypeScript;
    const TYPES_TO_TS = {
        bool: 'boolean',
        byte: 'number',
        float: 'number',
        fraction: `${RUNTIME_LIB_NAME}.Fraction`,
        grid: `${RUNTIME_LIB_NAME}.Grid`,
        int: 'number',
        pattern: `${RUNTIME_LIB_NAME}.Pattern`,
        prng: `${RUNTIME_LIB_NAME}.PRNG`,
        rewriteinfo: `${RUNTIME_LIB_NAME}.RewriteInfo`,
        sampler: `${RUNTIME_LIB_NAME}.Sampler`,
        str: 'string',
        void: 'void',
    };
})(CodeGen || (CodeGen = {}));
///<reference path="base.ts"/>
var CodeGen;
(function (CodeGen) {
    const RUNTIME_LIB_NAME = 'MJr';
    // https://docs.python.org/3/reference/lexical_analysis.html#keywords
    const PYTHON_KEYWORDS = `_ False None True and as assert async await break case class continue def del elif else except finally for from global if import in is lambda match nonlocal not or pass raise return try while with yield`;
    const _literal = [18 /* Precedence.MAX */, (out, expr) => {
            out.write(JSON.stringify(expr.value));
        }];
    class Python extends CodeGen.Base {
        RESERVED_WORDS = `${RUNTIME_LIB_NAME} ${PYTHON_KEYWORDS} Error Fraction array float int32 int_ctz math print range str`;
        LBLOCK = ":";
        RBLOCK = "";
        constructor(config) {
            super(config);
            if (config.indentSpaces === 0) {
                this.diagnostics.configError(`Python output requires 'indentSpaces' to be non-zero`);
            }
        }
        STMT_WRITE_FUNCS = {
            'stmt.assign': (out, stmt) => {
                const { left, op, right } = stmt;
                out.beginLine();
                out.writeExpr(left);
                out.write(` ${op} `);
                out.writeExpr(right);
            },
            'stmt.break': (out, stmt) => {
                out.beginLine();
                out.write('break');
            },
            'stmt.comment': (out, stmt) => {
                out.beginLine();
                out.write(`# ${stmt.comment}`);
            },
            'stmt.continue': (out, stmt) => {
                out.beginLine();
                out.write('continue');
            },
            'stmt.export': (out, stmt) => {
                out.writeDecl(stmt.decl);
            },
            'stmt.expr': (out, stmt) => {
                out.beginLine();
                out.writeExpr(stmt.expr);
            },
            'stmt.for.range': (out, stmt) => {
                const { low, high } = stmt;
                out.beginLine();
                out.write(`for ${this.getName(stmt.index.name)} in range(`);
                if (stmt.reverse) {
                    out.writeExpr(IR.OP.minus(high, IR.ONE));
                    out.write(`, `);
                    out.writeExpr(IR.OP.minus(low, IR.ONE));
                    out.write(`, -1)`);
                }
                else {
                    if (low !== IR.ZERO) {
                        out.writeExpr(low);
                        out.write(`, `);
                    }
                    out.writeExpr(high);
                    out.write(`)`);
                }
                out.writeIndentedBlock(stmt.body);
            },
            'stmt.if': (out, stmt) => {
                let cur = stmt;
                out.beginLine();
                out.write('if ');
                while (true) {
                    out.writeExpr(cur.condition);
                    out.writeIndentedBlock(cur.then);
                    cur = cur.otherwise;
                    if (cur === undefined) {
                        break;
                    }
                    out.beginLine();
                    if (cur.kind === 'stmt.if') {
                        out.write('elif ');
                    }
                    else {
                        out.write('else');
                        out.writeIndentedBlock(cur);
                        break;
                    }
                }
            },
            'stmt.log': (out, stmt) => {
                out.beginLine();
                out.write('print(');
                out.writeExpr(stmt.expr);
                out.write(')');
            },
            'stmt.pass': (out, stmt) => {
                out.beginLine();
                out.write('pass');
            },
            'stmt.preamble': (out, stmt) => {
                out.beginLine();
                out.write(`import ${RUNTIME_LIB_NAME}`);
                if (stmt.emitChecks) {
                    // TODO: add code to check params are valid at runtime
                    const { libVersion } = stmt;
                    out.beginLine();
                    out.write(`if ${RUNTIME_LIB_NAME}.VERSION != ${libVersion}: raise Error("Requires ${RUNTIME_LIB_NAME} runtime library version ${libVersion}")`);
                }
                out.beginLine();
                out.write(`if rng is None: rng = ${RUNTIME_LIB_NAME}.DefaultPRNG()`);
                out.beginLine();
                out.write(`import array`);
                out.beginLine();
                out.write(`int32 = ${RUNTIME_LIB_NAME}.int32`);
                out.beginLine();
                out.write(`int_ctz = ${RUNTIME_LIB_NAME}.int_ctz`);
                if (stmt.opsUsed.includes('int_truediv') || stmt.opsUsed.includes('int_to_fraction')) {
                    out.beginLine();
                    out.write('from fractions import Fraction');
                }
                if (stmt.opsUsed.includes('float_log2')) {
                    out.beginLine();
                    out.write('import math');
                }
            },
            'stmt.return': (out, stmt) => {
                out.beginLine();
                out.write('return');
                if (stmt.expr !== undefined) {
                    out.write(' ');
                    out.writeExpr(stmt.expr);
                }
            },
            'stmt.switch': (out, stmt) => {
                out.beginLine();
                out.write('match ');
                out.writeExpr(stmt.expr);
                out.write(':');
                out.indent();
                for (const c of stmt.cases) {
                    out.beginLine();
                    out.write(`case ${c.values.join(' | ')}`);
                    out.writeIndentedBlock(c.then);
                }
                out.dedent();
            },
            'stmt.throw': (out, stmt) => {
                out.beginLine();
                out.write(`raise Error(${JSON.stringify(stmt.message)})`);
            },
            'stmt.while': (out, stmt) => {
                out.beginLine();
                out.write('while ');
                out.writeExpr(stmt.condition);
                out.writeIndentedBlock(stmt.then);
            },
            'stmt.yield': (out, stmt) => {
                out.beginLine();
                out.write('yield');
                if (stmt.expr !== undefined) {
                    out.write(' ');
                    out.writeExpr(stmt.expr);
                }
            },
        };
        DECL_WRITE_FUNCS = {
            'decl.func': (out, decl) => {
                const { params } = decl;
                out.beginLine();
                out.write(`def ${this.getName(decl.name)}(`);
                out.withScope(() => {
                    out.writeList(i => out.writeVarDecl(params[i]), params.length);
                    out.write(')');
                    out.writeReturnType(decl.returnType);
                    out.writeIndentedBlock(decl.body);
                });
            },
            'decl.var.const': (out, decl) => this.writeVarDecl(decl),
            'decl.var.mut': (out, decl) => this.writeVarDecl(decl),
            'decl.var.param': (out, decl) => this.writeVarDecl(decl),
        };
        EXPR_WRITE_FUNCS = {
            'expr.array.const': [18 /* Precedence.MAX */, (out, expr) => {
                    const { from, domainSize, rowLength } = expr;
                    const bits = CodeGen.uintBitsFours(domainSize);
                    const s = CodeGen.arrayToHex(from, bits);
                    out.write(`${RUNTIME_LIB_NAME}.hex_to_arr("${_typecode(bits)}", ${bits >> 2}, `);
                    out.writeLongStringLiteral(s, rowLength * s.length / from.length, '');
                    out.write(`)`);
                }],
            'expr.array.new': [13 /* Precedence.MULT_DIV_MOD */, (out, expr) => {
                    const bits = CodeGen.uintBits(expr.domainSize);
                    // https://docs.python.org/3/library/array.html
                    out.write(`array.array("${_typecode(bits)}", (0,)) * `);
                    out.writeExpr(expr.length, 13 /* Precedence.MULT_DIV_MOD */);
                }],
            'expr.attr': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.left, 17 /* Precedence.ATTR_ACCESS_CALL */);
                    out.write(`.${expr.attr}`);
                }],
            'expr.dict': [18 /* Precedence.MAX */, (out, expr) => {
                    const { type: { keys }, values } = expr;
                    out.write('{');
                    out.writeList(i => {
                        out.write(`"${keys[i]}": `);
                        out.writeExpr(values[i]);
                    }, keys.length, 1);
                    out.write('}');
                }],
            'expr.letin': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write('(');
                    let e = expr;
                    do {
                        out.writeAssignExpr(e.decl.name, e.decl.initialiser);
                        out.write(', ');
                        e = e.child;
                    } while (e.kind === 'expr.letin');
                    out.writeExpr(e);
                    out.write(')[-1]');
                }],
            'expr.literal.bool': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write(expr.value ? 'True' : 'False');
                }],
            'expr.literal.float': [18 /* Precedence.MAX */, (out, expr) => {
                    const s = `${expr.value}`;
                    out.write(s);
                    if (/^-?[0-9]+$/.test(s)) {
                        out.write('.0');
                    }
                }],
            'expr.literal.int': _literal,
            'expr.literal.str': _literal,
            'expr.literal.null': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write('None');
                }],
            'expr.name': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write(out.getName(expr));
                }],
            'expr.op.call.lib.constructor': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write(`${RUNTIME_LIB_NAME}.${out.getLibName(expr.className)}(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.lib.function': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write(`${RUNTIME_LIB_NAME}.${out.getLibName(expr.name)}(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.lib.method': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.obj);
                    out.write(`.${out.getLibName(expr.name)}(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.local': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write(`${out.getName(expr.name)}(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.param': [3 /* Precedence.TERNARY */, (out, expr) => {
                    // TODO: take params as **kwargs
                    out.write(`params['${expr.name}'] if params is not None and '${expr.name}' in params else `);
                    out.writeExpr(expr.otherwise, 3 /* Precedence.TERNARY */);
                }],
            'expr.op.ternary': [3 /* Precedence.TERNARY */, (out, expr) => {
                    out.writeExpr(expr.then, 3 /* Precedence.TERNARY */ + 1);
                    out.write(' if ');
                    out.writeExpr(expr.condition, 3 /* Precedence.TERNARY */ + 1);
                    out.write(' else ');
                    out.writeExpr(expr.otherwise, 3 /* Precedence.TERNARY */);
                }],
        };
        BINARY_OPS = (function () {
            function _cmpOp(op) {
                // Python's comparison ops are neither left- nor right-associative; need to avoid them chaining
                return CodeGen.infixOp(7 /* Precedence.CMP_EQ */, op, 0 /* Associativity.NEITHER */);
            }
            function _intOp(p, op) {
                // all of these ops are left-associative
                return CodeGen.binaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, 'int32(', p, ` ${op} `, p + 1, ')');
            }
            function _func(name) {
                return CodeGen.binaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `${name}(`, 0 /* Precedence.MIN */, ', ', 0 /* Precedence.MIN */, ')');
            }
            // PLUS and MULT are not strictly right-associative for floats
            const PLUS = CodeGen.infixOp(12 /* Precedence.PLUS_MINUS */, '+'), MINUS = CodeGen.infixOp(12 /* Precedence.PLUS_MINUS */, '-'), MULT = CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '*'), DIV = CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '/'), FLOORDIV = CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '//'), MOD = CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '%'), EQ = _cmpOp('=='), NE = _cmpOp('!='), LT = _cmpOp('<'), LE = _cmpOp('<='), GT = _cmpOp('>'), GE = _cmpOp('>=');
            return {
                array_access: CodeGen.binaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, '', 17 /* Precedence.ATTR_ACCESS_CALL */, '[', 0 /* Precedence.MIN */, ']'),
                bool_and: CodeGen.infixOp(5 /* Precedence.BOOL_AND */, 'and', 3 /* Associativity.BOTH */),
                bool_or: CodeGen.infixOp(4 /* Precedence.BOOL_OR */, 'or', 3 /* Associativity.BOTH */),
                bool_eq: EQ,
                bool_ne: NE,
                // + and * are not strictly right-associative for floats
                float_plus: CodeGen.infixOp(12 /* Precedence.PLUS_MINUS */, '+'),
                float_minus: MINUS,
                float_mult: CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '*'),
                float_truediv: CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '/'),
                float_mod: MOD,
                float_eq: EQ,
                float_ne: NE,
                float_lt: LT,
                float_le: LE,
                float_gt: GT,
                float_ge: GE,
                fraction_plus: PLUS,
                fraction_minus: MINUS,
                fraction_mult: MULT,
                fraction_truediv: DIV,
                fraction_eq: EQ,
                fraction_ne: NE,
                fraction_lt: LT,
                fraction_le: LE,
                fraction_gt: GT,
                fraction_ge: GE,
                int_plus: _intOp(12 /* Precedence.PLUS_MINUS */, '+'),
                int_minus: _intOp(12 /* Precedence.PLUS_MINUS */, '-'),
                int_mult: _intOp(13 /* Precedence.MULT_DIV_MOD */, '*'),
                int_truediv: _func('Fraction'),
                int_floordiv: FLOORDIV,
                int_mod: MOD,
                int_eq: EQ,
                int_ne: NE,
                int_lt: LT,
                int_le: LE,
                int_gt: GT,
                int_ge: GE,
                int_and: CodeGen.infixOp(10 /* Precedence.BITWISE_AND */, '&', 3 /* Associativity.BOTH */),
                int_or: CodeGen.infixOp(8 /* Precedence.BITWISE_OR */, '|', 3 /* Associativity.BOTH */),
                int_xor: CodeGen.infixOp(9 /* Precedence.BITWISE_XOR */, '^', 3 /* Associativity.BOTH */),
                int_lshift: CodeGen.infixOp(11 /* Precedence.BITWISE_SHIFT */, '<<'),
                int_rshift: CodeGen.infixOp(11 /* Precedence.BITWISE_SHIFT */, '>>'),
                str_concat: PLUS,
                str_eq: EQ,
                str_ne: NE,
                loose_int_plus: PLUS,
                loose_int_minus: MINUS,
                loose_int_mult: MULT,
                loose_int_floordiv: FLOORDIV,
                loose_int_mod: MOD,
            };
        })();
        UNARY_OPS = (function () {
            function _intOp(p, op) {
                return CodeGen.unaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `int32(${op}`, p, ')');
            }
            function _func(name) {
                return CodeGen.unaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `${name}(`, 0 /* Precedence.MIN */, ')');
            }
            const UMINUS = CodeGen.prefixOp(14 /* Precedence.UPLUS_UMINUS */, '-'), TO_STR = _func('str');
            // 'checkzero' ops are NOOPs in Python; all of the relevant operations already raise errors for divzero
            return {
                bool_not: CodeGen.prefixOp(6 /* Precedence.BOOL_NOT */, 'not '),
                float_uminus: UMINUS,
                float_checkzero: CodeGen.NOOP,
                float_log2: _func('math.log2'),
                fraction_uminus: UMINUS,
                fraction_checkzero: CodeGen.NOOP,
                int_uminus: _intOp(14 /* Precedence.UPLUS_UMINUS */, '-'),
                int_checkzero: CodeGen.NOOP,
                int_not: CodeGen.prefixOp(14 /* Precedence.BITWISE_NOT */, '~'),
                int_ctz: _func('int_ctz'),
                int_to_float: _func('float'),
                int_to_fraction: _func('Fraction'),
                bool_to_str: TO_STR,
                float_to_str: TO_STR,
                fraction_to_str: TO_STR,
                grid_to_str: TO_STR,
                int_to_str: TO_STR,
            };
        })();
        writeAssignExpr(left, right) {
            this.writeExpr(left);
            this.write(' := ');
            this.writeExpr(right, 2 /* Precedence.ASSIGN */);
        }
        writeVarDecl(decl) {
            this.writeExpr(decl.name);
            if (decl.kind !== 'decl.var.param') {
                this.write(' = ');
                this.writeExpr(decl.initialiser ?? IR.NULL);
            }
            else if (decl.isOptional) {
                this.write('=None');
            }
        }
        writeReturnType(type) { }
    }
    CodeGen.Python = Python;
    class PythonWithTypes extends Python {
        writeVarDecl(decl) {
            this.writeExpr(decl.name);
            this.write(': ');
            this.writeType(decl.type);
            if (decl.initialiser !== undefined) {
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type) {
            this.write(' -> ');
            this.writeType(type);
        }
        writeType(type) {
            switch (type.kind) {
                case 'dict': {
                    const { keys, values } = type;
                    // TODO: need `stmt.typedecl` or otherwise; declare namedtuple types
                    this.write('{');
                    this.writeList(i => {
                        this.write(`${keys[i]}: `);
                        this.writeType(values[i]);
                    }, keys.length);
                    this.write('}');
                    return;
                }
                case 'array.const': {
                    this.write(type.domainSize <= 256 ? 'bytes' : 'array.array');
                    return;
                }
                case 'array.mutable': {
                    this.write(type.domainSize <= 256 ? 'bytearray' : 'array.array');
                    return;
                }
                case 'nullable': {
                    this.writeType(type.componentType);
                    this.write(' | None');
                    return;
                }
                default: {
                    this.write(TYPES_TO_PY[type.kind]);
                    return;
                }
            }
        }
    }
    CodeGen.PythonWithTypes = PythonWithTypes;
    function _typecode(bits) {
        return bits <= 8 ? 'B'
            : bits <= 16 ? 'I'
                : 'L';
    }
    const TYPES_TO_PY = {
        bool: 'bool',
        byte: 'int',
        float: 'float',
        fraction: `Fraction`,
        grid: `${RUNTIME_LIB_NAME}.Grid`,
        int: 'int',
        pattern: `${RUNTIME_LIB_NAME}.Pattern`,
        prng: `${RUNTIME_LIB_NAME}.PRNG`,
        rewriteinfo: `${RUNTIME_LIB_NAME}.RewriteInfo`,
        sampler: `${RUNTIME_LIB_NAME}.Sampler`,
        str: 'str',
        void: 'None',
    };
})(CodeGen || (CodeGen = {}));
/**
 * Type declarations for the abstract semantic graph (ASG).
 */
var ASG;
(function (ASG) {
    function exprIsRuntimeConstant(expr) {
        return (expr.flags & 1 /* ExprFlags.RUNTIME_CONSTANT */) !== 0;
    }
    ASG.exprIsRuntimeConstant = exprIsRuntimeConstant;
    function exprIsGridIndependent(expr) {
        return (expr.flags & 16 /* ExprFlags.GRID_INDEPENDENT */) !== 0;
    }
    ASG.exprIsGridIndependent = exprIsGridIndependent;
    function exprIsSameEverywhere(expr) {
        return (expr.flags & 12 /* ExprFlags.SAME_EVERYWHERE */) === 12 /* ExprFlags.SAME_EVERYWHERE */;
    }
    ASG.exprIsSameEverywhere = exprIsSameEverywhere;
})(ASG || (ASG = {}));
var MJr;
(function (MJr) {
    // MJr runtime version 0 is unstable!
    MJr.VERSION = 0;
    MJr.DIV_ZERO_MESSAGE = 'division by zero';
    function checkZero(y) {
        if (y === 0) {
            throw new Error(MJr.DIV_ZERO_MESSAGE);
        }
        return y;
    }
    function modulo(x, y) {
        return x - y * Math.floor(x / y);
    }
    function fraction(p, q) {
        checkZero(q);
        // Euclid's algorithm
        let x = p, y = q;
        while (y !== 0) {
            x %= y;
            const tmp = x;
            x = y;
            y = tmp;
        }
        return { p: p / x, q: q / x };
    }
    MJr.fraction = fraction;
    MJr.OPS = {
        float_mod: modulo,
        float_checkzero: checkZero,
        fraction_plus: (x, y) => fraction(x.p * y.q + y.p * x.q, x.q * y.q),
        fraction_minus: (x, y) => fraction(x.p * y.q - y.p * x.q, x.q * y.q),
        fraction_mult: (x, y) => fraction(x.p * y.p, x.q * y.q),
        fraction_truediv: (x, y) => fraction(x.p * y.q, x.q * y.p),
        fraction_uminus: (x) => ({ p: -x.p, q: x.q }),
        fraction_eq: (x, y) => x.p === y.p && x.q === y.q,
        fraction_ne: (x, y) => x.p !== y.p || x.q !== y.q,
        fraction_lt: (x, y) => x.p * y.q < y.p * x.q,
        fraction_le: (x, y) => x.p * y.q <= y.p * x.q,
        fraction_gt: (x, y) => x.p * y.q > y.p * x.q,
        fraction_ge: (x, y) => x.p * y.q >= y.p * x.q,
        fraction_to_str: (x) => x.q === 1 ? `${x.p}` : `${x.p}/${x.q}`,
        int_truediv: fraction,
        int_floordiv: (x, y) => Math.floor(x / y) | 0,
        int_mod: (x, y) => modulo(x, y) | 0,
        int_ctz: (x) => 31 - Math.clz32(x & -x),
        int_checkzero: checkZero,
        int_to_fraction: (x) => ({ p: x, q: 1 }),
    };
    MJr.HEX = {
        u4(s) {
            const arr = new Uint8Array(s.length);
            for (let i = 0; i < s.length; ++i) {
                arr[i] = parseInt(s.substring(i, i + 1), 16);
            }
            return arr;
        },
        u8(s) {
            const arr = new Uint8Array(s.length >> 1);
            for (let i = 0; i < s.length; i += 2) {
                arr[i >> 1] = parseInt(s.substring(i, i + 2), 16);
            }
            return arr;
        },
        u12(s) {
            const n = (s.length / 3) | 0;
            const arr = new Uint16Array(n);
            for (let i = 0, j = 0; i < n; ++i, j += 3) {
                arr[i] = parseInt(s.substring(j, j + 3), 16);
            }
            return arr;
        },
        u16(s) {
            const arr = new Uint16Array(s.length >> 2);
            for (let i = 0; i < s.length; i += 4) {
                arr[i >> 2] = parseInt(s.substring(i, i + 4), 16);
            }
            return arr;
        },
        u20(s) {
            const n = (s.length / 5) | 0;
            const arr = new Uint32Array(n);
            for (let i = 0, j = 0; i < n; ++i, j += 5) {
                arr[i] = parseInt(s.substring(j, j + 5), 16);
            }
            return arr;
        },
        u24(s) {
            const n = (s.length / 6) | 0;
            const arr = new Uint32Array(n);
            for (let i = 0, j = 0; i < n; ++i, j += 6) {
                arr[i] = parseInt(s.substring(j, j + 6), 16);
            }
            return arr;
        },
        u28(s) {
            const n = (s.length / 7) | 0;
            const arr = new Uint32Array(n);
            for (let i = 0, j = 0; i < n; ++i, j += 7) {
                arr[i] = parseInt(s.substring(j, j + 7), 16);
            }
            return arr;
        },
        u32(s) {
            const arr = new Uint32Array(s.length >> 3);
            for (let i = 0; i < s.length; i += 8) {
                arr[i >> 3] = parseInt(s.substring(i, i + 8), 16);
            }
            return arr;
        },
    };
    MJr.DEFAULT_PRNG = {
        nextDouble: Math.random,
        nextInt: n => (Math.random() * n) | 0,
    };
    MJr.SAMPLE_EMPTY_MESSAGE = 'sample from empty range';
    function nextIntChecked(rng, n) {
        if (n <= 0) {
            throw new Error(MJr.SAMPLE_EMPTY_MESSAGE);
        }
        return rng.nextInt(n);
    }
    MJr.nextIntChecked = nextIntChecked;
    function lfsrFeedbackTerm(n) {
        // http://users.ece.cmu.edu/~koopman/lfsr/
        if (n < 0xFF) {
            return 0xA6;
        }
        else if (n < 0x3FF) {
            return 0x344;
        }
        else if (n < 0xFFF) {
            return 0xAF5;
        }
        else if (n < 0x3FFF) {
            return 0x243F;
        }
        else if (n < 0xFFFF) {
            return 0x8580;
        }
        else if (n < 0x3FFFF) {
            return 0x204C9;
        }
        else if (n < 0xFFFFF) {
            return 0x80534;
        }
        else if (n < 0x3FFFFF) {
            return 0x200634;
        }
        else if (n < 0xFFFFFF) {
            return 0x8009F8;
        }
        else if (n < 0x3FFFFFF) {
            return 0x20006B9;
        }
        else if (n < 0xFFFFFFF) {
            return 0x8000893;
        }
        else {
            return 0x20000A46;
        }
    }
    MJr.lfsrFeedbackTerm = lfsrFeedbackTerm;
    class Grid {
        width;
        height;
        data;
        alphabet;
        constructor(width, height, data, alphabet) {
            this.width = width;
            this.height = height;
            this.data = data;
            this.alphabet = alphabet;
        }
        index(x, y) {
            if (x < 0 || x >= this.width || y < 0 || y >= this.height) {
                throw new Error(`position out of bounds: (${x}, ${y})`);
            }
            return x + y * this.width;
        }
        wrapIndex(x, y) {
            return MJr.OPS.int_mod(x, this.width) + MJr.OPS.int_mod(y, this.height) * this.width;
        }
        toString() {
            const { width, data, alphabet } = this;
            const out = [];
            for (let i = 0; i < data.length; ++i) {
                if (i > 0 && i % width === 0) {
                    out.push('\n');
                }
                out.push(alphabet[data[i]]);
            }
            return out.join('');
        }
    }
    MJr.Grid = Grid;
    class Pattern {
        width;
        height;
        pattern;
        vectorData;
        minX;
        minY;
        effectiveWidth;
        effectiveHeight;
        constructor(width, height, pattern) {
            this.width = width;
            this.height = height;
            this.pattern = pattern;
            let minX = width, minY = height, maxX = 0, maxY = 0;
            const v = [];
            for (let y = 0; y < height; ++y) {
                for (let x = 0; x < width; ++x) {
                    const c = pattern[x + width * y];
                    if (c >= 128) {
                        continue;
                    }
                    v.push(x, y, c);
                    minX = Math.min(minX, x);
                    minY = Math.min(minY, y);
                    maxX = Math.max(maxX, x + 1);
                    maxY = Math.max(maxY, y + 1);
                }
            }
            this.vectorData = v;
            this.minX = minX;
            this.minY = minY;
            this.effectiveWidth = Math.max(maxX - minX, 0);
            this.effectiveHeight = Math.max(maxY - minY, 0);
        }
        fitsMask(grid, mask, atX, atY) {
            const v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                if ((mask[index >> 5] & 1 << (index & 31)) !== 0) {
                    return false;
                }
            }
            return true;
        }
        hasEffect(grid, atX, atY) {
            const g = grid.data, v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                if (g[index] !== v[i + 2]) {
                    return true;
                }
            }
            return false;
        }
        put(grid, mask, atX, atY) {
            const g = grid.data, v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                g[index] = v[i + 2];
                if (mask !== undefined) {
                    mask[index >> 5] |= 1 << (index & 31);
                }
            }
        }
    }
    MJr.Pattern = Pattern;
    class RewriteInfo {
        grid;
        x;
        y;
        width;
        height;
        constructor(grid, x, y, width, height) {
            this.grid = grid;
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
        }
    }
    MJr.RewriteInfo = RewriteInfo;
    class Sampler {
        arr;
        indices;
        count = 0;
        constructor(domainSize) {
            const arr = new Uint32Array(domainSize);
            const indices = new Uint32Array(domainSize);
            for (let i = 0; i < domainSize; ++i) {
                arr[i] = indices[i] = i;
            }
            this.arr = arr;
            this.indices = indices;
        }
        copyInto(out) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                out[i] = arr[i];
            }
        }
        copyIntoOffset(out, offset, m, c) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                out[offset + i] = m * arr[i] + c;
            }
        }
        shuffleInto(out, rng) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                const j = rng.nextInt(i + 1);
                out[i] = out[j];
                out[j] = arr[i];
            }
        }
        shuffleIntoOffset(out, offset, m, c, rng) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                const j = rng.nextInt(offset + i + 1);
                out[offset + i] = out[j];
                out[j] = m * arr[i] + c;
            }
        }
        has(x) {
            return this.indices[x] < this.count;
        }
        add(x) {
            const { arr, indices, count } = this;
            const i = indices[x];
            if (i >= count) {
                const j = count;
                const y = arr[j];
                arr[j] = x;
                indices[x] = j;
                arr[i] = y;
                indices[y] = i;
                ++this.count;
            }
        }
        del(x) {
            const { arr, indices, count } = this;
            const i = indices[x];
            if (i < count) {
                const j = count - 1;
                const y = arr[j];
                arr[j] = x;
                indices[x] = j;
                arr[i] = y;
                indices[y] = i;
                --this.count;
            }
        }
        sample(max, rng) {
            const { arr, indices } = this;
            const i = rng.nextInt(max);
            const j = max - 1;
            const x = arr[i];
            const y = arr[j];
            arr[i] = y;
            indices[y] = i;
            arr[j] = x;
            indices[x] = j;
            return x;
        }
    }
    MJr.Sampler = Sampler;
})(MJr || (MJr = {}));
///<reference path="../runtime/mjr.ts"/>
var Op;
(function (Op) {
    Op.UNARY_FUNCS = {
        bool_not: x => !x,
        float_uminus: x => -x,
        fraction_uminus: x => ({ p: -x.p, q: x.q }),
        int_uminus: x => (-x) | 0,
        int_to_float: x => +x,
        int_to_fraction: MJr.OPS.int_to_fraction,
        bool_to_str: x => x.toString(),
        float_to_str: x => x.toString(),
        fraction_to_str: MJr.OPS.fraction_to_str,
        int_to_str: x => x.toString(),
    };
    Op.BINARY_FUNCS = ((funcs) => funcs)({
        bool_and: (x, y) => x && y,
        bool_or: (x, y) => x || y,
        bool_eq: (x, y) => x === y,
        bool_ne: (x, y) => x !== y,
        float_plus: (x, y) => x + y,
        float_minus: (x, y) => x - y,
        float_mult: (x, y) => x * y,
        float_truediv: (x, y) => x / y,
        float_mod: MJr.OPS.float_mod,
        float_eq: (x, y) => x === y,
        float_ne: (x, y) => x !== y,
        float_lt: (x, y) => x < y,
        float_le: (x, y) => x <= y,
        float_gt: (x, y) => x > y,
        float_ge: (x, y) => x >= y,
        fraction_plus: MJr.OPS.fraction_plus,
        fraction_minus: MJr.OPS.fraction_minus,
        fraction_mult: MJr.OPS.fraction_mult,
        fraction_truediv: MJr.OPS.fraction_truediv,
        fraction_eq: MJr.OPS.fraction_eq,
        fraction_ne: MJr.OPS.fraction_ne,
        fraction_lt: MJr.OPS.fraction_lt,
        fraction_le: MJr.OPS.fraction_le,
        fraction_gt: MJr.OPS.fraction_gt,
        fraction_ge: MJr.OPS.fraction_ge,
        int_plus: (x, y) => (x + y) | 0,
        int_minus: (x, y) => (x - y) | 0,
        int_mult: Math.imul,
        int_truediv: MJr.OPS.int_truediv,
        int_floordiv: MJr.OPS.int_floordiv,
        int_mod: MJr.OPS.int_mod,
        int_eq: (x, y) => x === y,
        int_ne: (x, y) => x !== y,
        int_lt: (x, y) => x < y,
        int_le: (x, y) => x <= y,
        int_gt: (x, y) => x > y,
        int_ge: (x, y) => x >= y,
        str_concat: (x, y) => x + y,
        str_eq: (x, y) => x === y,
        str_ne: (x, y) => x !== y,
    });
    Op.UNARY_OP_TYPES = {
        '-': [['int', 'int', 'int_uminus'], ['float', 'float', 'float_uminus']],
        'not': [['bool', 'bool', 'bool_not']],
    };
    Op.BINARY_OP_TYPES = {
        '+': [['int', 'int', 'int', 'int_plus'], ['float', 'float', 'float', 'float_plus'], ['fraction', 'fraction', 'fraction', 'fraction_plus'], ['str', 'str', 'str', 'str_concat']],
        '-': [['int', 'int', 'int', 'int_minus'], ['float', 'float', 'float', 'float_minus'], ['fraction', 'fraction', 'fraction', 'fraction_minus']],
        '*': [['int', 'int', 'int', 'int_mult'], ['float', 'float', 'float', 'float_mult'], ['fraction', 'fraction', 'fraction', 'fraction_mult']],
        '/': [['int', 'int', 'fraction', 'int_truediv'], ['float', 'float', 'float', 'float_truediv'], ['fraction', 'fraction', 'fraction', 'fraction_truediv']],
        '//': [['int', 'int', 'int', 'int_floordiv']],
        '%': [['int', 'int', 'int', 'int_mod'], ['float', 'float', 'float', 'float_mod']],
        '==': [['int', 'int', 'bool', 'int_eq'], ['float', 'float', 'bool', 'float_eq'], ['fraction', 'fraction', 'bool', 'fraction_eq'], ['str', 'str', 'bool', 'str_eq']],
        '!=': [['int', 'int', 'bool', 'int_ne'], ['float', 'float', 'bool', 'float_ne'], ['fraction', 'fraction', 'bool', 'fraction_ne'], ['str', 'str', 'bool', 'str_ne']],
        '<': [['int', 'int', 'bool', 'int_lt'], ['float', 'float', 'bool', 'float_lt'], ['fraction', 'fraction', 'bool', 'fraction_lt']],
        '<=': [['int', 'int', 'bool', 'int_le'], ['float', 'float', 'bool', 'float_le'], ['fraction', 'fraction', 'bool', 'fraction_le']],
        '>': [['int', 'int', 'bool', 'int_gt'], ['float', 'float', 'bool', 'float_gt'], ['fraction', 'fraction', 'bool', 'fraction_gt']],
        '>=': [['int', 'int', 'bool', 'int_ge'], ['float', 'float', 'bool', 'float_ge'], ['fraction', 'fraction', 'bool', 'fraction_ge']],
        'and': [['bool', 'bool', 'bool', 'bool_and']],
        'or': [['bool', 'bool', 'bool', 'bool_or']],
    };
})(Op || (Op = {}));
var Parser;
(function (Parser_1) {
    /**
     * Maximum size for a grid alphabet; this is set at 128 so that alphabet
     * symbols can be represented by signed byte values.
     */
    const MAX_ALPHABET_SIZE = 128;
    /**
     * Precedence table for binary operators.
     */
    Parser_1.BINARY_OPS = {
        'or': 2 /* Precedence.OR */,
        'and': 3 /* Precedence.AND */,
        '<': 5 /* Precedence.CMP */,
        '<=': 5 /* Precedence.CMP */,
        '>': 5 /* Precedence.CMP */,
        '>=': 5 /* Precedence.CMP */,
        '==': 5 /* Precedence.CMP */,
        '!=': 5 /* Precedence.CMP */,
        '+': 6 /* Precedence.PLUS_MINUS */,
        '-': 6 /* Precedence.PLUS_MINUS */,
        '*': 7 /* Precedence.MULT_DIV_MOD */,
        '/': 7 /* Precedence.MULT_DIV_MOD */,
        '//': 7 /* Precedence.MULT_DIV_MOD */,
        '%': 7 /* Precedence.MULT_DIV_MOD */,
    };
    /**
     * Precedence table for unary operators.
     */
    Parser_1.UNARY_OPS = {
        'not': 4 /* Precedence.NOT */,
        '+': 8 /* Precedence.UPLUS_UMINUS */,
        '-': 8 /* Precedence.UPLUS_UMINUS */,
        'count': 9 /* Precedence.FUNC */,
        'load': 9 /* Precedence.FUNC */,
        'randint': 9 /* Precedence.FUNC */,
        'sum': 9 /* Precedence.FUNC */,
    };
    const UNESCAPE_STRING_CHAR = {
        '\\n': '\n',
        '\\t': '\t',
    };
    /**
     * Arguments for each node type; `true` means a required argument.
     */
    const ARGS = ((specs) => specs)({
        all: { temperature: false, search: false, maxStates: false, depthCoefficient: false },
        convchain: { sample: true, n: true, on: false, temperature: false, anneal: false, epsilon: false, periodic: false },
        convolution: { kernel: true, boundary: false },
        field: { for_: true, on: true, from: false, to: false, essential: false, recompute: false },
        grid: { scaleX: false, scaleY: false, periodic: false },
        map: { outGrid: true },
        one: { temperature: false, search: false, maxStates: false, depthCoefficient: false },
        path: { from: true, to: true, input: true, output: true, longest: false, inertia: false },
    });
    /**
     * Lookup table for which nodes have each argument; used to generate hints
     * in error messages.
     */
    const ARGS_TO_NODES = {};
    for (const [k, args] of Object.entries(ARGS)) {
        for (const j of Object.keys(args)) {
            (ARGS_TO_NODES[j] ??= []).push(k);
        }
    }
    function quoteNode(tok) {
        return tok.kind === 'KEYWORD' ? `keyword '${tok.s}'`
            : tok.kind === 'OP' || tok.kind === 'PUNCTUATION' ? `'${tok.s}'`
                : `'${tok.kind}'`;
    }
    class Parser {
        diagnostics = new Diagnostics();
        q;
        constructor(src) {
            const tokens = Tokenizer.tokenize(src, true);
            this.q = new Tokenizer.TokenQueue(tokens);
        }
        // error reporting and checking
        errorUnexpected(part) {
            const tok = this.q.poll();
            this.diagnostics.syntaxError(`unexpected ${quoteNode(tok)}, expected ${part}`, tok.pos);
        }
        errorExpected(was, ...hints) {
            this.diagnostics.syntaxError(`expected ${quoteJoin(hints, ' | ')}, was ${quoteNode(was)}`, was.pos);
        }
        errorOperatorPrecedence(tok) {
            this.diagnostics.syntaxError(`${quoteNode(tok)} not allowed here due to operator precedence`, tok.pos);
        }
        expectPoll(kind) {
            const tok = this.q.poll();
            if (tok.kind === kind) {
                return tok;
            }
            else {
                this.errorExpected(tok, kind);
                return undefined;
            }
        }
        expectPollS(...strings) {
            const tok = this.q.poll();
            if (strings.includes(tok.s)) {
                return tok;
            }
            else {
                this.errorExpected(tok, ...strings);
                return undefined;
            }
        }
        expectPollIf(kind) {
            return this.q.pollIf(kind)
                || (this.errorExpected(this.q.peek(), kind), false);
        }
        expectPollIfS(s) {
            if (!this.q.pollIfS(s)) {
                this.errorExpected(this.q.peek(), s);
            }
        }
        assertPoll(kind) {
            return this.q.hasNext(kind) ? this.q.poll() : fail();
        }
        assertPollS(...strings) {
            return this.q.hasNextS(...strings) ? this.q.poll() : fail();
        }
        // entry points
        /**
         * ```none
         * CompilationUnit = Statement* EOF
         * ```
         */
        root = () => {
            const stmts = this.parseUntil('stmt');
            this.assertPoll('EOF');
            return { kind: 'root', stmts, pos: { line: 1, col: 0 } };
        };
        /**
         * ```none
         * Declaration = LegendDecl | LetDecl | SymmetryDecl | UnionDecl
         * ```
         */
        decl = () => {
            const tok = this.q.peek();
            if (tok.kind === 'KEYWORD') {
                switch (tok.s) {
                    case 'legend':
                        return this.parseLegendDecl();
                    case 'let':
                        return this.parseLetDecl();
                    case 'symmetry':
                        return this.parseSymmetryDecl();
                    case 'union':
                        return this.parseUnionDecl();
                }
            }
            this.errorUnexpected('declaration');
            this.q.skipLine();
            return undefined;
        };
        /**
         * ```none
         * Rule = FieldRule | ObserveRule | RewriteRule | WithDeclaration<Rule>
         * ```
         */
        rule = () => {
            const tok = this.q.peek();
            if (tok.kind === 'KEYWORD') {
                switch (tok.s) {
                    case 'legend':
                    case 'let':
                    case 'symmetry':
                    case 'union':
                        const declaration = this.decl();
                        return declaration && this.parseDeclChildren('rule', declaration);
                    case 'field':
                        return this.parseFieldRule();
                    case 'observe':
                        return this.parseObserveRule();
                }
            }
            const rule = this.parseRewriteRule();
            if (rule !== undefined) {
                return rule;
            }
            this.errorUnexpected('rule');
            this.q.skipLine();
            return undefined;
        };
        /**
         * ```none
         * Statement = BaseUseStmt | LogStmt | ModifiableStmt | PassStmt | UseStmt | WithDeclaration<Statement>
         * ```
         */
        stmt = () => {
            const tok = this.q.peek();
            if (tok.kind === 'KEYWORD') {
                switch (tok.s) {
                    case 'legend':
                    case 'let':
                    case 'symmetry':
                    case 'union':
                        const declaration = this.decl();
                        return declaration && this.parseDeclChildren('stmt', declaration);
                    case 'grid':
                        return this.parseBareUseStmt();
                    case 'pass':
                        return this.parsePassStmt();
                    case 'log':
                        return this.parseLogStmt();
                    case 'use':
                        return this.parseUseStmt();
                }
            }
            return this.parseModifiableStmt();
        };
        /**
         * ```none
         * ModifiableStmt = BlockStmt | ConvChainStmt | ModifiedStmt | PathStmt | PutStmt | RuleBlockStmt
         * ```
         */
        parseModifiableStmt() {
            const tok = this.q.peek();
            if (tok.kind === 'KEYWORD') {
                switch (tok.s) {
                    case 'all':
                    case 'convolution':
                    case 'map':
                    case 'once':
                    case 'one':
                    case 'prl':
                        return this.parseRuleBlockStmt();
                    case 'markov':
                    case 'sequence':
                        return this.parseBlockStmt();
                    case 'convchain':
                    case 'path':
                        return this.parseLineArgsStmt();
                    case 'put':
                        return this.parsePutStmt();
                }
            }
            else if (tok.s === '@') {
                return this.parseModifiedStmt();
            }
            this.errorUnexpected('statement');
            this.q.skipLine();
            return undefined;
        }
        /**
         * ```none
         * Expression = TernaryExpr
         * ```
         */
        expr = (minPrecedence = 1 /* Precedence.IF_ELSE */) => {
            const unaryExpr = this.parseUnaryOpExpr(minPrecedence);
            const binaryExpr = this.parseBinaryOpExpr(unaryExpr, minPrecedence);
            return this.parseTernaryExpr(binaryExpr, minPrecedence);
        };
        /**
         * ```none
         * TernaryExpr = BinaryOpExpr ('if' Expression 'else' Expression)?
         * ```
         */
        parseTernaryExpr(expr, minPrecedence) {
            if (expr === undefined || minPrecedence > 1 /* Precedence.IF_ELSE */ || !this.q.pollIfS('if')) {
                return expr;
            }
            let condition, otherwise;
            return (condition = this.expr(2 /* Precedence.OR */))
                && this.expectPollS('else')
                && (otherwise = this.expr(1 /* Precedence.IF_ELSE */))
                && { kind: 'expr.op.ternary', condition, then: expr, otherwise, pos: expr.pos };
        }
        hasNextBinaryOp(minPrecedence) {
            const op = this.q.peek().s;
            return objHasKey(Parser_1.BINARY_OPS, op) && Parser_1.BINARY_OPS[op] >= minPrecedence;
        }
        /**
         * ```none
         * BinaryOpExpr = UnaryOpExpr (BinaryOp UnaryOpExpr)*
         * BinaryOp = OP | 'and' | 'or'
         * ```
         */
        parseBinaryOpExpr(left, minPrecedence) {
            // https://en.wikipedia.org/wiki/Operator-precedence_parser#Pseudocode
            let prevWasComparison = false;
            while (left !== undefined && this.hasNextBinaryOp(minPrecedence)) {
                const opTok = this.q.poll();
                const op = opTok.s;
                const opPrecedence = Parser_1.BINARY_OPS[op];
                const isComparison = opPrecedence === 5 /* Precedence.CMP */;
                // MJr syntax is similar to Python's, but `x < y < z` chains in Python and not in
                // MJr; such expressions are mistakes, so report a useful error message now rather
                // than a type error later. If the programmer wrote `x < y == z` intending to
                // compare the result of `x < y` with a boolean `z`, they should use brackets.
                if (prevWasComparison && isComparison) {
                    this.diagnostics.syntaxError(`comparison operators do not chain; use 'and' to do multiple comparisons, or brackets to disambiguate`, opTok.pos);
                }
                let right = this.parseUnaryOpExpr(opPrecedence);
                // none of the binary ops are right-associative
                while (right !== undefined && this.hasNextBinaryOp(opPrecedence + 1)) {
                    right = this.parseBinaryOpExpr(right, opPrecedence + 1);
                }
                left = right && { kind: 'expr.op.binary', op, left, right, pos: left.pos };
                prevWasComparison = isComparison;
            }
            return left;
        }
        /**
         * ```none
         * UnaryOpExpr = DeclarationExpr | UnaryOp Expression | PrimaryExpr
         * UnaryOp = '+' | '-' | 'count' | 'load' | 'not' | 'randint' | 'sum'
         * ```
         */
        parseUnaryOpExpr(minPrecedence) {
            if (this.q.hasNextS('legend', 'let', 'symmetry', 'union')) {
                if (minPrecedence > 0 /* Precedence.DECLARATION */) {
                    this.errorOperatorPrecedence(this.q.peek());
                }
                return this.parseDeclarationExpr();
            }
            const { s: op, pos } = this.q.peek();
            if (!objHasKey(Parser_1.UNARY_OPS, op)) {
                return this.parsePrimaryExpression();
            }
            const tok = this.q.poll();
            const opPrecedence = Parser_1.UNARY_OPS[op];
            if (opPrecedence < minPrecedence) {
                this.errorOperatorPrecedence(tok);
            }
            const child = this.expr(opPrecedence);
            return child && { kind: 'expr.op.unary', op, child, pos };
        }
        /**
         * ```none
         * PrimaryExpr = DictExpr | GridExpr | LiteralExpr | NameExpr | '(' Expression ')'
         * LiteralExpr = BoolLiteralExpr | FloatLiteralExpr | IntLiteralExpr | PatternLiteralExpr | StrLiteralExpr
         * ```
         */
        parsePrimaryExpression() {
            const tok = this.q.peek();
            switch (tok.kind) {
                case 'KEYWORD':
                    switch (tok.s) {
                        case 'at':
                        case 'origin':
                        case 'random':
                            return this.parseNameExpr();
                        case 'false':
                        case 'true':
                            return this.parseBoolLiteralExpr();
                        case 'grid':
                            return this.parseGridExpr();
                    }
                    break;
                case 'FLOAT':
                    return this.parseFloatLiteralExpr();
                case 'INT':
                    return this.parseIntLiteralExpr();
                case 'NAME':
                    return this.parseNameExpr();
                case 'QUOTE':
                    return this.parseStrLiteralExpr();
                case 'PUNCTUATION':
                    switch (tok.s) {
                        case '(':
                            return this.parseBracketedExpr();
                        case '[':
                            return this.parsePatternLiteralExpr();
                        case '{':
                            return this.parseDictExpr();
                    }
                    break;
            }
            this.errorUnexpected('expression');
            return undefined;
        }
        // helpers
        /**
         * Parses `T*`. `stopBefore` must only contain tokens which `T` cannot begin with.
         */
        parseUntil(part, ...stopBefore) {
            const parseChild = this[part];
            const children = [];
            while (!this.q.hasNext('EOF', ...stopBefore)) {
                const child = parseChild();
                if (child !== undefined) {
                    children.push(child);
                }
            }
            return children;
        }
        /**
         * ```none
         * BlockChildren<T> = ':' (T | NEWLINE INDENT T+ DEDENT)
         * ```
         */
        parseBlockChildren(kind) {
            this.expectPollIfS(':');
            if (this.q.pollIf('NEWLINE')) {
                if (!this.expectPollIf('INDENT')) {
                    return [];
                }
                const children = this.parseUntil(kind, 'DEDENT');
                this.assertPoll('DEDENT');
                return children;
            }
            else {
                const child = this[kind]();
                return child !== undefined ? [child] : [];
            }
        }
        /**
         * ```none
         * WithDeclaration<T> = Declaration (NEWLINE T* | 'in' BlockChildren<T>)
         * ```
         */
        parseDeclChildren(kind, declaration) {
            const children = this.q.pollIf('NEWLINE')
                ? this.parseUntil(kind, 'DEDENT')
                : (this.expectPollIfS('in'), this.parseBlockChildren(kind));
            return { kind: `${kind}.decl`, declaration, children, pos: declaration.pos };
        }
        /**
         * ```none
         * Args = NameValuePairs?
         * ```
         */
        parseArgs(kind) {
            const pairs = this.q.hasNextS('{') ? this.parseNameValuePairs() : [];
            if (pairs === undefined) {
                return undefined;
            }
            const spec = ARGS[kind];
            const args = {};
            for (const [name, expr] of pairs) {
                // sanitise JS keyword
                const argName = name.name === 'for' ? 'for_' : name.name;
                args[argName] = expr;
                if (!objHasKey(spec, argName)) {
                    const hints = ARGS_TO_NODES[argName];
                    const msg = hints !== undefined ? `argument '${name.name}' only valid for ${quoteJoin(hints)}` : `invalid argument '${name.name}'`;
                    this.diagnostics.syntaxError(msg, name.pos);
                }
            }
            const missing = Object.keys(spec).filter(k => spec[k] && args[k] === undefined);
            if (missing.length > 0) {
                this.diagnostics.syntaxError(`missing required argument${missing.length > 1 ? 's' : ''} ${quoteJoin(missing)}`, this.q.peek().pos);
                return undefined;
            }
            return args;
        }
        /**
         * ```none
         * NameValuePairs = '{' (NameValuePair ',')* NameValuePair? '}'
         * NameValuePair = SimpleNameExpr '=' Expression
         * ```
         */
        parseNameValuePairs() {
            const pairs = [];
            const names = new Set();
            if (this.q.pollIfS('{')) {
                while (true) {
                    const name = this.parseSimpleNameExpr();
                    if (name === undefined) {
                        return undefined;
                    }
                    if (names.has(name.name)) {
                        this.diagnostics.syntaxError(`duplicate name '${name.name}'`, name.pos);
                    }
                    const arg = this.expectPollS('=') && this.expr();
                    if (arg === undefined) {
                        return undefined;
                    }
                    ;
                    pairs.push([name, arg]);
                    names.add(name.name);
                    if (this.q.pollIfS(',')) {
                        if (this.q.pollIfS('}')) {
                            break;
                        }
                    }
                    else if (this.expectPollS('}')) {
                        break;
                    }
                    else {
                        return undefined;
                    }
                }
            }
            return pairs;
        }
        // declarations
        /**
         * ```none
         * LegendDecl = 'legend' PatternLiteralExpr
         * SymmetryDecl = 'symmetry' StrLiteralExpr
         * ```
         */
        parseLegendDecl() {
            const { pos } = this.assertPollS('legend', 'symmetry');
            const expr = this.parsePatternLiteralExpr();
            return expr && { kind: 'decl.legend', expr, pos };
        }
        /**
         * ```none
         * LetDecl = 'let' 'param'? SimpleNameExpr '=' Expression
         * ```
         */
        parseLetDecl() {
            const { pos } = this.assertPollS('let');
            const isParam = this.q.pollIfS('param');
            /*
            const names: AST.SimpleNameExpr[] = [];
            while(true) {
                const nameExpr = this.parseSimpleNameExpr();
                if(nameExpr === undefined) { return undefined; }
                names.push(nameExpr);
                
                if(this.q.pollIfS('=')) { break; }
                if(this.expectPollS('|', '=') === undefined) { return undefined; }
            }
           
            if(isParam && names.length !== 1) {
                this.diagnostics.syntaxError(`'let param' must only declare one name`, pos);
            }
            */
            let name, rhs;
            return (name = this.parseSimpleNameExpr())
                && this.expectPollS('=')
                && (rhs = this.expr())
                && { kind: 'decl.let', name, rhs, pos, isParam };
        }
        /**
         * ```none
         * LegendDecl = 'legend' PatternLiteralExpr
         * SymmetryDecl = 'symmetry' StrLiteralExpr
         * ```
         */
        parseSymmetryDecl() {
            const { pos } = this.assertPollS('symmetry');
            const expr = this.parseStrLiteralExpr();
            return expr && { kind: 'decl.symmetry', expr, pos };
        }
        /**
         * ```none
         * UnionDecl = 'union' PatternLiteralExpr '=' Expression
         * ```
         */
        parseUnionDecl() {
            const { pos } = this.assertPollS('union');
            let label, chars;
            return (label = this.parsePatternLiteralExpr())
                && this.expectPollS('=')
                && (chars = this.expr())
                && { kind: 'decl.union', label, chars, pos };
        }
        // rules
        /**
         * ```none
         * FieldRule = 'field' Args NEWLINE
         * ```
         */
        parseFieldRule() {
            const { pos } = this.assertPollS('field');
            const args = this.parseArgs('field');
            return args
                && this.expectPoll('NEWLINE')
                && ({ kind: 'rule.field', ...args, pos });
        }
        /**
         * ObserveRule = 'observe' Expression ('->' Expression)? '->' Expression ('if' Expression)? NEWLINE
         */
        parseObserveRule() {
            const { pos } = this.assertPollS('observe');
            const from = this.expr(2 /* Precedence.OR */);
            if (!from) {
                return undefined;
            }
            this.expectPollS('->');
            let via = this.expr(2 /* Precedence.OR */);
            if (via === undefined) {
                return undefined;
            }
            let to;
            if (this.q.pollIfS('->')) {
                to = this.expr(2 /* Precedence.OR */);
                if (to === undefined) {
                    return undefined;
                }
            }
            else {
                to = via;
                via = undefined;
            }
            let condition;
            return (this.q.pollIfS('if') ? condition = this.expr(2 /* Precedence.OR */) : true)
                && this.expectPoll('NEWLINE')
                && { kind: 'rule.observe', from, via, to, condition, pos };
        }
        /**
         * ```none
         * RewriteRule = Expression '->' Expression ('if' Expression)? NEWLINE
         * ```
         */
        parseRewriteRule() {
            let from, to, condition = undefined;
            return (from = this.expr(2 /* Precedence.OR */))
                && this.expectPollS('->')
                && (to = this.expr(2 /* Precedence.OR */))
                && (this.q.pollIfS('if') ? condition = this.expr(2 /* Precedence.OR */) : true)
                && this.expectPoll('NEWLINE')
                && { kind: 'rule.rewrite', from, to, condition, pos: from.pos };
        }
        // statements
        /**
         * ```none
         * ModifiedStmt = '@' 'limit' Expression NEWLINE ModifiableStmt
         * ```
         */
        parseModifiedStmt() {
            const { pos } = this.assertPollS('@');
            let name, arg, child;
            return (name = this.expectPollS('limit'))
                && (arg = this.expr())
                && this.expectPoll('NEWLINE')
                && (child = this.parseModifiableStmt())
                && { kind: `stmt.modified.${name.s}`, arg, child, pos };
        }
        /**
         * ```none
         * BlockStmt = MarkovStmt | SequenceStmt
         * MarkovStmt = 'markov' BlockChildren<Statement>
         * SequenceStmt = 'sequence' BlockChildren<Statement>
         * ```
         */
        parseBlockStmt() {
            const { s: kind, pos } = this.assertPollS('markov', 'sequence');
            const children = this.parseBlockChildren('stmt');
            return { kind: `stmt.block.${kind}`, children, pos };
        }
        /**
         * ```none
         * RuleBlockStmt = AllStmt | ConvolutionStmt | MapStmt | OnceStmt | OneStmt | PrlStmt
         * AllStmt = 'all' Args BlockChildren<Rule>
         * ConvolutionStmt = 'convolution' Args BlockChildren<Rule>
         * MapStmt = 'map' Args BlockChildren<Rule>
         * OnceStmt = 'once' BlockChildren<Rule>
         * OneStmt = 'one' Args BlockChildren<Rule>
         * PrlStmt = 'prl' BlockChildren<Rule>
         * ```
         */
        parseRuleBlockStmt() {
            const { s: kind, pos } = this.assertPollS('all', 'convolution', 'map', 'once', 'one', 'prl');
            const args = kind === 'prl' || kind === 'once' ? {} : this.parseArgs(kind);
            const rules = this.parseBlockChildren('rule');
            return args && { kind: `stmt.rules.${kind}`, ...args, rules, pos };
        }
        /**
         * ```none
         * PassStmt = 'pass' NEWLINE
         * ```
         */
        parsePassStmt() {
            const { pos } = this.assertPollS('pass');
            return { kind: 'stmt.pass', pos };
        }
        /**
         * ```none
         * PutStmt = 'put' Expression 'at' Expression ('if' Expression)? NEWLINE
         * ```
         */
        parsePutStmt() {
            const { pos } = this.assertPollS('put');
            let pattern, at, condition;
            return (pattern = this.expr(2 /* Precedence.OR */))
                && this.expectPollS('at')
                && (at = this.expr(2 /* Precedence.OR */))
                && (this.q.pollIfS('if') ? condition = this.expr(2 /* Precedence.OR */) : true)
                && this.expectPoll('NEWLINE')
                && { kind: 'stmt.put', pattern, at, condition, pos };
        }
        /**
         * ```none
         * ConvChainStmt = 'convchain' Args NEWLINE
         * PathStmt = 'path' Args NEWLINE
         * ```
         */
        parseLineArgsStmt() {
            const { s: kind, pos } = this.assertPollS('convchain', 'path');
            const args = this.parseArgs(kind);
            return args
                && this.expectPoll('NEWLINE')
                && { kind: `stmt.${kind}`, ...args, pos };
        }
        /**
         * ```none
         * BareUseStmt = GridExpr NEWLINE
         * ```
         */
        parseBareUseStmt() {
            const expr = this.parseGridExpr();
            return expr
                && this.expectPoll('NEWLINE')
                && { kind: 'stmt.use.expr', expr, pos: expr.pos };
        }
        /**
         * LogStmt = 'log' Expression NEWLINE
         */
        parseLogStmt() {
            const { pos, s: kind } = this.assertPollS('log');
            let expr;
            return (expr = this.expr())
                && this.expectPoll('NEWLINE')
                && { kind: `stmt.${kind}`, expr, pos };
        }
        /**
         * UseStmt = UseExprStmt | UseLetStmt
         * UseExprStmt = 'use' Expression NEWLINE
         * UseLetStmt = 'use' LetDecl NEWLINE Statement*
         */
        parseUseStmt() {
            const { pos } = this.assertPollS('use');
            if (this.q.hasNextS('let')) {
                let decl;
                return (decl = this.parseLetDecl())
                    && this.expectPoll('NEWLINE')
                    && { kind: 'stmt.use.let', decl, children: this.parseUntil('stmt', 'DEDENT'), pos };
            }
            else {
                let expr;
                return (expr = this.expr())
                    && this.expectPoll('NEWLINE')
                    && { kind: 'stmt.use.expr', expr, pos };
            }
        }
        // expressions
        parseBracketedExpr() {
            this.assertPollS('(');
            const expr = this.expr(0 /* Precedence.DECLARATION */);
            this.expectPollS(')');
            return expr;
        }
        /**
         * ```none
         * DeclarationExpr = Declaration 'in' Expression
         * ```
         */
        parseDeclarationExpr() {
            let declaration, child;
            return (declaration = this.decl())
                && (this.expectPollIfS('in'), child = this.expr(0 /* Precedence.DECLARATION */))
                && { kind: 'expr.decl', declaration, child, pos: declaration.pos };
        }
        /**
         * ```none
         * DictExpr = NameValuePairs
         * ```
         */
        parseDictExpr() {
            const { pos } = this.q.peek();
            const pairs = this.parseNameValuePairs();
            return pairs && { kind: 'expr.dict', pairs, pos };
        }
        /**
         * ```none
         * BoolLiteralExpr = 'false' | 'true'
         * ```
         */
        parseBoolLiteralExpr() {
            const tok = this.expectPollS('false', 'true');
            return tok && { kind: 'expr.literal.bool', value: tok.s === 'true', pos: tok.pos };
        }
        /**
         * ```none
         * FloatLiteralExpr = FLOAT
         * ```
         */
        parseFloatLiteralExpr() {
            const tok = this.expectPoll('FLOAT');
            return tok && { kind: 'expr.literal.float', value: parseFloat(tok.s), pos: tok.pos };
        }
        /**
         * ```none
         * IntLiteralExpr = INT
         * ```
         */
        parseIntLiteralExpr() {
            const tok = this.expectPoll('INT');
            if (tok === undefined) {
                return undefined;
            }
            const value = parseInt(tok.s) | 0;
            if (`${value}` !== tok.s) {
                this.diagnostics.syntaxError(`int literal '${tok.s}' out of range`, tok.pos);
            }
            return { kind: 'expr.literal.int', value, pos: tok.pos };
        }
        /**
         * ```none
         * PatternLiteralExpr = '[' (Char | CharSet)+ ('/' (Char | CharSet)+)* ']'
         * Char = PATTERN_CHAR
         * ```
         */
        parsePatternLiteralExpr() {
            const beginTok = this.expectPollS('[');
            if (beginTok === undefined) {
                return undefined;
            }
            const { pos } = beginTok;
            let row = [];
            const rows = [row];
            while (!this.q.pollIfS(']')) {
                const tok = this.q.poll();
                switch (tok.s) {
                    case '[':
                        row.push(this.parseCharSet(tok));
                        break;
                    case '/':
                        rows.push(row = []);
                        break;
                    default:
                        row.push(tok);
                        break;
                }
            }
            const width = row.length, height = rows.length;
            if (rows.some(row => row.length !== width)) {
                this.diagnostics.syntaxError('pattern must be rectangular', pos);
            }
            else if (width === 0) {
                this.diagnostics.syntaxError('empty pattern', pos);
            }
            return { kind: 'expr.literal.pattern', width, height, value: rows.flat(), pos };
        }
        /**
         * ```none
         * CharSet = '[' Char+ ']'
         * ```
         */
        parseCharSet(beginTok) {
            const { pos } = beginTok;
            const inverted = this.q.pollIfS('^');
            const chars = [];
            while (!this.q.pollIfS(']')) {
                const tok = this.expectPoll('PATTERN_CHAR');
                if (tok !== undefined) {
                    chars.push(tok);
                }
            }
            if (chars.length === 0 && !inverted) {
                this.diagnostics.syntaxError('empty charset', pos);
            }
            return { kind: 'CHARSET', chars, inverted, pos };
        }
        /**
         * ```none
         * StrLiteralExpr = STRING
         * ```
         */
        parseStrLiteralExpr() {
            const { pos } = this.assertPoll('QUOTE');
            const s = [];
            while (!this.q.pollIf('QUOTE')) {
                const tok = this.q.poll();
                if (tok.kind === 'ESCAPED_CHAR') {
                    s.push(UNESCAPE_STRING_CHAR[tok.s] ?? tok.s.substring(1));
                }
                else {
                    s.push(tok.s);
                }
            }
            return { kind: 'expr.literal.str', value: s.join(''), pos };
        }
        /**
         * ```none
         * NameExpr = AttributeExpr | KeywordNameExpr | SimpleNameExpr
         * AttributeExpr = NameExpr '.' NAME
         * KeywordNameExpr = 'at' | 'origin' | 'random'
         * ```
         */
        parseNameExpr() {
            let expr;
            if (this.q.hasNext('NAME')) {
                expr = this.parseSimpleNameExpr();
            }
            else {
                const { s: name, pos } = this.assertPollS('at', 'origin', 'random');
                expr = { kind: 'expr.name.keyword', name, pos };
            }
            while (expr !== undefined && this.q.pollIfS('.')) {
                const attr = this.expectPoll('NAME');
                expr = attr && { kind: 'expr.attr', left: expr, attr: attr.s, pos: expr.pos };
            }
            return expr;
        }
        /**
         * ```none
         * SimpleNameExpr = NAME
         * ```
         */
        parseSimpleNameExpr() {
            const tok = this.expectPoll('NAME');
            return tok && { kind: 'expr.name.simple', name: tok.s, pos: tok.pos };
        }
        /**
         * ```none
         * GridExpr = 'grid' Args PatternLiteralExpr
         * ```
         */
        parseGridExpr() {
            const { pos } = this.assertPollS('grid');
            const args = this.parseArgs('grid');
            const alphabet = this.parsePatternLiteralExpr();
            if (alphabet === undefined) {
                return undefined;
            }
            if (alphabet.height > 1) {
                this.diagnostics.syntaxError(`alphabet must be a single row`, alphabet.pos);
            }
            else if (alphabet.width < 2) {
                this.diagnostics.syntaxError(`alphabet size must be at least 2`, alphabet.pos);
            }
            else if (alphabet.width > MAX_ALPHABET_SIZE) {
                this.diagnostics.syntaxError(`alphabet size cannot exceed ${MAX_ALPHABET_SIZE}`, alphabet.pos);
            }
            const alphabetKey = [];
            for (const c of alphabet.value) {
                if (c.kind === 'CHARSET') {
                    this.diagnostics.syntaxError(`alphabet cannot have unions`, c.pos);
                }
                else if (c.s === '.') {
                    this.diagnostics.syntaxError(`'.' cannot be an alphabet symbol`, c.pos);
                }
                else if (alphabetKey.includes(c.s)) {
                    this.diagnostics.syntaxError(`repeated alphabet symbol '${c.s}'`, c.pos);
                }
                else {
                    alphabetKey.push(c.s);
                }
            }
            return { kind: 'expr.grid', alphabetKey: alphabetKey.join(''), ...args, pos };
        }
    }
    function parse(src, part = 'root') {
        const parser = new Parser(src);
        const ast = parser[part]();
        parser.diagnostics.throwIfAnyErrors();
        return ast;
    }
    Parser_1.parse = parse;
})(Parser || (Parser = {}));
///<reference path="../runtime/mjr.ts"/>
var Resolver;
(function (Resolver) {
    const FRACTION_ONE = MJr.fraction(1, 1);
    /**
     * Specifies types and other static checks for properties of AST nodes.
     * A spec of '?' indicates that the property should not be present, but
     * enables some nodes to have the same property names and therefore be
     * resolved by the same functions.
     */
    const PROP_SPECS = ((specs) => specs)({
        'expr.grid': {
            scaleX: 'const int?',
            scaleY: 'const int?',
            periodic: 'const bool?',
        },
        'rule.field': {
            for_: 'charset.in',
            on: 'charset.in',
            from: 'charset.in?',
            to: 'charset.in?',
            essential: 'const bool?',
            recompute: 'const bool?',
        },
        'stmt.convchain': {
            sample: 'const pattern.out',
            n: 'const int',
            temperature: 'float?',
            anneal: 'float?',
            on: 'const charset.in?',
            epsilon: 'const float?',
            periodic: 'const bool?',
        },
        'stmt.log': {
            expr: 'str~',
        },
        'stmt.path': {
            from: 'charset.in',
            to: 'charset.in',
            input: 'charset.in',
            output: 'charset.out',
            longest: 'bool?',
            inertia: 'bool?',
        },
        'stmt.put': {
            // the pattern and condition are rule contexts, but the position is not
            pattern: 'pattern.out',
            condition: 'bool?',
        },
        'stmt.rules.all': {
            temperature: 'float?',
            search: 'const bool?',
            maxStates: 'int?',
            depthCoefficient: 'float?',
        },
        'stmt.rules.one': {
            temperature: 'float?',
            search: 'const bool?',
            maxStates: 'int?',
            depthCoefficient: 'float?',
        },
    });
    function _convPatternKey(p) {
        return `${Convolution.Kernel.key(p.kernel)}:${ISet.key(p.chars)}`;
    }
    class Alphabet {
        key;
        map;
        charsets = new Map();
        wildcard;
        legend = undefined;
        constructor(key) {
            this.key = key;
            this.map = IDMap.of(key);
            const n = key.length;
            const { charsets } = this;
            for (let i = 0; i < n; ++i) {
                charsets.set(key[i], ISet.of(n, [i]));
            }
            charsets.set('.', this.wildcard = ISet.full(n));
        }
        withUnion(label, set, f) {
            const { charsets } = this;
            const oldSet = charsets.get(label);
            charsets.set(label, set);
            const result = f();
            if (oldSet !== undefined) {
                charsets.set(label, oldSet);
            }
            else {
                charsets.delete(label);
            }
            return result;
        }
    }
    class Context {
        diagnostics = new Diagnostics();
        globals = {
            grids: [],
            params: new Map(),
            potentials: [],
            variables: [],
        };
        uniqueVariableNames = new Set();
        symmetryName = 'all';
        variables = new Map();
        errorVariables = new Set();
        kernel = undefined;
        boundaryMask = undefined;
        grid = undefined;
        inputPattern = undefined;
        isRuleContext = false;
        rewriteScaleX = FRACTION_ONE;
        rewriteScaleY = FRACTION_ONE;
        resolveRoot(root) {
            const children = this.resolveStmts(root.stmts);
            return { kind: 'stmt.block.sequence', children, pos: root.pos };
        }
        resolveDecl(node, callback) {
            const f = DECL_RESOLVE_FUNCS[node.kind];
            return f(node, this, callback);
        }
        resolveExpr(node) {
            const f = EXPR_RESOLVE_FUNCS[node.kind];
            return f(node, this);
        }
        resolveRule(node, outGrid) {
            if (!this.expectGrid(node.pos)) {
                return undefined;
            }
            const f = RULE_RESOLVE_FUNCS[node.kind];
            return f(node, this, outGrid);
        }
        resolveStmt(node) {
            const f = STMT_RESOLVE_FUNCS[node.kind];
            return f(node, this);
        }
        resolveChar(c) {
            const grid = this.grid ?? fail();
            const set = grid.alphabet.charsets.get(c.s);
            if (set === undefined) {
                this.error(`'${c.s}' is not an alphabet symbol or union label`, c.pos);
            }
            return set;
        }
        resolveCharSet(charset) {
            const grid = this.grid ?? fail();
            const k = grid.alphabet.key.length;
            const mask = charset.inverted ? ISet.full(k) : ISet.empty(k);
            for (const c of charset.chars) {
                const cMask = this.resolveChar(c);
                if (cMask === undefined) {
                    return undefined;
                }
                else if (charset.inverted) {
                    ISet.removeAll(mask, cMask);
                }
                else {
                    ISet.addAll(mask, cMask);
                }
            }
            return mask;
        }
        resolveStmts(children) {
            return children.flatMap(c => {
                const r = this.resolveStmt(c);
                return r === undefined ? []
                    : r.kind === 'stmts' ? r.stmts
                        : r.assigns !== undefined ? [...r.assigns, r.stmt]
                            : [r.stmt];
            });
        }
        expectGrid(pos) {
            if (this.grid !== undefined) {
                return true;
            }
            else {
                this.error(this.globals.grids.length === 0 ? `no grid declared` : `no grid in use`, pos);
                return false;
            }
        }
        checkType(expected, expr) {
            if (Type.isSubtype(expr.type, expected)) {
                return true;
            }
            else {
                this.typeError((expected.kind === 'union' ? expected.options : [expected]).map(Type.toStr), expr);
                return false;
            }
        }
        error(msg, pos) {
            this.diagnostics.compilationError(msg, pos);
        }
        typeError(expected, expr) {
            this.error(`expected ${quoteJoin(expected, ' | ')}, was '${Type.toStr(expr.type)}'`, expr.pos);
        }
        makeVariable(name, type, flags, initialiser, isParam, pos) {
            if (this.variables.has(name) || this.errorVariables.has(name)) {
                this.error(`cannot redeclare name '${name}'`, pos);
            }
            else if (isParam && this.globals.params.has(name)) {
                this.error(`cannot redeclare parameter '${name}'`, pos);
            }
            const { uniqueVariableNames } = this;
            let uniqueName = name, i = 1;
            while (uniqueVariableNames.has(uniqueName)) {
                uniqueName = `${name}_${++i}`;
            }
            uniqueVariableNames.add(uniqueName);
            if (isParam) {
                this.globals.params.set(name, type);
            }
            flags |= 4 /* ASG.ExprFlags.LOCALLY_DETERMINISTIC */ | 8 /* ASG.ExprFlags.POSITION_INDEPENDENT */ | 16 /* ASG.ExprFlags.GRID_INDEPENDENT */;
            return withNextID(this.globals.variables, { name, uniqueName, type, initialiser, flags, references: 0 });
        }
        withOutGrid(outGrid, inputPatternPos, f) {
            const { grid: inGrid, inputPattern } = this;
            if (inGrid === undefined || inputPattern === undefined) {
                fail();
            }
            if (outGrid.id === inGrid.id) {
                return f();
            }
            // if scale is not positive, `makeGrid` already reported the error
            if (inGrid.scaleX <= 0 || inGrid.scaleY <= 0 || outGrid.scaleX <= 0 || outGrid.scaleY <= 0) {
                return undefined;
            }
            const scaleX = MJr.fraction(outGrid.scaleX, inGrid.scaleX);
            const scaleY = MJr.fraction(outGrid.scaleY, inGrid.scaleY);
            if ((inputPattern.width * scaleX.p) % scaleX.q !== 0 || (inputPattern.height * scaleY.p) % scaleY.q !== 0) {
                this.error(`input pattern size must be a multiple of ${scaleX.q}x${scaleY.q}, was ${inputPattern.width}x${inputPattern.height}`, inputPatternPos);
                return undefined;
            }
            this.grid = outGrid;
            this.rewriteScaleX = scaleX;
            this.rewriteScaleY = scaleY;
            const result = f();
            this.grid = inGrid;
            this.rewriteScaleX = this.rewriteScaleY = FRACTION_ONE;
            return result;
        }
        withLegend(decl, f) {
            if (!this.expectGrid(decl.pos)) {
                return undefined;
            }
            const { alphabet } = this.grid;
            const oldLegend = alphabet.legend;
            if (oldLegend !== undefined) {
                this.error(`'legend' already declared`, decl.pos);
            }
            const legend = _resolvePatternLiteralExpr(decl.expr, this);
            if (legend === undefined) {
                return f();
            }
            if (legend.height !== 1) {
                this.error(`'legend' pattern cannot have multiple rows`, decl.expr.pos);
            }
            alphabet.legend = legend;
            const result = f();
            alphabet.legend = oldLegend;
            return result;
        }
        withKernel(stmt, f) {
            if (this.kernel !== undefined) {
                fail();
            }
            const kernel = _resolveProp(stmt, 'kernel', 'const str', this);
            if (objHasKey(Convolution.KERNELS, kernel)) {
                const k = this.kernel = Convolution.KERNELS[kernel];
                const result = f(k);
                this.kernel = undefined;
                return result;
            }
            else {
                // this also handles PROP_ERROR
                this.error(`convolution kernel must be one of ${quoteJoin(Object.keys(Convolution.KERNELS))}`, stmt.kernel.pos);
                return undefined;
            }
        }
        withSymmetry(decl, f) {
            const symmetryName = _resolveProp(decl, 'expr', 'const str', this);
            if (objHasKey(Symmetry.SYMMETRY_GROUPS, symmetryName)) {
                const oldSymmetryName = this.symmetryName;
                this.symmetryName = symmetryName;
                const result = f();
                this.symmetryName = oldSymmetryName;
                return result;
            }
            else {
                // this also handles PROP_ERROR
                this.error(`symmetry group must be one of ${quoteJoin(Object.keys(Symmetry.SYMMETRY_GROUPS))}`, decl.expr.pos);
                return f();
            }
        }
        withUnion(decl, f) {
            if (!this.expectGrid(decl.pos)) {
                return undefined;
            }
            const { alphabet } = this.grid;
            if (decl.label.width !== 1 || decl.label.height !== 1) {
                this.error(`union label must be a single character`, decl.label.pos);
                return f();
            }
            const label = decl.label.value[0];
            if (label.kind === 'CHARSET') {
                this.error(`union label cannot be a charset`, label.pos);
                return f();
            }
            if (alphabet.map.has(label.s)) {
                this.error(`alphabet symbol '${label.s}' cannot be union label`, decl.label.pos);
                return f();
            }
            if (alphabet.charsets.has(label.s)) {
                this.error(`union label '${label.s}' already declared`, decl.label.pos);
            }
            const charset = _resolveProp(decl, 'chars', 'const charset.in', this);
            if (charset === PROP_ERROR) {
                return undefined;
            }
            switch (charset.kind) {
                case 'leaf':
                case 'top':
                    return alphabet.withUnion(label.s, charset.masks[0], f);
                case 'bottom':
                    this.error(`empty charset`, decl.chars.pos);
                    return undefined;
                default:
                    this.error(`union must be a constant charset`, decl.chars.pos);
                    return undefined;
            }
        }
        withVariable(variable, f) {
            const { variables } = this;
            const { name } = variable;
            const oldVariable = variables.get(name);
            variables.set(name, variable);
            const result = f();
            if (oldVariable !== undefined) {
                variables.set(name, oldVariable);
            }
            else {
                variables.delete(name);
            }
            return result;
        }
        withErrorVariable(name, f) {
            const { errorVariables } = this;
            const alreadyHad = errorVariables.has(name);
            errorVariables.add(name);
            const result = f();
            if (!alreadyHad) {
                errorVariables.delete(name);
            }
            return result;
        }
    }
    function _makeConstantValue(type, value) {
        return value !== undefined ? { kind: type.kind, type, value } : undefined;
    }
    function _makeConstantExpr(type, value, pos) {
        return { kind: 'expr.constant', type, constant: _makeConstantValue(type, value), flags: 31 /* ASG.ExprFlags.CONSTANT */, pos };
    }
    function _coerceFromInt(expr, type) {
        const op = expr.type.kind === 'int'
            ? `int_to_${type.kind}`
            : fail();
        const f = Op.UNARY_FUNCS[op] ?? fail();
        if (expr.kind === 'expr.constant') {
            const value = f(expr.constant.value);
            if (value !== undefined) {
                return _makeConstantExpr(type, value, expr.pos);
            }
        }
        return { kind: 'expr.op.unary', type, op, child: expr, flags: expr.flags, pos: expr.pos };
    }
    function _coerceToStr(expr) {
        switch (expr.type.kind) {
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int': {
                const op = `${expr.type.kind}_to_str`;
                const f = Op.UNARY_FUNCS[op];
                if (expr.kind === 'expr.constant' && f !== undefined) {
                    const value = f(expr.constant.value);
                    if (value !== undefined) {
                        return _makeConstantExpr(Type.STR, value, expr.pos);
                    }
                }
                return { kind: 'expr.op.unary', type: Type.STR, flags: expr.flags, op, child: expr, pos: expr.pos };
            }
            default:
                return expr;
        }
    }
    function _parsePropTypeSpec(typeSpec, ctx) {
        switch (typeSpec) {
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int':
            case 'str':
                return Type.PRIMITIVES[typeSpec];
            case 'str~':
                return Type.STR;
            case 'dict':
                return Type.ANY_DICT;
            case 'object':
                return Type.OBJECT;
        }
        const grid = ctx.grid ?? fail();
        const alphabetKey = grid.alphabet.key;
        switch (typeSpec) {
            case 'charset.in':
                return { kind: 'pattern.in', width: 1, height: 1, alphabetKey };
            case 'charset.out':
                return { kind: 'pattern.out', width: 1, height: 1, alphabetKey };
            case 'position':
                return { kind: 'position', inGrid: grid.id };
            case 'pattern.in':
                const { inputPattern } = ctx;
                if (inputPattern === undefined) {
                    return { kind: 'any_pattern', alphabetKey, allowUnions: true };
                }
                else {
                    const { width, height } = inputPattern;
                    return { kind: 'pattern.in', alphabetKey, width, height };
                }
            case 'pattern.out': {
                const { inputPattern, rewriteScaleX, rewriteScaleY } = ctx;
                if (inputPattern === undefined) {
                    return { kind: 'any_pattern', alphabetKey, allowUnions: false };
                }
                else {
                    const w = MJr.fraction(inputPattern.width * rewriteScaleX.p, rewriteScaleX.q);
                    const h = MJr.fraction(inputPattern.height * rewriteScaleY.p, rewriteScaleY.q);
                    // ensure integer results
                    if (w.q !== 1 || h.q !== 1) {
                        fail();
                    }
                    return { kind: 'pattern.out', alphabetKey, width: w.p, height: h.p };
                }
            }
        }
    }
    function _parsePropSpec(spec, ctx) {
        const isConst = spec.startsWith('const ');
        const typeSpec = spec.replace(/^const /, '').replace(/\?$/, '');
        const expectedType = _parsePropTypeSpec(typeSpec, ctx);
        const coerceToStr = typeSpec === 'str~';
        const isRequired = !spec.endsWith('?');
        return { isConst, expectedType, coerceToStr, isRequired };
    }
    /**
     * Sentinel value indicating that an error occurred when resolving a prop.
     * The value `undefined` cannot be used for this purpose, as it is valid
     * for an optional const prop to be `undefined`.
     */
    const PROP_ERROR = Symbol();
    function _resolveProp(node, propName, spec, ctx) {
        const ast = node[propName];
        const { isConst, expectedType, coerceToStr, isRequired } = _parsePropSpec(spec, ctx);
        if (ast === undefined && isRequired) {
            fail();
        }
        let resolved = ast && ctx.resolveExpr(ast);
        if (resolved === undefined) {
            return isRequired ? PROP_ERROR : undefined;
        }
        if (resolved.type.kind === 'int' && (expectedType.kind === 'float' || expectedType.kind === 'fraction')) {
            resolved = _coerceFromInt(resolved, expectedType);
        }
        else if (coerceToStr) {
            resolved = _coerceToStr(resolved);
        }
        if (!ctx.checkType(expectedType, resolved)) {
            return PROP_ERROR;
        }
        else if (!isConst) {
            return resolved;
        }
        else if (resolved.kind === 'expr.constant') {
            return resolved.constant.value;
        }
        else {
            ctx.error(`expected compile-time constant`, resolved.pos);
            return PROP_ERROR;
        }
    }
    function _resolveProps(node, ctx) {
        const specs = PROP_SPECS[node.kind];
        const props = {};
        let allOK = true;
        for (const [propName, spec] of Object.entries(specs)) {
            const resolved = _resolveProp(node, propName, spec, ctx);
            if (resolved === PROP_ERROR) {
                allOK = false;
            }
            else {
                props[propName] = resolved;
            }
        }
        return allOK ? props : undefined;
    }
    // check AST instead of resolved, because we want to allow rules with constant false conditions
    function _hasAtLeastOneRewriteRule(rules) {
        return rules.some(rule => rule.kind === 'rule.rewrite'
            || (rule.kind === 'rule.decl' && _hasAtLeastOneRewriteRule(rule.children)));
    }
    function _resolveRules(node, ctx, outGrid, allowFieldObserve) {
        const out = {
            assigns: [],
            rewrites: [],
            fields: allowFieldObserve ? [] : undefined,
            observations: allowFieldObserve ? [] : undefined,
        };
        if (!_hasAtLeastOneRewriteRule(node.rules)) {
            ctx.error(`'${node.kind}' block must have at least one rewrite rule`, node.pos);
        }
        for (const rule of node.rules) {
            const r = ctx.resolveRule(rule, outGrid);
            if (r === undefined) {
                continue;
            }
            if (r.assigns !== undefined) {
                (out.assigns ??= []).push(...r.assigns);
            }
            for (const resolved of r.rules) {
                const arr = resolved.kind === 'rule.field' ? out.fields
                    : resolved.kind === 'rule.observe' ? out.observations
                        : resolved.kind === 'rule.rewrite' ? out.rewrites
                            : undefined;
                if (arr !== undefined) {
                    arr.push(resolved);
                }
                else {
                    ctx.error(`'${resolved.kind}' not allowed in '${node.kind}' block`, resolved.pos);
                }
            }
        }
        // sorting makes it more likely that samplers can be reused
        out.rewrites.sort((a, b) => _cmpPatternKey(a.from, b.from));
        out.observations?.sort((a, b) => _cmpPatternKey(a.from, b.from));
        return out;
    }
    function _cmpPatternKey(a, b) {
        const aKey = PatternTree.key(a), bKey = PatternTree.key(b);
        return aKey < bKey ? -1 : aKey === bKey ? 0 : 1;
    }
    /**
     * Conservatively estimates whether the given rewrite rules commute.
     */
    function _rewritesCommute(rewrites) {
        if (rewrites.length === 0) {
            return true;
        }
        const mapping = emptyArray(rewrites[0].to.type.alphabetKey.length, -1);
        return rewrites.every(({ from, to }) => to.kind === 'expr.constant' && from.kind === 'leaf' && to.constant.value.every((x, y, toColour) => ISet.every(from.masks[x + y * from.width], fromColour => {
            if (mapping[fromColour] >= 0 && mapping[fromColour] !== toColour) {
                return false;
            }
            mapping[fromColour] = toColour;
            return true;
        })));
    }
    // resolver functions which can handle multiple kinds of node
    function _resolveSimpleLiteralExpr(expr) {
        const kind = expr.kind.replace(/^expr\.literal\./, '');
        return _makeConstantExpr(Type.PRIMITIVES[kind], expr.value, expr.pos);
    }
    function _resolvePatternLiteralExpr(expr, ctx) {
        if (!ctx.expectGrid(expr.pos)) {
            return undefined;
        }
        const { alphabet } = ctx.grid;
        const { width, height, value } = expr;
        const pattern = [];
        const masks = [];
        let ok = true, hasUnions = false;
        for (const c of value) {
            let charID = -1, isUnion = false;
            let mask;
            if (c.kind === 'CHARSET') {
                mask = ctx.resolveCharSet(c);
                if (mask !== undefined) {
                    const size = ISet.size(mask);
                    if (size === 0) {
                        ctx.error('empty charset', c.pos);
                        ok = false;
                    }
                    else if (size === 1) {
                        charID = ISet.first(mask);
                    }
                    else {
                        isUnion = size < alphabet.key.length;
                    }
                }
            }
            else {
                mask = ctx.resolveChar(c);
                if (mask !== undefined) {
                    charID = alphabet.map.getIDOrDefault(c.s);
                    isUnion = charID < 0 && c.s !== '.';
                }
            }
            if (mask !== undefined) {
                pattern.push(charID >= 0 ? charID : isUnion ? 254 /* PatternValue.UNION */ : 255 /* PatternValue.WILDCARD */);
                masks.push(mask);
                hasUnions ||= isUnion;
            }
            else {
                ok = false;
            }
        }
        return ok ? new Pattern(width, height, alphabet.key, pattern, masks, hasUnions) : undefined;
    }
    function _resolveCountExpr(expr, ctx) {
        const { pos } = expr;
        if (!ctx.expectGrid(pos)) {
            return undefined;
        }
        const pattern = _resolveProp(expr, 'child', 'const pattern.in', ctx);
        if (pattern === PROP_ERROR) {
            return undefined;
        }
        const patterns = Symmetry.generate(pattern, ctx.symmetryName, PatternTree.rotate, PatternTree.reflect, PatternTree.key);
        // sorting makes it more likely that samplers can be reused
        patterns.sort(_cmpPatternKey);
        const inGrid = ctx.grid.id;
        const flags = 2 /* ASG.ExprFlags.DETERMINISTIC */ | 4 /* ASG.ExprFlags.LOCALLY_DETERMINISTIC */ | 8 /* ASG.ExprFlags.POSITION_INDEPENDENT */;
        return { kind: 'expr.count', type: Type.INT, flags, inGrid, patterns, pos };
    }
    function _resolveLoadExpr(expr, ctx) {
        if (!ctx.expectGrid(expr.pos)) {
            return undefined;
        }
        const { legend, key: alphabetKey } = ctx.grid.alphabet;
        if (legend === undefined) {
            ctx.error(`missing 'legend' declaration`, expr.pos);
            return undefined;
        }
        const path = _resolveProp(expr, 'child', 'const str', ctx);
        if (path === PROP_ERROR) {
            return undefined;
        }
        const width = -1, height = -1, hasUnions = legend.hasUnions;
        const type = { kind: hasUnions ? 'pattern.in' : 'pattern.out', alphabetKey, width, height };
        // TODO
        ctx.error(`'load' expression is currently unsupported`, expr.pos);
        return undefined;
    }
    function _resolveRandIntExpr(expr, ctx) {
        const { pos } = expr;
        const max = _resolveProp(expr, 'child', 'int', ctx);
        if (max === PROP_ERROR) {
            return undefined;
        }
        if (max.kind === 'expr.constant') {
            if (max.constant.value <= 0) {
                ctx.error(MJr.SAMPLE_EMPTY_MESSAGE, pos);
                return undefined;
            }
            if (max.constant.value === 1) {
                return _makeConstantExpr(Type.INT, 0, expr.pos);
            }
        }
        const flags = 16 /* ASG.ExprFlags.GRID_INDEPENDENT */ | 8 /* ASG.ExprFlags.POSITION_INDEPENDENT */;
        return { kind: 'expr.randint', type: Type.INT, flags, max, pos };
    }
    function _resolveSumExpr(expr, ctx) {
        const { pos } = expr;
        if (!ctx.expectGrid(expr.pos)) {
            return undefined;
        }
        const { kernel, grid } = ctx;
        if (kernel === undefined) {
            ctx.error(`'sum' expression may only be used 'convolution' statement`, pos);
            return undefined;
        }
        if (!ctx.isRuleContext) {
            ctx.error(`'sum' expression may only be used in a rule condition or output pattern`, pos);
            return undefined;
        }
        const charsPattern = _resolveProp(expr, 'child', 'const charset.in', ctx);
        if (charsPattern === PROP_ERROR) {
            return undefined;
        }
        switch (charsPattern.kind) {
            case 'bottom': {
                return _makeConstantExpr(Type.INT, 0, pos);
            }
            case 'top':
            case 'leaf': {
                const chars = charsPattern.masks[0];
                const includeBoundary = ctx.boundaryMask !== undefined && !ISet.isDisjoint(ctx.boundaryMask, chars);
                if (charsPattern.kind === 'top' && includeBoundary) {
                    return _makeConstantExpr(Type.INT, kernel.total(), pos);
                }
                const patternID = grid.convPatterns.getOrCreateID({ chars, kernel, includeBoundary });
                const flags = 2 /* ASG.ExprFlags.DETERMINISTIC */ | 4 /* ASG.ExprFlags.LOCALLY_DETERMINISTIC */;
                return { kind: 'expr.sum', type: Type.INT, flags, inGrid: grid.id, patternID, pos };
            }
            default: {
                // logical ops on const 1x1 patterns should already be folded
                fail();
            }
        }
    }
    function _checkZero(ctx, child) {
        if (child.kind !== 'expr.constant') {
            const type = child.type;
            return { kind: 'expr.op.unary', type, flags: child.flags, op: `${type.kind}_checkzero`, child, pos: child.pos };
        }
        else if (child.constant.value !== 0) {
            return child;
        }
        else {
            ctx.diagnostics.compilationError('division by zero in constant expression', child.pos);
            return undefined;
        }
    }
    function _resolveObserveOrRewriteRule(rule, ctx, outGrid) {
        const { pos } = rule;
        let from = _resolveProp(rule, 'from', 'const pattern.in', ctx);
        if (from === PROP_ERROR) {
            return undefined;
        }
        ctx.inputPattern = from;
        ctx.isRuleContext = true;
        const via = _resolveProp(rule, 'via', 'pattern.out?', ctx);
        const to = ctx.withOutGrid(outGrid, rule.from.pos, () => _resolveProp(rule, 'to', 'pattern.out', ctx));
        ctx.inputPattern = undefined;
        const condition = _resolveProp(rule, 'condition', 'bool?', ctx) ?? _makeConstantExpr(Type.BOOL, true, pos);
        ctx.isRuleContext = false;
        if (via === PROP_ERROR || to === undefined || to === PROP_ERROR || condition === PROP_ERROR) {
            return undefined;
        }
        if (rule.kind === 'rule.rewrite' && to.kind === 'expr.constant') {
            from = PatternTree.and(from, PatternTree.not(to.constant.value, to.type.alphabetKey));
        }
        if (from.kind === 'bottom') {
            ctx.error('input pattern has no effect', rule.from.pos);
            return { rules: [] };
        }
        else if (to.kind === 'expr.constant' && to.constant.value.kind === 'top') {
            ctx.error('output pattern has no effect', rule.to.pos);
            return { rules: [] };
        }
        else if (via !== undefined && via.kind === 'expr.constant' && via.constant.value.kind === 'top') {
            ctx.error(`'via' pattern has no effect`, rule.via.pos);
            return { rules: [] };
        }
        else if (condition.kind === 'expr.constant' && !condition.constant.value) {
            // condition is constant `false` expression
            return { rules: [] };
        }
        const rules = [];
        const makeRule = (from, via, to) => {
            rules.push({ kind: rule.kind, from, via, to, condition, pos });
        };
        if ((via === undefined || via.kind === 'expr.constant') && to.kind === 'expr.constant') {
            const symmetries = Symmetry.generate({ from, via: via?.constant.value, to: to.constant.value }, ctx.symmetryName, s => ({ from: PatternTree.rotate(s.from), via: s.via && Pattern.rotate(s.via), to: Pattern.rotate(s.to) }), s => ({ from: PatternTree.reflect(s.from), via: s.via && Pattern.reflect(s.via), to: Pattern.reflect(s.to) }), s => `${PatternTree.key(s.from)} -> ${s.via && Pattern.key(s.via)} -> ${Pattern.key(s.to)}`);
            function makeExpr(p, original) {
                const { width, height } = p;
                const { alphabetKey } = original.type;
                return _makeConstantExpr({ kind: 'pattern.out', width, height, alphabetKey }, p, original.pos);
            }
            for (const s of symmetries) {
                makeRule(s.from, via && s.via && makeExpr(s.via, via), makeExpr(s.to, to));
            }
        }
        else {
            // TODO: need to apply symmetries as ASG ops
            if (ctx.symmetryName === 'none' || (from.width === 1 && from.height === 1 && to.type.width === 1 && to.type.height === 1)) {
                makeRule(from, via, to);
            }
            else {
                ctx.error(`non-constant pattern requires 'symmetry "none"'`, rule.pos);
            }
        }
        return { rules };
    }
    function _resolveBlockStmt(stmt, ctx) {
        const { kind, pos } = stmt;
        const children = ctx.resolveStmts(stmt.children);
        if (children.length === 0) {
            return undefined;
        }
        return { kind: 'stmt', stmt: { kind, children, pos } };
    }
    function _resolvePropsStmt(stmt, ctx) {
        if (!ctx.expectGrid(stmt.pos)) {
            return undefined;
        }
        const props = _resolveProps(stmt, ctx);
        if (props === undefined) {
            return undefined;
        }
        return { kind: 'stmt', stmt: { kind: stmt.kind, inGrid: ctx.grid.id, pos: stmt.pos, ...props } };
    }
    function _resolveAllOnceOneStmt(stmt, ctx) {
        const { pos } = stmt;
        if (!ctx.expectGrid(pos)) {
            return undefined;
        }
        const props = stmt.kind !== 'stmt.rules.once' ? _resolveProps(stmt, ctx) : { search: undefined, maxStates: undefined, depthCoefficient: undefined, temperature: undefined };
        if (props === undefined) {
            return undefined;
        }
        const { search: isSearch = false, maxStates, depthCoefficient, temperature } = props;
        const { rewrites, fields, observations, assigns } = _resolveRules(stmt, ctx, ctx.grid, true);
        if (rewrites.length === 0) {
            return undefined;
        }
        const inGrid = ctx.grid.id;
        const kind = stmt.kind === 'stmt.rules.all' ? 'all' : 'one';
        const isBasic = fields.length === 0 && observations.length === 0;
        if (isSearch) {
            for (const field of fields) {
                ctx.error(`'field' cannot be used with 'search'`, field.pos);
            }
            if (observations.length === 0) {
                ctx.error(`'search' requires at least one 'observe'`, pos);
            }
        }
        else {
            if (maxStates !== undefined) {
                ctx.error(`argument 'maxStates' can only be used with 'search'`, maxStates.pos);
            }
            if (depthCoefficient !== undefined) {
                ctx.error(`argument 'depthCoefficient' can only be used with 'search'`, depthCoefficient.pos);
            }
        }
        let r;
        if (isBasic) {
            if (temperature !== undefined) {
                ctx.error(`'temperature' requires at least one 'field' or 'observe'`, temperature.pos);
            }
            r = { kind: `stmt.rules.basic.${kind}`, inGrid, rewrites, commutative: _rewritesCommute(rewrites), pos };
        }
        else if (isSearch) {
            r = { kind: `stmt.rules.search.${kind}`, inGrid, temperature, maxStates, depthCoefficient, rewrites, observations, pos };
        }
        else {
            if (fields.length > 0 && observations.length > 0) {
                ctx.error(`cannot have 'field' and 'observe' rules in the same block`, pos);
            }
            r = { kind: `stmt.rules.biased.${kind}`, inGrid, temperature, rewrites, fields, observations, pos };
        }
        if (stmt.kind === 'stmt.rules.once') {
            const limit = _makeConstantExpr(Type.INT, 1, pos);
            r = { kind: 'stmt.modified.limit', limit, child: r, pos };
        }
        return { kind: 'stmt', assigns, stmt: r };
    }
    const DECL_RESOLVE_FUNCS = {
        'decl.legend': (decl, ctx, f) => [
            undefined,
            ctx.withLegend(decl, f),
        ],
        'decl.let': (decl, ctx, f) => {
            const { name, pos } = decl.name;
            let rhs = ctx.resolveExpr(decl.rhs);
            if (rhs === undefined) {
                return [undefined, ctx.withErrorVariable(name, f)];
            }
            const { type, flags } = rhs;
            if (decl.isParam) {
                rhs = { kind: 'expr.param', type, flags, otherwise: rhs, name, pos };
            }
            const isMutable = rhs.kind !== 'expr.constant';
            const variable = ctx.makeVariable(name, type, flags, isMutable ? undefined : rhs, decl.isParam, decl.name.pos);
            return [
                // constants will be folded, so don't emit assignment statements for them
                isMutable ? { kind: 'stmt.assign', variable, rhs, pos } : undefined,
                ctx.withVariable(variable, f),
            ];
        },
        'decl.symmetry': (decl, ctx, f) => [
            undefined,
            ctx.withSymmetry(decl, f),
        ],
        'decl.union': (decl, ctx, f) => [
            undefined,
            ctx.withUnion(decl, f),
        ],
    };
    const EXPR_RESOLVE_FUNCS = {
        'expr.attr': (expr, ctx) => {
            const left = _resolveProp(expr, 'left', 'object', ctx);
            if (left === PROP_ERROR) {
                return undefined;
            }
            const { attr, pos } = expr;
            const { kind } = left.type;
            const entryTypes = kind === 'grid' ? Type.GRID_ATTRS
                : kind === 'position' ? Type.POSITION_ATTRS
                    : left.type.entryTypes;
            const type = entryTypes.get(attr);
            if (type === undefined) {
                ctx.diagnostics.typeError(`type '${Type.toStr(left.type)}' has no attribute named '${attr}'`, pos);
                return undefined;
            }
            if (kind === 'grid') {
                const grid = _resolveProp(expr, 'left', 'const grid', ctx);
                if (grid === PROP_ERROR) {
                    return undefined;
                }
                return { kind: 'expr.attr.grid', type, flags: 31 /* ASG.ExprFlags.CONSTANT */, grid, attr: attr, pos };
            }
            else if (left.kind === 'expr.constant') {
                if (kind === 'dict') {
                    const constant = left.constant.value.get(attr) ?? fail();
                    return _makeConstantExpr(type, constant.value, pos);
                }
                else if (kind === 'position') {
                    const value = left.constant.value[attr];
                    return _makeConstantExpr(type, value, pos);
                }
            }
            return { kind: `expr.attr.${kind}`, type, flags: left.flags, left, attr, pos };
        },
        'expr.decl': (expr, ctx) => {
            const [decl, child] = ctx.resolveDecl(expr.declaration, () => ctx.resolveExpr(expr.child));
            return decl === undefined || child === undefined || child.kind === 'expr.constant'
                ? child
                : { kind: 'expr.decl', type: child.type, flags: decl.rhs.flags & child.flags, decl, child, pos: decl.pos };
        },
        'expr.dict': (expr, ctx) => {
            const { pos } = expr;
            const entryExprs = new Map();
            const entryTypes = new Map();
            const value = new Map();
            let ok = true, flags = 31 /* ASG.ExprFlags.ALL */;
            for (const [{ name }, v] of expr.pairs) {
                const resolved = ctx.resolveExpr(v);
                if (resolved !== undefined) {
                    entryExprs.set(name, resolved);
                    entryTypes.set(name, resolved.type);
                    if (resolved.kind === 'expr.constant') {
                        value.set(name, resolved.constant);
                    }
                    flags &= resolved.flags;
                }
                else {
                    ok = false;
                }
            }
            if (!ok) {
                return undefined;
            }
            const type = { kind: 'dict', entryTypes };
            if (value.size === entryExprs.size) {
                return _makeConstantExpr(type, value, pos);
            }
            return { kind: 'expr.dict', type, flags, entryExprs, pos };
        },
        'expr.grid': (expr, ctx) => {
            const { pos } = expr;
            const props = _resolveProps(expr, ctx);
            if (props === undefined) {
                return undefined;
            }
            const { scaleX = 1, scaleY = 1 } = props;
            if (scaleX <= 0) {
                ctx.error(`'scaleX' must be positive`, expr.scaleX.pos);
            }
            if (scaleY <= 0) {
                ctx.error(`'scaleY' must be positive`, expr.scaleY.pos);
            }
            const alphabet = new Alphabet(expr.alphabetKey);
            const grid = withNextID(ctx.globals.grids, {
                alphabet,
                scaleX,
                scaleY,
                periodic: props.periodic ?? false,
                convPatterns: IDMap.withKey(_convPatternKey),
                pos,
            });
            return _makeConstantExpr(Type.GRID, grid.id, pos);
        },
        'expr.literal.bool': _resolveSimpleLiteralExpr,
        'expr.literal.float': _resolveSimpleLiteralExpr,
        'expr.literal.int': _resolveSimpleLiteralExpr,
        'expr.literal.pattern': (expr, ctx) => {
            const p = _resolvePatternLiteralExpr(expr, ctx);
            if (p === undefined) {
                return undefined;
            }
            const { width, height, alphabetKey } = p;
            return _makeConstantExpr({ kind: p.hasUnions ? 'pattern.in' : 'pattern.out', alphabetKey, width, height }, p, expr.pos);
        },
        'expr.literal.str': _resolveSimpleLiteralExpr,
        'expr.name.keyword': (expr, ctx) => {
            const { name, pos } = expr;
            const flags = {
                at: 2 /* ASG.ExprFlags.DETERMINISTIC */ | 4 /* ASG.ExprFlags.LOCALLY_DETERMINISTIC */ | 16 /* ASG.ExprFlags.GRID_INDEPENDENT */,
                origin: 2 /* ASG.ExprFlags.DETERMINISTIC */ | 4 /* ASG.ExprFlags.LOCALLY_DETERMINISTIC */ | 16 /* ASG.ExprFlags.GRID_INDEPENDENT */ | 1 /* ASG.ExprFlags.RUNTIME_CONSTANT */ | 8 /* ASG.ExprFlags.POSITION_INDEPENDENT */,
                random: 16 /* ASG.ExprFlags.GRID_INDEPENDENT */ | 8 /* ASG.ExprFlags.POSITION_INDEPENDENT */,
            }[name];
            switch (name) {
                case 'random':
                    return { kind: 'expr.name.keyword', type: Type.FLOAT, flags, name, pos };
            }
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            switch (name) {
                case 'at':
                    if (!ctx.isRuleContext) {
                        ctx.error(`'at' expression may only be used in a rule condition or output pattern`, pos);
                    }
                    break;
            }
            const type = { kind: 'position', inGrid: ctx.grid.id };
            return { kind: 'expr.name.keyword', type, flags, name, pos };
        },
        'expr.name.simple': (expr, ctx) => {
            const { name, pos } = expr;
            if (ctx.errorVariables.has(name)) {
                return undefined;
            }
            const variable = ctx.variables.get(name);
            if (variable === undefined) {
                ctx.error(`no such variable '${name}'`, pos);
                return undefined;
            }
            const { type, initialiser } = variable;
            if (initialiser !== undefined && initialiser.kind === 'expr.constant') {
                return _makeConstantExpr(type, initialiser.constant.value, pos);
            }
            else {
                ++variable.references;
                return { kind: 'expr.name.simple', type, flags: variable.flags, variable, pos };
            }
        },
        'expr.op.binary': (expr, ctx) => {
            let left = ctx.resolveExpr(expr.left), right = ctx.resolveExpr(expr.right);
            if (left === undefined || right === undefined) {
                return undefined;
            }
            const { pos } = expr;
            if ((expr.op === 'and' || expr.op === 'or') && (left.type.kind === 'pattern.in' || left.type.kind === 'pattern.out')) {
                const type = { ...left.type, kind: 'pattern.in' };
                if (!ctx.checkType(type, right)) {
                    return undefined;
                }
                if (left.kind !== 'expr.constant' || right.kind !== 'expr.constant') {
                    // TODO?
                    ctx.error(`'${expr.op}' may only be used on constant patterns`, pos);
                    return undefined;
                }
                const value = PatternTree[expr.op](left.constant.value, right.constant.value);
                return _makeConstantExpr(type, value, pos);
            }
            // type coercion, from int to float or fraction
            if (left.type.kind === 'int' && (right.type.kind === 'float' || right.type.kind === 'fraction')) {
                left = _coerceFromInt(left, right.type);
            }
            else if (right.type.kind === 'int' && (left.type.kind === 'float' || left.type.kind === 'fraction')) {
                right = _coerceFromInt(right, left.type);
            }
            else if (left.type.kind === 'str' && expr.op === '+') {
                right = _coerceToStr(right);
            }
            else if (right.type.kind === 'str' && expr.op === '+') {
                left = _coerceToStr(left);
            }
            const spec = Op.BINARY_OP_TYPES[expr.op];
            for (const [leftType, rightType, outType, op] of spec) {
                // OK to only check `kind`, since it's a primitive type
                if (left.type.kind !== leftType) {
                    continue;
                }
                // binary operators don't have multiple `rightType` options for the same `left.type`
                if (!ctx.checkType(Type.PRIMITIVES[rightType], right)) {
                    return undefined;
                }
                switch (op) {
                    case 'float_truediv':
                    case 'float_mod':
                    case 'fraction_truediv':
                    case 'int_truediv':
                    case 'int_floordiv':
                    case 'int_mod':
                        right = _checkZero(ctx, right);
                        if (right === undefined) {
                            return undefined;
                        }
                        break;
                }
                const type = Type.PRIMITIVES[outType];
                const f = Op.BINARY_FUNCS[op];
                if (left.kind === 'expr.constant' && right.kind === 'expr.constant') {
                    const value = f(left.constant.value, right.constant.value);
                    if (value !== undefined) {
                        return _makeConstantExpr(type, value, pos);
                    }
                }
                const flags = left.flags & right.flags;
                return { kind: 'expr.op.binary', type, flags, op, left, right, pos };
            }
            ctx.typeError(spec.map(opt => opt[0]), left);
            return undefined;
        },
        'expr.op.ternary': (expr, ctx) => {
            const condition = _resolveProp(expr, 'condition', 'bool', ctx);
            let then = ctx.resolveExpr(expr.then);
            let otherwise = ctx.resolveExpr(expr.otherwise);
            if (condition === PROP_ERROR || then === undefined || otherwise === undefined) {
                return undefined;
            }
            if (then.type.kind === 'int' && (otherwise.type.kind === 'float' || otherwise.type.kind === 'fraction')) {
                then = _coerceFromInt(then, otherwise.type);
            }
            else if (otherwise.type.kind === 'int' && (then.type.kind === 'float' || then.type.kind === 'fraction')) {
                otherwise = _coerceFromInt(otherwise, then.type);
            }
            else if (then.type.kind === 'str') {
                otherwise = _coerceToStr(otherwise);
            }
            else if (otherwise.type.kind === 'str') {
                then = _coerceToStr(then);
            }
            const type = Type.leastUpperBound(then.type, otherwise.type);
            if (type === undefined) {
                ctx.typeError([Type.toStr(then.type)], otherwise);
                return undefined;
            }
            const flags = condition.flags & then.flags & otherwise.flags;
            if (condition.kind === 'expr.constant') {
                return condition.constant.value ? then : otherwise;
            }
            return { kind: 'expr.op.ternary', type, flags, condition, then, otherwise, pos: expr.pos };
        },
        'expr.op.unary': (expr, ctx) => {
            switch (expr.op) {
                case 'count':
                    return _resolveCountExpr(expr, ctx);
                case 'load':
                    return _resolveLoadExpr(expr, ctx);
                case 'randint':
                    return _resolveRandIntExpr(expr, ctx);
                case 'sum':
                    return _resolveSumExpr(expr, ctx);
            }
            const child = ctx.resolveExpr(expr.child);
            if (child === undefined) {
                return undefined;
            }
            const { pos } = expr;
            if (expr.op === 'not' && (child.type.kind === 'pattern.in' || child.type.kind === 'pattern.out')) {
                const type = { ...child.type, kind: 'pattern.in' };
                if (child.kind !== 'expr.constant') {
                    // TODO?
                    ctx.error(`'not' may only be used on constant patterns`, pos);
                    return undefined;
                }
                const value = PatternTree.not(child.constant.value, type.alphabetKey);
                return _makeConstantExpr(type, value, pos);
            }
            // unary + is a NOOP
            if (expr.op === '+') {
                return ctx.checkType(Type.NUMERIC, child) ? child : undefined;
            }
            const spec = Op.UNARY_OP_TYPES[expr.op];
            for (const [inType, outType, op] of spec) {
                // OK to only check `kind`, since it's a primitive type
                if (child.type.kind !== inType) {
                    continue;
                }
                const type = Type.PRIMITIVES[outType];
                const f = Op.UNARY_FUNCS[op];
                if (child.kind === 'expr.constant' && f !== undefined) {
                    const value = f(child.constant.value);
                    if (value !== undefined) {
                        return _makeConstantExpr(type, value, pos);
                    }
                }
                return { kind: 'expr.op.unary', type, flags: child.flags, op, child, pos };
            }
            ctx.typeError(spec.map(opt => opt[0]), child);
            return undefined;
        },
    };
    const RULE_RESOLVE_FUNCS = {
        'rule.decl': (rule, ctx, outGrid) => {
            const assigns = [];
            const rules = [];
            let [decl, _] = ctx.resolveDecl(rule.declaration, () => {
                for (const c of rule.children) {
                    const r = ctx.resolveRule(c, outGrid);
                    if (r === undefined) {
                        continue;
                    }
                    if (r.assigns !== undefined) {
                        assigns.push(...r.assigns);
                    }
                    rules.push(...r.rules);
                }
            });
            if (decl !== undefined) {
                assigns.unshift(decl);
            }
            return { assigns, rules };
        },
        'rule.field': (rule, ctx, outGrid) => {
            const { pos } = rule;
            const props = _resolveProps(rule, ctx);
            if (props === undefined) {
                return undefined;
            }
            const { for_, on, from, to, recompute = false, essential = false } = props;
            const zero = from ?? to;
            const inversed = from !== undefined;
            if (zero === undefined) {
                ctx.error(`'field' must have either 'from' or 'to'`, pos);
                return undefined;
            }
            else if (from !== undefined && to !== undefined) {
                ctx.error(`'field' cannot have both 'from' and 'to'`, pos);
            }
            const potential = withNextID(ctx.globals.potentials, { inGrid: ctx.grid.id, for_ });
            return { rules: [{ kind: 'rule.field', potential, for_, on, zero, inversed, recompute, essential, pos }] };
        },
        'rule.observe': _resolveObserveOrRewriteRule,
        'rule.rewrite': _resolveObserveOrRewriteRule,
    };
    const STMT_RESOLVE_FUNCS = {
        'stmt.block.markov': _resolveBlockStmt,
        'stmt.block.sequence': _resolveBlockStmt,
        'stmt.convchain': (stmt, ctx) => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            const props = _resolveProps(stmt, ctx);
            if (props === undefined) {
                return undefined;
            }
            const { on, sample, n, periodic = true, temperature, anneal, epsilon = 0.125 } = props;
            // 1x1 patterns should be folded
            if (on !== undefined && on.kind !== 'leaf' && on.kind !== 'top') {
                fail();
            }
            if (sample.pattern.some(c => c === 255 /* PatternValue.WILDCARD */ || c === 254 /* PatternValue.UNION */)) {
                ctx.error(`'sample' must not have wildcards or unions`, stmt.sample.pos);
            }
            if (n < 1 || n > sample.width || n > sample.height) {
                ctx.error(`'n' must be at least 1 and at most the sample dimensions`, stmt.n.pos);
                return undefined;
            }
            if (epsilon <= 0) {
                ctx.error(`'epsilon' must be positive`, stmt.epsilon.pos);
            }
            if (temperature !== undefined && temperature.kind === 'expr.constant' && temperature.constant.value < 0) {
                ctx.error(`'temperature' must be non-negative`, stmt.temperature.pos);
            }
            if (anneal !== undefined && anneal.kind === 'expr.constant' && anneal.constant.value < 0) {
                ctx.error(`'anneal' must be non-negative`, stmt.anneal.pos);
            }
            const output = ISet.toArray(ISet.of(ctx.grid.alphabet.key.length, sample.pattern));
            if (output.length < 2) {
                ctx.error(`'sample' must use at least two different alphabet symbols`, stmt.sample.pos);
                return undefined;
            }
            const samplePatterns = IDMap.withKey(Pattern.key);
            const sampleWeights = [];
            for (const symmetry of Symmetry.generate(sample, ctx.symmetryName, Pattern.rotate, Pattern.reflect, Pattern.key)) {
                for (const p of Pattern.windowsOf(symmetry, n, periodic)) {
                    const id = samplePatterns.getOrCreateID(p);
                    if (id === sampleWeights.length) {
                        sampleWeights.push(0);
                    }
                    ++sampleWeights[id];
                }
            }
            const weightMap = new Map();
            samplePatterns.forEach((p, i) => {
                getOrCompute(weightMap, sampleWeights[i], () => []).push(p);
            });
            const weights = Array.from(weightMap.entries(), ([weight, v]) => ({
                pattern: v.reduce(PatternTree.or),
                weight,
            }));
            return { kind: 'stmt', stmt: {
                    kind: 'stmt.convchain',
                    inGrid: ctx.grid.id,
                    on: on ?? PatternTree.top(1, 1, ctx.grid.alphabet.key),
                    weights, output, temperature, anneal, epsilon,
                    pos,
                } };
        },
        'stmt.decl': (stmt, ctx) => {
            let [decl, stmts] = ctx.resolveDecl(stmt.declaration, () => ctx.resolveStmts(stmt.children));
            stmts ??= [];
            if (decl !== undefined) {
                stmts.unshift(decl);
            }
            return { kind: 'stmts', stmts };
        },
        'stmt.log': _resolvePropsStmt,
        'stmt.modified.limit': (stmt, ctx) => {
            const limit = _resolveProp(stmt, 'arg', 'int', ctx);
            const r = ctx.resolveStmt(stmt.child);
            if (limit === PROP_ERROR) {
                return undefined;
            }
            if (limit.kind === 'expr.constant' && limit.constant.value <= 0) {
                ctx.error(`limit must be positive (was ${limit})`, stmt.arg.pos);
            }
            // TODO: loosen this to allow e.g. random limits; problem is that limit initialisers will be hoisted
            // to the start of their parent blocks, where referenced variables might not yet be assigned
            if ((limit.flags & 1 /* ASG.ExprFlags.RUNTIME_CONSTANT */) === 0) {
                ctx.error(`limit must be a runtime constant`, stmt.arg.pos);
            }
            if (r === undefined) {
                return undefined;
            }
            else if (r.kind === 'stmts' || r.stmt.kind === 'stmt.log' || r.stmt.kind === 'stmt.rules.map' || r.stmt.kind === 'stmt.put' || r.stmt.kind === 'stmt.use') {
                ctx.error(`'@limit' cannot modify '${stmt.child.kind}'`, stmt.child.pos);
                return undefined;
            }
            else if (r.stmt.kind === 'stmt.modified.limit') {
                ctx.error(`statement cannot have multiple limits`, stmt.child.pos);
            }
            const { assigns, stmt: child } = r;
            return { kind: 'stmt', assigns, stmt: { kind: 'stmt.modified.limit', limit, child, pos: stmt.pos } };
        },
        'stmt.path': _resolvePropsStmt,
        'stmt.pass': (stmt, ctx) => undefined,
        'stmt.put': (stmt, ctx) => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            const at = _resolveProp(stmt, 'at', 'position', ctx);
            ctx.isRuleContext = true;
            const props = _resolveProps(stmt, ctx);
            ctx.isRuleContext = false;
            if (at === PROP_ERROR || props === undefined) {
                return undefined;
            }
            const { pattern, condition } = props;
            const inGrid = ctx.grid.id;
            return { kind: 'stmt', stmt: { kind: 'stmt.put', inGrid, at, pattern, condition, pos } };
        },
        'stmt.rules.all': _resolveAllOnceOneStmt,
        'stmt.rules.convolution': (stmt, ctx) => ctx.withKernel(stmt, kernel => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            let boundary = _resolveProp(stmt, 'boundary', 'const charset.in?', ctx);
            if (boundary !== undefined && ctx.grid.periodic) {
                ctx.error(`periodic grid has no boundary`, stmt.boundary.pos);
            }
            if (boundary === PROP_ERROR) {
                boundary = undefined;
            }
            ctx.boundaryMask
                = boundary === undefined ? undefined
                    : PatternTree.isLeafOrTop(boundary) ? boundary.masks[0]
                        : fail();
            const { rewrites, assigns } = _resolveRules(stmt, ctx, ctx.grid, false);
            ctx.boundaryMask = undefined;
            if (rewrites.length === 0) {
                return undefined;
            }
            const charsUsed = ISet.empty(ctx.grid.alphabet.key.length);
            for (const rule of rewrites) {
                if (rule.from.width !== 1 || rule.from.height !== 1) {
                    ctx.error(`'convolution' rule patterns must be 1x1`, rule.pos);
                }
                if (rule.from.kind === 'bottom') {
                    continue;
                }
                // logical ops on const 1x1 patterns should already be folded
                const chars = PatternTree.isLeafOrTop(rule.from) ? rule.from.masks[0] : fail();
                if (!ISet.isDisjoint(chars, charsUsed)) {
                    ctx.error(`'convolution' input patterns must be disjoint`, rule.pos);
                }
                ISet.addAll(charsUsed, chars);
            }
            return {
                kind: 'stmt',
                assigns,
                stmt: { kind: 'stmt.rules.convolution', inGrid: ctx.grid.id, rewrites, kernel, boundary, pos },
            };
        }),
        'stmt.rules.map': (stmt, ctx) => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            const inGrid = ctx.grid.id;
            const outGrid = _resolveProp(stmt, 'outGrid', 'const grid', ctx);
            if (outGrid === PROP_ERROR) {
                return undefined;
            }
            if (outGrid === inGrid) {
                ctx.error(`'outGrid' must be different to the input grid`, stmt.outGrid.pos);
            }
            const formalOutGrid = ctx.globals.grids[outGrid];
            const { assigns, rewrites } = _resolveRules(stmt, ctx, formalOutGrid, false);
            ctx.grid = formalOutGrid;
            if (rewrites.length === 0) {
                return undefined;
            }
            const commutative = rewrites.every(rule => rule.from.width === 1 && rule.from.height === 1);
            return { kind: 'stmt', assigns, stmt: { kind: 'stmt.rules.map', inGrid, outGrid, rewrites, commutative, pos } };
        },
        'stmt.rules.once': _resolveAllOnceOneStmt,
        'stmt.rules.one': _resolveAllOnceOneStmt,
        'stmt.rules.prl': (stmt, ctx) => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            const { rewrites, assigns } = _resolveRules(stmt, ctx, ctx.grid, false);
            if (rewrites.length === 0) {
                return undefined;
            }
            return {
                kind: 'stmt',
                assigns,
                stmt: { kind: 'stmt.rules.basic.prl', inGrid: ctx.grid.id, rewrites, commutative: _rewritesCommute(rewrites), pos },
            };
        },
        'stmt.use.expr': (stmt, ctx) => {
            const grid = _resolveProp(stmt, 'expr', 'const grid', ctx);
            if (grid === PROP_ERROR) {
                return undefined;
            }
            ctx.grid = ctx.globals.grids[grid];
            return { kind: 'stmt', stmt: { kind: 'stmt.use', grid, pos: stmt.pos } };
        },
        'stmt.use.let': (stmt, ctx) => {
            if (stmt.decl.isParam) {
                ctx.error(`'use let' declaration cannot be a 'param'`, stmt.pos);
            }
            const grid = _resolveProp(stmt.decl, 'rhs', 'const grid', ctx);
            if (grid === PROP_ERROR) {
                return undefined;
            }
            ctx.grid = ctx.globals.grids[grid];
            const { name, rhs, pos } = stmt.decl;
            const variable = ctx.makeVariable(name.name, Type.GRID, 31 /* ASG.ExprFlags.CONSTANT */, _makeConstantExpr(Type.GRID, grid, rhs.pos), false, name.pos);
            const stmts = ctx.withVariable(variable, () => ctx.resolveStmts(stmt.children));
            stmts.unshift(
            //{kind: 'stmt.assign', variable, rhs: _makeConstantExpr(Type.GRID, grid, rhs.pos), pos},
            { kind: 'stmt.use', grid, pos: stmt.pos });
            return { kind: 'stmts', stmts };
        },
    };
    function resolve(ast) {
        const ctx = new Context();
        const root = ctx.resolveRoot(ast);
        const endGrid = ctx.grid;
        if (endGrid === undefined) {
            ctx.error('program uses no grid', ast.pos);
        }
        ctx.diagnostics.throwIfAnyErrors();
        const { grids, params, potentials, variables } = ctx.globals;
        return {
            root,
            grids,
            params,
            potentials,
            variables,
            endGridID: endGrid.id,
        };
    }
    Resolver.resolve = resolve;
})(Resolver || (Resolver = {}));
var Symmetry;
(function (Symmetry) {
    Symmetry.SYMMETRY_GROUPS = ((groups) => groups)({
        all: [true, true, true, true, true, true, true, true],
        none: [true, false, false, false, false, false, false, false],
        rot90: [true, true, true, true, false, false, false, false],
        rot180: [true, false, true, false, false, false, false, false],
        x: [true, false, false, false, false, false, true, false],
        y: [true, false, false, false, true, false, false, false],
        xy: [true, false, true, false, true, false, true, false],
    });
    const TRANSFORMS = [
        { a: 1, b: 0, c: 0, d: 1 },
        { a: 0, b: 1, c: -1, d: 0 },
        { a: -1, b: 0, c: 0, d: -1 },
        { a: 0, b: -1, c: 1, d: 0 },
        { a: 1, b: 0, c: 0, d: -1 },
        { a: 0, b: 1, c: 1, d: 0 },
        { a: -1, b: 0, c: 0, d: 1 },
        { a: 0, b: -1, c: -1, d: 0 }, // flip_yx
    ];
    function transformAll(p, groupName) {
        const group = Symmetry.SYMMETRY_GROUPS[groupName];
        const out = [];
        for (let i = 0; i < group.length; ++i) {
            if (group[i]) {
                out.push(transform(p, TRANSFORMS[i]));
            }
        }
        return out;
    }
    Symmetry.transformAll = transformAll;
    /**
     * Applies a transformation matrix to a pattern.
     */
    function transform(p, m) {
        const { width, height, pattern, masks } = p;
        const newData = [];
        const newMasks = [];
        const newWidth = m.b === 0 ? width : height;
        const newHeight = m.b === 0 ? height : width;
        const xOffset = m.a < 0 || m.b < 0 ? newWidth - 1 : 0;
        const yOffset = m.c < 0 || m.d < 0 ? newHeight - 1 : 0;
        for (let y = 0; y < newHeight; ++y) {
            for (let x = 0; x < newWidth; ++x) {
                const px = m.a * x + m.b * y + xOffset;
                const py = m.c * x + m.d * y + yOffset;
                const index = px + width * py;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(newWidth, newHeight, p.alphabetKey, newData, newMasks, p.hasUnions);
    }
    Symmetry.transform = transform;
    function generate(original, groupName, rotate, reflect, keyFunc) {
        const r1 = rotate(original), r2 = rotate(r1), r3 = rotate(r2), s0 = reflect(original), s1 = reflect(r1), s2 = reflect(r2), s3 = reflect(r3);
        const group = Symmetry.SYMMETRY_GROUPS[groupName];
        const out = [original, r1, r2, r3, s0, s1, s2, s3].filter((x, i) => group[i]);
        return keyFunc !== undefined ? IDMap.distinctByKey(out, keyFunc) : out;
    }
    Symmetry.generate = generate;
})(Symmetry || (Symmetry = {}));
var Tokenizer;
(function (Tokenizer) {
    Tokenizer.KEYWORDS = [
        'all',
        'and',
        'at',
        'convchain',
        'convolution',
        'count',
        'else',
        'false',
        'field',
        'grid',
        'if',
        'in',
        'legend',
        'let',
        'limit',
        'load',
        'log',
        'map',
        'markov',
        'not',
        'observe',
        'once',
        'one',
        'or',
        'origin',
        'param',
        'pass',
        'path',
        'prl',
        'put',
        'randint',
        'random',
        'sequence',
        'sum',
        'symmetry',
        'true',
        'union',
        'use',
    ];
    const C_MAP = (function (...pairs) {
        const arr = emptyArray(128, 21 /* C.OTHER */);
        for (const [chars, c] of pairs) {
            for (let i = 0; i < chars.length; ++i) {
                arr[chars.charCodeAt(i)] = c;
            }
        }
        return arr;
    })([' \t', 0 /* C.WHITESPACE */], ['abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_', 1 /* C.LETTER_OR_UNDERSCORE */], ['0123456789', 2 /* C.DIGIT */], ['({', 3 /* C.LPAREN */], [')}', 4 /* C.RPAREN */], ['[', 5 /* C.LSQB */], [']', 6 /* C.RSQB */], ['<', 7 /* C.LANGLE */], ['>', 8 /* C.RANGLE */], ["'", 9 /* C.QUOTE */], ['"', 10 /* C.DBLQUOTE */], ['=', 11 /* C.EQUALS */], ['!', 12 /* C.EXCLAMATION_MARK */], ['-', 13 /* C.MINUS */], ['/', 14 /* C.SLASH */], ['#', 15 /* C.HASH */], ['\\', 16 /* C.BACKSLASH */], ['^', 17 /* C.CARET */], ['.', 18 /* C.DOT */], ['+*%', 19 /* C.OTHER_OP */], ['@|,:', 20 /* C.OTHER_PUNCTUATION */]);
    class LineTokenizer {
        lineString;
        lineCodes;
        constructor(lineString) {
            this.lineString = lineString;
            this.lineCodes = makeArray(lineString.length, i => {
                const charCode = lineString.charCodeAt(i);
                return charCode >= 0 && charCode < C_MAP.length ? C_MAP[charCode] : 21 /* C.OTHER */;
            });
        }
        scan(i, cs) {
            const { lineCodes } = this;
            while (i < lineCodes.length && cs.includes(lineCodes[i])) {
                ++i;
            }
            return i;
        }
        has(i, c) {
            const { lineCodes } = this;
            return i < lineCodes.length && lineCodes[i] === c;
        }
        getNextToken(mode, col, depth) {
            const { lineCodes } = this;
            const c = lineCodes[col];
            switch (mode) {
                case 0 /* Mode.NORMAL */:
                    switch (c) {
                        case 0 /* C.WHITESPACE */:
                            return ['WHITESPACE', this.scan(col, [0 /* C.WHITESPACE */]), depth, mode];
                        case 15 /* C.HASH */:
                            return ['COMMENT', lineCodes.length, depth, mode];
                        case 3 /* C.LPAREN */:
                            return ['PUNCTUATION', col + 1, depth + 1, mode];
                        case 4 /* C.RPAREN */:
                            return ['PUNCTUATION', col + 1, depth - 1, mode];
                        case 5 /* C.LSQB */:
                            return ['PUNCTUATION', col + 1, depth + 1, 1 /* Mode.PATTERN */];
                        case 9 /* C.QUOTE */:
                            return ['QUOTE', col + 1, depth, 3 /* Mode.QUOTE_STRING */];
                        case 10 /* C.DBLQUOTE */:
                            return ['QUOTE', col + 1, depth, 4 /* Mode.DBLQUOTE_STRING */];
                        case 1 /* C.LETTER_OR_UNDERSCORE */: {
                            const end = this.scan(col, [1 /* C.LETTER_OR_UNDERSCORE */, 2 /* C.DIGIT */]);
                            const s = this.lineString.substring(col, end);
                            return [KEYWORDS_SET.has(s) ? 'KEYWORD' : 'NAME', end, depth, mode];
                        }
                        case 11 /* C.EQUALS */:
                        case 12 /* C.EXCLAMATION_MARK */:
                        case 7 /* C.LANGLE */:
                        case 8 /* C.RANGLE */:
                            // '=', '!', '<', '>', '==', '!=', '<=' or '>='
                            // note that '!' is not a valid operator, but this will be caught by the parser
                            return ['OP', this.has(col + 1, 11 /* C.EQUALS */) ? col + 2 : col + 1, depth, mode];
                        case 14 /* C.SLASH */:
                            // '/' or '//'
                            return ['OP', this.has(col + 1, 14 /* C.SLASH */) ? col + 2 : col + 1, depth, mode];
                        case 13 /* C.MINUS */:
                            // '-' or '->'
                            return ['OP', this.has(col + 1, 8 /* C.RANGLE */) ? col + 2 : col + 1, depth, mode];
                        case 2 /* C.DIGIT */: {
                            const end = this.scan(col, [2 /* C.DIGIT */]);
                            if (!this.has(end, 18 /* C.DOT */)) {
                                return ['INT', end, depth, mode];
                            }
                            else if (this.has(end + 1, 2 /* C.DIGIT */)) {
                                return ['FLOAT', this.scan(end + 1, [2 /* C.DIGIT */]), depth, mode];
                            }
                            else {
                                return ['ERROR', end + 1, depth, mode];
                            }
                        }
                        case 18 /* C.DOT */:
                        case 19 /* C.OTHER_OP */:
                            // '.', '+', '*' or '%'
                            return ['OP', col + 1, depth, mode];
                        case 20 /* C.OTHER_PUNCTUATION */:
                            return ['PUNCTUATION', col + 1, depth, mode];
                        default:
                            return ['ERROR', col + 1, depth, mode];
                    }
                case 1 /* Mode.PATTERN */:
                case 2 /* Mode.CHARSET */:
                    switch (c) {
                        case 0 /* C.WHITESPACE */:
                            return ['WHITESPACE', this.scan(col, [0 /* C.WHITESPACE */]), depth, mode];
                        case 15 /* C.HASH */:
                            return ['COMMENT', lineCodes.length, depth, mode];
                        case 5 /* C.LSQB */:
                            return mode === 1 /* Mode.PATTERN */
                                ? ['PUNCTUATION', col + 1, depth + 1, 2 /* Mode.CHARSET */]
                                : ['ERROR', col + 1, depth, mode];
                        case 6 /* C.RSQB */:
                            return ['PUNCTUATION', col + 1, depth - 1, mode === 1 /* Mode.PATTERN */ ? 0 /* Mode.NORMAL */ : 1 /* Mode.PATTERN */];
                        case 17 /* C.CARET */:
                            return [mode === 2 /* Mode.CHARSET */ ? 'PUNCTUATION' : 'ERROR', col + 1, depth, mode];
                        case 18 /* C.DOT */:
                            return [mode === 1 /* Mode.PATTERN */ ? 'PATTERN_CHAR' : 'ERROR', col + 1, depth, mode];
                        case 1 /* C.LETTER_OR_UNDERSCORE */:
                        case 2 /* C.DIGIT */:
                        case 11 /* C.EQUALS */:
                        case 12 /* C.EXCLAMATION_MARK */:
                        case 13 /* C.MINUS */:
                        case 19 /* C.OTHER_OP */:
                        case 20 /* C.OTHER_PUNCTUATION */:
                        case 21 /* C.OTHER */:
                            return ['PATTERN_CHAR', col + 1, depth, mode];
                        default:
                            return ['ERROR', col + 1, depth, mode];
                    }
                case 3 /* Mode.QUOTE_STRING */:
                case 4 /* Mode.DBLQUOTE_STRING */:
                    switch (c) {
                        case 9 /* C.QUOTE */:
                        case 10 /* C.DBLQUOTE */:
                            return (mode === 3 /* Mode.QUOTE_STRING */ && c === 9 /* C.QUOTE */) || (mode === 4 /* Mode.DBLQUOTE_STRING */ && c === 10 /* C.DBLQUOTE */)
                                ? ['QUOTE', col + 1, depth, 0 /* Mode.NORMAL */]
                                : ['STRING_CHAR', col + 1, depth, mode];
                        case 16 /* C.BACKSLASH */:
                            return ['ESCAPED_CHAR', Math.min(col + 2, lineCodes.length), depth, mode];
                        default:
                            return ['STRING_CHAR', col + 1, depth, mode];
                    }
            }
        }
    }
    const KEYWORDS_SET = new Set(Tokenizer.KEYWORDS);
    function tokenize(src, skipWhitespace = false) {
        const lines = src.split('\n');
        const tokens = [];
        const indentation = [''];
        const diagnostics = new Diagnostics();
        function _makeToken(kind, s, pos) {
            if ((kind === 'WHITESPACE' || kind === 'COMMENT') && skipWhitespace) {
                return;
            }
            if (kind === 'NAME' && KEYWORDS_SET.has(s)) {
                kind = 'KEYWORD';
            }
            tokens.push({ kind, s, pos });
        }
        let depth = 0;
        let mode = 0 /* Mode.NORMAL */;
        for (let line = 1; line <= lines.length; ++line) {
            const lineString = lines[line - 1];
            // ignore lines with only whitespace and comments
            if (/^\s*(?:#.*)?$/.test(lineString)) {
                const i = lineString.indexOf('#');
                if (i >= 0) {
                    if (i > 0) {
                        _makeToken('WHITESPACE', lineString.substring(0, i), { line, col: 0 });
                    }
                    _makeToken('COMMENT', lineString.substring(i), { line, col: i });
                }
                else if (lineString.length > 0) {
                    _makeToken('WHITESPACE', lineString, { line, col: 0 });
                }
                if (!skipWhitespace) {
                    _makeToken('NEWLINE', '\n', { line, col: lineString.length });
                }
                continue;
            }
            let col = 0;
            // check for indent or dedents
            if (mode === 0 /* Mode.NORMAL */ && depth === 0) {
                const initialWhitespace = /^\s*/.exec(lineString)[0];
                const currentIndentation = indentation[indentation.length - 1];
                if (initialWhitespace.startsWith(currentIndentation)) {
                    if (initialWhitespace.length > currentIndentation.length) {
                        indentation.push(initialWhitespace);
                        _makeToken('INDENT', '', { line, col });
                    }
                }
                else {
                    while (true) {
                        const c = indentation[indentation.length - 1];
                        if (c === initialWhitespace) {
                            break;
                        }
                        indentation.pop();
                        _makeToken('DEDENT', '', { line, col });
                        if (!c.startsWith(initialWhitespace)) {
                            indentation.push(initialWhitespace);
                            diagnostics.syntaxError('inconsistent indentation', { line, col });
                            break;
                        }
                    }
                }
                col = initialWhitespace.length;
            }
            const lineTokenizer = new LineTokenizer(lineString);
            while (col < lineString.length) {
                const [kind, nextCol, nextDepth, nextMode] = lineTokenizer.getNextToken(mode, col, depth);
                _makeToken(kind, lineString.substring(col, nextCol), { line, col });
                col = nextCol;
                depth = nextDepth;
                mode = nextMode;
            }
            if (mode === 3 /* Mode.QUOTE_STRING */ || mode === 4 /* Mode.DBLQUOTE_STRING */) {
                diagnostics.syntaxError('unexpected end of line in string literal', { line, col });
                mode = 0 /* Mode.NORMAL */;
            }
            if (depth === 0 || !skipWhitespace) {
                _makeToken('NEWLINE', '\n', { line, col });
            }
        }
        const pos = { line: lines.length, col: lines[lines.length - 1].length };
        for (let i = 1; i < indentation.length; ++i) {
            _makeToken('DEDENT', '', pos);
        }
        _makeToken('EOF', '', pos);
        if (mode !== 0 /* Mode.NORMAL */) {
            diagnostics.syntaxError('unexpected end of source', pos);
        }
        diagnostics.throwIfAnyErrors();
        return tokens;
    }
    Tokenizer.tokenize = tokenize;
    class TokenQueue {
        tokens;
        i = 0;
        constructor(tokens) {
            this.tokens = tokens;
        }
        peek() {
            return this.tokens[this.i];
        }
        poll() {
            return this.tokens[this.i++];
        }
        hasNext(...kinds) {
            return kinds.includes(this.peek().kind);
        }
        hasNextS(...strings) {
            return strings.includes(this.peek().s);
        }
        pollIf(kind) {
            return this.hasNext(kind) && (++this.i, true);
        }
        pollIfS(s) {
            return this.hasNextS(s) && (++this.i, true);
        }
        skipLine() {
            while (!this.hasNext('EOF') && this.poll().kind !== 'NEWLINE') { }
        }
    }
    Tokenizer.TokenQueue = TokenQueue;
})(Tokenizer || (Tokenizer = {}));
var Type;
(function (Type) {
    Type.BOOL = { kind: 'bool' };
    Type.FLOAT = { kind: 'float' };
    Type.FRACTION = { kind: 'fraction' };
    Type.GRID = { kind: 'grid' };
    Type.INT = { kind: 'int' };
    Type.STR = { kind: 'str' };
    Type.PRIMITIVES = {
        bool: Type.BOOL,
        float: Type.FLOAT,
        fraction: Type.FRACTION,
        grid: Type.GRID,
        int: Type.INT,
        str: Type.STR,
    };
    Type.ANY_DICT = { kind: 'any_dict' };
    Type.ANY_POSITION = { kind: 'any_position' };
    Type.OBJECT = { kind: 'union', options: [Type.ANY_DICT, Type.ANY_POSITION, Type.GRID] };
    Type.NUMERIC = { kind: 'union', options: [Type.FLOAT, Type.FRACTION, Type.INT] };
    Type.GRID_ATTRS = new Map([
        ['width', Type.INT],
        ['height', Type.INT],
        ['area', Type.INT],
    ]);
    Type.POSITION_ATTRS = new Map([
        ['x', Type.INT],
        ['y', Type.INT],
    ]);
    function toStr(type) {
        switch (type.kind) {
            case 'any_dict':
                return 'dict';
            case 'any_pattern':
                return `pattern.${type.allowUnions ? 'in' : 'out'}[${type.alphabetKey}]`;
            case 'any_position':
                return 'position';
            case 'union':
                return type.options.map(toStr).join(' | ');
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int':
            case 'str':
                return type.kind;
            case 'dict':
                return `{${Array.from(type.entryTypes, ([k, t]) => `${k}: ${toStr(t)}`).join(', ')}}`;
            case 'pattern.in':
            case 'pattern.out':
                return `${type.kind}.${type.width}x${type.height}[${type.alphabetKey}]`;
            case 'position':
                return `position.grid${type.inGrid}`;
        }
    }
    Type.toStr = toStr;
    function equals(t1, t2) {
        switch (t1.kind) {
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int':
            case 'str':
                return t1.kind === t2.kind;
            case 'dict':
                return t2.kind === 'dict' && t1.entryTypes.size === t2.entryTypes.size && [...t1.entryTypes].every(([k, v1]) => {
                    const v2 = t2.entryTypes.get(k);
                    return v2 !== undefined && equals(v1, v2);
                });
            case 'pattern.in':
            case 'pattern.out':
                return t1.kind === t2.kind
                    && t1.alphabetKey === t2.alphabetKey
                    && t1.width === t2.width && t1.height === t2.height;
            case 'position':
                return t2.kind === 'position' && t1.inGrid === t2.inGrid;
        }
    }
    Type.equals = equals;
    function isSubtype(t1, t2) {
        switch (t2.kind) {
            case 'any_dict':
                return t1.kind === 'dict';
            case 'any_pattern':
                return (t1.kind === 'pattern.out' || (t1.kind === 'pattern.in' && t2.allowUnions))
                    && t1.alphabetKey === t2.alphabetKey;
            case 'any_position':
                return t1.kind === 'position';
            case 'union':
                return t2.options.some(t => isSubtype(t1, t));
            case 'dict':
                return t1.kind === 'dict' && t1.entryTypes.size === t2.entryTypes.size && [...t1.entryTypes].every(([k, v1]) => {
                    const v2 = t2.entryTypes.get(k);
                    return v2 !== undefined && isSubtype(v1, v2);
                });
            case 'pattern.in':
            case 'pattern.out':
                return (t1.kind === 'pattern.in' || t1.kind === 'pattern.out')
                    && t1.alphabetKey === t2.alphabetKey
                    && t1.width === t2.width && t1.height === t2.height
                    && (t1.kind === 'pattern.out' || t2.kind === 'pattern.in');
            default:
                return equals(t1, t2);
        }
    }
    Type.isSubtype = isSubtype;
    function leastUpperBound(t1, t2) {
        return isSubtype(t1, t2) ? t2
            : isSubtype(t2, t1) ? t1
                : undefined;
    }
    Type.leastUpperBound = leastUpperBound;
})(Type || (Type = {}));
var IR;
(function (IR) {
    function makeConstArray(ir, namePart, from, domainSize) {
        if (from.length === 0) {
            return { decl: IR.NO_DECL, get: () => IR.unusedExpr('array of length zero') };
        }
        // check if table is constant
        const start = from[0];
        if (from.every(x => x === start)) {
            return { decl: IR.NO_DECL, get: () => IR.int(start) };
        }
        // check if table is an arithmetic progression
        // only consider domain sizes where loose int arithmetic is safe
        const step = from[1] - start;
        if (domainSize < 2 ** 31 && from.every((x, i) => x === start + i * step)) {
            return step >= 0
                ? { decl: IR.NO_DECL, get: index => IR.OP.add(IR.int(start), IR.OP.multConstant(index, step)) }
                : { decl: IR.NO_DECL, get: index => IR.OP.minus(IR.int(start), IR.OP.multConstant(index, -step)) };
        }
        const decl = ir.constArrayDecl(namePart, from, domainSize);
        return {
            decl,
            get: index => IR.access(decl.name, index),
        };
    }
    IR.makeConstArray = makeConstArray;
    function makeConstArray2D(ir, namePart, from, rowLength, domainSize) {
        if (from.every((x, i) => x === from[i % rowLength])) {
            const row = from.slice(0, rowLength);
            const table = makeConstArray(ir, namePart, row, domainSize);
            return { decl: table.decl, get: (j, i) => table.get(i) };
        }
        else if (from.every((x, i) => x === from[i - i % rowLength])) {
            const col = makeArray((from.length / rowLength) | 0, i => from[i * rowLength]);
            return makeConstArray(ir, namePart, col, domainSize);
        }
        const decl = ir.constArrayDecl(namePart, from, domainSize, rowLength);
        return {
            decl,
            get: (j, i) => IR.access(decl.name, IR.OP.multAddConstant(j, rowLength, i)),
        };
    }
    IR.makeConstArray2D = makeConstArray2D;
    function makeMutableArray(ir, namePart, length, domainSize) {
        const decl = ir.mutArrayDecl(namePart, length, domainSize);
        if (domainSize <= 1 || length === IR.ZERO) {
            return { array: decl.name, decl, get: () => IR.ZERO, set: () => IR.PASS };
        }
        const arr = {
            array: decl.name,
            decl,
            get(index) {
                return IR.access(decl.name, index);
            },
            set(index, op, value) {
                return IR.assign(this.get(index), op, value);
            },
        };
        return arr;
    }
    IR.makeMutableArray = makeMutableArray;
    function makeMutableArray2D(ir, namePart, numRows, rowLength, domainSize) {
        const decl = ir.mutArrayDecl(namePart, IR.OP.mult(numRows, rowLength), domainSize);
        if (domainSize <= 1 || rowLength === IR.ZERO || numRows === IR.ZERO) {
            return { name: decl.name, decl, get: () => IR.ZERO, set: () => IR.PASS };
        }
        const get = IR.isInt(rowLength)
            ? (j, i) => IR.access(decl.name, IR.OP.multAddConstant(j, rowLength.value, i))
            : (j, i) => IR.access(decl.name, IR.OP.add(IR.OP.mult(j, rowLength), i));
        return {
            name: decl.name,
            decl,
            get,
            set: (j, i, op, value) => IR.assign(get(j, i), op, value),
        };
    }
    IR.makeMutableArray2D = makeMutableArray2D;
})(IR || (IR = {}));
///<reference path="./factory.ts"/>
var IR;
(function (IR) {
    function _key(p) {
        return `${ISet.key(p.chars)}:${p.includeBoundary ? 1 : 0}`;
    }
    class ConvBuffer {
        ir;
        g;
        kernel;
        buffer;
        width;
        height;
        n;
        patternIndices;
        repsWithBoundary;
        constDecls;
        constructor(ir, g, patterns, kernel) {
            this.ir = ir;
            this.g = g;
            this.kernel = kernel;
            const constDecls = this.constDecls = [];
            if (kernel.width === 1) {
                this.width = g.width;
            }
            else {
                const width = ir.constDecl('convBufferWidth', IR.INT_TYPE, IR.OP.addConstant(g.width, kernel.width - 1));
                constDecls.push(width);
                this.width = width.name;
            }
            if (kernel.height === 1) {
                this.height = g.height;
            }
            else {
                const height = ir.constDecl('convBufferHeight', IR.INT_TYPE, IR.OP.addConstant(g.height, kernel.height - 1));
                constDecls.push(height);
                this.height = height.name;
            }
            if (kernel.width === 1 && kernel.height === 1) {
                this.n = g.n;
            }
            else {
                const n = ir.constDecl('convBufferN', IR.INT_TYPE, IR.OP.mult(this.width, this.height));
                constDecls.push(n);
                this.n = n.name;
            }
            // partition the alphabet, so that each alphabet symbol contributes to at most one gBuffer
            const alphabetKey = g.grid.alphabet.key;
            const alphabetPartition = new Partition(alphabetKey.length);
            patterns.forEach(p => alphabetPartition.refine(p.chars));
            const repMap = IDMap.withKey(i => alphabetPartition.getRepresentative(i));
            const mappedReps = makeArray(patterns.length, () => ISet.empty(alphabetPartition.countSubsets()));
            for (let i = 0; i < patterns.length; ++i) {
                ISet.forEach(patterns[i].chars, c => {
                    ISet.add(mappedReps[i], repMap.getOrCreateID(c));
                });
            }
            const numReps = repMap.size();
            repMap.forEach((rep, i) => {
                const chars = alphabetPartition.getSet(rep);
                const pattern = new Pattern(1, 1, alphabetKey, [-2], [chars], true);
                g.matcher.addMatchHandler({ kind: 'convolution', buffer: this, pattern, i });
            });
            // find a minimal subset of reps such that:
            // - no rep in the subset occurs in a pattern `p` for which `p.includesBoundary` is false, and
            // - for each `p` where `p.includeBoundary` is true, `p.chars` contains at most one rep from the subset;
            // - the number of patterns `p` for which `p.includeBoundary` is true and `p.chars` contains a rep is maximised
            // then add those reps to `repsWithBoundary`.
            const allowedReps = ISet.full(alphabetPartition.countSubsets());
            const toCover = [];
            for (let i = 0; i < patterns.length; ++i) {
                const p = patterns[i], m = mappedReps[i];
                if (p.includeBoundary) {
                    toCover.push(m);
                }
                else {
                    ISet.removeAll(allowedReps, m);
                }
            }
            const repsWithBoundary = this.repsWithBoundary = _findCover(ISet.empty(numReps + 1), allowedReps, toCover);
            const anyExtraBoundary = patterns.some((p, i) => p.includeBoundary && ISet.isDisjoint(repsWithBoundary, mappedReps[i]));
            if (anyExtraBoundary) {
                ISet.add(repsWithBoundary, numReps);
            }
            this.buffer = IR.makeMutableArray2D(ir, 'convBuffer', IR.int(numReps + (anyExtraBoundary ? 1 : 0)), this.n, kernel.width * kernel.height);
            const patternIndices = this.patternIndices = new Map();
            patterns.forEach((p, i) => {
                const reps = mappedReps[i];
                const indices = ISet.toArray(reps);
                // sanity check
                if (indices.length === 0) {
                    fail();
                }
                if (p.includeBoundary && ISet.isDisjoint(reps, repsWithBoundary)) {
                    indices.push(numReps);
                }
                patternIndices.set(_key(p), indices);
            });
        }
        declare() {
            const { ir, g, kernel, buffer } = this;
            const { centreX, centreY } = kernel;
            const atX = ir.loopVarDecl('x'), atY = ir.loopVarDecl('y');
            const boundaryValues = kernel.boundaryValues();
            const xLoop = [], yLoop = [], noLoop = [];
            ISet.forEach(this.repsWithBoundary, j => {
                for (let dy = -centreY; dy <= centreY; ++dy) {
                    const y = dy < 0 ? IR.int(-dy - 1)
                        : dy === 0 ? atY.name
                            : IR.OP.minusConstant(g.height, dy);
                    for (let dx = -centreX; dx <= centreX; ++dx) {
                        const value = boundaryValues[(centreX + dx) + (centreY + dy) * kernel.width];
                        if (value === 0) {
                            continue;
                        }
                        const x = dx < 0 ? IR.int(-dx - 1)
                            : dx === 0 ? atX.name
                                : IR.OP.minusConstant(g.width, dx);
                        const arr = dx === 0 ? xLoop
                            : dy === 0 ? yLoop
                                : noLoop;
                        arr.push(buffer.set(IR.int(j), this.index(x, y), '=', IR.int(value)));
                    }
                }
            });
            return IR.initDecl(IR.multiDecl([...this.constDecls, buffer.decl]), IR.seq([
                ...noLoop,
                IR.forRange(atX, IR.int(centreX), IR.OP.minusConstant(g.width, centreX), IR.seq(xLoop)),
                IR.forRange(atY, IR.int(centreY), IR.OP.minusConstant(g.height, centreY), IR.seq(yLoop)),
            ]));
        }
        get(p, at) {
            return (this.patternIndices.get(_key(p)) ?? fail())
                .map(j => this.buffer.get(IR.int(j), at))
                .reduce(IR.OP.add);
        }
        indexRaw(x, y) {
            return IR.OP.add(x, IR.OP.mult(y, this.width));
        }
        index(x, y) {
            const { centreX, centreY } = this.kernel;
            return this.indexRaw(IR.OP.addConstant(x, centreX), IR.OP.addConstant(y, centreY));
        }
        update(i, at, op) {
            // x, y must be simple constants since they are repeated
            const { buffer, kernel } = this;
            const out = [];
            const iExpr = IR.int(i);
            kernel.forEach((dx, dy, value) => {
                const index = this.indexRaw(IR.OP.addConstant(at.x, dx), IR.OP.addConstant(at.y, dy));
                out.push(buffer.set(iExpr, index, op, IR.int(value)));
            });
            return IR.seq(out);
        }
    }
    IR.ConvBuffer = ConvBuffer;
    function _findCover(repsUsed, allowedReps, toCover) {
        // fast path for convolutions without boundary
        if (toCover.length === 0) {
            return repsUsed;
        }
        const initialState = {
            allowedReps,
            toCover,
            covered: 0,
            countUsed: 0,
            repsUsed: repsUsed,
        };
        // depth-first search loop
        let best = initialState;
        const stack = [initialState];
        while (stack.length > 0) {
            const cur = stack.pop();
            if (cur.covered > best.covered || (cur.covered === best.covered && cur.countUsed < best.countUsed)) {
                best = cur;
                // break if this set can't be improved on; fast path for common case where only one `p.includesBoundary` is true
                if (best.covered === toCover.length && best.countUsed === 1) {
                    break;
                }
            }
            const filteredToCover = cur.toCover.filter(s => !ISet.isDisjoint(cur.allowedReps, s));
            const upperBound = cur.covered + filteredToCover.length;
            if (filteredToCover.length === 0 || upperBound < best.covered || (upperBound === best.covered && cur.countUsed >= best.countUsed)) {
                continue;
            }
            const x = ISet.first(ISet.intersection(filteredToCover[0], cur.allowedReps));
            if (x < 0) {
                fail();
            }
            // add child state where x is not used
            const xNotAllowed = ISet.copy(cur.allowedReps);
            ISet.remove(xNotAllowed, x);
            stack.push({
                allowedReps: xNotAllowed,
                toCover: filteredToCover.filter(s => !ISet.isDisjoint(xNotAllowed, s)),
                covered: cur.covered,
                countUsed: cur.countUsed,
                repsUsed: cur.repsUsed,
            });
            // add child state where x is used
            const newRepsUsed = ISet.copy(cur.repsUsed);
            ISet.add(newRepsUsed, x);
            const newAllowedReps = ISet.copy(cur.allowedReps);
            let newCovered = cur.covered;
            const newToCover = filteredToCover.filter(s => {
                if (ISet.has(s, x)) {
                    ISet.removeAll(newAllowedReps, s);
                    ++newCovered;
                    return false;
                }
                else {
                    return true;
                }
            });
            stack.push({
                allowedReps: newAllowedReps,
                toCover: newToCover,
                covered: newCovered,
                countUsed: cur.countUsed + 1,
                repsUsed: newRepsUsed,
            });
        }
        return best.repsUsed;
    }
})(IR || (IR = {}));
///<reference path="info.ts"/>
var IR;
(function (IR) {
    function funcDecl(name, yields, params, returnType, body) {
        let info = body.info.asFuncDecl(name);
        for (const param of params) {
            info = info.asVarDecl(param.name);
        }
        return { kind: 'decl.func', name, yields, params, returnType, body, info };
    }
    IR.funcDecl = funcDecl;
    function initDecl(child, stmt) {
        const info = child.info.then(stmt.info);
        return { kind: 'decl.init', child, stmt, info };
    }
    IR.initDecl = initDecl;
    function multiDecl(decls) {
        const children = [];
        let info = IR.Info.DO_NOTHING;
        for (const decl of decls) {
            if (decl.kind === 'decl.multi') {
                children.push(...decl.children);
            }
            else if (decl.kind !== 'decl.none') {
                children.push(decl);
            }
            info = info.then(decl.info);
        }
        return children.length === 0 ? IR.NO_DECL
            : children.length === 1 ? children[0]
                : { kind: 'decl.multi', children, info };
    }
    IR.multiDecl = multiDecl;
    IR.NO_DECL = { kind: 'decl.none', info: IR.Info.DO_NOTHING };
})(IR || (IR = {}));
///<reference path="factory.ts"/>
var IR;
(function (IR) {
    class Grid {
        ir;
        grid;
        width;
        height;
        n;
        data;
        obj;
        origin;
        buffer;
        lfsrFeedbackTerm;
        originX;
        originY;
        counters = new Map();
        samplers = new Map();
        convBuffers = new Map();
        matcher;
        decls;
        scale = 1;
        constructor(ir, grid, baseWidth, baseHeight) {
            this.ir = ir;
            this.grid = grid;
            const { scaleX, scaleY } = grid;
            const width = ir.constDecl(`grid${grid.id}_width`, IR.INT_TYPE, IR.OP.multConstant(baseWidth, scaleX)), height = ir.constDecl(`grid${grid.id}_height`, IR.INT_TYPE, IR.OP.multConstant(baseHeight, scaleY)), w = width.name, h = height.name, area = ir.constDecl(`grid${grid.id}_n`, IR.INT_TYPE, IR.OP.mult(w, h)), n = area.name, data = IR.makeMutableArray(ir, `grid${grid.id}_data`, n, IR.GRID_DATA_ARRAY_TYPE.domainSize), obj = ir.constDecl(`grid${grid.id}_obj`, IR.GRID_TYPE, IR.libConstructorCall('Grid', [w, h, data.array, IR.str(grid.alphabet.key)])), originX = scaleX % 2 === 0 ? IR.OP.multConstant(baseWidth, scaleX >> 1) : IR.OP.divConstant(w, 2), originY = scaleY % 2 === 0 ? IR.OP.multConstant(baseHeight, scaleY >> 1) : IR.OP.divConstant(h, 2), origin = ir.constDecl(`grid${grid.id}_origin`, IR.INT_TYPE, IR.OP.add(originX, IR.OP.mult(originY, w))), lfsrFeedbackTerm = ir.constDecl(`grid${grid.id}_lfsrFeedbackTerm`, IR.INT_TYPE, IR.libFunctionCall('lfsrFeedbackTerm', [n])), buffer = IR.makeMutableArray(ir, `grid${grid.id}_buffer`, n, IR.GRID_DATA_ARRAY_TYPE.domainSize);
            this.width = width.name;
            this.height = height.name;
            this.n = n;
            this.data = data;
            this.obj = obj.name;
            this.origin = origin.name;
            this.originX = originX;
            this.originY = originY;
            this.lfsrFeedbackTerm = lfsrFeedbackTerm.name;
            this.buffer = buffer;
            // TODO: multiple matchers per grid?
            this.matcher = new IR.Matcher(ir, this);
            this.decls = [
                width,
                height,
                area,
                data.decl,
                obj,
                origin,
                lfsrFeedbackTerm,
                buffer.decl,
            ];
        }
        getScale() {
            return this.grid.scaleX * this.grid.scaleY * this.scale;
        }
        makeCounter(patterns) {
            const { counters, samplers, matcher } = this;
            const key = patterns.map(PatternTree.key).join('\n');
            // TODO: this is order-dependent, a matching sampler might be declared later
            const sampler = samplers.get(key);
            if (sampler !== undefined) {
                return sampler.count;
            }
            return getOrCompute(counters, key, () => {
                const counterDecl = this.ir.varDecl(`grid${this.grid.id}_counter`, IR.INT_TYPE, IR.ZERO);
                for (const pattern of patterns) {
                    matcher.addMatchHandler({ kind: 'counter', pattern, counter: counterDecl.name, weight: IR.ONE });
                }
                // need to set scale to avoid overflowing the counter
                this.scale = Math.max(this.scale, patterns.length);
                return counterDecl;
            }).name;
        }
        makeWeightedCounter(weights, useFloat) {
            const { counters, matcher } = this;
            const key = weights.map(w => `${PatternTree.key(w.pattern)} @ ${w.weight}`).join('\n') + `\nuseFloat=${useFloat}`;
            return getOrCompute(counters, key, () => {
                const counterDecl = this.ir.varDecl(`grid${this.grid.id}_counter`, useFloat ? IR.FLOAT_TYPE : IR.INT_TYPE, useFloat ? IR.FLOAT_ZERO : IR.ZERO);
                let totalWeight = 0;
                for (const { pattern, weight } of weights) {
                    matcher.addMatchHandler({
                        kind: 'counter',
                        pattern,
                        counter: counterDecl.name,
                        weight: useFloat ? IR.float(weight) : IR.int(weight),
                    });
                    totalWeight += weight;
                }
                // need to set scale to avoid overflowing the counter
                if (!useFloat) {
                    this.scale = Math.max(this.scale, totalWeight);
                }
                return counterDecl;
            }).name;
        }
        makeSampler(patterns) {
            const { samplers, matcher } = this;
            const key = patterns.map(PatternTree.key).join('\n');
            return getOrCompute(samplers, key, () => {
                if (patterns.length === 1 && patterns[0].kind === 'top') {
                    const { width, height } = patterns[0];
                    return new IR.TrivialSampler(this.ir, this, width, height);
                }
                else if (patterns.length === 0 || (patterns.length === 1 && patterns[0].kind === 'bottom')) {
                    return new IR.EmptySampler();
                }
                const sampler = new IR.Sampler(this.ir, this, patterns.length);
                for (let i = 0; i < patterns.length; ++i) {
                    const pattern = patterns[i];
                    matcher.addMatchHandler({ kind: 'sampler', pattern, sampler, i });
                }
                this.scale = Math.max(this.scale, patterns.length);
                return sampler;
            });
        }
        makeConvBuffer(kernel) {
            const { convBuffers } = this;
            const key = Convolution.Kernel.key(kernel);
            return getOrCompute(convBuffers, key, () => {
                const patterns = this.grid.convPatterns.filter(p => p.kernel.equals(kernel));
                this.scale = Math.max(this.scale, patterns.length);
                return new IR.ConvBuffer(this.ir, this, patterns, kernel);
            });
        }
        declare() {
            const decls = [
                ...this.decls,
                ...this.counters.values(),
            ];
            for (const sampler of this.samplers.values()) {
                decls.push(sampler.declare());
            }
            for (const buffer of this.convBuffers.values()) {
                decls.push(buffer.declare());
            }
            decls.push(this.matcher.declare());
            return IR.multiDecl(decls);
        }
        attr(attr) {
            switch (attr) {
                case 'area': return this.n;
                case 'width': return this.width;
                case 'height': return this.height;
            }
        }
        /**
         * Used internally for indices which are known to be in-bounds.
         * `x` and `y` must be non-negative.
         */
        index(x, y) {
            return IR.OP.add(x, IR.OP.mult(y, this.width));
        }
        /**
         * Used internally for relative indices which are known to be in-bounds.
         * The variables `dx` and `dy` must be non-negative.
         */
        relativeIndex(at, dx, dy) {
            if (this.grid.periodic) {
                const x = IR.OP.mod(IR.OP.addConstant(at.x, dx), this.width);
                const y = IR.OP.mod(IR.OP.addConstant(at.y, dy), this.height);
                return IR.OP.add(x, IR.OP.mult(y, this.width));
            }
            else {
                return IR.OP.add(IR.OP.addConstant(at.index, dx), IR.OP.multConstant(this.width, dy));
            }
        }
        checkedIndex(x, y) {
            return IR.libMethodCall('Grid', this.grid.periodic ? 'wrapIndex' : 'index', this.obj, [x, y]);
        }
        declareAtIndex(index, child) {
            const ir = this.ir;
            return ir.withConst('at', IR.INT_TYPE, index, index => ir.withConst('x', IR.INT_TYPE, IR.OP.mod(index, this.width), x => ir.withConst('y', IR.INT_TYPE, IR.OP.floordiv(index, this.width), y => child({ index, x, y }))));
        }
        declareAtXY(x, y, child) {
            const ir = this.ir;
            return ir.withConst('x', IR.INT_TYPE, x, x => ir.withConst('y', IR.INT_TYPE, y, y => ir.withConst('at', IR.INT_TYPE, this.index(x, y), index => child({ index, x, y }))));
        }
        write(index, colour, mask) {
            return mask !== undefined ? mask.set(this, index, colour)
                : this.data.set(index, '=', colour);
        }
        update(x, y, w, h) {
            return this.matcher.update(x, y, w, h);
        }
        yield_() {
            return this.yieldRewriteInfo(IR.ZERO, IR.ZERO, this.width, this.height);
        }
        yieldRewriteInfo(x, y, w, h) {
            return IR.yield_(IR.libConstructorCall('RewriteInfo', [this.obj, x, y, w, h]));
        }
    }
    IR.Grid = Grid;
})(IR || (IR = {}));
///<reference path="factory.ts"/>
var IR;
(function (IR) {
    function _maskN(length) {
        return IR.OP.divConstant(IR.OP.addConstant(length, 31), 32);
    }
    class Mask {
        ir;
        arr;
        clearFunc;
        setFunc;
        hasntFunc;
        capacity;
        constructor(ir) {
            this.ir = ir;
            const capacity = this.capacity = ir.deferredExpr('Mask.capacity');
            this.arr = IR.makeMutableArray(ir, 'mask', capacity, IR.INT32_ARRAY_TYPE.domainSize);
            this.clearFunc = ir.func('mask_clear');
            this.setFunc = ir.func('mask_set');
            this.hasntFunc = ir.func('mask_hasnt');
        }
        declare(capacity) {
            const { ir, arr } = this;
            const n = ir.paramDecl('n', IR.INT_TYPE), grid = ir.paramDecl('g', IR.GRID_DATA_ARRAY_TYPE), bitIndex = ir.paramDecl('j', IR.INT_TYPE), setColour = ir.paramDecl('s', IR.BYTE_TYPE);
            const index = IR.OP.divConstant(bitIndex.name, 32);
            const bit = IR.OP.lshift(IR.ONE, IR.OP.modConstant(bitIndex.name, 32));
            return IR.multiDecl([
                IR.replaceInDecl(arr.decl, this.capacity, _maskN(capacity)),
                IR.funcDecl(this.clearFunc, undefined, [n], IR.VOID_TYPE, ir.withConst('len', IR.INT_TYPE, _maskN(n.name), len => ir.forRange('i', IR.ZERO, len, i => arr.set(i, '=', IR.ZERO)))),
                IR.funcDecl(this.setFunc, undefined, [grid, bitIndex, setColour], IR.VOID_TYPE, IR.seq([
                    IR.assign(IR.access(grid.name, bitIndex.name), '=', setColour.name),
                    arr.set(index, '|=', bit),
                ])),
                IR.funcDecl(this.hasntFunc, undefined, [bitIndex], IR.BOOL_TYPE, IR.return_(IR.OP.eq(IR.OP.bitwiseAnd(arr.get(index), bit), IR.ZERO))),
            ]);
        }
        clear(g) {
            return IR.localCallStmt(this.clearFunc, [g.n]);
        }
        set(g, index, colour) {
            return IR.localCallStmt(this.setFunc, [g.data.array, index, colour]);
        }
        hasnt(index) {
            return IR.localCall(this.hasntFunc, [index], true);
        }
        patternFits(g, p, at) {
            return p.constant !== undefined
                ? IR.OP.all(p.constant, (dx, dy) => this.hasnt(g.relativeIndex(at, dx, dy)))
                : IR.libMethodCall('Pattern', 'fitsMask', p.expr, [g.obj, this.arr.array, at.x, at.y]);
        }
    }
    IR.Mask = Mask;
})(IR || (IR = {}));
///<reference path="factory.ts"/>
var IR;
(function (IR) {
    class Matcher {
        ir;
        g;
        matchHandlers = [];
        updateFuncName;
        constructor(ir, g) {
            this.ir = ir;
            this.g = g;
            this.updateFuncName = ir.func('matcherUpdate');
        }
        addMatchHandler(handler) {
            this.matchHandlers.push(handler);
        }
        makeMatchHandler(h, at, bitIndex, acceptID, f) {
            switch (h.kind) {
                case 'sampler':
                    // `I - constant` allows code to be shared among consecutive match handlers
                    return h.sampler.handleMatch(f, IR.OP.addConstant(bitIndex, h.i - acceptID), at);
                case 'counter':
                    return IR.assign(h.counter, f === 'add' ? '+=' : '-=', h.weight);
                case 'convolution':
                    return h.buffer.update(h.i, at, f === 'add' ? '+=' : '-=');
            }
        }
        update(x, y, w, h) {
            return IR.localCallStmt(this.updateFuncName, [x, y, w, h]);
        }
        declare() {
            const { ir, g } = this;
            const gridAlphabetSize = g.grid.alphabet.key.length, dfas = makePatternMatcherDFAs(gridAlphabetSize, this.matchHandlers.map(h => h.pattern)), colsAlphabetSize = dfas.colDFA.alphabetSize, rowDFASize = dfas.rowDFA.size(), colDFASize = dfas.colDFA.size(), numRowPatterns = dfas.rowsAcceptMap.size(), numColPatterns = dfas.colsAcceptMap.size(), rowAcceptSetMasks = dfas.rowsAcceptSetMap.flatMap(entry => [...dfas.rowsAcceptMap.getIDSet(entry)]), colAcceptSetMasks = dfas.colsAcceptSetMap.flatMap(entry => [...dfas.colsAcceptMap.getIDSet(entry)]), rowDFA = IR.makeConstArray2D(ir, 'rowDFA', dfas.rowDFA.toFlatArray(), gridAlphabetSize, rowDFASize), rowAcceptSetIDs = IR.makeConstArray(ir, 'rowAcceptSetIDs', dfas.rowsAcceptSetIDs, dfas.rowsAcceptSetMap.size()), rowAcceptSets = IR.makeConstArray2D(ir, 'rowAcceptSets', rowAcceptSetMasks, (numRowPatterns + 31) >> 5, numRowPatterns <= 31 ? 1 << numRowPatterns : IR.INT32_ARRAY_TYPE.domainSize), rowsToCols = IR.makeConstArray(ir, 'rowsToCols', dfas.rowsToCols, colsAlphabetSize), colDFA = IR.makeConstArray2D(ir, 'colDFA', dfas.colDFA.toFlatArray(), colsAlphabetSize, colDFASize), colAcceptSetIDs = IR.makeConstArray(ir, 'colAcceptSetIDs', dfas.colsAcceptSetIDs, dfas.colsAcceptSetMap.size()), colAcceptSets = IR.makeConstArray2D(ir, 'colAcceptSets', colAcceptSetMasks, (numColPatterns + 31) >> 5, numColPatterns <= 31 ? 1 << numColPatterns : IR.INT32_ARRAY_TYPE.domainSize), rowStates = IR.makeMutableArray(ir, 'rowStates', g.n, rowDFASize), colStates = IR.makeMutableArray(ir, 'colStates', g.n, colDFASize);
            const handlersByPattern = new Map();
            for (const handler of this.matchHandlers) {
                const key = PatternTree.key(handler.pattern);
                getOrCompute(handlersByPattern, key, () => []).push(handler);
            }
            const maskDiff = (acceptSets, t1, t2, index) => {
                const indexExpr = IR.int(index);
                return IR.OP.bitwiseAnd(acceptSets.get(t1, indexExpr), IR.OP.bitwiseNot(acceptSets.get(t2, indexExpr)));
            };
            const startX = ir.paramDecl('startX', IR.INT_TYPE), startY = ir.paramDecl('startY', IR.INT_TYPE), effectiveWidth = ir.paramDecl('w', IR.INT_TYPE), effectiveHeight = ir.paramDecl('h', IR.INT_TYPE), minX = ir.varDecl('minX', IR.INT_TYPE, startX.name), minY = ir.varDecl('minY', IR.INT_TYPE, startY.name), maxX = ir.constDecl('maxX', IR.INT_TYPE, IR.OP.add(startX.name, effectiveWidth.name)), maxY = ir.constDecl('maxY', IR.INT_TYPE, IR.OP.add(startY.name, effectiveHeight.name));
            const makeStateChangeHandlers = (at, t1, t2, mask, acceptSets, acceptMap, f) => {
                const maskSize = (acceptMap.size() + 31) >> 5;
                const out = [];
                for (let index = 0; index < maskSize; ++index) {
                    const bitIndex = ir.constDecl('i', IR.INT_TYPE, IR.OP.countTrailingZeros(mask));
                    const cases = [];
                    const minAcceptID = index << 5, maxAcceptID = Math.min(minAcceptID + 32, acceptMap.size());
                    for (let acceptID = minAcceptID; acceptID < maxAcceptID; ++acceptID) {
                        const key = PatternTree.key(acceptMap.getByID(acceptID));
                        const handlers = handlersByPattern.get(key) ?? fail();
                        cases.push(IR.seq(handlers.map(h => this.makeMatchHandler(h, at, bitIndex.name, acceptID & 31, f))));
                    }
                    out.push(IR.assign(mask, '=', maskDiff(acceptSets, t1, t2, index)), 
                    // special cases for short switches, when there is no need for `countTrailingZeros`
                    cases.length === 1 ? IR.if_(IR.OP.ne(mask, IR.ZERO), IR.replace(cases[0], bitIndex.name, IR.ZERO))
                        : cases.length <= 4 ? IR.seq(cases.map((c, i) => IR.if_(IR.OP.ne(IR.OP.bitwiseAnd(mask, IR.int(1 << i)), IR.ZERO), IR.replace(c, bitIndex.name, IR.int(i)))))
                            : IR.while_(IR.OP.ne(mask, IR.ZERO), IR.seq([
                                IR.withDecls([bitIndex], IR.switch_(bitIndex.name, cases, true)),
                                IR.assign(mask, '&=', IR.OP.minus(mask, IR.ONE)),
                            ])));
                }
                return out;
            };
            const makeUpdateSamplers = (acceptSetIDs, acceptSets, acceptMap, at, state, oldState) => {
                function _update(oldT, t) {
                    return ir.withVar('u', IR.INT_TYPE, undefined, mask => IR.seq([
                        ...makeStateChangeHandlers(at, oldT, t, mask, acceptSets, acceptMap, 'del'),
                        ...makeStateChangeHandlers(at, t, oldT, mask, acceptSets, acceptMap, 'add'),
                    ]));
                }
                const t = acceptSetIDs.get(state), oldT = acceptSetIDs.get(oldState);
                if (t === state && oldT === oldState) {
                    return _update(oldT, t);
                }
                else {
                    return ir.withConst('t', IR.INT_TYPE, t, t => ir.withConst('oldT', IR.INT_TYPE, oldT, oldT => IR.if_(IR.OP.ne(t, oldT), _update(oldT, t))));
                }
            };
            const recomputeRowStates = [
                IR.BLANK_LINE,
                IR.comment('recompute row states'),
                ir.forRange('y', minY.name, maxY.name, y => ir.withVar('s', IR.INT_TYPE, IR.ternary(IR.OP.lt(maxX.name, g.width), rowStates.get(g.index(maxX.name, y)), IR.ZERO), rowState => ir.forRangeReverse('x', IR.ZERO, maxX.name, x => g.declareAtXY(x, y, at => ir.withConst('oldS', IR.INT_TYPE, rowStates.get(at.index), oldRowState => IR.seq([
                    IR.assign(rowState, '=', rowDFA.get(rowState, g.data.get(at.index))),
                    IR.if_(IR.OP.eq(rowState, oldRowState), IR.if_(IR.OP.lt(x, startX.name), IR.BREAK, IR.CONTINUE)),
                    rowStates.set(at.index, '=', rowState),
                    numColPatterns === 0 ? IR.PASS : IR.if_(IR.OP.lt(x, minX.name), IR.assign(minX.name, '=', x)),
                    // must occur last, since it uses `continue`
                    makeUpdateSamplers(rowAcceptSetIDs, rowAcceptSets, dfas.rowsAcceptMap, at, rowState, oldRowState),
                ])))))),
            ];
            const recomputeColStates = numColPatterns === 0 ? [] : [
                IR.BLANK_LINE,
                IR.comment('recompute col states'),
                ir.forRange('x', minX.name, maxX.name, x => ir.withVar('s', IR.INT_TYPE, IR.ternary(IR.OP.lt(maxY.name, g.height), colStates.get(g.index(x, maxY.name)), IR.ZERO), state => ir.forRangeReverse('y', IR.ZERO, maxY.name, y => g.declareAtXY(x, y, at => ir.withConst('oldS', IR.INT_TYPE, colStates.get(at.index), oldState => IR.seq([
                    IR.assign(state, '=', colDFA.get(state, rowsToCols.get(rowStates.get(at.index)))),
                    IR.if_(IR.OP.eq(state, oldState), IR.if_(IR.OP.lt(y, startY.name), IR.BREAK, IR.CONTINUE)),
                    colStates.set(at.index, '=', state),
                    // must occur last, since it uses `continue`
                    makeUpdateSamplers(colAcceptSetIDs, colAcceptSets, dfas.colsAcceptMap, at, state, oldState),
                ])))))),
            ];
            return IR.initDecl(IR.multiDecl([
                rowDFA.decl,
                rowAcceptSetIDs.decl,
                rowAcceptSets.decl,
                rowsToCols.decl,
                colDFA.decl,
                colAcceptSetIDs.decl,
                colAcceptSets.decl,
                rowStates.decl,
                colStates.decl,
                IR.funcDecl(this.updateFuncName, undefined, [startX, startY, effectiveWidth, effectiveHeight], IR.VOID_TYPE, numRowPatterns === 0 && numColPatterns === 0 ? IR.PASS : IR.withDecls([minX, minY, maxX, maxY], IR.seq([
                    ...recomputeRowStates,
                    ...recomputeColStates,
                ]))),
            ]), this.update(IR.ZERO, IR.ZERO, g.width, g.height));
        }
    }
    IR.Matcher = Matcher;
    /**
     * Builds a pair of DFAs which can be used to match 2D patterns. The `rowDFA`
     * recognises pattern rows, and the `colDFA` recognises sequences of pattern
     * rows matched by the `rowDFA`.
     *
     * The DFAs recognise the patterns in reverse order, for convenience so that
     * matches are reported where the patterns start rather than where they end.
     */
    function makePatternMatcherDFAs(alphabetSize, allPatterns) {
        const rowPatterns = allPatterns.filter(p => p.height === 1);
        const colPatterns = allPatterns.filter(p => p.height > 1);
        const rowsAcceptMap = IDMap.ofWithKey(rowPatterns, PatternTree.key);
        const rowsKeepMap = IDMap.withKey(Pattern.key);
        const rowsAcceptOrKeepMap = IDMap.ofWithKey(rowPatterns, PatternTree.key);
        const colsAcceptMap = IDMap.ofWithKey(colPatterns, PatternTree.key);
        const allLeaves = IDMap.ofWithKey(allPatterns.flatMap(PatternTree.getLeaves), PatternTree.key);
        const allLeafRows = IDMap.withKey(Pattern.key);
        const leafRowMap = [];
        allLeaves.forEach(pattern => {
            const rows = Pattern.rowsOf(pattern);
            // determine which rows to keep when mapping from rowDFA to colDFA
            if (rows.length > 1) {
                rowsAcceptOrKeepMap.addAll(rows);
                rowsKeepMap.addAll(rows);
            }
            // associate this pattern with the IDs of its rows in `allLeafRows`
            leafRowMap.push(rows.map(row => allLeafRows.getOrCreateID(row)));
        });
        const rowDFA = Regex.compile(alphabetSize, _makeRegex(allLeafRows.map(pattern => ({
            pattern,
            seq: pattern.kind === 'top'
                ? emptyArray(pattern.width, Regex.WILDCARD)
                : pattern.masks.map(ISet.toArray).map(Regex.letters),
        }))))
            .map(literalRows => {
            const acceptSet = new Set(literalRows.map(Pattern.key));
            return rowsAcceptOrKeepMap.filter(pattern => PatternTree.matches(pattern, p => acceptSet.has(Pattern.key(p))));
        })
            .minimise(PatternTree.key);
        const [rowsAcceptSetIDs, rowsAcceptSetMap] = rowDFA.getAcceptSetMap(rowsAcceptOrKeepMap, rowsAcceptMap.predicate());
        // reduce alphabet size of colDFA by not distinguishing rows which aren't part of taller patterns
        const [rowsToCols, rowsToColsMap] = rowDFA.getAcceptSetMap(rowsAcceptOrKeepMap, rowsKeepMap.predicate());
        const acceptingSetIDs = makeArray(allLeafRows.size(), () => []);
        rowsToColsMap.forEach((rowSet, rowSetID) => {
            for (const row of rowSet) {
                const rowID = allLeafRows.getID(row);
                acceptingSetIDs[rowID].push(rowSetID);
            }
        });
        const colRegexLetters = acceptingSetIDs.map(Regex.letters);
        const colRegexPatterns = [];
        allLeaves.forEach((pattern, i) => {
            // patterns with only one row will be matched by the rowDFA directly
            if (pattern.height > 1) {
                const seq = leafRowMap[i].map(rowID => colRegexLetters[rowID]);
                colRegexPatterns.push({ pattern, seq });
            }
        });
        const colDFA = Regex.compile(rowsToColsMap.size(), _makeRegex(colRegexPatterns))
            .map(literals => {
            const acceptSet = new Set(literals.map(Pattern.key));
            return allPatterns.filter(pattern => PatternTree.matches(pattern, p => acceptSet.has(Pattern.key(p))));
        })
            .minimise(PatternTree.key);
        const [colsAcceptSetIDs, colsAcceptSetMap] = colDFA.getAcceptSetMap(colsAcceptMap);
        return {
            rowDFA,
            rowsAcceptMap,
            rowsAcceptSetIDs,
            rowsAcceptSetMap,
            rowsToCols,
            colDFA,
            colsAcceptMap,
            colsAcceptSetIDs,
            colsAcceptSetMap,
        };
    }
    function _makeRegex(patterns) {
        return Regex.concat([
            Regex.DOT_STAR,
            Regex.union(patterns.map(p => Regex.concat([
                Regex.accept(p.pattern),
                ...p.seq,
            ].reverse()))),
        ]);
    }
})(IR || (IR = {}));
var IR;
(function (IR) {
    function replaceInDecl(decl, from, to) {
        switch (decl.kind) {
            case 'decl.none':
            case 'decl.var.loop':
            case 'decl.var.param':
                return decl;
            case 'decl.func':
                return IR.funcDecl(decl.name, decl.yields, decl.params, decl.returnType, replace(decl.body, from, to));
            case 'decl.init':
                return IR.initDecl(replaceInDecl(decl.child, from, to), replace(decl.stmt, from, to));
            case 'decl.multi':
                return IR.multiDecl(decl.children.map(d => replaceInDecl(d, from, to)));
            case 'decl.var.const': {
                const initialiser = replace(decl.initialiser, from, to);
                return { kind: 'decl.var.const', name: decl.name, type: decl.type, initialiser, info: initialiser.info };
            }
            case 'decl.var.mut': {
                const initialiser = decl.initialiser !== undefined ? replace(decl.initialiser, from, to) : undefined;
                const info = initialiser !== undefined ? initialiser.info : IR.Info.DO_NOTHING;
                return { kind: 'decl.var.mut', name: decl.name, type: decl.type, initialiser, info };
            }
        }
    }
    IR.replaceInDecl = replaceInDecl;
    function replace(node, from, to) {
        if (IR.equals(node, from)) {
            return to;
        }
        switch (node.kind) {
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
                return IR.newArray(replace(node.length, from, to), node.domainSize);
            case 'expr.attr':
                return IR.attr(replace(node.left, from, to), node.attr);
            case 'expr.dict':
                return IR.dict(node.type, node.values.map(v => replace(v, from, to)));
            case 'expr.letin':
                return IR.letIn(replaceInDecl(node.decl, from, to), replace(node.child, from, to));
            case 'expr.op.binary':
                const left = replace(node.left, from, to), right = replace(node.right, from, to);
                switch (node.op) {
                    case 'int_and':
                        return IR.OP.bitwiseAnd(left, right);
                    case 'int_or':
                        return IR.OP.bitwiseOr(left, right);
                    case 'int_xor':
                        return IR.OP.bitwiseXor(left, right);
                    case 'int_lshift':
                        return IR.OP.lshift(left, right);
                    case 'int_rshift':
                        return IR.OP.rshift(left, right);
                    case 'loose_int_plus':
                        return IR.OP.add(left, right);
                    case 'loose_int_minus':
                        return IR.OP.minus(left, right);
                    case 'loose_int_mult':
                        return IR.OP.mult(left, right);
                    case 'loose_int_floordiv':
                        return IR.OP.floordiv(left, right);
                    case 'loose_int_mod':
                        return IR.OP.mod(left, right);
                    default:
                        return IR.binaryOp(node.op, left, right);
                }
            case 'expr.op.call.lib.constructor':
                return IR.libConstructorCall(node.className, node.args.map(v => replace(v, from, to)));
            case 'expr.op.call.lib.function':
                return IR.libFunctionCall(node.name, node.args.map(v => replace(v, from, to)));
            case 'expr.op.call.lib.method':
                return IR.libMethodCall(node.className, node.name, replace(node.obj, from, to), node.args.map(v => replace(v, from, to)));
            case 'expr.op.call.local':
                return IR.localCall(node.name, node.args.map(v => replace(v, from, to)), !node.info.hasSideEffects());
            case 'expr.op.ternary':
                return IR.ternary(replace(node.condition, from, to), replace(node.then, from, to), replace(node.otherwise, from, to));
            case 'expr.op.unary':
                return IR.unaryOp(node.op, replace(node.child, from, to));
            case 'expr.param':
                return IR.param(node.name, replace(node.otherwise, from, to));
            case 'stmt.assign':
                return IR.assign(replace(node.left, from, to), node.op, replace(node.right, from, to));
            case 'stmt.decl':
                return IR.withDecl(replaceInDecl(node.decl, from, to), replace(node.child, from, to));
            case 'stmt.expr':
                return IR.exprStmt(replace(node.expr, from, to));
            case 'stmt.export':
                return IR.exportDecl(replaceInDecl(node.decl, from, to));
            case 'stmt.for.range':
                return IR.forRange(node.index, replace(node.low, from, to), replace(node.high, from, to), replace(node.body, from, to), node.reverse);
            case 'stmt.if':
                return IR.if_(replace(node.condition, from, to), replace(node.then, from, to), node.otherwise && replace(node.otherwise, from, to));
            case 'stmt.log':
                return IR.log(replace(node.expr, from, to));
            case 'stmt.return':
                return IR.return_(node.expr !== undefined ? replace(node.expr, from, to) : undefined);
            case 'stmt.sequence':
                return IR.seq(node.children.map(c => replace(c, from, to)));
            case 'stmt.switch':
                return IR.switchCases(replace(node.expr, from, to), node.cases.map(c => ({ values: c.values, then: replace(c.then, from, to) })), node.exhaustive);
            case 'stmt.while':
                return IR.while_(replace(node.condition, from, to), replace(node.then, from, to));
            case 'stmt.yield':
                return IR.yield_(node.expr !== undefined ? replace(node.expr, from, to) : undefined);
        }
    }
    IR.replace = replace;
})(IR || (IR = {}));
var IR;
(function (IR) {
    class Sampler {
        ir;
        g;
        numPatterns;
        decl;
        count;
        isNotEmpty;
        arr;
        constructor(ir, g, numPatterns) {
            this.ir = ir;
            this.g = g;
            this.numPatterns = numPatterns;
            const capacity = IR.OP.multConstant(g.n, numPatterns);
            this.decl = ir.constDecl(`sampler`, IR.SAMPLER_TYPE, IR.libConstructorCall('Sampler', [capacity]));
            this.count = IR.attr(this.decl.name, 'count');
            this.isNotEmpty = IR.OP.gt(this.count, IR.ZERO);
            this.arr = IR.attr(this.decl.name, 'arr');
        }
        declare() {
            return this.decl;
        }
        handleMatch(f, patternIndex, at) {
            const match = IR.OP.multAddConstant(at.index, this.numPatterns, patternIndex);
            return IR.libMethodCallStmt('Sampler', f, this.decl.name, [match]);
        }
        matchSwitch(match, cases) {
            const k = this.numPatterns;
            return this.g.declareAtIndex(IR.OP.divConstant(match, k), at => IR.switch_(IR.OP.modConstant(match, k), cases(at)));
        }
        sampleWithReplacement(prng, cases) {
            return this.ir.withConst('match', IR.INT_TYPE, IR.access(this.arr, prng.nextInt(this.count)), match => this.matchSwitch(match, cases));
        }
        sampleWithoutReplacement(prng, cases, ifTrue, ifFalse) {
            const ir = this.ir;
            const flag = ir.flag();
            return ir.withVar('count', IR.INT_TYPE, this.count, count => IR.withDecl(flag.decl, IR.seq([
                IR.while_(IR.OP.and(IR.OP.not(flag.check), IR.OP.gt(count, IR.ZERO)), ir.withConst('match', IR.INT_TYPE, IR.libMethodCall('Sampler', 'sample', this.decl.name, [count, prng.name]), match => IR.seq([
                    this.matchSwitch(match, at => cases(at, flag.set, IR.PASS)),
                    IR.assign(count, '-=', IR.ONE),
                ]))),
                IR.if_(flag.check, ifTrue, ifFalse),
            ])));
        }
        copyInto(matchesArray) {
            return IR.libMethodCallStmt('Sampler', 'copyInto', this.decl.name, [matchesArray.array]);
        }
        copyIntoOffset(matches, offset, m, c) {
            return IR.libMethodCallStmt('Sampler', 'copyIntoOffset', this.decl.name, [matches.array, offset, IR.int(m), IR.int(c)]);
        }
        shuffleInto(prng, matches) {
            return IR.libMethodCallStmt('Sampler', 'shuffleInto', this.decl.name, [matches.array, prng.name]);
        }
        shuffleIntoOffset(prng, matches, m, c) {
            return IR.seq([
                IR.libMethodCallStmt('Sampler', 'shuffleIntoOffset', this.decl.name, [matches.array, matches.count, IR.int(m), IR.int(c), prng.name]),
                IR.assign(matches.count, '+=', this.count),
            ]);
        }
        forEach(then) {
            if (this.numPatterns !== 1) {
                fail();
            }
            return this.ir.forRange('i', IR.ZERO, this.count, i => this.g.declareAtIndex(IR.access(this.arr, i), then));
        }
    }
    IR.Sampler = Sampler;
    class TrivialSampler {
        ir;
        g;
        count;
        isNotEmpty = IR.TRUE;
        width;
        height;
        is1x1;
        constructor(ir, g, patternWidth, patternHeight) {
            this.ir = ir;
            this.g = g;
            this.width = IR.OP.minusConstant(g.width, patternWidth - 1);
            this.height = IR.OP.minusConstant(g.height, patternHeight - 1);
            // TODO: optimisations also apply for 1-by-N patterns
            this.is1x1 = patternWidth === 1 && patternHeight === 1;
            this.count = this.is1x1 ? g.n : IR.OP.mult(this.width, this.height);
        }
        declare() {
            return IR.NO_DECL;
        }
        handleMatch(f, patternIndex, at) {
            fail();
        }
        sampleWithReplacement(prng, cases) {
            const then = (at) => {
                const cs = cases(at);
                if (cs.length !== 1) {
                    fail();
                }
                return cs[0];
            };
            return this.is1x1
                ? this.g.declareAtIndex(prng.nextInt(this.count), then)
                : this.g.declareAtXY(prng.nextInt(this.width), prng.nextInt(this.height), then);
        }
        sampleWithoutReplacement(prng, cases, ifTrue, ifFalse) {
            const ir = this.ir;
            const flag = ir.flag();
            return ir.withVar('match', IR.INT_TYPE, IR.OP.add(prng.nextInt(this.count), IR.ONE), match => {
                const nextMatch = IR.while_(IR.TRUE, IR.seq([
                    IR.assign(match, '=', IR.ternary(IR.OP.ne(IR.OP.bitwiseAnd(match, IR.ONE), IR.ZERO), IR.OP.bitwiseXor(IR.OP.rshift(match, IR.ONE), this.g.lfsrFeedbackTerm), IR.OP.rshift(match, IR.ONE))),
                    IR.if_(IR.OP.le(match, this.count), IR.BREAK),
                ]));
                const then = (at) => {
                    const cs = cases(at, IR.seq([flag.set, IR.BREAK]), IR.PASS);
                    if (cs.length !== 1) {
                        fail();
                    }
                    return cs[0];
                };
                const declAt = this.is1x1
                    ? this.g.declareAtIndex(IR.OP.minus(match, IR.ONE), then)
                    : this.g.declareAtXY(IR.OP.mod(match, this.width), IR.OP.floordiv(IR.OP.minus(match, IR.ONE), this.width), then);
                return ir.withVar('count', IR.INT_TYPE, this.count, count => IR.withDecl(flag.decl, IR.seq([
                    IR.while_(IR.OP.and(IR.OP.not(flag.check), IR.OP.gt(count, IR.ZERO)), IR.seq([
                        declAt,
                        IR.assign(count, '-=', IR.ONE),
                        nextMatch,
                    ])),
                    IR.if_(flag.check, ifTrue, ifFalse),
                ])));
            });
        }
        copyInto(matches) {
            return this.copyIntoOffset(matches, IR.ZERO, 1, 0);
        }
        copyIntoOffset(matches, offset, m, c) {
            const { ir } = this;
            return this.is1x1
                ? this.forEach(at => matches.set(IR.OP.add(at.index, offset), '=', IR.OP.multAddConstant(at.index, m, IR.int(c))))
                : ir.withVar('j', IR.INT_TYPE, offset, j => this.forEach(at => IR.seq([
                    matches.set(j, '=', IR.OP.multAddConstant(at.index, m, IR.int(c))),
                    IR.assign(j, '+=', IR.ONE),
                ])));
        }
        shuffleInto(prng, matches) {
            return this.shuffleIntoOffset(prng, matches, 1, 0);
        }
        shuffleIntoOffset(prng, matches, m, c) {
            return this.forEach(at => matches.insertShuffled(prng, IR.OP.multAddConstant(at.index, m, IR.int(c))));
        }
        forEach(then) {
            const { ir, g } = this;
            return this.is1x1
                ? ir.forRange('i', IR.ZERO, this.count, i => g.declareAtIndex(i, then))
                : ir.forRange('y', IR.ZERO, this.height, y => ir.forRange('x', IR.ZERO, this.width, x => g.declareAtXY(x, y, then)));
        }
    }
    IR.TrivialSampler = TrivialSampler;
    class EmptySampler {
        count = IR.ZERO;
        isNotEmpty = IR.FALSE;
        declare() {
            return IR.NO_DECL;
        }
        handleMatch() {
            fail();
        }
        sampleWithReplacement() {
            return IR.exprStmt(IR.unusedExpr(`empty sampler`));
        }
        sampleWithoutReplacement(prng, cases, ifTrue, ifFalse) {
            return IR.exprStmt(IR.unusedExpr(`empty sampler`));
        }
        copyInto(matches) {
            return IR.PASS;
        }
        copyIntoOffset(matches, offset, m, c) {
            return IR.PASS;
        }
        shuffleInto(prng, matches) {
            return matches.declareCount(IR.ZERO);
        }
        shuffleIntoOffset(prng, matches, m, c) {
            return IR.PASS;
        }
        forEach(then) {
            return IR.PASS;
        }
    }
    IR.EmptySampler = EmptySampler;
})(IR || (IR = {}));
///<reference path="factory.ts"/>
var IR;
(function (IR) {
    class TempArray {
        ir;
        capacity;
        decl;
        countDecl;
        array;
        count;
        isNotEmpty;
        incrementCount;
        constructor(ir) {
            this.ir = ir;
            const capacity = this.capacity = ir.deferredExpr('TempArray.capacity');
            const decl = this.decl = ir.mutArrayDecl('matches', capacity, IR.INT32_ARRAY_TYPE.domainSize);
            const countDecl = this.countDecl = ir.varDecl('count', IR.INT_TYPE);
            this.array = decl.name;
            const count = this.count = countDecl.name;
            this.isNotEmpty = IR.OP.gt(count, IR.ZERO);
            this.incrementCount = IR.assign(count, '+=', IR.ONE);
        }
        declare(capacity) {
            return IR.multiDecl([
                IR.replaceInDecl(this.decl, this.capacity, capacity),
                this.countDecl, // TODO
            ]);
        }
        declareCount(initial) {
            return IR.assign(this.count, '=', initial);
        }
        get(index) {
            return IR.access(this.array, index);
        }
        set(index, op, value) {
            return IR.assign(this.get(index), op, value);
        }
        push(value) {
            return IR.seq([
                IR.assign(this.get(this.count), '=', value),
                this.incrementCount,
            ]);
        }
        insertShuffled(prng, value) {
            return this.ir.withConst('j', IR.INT_TYPE, prng.nextInt(IR.OP.add(this.count, IR.ONE)), j => IR.seq([
                IR.assign(this.get(this.count), '=', this.get(j)),
                IR.assign(this.get(j), '=', value),
                this.incrementCount,
            ]));
        }
    }
    IR.TempArray = TempArray;
})(IR || (IR = {}));
var Convolution;
(function (Convolution) {
    class Kernel {
        width;
        height;
        data;
        static key(kernel) {
            return kernel._key ??= `${kernel.width}x${kernel.height}:${kernel.data.join('')}`;
        }
        _key = undefined;
        centreX;
        centreY;
        constructor(width, height, data) {
            this.width = width;
            this.height = height;
            this.data = data;
            this.centreX = width >> 1;
            this.centreY = height >> 1;
        }
        forEach(f) {
            const { width, height, data } = this;
            for (let dy = 0; dy < height; ++dy) {
                for (let dx = 0; dx < width; ++dx) {
                    const value = data[dx + width * dy];
                    if (value !== 0) {
                        f(dx, dy, value);
                    }
                }
            }
        }
        equals(other) {
            if (this === other) {
                return true;
            }
            return this.width === other.width
                && this.height === other.height
                && this.data.every((x, i) => x === other.data[i]);
        }
        total() {
            let t = 0;
            for (const x of this.data) {
                t += x;
            }
            return t;
        }
        boundaryValues() {
            const { width, height, centreX, centreY, data } = this;
            const out = emptyArray(data.length, 0);
            this.forEach((dx, dy) => {
                dx -= centreX;
                dy -= centreY;
                for (let y = 0; y < height; ++y) {
                    const diffY = y - centreY;
                    const yOK = dy < 0 && diffY <= dy
                        || dy > 0 && diffY >= dy;
                    for (let x = 0; x < width; ++x) {
                        const diffX = x - centreX;
                        const xOK = dx < 0 && diffX <= dx
                            || dx > 0 && diffX >= dx;
                        if (xOK || yOK) {
                            ++out[x + width * y];
                        }
                    }
                }
            });
            return out;
        }
    }
    Convolution.Kernel = Kernel;
    Convolution.KERNELS = {
        Moore: new Kernel(3, 3, [1, 1, 1, 1, 0, 1, 1, 1, 1]),
        VonNeumann: new Kernel(3, 3, [0, 1, 0, 1, 0, 1, 0, 1, 0]),
    };
})(Convolution || (Convolution = {}));
/**
 * Data structure representing a partition of the natural numbers from 0 to n - 1,
 * for use in the `DFA.minimise` algorithm. The main operations are `refine` and
 * `pollUnprocessed`.
 *
 * https://en.wikipedia.org/wiki/Partition_refinement#Data_structure
 */
class Partition {
    /**
     * The numbers from 0 to n - 1, ordered so that each subset in the partition
     * is a contiguous range.
     *
     * Invariant: `arr` is a permutation of the numbers from 0 to n - 1
     */
    arr;
    /**
     * Maps the numbers from 0 to n - 1 to their indices in `arr`.
     *
     * Invariant: `arr[i] === x` if and only if `indices[x] === i`
     */
    indices;
    /**
     * The boundaries in `arr` for each subset in the partition.
     *
     * Invariant: `subsets[i].index === i`
     * Invariant: `subsets[i].start < subsets[i].end`
     * Invariant: `subsets[i].start === 0` or there is a unique `j` such that `subsets[i].start === subsets[j].end`
     * Invariant: `subsets[i].end === n` or there is a unique `j` such that `subsets[i].end === subsets[j].start`
     */
    subsets = [];
    /**
     * The subsets which have yet to be processed by the `DFA.minimise` algorithm,
     * plus possibly some empty subsets which do not need to be processed.
     *
     * Invariant: if `subset.isUnprocessed` then `unprocessed.includes(subset)`
     * Invariant: if `unprocessed.includes(subset)` and not `subset.isUnprocessed`, then `subset.start === subset.end`
     */
    unprocessed = [];
    /**
     * Maps each number from 0 to n - 1 to the subset it is a member of.
     *
     * Invariant: `map[x].start <= indices[x] && indices[x] < map[x].end`
     */
    map;
    /**
     * Constructs a new instance representing a partition of the numbers from
     * 0 to n - 1. The partition initially contains only a single subset (the
     * whole range).
     */
    constructor(n) {
        this.arr = makeArray(n, i => i);
        this.indices = makeArray(n, i => i);
        const initialSubset = this.makeSubset(0, n, true);
        this.map = emptyArray(n, initialSubset);
    }
    /**
     * Returns the number of subsets in this partition.
     */
    countSubsets() {
        return this.subsets.length;
    }
    makeSubset(start, end, isUnprocessed) {
        const { subsets } = this;
        const subset = {
            index: subsets.length,
            start,
            end,
            isUnprocessed,
            sibling: undefined,
        };
        subsets.push(subset);
        if (isUnprocessed) {
            this.unprocessed.push(subset);
        }
        return subset;
    }
    deleteSubset(subset) {
        // sanity check
        if (subset.start !== subset.end) {
            fail();
        }
        const { index } = subset;
        const removed = this.subsets.pop();
        if (removed.index !== index) {
            this.subsets[removed.index = index] = removed;
        }
        subset.isUnprocessed = false;
    }
    /**
     * Returns a subset which needs to be processed, and marks it as processed.
     * The elements are in no particular order.
     *
     * If no subsets remain to be processed, `undefined` is returned.
     */
    pollUnprocessed() {
        const { unprocessed } = this;
        while (unprocessed.length > 0) {
            const subset = unprocessed.pop();
            // have to check `isUnprocessed` because deleted subsets may still be in the stack
            if (subset.isUnprocessed) {
                subset.isUnprocessed = false;
                return this.arr.slice(subset.start, subset.end);
            }
        }
        return undefined;
    }
    /**
     * Returns a representative element from the subset in the partition which
     * contains the number `x`.
     */
    getRepresentative(x) {
        return this.arr[this.map[x].start];
    }
    /**
     * Calls the provided callback function with a representative element
     * from each subset in the partition.
     */
    forEachRepresentative(f) {
        const { arr } = this;
        for (const subset of this.subsets) {
            f(arr[subset.start]);
        }
    }
    getSet(x) {
        const { arr } = this;
        const { start, end } = this.map[x];
        const out = ISet.empty(arr.length);
        for (let i = start; i < end; ++i) {
            ISet.add(out, arr[i]);
        }
        return out;
    }
    /**
     * Refines this partition by splitting any subsets which partly intersect
     * with the given set. If an unprocessed subset is split, both parts are
     * marked unprocessed; otherwise, the smaller part is marked.
     */
    refine(set) {
        const { unprocessed, map } = this;
        const splits = [];
        ISet.forEach(set, x => {
            const subset = map[x];
            if (subset.sibling === undefined) {
                splits.push(subset);
                subset.sibling = this.makeSubset(subset.end, subset.end, subset.isUnprocessed);
            }
            this.moveToSibling(x, subset);
        });
        for (const subset of splits) {
            if (subset.start === subset.end) {
                this.deleteSubset(subset);
            }
            else if (!subset.isUnprocessed) {
                const sibling = subset.sibling;
                const min = subset.end - subset.start <= sibling.end - sibling.start ? subset : sibling;
                min.isUnprocessed = true;
                unprocessed.push(min);
            }
            subset.sibling = undefined;
        }
    }
    /**
     * Moves the element x from `subset` to `subset.sibling`, in O(1) time. The
     * sibling appears immediately afterwards in `arr`, so `x` is swapped with
     * the last member of `subset` and then the boundary is adjusted.
     */
    moveToSibling(x, subset) {
        const { arr, map, indices } = this;
        const sibling = subset.sibling;
        const i = indices[x];
        const j = subset.end = --sibling.start;
        const y = arr[j];
        arr[i] = y;
        indices[y] = i;
        arr[j] = x;
        indices[x] = j;
        map[x] = sibling;
    }
}
///<reference path="partition.ts"/>
var Regex;
(function (Regex) {
    function letters(letterIDs) {
        return { kind: 0 /* Kind.LETTERS */, letterIDs };
    }
    Regex.letters = letters;
    Regex.WILDCARD = { kind: 1 /* Kind.WILDCARD */ };
    function concat(children) {
        return { kind: 2 /* Kind.CONCAT */, children };
    }
    Regex.concat = concat;
    function union(children) {
        return { kind: 3 /* Kind.UNION */, children };
    }
    Regex.union = union;
    function kleeneStar(child) {
        return { kind: 4 /* Kind.KLEENESTAR */, child };
    }
    Regex.kleeneStar = kleeneStar;
    function accept(accept) {
        return { kind: 5 /* Kind.ACCEPT */, accept };
    }
    Regex.accept = accept;
    Regex.DOT_STAR = kleeneStar(Regex.WILDCARD);
    function compile(alphabetSize, regex) {
        return new NFA(alphabetSize, regex).toDFA();
    }
    Regex.compile = compile;
})(Regex || (Regex = {}));
class NFA {
    alphabetSize;
    nodes = [];
    startID;
    constructor(alphabetSize, regex) {
        this.alphabetSize = alphabetSize;
        this.startID = this.makeFromRegex(regex, this.makeNode([]));
        //console.log(`NFA with ${this.nodes.length} nodes on alphabet of size ${alphabetSize}`);
    }
    makeNode(epsilons, letters = [], nextID = -1) {
        const { nodes } = this;
        const id = nodes.length;
        nodes.push({ epsilons, letters, nextID, acceptSet: [], epsilonClosure: undefined });
        return id;
    }
    makeFromRegex(regex, outID) {
        // https://en.wikipedia.org/wiki/Thompson's_construction
        switch (regex.kind) {
            case 0 /* Regex.Kind.LETTERS */: {
                return this.makeNode([], regex.letterIDs, outID);
            }
            case 1 /* Regex.Kind.WILDCARD */: {
                return this.makeNode([], makeArray(this.alphabetSize, i => i), outID);
            }
            case 2 /* Regex.Kind.CONCAT */: {
                const { children } = regex;
                for (let i = children.length - 1; i >= 0; --i) {
                    outID = this.makeFromRegex(children[i], outID);
                }
                return outID;
            }
            case 3 /* Regex.Kind.UNION */: {
                const epsilons = regex.children.map(child => this.makeFromRegex(child, this.makeNode([outID])));
                return this.makeNode(epsilons);
            }
            case 4 /* Regex.Kind.KLEENESTAR */: {
                const childOutID = this.makeNode([outID]);
                const childInID = this.makeFromRegex(regex.child, childOutID);
                this.nodes[childOutID].epsilons.push(childInID);
                return this.makeNode([childInID, outID]);
            }
            case 5 /* Regex.Kind.ACCEPT */: {
                const node = this.nodes[outID];
                node.acceptSet.push(regex.accept);
                return outID;
            }
        }
    }
    getEpsilonClosure(nodeID) {
        const { nodes } = this;
        // epsilon closure, by depth-first search
        // use ISet instead of Set<number> or bigint for the state, for performance
        const cached = nodes[nodeID].epsilonClosure;
        if (cached !== undefined) {
            return cached;
        }
        const out = ISet.empty(nodes.length);
        const stack = [nodeID];
        while (stack.length > 0) {
            const id = stack.pop();
            if (ISet.has(out, id)) {
                continue;
            }
            ISet.add(out, id);
            for (const eps of nodes[id].epsilons) {
                if (!ISet.has(out, eps)) {
                    stack.push(eps);
                }
            }
        }
        nodes[nodeID].epsilonClosure = out;
        return out;
    }
    toDFA() {
        // https://en.wikipedia.org/wiki/Powerset_construction
        const { alphabetSize, nodes } = this;
        const nfaStates = IDMap.withKey(ISet.key);
        const dfaNodes = [];
        const cache = new Map();
        const getNodeID = (nfaState) => getOrCompute(cache, ISet.key(nfaState), () => {
            for (let nfaNodeID of ISet.toArray(nfaState)) {
                while (true) {
                    const epsilons = nodes[nfaNodeID].epsilons;
                    if (epsilons.length === 0) {
                        break;
                    }
                    else if (epsilons.length === 1) {
                        const nfaNodeID = epsilons[0];
                        if (ISet.has(nfaState, nfaNodeID)) {
                            break;
                        }
                        ISet.add(nfaState, nfaNodeID);
                    }
                    else {
                        ISet.addAll(nfaState, this.getEpsilonClosure(nfaNodeID));
                        break;
                    }
                }
            }
            return nfaStates.getOrCreateID(nfaState);
        });
        const startID = getNodeID(ISet.of(nodes.length, [this.startID]));
        // sanity check
        if (startID !== 0) {
            fail();
        }
        // this loop iterates over `nfaStates`, while adding to it via `getNodeID`
        for (let nfaStateID = 0; nfaStateID < nfaStates.size(); ++nfaStateID) {
            const transitionStates = makeArray(alphabetSize, () => ISet.empty(nodes.length));
            const accepts = [];
            ISet.forEach(nfaStates.getByID(nfaStateID), nfaNodeID => {
                const nfaNode = nodes[nfaNodeID];
                for (const letterID of nfaNode.letters) {
                    ISet.add(transitionStates[letterID], nfaNode.nextID);
                }
                accepts.push(...nfaNode.acceptSet);
            });
            dfaNodes.push({
                transitions: transitionStates.map(getNodeID),
                accepts,
            });
        }
        return new DFA(alphabetSize, dfaNodes);
    }
}
class DFA {
    alphabetSize;
    nodes;
    constructor(alphabetSize, nodes) {
        this.alphabetSize = alphabetSize;
        this.nodes = nodes;
        //console.log(`DFA with ${nodes.length} nodes on alphabet of size ${alphabetSize} and ${acceptSetMap.size()} accept sets`);
    }
    /**
     * Returns the number of distinct states of this DFA.
     */
    size() {
        return this.nodes.length;
    }
    toFlatArray() {
        return this.nodes.flatMap(node => node.transitions);
    }
    getAcceptSetMap(acceptMap, predicate) {
        const { nodes } = this;
        const acceptSets = nodes.map(predicate !== undefined
            ? node => node.accepts.filter(predicate)
            : node => node.accepts);
        const acceptSetMap = IDMap.ofWithKey(acceptSets, set => ISet.key(acceptMap.getIDSet(set)));
        return [
            acceptSetMap.getIDs(acceptSets),
            acceptSetMap,
        ];
    }
    /**
     * Returns an iterable mapping each accept to the set of node IDs which
     * accept it.
     */
    computeAcceptingStates(keyFunc) {
        const { nodes } = this;
        const n = nodes.length;
        const map = new Map();
        for (let id = 0; id < n; ++id) {
            for (const accept of nodes[id].accepts) {
                const key = keyFunc(accept);
                const set = getOrCompute(map, key, () => ISet.empty(n));
                ISet.add(set, id);
            }
        }
        return map.values();
    }
    /**
     * Returns an equivalent DFA with the minimum possible number of states.
     */
    minimise(keyFunc) {
        // https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft's_algorithm
        const { alphabetSize, nodes } = this;
        const n = nodes.length;
        const inverseTransitions = emptyArray(alphabetSize * n, undefined);
        for (let id = 0; id < n; ++id) {
            const { transitions } = nodes[id];
            for (let c = 0; c < alphabetSize; ++c) {
                (inverseTransitions[c * n + transitions[c]] ??= []).push(id);
            }
        }
        const partition = new Partition(n);
        for (const d of this.computeAcceptingStates(keyFunc)) {
            partition.refine(d);
        }
        // pre-allocate
        const refinement = ISet.empty(n);
        while (true) {
            const a = partition.pollUnprocessed();
            if (a === undefined) {
                break;
            }
            for (let c = 0; c < alphabetSize; ++c) {
                ISet.clear(refinement);
                for (const id of a) {
                    const arr = inverseTransitions[c * n + id];
                    if (arr !== undefined) {
                        // `ISet.addAll` would be faster for dense sets, but most of these sets are small
                        for (const x of arr) {
                            ISet.add(refinement, x);
                        }
                    }
                }
                partition.refine(refinement);
                // shortcut if the DFA cannot be minimised
                if (partition.countSubsets() === n) {
                    return this;
                }
            }
        }
        const reps = IDMap.withKey(id => partition.getRepresentative(id));
        // ensure id(rep(0)) === 0, so that 0 is still the starting state
        reps.getOrCreateID(0);
        partition.forEachRepresentative(x => reps.getOrCreateID(x));
        const repNodes = reps.map(rep => {
            const { transitions, accepts } = this.nodes[rep];
            return {
                transitions: reps.getIDs(transitions),
                accepts,
            };
        });
        return new DFA(alphabetSize, repNodes);
    }
    /**
     * Returns a new DFA, with the accept sets replaced using the function `f`.
     */
    map(f) {
        const mappedNodes = this.nodes.map(node => {
            const { transitions, accepts } = node;
            return {
                transitions,
                accepts: f(accepts),
            };
        });
        return new DFA(this.alphabetSize, mappedNodes);
    }
}
class Diagnostics {
    static MAX_ERRORS = 100;
    errors = [];
    throwIfAnyErrors() {
        if (this.errors.length > 0) {
            throw this;
        }
    }
    error(prefix, msg, pos) {
        if (pos !== undefined) {
            msg += ` at line ${pos.line}, col ${pos.col}`;
        }
        this.errors.push(`${prefix}: ${msg}`);
        if (this.errors.length >= Diagnostics.MAX_ERRORS) {
            throw this;
        }
    }
    configError(msg) {
        this.error('Configuration error', msg);
    }
    syntaxError(msg, pos) {
        this.error('Syntax error', msg, pos);
    }
    compilationError(msg, pos) {
        this.error('Compilation error', msg, pos);
    }
    typeError(msg, pos) {
        this.error('Type error', msg, pos);
    }
}
/**
 * Assigns unique, incremental IDs to a set of values.
 */
class IDMap {
    keyFunc;
    static IDENTITY = (x) => x;
    static empty() {
        return new IDMap(IDMap.IDENTITY);
    }
    static withKey(keyFunc) {
        return new IDMap(keyFunc);
    }
    /**
     * Creates a new IDMap with the distinct elements from `iterable`, with IDs
     * in order of first occurrence.
     */
    static of(iterable) {
        return IDMap.ofWithKey(iterable, IDMap.IDENTITY);
    }
    static ofWithKey(iterable, keyFunc) {
        const map = new IDMap(keyFunc);
        map.addAll(iterable);
        return map;
    }
    /**
     * Returns a new array of the distinct elements from `iterable`, in order
     * of first occurrence.
     */
    static distinct(iterable) {
        return IDMap.of(iterable).arr;
    }
    /**
     * Returns a new array of the elements from `iterable`, deduplicated using
     * the given key function, in order of first occurrence. If multiple values
     * have the same key, only the first is included.
     */
    static distinctByKey(iterable, keyFunc) {
        return IDMap.ofWithKey(iterable, keyFunc).arr;
    }
    /**
     * The distinct elements in this map, in insertion order.
     */
    arr = [];
    /**
     * Maps elements to their indices in `arr`.
     *
     * Invariant: `ids.get(keyFunc(x)) === i` if and only if `arr[i] === x`
     */
    ids = new Map();
    constructor(keyFunc) {
        this.keyFunc = keyFunc;
    }
    /**
     * Returns the number of elements in the map.
     */
    size() {
        return this.arr.length;
    }
    /**
     * Adds an element to the map if it is not already present, and returns the
     * element's ID, in O(1) time.
     */
    getOrCreateID(x) {
        const key = this.keyFunc(x);
        return getOrCompute(this.ids, key, () => {
            const id = this.arr.length;
            this.arr.push(x);
            return id;
        });
    }
    addAll(xs) {
        for (const x of xs) {
            this.getOrCreateID(x);
        }
    }
    /**
     * Indicates whether the given element is associated with an ID, in O(1)
     * time.
     */
    has(x) {
        return this.ids.has(this.keyFunc(x));
    }
    /**
     * Returns a type guard function which tests for membership of this map.
     */
    predicate() {
        return this.has.bind(this);
    }
    /**
     * Returns the ID of the given element, in O(1) time. An error is thrown if
     * the element is not associated with an ID.
     */
    getID(x) {
        return this.ids.get(this.keyFunc(x)) ?? fail();
    }
    getIDs(xs) {
        return xs.map(x => this.getID(x));
    }
    getIDSet(xs) {
        const set = ISet.empty(this.arr.length);
        for (const x of xs) {
            ISet.add(set, this.getID(x));
        }
        return set;
    }
    /**
     * Returns the ID of the given element, or -1 if the given element is not
     * associated with an ID, in O(1) time.
     */
    getIDOrDefault(x) {
        return this.ids.get(this.keyFunc(x)) ?? -1;
    }
    /**
     * Returns the element associated with the given ID, in O(1) time. An error
     * is thrown if there is no element with the given ID.
     */
    getByID(id) {
        return id >= 0 && id < this.arr.length ? this.arr[id] : fail();
    }
    forEach(f) {
        this.arr.forEach(f);
    }
    filter(f) {
        return this.arr.filter(f);
    }
    map(f) {
        return this.arr.map(f);
    }
    flatMap(f) {
        return this.arr.flatMap(f);
    }
}
/**
 * Helper functions for using a typed array as a set of natural numbers.
 *
 * Aggregate operations `addAll`, `toArray` and `forEach` are O(N), where N is
 * the domain size; therefore they must not be used in the pattern matching loop.
 */
var ISet;
(function (ISet) {
    /**
     * Creates an empty set, which can contain numbers `0 <= x < domainSize`.
     */
    function empty(domainSize) {
        return new Uint32Array((domainSize + 31) >> 5);
    }
    ISet.empty = empty;
    /**
     * Creates a set containing the whole domain `0 <= x < domainSize`.
     */
    function full(domainSize) {
        const set = empty(domainSize);
        set.fill(~0);
        if ((domainSize & 31) !== 0) {
            set[set.length - 1] = (1 << (domainSize & 31)) - 1;
        }
        return set;
    }
    ISet.full = full;
    /**
     * Creates a set from an iterable of natural numbers, all of which must be
     * less than `domainSize`.
     */
    function of(domainSize, xs) {
        const set = empty(domainSize);
        for (const x of xs) {
            add(set, x);
        }
        return set;
    }
    ISet.of = of;
    /**
     * Returns a new copy of the given set.
     */
    function copy(set) {
        return new Uint32Array(set);
    }
    ISet.copy = copy;
    /**
     * Indicates whether `set` contains the element `x`, in O(1) time.
     */
    function has(set, x) {
        return (set[x >> 5] & (1 << (x & 31))) !== 0;
    }
    ISet.has = has;
    /**
     * Returns the size of the set, in O(N) time.
     */
    function size(set) {
        let count = 0;
        for (let x of set) {
            while (x !== 0) {
                x &= x - 1;
                ++count;
            }
        }
        return count;
    }
    ISet.size = size;
    /**
     * Returns some element of the set, or -1 if the set is empty, in O(N) time.
     */
    function first(set) {
        for (let i = 0; i < set.length; ++i) {
            const x = set[i];
            if (x !== 0) {
                return (i << 5) | (31 - Math.clz32(x));
            }
        }
        return -1;
    }
    ISet.first = first;
    /**
     * Adds the element `x` to the set if it is not already present, in O(1) time.
     */
    function add(set, x) {
        set[x >> 5] |= 1 << (x & 31);
    }
    ISet.add = add;
    /**
     * Removes the element `x` to the set if it is present, in O(1) time.
     */
    function remove(set, x) {
        set[x >> 5] &= ~(1 << (x & 31));
    }
    ISet.remove = remove;
    /**
     * Adds all the members of the set `b` to the set `a`, in O(N) time.
     */
    function addAll(a, b) {
        if (a.length < b.length) {
            fail();
        }
        for (let i = 0; i < b.length; ++i) {
            a[i] |= b[i];
        }
    }
    ISet.addAll = addAll;
    /**
     * Removes the members from the set `a` which are not in `b`, in O(N) time.
     */
    function retainAll(a, b) {
        if (a.length < b.length) {
            fail();
        }
        for (let i = 0; i < b.length; ++i) {
            a[i] &= b[i];
        }
    }
    ISet.retainAll = retainAll;
    /**
     * Removes all the members of the set `b` to the set `a`, in O(N) time.
     */
    function removeAll(a, b) {
        if (a.length < b.length) {
            fail();
        }
        for (let i = 0; i < b.length; ++i) {
            a[i] &= ~b[i];
        }
    }
    ISet.removeAll = removeAll;
    /**
     * Removes all elements from the set, in O(N) time.
     */
    function clear(a) {
        for (let i = 0; i < a.length; ++i) {
            a[i] = 0;
        }
    }
    ISet.clear = clear;
    /**
     * Returns a new set which is the union of `a` and `b`, in O(N) time.
     */
    function union(a, b) {
        const out = new Uint32Array(a);
        addAll(out, b);
        return out;
    }
    ISet.union = union;
    /**
     * Returns a new set which is the intersection of `a` and `b`, in O(N) time.
     */
    function intersection(a, b) {
        const out = new Uint32Array(a);
        retainAll(out, b);
        return out;
    }
    ISet.intersection = intersection;
    /**
     * Returns a new set which is the difference of `a` and `b`, in O(N) time.
     */
    function difference(a, b) {
        const out = new Uint32Array(a);
        removeAll(out, b);
        return out;
    }
    ISet.difference = difference;
    /**
     * Determines whether the two sets are disjoint (i.e. they have no elements
     * in common).
     */
    function isDisjoint(a, b) {
        if (a.length < b.length) {
            fail();
        }
        for (let i = 0; i < b.length; ++i) {
            if ((a[i] & b[i]) !== 0) {
                return false;
            }
        }
        return true;
    }
    ISet.isDisjoint = isDisjoint;
    /**
     * Determines whether `a` is a subset of `b`, in O(N) time.
     */
    function isSubset(a, b) {
        if (a.length > b.length) {
            fail();
        }
        for (let i = 0; i < a.length; ++i) {
            if ((a[i] & ~b[i]) !== 0) {
                return false;
            }
        }
        return true;
    }
    ISet.isSubset = isSubset;
    /**
     * Converts an unordered array to a primitive type, suitable for use as a
     * Map key, in O(N) time.
     */
    function arrayToKey(xs) {
        if (xs.length === 0) {
            return 0;
        }
        const domainSize = Math.max(...xs) + 1;
        return key(of(domainSize, xs));
    }
    ISet.arrayToKey = arrayToKey;
    /**
     * Converts a set to a primitive type, suitable for use as a Map key, in
     * O(N) time.
     */
    function key(set) {
        // this function is part of the hot loop in `NFA.toDFA`, so it needs to be fast
        switch (set.length) {
            case 0:
                return 0;
            case 1:
                return set[0];
            case 2:
                return BigInt(set[0]) | BigInt(set[1]) << 32n;
            case 3:
                return BigInt(set[0]) | BigInt(set[1]) << 32n | BigInt(set[2]) << 64n;
            case 4:
                return (BigInt(set[0]) | BigInt(set[1]) << 32n)
                    | (BigInt(set[2]) | BigInt(set[3]) << 32n) << 64n;
            default:
                // O(N) time
                return String.fromCharCode(...new Uint16Array(set.buffer));
        }
    }
    ISet.key = key;
    /**
     * Sentinel value used to halt the `_forEach` function.
     */
    const STOP_ITERATION = Symbol();
    function _forEach(set, f) {
        for (let i = 0; i < set.length; ++i) {
            const x = i << 5;
            let setPart = set[i];
            while (setPart !== 0) {
                const lowBit = setPart & -setPart;
                const dx = 31 - Math.clz32(lowBit);
                // 'x ^ dx' is equivalent to `x + dx` here
                if (f(x ^ dx) === STOP_ITERATION) {
                    return false;
                }
                // clear this bit
                setPart ^= lowBit;
            }
        }
        return true;
    }
    /**
     * Calls the function `f` for each element of the set, in order.
     */
    ISet.forEach = _forEach;
    /**
     * Returns a new array of the natural numbers in the given set, in order.
     */
    function toArray(set) {
        const arr = [];
        _forEach(set, x => arr.push(x));
        return arr;
    }
    ISet.toArray = toArray;
    /**
     * Returns a new array by mapping the natural numbers in the given set,
     * in order.
     */
    function map(set, f) {
        const arr = [];
        _forEach(set, x => arr.push(f(x)));
        return arr;
    }
    ISet.map = map;
    /**
     * Determines whether the predicate is true for every element of the set.
     */
    function every(set, f) {
        return _forEach(set, x => !f(x) ? STOP_ITERATION : undefined);
    }
    ISet.every = every;
    /**
     * Determines whether the predicate is true for some element of the set.
     */
    function some(set, f) {
        return !_forEach(set, x => f(x) ? STOP_ITERATION : undefined);
    }
    ISet.some = some;
})(ISet || (ISet = {}));
function fail(...args) {
    console.log(args.length > 0 ? 'Assertion failed with arguments:' : 'Assertion failed', ...args);
    throw new Error('Internal compiler error: assertion failed');
}
function objHasKey(obj, key) {
    return Object.prototype.hasOwnProperty.call(obj, key);
}
/**
 * Creates an empty array of length `n`, filled with the given value.
 */
function emptyArray(n, value) {
    return makeArray(n, () => value);
}
/**
 * Creates an array of length `n`, initialised using the given callback function.
 */
function makeArray(n, f) {
    // equivalent to `Array(n).map((_, i) => f(i))`, but guarantees an array without holes, which may be more performant to use
    const arr = [];
    for (let i = 0; i < n; ++i) {
        arr.push(f(i));
    }
    return arr;
}
function withNextID(arr, obj) {
    const t = obj;
    t.id = arr.length;
    arr.push(t);
    return t;
}
function getOrCompute(map, key, f) {
    let v = map.get(key);
    if (v === undefined) {
        map.set(key, v = f());
    }
    return v;
}
function quoteJoin(hints, delimiter = ', ') {
    return hints.map(s => `'${s}'`).join(delimiter);
}
///<reference path="../runtime/mjr.ts"/>
var PatternTree;
(function (PatternTree) {
    /**
     * Returns the intersection of two patterns, or `undefined` if they are
     * mutually exclusive.
     */
    function _intersect(left, right) {
        const { alphabetKey, masks: leftMasks } = left;
        const pattern = [];
        const masks = [];
        for (let i = 0; i < leftMasks.length; ++i) {
            const mask = ISet.intersection(leftMasks[i], right.masks[i]);
            const maskSize = ISet.size(mask);
            if (maskSize === 0) {
                return undefined;
            }
            pattern.push(maskSize === 1 ? ISet.first(mask)
                : maskSize === alphabetKey.length ? 255 /* PatternValue.WILDCARD */
                    : 254 /* PatternValue.UNION */);
            masks.push(mask);
        }
        return new Pattern(left.width, left.height, alphabetKey, pattern, masks, true);
    }
    /**
     * Conservatively estimates of whether, if `left` matches, `right` must too.
     */
    function _entails(left, right) {
        switch (left.kind) {
            case 'leaf':
            case 'top':
                return left.masks.every((mask, i) => ISet.isSubset(mask, right.masks[i]));
            case 'bottom':
                return true;
            case 'and':
                return _entails(left.left, right) || _entails(left.right, right);
            case 'or':
                return _entails(left.left, right) && _entails(left.right, right);
            case 'not':
                return false;
        }
    }
    function top(width, height, alphabetKey) {
        const pattern = emptyArray(width * height, 255 /* PatternValue.WILDCARD */);
        const masks = emptyArray(width * height, ISet.full(alphabetKey.length));
        return new Pattern(width, height, alphabetKey, pattern, masks, false);
    }
    PatternTree.top = top;
    function bottom(width, height) {
        return { kind: 'bottom', width, height, _key: undefined };
    }
    PatternTree.bottom = bottom;
    function and(left, right) {
        const { width, height } = left;
        if (right.width !== width || right.height !== height) {
            fail();
        }
        if (left.kind === 'bottom' || right.kind === 'top') {
            return left;
        }
        else if (left.kind === 'top' || right.kind === 'bottom') {
            return right;
        }
        else if (left.kind === 'leaf' && right.kind === 'leaf') {
            return _intersect(left, right) ?? bottom(width, height);
        }
        else if (left.kind === 'leaf' && right.kind === 'not' && _intersect(left, right.child) === undefined) {
            return left;
        }
        else if (right.kind === 'leaf' && left.kind === 'not' && _intersect(left.child, right) === undefined) {
            return right;
        }
        else if ((left.kind === 'not' && _entails(right, left.child)) || (right.kind === 'not' && _entails(left, right.child))) {
            return bottom(width, height);
        }
        return { kind: 'and', left, right, width, height, _key: undefined };
    }
    PatternTree.and = and;
    function or(left, right) {
        const { width, height } = left;
        if (right.width !== width || right.height !== height) {
            fail();
        }
        if (left.kind === 'top' || right.kind === 'bottom') {
            return left;
        }
        else if (left.kind === 'bottom' || right.kind === 'top') {
            return right;
        }
        else if (left.kind === 'leaf' && right.kind === 'leaf' && width === 1 && height === 1) {
            const { alphabetKey } = left;
            const mask = ISet.union(left.masks[0], right.masks[0]);
            const pattern = [ISet.size(mask) === alphabetKey.length ? 255 /* PatternValue.WILDCARD */ : 254 /* PatternValue.UNION */];
            return new Pattern(1, 1, alphabetKey, pattern, [mask], true);
        }
        return { kind: 'or', left, right, width, height, _key: undefined };
    }
    PatternTree.or = or;
    function not(child, alphabetKey) {
        const { width, height } = child;
        switch (child.kind) {
            case 'top':
                return bottom(width, height);
            case 'bottom':
                return top(width, height, alphabetKey);
            case 'and':
                return or(not(child.left, alphabetKey), not(child.right, alphabetKey));
            case 'or':
                return and(not(child.left, alphabetKey), not(child.right, alphabetKey));
            case 'not':
                return child.child;
        }
        // leaf
        if (width === 1 && height === 1) {
            const { alphabetKey } = child;
            const mask = ISet.full(alphabetKey.length);
            ISet.removeAll(mask, child.masks[0]);
            return new Pattern(width, height, alphabetKey, [-2], [mask], true);
        }
        return { kind: 'not', child, width, height, _key: undefined };
    }
    PatternTree.not = not;
    function isLeafOrTop(p) {
        return p.kind === 'leaf' || p.kind === 'top';
    }
    PatternTree.isLeafOrTop = isLeafOrTop;
    function isDisjoint(p, q) {
        // double negation here due to and/or patterns
        return !matches(p, p => !p.isDisjoint(q));
    }
    PatternTree.isDisjoint = isDisjoint;
    function rotate(p) {
        switch (p.kind) {
            case 'leaf':
            case 'top':
                return Pattern.rotate(p);
            case 'bottom':
                return bottom(p.height, p.width);
            case 'and':
                return and(rotate(p.left), rotate(p.right));
            case 'or':
                return or(rotate(p.left), rotate(p.right));
            case 'not':
                return not(Pattern.rotate(p.child), p.child.alphabetKey);
        }
    }
    PatternTree.rotate = rotate;
    function reflect(p) {
        switch (p.kind) {
            case 'leaf':
                return Pattern.reflect(p);
            case 'top':
            case 'bottom':
                return p;
            case 'and':
                return and(reflect(p.left), reflect(p.right));
            case 'or':
                return or(reflect(p.left), reflect(p.right));
            case 'not':
                return not(Pattern.reflect(p.child), p.child.alphabetKey);
        }
    }
    PatternTree.reflect = reflect;
    function getLeaves(p) {
        switch (p.kind) {
            case 'leaf':
            case 'top':
                return [p];
            case 'bottom':
                return [];
            case 'and':
            case 'or': {
                const out = getLeaves(p.left);
                out.push(...getLeaves(p.right));
                return out;
            }
            case 'not': {
                const out = getLeaves(p.child);
                out.push(top(p.width, p.height, p.child.alphabetKey));
                return out;
            }
        }
    }
    PatternTree.getLeaves = getLeaves;
    /**
     * Evaluates a pattern tree as a boolean expression, using the given
     * predicate to evaluate the leaf and top nodes.
     */
    function matches(p, predicate) {
        switch (p.kind) {
            case 'leaf':
            case 'top':
                return predicate(p);
            case 'bottom':
                return false;
            case 'and':
                return matches(p.left, predicate) && matches(p.right, predicate);
            case 'or':
                return matches(p.left, predicate) || matches(p.right, predicate);
            case 'not':
                return predicate(top(p.width, p.height, p.child.alphabetKey)) && !matches(p.child, predicate);
        }
    }
    PatternTree.matches = matches;
    function key(p) {
        switch (p.kind) {
            case 'leaf':
            case 'top':
                return Pattern.key(p);
            case 'bottom':
                return p._key ??= `${p.width}x${p.height}:${p.kind}`;
            case 'and':
            case 'or':
                return p._key ??= `(${key(p.left)}) ${p.kind} (${key(p.right)})`;
            case 'not':
                return p._key ??= `not (${key(p.child)})`;
        }
    }
    PatternTree.key = key;
})(PatternTree || (PatternTree = {}));
class Pattern extends MJr.Pattern {
    alphabetKey;
    masks;
    hasUnions;
    static rowsOf(p) {
        const { width, height } = p;
        if (height === 1) {
            return [p];
        }
        if (p.kind === 'top') {
            return emptyArray(height, PatternTree.top(width, 1, p.alphabetKey));
        }
        const { pattern, masks } = p;
        const out = [];
        const n = width * height;
        for (let offset = 0; offset < n; offset += width) {
            const row = pattern.slice(offset, offset + width);
            const rowMasks = masks.slice(offset, offset + width);
            out.push(new Pattern(width, 1, p.alphabetKey, row, rowMasks, p.hasUnions));
        }
        return out;
    }
    static windowsOf(p, n, periodic) {
        const { width, height, alphabetKey, pattern, masks, hasUnions } = p;
        const maxX = periodic ? width : width - n + 1;
        const maxY = periodic ? height : height - n + 1;
        const out = [];
        for (let y = 0; y < maxY; ++y) {
            for (let x = 0; x < maxX; ++x) {
                const qPattern = [];
                const qMasks = [];
                for (let dy = 0; dy < n; ++dy) {
                    const py = (y + dy) % height;
                    for (let dx = 0; dx < n; ++dx) {
                        const px = (x + dx) % width;
                        qPattern.push(pattern[px + width * py]);
                        qMasks.push(masks[px + width * py]);
                    }
                }
                out.push(new Pattern(n, n, alphabetKey, qPattern, qMasks, hasUnions));
            }
        }
        return out;
    }
    static rotate(p) {
        const { width, height, pattern, masks } = p;
        if (p.kind === 'top') {
            return new Pattern(height, width, p.alphabetKey, pattern, masks, p.hasUnions);
        }
        const newData = [];
        const newMasks = [];
        for (let x = 0; x < width; ++x) {
            for (let y = height - 1; y >= 0; --y) {
                const index = x + width * y;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(height, width, p.alphabetKey, newData, newMasks, p.hasUnions);
    }
    static reflect(p) {
        if (p.kind === 'top') {
            return p;
        }
        const { width, height, pattern, masks } = p;
        const newData = [];
        const newMasks = [];
        for (let y = height - 1; y >= 0; --y) {
            for (let x = 0; x < width; ++x) {
                const index = x + width * y;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(width, height, p.alphabetKey, newData, newMasks, p.hasUnions);
    }
    static key(p) {
        return p._key ??= `${p.width}x${p.height}:${p.masks.map(ISet.key).join(';')}`;
    }
    kind;
    _key = undefined;
    constructor(
    /**
     * The pattern's width.
     */
    width, 
    /**
     * The pattern's height.
     */
    height, alphabetKey, 
    /**
     * The pattern, as a flat array. Wildcards are represented as 0xFF, and
     * unions as 0xFE.
     */
    pattern, 
    /**
     * The pattern, as a flat array of bitmasks.
     */
    masks, 
    /**
     * Indicates whether this pattern has any unions, i.e. cells which can
     * match multiple alphabet symbols, but are not wildcards.
     */
    hasUnions) {
        super(width, height, pattern);
        this.alphabetKey = alphabetKey;
        this.masks = masks;
        this.hasUnions = hasUnions;
        this.kind = pattern.every(p => p === 255 /* PatternValue.WILDCARD */) ? 'top' : 'leaf';
    }
    canBe(dx, dy, c) {
        return ISet.has(this.masks[dx + dy * this.width], c);
    }
    /**
     * Calls the given function for each non-wildcard, non-union symbol in this
     * pattern.
     */
    forEach(f) {
        const v = this.vectorData;
        for (let i = 0; i < v.length; i += 3) {
            f(v[i], v[i + 1], v[i + 2]);
        }
    }
    map(f) {
        const out = [];
        this.forEach((dx, dy, c) => out.push(f(dx, dy, c)));
        return out;
    }
    every(f) {
        const v = this.vectorData;
        for (let i = 0; i < v.length; i += 3) {
            if (!f(v[i], v[i + 1], v[i + 2])) {
                return false;
            }
        }
        return true;
    }
    some(f) {
        return !this.every((dx, dy, c) => !f(dx, dy, c));
    }
    isDisjoint(other) {
        return this.masks.some((mask, i) => ISet.isDisjoint(mask, other.masks[i]));
    }
}
