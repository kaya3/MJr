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
    
    // all type declarations here must be JSON serialisable, since `JSON.stringify` is used to detect repeated IR code and simplify it
    const JSON_KEY = Symbol();
    export function key(a: Stmt | Expr): string {
        return a[JSON_KEY] ??= JSON.stringify(a);
    }
    export function equals(a: Stmt | Expr | undefined, b: Stmt | Expr | undefined): boolean {
        if(a === b) { return true; }
        if(a === undefined || b === undefined || a.kind !== b.kind) { return false; }
        return key(a) === key(b);
    }
    
    export type Expr = AttrExpr | LetInExpr | LiteralExpr | NameExpr | ObjectExpr | OpExpr | ParamExpr
    export type Stmt = AssignStmt | BlankLineStmt | BlockStmt | BreakStmt | CommentStmt | DeclFuncStmt | DeclVarsStmt | ExprStmt | ForRangeStmt | IfStmt | LogStmt | PassStmt | PreambleStmt | ReturnStmt | SwitchStmt | ThrowStmt | WhileStmt | YieldStmt
    
    export type IRType = Readonly<
        | {kind: Type.PrimitiveKind | 'byte' | 'pattern' | 'prng' | 'rewriteinfo' | 'sampler' | 'void'}
        | ConstArrayType
        | MutableArrayType
        | DictType
        | {kind: 'nullable', componentType: IRType}
    >
    export interface ConstArrayType extends Readonly<{kind: 'array.const', domainSize: number}> {}
    export interface MutableArrayType extends Readonly<{kind: 'array.mutable', domainSize: number}> {}
    export interface DictType extends Readonly<{kind: 'dict', keys: readonly string[], values: readonly IRType[]}> {}
    
    export type AssignOp = '=' | '+=' | '-=' | '&=' | '|='
    export type BinaryOp = Op.BinaryOp | 'int_and' | 'int_or' | 'int_lshift' | 'int_rshift' | 'loose_int_plus' | 'loose_int_minus' | 'loose_int_mult' | 'loose_int_floordiv' | 'loose_int_mod'
    export type UnaryOp = Op.UnaryOp | 'int_not'
    export type Op = BinaryOp | UnaryOp
    
    type LibClass = KeysMatching<typeof MJr, new (...args: never[]) => unknown>
    type LibMethod<K extends LibClass | 'PRNG'>
        = K extends LibClass
        ? `${KeysMatching<InstanceType<typeof MJr[K]>, Function>}`
        : KeysMatching<MJr.PRNG, Function>
    
    type LibFunction = Exclude<KeysMatching<typeof MJr, Function>, LibClass | 'fraction'>
    
    export interface Case extends Readonly<{values: readonly number[], then: Stmt}> {}
    export interface VarDecl extends Readonly<{name: string, type: IRType, initialiser?: Expr}> {}
    export interface VarDeclWithInitialiser extends VarDecl {readonly initialiser: Expr}
    
    type _StmtNode<K extends string, T> = Readonly<{kind: `stmt.${K}`} & T> & {[JSON_KEY]?: string}
    export interface AssignStmt extends _StmtNode<'assign', {op: AssignOp, left: NameExpr | AttrExpr | ArrayAccessExpr, right: Expr}> {}
    export interface BlankLineStmt extends _StmtNode<'blankline', {}> {}
    export interface BlockStmt extends _StmtNode<'block', {children: readonly Stmt[]}> {}
    export interface BreakStmt extends _StmtNode<'break', {}> {}
    export interface CommentStmt extends _StmtNode<'comment', {comment: string}> {}
    export interface DeclFuncStmt extends _StmtNode<'decl.func', {name: string, yields: IRType | undefined, params: readonly string[], paramTypes: readonly IRType[], returnType: IRType, body: Stmt}> {}
    export interface DeclVarsStmt extends _StmtNode<'decl.vars', {decls: readonly VarDecl[], mutable: boolean}> {}
    export interface ExprStmt extends _StmtNode<'expr', {expr: CallLibExpr | CallLocalExpr}> {}
    export interface ForRangeStmt extends _StmtNode<'for.range', {indexName: string, low: Expr, high: Expr, reverse: boolean, body: Stmt}> {}
    export interface IfStmt extends _StmtNode<'if', {condition: Expr, then: Stmt, otherwise?: Stmt}> {}
    export interface LogStmt extends _StmtNode<'log', {expr: Expr}> {}
    export interface PassStmt extends _StmtNode<'pass', {}> {}
    export interface PreambleStmt extends _StmtNode<'preamble', {paramTypes: DictType, emitChecks: boolean, libVersion: number, opsUsed: readonly Op[]}> {}
    export interface ReturnStmt extends _StmtNode<'return', {expr?: Expr}> {}
    export interface SwitchStmt extends _StmtNode<'switch', {expr: Expr, cases: readonly Case[]}> {}
    export interface ThrowStmt extends _StmtNode<'throw', {message: string}> {}
    export interface WhileStmt extends _StmtNode<'while', {condition: Expr, then: Stmt}> {}
    export interface YieldStmt extends _StmtNode<'yield', {expr?: Expr}> {}
    
    type _ExprNode<K extends string, T> = Readonly<{kind: `expr.${K}`} & T> & {[JSON_KEY]?: string}
    export interface AttrExpr extends _ExprNode<'attr', {left: Expr, attr: string}> {}
    export interface LetInExpr extends _ExprNode<'letin', {decls: readonly VarDeclWithInitialiser[], child: Expr}> {}
    export interface NameExpr extends _ExprNode<'name', {name: string}> {}
    export interface ParamExpr extends _ExprNode<'param', {name: string, otherwise: Expr}> {}
    
    export type LiteralExpr = BoolLiteralExpr | FloatLiteralExpr | IntLiteralExpr | NullLiteralExpr | StrLiteralExpr
    export interface BoolLiteralExpr extends _ExprNode<'literal.bool', {value: boolean}> {}
    export interface FloatLiteralExpr extends _ExprNode<'literal.float', {value: number}> {}
    export interface IntLiteralExpr extends _ExprNode<'literal.int', {value: number}> {}
    export interface NullLiteralExpr extends _ExprNode<'literal.null', {}> {}
    export interface StrLiteralExpr extends _ExprNode<'literal.str', {value: string}> {}
    
    export type ObjectExpr = ConstArrayExpr | DictExpr | NewArrayExpr
    export interface ConstArrayExpr extends _ExprNode<'array.const', {from: readonly number[], domainSize: number, rowLength: number}> {}
    export interface DictExpr extends _ExprNode<'dict', {type: DictType, values: readonly Expr[]}> {}
    export interface NewArrayExpr extends _ExprNode<'array.new', {domainSize: number, length: Expr}> {}
    
    export type OpExpr = ArrayAccessExpr | BinaryOpExpr | CallLibExpr | CallLocalExpr | TernaryExpr | UnaryOpExpr
    export interface ArrayAccessExpr extends _ExprNode<'op.access', {left: Expr, right: Expr}> {}
    export interface BinaryOpExpr extends _ExprNode<'op.binary', {op: BinaryOp, left: Expr, right: Expr}> {}
    export interface CallLocalExpr extends _ExprNode<'op.call.local', {name: string, args: readonly Expr[]}> {}
    export interface TernaryExpr extends _ExprNode<'op.ternary', {condition: Expr, then: Expr, otherwise: Expr}> {}
    export interface UnaryOpExpr extends _ExprNode<'op.unary', {op: UnaryOp, child: Expr}> {}
    
    export type CallLibExpr = CallLibConstructorExpr | CallLibFunctionExpr | CallLibMethodExpr
    export interface CallLibConstructorExpr extends _ExprNode<'op.call.lib.constructor', {className: LibClass, args: readonly Expr[]}> {}
    export interface CallLibFunctionExpr extends _ExprNode<'op.call.lib.function', {name: LibFunction, args: readonly Expr[]}> {}
    export interface CallLibMethodExpr extends _ExprNode<'op.call.lib.method', {className: LibClass | 'PRNG', name: string, obj: Expr, args: readonly Expr[]}> {}
    
    export function float(value: number): FloatLiteralExpr {
        return {kind: 'expr.literal.float', value};
    }
    export function int(value: number): IntLiteralExpr {
        return {kind: 'expr.literal.int', value};
    }
    export function str(value: string): StrLiteralExpr {
        return {kind: 'expr.literal.str', value};
    }
    export const ZERO = int(0);
    export const ONE = int(1);
    export const TRUE: BoolLiteralExpr = {kind: 'expr.literal.bool', value: true};
    export const FALSE: BoolLiteralExpr = {kind: 'expr.literal.bool', value: false};
    export const NULL: NullLiteralExpr = {kind: 'expr.literal.null'};
    
    export function attr(left: Expr, attr: string): AttrExpr {
        return {kind: 'expr.attr', left, attr};
    }
    export function letIn(decls: readonly VarDeclWithInitialiser[], child: Expr): Expr {
        return decls.length === 0 ? child
            : {kind: 'expr.letin', decls, child};
    }
    export function name(name: string): NameExpr {
        return {kind: 'expr.name', name};
    }
    export function param(name: string, otherwise: Expr): ParamExpr {
        return {kind: 'expr.param', name, otherwise};
    }
    
    export function dict(type: DictType, values: readonly Expr[]): DictExpr {
        return {kind: 'expr.dict', type, values};
    }
    export function newArray(length: Expr, domainSize: number): NewArrayExpr {
        return {kind: 'expr.array.new', length, domainSize};
    }
    export function newInt32Array(length: Expr): NewArrayExpr {
        return newArray(length, 2 ** 32);
    }
    export function constArray(from: readonly number[], domainSize: number, rowLength: number = DEFAULT_ROW_LENGTH): ConstArrayExpr {
        return {kind: 'expr.array.const', from, domainSize, rowLength};
    }
    
    export function access(left: Expr, right: Expr): ArrayAccessExpr {
        return {kind: 'expr.op.access', left, right};
    }
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
    export function libConstructorCall(className: LibClass, args: readonly Expr[]): CallLibConstructorExpr {
        return {kind: 'expr.op.call.lib.constructor', className, args};
    }
    export function libFunctionCall(name: LibFunction, args: readonly Expr[]): CallLibFunctionExpr {
        return {kind: 'expr.op.call.lib.function', name, args};
    }
    export function libMethodCall<K extends LibClass | 'PRNG'>(className: K, name: LibMethod<K>, obj: Expr, args: readonly Expr[]): CallLibMethodExpr {
        return {kind: 'expr.op.call.lib.method', className, name, obj, args};
    }
    export function localCall(name: string, args: readonly Expr[]): CallLocalExpr {
        return {kind: 'expr.op.call.local', name, args};
    }
    export function ternary(condition: Expr, then: Expr, otherwise: Expr): TernaryExpr {
        if(condition.kind === 'expr.op.unary' && condition.op === 'bool_not') {
            condition = condition.child;
            const tmp = then; then = otherwise; otherwise = tmp;
        }
        return {kind: 'expr.op.ternary', condition, then, otherwise};
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
    
    export const BLANK_LINE: BlankLineStmt = {kind: 'stmt.blankline'};
    export const BREAK: BreakStmt = {kind: 'stmt.break'};
    export const PASS: PassStmt = {kind: 'stmt.pass'};
    
    export function assign(left: NameExpr | AttrExpr | ArrayAccessExpr, op: AssignOp, right: Expr): AssignStmt {
        return {kind: 'stmt.assign', op, left, right};
    }
    export function block(children: readonly Stmt[]): Stmt {
        children = children.flatMap(c =>
            c.kind === 'stmt.block' ? c.children
            : c.kind === 'stmt.pass' ? []
            : [c]
        );
        return children.length === 0 ? PASS
            : children.length === 1 ? children[0]
            : {kind: 'stmt.block', children};
    }
    export function comment(comment: string): CommentStmt {
        return {kind: 'stmt.comment', comment};
    }
    export function declFunc(name: string, yields: IRType | undefined, params: readonly string[], paramTypes: readonly IRType[], returnType: IRType, body: Stmt): DeclFuncStmt {
        return {kind: 'stmt.decl.func', name, yields, params, paramTypes, returnType, body};
    }
    export function declVar(name: string, type: IRType, initialiser?: Expr, mutable: boolean = false): Stmt {
        return declVars([{name, type, initialiser}], mutable);
    }
    export function declVars(decls: readonly VarDecl[], mutable: boolean = false): Stmt {
        return decls.length > 0 ? {kind: 'stmt.decl.vars', decls, mutable} : PASS;
    }
    export function forRange(indexName: string, low: Expr, high: Expr, body: Stmt): ForRangeStmt {
        return {kind: 'stmt.for.range', indexName, low, high, reverse: false, body};
    }
    export function forRangeReverse(indexName: string, low: Expr, high: Expr, body: Stmt): ForRangeStmt {
        return {kind: 'stmt.for.range', indexName, low, high, reverse: true, body};
    }
    export function if_(condition: Expr, then: Stmt, otherwise?: Stmt): Stmt {
        if(otherwise !== undefined && otherwise.kind === 'stmt.pass') { otherwise = undefined; }
        
        if(condition.kind === 'expr.literal.bool') {
            return condition.value ? then : (otherwise ?? PASS);
        } else if(equals(then, otherwise)) {
            return then;
        } else if(then.kind === 'stmt.pass') {
            return otherwise === undefined ? PASS : if_(unaryOp('bool_not', condition), otherwise);
        } else if(then.kind === 'stmt.assign' && otherwise !== undefined && otherwise.kind === 'stmt.assign' && equals(then.left, otherwise.left) && then.op === otherwise.op) {
            return assign(then.left, then.op, ternary(condition, then.right, otherwise.right));
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
    export function localCallStmt(f: string, args: readonly Expr[]): ExprStmt {
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
    export function switch_(expr: Expr, casesByIndex: readonly Stmt[]): Stmt {
        const firstCase = casesByIndex[0];
        if(firstCase.kind === 'stmt.if' && casesByIndex.every(c => c.kind === 'stmt.if' && equals(c.condition, firstCase.condition))) {
            // factor out common condition; the `otherwise` part will generally be trivial
            return if_(
                firstCase.condition,
                switch_(expr, casesByIndex.map(c => (c as IfStmt).then)),
                switch_(expr, casesByIndex.map(c => (c as IfStmt).otherwise ?? PASS)),
            );
        }
        
        // de-duplicate cases
        const map = new Map<string, {values: number[], then: Stmt}>();
        let exhaustive = true;
        for(let i = 0; i < casesByIndex.length; ++i) {
            const c = casesByIndex[i];
            if(c.kind === 'stmt.pass') { exhaustive = false; continue; }
            
            const k = key(c);
            let aggregated = map.get(k);
            if(aggregated === undefined) { map.set(k, aggregated = {values: [], then: c}); }
            aggregated.values.push(i);
        }
        
        const cases = Array.from(map.values());
        return cases.length === 0 ? PASS
            : cases.length === 1 && exhaustive ? firstCase
            : {kind: 'stmt.switch', expr, cases};
    }
    export function throw_(message: string): ThrowStmt {
        return {kind: 'stmt.throw', message};
    }
    export function while_(condition: Expr, then: Stmt): Stmt {
        return {kind: 'stmt.while', condition, then};
    }
    export function yield_(expr?: Expr): YieldStmt {
        return {kind: 'stmt.yield', expr};
    }
    
    const OP_NEGATIONS: {readonly [K in BinaryOp]?: BinaryOp} = {
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
    };
    
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
            return expr.kind === 'expr.op.unary' && expr.op === 'bool_not' ? expr.child
                : expr.kind === 'expr.op.binary' && expr.op in OP_NEGATIONS ? _binOp(OP_NEGATIONS[expr.op]!, expr.left, expr.right)
                : _unOp('bool_not', expr);
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
            return _binOp('int_eq', left, right);
        },
        ne(left: Expr, right: Expr): Expr {
            return _binOp('int_ne', left, right);
        },
        lt(left: Expr, right: Expr): Expr {
            return _binOp('int_lt', left, right);
        },
        le(left: Expr, right: Expr): Expr {
            return _binOp('int_le', left, right);
        },
        gt(left: Expr, right: Expr): Expr {
            return _binOp('int_gt', left, right);
        },
        ge(left: Expr, right: Expr): Expr {
            return _binOp('int_ge', left, right);
        },
    };
    
    export const BOOL_TYPE: IRType = {kind: 'bool'};
    export const BYTE_TYPE: IRType = {kind: 'byte'};
    export const FLOAT_TYPE: IRType = {kind: 'float'};
    export const FRACTION_TYPE: IRType = {kind: 'fraction'};
    export const GRID_TYPE: IRType = {kind: 'grid'};
    export const INT_TYPE: IRType = {kind: 'int'};
    export const PATTERN_TYPE: IRType = {kind: 'pattern'};
    export const PRNG_TYPE: IRType = {kind: 'prng'};
    export const REWRITE_INFO_TYPE: IRType = {kind: 'rewriteinfo'};
    export const SAMPLER_TYPE: IRType = {kind: 'sampler'};
    export const STR_TYPE: IRType = {kind: 'str'};
    export const VOID_TYPE: IRType = {kind: 'void'};
    
    export const GRID_DATA_ARRAY_TYPE: IRType = constArrayType(128);
    export const INT32_ARRAY_TYPE: IRType = mutableArrayType(2 ** 32);
    
    export function mutableArrayType(domainSize: number): MutableArrayType {
        return {kind: 'array.mutable', domainSize};
    }
    export function constArrayType(domainSize: number): ConstArrayType {
        return {kind: 'array.const', domainSize};
    }
    
    export function nullableType(componentType: IRType): IRType {
        return {kind: 'nullable', componentType};
    }
}
