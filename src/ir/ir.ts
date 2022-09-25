/**
 * Abstract syntax tree for a high-level intermediate representation (IR) which
 * is output by the compiler. The IR is converted into output source code by a
 * class in the `CodeGen` namespace.
 * 
 * The IR should be convertible to most imperative languages, so long as they
 * allow local function declarations to access constants from the outer scope.
 */
namespace IR {
    export interface IRCompiler {
        expr(expr: ASG.Expression): Expr;
        type(type: Type.Type): IRType;
    }
    
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
    
    export type AssignOp = '=' | '+=' | '-=' | '&=' | '|='
    export type BinaryOp = Op.BinaryOp | 'int_and' | 'int_or' | 'int_xor' | 'int_lshift' | 'int_rshift' | 'loose_int_plus' | 'loose_int_minus' | 'loose_int_mult' | 'loose_int_floordiv' | 'loose_int_mod'
    export type UnaryOp = Op.UnaryOp | 'int_not'
    export type Op = BinaryOp | UnaryOp
    
    type LibClass = KeysMatching<typeof MJr, new (...args: never[]) => unknown>
    type LibMethod<K extends LibClass | 'PRNG'>
        = K extends LibClass
        ? `${KeysMatching<InstanceType<typeof MJr[K]>, Function>}`
        : KeysMatching<MJr.PRNG, Function>
    
    type LibFunction = Exclude<KeysMatching<typeof MJr, Function>, LibClass | 'fraction'>
    
    export interface Case extends Readonly<{values: readonly number[], then: Stmt}> {}
    export interface VarDecl extends Readonly<{name: NameExpr, type: IRType, initialiser?: Expr}> {}
    export interface VarDeclWithInitialiser extends VarDecl {readonly initialiser: Expr}
    
    type _StmtNode<K extends string, T> = Readonly<{kind: `stmt.${K}`} & T> & {[JSON_KEY]?: string}
    export interface AssignStmt extends _StmtNode<'assign', {op: AssignOp, left: NameExpr | AttrExpr | ArrayAccessExpr, right: Expr}> {}
    export interface BlankLineStmt extends _StmtNode<'blankline', {}> {}
    export interface BlockStmt extends _StmtNode<'block', {children: readonly Stmt[]}> {}
    export interface BreakStmt extends _StmtNode<'break', {}> {}
    export interface CommentStmt extends _StmtNode<'comment', {comment: string}> {}
    export interface DeclFuncStmt extends _StmtNode<'decl.func', {name: NameExpr, yields: IRType | undefined, params: readonly NameExpr[], paramTypes: readonly IRType[], returnType: IRType, body: Stmt}> {}
    export interface DeclVarsStmt extends _StmtNode<'decl.vars', {decls: readonly VarDecl[], mutable: boolean}> {}
    export interface ExprStmt extends _StmtNode<'expr', {expr: CallLibExpr | CallLocalExpr}> {}
    export interface ForRangeStmt extends _StmtNode<'for.range', {index: NameExpr, low: Expr, high: Expr, reverse: boolean, body: Stmt}> {}
    export interface IfStmt extends _StmtNode<'if', {condition: Expr, then: Stmt, otherwise: Stmt | undefined}> {}
    export interface LogStmt extends _StmtNode<'log', {expr: Expr}> {}
    export interface PassStmt extends _StmtNode<'pass', {}> {}
    export interface PreambleStmt extends _StmtNode<'preamble', {paramTypes: DictType, emitChecks: boolean, libVersion: number, opsUsed: readonly Op[]}> {}
    export interface ReturnStmt extends _StmtNode<'return', {expr: Expr | undefined}> {}
    export interface SwitchStmt extends _StmtNode<'switch', {expr: Expr, cases: readonly Case[]}> {}
    export interface ThrowStmt extends _StmtNode<'throw', {message: string}> {}
    export interface WhileStmt extends _StmtNode<'while', {condition: Expr, then: Stmt}> {}
    export interface YieldStmt extends _StmtNode<'yield', {expr: Expr | undefined}> {}
    
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
    export interface CallLocalExpr extends _ExprNode<'op.call.local', {name: NameExpr, args: readonly Expr[]}> {}
    export interface TernaryExpr extends _ExprNode<'op.ternary', {condition: Expr, then: Expr, otherwise: Expr}> {}
    export interface UnaryOpExpr extends _ExprNode<'op.unary', {op: UnaryOp, child: Expr}> {}
    
    export type CallLibExpr = CallLibConstructorExpr | CallLibFunctionExpr | CallLibMethodExpr
    export interface CallLibConstructorExpr extends _ExprNode<'op.call.lib.constructor', {className: LibClass, args: readonly Expr[]}> {}
    export interface CallLibFunctionExpr extends _ExprNode<'op.call.lib.function', {name: LibFunction, args: readonly Expr[]}> {}
    export interface CallLibMethodExpr extends _ExprNode<'op.call.lib.method', {className: LibClass | 'PRNG', name: string, obj: Expr, args: readonly Expr[]}> {}
    
    export function float(value: number): FloatLiteralExpr {
        return value === 0 ? FLOAT_ZERO
            : value === 1 ? FLOAT_ONE
            : value === -1 ? FLOAT_MINUS_ONE
            : {kind: 'expr.literal.float', value};
    }
    export function int(value: number): IntLiteralExpr {
        return value === 0 ? ZERO
            : value === 1 ? ONE
            : value === -1 ? MINUS_ONE
            : {kind: 'expr.literal.int', value};
    }
    export function str(value: string): StrLiteralExpr {
        return {kind: 'expr.literal.str', value};
    }
    
    // singleton objects for common values
    export const TRUE: BoolLiteralExpr = {kind: 'expr.literal.bool', value: true};
    export const FALSE: BoolLiteralExpr = {kind: 'expr.literal.bool', value: false};
    export const NULL: NullLiteralExpr = {kind: 'expr.literal.null'};
    export const ZERO: IntLiteralExpr = {kind: 'expr.literal.int', value: 0};
    export const ONE: IntLiteralExpr = {kind: 'expr.literal.int', value: 1};
    export const MINUS_ONE: IntLiteralExpr = {kind: 'expr.literal.int', value: -1};
    export const FLOAT_ZERO: FloatLiteralExpr = {kind: 'expr.literal.float', value: 0};
    export const FLOAT_ONE: FloatLiteralExpr = {kind: 'expr.literal.float', value: 1};
    export const FLOAT_MINUS_ONE: FloatLiteralExpr = {kind: 'expr.literal.float', value: -1};
    
    export function attr(left: Expr, attr: string): AttrExpr {
        return {kind: 'expr.attr', left, attr};
    }
    export function letIn(decls: readonly VarDeclWithInitialiser[], child: Expr): Expr {
        return decls.length === 0 ? child
            : {kind: 'expr.letin', decls, child};
    }
    export function nameExpr(name: string): NameExpr {
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
    export function newGridDataArray(length: Expr): NewArrayExpr {
        return newArray(length, 128);
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
    export function libConstructorCall(className: LibClass, args: readonly Expr[]): CallLibConstructorExpr {
        return {kind: 'expr.op.call.lib.constructor', className, args};
    }
    export function libFunctionCall(name: LibFunction, args: readonly Expr[]): CallLibFunctionExpr {
        return {kind: 'expr.op.call.lib.function', name, args};
    }
    export function libMethodCall<K extends LibClass | 'PRNG'>(className: K, name: LibMethod<K>, obj: Expr, args: readonly Expr[]): CallLibMethodExpr {
        return {kind: 'expr.op.call.lib.method', className, name, obj, args};
    }
    export function localCall(name: NameExpr, args: readonly Expr[]): CallLocalExpr {
        return {kind: 'expr.op.call.local', name, args};
    }
    export function ternary(condition: Expr, then: Expr, otherwise: Expr): Expr {
        if(condition === TRUE) {
            return then;
        } else if(condition === FALSE) {
            return otherwise;
        } else if(condition.kind === 'expr.op.unary' && condition.op === 'bool_not') {
            condition = condition.child;
            const tmp = then; then = otherwise; otherwise = tmp;
        }
        return {kind: 'expr.op.ternary', condition, then, otherwise};
    }
    
    // singletons
    export const BLANK_LINE: BlankLineStmt = {kind: 'stmt.blankline'};
    export const BREAK: BreakStmt = {kind: 'stmt.break'};
    export const PASS: PassStmt = {kind: 'stmt.pass'};
    
    export function assign(left: NameExpr | AttrExpr | ArrayAccessExpr, op: AssignOp, right: Expr): AssignStmt {
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
    export function forRange(index: NameExpr, low: Expr, high: Expr, body: readonly Stmt[]): ForRangeStmt {
        return {kind: 'stmt.for.range', index, low, high, reverse: false, body: block(body)};
    }
    export function forRangeReverse(index: NameExpr, low: Expr, high: Expr, body: readonly Stmt[]): ForRangeStmt {
        return {kind: 'stmt.for.range', index, low, high, reverse: true, body: block(body)};
    }
    export function if_(condition: Expr, then: Stmt, otherwise?: Stmt): Stmt {
        if(otherwise === PASS) { otherwise = undefined; }
        
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
        } else if(then.kind === 'stmt.if' && otherwise === undefined && then.otherwise === undefined) {
            // collapse nested `if` statements
            return if_(OP.and(condition, then.condition), then.then);
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
    export function switch_(expr: Expr, casesByIndex: readonly Stmt[]): Stmt {
        if(casesByIndex.length === 0) {
            return PASS;
        } else if(casesByIndex.length === 1) {
            return casesByIndex[0];
        } else if(casesByIndex.length === 2) {
            return if_(OP.eq(expr, ZERO), casesByIndex[0], casesByIndex[1]);
        }
        
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
            if(c === PASS) { exhaustive = false; continue; }
            
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
        // the compiler won't ever output an infinite loop that does nothing; if the loop body is empty, then the condition must be false
        if(then === PASS) { return PASS; }
        
        return {kind: 'stmt.while', condition, then};
    }
    export function yield_(expr?: Expr): YieldStmt {
        return {kind: 'stmt.yield', expr};
    }
}
