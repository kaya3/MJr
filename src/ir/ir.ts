/**
 * Abstract syntax tree for a high-level intermediate representation (IR) which
 * is output by the compiler. The IR is converted into output source code by a
 * class in the `CodeGen` namespace.
 * 
 * The IR should be convertible to most imperative languages, so long as they
 * allow local function declarations to access constants from the outer scope.
 */
namespace IR {
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
    
    export type Decl = StmtLevelDecl | VarDecl
    export type ExportableDecl = ConstDecl | FuncDecl
    export type StmtLevelDecl = ExportableDecl | MutVarDecl | InitDecl | NoDecl | MultiDecl
    export type VarDecl = ConstDecl | LoopVarDecl | MutVarDecl | ParamDecl
    
    type _Node<K extends string, T> = Readonly<{kind: K, info: Info} & T> & {[JSON_KEY]?: string}
    
    type _DeclNode<K extends string, T> = _Node<`decl.${K}`, T>
    type _VarDeclNode<K extends string, N extends NameExpr, T> = _DeclNode<`var.${K}`, {name: N, type: IRType} & T>
    export interface ConstDecl extends _VarDeclNode<'const', ConstNameExpr, {initialiser: Expr}> {}
    export interface LoopVarDecl extends _VarDeclNode<'loop', ConstNameExpr, {initialiser?: never}> {}
    export interface MutVarDecl extends _VarDeclNode<'mut', MutNameExpr, {initialiser: Expr | undefined}> {}
    export interface ParamDecl extends _VarDeclNode<'param', ConstNameExpr, {initialiser: Expr | undefined, isOptional: boolean}> {}
    
    export interface NoDecl extends _DeclNode<'none', {}> {}
    export interface MultiDecl extends _DeclNode<'multi', {children: readonly StmtLevelDecl[]}> {}
    export interface InitDecl extends _DeclNode<'init', {child: StmtLevelDecl, stmt: Stmt}> {}
    export interface FuncDecl extends _DeclNode<'func', {name: ConstNameExpr, yields: IRType | undefined, params: readonly ParamDecl[], returnType: IRType, body: Stmt}> {}
    
    export type Stmt = AssignStmt | BlankLineStmt | BreakStmt | CommentStmt | ContinueStmt | DeclInStmt | ExportStmt | ExprStmt | ForRangeStmt | IfStmt | LogStmt | PassStmt | PreambleStmt | ReturnStmt | SequenceStmt | SwitchStmt | ThrowStmt | WhileStmt | YieldStmt
    
    export interface Case extends Readonly<{values: readonly number[], then: Stmt}> {}
    
    type _StmtNode<K extends string, T = {}> = _Node<`stmt.${K}`, T>
    export interface AssignStmt extends _StmtNode<'assign', {op: AssignOp, left: MutNameExpr | AttrExpr | ArrayAccessExpr, right: Expr}> {}
    export interface BlankLineStmt extends _StmtNode<'blankline'> {}
    export interface BreakStmt extends _StmtNode<'break'> {}
    export interface CommentStmt extends _StmtNode<'comment', {comment: string}> {}
    export interface ContinueStmt extends _StmtNode<'continue'> {}
    export interface DeclInStmt extends _StmtNode<'decl', {decl: StmtLevelDecl, child: Stmt}> {}
    export interface ExportStmt extends _StmtNode<'export', {decl: ExportableDecl}> {}
    export interface ExprStmt extends _StmtNode<'expr', {expr: Expr}> {}
    export interface ForRangeStmt extends _StmtNode<'for.range', {index: LoopVarDecl, low: Expr, high: Expr, reverse: boolean, body: Stmt}> {}
    export interface IfStmt extends _StmtNode<'if', {condition: Expr, then: Stmt, otherwise: Stmt | undefined}> {}
    export interface LogStmt extends _StmtNode<'log', {expr: Expr}> {}
    export interface PassStmt extends _StmtNode<'pass'> {}
    export interface PreambleStmt extends _StmtNode<'preamble', {paramTypes: DictType, emitChecks: boolean, libVersion: number, opsUsed: readonly Op[]}> {}
    export interface ReturnStmt extends _StmtNode<'return', {expr: Expr | undefined}> {}
    export interface SequenceStmt extends _StmtNode<'sequence', {children: readonly Stmt[]}> {}
    export interface SwitchStmt extends _StmtNode<'switch', {expr: Expr, cases: readonly Case[], exhaustive: boolean}> {}
    export interface ThrowStmt extends _StmtNode<'throw', {message: string}> {}
    export interface WhileStmt extends _StmtNode<'while', {condition: Expr, then: Stmt}> {}
    export interface YieldStmt extends _StmtNode<'yield', {expr: Expr | undefined}> {}
    
    export type Expr = AttrExpr | DeferredExpr | LetInExpr | LiteralExpr | NameExpr | ObjectExpr | OpExpr | ParamExpr | UnusedExpr
    export type ConstExpr = LiteralExpr | ConstNameExpr | UnusedExpr
    
    export type AssignOp = '=' | '+=' | '-=' | '&=' | '|='
    export type BinaryOp = Op.BinaryOp | 'array_access' | 'int_and' | 'int_or' | 'int_xor' | 'int_lshift' | 'int_rshift' | 'loose_int_plus' | 'loose_int_minus' | 'loose_int_mult' | 'loose_int_floordiv' | 'loose_int_mod'
    export type UnaryOp = Op.UnaryOp | 'int_not' | 'int_ctz' | 'float_log2'
    export type Op = BinaryOp | UnaryOp
    
    type _ExprNode<K extends string, T> = _Node<`expr.${K}`, T>
    export interface AttrExpr extends _ExprNode<'attr', {left: Expr, attr: string}> {}
    export interface DeferredExpr extends _ExprNode<'unused.deferred', {id: number, purpose: string}> {}
    export interface LetInExpr extends _ExprNode<'letin', {decl: ConstDecl, child: Expr}> {}
    export interface NameExpr extends _ExprNode<'name', {id: number, namePart: string, isMutable: boolean}> {}
    export interface ParamExpr extends _ExprNode<'param', {name: string, otherwise: Expr}> {}
    export interface UnusedExpr extends _ExprNode<'unused.error', {error: string}> {}
    
    export type ConstNameExpr = NameExpr & {readonly isMutable: false}
    export type MutNameExpr = NameExpr & {readonly isMutable: true}
    
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
    export interface ArrayAccessExpr extends _ExprNode<'op.binary', {op: 'array_access', left: Expr, right: Expr}> {}
    export interface BinaryOpExpr extends _ExprNode<'op.binary', {op: BinaryOp, left: Expr, right: Expr}> {}
    export interface CallLocalExpr extends _ExprNode<'op.call.local', {name: ConstNameExpr, args: readonly Expr[]}> {}
    export interface TernaryExpr extends _ExprNode<'op.ternary', {condition: Expr, then: Expr, otherwise: Expr}> {}
    export interface UnaryOpExpr extends _ExprNode<'op.unary', {op: UnaryOp, child: Expr}> {}
    
    export type CallLibExpr = CallLibConstructorExpr | CallLibFunctionExpr | CallLibMethodExpr
    export interface CallLibConstructorExpr extends _ExprNode<'op.call.lib.constructor', {className: LibClass, args: readonly Expr[]}> {}
    export interface CallLibFunctionExpr extends _ExprNode<'op.call.lib.function', {name: LibFunction, args: readonly Expr[]}> {}
    export interface CallLibMethodExpr extends _ExprNode<'op.call.lib.method', {className: LibInterface, name: string, obj: Expr, args: readonly Expr[]}> {}
    
    export type LibClass = KeysMatching<typeof MJr, new (...args: never[]) => unknown>
    export type LibInterface = LibClass | 'PRNG'
    export type LibMethod<K extends LibInterface>
        = K extends LibClass
        ? `${KeysMatching<InstanceType<typeof MJr[K]>, Function>}`
        : KeysMatching<MJr.PRNG, Function>
    
    export type LibFunction = Exclude<KeysMatching<typeof MJr, Function>, LibClass | 'fraction'>
    
    export type LocalFunction = 'mask_clear' | 'mask_set' | 'mask_hasnt'
    
    export interface PatternResult extends Readonly<{
        expr: IR.ConstExpr,
        constant: Pattern | undefined,
    }> {}
}
