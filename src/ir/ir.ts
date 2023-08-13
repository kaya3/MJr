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
    
    export type Stmt = AssignStmt | BlankLineStmt | BlockStmt | BreakStmt | CommentStmt | ContinueStmt | DeclFuncStmt | DeclVarsStmt | ExprStmt | ForRangeStmt | IfStmt | LogStmt | PassStmt | PreambleStmt | ReturnStmt | SwitchStmt | ThrowStmt | WhileStmt | YieldStmt
    
    export interface Case extends Readonly<{values: readonly number[], then: Stmt}> {}
    export interface VarDecl extends Readonly<{name: NameExpr, type: IRType, initialiser?: Expr}> {}
    export interface VarDeclWithInitialiser extends VarDecl {readonly initialiser: Expr}
    
    type _StmtNode<K extends string, T> = Readonly<{kind: `stmt.${K}`} & T> & {[JSON_KEY]?: string}
    export interface AssignStmt extends _StmtNode<'assign', {op: AssignOp, left: NameExpr | AttrExpr | ArrayAccessExpr, right: Expr}> {}
    export interface BlankLineStmt extends _StmtNode<'blankline', {}> {}
    export interface BlockStmt extends _StmtNode<'block', {children: readonly Stmt[]}> {}
    export interface BreakStmt extends _StmtNode<'break', {}> {}
    export interface CommentStmt extends _StmtNode<'comment', {comment: string}> {}
    export interface ContinueStmt extends _StmtNode<'continue', {}> {}
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
    
    export type Expr = AttrExpr | LetInExpr | LiteralExpr | NameExpr | ObjectExpr | OpExpr | ParamExpr
    
    export type AssignOp = '=' | '+=' | '-=' | '&=' | '|='
    export type BinaryOp = Op.BinaryOp | 'int_and' | 'int_or' | 'int_xor' | 'int_lshift' | 'int_rshift' | 'loose_int_plus' | 'loose_int_minus' | 'loose_int_mult' | 'loose_int_floordiv' | 'loose_int_mod'
    export type UnaryOp = Op.UnaryOp | 'int_not' | 'int_ctz' | 'float_log2'
    export type Op = BinaryOp | UnaryOp
    
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
    
    export type LibClass = KeysMatching<typeof MJr, new (...args: never[]) => unknown>
    export type LibMethod<K extends LibClass | 'PRNG'>
        = K extends LibClass
        ? `${KeysMatching<InstanceType<typeof MJr[K]>, Function>}`
        : KeysMatching<MJr.PRNG, Function>
    
    export type LibFunction = Exclude<KeysMatching<typeof MJr, Function>, LibClass | 'fraction'>
}
