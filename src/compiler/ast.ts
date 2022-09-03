/**
 * Type declarations for the abstract syntax tree (AST).
 */
namespace AST {
    export type Node = CompilationUnit | Declaration | Expression | Rule | Statement
    export type Kind = Node['kind']
    
    export type Declaration = LegendDecl | LetDecl | SymmetryDecl | UnionDecl
    export type Expression = DeclarationExpr | DictExpr | GridExpr | LiteralExpr | NameExpr | OpExpr
    export type Rule = DeclarationRule | FieldRule | ObserveRule | RewriteRule
    export type Statement = BlockStmt | DeclarationStmt | LineStmt | ModifiedStmt | RuleBlockStmt
    
    type _Node<K extends string, T> = Readonly<{kind: K, pos: SourcePosition} & T>
    export interface CompilationUnit extends _Node<'root', {stmts: readonly Statement[]}> {}
    export type DeclarationChildren<K extends string, T extends Node> = _Node<`${K}.decl`, {declaration: Declaration, children: readonly T[]}>
    
    // declarations
    type _DeclNode<K extends string, T> = _Node<`decl.${K}`, T>
    export interface LegendDecl extends _DeclNode<'legend', {expr: Expression}> {}
    export interface LetDecl extends _DeclNode<'let', {name: SimpleNameExpr, rhs: Expression, isParam: boolean}> {}
    export interface SymmetryDecl extends _DeclNode<'symmetry', {expr: Expression}> {}
    export interface UnionDecl extends _DeclNode<'union', {label: PatternLiteralExpr, chars: Expression}> {}
    
    // expressions
    type _ExprNode<K extends string, T> = _Node<`expr.${K}`, T>
    
    export type LiteralExpr = BoolLiteralExpr | FloatLiteralExpr | IntLiteralExpr | PatternLiteralExpr | StringLiteralExpr
    type _LiteralNode<K extends string, T> = _ExprNode<`literal.${K}`, {value: T}>
    export interface BoolLiteralExpr extends _LiteralNode<'bool', boolean> {}
    export interface FloatLiteralExpr extends _LiteralNode<'float', number> {}
    export interface IntLiteralExpr extends _LiteralNode<'int', number> {}
    export interface PatternLiteralExpr extends _ExprNode<'literal.pattern', {width: number, height: number, value: readonly (Char | CharSet)[]}> {}
    export interface StringLiteralExpr extends _LiteralNode<'str', string> {}
    
    export type NameExpr = AttributeExpr | KeywordNameExpr | SimpleNameExpr
    export type KeywordName = 'at' | 'origin' | 'random'
    export interface AttributeExpr extends _ExprNode<'attr', {left: NameExpr, attr: string}> {}
    export interface KeywordNameExpr extends _ExprNode<'name.keyword', {name: KeywordName}> {}
    export interface SimpleNameExpr extends _ExprNode<'name.simple', {name: string}> {}
    
    export type BinaryOp = keyof typeof Parser.BINARY_OPS
    export type UnaryOp = keyof typeof Parser.UNARY_OPS
    
    export type OpExpr = BinaryOpExpr | TernaryOpExpr | UnaryOpExpr
    export interface BinaryOpExpr extends _ExprNode<'op.binary', {op: BinaryOp, left: Expression, right: Expression}> {}
    export interface TernaryOpExpr extends _ExprNode<'op.ternary', {condition: Expression, then: Expression, otherwise: Expression}> {}
    export interface UnaryOpExpr extends _ExprNode<'op.unary', {op: UnaryOp, child: Expression}> {}
    
    export interface DeclarationExpr extends _ExprNode<'decl', {declaration: Declaration, child: Expression}> {}
    export interface DictExpr extends _ExprNode<'dict', {pairs: readonly (readonly [SimpleNameExpr, Expression])[]}> {}
    export interface GridExpr extends _ExprNode<'grid', {alphabetKey: string, scaleX?: Expression, scaleY?: Expression, periodic?: Expression}> {}
    
    export type Char = Tokenizer.Token<'PATTERN_CHAR'>
    export interface CharSet extends Readonly<{kind: 'CHARSET', chars: readonly Char[], pos: SourcePosition}> {}
    
    // rules
    type _RuleNode<K extends string, T> = _Node<`rule.${K}`, T>
    
    export interface DeclarationRule extends DeclarationChildren<'rule', Rule> {}
    export interface FieldRule extends _RuleNode<'field', {for_: Expression, from?: Expression, to?: Expression, on: Expression, essential?: Expression, recompute?: Expression}> {}
    export interface ObserveRule extends _RuleNode<'observe', {from: Expression, via?: Expression, to: Expression, condition?: Expression}> {}
    export interface RewriteRule extends _RuleNode<'rewrite', {from: Expression, via?: undefined, to: Expression, condition?: Expression}> {}
    
    // statements
    type _StmtNode<K extends string, T> = _Node<`stmt.${K}`, T>
    
    export interface DeclarationStmt extends DeclarationChildren<'stmt', Statement> {}
    
    // TODO: other modifiers - 'search'?
    export type ModifiedStmtName = 'limit'
    export interface ModifiedStmt extends _StmtNode<`modified.${ModifiedStmtName}`, {arg: Expression, child: Exclude<Statement, DeclarationStmt>}> {}
    
    export type BlockStmt = MarkovStmt | SequenceStmt
    type _BlockStmtNode<K extends string> = _StmtNode<`block.${K}`, {children: readonly Statement[]}>
    export interface MarkovStmt extends _BlockStmtNode<'markov'> {}
    export interface SequenceStmt extends _BlockStmtNode<'sequence'> {}
    
    export type RuleBlockStmt = AllStmt | ConvolutionStmt | MapStmt | OnceStmt | OneStmt | PrlStmt
    type _RuleStmtNode<K extends string, T> = _StmtNode<`rules.${K}`, {rules: readonly Rule[]} & T>
    export interface AllStmt extends _RuleStmtNode<'all', {temperature?: Expression, search?: Expression, maxStates?: Expression, depthCoefficient?: Expression}> {}
    export interface ConvolutionStmt extends _RuleStmtNode<'convolution', {kernel: Expression}> {}
    export interface MapStmt extends _RuleStmtNode<'map', {scaleX: Expression, scaleY: Expression, outGrid: Expression}> {}
    export interface OnceStmt extends _RuleStmtNode<'once', {}> {}
    export interface OneStmt extends _RuleStmtNode<'one', {temperature?: Expression, search?: Expression, maxStates?: Expression, depthCoefficient?: Expression}> {}
    export interface PrlStmt extends _RuleStmtNode<'prl', {kernel?: undefined}> {}
    
    export type LineStmt = ConvChainStmt | LogStmt | PassStmt | PathStmt | PutStmt | UseExprStmt | UseLetStmt
    export interface ConvChainStmt extends _StmtNode<'convchain', {sample: Expression, n: Expression, temperature?: Expression, on: Expression, periodic?: Expression}> {}
    export interface LogStmt extends _StmtNode<'log', {expr: Expression}> {}
    export interface PassStmt extends _StmtNode<'pass', {}> {}
    export interface PathStmt extends _StmtNode<'path', {from: Expression, to: Expression, input: Expression, output: Expression, longest?: Expression, inertia?: Expression}> {}
    export interface PutStmt extends _StmtNode<'put', {pattern: Expression, at: Expression, condition?: Expression}> {}
    export interface UseExprStmt extends _StmtNode<'use.expr', {expr: Expression}> {}
    export interface UseLetStmt extends _StmtNode<'use.let', {decl: LetDecl, children: readonly Statement[]}> {}
}
