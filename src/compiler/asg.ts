/**
 * Type declarations for the abstract semantic graph (ASG).
 */
namespace ASG {
    export interface ASG extends Readonly<{
        root: ASG.BlockStmt,
        grids: readonly FormalGrid[],
        limits: readonly FormalLimit[],
        params: ReadonlyMap<string, Type.Type>,
        potentials: readonly FormalPotential[],
        variables: readonly FormalVariable[],
        endGridID: number,
    }> {}
    
    export interface FormalGrid extends Readonly<{
        id: number,
        alphabet: Readonly<{key: string, map: ReadonlyIDMap<string>, wildcard: ISet}>,
        scaleX: number,
        scaleY: number,
        periodic: boolean,
        convPatterns: ReadonlyIDMap<ConvPattern>,
        pos: SourcePosition,
    }> {}
    
    export interface FormalLimit extends Readonly<{
        id: number,
        /**
         * An expression determining the limit's initial value.
         */
        initialiser: Prop<'int'>,
        canReset: boolean,
        /**
         * A limit is transparent if its parent block is a `sequence` and its
         * initialiser is a constant `1`. A transparent limit can be elided
         * when the CFG is built, as its state is deterministic at every node
         * in the CFG.
         */
        isTransparent: boolean,
    }> {}
    
    export interface ConvPattern extends Readonly<{
        kernel: Convolution.Kernel,
        chars: ISet,
        includeBoundary: boolean,
    }> {}
    
    export interface FormalPotential extends Readonly<{
        id: number,
        inGrid: number,
        for_: ASG.Prop<'charset.in'>,
    }> {}
    
    export interface FormalVariable extends Readonly<{
        id: number,
        name: string,
        type: Type.Type,
        flags: ExprFlags,
        initialiser: Expression | undefined,
        references: number,
    }> {}
    
    export type Node = Expression | Rule | Statement
    export type Kind = Node['kind']
    
    export type Expression = AttributeExpr | DeclarationExpr | OpExpr | PrimaryExpr
    export type Rule = FieldRule | ObserveRule | RewriteRule
    export type Statement = BlockStmt | BranchingStmt | ModifiedStmt | NonBranchingStmt
    
    export type PropSpec = `${'const ' | ''}${PropTypeSpec}${'?' | ''}`
    export type PropTypeSpec = keyof _PropTypeMap
    type _PropTypeMap = {
        'bool': 'bool',
        'charset.in': 'pattern.in',
        'charset.out': 'pattern.out',
        'dict': 'dict',
        'float': 'float',
        'fraction': 'fraction',
        'grid': 'grid',
        'int': 'int',
        'object': 'dict' | 'grid' | 'position',
        'pattern.in': 'pattern.in',
        'pattern.out': 'pattern.out',
        'position': 'position',
        'str': 'str',
        'str~': 'str',
    }
    type _PropConstant<S extends PropTypeSpec> = Type.Value<_PropTypeMap[S]>
    type _PropExpression<S extends PropTypeSpec> = (Exclude<Expression, ConstantExpr> & {type: Type.OfKind<_PropTypeMap[S]>}) | ConstantExpr<_PropTypeMap[S]>
    export type Prop<T extends PropSpec>
        = T extends `const ${infer S extends PropTypeSpec}?` ? _PropConstant<S> | undefined
        : T extends `const ${infer S extends PropTypeSpec}` ? _PropConstant<S>
        : T extends `${infer S extends PropTypeSpec}?` ? _PropExpression<S> | undefined
        : T extends PropTypeSpec ? _PropExpression<T>
        : never
    
    type _Node<K extends string, T> = Readonly<{kind: K, pos: SourcePosition} & T>
    
    // expressions
    type _ExprNode<K extends string, T> = _Node<`expr.${K}`, {type: Type.Type, flags: ExprFlags} & T>
    
    export type AttributeExpr = DictAttributeExpr | GridAttributeExpr | PositionAttributeExpr
    export interface DictAttributeExpr extends _ExprNode<'attr.dict', {left: Prop<'dict'>, attr: string}> {}
    export interface GridAttributeExpr extends _ExprNode<'attr.grid', {grid: number, attr: Type.GridAttribute}> {}
    export interface PositionAttributeExpr extends _ExprNode<'attr.position', {left: Prop<'position'>, attr: Type.PositionAttribute}> {}
    
    type OpExpr = BinaryOpExpr | CountExpr | RandIntExpr | SumExpr | TernaryExpr | UnaryOpExpr
    export interface BinaryOpExpr extends _ExprNode<'op.binary', {op: Op.BinaryOp, left: Expression, right: Expression}> {}
    export interface CountExpr extends _ExprNode<'count', {inGrid: number, patterns: readonly PatternTree[]}> {}
    export interface RandIntExpr extends _ExprNode<'randint', {max: Prop<'int'>}> {}
    export interface SumExpr extends _ExprNode<'sum', {inGrid: number, patternID: number}> {}
    export interface TernaryExpr extends _ExprNode<'op.ternary', {condition: Prop<'bool'>, then: Expression, otherwise: Expression}> {}
    export interface UnaryOpExpr extends _ExprNode<'op.unary', {op: Op.UnaryOp, child: Expression}> {}
    
    type PrimaryExpr = Simplify<ConstantExpr> | DictExpr | KeywordNameExpr | ParamExpr | SimpleNameExpr
    export type ConstantExpr<K extends Type.Kind = Type.Kind> = K extends unknown ? _ExprNode<'constant', {type: Type.OfKind<K>, constant: Type.ConstantValue<K>}> : never
    export interface DictExpr extends _ExprNode<'dict', {type: Type.OfKind<'dict'>, entryExprs: ReadonlyMap<string, Expression>}> {}
    export interface KeywordNameExpr extends _ExprNode<'name.keyword', {name: AST.KeywordName}> {}
    export interface ParamExpr extends _ExprNode<'param', {name: string, otherwise: Expression}> {}
    export interface SimpleNameExpr extends _ExprNode<'name.simple', {variableID: number}> {}
    
    export interface DeclarationExpr extends _ExprNode<'decl', {decl: AssignStmt, child: Expression}> {}
    
    // rules
    type _RuleNode<K extends string, T> = _Node<`rule.${K}`, T>
    export interface FieldRule extends _RuleNode<'field', {potential: FormalPotential, for_: ASG.Prop<'charset.in'>, zero: ASG.Prop<'charset.in'>, on: ASG.Prop<'charset.in'>, inversed: boolean, essential: boolean, recompute: boolean}> {}
    export interface ObserveRule extends _RuleNode<'observe', {from: Prop<'const pattern.in'>, via: Prop<'pattern.out?'>, to: Prop<'pattern.out'>, condition: Prop<'bool'>}> {}
    export interface RewriteRule extends _RuleNode<'rewrite', {from: Prop<'const pattern.in'>, to: Prop<'pattern.out'>, condition: Prop<'bool'>}> {}
    
    // statements
    type _StmtNode<K extends string, T> = _Node<`stmt.${K}`, T>
    
    export interface BlockReset extends Readonly<{
        limitIDs: readonly number[],
    }> {}
    
    export type BlockStmt = MarkovStmt | SequenceStmt
    type _BlockStmtNode<K extends string> = _StmtNode<`block.${K}`, {children: readonly Statement[], reset: BlockReset | undefined}>
    export interface MarkovStmt extends _BlockStmtNode<'markov'> {}
    export interface SequenceStmt extends _BlockStmtNode<'sequence'> {}
    
    export type ModifiedStmt = LimitStmt
    export interface LimitStmt extends _StmtNode<'modified.limit', {limit: FormalLimit, child: BlockStmt | BranchingStmt | ModifiedStmt}> {}
    
    export type BranchingStmt = BranchingRulesStmt | ConvChainStmt | PathStmt
    
    export type BranchingRulesStmt = BasicRulesStmt | BiasedRulesStmt | ConvolutionStmt | SearchRulesStmt
    type _RulesStmtNode<K extends string, T> = _StmtNode<`rules.${K}`, {inGrid: number, rewrites: readonly RewriteRule[]} & T>
    export interface BasicRulesStmt extends _RulesStmtNode<'basic.all' | 'basic.one' | 'basic.prl', {commutative: boolean}> {}
    export interface BiasedRulesStmt extends _RulesStmtNode<'biased.all' | 'biased.one', {temperature: Prop<'float?'>, fields: readonly FieldRule[], observations: readonly ObserveRule[]}> {}
    export interface ConvolutionStmt extends _RulesStmtNode<'convolution', {kernel: Convolution.Kernel, boundary: Prop<'const charset.in?'>}> {}
    export interface SearchRulesStmt extends _RulesStmtNode<'search.all' | 'search.one', {temperature: Prop<'float?'>, maxStates: Prop<'int?'>, depthCoefficient: Prop<'float?'>, observations: readonly ObserveRule[]}> {}
    
    export interface ConvChainStmt extends _StmtNode<'convchain', {inGrid: number, sample: Type.Value<'pattern.out'>, n: number, temperature: Prop<'float?'>, on: Prop<'charset.in'>, periodic: boolean | undefined}> {}
    export interface PathStmt extends _StmtNode<'path', {inGrid: number, from: Prop<'charset.in'>, to: Prop<'charset.in'>, input: Prop<'charset.in'>, output: Prop<'charset.out'>, longest: Prop<'bool?'>, inertia: Prop<'bool?'>}> {}
    
    export type NonBranchingStmt = AssignStmt | LogStmt | MapStmt | PutStmt | UseStmt
    export interface AssignStmt extends _StmtNode<'assign', {variable: FormalVariable, rhs: Expression}> {}
    export interface LogStmt extends _StmtNode<'log', {expr: Prop<'str'>}> {}
    export interface MapStmt extends _RulesStmtNode<'map', {outGrid: number}> {}
    export interface PutStmt extends _StmtNode<'put', {inGrid: number, pattern: Prop<'pattern.out'>, at: Prop<'position'>, condition: Prop<'bool?'>}> {}
    export interface UseStmt extends _StmtNode<'use', {grid: number}> {}
}

const enum ExprFlags {
    RUNTIME_CONSTANT = 1,
    DETERMINISTIC = 2,
    LOCALLY_DETERMINISTIC = 4,
    POSITION_INDEPENDENT = 8,
    GRID_INDEPENDENT = 16,
    
    SAME_EVERYWHERE = LOCALLY_DETERMINISTIC | POSITION_INDEPENDENT,
    CONSTANT = RUNTIME_CONSTANT | DETERMINISTIC | LOCALLY_DETERMINISTIC | POSITION_INDEPENDENT | GRID_INDEPENDENT,
    ALL = 31,
}
