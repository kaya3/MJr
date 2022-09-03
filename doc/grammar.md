# Grammar

The grammar for the MJr programming language has five main parts:

- A **compilation unit** is parsed by the `root` method; it is a sequence of statements.
- **Declarations** are parsed by the `decl` method. 
- **Expressions** are parsed by the `expr` method. They may contain sub-expressions, and declarations.
- **Rules** are parsed by the `rule` method. They may contain expressions and declarations.
- **Statements** are parsed by the `stmt` method. They may contain sub-statements, rules, expressions and declarations.

The grammar assumes a filtered token stream, in which `WHITESPACE` and `COMMENT` tokens are removed, `NEWLINE` tokens inside brackets are removed, and consecutive `NEWLINE` tokens are collapsed into one. Newlines which occur between parentheses, square brackets or braces are also removed. Additionally, a `NEWLINE` token is assumed to be present before the `EOF` (end-of-file) token, even if the input source code does not have a trailing newline.


## Parsing expression grammar

The language grammar is formally described by the following [parsing expression grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) (PEG).

In the following grammar, `UpperCamelCase` indicates a non-terminal symbol, `UPPER_SNAKE_CASE` indicates a token of the respective kind, and `'quotes'` indicate a literal token. The symbol `|` is used for ordered choice, but to the best of my knowledge, the grammar is unambiguous even if it is taken to mean unordered choice.

```
CompilationUnit = Statement* EOF

Args = NameValuePairs?

NameValuePairs = '{' (NameValuePair ',')* NameValuePair? '}'
NameValuePair  = SimpleNameExpr '=' Expression

CharSet = '[' Char+ ']'
Char    = PATTERN_CHAR
```

## Declarations

```
Declaration = LegendDecl | LetDecl | SymmetryDecl | UnionDecl

LegendDecl   = 'legend' Expression
LetDecl      = 'let' 'param'? SimpleNameExpr '=' Expression
SymmetryDecl = 'symmetry' Expression
UnionDecl    = 'union' PatternExpr '=' Expression
```

## Expressions

```
Expression = TernaryExpr

TernaryExpr     = OrExpr ('if' OrExpr 'else' TernaryExpr)?
OrExpr          = (OrExpr 'or')? AndExpr
AndExpr         = (AndExpr 'and')? NotExpr
NotExpr         = 'not' NotExpr | CmpExpr
CmpExpr         = (AddExpr CmpOp)? AddExpr
AddExpr         = (AddExpr AddOp)? MultExpr
MultExpr        = (MultExpr MultOp)? UnaryExpr
UnaryExpr       = UnaryOp UnaryExpr | FuncExpr
FuncExpr        = FuncOp FuncExpr | BracketedExpr
BracketedExpr   = '(' DeclarationExpr ')' | PrimaryExpr
DeclarationExpr = Declaration 'in' DeclarationExpr | Expression

CmpOp   = '==' | '!=' | '<' | '<=' | '>' | '>='
AddOp   = '+' | '-'
MultOp  = '*' | '/' | '//' | '%'
UnaryOp = '+' | '-'
FuncOp  = 'count' | 'load' | 'randint' | 'sum'

PrimaryExpr = DictExpr | GridExpr | LiteralExpr | NameExpr
DictExpr    = NameValuePairs
GridExpr    = 'grid' Args PatternExpr

LiteralExpr        = BoolLiteralExpr | FloatLiteralExpr | IntLiteralExpr | PatternLiteralExpr | StringLiteralExpr
BoolLiteralExpr    = 'false' | 'true'
FloatLiteralExpr   = FLOAT
IntLiteralExpr     = INT
PatternLiteralExpr = '[' (Char | CharSet)+ ('/' (Char | CharSet)+)* ']'
StringLiteralExpr  = QUOTE (STRING_CHAR | ESCAPED_CHAR)* QUOTE

NameExpr        = AttributeExpr | KeywordNameExpr | SimpleNameExpr
AttributeExpr   = NameExpr '.' NAME
KeywordNameExpr = 'at' | 'origin' | 'random'
SimpleNameExpr  = NAME
```

## Rules

```
Rule = DeclarationRule | FieldRule | ObserveRule | RewriteRule

DeclarationRule = Declaration (NEWLINE Rule* | 'in' RuleBlockChildren)
FieldRule       = 'field' Args NEWLINE
ObserveRule     = 'observe' OrExpr '->' OrExpr ('->' OrExpr)? ('if' OrExpr)? NEWLINE
RewriteRule     = OrExpr '->' OrExpr ('if' OrExpr)? NEWLINE

RuleBlockChildren = ':' (Rule | NEWLINE INDENT Rule+ DEDENT)
```

## Statements

```
Statement = BareUseStmt | DeclarationStmt | LogStmt | ModifiableStmt | PassStmt | UseStmt

ModifiableStmt = BlockStmt | ConvChainStmt | ModifiedStmt | PathStmt | PutStmt | RuleBlockStmt
BlockStmt      = MarkovStmt | SequenceStmt
RuleBlockStmt  = AllStmt | ConvolutionStmt | MapStmt | OnceStmt | OneStmt | PrlStmt
ModifiedStmt   = LimitStmt

AllStmt         = 'all' Args RuleBlockChildren
BareUseStmt     = GridExpr NEWLINE
ConvChainStmt   = 'convchain' Args NEWLINE
ConvolutionStmt = 'convolution' Args RuleBlockChildren
DeclarationStmt = Declaration (NEWLINE Statement* | 'in' StmtBlockChildren)
LimitStmt       = '@' 'limit' Expression NEWLINE ModifiableStmt
LogStmt         = 'log' Expression NEWLINE
MapStmt         = 'map' Args RuleBlockChildren
MarkovStmt      = 'markov' StmtBlockChildren
OnceStmt        = 'once' RuleBlockChildren
OneStmt         = 'one' Args RuleBlockChildren
PassStmt        = 'pass' NEWLINE
PathStmt        = 'path' Args NEWLINE
PrlStmt         = 'prl' RuleBlockChildren
PutStmt         = 'put' OrExpr 'at' OrExpr ('if' OrExpr)? NEWLINE
SequenceStmt    = 'sequence' StmtBlockChildren
UseStmt         = 'use' Expression NEWLINE | 'use' LetDecl NEWLINE Statement*

StmtBlockChildren = ':' (Statement | NEWLINE INDENT Statement+ DEDENT)
```
