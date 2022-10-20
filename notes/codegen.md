# Code Generators

The MJr compiler supports multiple target languages through the use of a language-agnostic [intermediate representation](./ir.md) (IR), and a number of "code generator" classes which each convert the IR to a target language.

Code generator classes inherit from the abstract base class `CodeGen.Base`. To write a code generator for a new target language, the subclass must have:

- A constructor which accepts a `Compiler.Config` object, or no constructor (so that the base class's constructor is inherited).
- A `STMT_WRITE_FUNCS` property, which is an object containing functions for writing each kind of IR statement.
- An `EXPR_WRITE_FUNCS` property, which is an object containing precedences and functions for writing each kind of IR expression, excluding unary and binary operator expressions.
- A `BINARY_OPS` property, which is an object containing "specs" for each IR binary operator. The "specs" can be created using the `binaryOp` function, or the `infixOp` convenience function for infix operators.
- A `UNARY_OPS` property, which is an object containing "specs" for each IR unary operator. The "specs" can be created using the `unaryOp` function, or the `prefixOp` convenience function for prefix unary operators.
- If the target language supports assignment expressions, then the subclass should also provide a `writeAssignExpr` method, which will be used to generate code for "let in" expressions. Otherwise, these expressions must be handled in `EXPR_WRITE_FUNCS`.

Expression precedences from `EXPR_WRITE_FUNCS`, `BINARY_OPS` and `UNARY_OPS` are used by the base class to write parentheses around expressions when necessary. If some target language uses different symbols instead of '`(`' and '`)`' for this purpose, they should be provided as the subclass's `LPAREN` and `RPAREN` properties.
