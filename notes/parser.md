# Parser

The parser converts a sequence of tokens into an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST). Type definitions for the AST are found in [ast.ts](../src/compiler/ast.ts). See also the MJr language [grammar](grammar.md).

The parser is hand-written, to avoid a separate stage of converting a parse tree into an AST, and hopefully to give more helpful syntax error messages. It is a top-down [recursive descent parser](https://en.wikipedia.org/wiki/Recursive_descent_parser) without backtracking (also known as a predictive parser), with a single-token lookahead; expressions with unary and binary operators are parsed by an [operator precedence parser](https://en.wikipedia.org/wiki/Operator-precedence_parser).

For AST nodes with "arguments" (name/value pairs in braces), the `parseArgs` method is used to avoid repetition of code. The constant `ARGS` specifies the argument names for nodes which have arguments, and maps each argument name to a boolean indicating whether that argument is required.


## Operator precedence

The following table shows the precedences of operators, plus some other syntactic constructs which are similar to operators and which occur at the boundaries of some expressions.

| Operator | Description | Associativity |
| --- | --- | --- |
| `(`…`)`, `[`…`]`, `{`…`}` | Highest precedence | — |
| `x.attr` |  Not an operator; `x` must be a simple name, keyword name or attribute access expression | Left |
| `load`, `randint`, `sum` | Function expressions | — |
| `+x`, `-x` | Unary positive, unary negative | — |
| `*`, `/`, `//`, `%` | Multiplication, division, floor-division, modulo | Left |
| `+`, `-` | Addition and subtraction | Left |
| `==`, `!=`, `<`, `<=`, `>`, `>=` | Comparisons | Neither |
| `not` | Logical negation | — |
| `and` | Logical conjunction | Left |
| `or` | Logical disjunction | Left |
| `->` | Not an operator; delimits patterns in rules | — |
| …&nbsp;`if`&nbsp;…&nbsp;`else`&nbsp;… | Conditional expression | Right |
| `=` | Not an operator | — |
| `,` | Not an operator; delimits name/value pairs | — |
| Declaration&nbsp;`in`&nbsp;… | Lowest precedence. Not an operator; may only occur in bracketed expressions | Right |

The comparison operators are neither left- nor right-associative. This is intended to catch mistakes like `a < b < c` in the parser, so that a specific syntax error message can be emitted instead of a less helpful type error. Additionally, by not allowing otherwise-valid expressions like `a == b == c` (where `c` is a `bool`), this design decision also leaves open the possibility to later add chained comparisons to the language without breaking existing programs.
