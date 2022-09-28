# Expressions

Expressions never have any side-effects when they are evaluated.


## Literal expressions

### Int literal expressions
A non-empty seqence of decimal digits, optionally preceded by a `-` sign. The value must be in the range for a signed 32-bit integer, and has type `int`.

### Float literal expressions
A non-empty sequence of decimal digits, followed by a decimal point '`.`', followed by a non-empty sequence of decimal digits. The value has type `float`.

### String literal expressions
A sequence of characters delimited by either `'` or `"`. Characters may be escaped by a backslash `\`, and the escape sequences `\n` and `\t` mean newline and tab characters respectively. The value has type `str`.

### Dict literal expressions
A sequence of `key = value` pairs delimited by braces `{` and `}`, separated by commas, with an optional trailing comma before the closing brace. The keys must be simple names, and cannot be keywords. The value has a [dict type](types.md#dict-types).

### Pattern literal expressions
A non-empty sequence of pattern characters or character-sets, delimited by `[` and `]`, with rows separated by `/`. The character '`.`' means a wildcard which matches any alphabet symbol when it occurs in an input pattern, and writes nothing to the grid when it occurs in an output pattern. Character-sets are also delimited by `[` and `]`, and may not contain wildcards.


## Name expressions

### Simple name expressions

A *simple name* is an identifier, and refers to a variable of that name. A compilation error occurs if no such variable exists in the lexical context.

### Keyword name expressions

A *keyword name* is one of the following:

- `at` refers to the "current position" which is defined in a [rule context](rules.md#rule-contexts). A compilation error occurs if this keyword is used outside of a rule context.
- `origin` refers to the position in the centre of the current grid. If the grid's dimensions are even, then the origin is biased towards the bottom right corner, not the top left. A compilation error occurs if this keyword is used in a context without a current grid.
- `random` evaluates as a pseudorandom `float` value between 0 (inclusive) and 1 (exclusive).

### Attribute expressions

An *attribute expression* is of the form `object.attribute`, where `attribute` is an identifier, and `object` must be either a simple name, a keyword name or itself an attribute expression. The type of `object` must be either:

- A `dict` type, in which case the attribute name must be one of the dictionary type's keys,
- `grid`, in which case the attribute must be either `width` or `height` (and the resulting type is `int`), or
- A `position` type, in which case the attribute name must be either `x` or `y` (and the resulting type is `int`).


## Grid expressions

A *grid expression* is of the form <code>grid [<i>Alphabet</i>]</code> or <code>grid {<i>Arguments</i>} [<i>Alphabet</i>]</code>. Its result is a reference to a new grid; however, the expression will always refer to the *same* new grid however many times it is evaluated. (This is so that grids never need to be dynamically allocated.) The alphabet must be a literal pattern containing no repeated characters, wildcards, row separators or character-sets.

The allowed arguments are listed below, and may be present in any combination; they must be compile-time constants.

- `scaleX`: the scaling of the grid's width relative to the program's `width` parameter, as an `int` (default 1).
- `scaleY`: the scaling of the grid's height relative to the program's `height` parameter, as an `int` (default 1).
- `periodic`: a `bool` flag indicating whether the grid's edges "wrap around" like a torus (default `false`). This flag is currently unsupported.

A grid expression may also occur as a [bare 'use' statement](statements.md#use-statements), in which case the new grid is also set as the context's current grid.


## Operator expressions

Operators in the table below are binary unless shown otherwise via their syntax; see also [operator precedence](../notes/parser.md#operator-precedence). The "numeric" types are `int`, `float` and `fraction`.

| Operator | Operand types | Meaning |
| --- | --- | --- |
| `+` | Numeric or `str` | Addition for numeric types; concatenation for `str` |
| `-` | Numeric | Subtraction |
| `*` | Numeric | Multiplication |
| `/` | Numeric | True division; if both operands are `int`, result is `fraction` |
| `//` | `int` | Floor division; rounds towards negative infinity, not zero |
| `%` | `int` or `float` | Modulo; result has same sign as second operand, not the first |
| `==`, `!=` | Numeric or `str` | Comparison; result is `bool` |
| `<`, `<=`, `>`, `>=` | Numeric | Comparison; result is `bool` |
| `and`, `or` | `bool` or `pattern.in` | Logical operations |
| `+`X | Numeric | Has no effect; it is included for symmetry with unary `-`, and by tradition |
| `-`X | Numeric | Negation |
| `not` X | `bool` | Logical negation |
| `count` X | `pattern.in` | [Count operator](#count-operator) |
| `load` X | `str` | [Load operator](#load-operator) |
| `randint` X | `int` | [Randint operator](#randint-operator) |
| `sum` X | 1x1 `pattern.in` | [Sum operator](#sum-operator) |
| A&nbsp;`if`&nbsp;condition&nbsp;`else`&nbsp;B | condition:&nbsp;`bool` | Conditional expression; A and B must have a common type (after [coercion](types.md#type-coercion)). |

The binary operators `+`, `-`, `*`, `//` and the unary operator `-` may overflow on values of type `int`, such that the result is within the signed 32-bit integer range.

A runtime error occurs if the second operand of a binary `/`, `//` or `%` expression is zero.

### Count operator

The `count` operator returns the number of matches of a given pattern, as an `int`. The count includes matches of symmetries of the given pattern, according to the context's current symmetry group. The operand must be a compile-time constant [pattern](types.md#pattern-types) for the current alphabet.

A compilation error occurs if the `count` operator is used in a context without a current grid.

### Load operator

The `load` operator loads a pattern from an image file, using the current [legend](declarations.md#legend-declarations) to translate pixel colours into pattern characters or character-sets.

The image file's path must be a compile-time constant of type `str`. A compilation error occurs if no legend is declared in the expression's lexical context.

### Randint operator

The `randint` operator generates a pseudorandom `int` value between zero (inclusive) and the operand's value (exclusive). The operand must be of type `int`.

A runtime error occurs if the operand's value is less than or equal to zero; a compilation error occurs if the operand is a compile-time constant with value less or equal to zero.

### Sum operator

The `sum` operator may only be used within a [convolution statement](statements.md#convolution-statements). The result of the expression is the number of neighbours of the "current position" (as defined in a [rule context](rules.md#rule-contexts)) which match the given 1x1 pattern, as an `int`. The `kernel` argument of the `convolution` statement determines which cells are "neighbours".

The operand must be a compile-time constant 1x1 [pattern](types.md#pattern-types) for the current alphabet. A compilation error occurs if the `sum` operator is used outside of a [rule context](rules.md#rule-contexts), or outside of a `convolution` statement.


## Declaration expressions

A *declaration expression* is of the form (D `in` E), where E is a sub-expression and D is a [declaration](declarations.md) which takes effect in E's lexical context.

A declaration expression must be written in parentheses, unless it is the immediate sub-expression of another declaration expression; see also [operator precedence](../notes/parser.md#operator-precedence).
