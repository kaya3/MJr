# Statements

Syntactically, there are three categories of statements: control-flow statements which contain sub-statements, statements which contain [rules](rules.md), and line statements. Additionally, [declarations](declarations.md) may occur where statements are expected.

When a block may have multiple children, these must occur either in an indented block after a colon '`:`', or if there is only one child it may occur on the same line, after the colon.

Statements in MJr are said to "return true" or "return false"; this determines the program's [control flow](https://en.wikipedia.org/wiki/Control_flow). A statement typically "returns true" if it makes any changes to the current grid's state.

A compilation unit is treated like a ['sequence' statement](#sequence-statements).


## Control-flow statements

### 'Markov' statements

A `markov` statement executes its children in order of priority, returning back to the first child each time any child statement "returns true". The `markov` statement itself "returns true" if any of its children did.

<pre>
markov:
    <i>Statement+</i>
</pre>

### 'Sequence' statements

A `sequence` statement executes each child repeatedly until it "returns false", then moves onto the next child. The `sequence` statement itself "returns true" if any of its children did.

<pre>
sequence:
    <i>Statement+</i>
</pre>

### 'Limit' statements

A `@limit` modifier is followed by an expression, which must be a [runtime constant](resolver.md#exprflags) of type `int`, and then a sub-statement on the next line. The value of the expression determines the initial value of a "limit counter" which is initialised each time the parent block is entered, and counts down each time the modified statement "returns true". Once the counter reaches zero, the limit statement "returns false" without executing its sub-statement.

A compilation error occurs if the sub-statement is another limit statement, a `once` statement, or a kind of statement which always "returns false".

<pre>
@limit <i>Expression</i>
<i>Statement</i>
</pre>


## Statements with rules

### 'One' and 'Once' statements

A `one` statement has a set of rewrite rules, and pseudorandomly chooses an applicable match of one of them. If an applicable match exists, that rewrite is applied to the grid and the statement "returns true"; otherwise it "returns false".

<pre>
one:
    <i>Rule+</i>
</pre>

<!-- TODO -->

<pre>
one {<i>Arguments</i>}:
    <i>Rule+</i>
</pre>

As a shorthand syntax, the keyword `once` may be written instead of `one`; this is equivalent to a `one` statement with a [limit](#limit-statements) of 1. A `once` statement has no arguments.

<pre>
once:
    <i>Rule+</i>
</pre>

### 'All' statements

An `all` statement has a set of rewrite rules, and applies rewrites in parallel. If any applicable matches exist, then a maximal subset of them is pseudorandomly chosen such that output patterns do not overlap, the rules are applied on these matches, and the statement "returns true"; otherwise, it returns "false".

<pre>
all:
    <i>Rule+</i>
</pre>

<!-- TODO -->

<pre>
all {<i>Arguments</i>}:
    <i>Rule+</i>
</pre>

### 'Prl' statements

A `prl` statement is similar to an `all` statement, except that overlapping output patterns are not prevented. If there are any applicable matches, then the rewrite rules are applied on every match in parallel, with any overlapping output patterns written in a pseudorandom order, and the statement "returns true"; otherwise, it "returns false".

A `prl` statement has no arguments.

<pre>
prl:
    <i>Rule+</i>
</pre>

### 'Convolution' statements

<!-- TODO -->

<pre>
convolution {kernel=<i>Expression</i>}:
    <i>Rule+</i>
</pre>

### 'Map' statements

A `map` statement has a set of rewrite rules whose input patterns are matched in an "input grid" and output patterns are written to a separate "output grid". The input grid is the current grid, and the output grid is specified as an argument. The input and output grids may have different scales, in which case the input and output patterns of each rewrite rule must be scaled in the same proportion, and the position of the match in the input grid is scaled to the output grid in the same proportion.

A `map` statement always "returns false", because it may make changes to `outGrid` but not the *current* grid. However, after a `map` statement executes, `outGrid` becomes the current grid.

A compilation error occurs if `outGrid` is the current grid.

<pre>
map {outGrid=<i>Expression</i>}:
    <i>Rule+</i>
</pre>


## Line statements

### 'ConvChain' statements

<!-- TODO -->

<pre>
convchain {<i>Arguments</i>}
</pre>

### 'Log' statements

A `log` statement logs a the value of an expression to the standard output stream (i.e. the console or terminal), and always "returns false". The expression's type must be one of `bool`, `float`, `fraction`, `grid`, `int` or `str`.

<pre>
log <i>Expression</i>
</pre>

### 'Pass' statements

A `pass` statement does nothing, and always "returns false". They may be used as placeholders in blocks which are not yet written.

<pre>
pass
</pre>

### 'Path' statements

<!-- TODO -->

<pre>
path {<i>Arguments</i>}
</pre>

### 'Put' statements

A `put` statement writes a pattern to the current grid at a given position, if it is not already present there. The position determines the top-left corner of where the pattern should be written. The pattern's type must be an [output pattern type](types.md#pattern-types).

A `put` statement always "returns false", even when writing the pattern changes the grid. This is because even when a `put` statement is not idempotent, it is normally intended only execute once per execution of their containing block.

<pre>
put <i>Expression</i> at <i>Expression</i>
</pre>

A `put` statement may have a condition, which must be an expression of type `bool`. If the condition is true, then the statement is executed as above; otherwise the pattern is not written.

<pre>
put <i>Expression</i> at <i>Expression</i> if <i>Expression</i>
</pre>

### 'Use' statements

A `use` statement changes the current grid to the value of an expression, which must be a compile-time constant of type `grid`. The context change persists beyond the current block. The statement always "returns false".

<pre>
use <i>Expression</i>
</pre>

If the expression is a [grid expression](expressions.md#grid-expressions), then the keyword `use` may be omitted. This is a shorthand syntax, referred to as a "bare 'use' statement".

<pre>
grid [<i>Alphabet</i>]
</pre>
<pre>
grid {<i>Arguments</i>} [<i>Alphabet</i>]
</pre>

In order to access the grid's attributes (i.e. its width and height) or otherwise refer to the grid elsewhere, it is necessary to declare a variable holding a reference to the grid. This can be done with a declaration such as `let g = grid ...` followed by the statement `use g`, or with the following shorthand syntax, referred to as a `use let` statement:

<pre>
use let <i>Name</i> = <i>Expression</i>
</pre>

The `use let` statement is equivalent to the combination of a `let` declaration and a `use` statement; in particular, the variable is only in scope until the end of the current block, but the context change of the `use` statement persists beyond the current block.


## Declaration statements

If a [declaration](declarations.md) occurs in a block of statements, then the declaration is in effect for the statements following it within that block.

<pre>
<i>Declaration</i>
<i>Statement*</i>
</pre>

Alternatively, the declaration may be followed by the keyword `in`, a colon '`:`' and a block of child statements, in which case the declaration is in effect for the statements in that block.

<pre>
<i>Declaration</i> in:
    <i>Statement+</i>
</pre>