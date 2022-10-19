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

A `convolution` statement applies a set of 1x1 rewrite rules to all matches in parallel, where the rules may make use of ['sum' expressions](expressions.md#sum-operator). There is one required argument, `kernel`, which determines the convolution kernel used by `sum` expressions within the statement. It must be a compile-time constant `str` equal to one of the following names:

- `Moore`: a [Moore neighbourhood](https://en.wikipedia.org/wiki/Moore_neighborhood).
- `VonNeumann`: a [Von Neumann neighbourhood](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood).

Additionally, there is an optional argument, `boundary`, which must be a compile-time constant 1x1 pattern. If present, cells outside of the grid are treated as if they take values from the subset of the alphabet defined by `boundary`. A compilation error occurs if `boundary` is specified for a periodic grid.

The statement "returns true" if there were any applicable matches, otherwise it "returns false".

<pre>
convolution {<i>Arguments</i>}:
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

A `convchain` statement runs the [ConvChain algorithm](https://github.com/mxgmn/ConvChain) to generate an image which is locally similar to a given sample pattern, by pseudorandomly replacing some grid cells. It has the following arguments:

- `sample`: the sample image. Must be a compile-time constant `pattern.out` with at least two distinct alphabet symbols, and no wildcards.
- `n`: the size of subpatterns used to determine "local similarity". Must be a compile-time constant `int` which is at least 2 and at most the width and height of `sample`.
- `periodic`: an optional `bool` indicating whether `sample` is a periodic image (default `true`). Must be a compile-time constant.
- `on`: an optional `charset.in` determining which grid cells may be modified (default `[.]`). Must be a compile-time constant.
- `epsilon`: an optional `float` which determines the weight of subpatterns which are not present in `sample` (default `0.125`). Must be a compile-time constant, and positive.
- `temperature`: an optional `float` which controls the probability of changes which decrease local similarity (default `1.0`). Must be non-negative.
- `anneal`: an optional `float` which is subtracted from the temperature on each iteration (default `0.0`). Must be non-negative.

The alphabet symbols present in `sample` form the "output set".

On the first iteration of a `convchain` statement, a set of grid cells is initialised containing those cells which match the pattern `on`. Each cell in this set which does not already have a symbol in the output set is replaced with pseudorandom symbol from the output set. If any replacements were made, then the statement completes and "returns true", otherwise it continues executing as below.

After initialisation, on each iteration the grid cells which originally matched `on` are pseudorandomly replaced with different symbols the output set, biased towards local similarity with `sample`. The statement "returns true" if any replacements were made, and "returns false" otherwise.

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

A `put` statement always "returns false", even when writing the pattern changes the grid. This is because even when a `put` statement is not idempotent, it is normally not intended to repeat.

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
