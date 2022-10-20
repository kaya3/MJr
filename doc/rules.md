# Rules

## Rewrite rules

A *rewrite rule* has an input pattern which can be matched in a grid, and an output pattern which can be written to a grid. The input pattern must be a compile-time constant [pattern](types.md#pattern-types) using the current alphabet.

In all statements except for `map` statements, a rewrite rule writes to the same grid that the input pattern is matched in, and the output pattern must have the same dimensions as the input pattern. A rewrite rule is never applied if the output pattern is already present at the match position.

When a rewrite rule occurs in a ['map' statement](statements.md#map-statements), the output pattern is written to a different grid which may have a different scale, and the input and output pattern dimensions must be in the same ratio as the input and output grid scales. The output pattern will be written to a position in the output grid according to the match position and the scale ratio. The rewrite rule will not be applied if the output pattern is already present at the scaled match position.

The output pattern expression is a [rule context](#rule-contexts).

<pre>
<i>Expression</i> -> <i>Expression</i>
</pre>

A rewrite rule may have a condition, which must be an expression of type `bool`. A rule with a condition may only be applied for a given match if the condition is true for that match.

The condition expression is a [rule context](#rule-contexts).

<pre>
<i>Expression</i> -> <i>Expression</i> if <i>Expression</i>
</pre>


## 'Field' rules

<!-- TODO -->

<pre>
field {<i>Arguments</i>}
</pre>


## 'Observe' rules

<!-- TODO -->

<pre>
observe <i>Expression</i> -> <i>Expression</i>
</pre>

<pre>
observe <i>Expression</i> -> <i>Expression</i> -> <i>Expression</i>
</pre>

<pre>
observe <i>Expression</i> -> <i>Expression</i> if <i>Expression</i>
</pre>
<pre>
observe <i>Expression</i> -> <i>Expression</i> -> <i>Expression</i> if <i>Expression</i>
</pre>


## Declaration rules

If a [declaration](declarations.md) occurs in a block of rules, then the declaration is in effect for the rules following it within that block.

<pre>
<i>Declaration</i>
<i>Rule*</i>
</pre>

Alternatively, the declaration may be followed by the keyword `in`, a colon '`:`' and a block of child rules, in which case the declaration is in effect for the rules in that block.

<pre>
<i>Declaration</i> in:
    <i>Rule+</i>
</pre>


## Rule contexts

A *rule context* is a lexical context in which there is a "current position", generally the position at which an input pattern may match and/or an output pattern may be written. Some kinds of expression may appear only in rule contexts:

- The [keyword name](expressions.md#keyword-name-expressions) `at`.
- The [sum operator](expressions.md#sum-operator).
 
The following contexts are rule contexts:

- The output pattern and condition of a [rewrite rule](#rewrite-rules), where the "current position" is the top-left corner of some match of the input pattern.
- The 'via' pattern, output pattern and condition of an ['observe' rule](#observe-rules), likewise.
- The output pattern and condition of a ['put' statement](statements.md#put-statements), where the "current position" is specified by an expression.
