# Declarations

A declaration has some effect in its lexical scope. Declarations may be attached to [statements](statements.md#declaration-statements), [rules](rules.md#declaration-rules) or [expressions](expressions.md#declaration-expressions).


## 'Legend' declarations

A `legend` declaration declares a legend which is used by ['load' expressions](expressions.md#load-operator) in the declaration's lexical scope. The legend must be a compile-time constant [pattern](types.md#pattern-types) of height 1 using the current alphabet.

<!-- TODO -->

`legend` declarations are currently useless, because `load` expressions are not yet supported.

<pre>
legend <i>Expression</i>
</pre>


## 'Let' declarations

A `let` declaration declares a variable with a given name and value. Variables are immutable and cannot be re-assigned elsewhere in the program, but a variable may take different values during different lifetimes the statement or rule containing the declaration is executed or applied multiple times.

A compilation error occurs if a variable of the same name is already declared in the current lexical context.

<pre>
let <i>Name</i> = <i>Expression</i>
</pre>

A `let param` declaration is as above, except that the variable's value will instead be a runtime parameter of that name, if one is provided, otherwise the variable's initialiser is used.

A compilation error occurs if a runtime parameter of the same name is declared anywhere else in the same program.

<pre>
let param <i>Name</i> = <i>Expression</i>
</pre>


## 'Symmetry' declarations

A `symmetry` declaration sets the symmetry group which is used for rules in the declaration's lexical context. The symmetry group name must be a compile-time constant of type `str` with one of the following values:

- `"all"`: all square symmetries (default)
- `"none"`: no symmetries
- `"rot90"`: 90-degree rotational symmetry
- `"rot180"`: 180-degree rotational symmetry
- `"x"`: horizontal mirror symmetry
- `"y"`: vertical mirror symmetry
- `"xy"`: horizontal and vertical mirror symmetries

<pre>
symmetry <i>Expression</i>
</pre>


## 'Union' declarations

A `union` declaration declares a *union label* to have the same meaning as a given character set. The union label can then be used as a pattern character in [pattern literals](expressions.md#pattern-literal-expressions) within the declaration's lexical scope.

The union label must be a pattern literal containing a single pattern character which is not already defined as a symbol in the current alphabet or a union label, and the character set must be a compile-time constant expression with a [1x1 pattern type](types.md#pattern-types) for the current alphabet.

<pre>
union [<i>U</i>] = [[<i>CharSet</i>]]
</pre>
