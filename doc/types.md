# Types

MJr is a statically-typed language; each expression and variable has a type, and expressions are type-checked at compile-time. This allows compilation to statically-typed target languages, including TypeScript. However, all types in MJr are inferred, so type annotations are never needed (and the language has no syntax for them anyway).


## Primitive types

The following types are primitive:

- `bool`: Boolean
- `float`: double-precision floating-point number
- `fraction`: exact rational number
- `grid`
- `int`: signed 32-bit integer
- `str`: string

There are no subtype relations between any primitive types.


## Dict types

Dicts are immutable unordered collections of key/value pairs; a `dict` type is the type of such a collection, i.e. an unordered set of key names, each associated with a value type. There are no "dynamic" dictionary types; every `dict` type has a fixed set of keys.

A `dict` type D1 is a subtype of a `dict` type D2 if D1 and D2 have the same keys, and each key's value type in D1 is a subtype of its value type in D2.

Note that a `dict` type with *more* keys than another `dict` type is *not* a subtype. This allows easier compilation to a wider range of target languages.


## Pattern types

A `pattern.in` or `pattern.out` type represents an immutable rectangular pattern which may occur in a grid, or which may be written to a grid, respectively. A pattern type consists of an alphabet, a pattern width, a pattern height.

A pattern type P1 is a subtype of a pattern type P2 if P1 and P2 are equal, or if P1 is a `pattern.out` type with the same alphabet, width and height as P2.


## Position types

A `position` type represents an immutable pair of `x` and `y` coordinates for a cell in a grid. The coordinates (0, 0) refer to the top-left corner. A `position` type is particular to some grid; position types are distinct (and not related by subtypes) if they are for different grids, even if those grids have the same alphabet and scale.


## Type coercion

MJr allows two kinds of type coercions.

### Coercion from 'int'

Expressions of type `int` can be coerced to `float` or `fraction`. This coercion occurs:

- Anywhere that a `float` or `fraction` is required;
- In any binary operator expression where the other operand has type `float` or `fraction`; or
- In a branch of a conditional expression, where the other branch has type `float` or `fraction`.

### Coercion to `str`

Expressions of type `bool`, `float`, `fraction`, `grid` or `int` can be coerced to `str`. This coercion occurs:

- In a `log` statement;
- In a binary `+` expression where the other operand has type `str`; or
- In a branch of a conditional expression, where the other branch has type `str`.
