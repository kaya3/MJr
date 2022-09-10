# Intermediate representation

The compiler does not write code in the target language directly; instead, it outputs an [intermediate representation](https://en.wikipedia.org/wiki/Intermediate_representation) (IR) which can be translated into a variety of target languages. The IR is intended to be an abstract syntax tree for a "lowest common denominator" imperative language with the following features:

- Basic integer, floating-point, boolean and string types and operations, including bitwise operations;
- Mutable variables and at least fixed-size arrays;
- Structs, simple classes or some other means to represent [dict types](../doc/types.md#dict-types) with fixed keys;
- Function declarations which can [close](https://en.wikipedia.org/wiki/Closure_(computer_programming)) over constants from the outer scope;
- `if`/`else` statements and conditional expressions;
- Loops, and `break` statements which escape from the inner-most loop;
- `switch` or `match` statements, or a similar branching feature.
- `yield` statements (see [below](#yield-statements));
- Some way of importing a library.

Translating via this IR also creates some opportunities for [additional optimisations](#simplifications-and-optimisations) which may improve the performance of the program and/or reduce the size of the generated code.


## Types

The IR has its own types, not all of which are shared by the MJr language's [type system](../doc/types.md):

- `byte`: an integer between 0 (inclusive) and 128 (exclusive); used for alphabet symbols in grids or patterns.
- `int`: a signed 32-bit integer.
- `bool`: a Boolean.
- `float`: a double-precision floating-point number.
- `str`: a string.
- `array.const` and `array.mutable`: an array of integers, immutable or mutable respectively, which are all between zero (inclusive) and `domainSize` (exclusive).
- Dict types, which are immutable structs, objects or some other kind of mapping.
- `grid`, `pattern`, `prng`, `rewriteinfo` and `sampler`: pre-defined struct types which can be declared in the MJr runtime library.
- `void`: return type for functions with no return value.
- Nullable types, for optional parameters.

Note that the target language only needs to support integers of *at least* the required sizes; for example, nothing will go wrong if `byte` and `int` are mapped to the same type in the target language, or if an array is capable of holding integers much larger than its domain size, other than more memory being used. Likewise, the target language does not need to enforce immutability of arrays or dicts, since the generated code won't attempt to mutate these anyway. These distinctions are only made to potentially allow more optimised output code.

Additionally, it's OK for the mutable `IR.INT32_ARRAY_TYPE` (which has a domain size of `2 ** 32`) to use signed 32-bit integers, since in practice these arrays are either used as bitmasks (so the signs don't matter) or otherwise the elements will not exceed `2 ** 31 - 1`.


## Simplifications and optimisations

The `IR` namespace has a number of factory functions for IR nodes; while most of these simply create a new node object with the given parameters and return it, there are some opportunities for simplifying or optimising the IR tree, in order to produce smaller and/or more efficient code output. For example, `stmt.switch` statements will group multiple cases together when their blocks are equal.

This requires being able to tell when two IR nodes are equal. Currently this is implemented using the `IR.key` function, which simply calls `JSON.stringify` to transform an IR node into a primitive value, in a way which preserves "equality". For this to work consistently,

- IR nodes must only use JSON-serialisable data structures, i.e. native arrays and plain objects; and
- They must be in a canonical form, in particular the properties of each kind of object must be in a consistent order (this is guaranteed by using factory functions rather than many object literals).


## Other notes

### 'Break' statements

The IR's `stmt.break` statement is only used for escaping from `stmt.for.range` and `stmt.while` loops, not from `stmt.switch` statements. Additionally, break statements are not used inside switch statements for escaping from a loop outside of the switch statement. This is to avoid the need for labelled breaks (which some languages don't have) whether or not the language's `switch` construct interacts with `break` statements.

### 'For range' statements

The IR's `stmt.for.range` loop statements iterate over a range of consecutive integer values, either forwards or in reverse. The range is determined by two expressions, `low` (inclusive) and `high` (exclusive), both of which must remain constant for the duration of the loop. A forwards range is equivalent to the following C-style `for` loop:

```js
for(let i = low; i < high; ++i) { ... }
```

A reversed range is equivalent to the following C-style `for` loop:

```js
for(let i = high - 1; i >= low; --i) { ... }
```

### 'Switch' statements

The IR's `stmt.switch` statements are always used with an integer-valued variable in some range starting at zero, and never use fall-throughs between different cases. This is so that they can be compiled in different ways depending on what the target language supports:

- When targeting JavaScript, they are compiled to `switch` statements, in which case each 'case' block needs to have a `break` statement appended to it.
- When targeting Python, they are compiled to `match` statements.
- If some target language does not have a similar control-flow construct as `switch` or `match`, then they may be compiled to a chain of `if`/`else` statements, or something else.

### Variable declaration statements

Some variable declarations in the IR do not have initialisers; this allows slightly more efficient code to be generated in some cases. However, a target language might not allow an uninitialised variable to be declared, particularly when the target language's static checker cannot determine that the variable will not be used before it is assigned.

In this case, the code generator class may need to emit a default initialiser of the appropriate type. (For TypeScript output, the code generator instead emits a "definitely assigned" assertion.)

### 'yield' statements

The `animate` flag is currently implemented by emitting `stmt.yield` statements; this means the target language must support [generator function](https://en.wikipedia.org/wiki/Generator_(computer_programming)). In principle, it would be possible later to support target languages without generator functions, by outputting a class which encapsulates the program's state and has a method to advance the program by one step.

### Bitwise operators

The IR supports the bitwise operators `int_and`, `int_or`, `int_xor`, `int_not`, `int_lshift` and `int_rshift`. These are mostly self-explanatory, but have a few subtleties:

- `int_lshift` and `int_rshift` are only used with a second operand in the range 0 (inclusive) to 32 (exclusive). This is because the shift amount is taken modulo 32 in some languages (e.g. JavaScript) but not others (e.g. Python).
- `int_lshift` is "[loose](#loose-integer-operators)" in the sense that it does not need to ensure that the result fits in the signed 32-bit integer range; this will be ensured by the compiler when it matters.
- `int_rshift` means a right arithmetic shift (i.e. sign-preserving), not a right logical shift.

### 'Loose' integer operators

The integer operators defined by the ASG are required to have the correct behaviour for signed 32-bit integers, even when the target language does not natively have a signed 32-bit integer type. This means that, for example:

- When compiling to JavaScript, `int_plus` is compiled to `(a + b) | 0`, `int_mult` is compiled to `Math.imul(a, b)`, and so on.
- When compiling to Python, `int_plus` is compiled to `int32(a + b)`, where `int32` is a function which performs a 32-bit signed integer overflow.

Additionally, the `int_floordiv` and `int_mod` operators have different behaviour for negative numbers compared to the similar native operators in some target languages.

Therefore, the IR supports the additional "loose" integer operators `loose_int_plus`, `loose_int_minus`, `loose_int_mult`, `loose_int_floordiv` and `loose_int_mod`. These operators are not required to have correct behaviour for overflows or negative operands, since the compiler must only emit them with operands that are non-negative and sufficiently small that the result cannot overflow. This allows for simpler code output in some cases.
