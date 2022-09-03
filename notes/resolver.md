# Resolver

The resolver converts an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST) into an [abstract semantic graph](https://en.wikipedia.org/wiki/Abstract_semantic_graph) (ASG). Type definitions for the AST, ASG and expression types are found in [ast.ts](../src/compiler/ast.ts), [asg.ts](../src/compiler/asg.ts) and [type.ts](../src/compiler/type.ts) respectively. The resolver also performs:

- [Type checking](https://en.wikipedia.org/wiki/Type_system#Static_type_checking) of expressions.
- Other static checking, e.g. ensuring that statements have valid combinations of arguments and children.
- [Constant folding](https://en.wikipedia.org/wiki/Constant_folding) and constant propagation, and elision of some assignment statements.
- Inserting explicit ASG nodes for [type coercions](../doc/types.md#type-coercion).

Much of the work is done by the functions in `DECL_RESOLVE_FUNCS`, `EXPR_RESOLVE_FUNCS`, `RULE_RESOLVE_FUNCS` and `STMT_RESOLVE_FUNCS`. These constant object map each AST node kind to a function which checks and resolves AST nodes of that kind.


## Context

The `Context` class represents the current lexical context, including the declared grids, current grid, symmetry group for patterns, and variables and their types. The class is mutable, and only one instance will be used to resolve an entire AST.

Most changes to the context are temporary; for example, when resolving a variable declaration, the variable will be added to the context for the duration of resolving the part of the AST where that variable is in scope, and the variable is then removed from the context. The methods `withOutGrid`, `withSymmetry`, `withVariable` etc. are used for some temporary context changes.

The main exception to the above is that when a `stmt.map` or `stmt.use` statement changes the context's current grid, the change persists beyond the statement's own lexical scope. Additionally, when a runtime parameter is declared with a `let param` declaration, another runtime parameter of the same name cannot later be declared even if the first parameter is no longer in scope.


## Props

To avoid excessive code repetition, most expressions contained within declarations, rules or statements are resolved through the `_resolveProp` and `_resolveProps` functions. These functions resolve and check an expression according to a `PropSpec`, which is a string consisting of:

- An optional `const` modifier, indicating the expression must be a compile-time constant. In this case the expression will be resolved to a `Type.Value` rather than an `ASG.Expression`.
- A `PropTypeSpec` which is either a primitive type name, or one of `charset.in`, `charset.out`, `dict`, `position`, `object`, `pattern.in`, `pattern.out`, `position` or `str~`.
  - If the expected type is `float` or `fraction` and the expression's actual type is `int`, then the expression will be coerced to the expected type.
  - `charset.in` and `charset.out` mean 1x1 pattern types for the current alphabet.
  - `dict` means any dictionary type.
  - `position` means a position type for the current grid.
  - `object` means either any dictionary type, or any `position` type, or `grid`.
  - In a [rule context](../doc/rules.md#rule-contexts) for which an the input pattern is known, `pattern.in` and `pattern.out` mean a pattern type for the current alphabet with the appropriate dimensions; otherwise they mean any pattern type for the current alphabet.
  - `str~` means the type `str` is expected but an expression of type `bool`, `float`, `fraction`, `grid` or `int` may be coerced to `str`.
- An optional `?` modifier, indicating that the expression may be absent from the corresponding AST node.

The `PROP_SPECS` object defines `PropSpec` strings for resolving some properties of some kinds of AST nodes. These are used by the `_resolveProps` function.


## ExprFlags

ASG expressions are additionally labelled with flags indicating some static properties. These are defined in the `ExprFlags` enum, and have the following meanings:

- `RUNTIME_CONSTANT`: the expression's value is fixed at runtime. It may be a compile-time constant, or it may depend on the grid dimensions or other runtime parameters (excluding the PRNG seed).
- `DETERMINISTIC`: the expression does not invoke the PRNG when it is evaluated, and it does not depend on any variable whose initialiser is not deterministic.
- `LOCALLY_DETERMINISTIC`: the expression does not invoke the PRNG when it is evaluated, but its value may depend on a variable whose initialiser is not deterministic or locally-deterministic.
- `POSITION_INDEPENDENT`: the expression's value does not depend on the "current position" in a [rule context](../doc/rules.md#rule-contexts), but it may depend on a variable whose initialiser is not position-independent.
- `GRID_INDEPENDENT`: the expression's value does not depend on the current state of the grid, but it may depend on a variable whose initialiser is not grid-independent.

The purpose of these flags is to allow certain optimisations to be made by the compiler; particularly, to avoid repeatedly evaluating expressions when it is statically-known that the result will not have changed.
