# Control-Flow Graph

After the resolver builds an [abstract semantic graph](https://en.wikipedia.org/wiki/Abstract_semantic_graph) (ASG), a [control-flow graph](https://en.wikipedia.org/wiki/Control-flow_graph) (CFG) is built. While it would be possible to compile directly from the ASG to the high-level intermediate representation (IR), the CFG can be used to:

- Prevent infinite loops in non-yielding programs, by terminating if a step counter exceeds some allowed maximum.
- Elide "transparent" limit variables, i.e. those which occur in `stmt.sequence` statements and have a fixed limit of 1.
- Simulate `yield` statements in target languages which do not support coroutines.


## Nodes

There are three categories of CFG node:

- Those which cannot branch:
  - Executing a non-branching ASG leaf statement (i.e. one which always "returns false"),
  - Resetting an ASG block statement (`stmt.markov` or `stmt.sequence`),
  - Setting a block's flag,
  - Decrementing a limit variable,
  - A 'pass' node which does nothing.
- Those which branch on some condition:
  - Executing any other ASG leaf statement, branching on whether the statement "returns true" or "returns false",
  - Branching on a block's flag,
  - Branching on whether a limit variable is above zero.
- The 'stop' node, which terminates the program.

Although `stmt.pass` AST nodes have already been eliminated by the resolver, it is convenient to include some new 'pass' nodes in the CFG as jump targets (and when eliding unnecessary ASG statements, such as assignments to variables which are never read). These will be skipped by the compiler, when the CFG is traversed by the `goto` method.
