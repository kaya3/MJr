# Tokenizer

The tokenizer converts raw MJr source code into a sequence of tokens. Additionally, there are "empty" tokens representing increases or decreases in the indentation level, which are used by the parser to determine the start and end of blocks.

The tokenizer is implemented as a state machine, where the current state is represented by:

- The tokenizer mode: `NORMAL`, `PATTERN`, `CHARSET`, `QUOTE_STRING` or `DBLQUOTE_STRING`.
- The depth of nested parentheses.
- The line and column position.

Most of the work is done by the `LineTokenizer` class. Given a current state, the `getNextToken` method returns a token kind and the next state, and then a token of that kind is emitted, bounded between the old and new column positions of the current line. Newlines and indentation are handled by a separate loop in the `tokenize` function, which iterates over the input one line at a time.

Some further notes:

- The characters from the input are mapped to character classes, represented by the enum `C`, to simplify the descriptions of which sequences of characters form which kinds of token.
- The nesting depth is incremented on `(`, `[` or `{`, and decremented on `)`, `]` or `}`. The depth is used to ignore indentation and newlines within bracketed expressions and patterns.
- Individual pattern and string literal characters are separate tokens, so that error messages can refer to their line and column positions, and potentially so that a syntax highlighter can use different styles for different pattern characters.
