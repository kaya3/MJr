interface SourcePosition extends Readonly<{line: number, col: number}> {}

namespace Tokenizer {
    export type Kind =
        | 'WHITESPACE'
        | 'COMMENT'
        | 'NAME'
        | 'KEYWORD'
        | 'FLOAT'
        | 'INT'
        | 'OP'
        | 'QUOTE'
        | 'PUNCTUATION'
        | 'PATTERN_CHAR'
        | 'STRING_CHAR'
        | 'ESCAPED_CHAR'
        | 'NEWLINE'
        | 'INDENT'
        | 'DEDENT'
        | 'EOF'
        | 'ERROR'
    
    export const KEYWORDS = [
        'all',
        'and',
        'at',
        'convchain',
        'convolution',
        'count',
        'else',
        'false',
        'field',
        'grid',
        'if',
        'in',
        'legend',
        'let',
        'limit',
        'load',
        'log',
        'map',
        'markov',
        'not',
        'observe',
        'once',
        'one',
        'or',
        'origin',
        'param',
        'pass',
        'path',
        'prl',
        'put',
        'randint',
        'random',
        'sequence',
        'sum',
        'symmetry',
        'true',
        'union',
        'use',
    ] as const;
    
    export type Keyword = typeof KEYWORDS[number]
    export interface Token<K extends Kind = Kind> extends Readonly<{kind: K, s: string, pos: SourcePosition}> {}
    
    const enum C {
        WHITESPACE = 0,
        LETTER_OR_UNDERSCORE,
        DIGIT,
        LPAREN,
        RPAREN,
        LSQB,
        RSQB,
        LANGLE,
        RANGLE,
        QUOTE,
        DBLQUOTE,
        EQUALS,
        EXCLAMATION_MARK,
        MINUS,
        SLASH,
        HASH,
        BACKSLASH,
        CARET,
        DOT,
        OTHER_OP,
        OTHER_PUNCTUATION,
        OTHER,
    }
    const C_MAP: readonly C[] = (function(...pairs: [string, C][]) {
        const arr = emptyArray(128, C.OTHER);
        for(const [chars, c] of pairs) {
            for(let i = 0; i < chars.length; ++i) {
                arr[chars.charCodeAt(i)] = c;
            }
        }
        return arr;
    })(
        [' \t', C.WHITESPACE],
        ['abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_', C.LETTER_OR_UNDERSCORE],
        ['0123456789', C.DIGIT],
        ['({', C.LPAREN],
        [')}', C.RPAREN],
        ['[', C.LSQB],
        [']', C.RSQB],
        ['<', C.LANGLE],
        ['>', C.RANGLE],
        ["'", C.QUOTE],
        ['"', C.DBLQUOTE],
        ['=', C.EQUALS],
        ['!', C.EXCLAMATION_MARK],
        ['-', C.MINUS],
        ['/', C.SLASH],
        ['#', C.HASH],
        ['\\', C.BACKSLASH],
        ['^', C.CARET],
        ['.', C.DOT],
        ['+*%', C.OTHER_OP],
        ['@|,:', C.OTHER_PUNCTUATION],
    );
    
    const enum Mode {
        NORMAL,
        PATTERN,
        CHARSET,
        QUOTE_STRING,
        DBLQUOTE_STRING,
    }
    class LineTokenizer {
        private readonly lineCodes: readonly C[];
        constructor(private readonly lineString: string) {
            this.lineCodes = makeArray(lineString.length, i => {
                const charCode = lineString.charCodeAt(i);
                return charCode >= 0 && charCode < C_MAP.length ? C_MAP[charCode] : C.OTHER;
            });
        }
        
        private scan(i: number, cs: readonly C[]): number {
            const {lineCodes} = this;
            while(i < lineCodes.length && cs.includes(lineCodes[i])) { ++i; }
            return i;
        }
        
        private has(i: number, c: C): boolean {
            const {lineCodes} = this;
            return i < lineCodes.length && lineCodes[i] === c;
        }
        
        getNextToken(mode: Mode, col: number, depth: number): [kind: Kind, endCol: number, newDepth: number, newMode: Mode] {
            const {lineCodes} = this;
            
            const c = lineCodes[col];
            switch(mode) {
                case Mode.NORMAL:
                    switch(c) {
                        case C.WHITESPACE:
                            return ['WHITESPACE', this.scan(col, [C.WHITESPACE]), depth, mode];
                        case C.HASH:
                            return ['COMMENT', lineCodes.length, depth, mode];
                        
                        case C.LPAREN:
                            return ['PUNCTUATION', col + 1, depth + 1, mode];
                        case C.RPAREN:
                            return ['PUNCTUATION', col + 1, depth - 1, mode];
                        
                        case C.LSQB:
                            return ['PUNCTUATION', col + 1, depth + 1, Mode.PATTERN];
                        case C.QUOTE:
                            return ['QUOTE', col + 1, depth, Mode.QUOTE_STRING];
                        case C.DBLQUOTE:
                            return ['QUOTE', col + 1, depth, Mode.DBLQUOTE_STRING];
                        
                        case C.LETTER_OR_UNDERSCORE: {
                            const end = this.scan(col, [C.LETTER_OR_UNDERSCORE, C.DIGIT]);
                            const s = this.lineString.substring(col, end);
                            return [KEYWORDS_SET.has(s) ? 'KEYWORD' : 'NAME', end, depth, mode];
                        }
                        
                        case C.EQUALS:
                        case C.EXCLAMATION_MARK:
                        case C.LANGLE:
                        case C.RANGLE:
                            // '=', '!', '<', '>', '==', '!=', '<=' or '>='
                            // note that '!' is not a valid operator, but this will be caught by the parser
                            return ['OP', this.has(col + 1, C.EQUALS) ? col + 2 : col + 1, depth, mode];
                        
                        case C.SLASH:
                            // '/' or '//'
                            return ['OP', this.has(col + 1, C.SLASH) ? col + 2 : col + 1, depth, mode];
                        
                        case C.MINUS:
                            // INT, FLOAT, '-' or '->'
                            if(!this.has(col + 1, C.DIGIT)) {
                                return ['OP', this.has(col + 1, C.RANGLE) ? col + 2 : col + 1, depth, mode];
                            }
                            ++col;
                            // intentional fall-through
                        case C.DIGIT: {
                            const end = this.scan(col, [C.DIGIT]);
                            if(!this.has(end, C.DOT)) {
                                return ['INT', end, depth, mode];
                            } else if(this.has(end + 1, C.DIGIT)) {
                                return ['FLOAT', this.scan(end + 1, [C.DIGIT]), depth, mode];
                            } else {
                                return ['ERROR', end + 1, depth, mode];
                            }
                        }
                        
                        case C.DOT:
                        case C.OTHER_OP:
                            // '.', '+', '*' or '%'
                            return ['OP', col + 1, depth, mode];
                        
                        case C.OTHER_PUNCTUATION:
                            return ['PUNCTUATION', col + 1, depth, mode];
                        
                        default:
                            return ['ERROR', col + 1, depth, mode];
                    }
                
                case Mode.PATTERN:
                case Mode.CHARSET:
                    switch(c) {
                        case C.WHITESPACE:
                            return ['WHITESPACE', this.scan(col, [C.WHITESPACE]), depth, mode];
                        case C.HASH:
                            return ['COMMENT', lineCodes.length, depth, mode];
                        
                        case C.LSQB:
                            return mode === Mode.PATTERN
                                ? ['PUNCTUATION', col + 1, depth + 1, Mode.CHARSET]
                                : ['ERROR', col + 1, depth, mode];
                        case C.RSQB:
                            return ['PUNCTUATION', col + 1, depth - 1, mode === Mode.PATTERN ? Mode.NORMAL : Mode.PATTERN];
                        
                        case C.CARET:
                            return [mode === Mode.CHARSET ? 'PUNCTUATION' : 'ERROR', col + 1, depth, mode];
                        case C.DOT:
                            return [mode === Mode.PATTERN ? 'PATTERN_CHAR' : 'ERROR', col + 1, depth, mode];
                        
                        case C.LETTER_OR_UNDERSCORE:
                        case C.DIGIT:
                        case C.EQUALS:
                        case C.EXCLAMATION_MARK:
                        case C.MINUS:
                        case C.OTHER_OP:
                        case C.OTHER_PUNCTUATION:
                        case C.OTHER:
                            return ['PATTERN_CHAR', col + 1, depth, mode];
                        
                        default:
                            return ['ERROR', col + 1, depth, mode];
                    }
                
                case Mode.QUOTE_STRING:
                case Mode.DBLQUOTE_STRING:
                    switch(c) {
                        case C.QUOTE:
                        case C.DBLQUOTE:
                            return (mode === Mode.QUOTE_STRING && c === C.QUOTE) || (mode === Mode.DBLQUOTE_STRING && c === C.DBLQUOTE)
                                ? ['QUOTE', col + 1, depth, Mode.NORMAL]
                                : ['STRING_CHAR', col + 1, depth, mode];
                        case C.BACKSLASH:
                            return ['ESCAPED_CHAR', Math.min(col + 2, lineCodes.length), depth, mode];
                        default:
                            return ['STRING_CHAR', col + 1, depth, mode];
                    }
            }
        }
    }
    
    const KEYWORDS_SET: ReadonlySet<string> = new Set(KEYWORDS);
    
    export function tokenize(src: string, skipWhitespace: boolean = false): Token[] {
        const lines = src.split('\n');
        const tokens: Token[] = [];
        const indentation: string[] = [''];
        const diagnostics = new Diagnostics();
        
        function _makeToken(kind: Kind, s: string, pos: SourcePosition): void {
            if((kind === 'WHITESPACE' || kind === 'COMMENT') && skipWhitespace) { return; }
            
            if(kind === 'NAME' && KEYWORDS_SET.has(s)) { kind = 'KEYWORD'; }
            tokens.push({kind, s, pos});
        }
        
        let depth = 0;
        let mode: Mode = Mode.NORMAL;
        for(let line = 1; line <= lines.length; ++line) {
            const lineString = lines[line - 1];
            
            // ignore lines with only whitespace and comments
            if(/^\s*(?:#.*)?$/.test(lineString)) {
                const i = lineString.indexOf('#');
                if(i >= 0) {
                    if(i > 0) { _makeToken('WHITESPACE', lineString.substring(0, i), {line, col: 0}); }
                    _makeToken('COMMENT', lineString.substring(i), {line, col: i});
                } else if(lineString.length > 0) {
                    _makeToken('WHITESPACE', lineString, {line, col: 0});
                }
                if(!skipWhitespace) { _makeToken('NEWLINE', '\n', {line, col: lineString.length}); }
                continue;
            }
            
            let col = 0;
            // check for indent or dedents
            if(mode === Mode.NORMAL && depth === 0) {
                const initialWhitespace = /^\s*/.exec(lineString)![0];
                const currentIndentation = indentation[indentation.length - 1];
                if(initialWhitespace.startsWith(currentIndentation)) {
                    if(initialWhitespace.length > currentIndentation.length) {
                        indentation.push(initialWhitespace);
                        _makeToken('INDENT', '', {line, col});
                    }
                } else {
                    while(true) {
                        const c = indentation[indentation.length - 1];
                        if(c === initialWhitespace) { break; }
                        
                        indentation.pop();
                        _makeToken('DEDENT', '', {line, col});
                        if(!c.startsWith(initialWhitespace)) {
                            indentation.push(initialWhitespace);
                            diagnostics.syntaxError('inconsistent indentation', {line, col});
                            break;
                        }
                    }
                }
                col = initialWhitespace.length;
            }
            
            const lineTokenizer = new LineTokenizer(lineString);
            while(col < lineString.length) {
                const [kind, nextCol, nextDepth, nextMode] = lineTokenizer.getNextToken(mode, col, depth);
                
                _makeToken(kind, lineString.substring(col, nextCol), {line, col});
                col = nextCol;
                depth = nextDepth;
                mode = nextMode;
            }
            
            if(mode === Mode.QUOTE_STRING || mode === Mode.DBLQUOTE_STRING) {
                diagnostics.syntaxError('unexpected end of line in string literal', {line, col});
                mode = Mode.NORMAL;
            }
            if(depth === 0 || !skipWhitespace) {
                _makeToken('NEWLINE', '\n', {line, col});
            }
        }
        
        const pos: SourcePosition = {line: lines.length, col: lines[lines.length - 1].length};
        for(let i = 1; i < indentation.length; ++i) {
            _makeToken('DEDENT', '', pos);
        }
        _makeToken('EOF', '', pos);
        
        if(mode !== Mode.NORMAL) { diagnostics.syntaxError('unexpected end of source', pos); }
        diagnostics.throwIfAnyErrors();
        return tokens;
    }
    
    export class TokenQueue {
        private i: number = 0;
        
        public constructor(private readonly tokens: readonly Token[]) {}
        
        public peek(): Token {
            return this.tokens[this.i];
        }
        
        public poll(): Token {
            return this.tokens[this.i++];
        }
        
        public hasNext(...kinds: Kind[]): boolean {
            return kinds.includes(this.peek().kind);
        }
        
        public hasNextS(...strings: string[]): boolean {
            return strings.includes(this.peek().s);
        }
        
        public pollIf(kind: Kind): boolean {
            return this.hasNext(kind) && (++this.i, true);
        }
        
        public pollIfS(s: string): boolean {
            return this.hasNextS(s) && (++this.i, true);
        }
        
        public skipLine(): void {
            while(!this.hasNext('EOF') && this.poll().kind !== 'NEWLINE') {}
        }
    }
}
