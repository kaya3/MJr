namespace Parser {
    /**
     * Maximum size for a grid alphabet; this is set at 128 so that alphabet
     * symbols can be represented by signed byte values.
     */
    const MAX_ALPHABET_SIZE = 128;
    
    export const enum Precedence {
        DECLARATION = 0,
        IF_ELSE = 1,
        OR = 2,
        AND = 3,
        NOT = 4,
        CMP = 5,
        PLUS_MINUS = 6,
        MULT_DIV_MOD = 7,
        UPLUS_UMINUS = 8,
        FUNC = 9,
    }
    
    /**
     * Precedence table for binary operators.
     */
    export const BINARY_OPS = {
        'or': Precedence.OR,
        'and': Precedence.AND,
        '<': Precedence.CMP,
        '<=': Precedence.CMP,
        '>': Precedence.CMP,
        '>=': Precedence.CMP,
        '==': Precedence.CMP,
        '!=': Precedence.CMP,
        '+': Precedence.PLUS_MINUS,
        '-': Precedence.PLUS_MINUS,
        '*': Precedence.MULT_DIV_MOD,
        '/': Precedence.MULT_DIV_MOD,
        '//': Precedence.MULT_DIV_MOD,
        '%': Precedence.MULT_DIV_MOD,
    };
    
    /**
     * Precedence table for unary operators.
     */
    export const UNARY_OPS = {
        'not': Precedence.NOT,
        '+': Precedence.UPLUS_UMINUS,
        '-': Precedence.UPLUS_UMINUS,
        'count': Precedence.FUNC,
        'load': Precedence.FUNC,
        'randint': Precedence.FUNC,
        'sum': Precedence.FUNC,
    };
    
    const UNESCAPE_STRING_CHAR: IRecord<string, string> = {
        '\\n': '\n',
        '\\t': '\t',
    };
    
    type Part = keyof PartMap
    type PartMap = {root: AST.CompilationUnit, decl: AST.Declaration, expr: AST.Expression, rule: AST.Rule, stmt: AST.Statement}
    
    /**
     * Arguments for each node type; `true` means a required argument.
     */
    const ARGS = (<T extends {[K in Tokenizer.Keyword]?: IRecord<string, boolean>}>(specs: T): T => specs)({
        all: {temperature: false, search: false, maxStates: false, depthCoefficient: false},
        convchain: {sample: true, n: true, on: true, temperature: false, periodic: false},
        convolution: {kernel: true},
        field: {for_: true, on: true, from: false, to: false, essential: false, recompute: false},
        grid: {scaleX: false, scaleY: false, periodic: false},
        map: {outGrid: true},
        one: {temperature: false, search: false, maxStates: false, depthCoefficient: false},
        path: {from: true, to: true, input: true, output: true, longest: false, inertia: false},
    } as const);
    
    /**
     * Lookup table for which nodes have each argument; used to generate hints
     * in error messages.
     */
    const ARGS_TO_NODES: {[k: string]: (keyof ParsedArgs)[]} = {};
    for(const [k, args] of Object.entries(ARGS) as [keyof typeof ARGS, object][]) {
        for(const j of Object.keys(args)) {
            (ARGS_TO_NODES[j] ??= []).push(k);
        }
    }
    
    type ParsedArgs = Simplify<{[K in keyof typeof ARGS]:
        {[J in KeysMatching<typeof ARGS[K], true>]: AST.Expression}
        & {[J in KeysMatching<typeof ARGS[K], false>]?: AST.Expression}
    }>
    
    function quoteNode(tok: Tokenizer.Token | AST.Node): string {
        return tok.kind === 'KEYWORD' ? `keyword '${tok.s}'`
            : tok.kind === 'OP' || tok.kind === 'PUNCTUATION' ? `'${tok.s}'`
            : `'${tok.kind}'`;
    }
    
    class Parser {
        readonly diagnostics = new Diagnostics();
        private readonly q: Tokenizer.TokenQueue;
        
        constructor(src: string) {
            const tokens = Tokenizer.tokenize(src, true);
            this.q = new Tokenizer.TokenQueue(tokens);
        }
        
        // error reporting and checking
        
        private errorUnexpected(part: 'statement' | 'rule' | 'declaration' | 'expression'): void {
            const tok = this.q.poll();
            this.diagnostics.syntaxError(`unexpected ${quoteNode(tok)}, expected ${part}`, tok.pos);
        }
        private errorExpected(was: Tokenizer.Token | AST.Node, ...hints: string[]): void {
            this.diagnostics.syntaxError(`expected ${quoteJoin(hints, ' | ')}, was ${quoteNode(was)}`, was.pos);
        }
        private errorOperatorPrecedence(tok: Tokenizer.Token): void {
            this.diagnostics.syntaxError(`${quoteNode(tok)} not allowed here due to operator precedence`, tok.pos);
        }
        
        private expectPoll<K extends Tokenizer.Kind>(kind: K): Tokenizer.Token<K> | undefined {
            const tok = this.q.poll();
            if(tok.kind === kind) {
                return tok as Tokenizer.Token<K>;
            } else {
                this.errorExpected(tok, kind);
                return undefined;
            }
        }
        private expectPollS<S extends string>(...strings: S[]): Tokenizer.Token & {s: S} | undefined {
            const tok = this.q.poll();
            if((strings as string[]).includes(tok.s)) {
                return tok as Tokenizer.Token & {s: S};
            } else {
                this.errorExpected(tok, ...strings);
                return undefined;
            }
        }
        private expectPollIf(kind: Tokenizer.Kind): boolean {
            return this.q.pollIf(kind)
                || (this.errorExpected(this.q.peek(), kind), false);
        }
        private expectPollIfS(s: string): void {
            if(!this.q.pollIfS(s)) {
                this.errorExpected(this.q.peek(), s);
            }
        }
        private assertPoll<K extends Tokenizer.Kind>(kind: K): Tokenizer.Token<K> {
            if(!this.q.hasNext(kind)) { throw new Error(); }
            return this.q.poll() as Tokenizer.Token<K>;
        }
        private assertPollS<S extends string>(...strings: S[]): Tokenizer.Token & {s: S} {
            if(!this.q.hasNextS(...strings)) { throw new Error(); }
            return this.q.poll() as Tokenizer.Token & {s: S};
        }
        
        // entry points
        
        /**
         * ```none
         * CompilationUnit = Statement* EOF
         * ```
         */
        readonly root = (): AST.CompilationUnit => {
            const stmts = this.parseUntil('stmt');
            this.assertPoll('EOF');
            return {kind: 'root', stmts, pos: {line: 1, col: 0}};
        };
        
        /**
         * ```none
         * Declaration = LegendDecl | LetDecl | SymmetryDecl | UnionDecl
         * ```
         */
        readonly decl = (): AST.Declaration | undefined => {
            const tok = this.q.peek();
            if(tok.kind === 'KEYWORD') {
                switch(tok.s as Tokenizer.Keyword) {
                    case 'legend':
                        return this.parseLegendDecl();
                    case 'let':
                        return this.parseLetDecl();
                    case 'symmetry':
                        return this.parseSymmetryDecl();
                    case 'union':
                        return this.parseUnionDecl();
                }
            }
            
            this.errorUnexpected('declaration');
            this.q.skipLine();
            return undefined;
        };
        
        /**
         * ```none
         * Rule = FieldRule | ObserveRule | RewriteRule | WithDeclaration<Rule>
         * ```
         */
        readonly rule = (): AST.Rule | undefined => {
            const tok = this.q.peek();
            if(tok.kind === 'KEYWORD') {
                switch(tok.s as Tokenizer.Keyword) {
                    case 'legend':
                    case 'let':
                    case 'symmetry':
                    case 'union':
                        const declaration = this.decl();
                        return declaration && this.parseDeclChildren('rule', declaration);
                    
                    case 'field':
                        return this.parseFieldRule();
                    
                    case 'observe':
                        return this.parseObserveRule();
                }
            }
            
            const rule = this.parseRewriteRule();
            if(rule !== undefined) { return rule; }
            
            this.errorUnexpected('rule');
            this.q.skipLine();
            return undefined;
        };
        
        /**
         * ```none
         * Statement = BaseUseStmt | LogStmt | ModifiableStmt | PassStmt | UseStmt | WithDeclaration<Statement>
         * ```
         */
        readonly stmt = (): AST.Statement | undefined => {
            const tok = this.q.peek();
            
            if(tok.kind === 'KEYWORD') {
                switch(tok.s as Tokenizer.Keyword) {
                    case 'legend':
                    case 'let':
                    case 'symmetry':
                    case 'union':
                        const declaration = this.decl();
                        return declaration && this.parseDeclChildren('stmt', declaration);
                    
                    case 'grid':
                        return this.parseBareUseStmt();
                    case 'pass':
                        return this.parsePassStmt();
                    case 'log':
                        return this.parseLogStmt();
                    case 'use':
                        return this.parseUseStmt();
                }
            }
            
            return this.parseModifiableStmt();
        };
        
        /**
         * ```none
         * ModifiableStmt = BlockStmt | ConvChainStmt | ModifiedStmt | PathStmt | PutStmt | RuleBlockStmt
         * ```
         */
        private parseModifiableStmt(): Exclude<AST.Statement, AST.DeclarationStmt> | undefined {
            const tok = this.q.peek();
            
            if(tok.kind === 'KEYWORD') {
                switch(tok.s as Tokenizer.Keyword) {
                    case 'all':
                    case 'convolution':
                    case 'map':
                    case 'once':
                    case 'one':
                    case 'prl':
                        return this.parseRuleBlockStmt();
                    
                    case 'markov':
                    case 'sequence':
                        return this.parseBlockStmt();
                    
                    case 'convchain':
                    case 'path':
                        return this.parseLineArgsStmt();
                    
                    case 'put':
                        return this.parsePutStmt();
                }
            } else if(tok.s === '@') {
                return this.parseModifiedStmt();
            }
            
            this.errorUnexpected('statement');
            this.q.skipLine();
            return undefined;
        }
        
        /**
         * ```none
         * Expression = TernaryExpr
         * ```
         */
        readonly expr = (minPrecedence: Precedence = Precedence.IF_ELSE): AST.Expression | undefined => {
            const unaryExpr = this.parseUnaryOpExpr(minPrecedence);
            const binaryExpr = this.parseBinaryOpExpr(unaryExpr, minPrecedence);
            return this.parseTernaryExpr(binaryExpr, minPrecedence);
        }
        
        /**
         * ```none
         * TernaryExpr = BinaryOpExpr ('if' Expression 'else' Expression)?
         * ```
         */
        private parseTernaryExpr(expr: AST.Expression | undefined, minPrecedence: Precedence): AST.Expression | undefined {
            if(expr === undefined || minPrecedence > Precedence.IF_ELSE || !this.q.pollIfS('if')) { return expr; }
            
            let condition: AST.Expression | undefined, otherwise: AST.Expression | undefined;
            return (condition = this.expr(Precedence.OR))
                && this.expectPollS('else')
                && (otherwise = this.expr(Precedence.IF_ELSE))
                && {kind: 'expr.op.ternary', condition, then: expr, otherwise, pos: expr.pos};
        }
        
        private hasNextBinaryOp(minPrecedence: Precedence): boolean {
            const op = this.q.peek().s;
            return objHasKey(BINARY_OPS, op) && BINARY_OPS[op] >= minPrecedence;
        }
        
        /**
         * ```none
         * BinaryOpExpr = UnaryOpExpr (BinaryOp UnaryOpExpr)*
         * BinaryOp = OP | 'and' | 'or'
         * ```
         */
        private parseBinaryOpExpr(left: AST.Expression | undefined, minPrecedence: Precedence): AST.Expression | undefined {
            // https://en.wikipedia.org/wiki/Operator-precedence_parser#Pseudocode
            
            let prevWasComparison = false;
            while(left !== undefined && this.hasNextBinaryOp(minPrecedence)) {
                const opTok = this.q.poll();
                const op = opTok.s as AST.BinaryOp;
                const opPrecedence = BINARY_OPS[op];
                const isComparison = opPrecedence === Precedence.CMP;
                
                // MJr syntax is similar to Python's, but `x < y < z` chains in Python and not in
                // MJr; such expressions are mistakes, so report a useful error message now rather
                // than a type error later. If the programmer wrote `x < y == z` intending to
                // compare the result of `x < y` with a boolean `z`, they should use brackets.
                if(prevWasComparison && isComparison) {
                    this.diagnostics.syntaxError(`comparison operators do not chain; use 'and' to do multiple comparisons, or brackets to disambiguate`, opTok.pos);
                }
                
                let right = this.parseUnaryOpExpr(opPrecedence);
                
                // none of the binary ops are right-associative
                while(right !== undefined && this.hasNextBinaryOp(opPrecedence + 1)) {
                    right = this.parseBinaryOpExpr(right, opPrecedence + 1);
                }
                left = right && {kind: 'expr.op.binary', op, left, right, pos: left.pos};
                
                prevWasComparison = isComparison;
            }
            return left;
        }
        
        /**
         * ```none
         * UnaryOpExpr = DeclarationExpr | UnaryOp Expression | PrimaryExpr
         * UnaryOp = '+' | '-' | 'count' | 'load' | 'not' | 'randint' | 'sum'
         * ```
         */
        private parseUnaryOpExpr(minPrecedence: Precedence): AST.Expression | undefined {
            if(this.q.hasNextS('legend', 'let', 'symmetry', 'union')) {
                if(minPrecedence > Precedence.DECLARATION) { this.errorOperatorPrecedence(this.q.peek()); }
                return this.parseDeclarationExpr();
            }
            
            const {s: op, pos} = this.q.peek();
            if(!objHasKey(UNARY_OPS, op)) { return this.parsePrimaryExpression(); }
            
            const tok = this.q.poll();
            const opPrecedence = UNARY_OPS[op];
            if(opPrecedence < minPrecedence) { this.errorOperatorPrecedence(tok); }
            
            const child = this.expr(opPrecedence);
            return child && {kind: 'expr.op.unary', op, child, pos};
        }
        
        /**
         * ```none
         * PrimaryExpr = DictExpr | GridExpr | LiteralExpr | NameExpr | '(' Expression ')'
         * LiteralExpr = BoolLiteralExpr | FloatLiteralExpr | IntLiteralExpr | PatternLiteralExpr | StrLiteralExpr
         * ```
         */
        private parsePrimaryExpression(): AST.Expression | undefined {
            const tok = this.q.peek();
            switch(tok.kind) {
                case 'KEYWORD':
                    switch(tok.s as Tokenizer.Keyword) {
                        case 'at':
                        case 'origin':
                        case 'random':
                            return this.parseNameExpr();
                        
                        case 'false':
                        case 'true':
                            return this.parseBoolLiteralExpr();
                        
                        case 'grid':
                            return this.parseGridExpr();
                    }
                    break;
                
                case 'FLOAT':
                    return this.parseFloatLiteralExpr();
                case 'INT':
                    return this.parseIntLiteralExpr();
                case 'NAME':
                    return this.parseNameExpr();
                
                case 'QUOTE':
                    return this.parseStrLiteralExpr();
                case 'PUNCTUATION':
                    switch(tok.s) {
                        case '(':
                            return this.parseBracketedExpr();
                        case '[':
                            return this.parsePatternLiteralExpr();
                        case '{':
                            return this.parseDictExpr();
                    }
                    break;
            }
            
            this.errorUnexpected('expression');
            return undefined;
        }
        
        // helpers
        
        /**
         * Parses `T*`. `stopBefore` must only contain tokens which `T` cannot begin with.
         */
        private parseUntil<K extends 'rule' | 'stmt'>(part: K, ...stopBefore: Tokenizer.Kind[]): PartMap[K][] {
            const parseChild = this[part] as () => PartMap[K];
            const children: PartMap[K][] = [];
            while(!this.q.hasNext('EOF', ...stopBefore)) {
                const child = parseChild();
                if(child !== undefined) { children.push(child); }
            }
            return children;
        }
        
        /**
         * ```none
         * BlockChildren<T> = ':' (T | NEWLINE INDENT T+ DEDENT)
         * ```
         */
        private parseBlockChildren<K extends 'rule' | 'stmt'>(kind: K): PartMap[K][] {
            this.expectPollIfS(':');
            
            if(this.q.pollIf('NEWLINE')) {
                if(!this.expectPollIf('INDENT')) { return []; }
                const children = this.parseUntil(kind, 'DEDENT');
                this.assertPoll('DEDENT');
                return children;
            } else {
                const child = this[kind]() as PartMap[K] | undefined;
                return child !== undefined ? [child] : [];
            }
        }
        
        /**
         * ```none
         * WithDeclaration<T> = Declaration (NEWLINE T* | 'in' BlockChildren<T>)
         * ```
         */
        private parseDeclChildren<K extends 'rule' | 'stmt'>(kind: K, declaration: AST.Declaration): AST.DeclarationChildren<K, PartMap[K]> | undefined {
            const children: PartMap[K][] = this.q.pollIf('NEWLINE')
                ? this.parseUntil(kind, 'DEDENT')
                : (this.expectPollIfS('in'), this.parseBlockChildren(kind));
            return {kind: `${kind}.decl`, declaration, children, pos: declaration.pos};
        }
        
        /**
         * ```none
         * Args = NameValuePairs?
         * ```
         */
        private parseArgs<K extends keyof typeof ARGS>(kind: K): ParsedArgs[K] | undefined {
            const pairs = this.q.hasNextS('{') ? this.parseNameValuePairs() : [];
            if(pairs === undefined) { return undefined; }
            
            const spec: IRecord<string, boolean> = ARGS[kind];
            const args: Record<string, AST.Expression> = {};
            
            for(const [name, expr] of pairs) {
                // sanitise JS keyword
                const argName = name.name === 'for' ? 'for_' : name.name;
                args[argName] = expr;
                if(!objHasKey(spec, argName)) {
                    const hints = ARGS_TO_NODES[argName];
                    const msg = hints !== undefined ? `argument '${name.name}' only valid for ${quoteJoin(hints)}` : `invalid argument '${name.name}'`;
                    this.diagnostics.syntaxError(msg, name.pos);
                }
            }
            
            const missing = Object.keys(spec).filter(k => spec[k] && args[k] === undefined);
            if(missing.length > 0) {
                this.diagnostics.syntaxError(`missing required argument${missing.length > 1 ? 's' : ''} ${quoteJoin(missing)}`, this.q.peek().pos);
                return undefined;
            }
            
            return args as ParsedArgs[K];
        }
        
        /**
         * ```none
         * NameValuePairs = '{' (NameValuePair ',')* NameValuePair? '}'
         * NameValuePair = SimpleNameExpr '=' Expression
         * ```
         */
        private parseNameValuePairs(): [name: AST.SimpleNameExpr, expr: AST.Expression][] | undefined {
            const pairs: [name: AST.SimpleNameExpr, expr: AST.Expression][] = [];
            const names = new Set<string>();
            if(this.q.pollIfS('{')) {
                while(true) {
                    const name = this.parseSimpleNameExpr();
                    if(name === undefined) { return undefined; }
                    
                    if(names.has(name.name)) { this.diagnostics.syntaxError(`duplicate name '${name.name}'`, name.pos); }
                    
                    const arg = this.expectPollS('=') && this.expr();
                    if(arg === undefined) { return undefined; };
                    pairs.push([name, arg]);
                    names.add(name.name);
                    
                    if(this.q.pollIfS(',')) {
                        if(this.q.pollIfS('}')) { break; }
                    } else if(this.expectPollS('}')) {
                        break;
                    } else {
                        return undefined;
                    }
                }
            }
            return pairs;
        }
        
        // declarations
        
        /**
         * ```none
         * LegendDecl = 'legend' PatternLiteralExpr
         * SymmetryDecl = 'symmetry' StrLiteralExpr
         * ```
         */
        private parseLegendDecl(): AST.LegendDecl | undefined {
            const {pos} = this.assertPollS('legend', 'symmetry');
            const expr = this.parsePatternLiteralExpr();
            return expr && {kind: 'decl.legend', expr, pos};
        }
        
        /**
         * ```none
         * LetDecl = 'let' 'param'? SimpleNameExpr '=' Expression
         * ```
         */
        private parseLetDecl(): AST.LetDecl | undefined {
            const {pos} = this.assertPollS('let');
            const isParam = this.q.pollIfS('param');
            
            /*
            const names: AST.SimpleNameExpr[] = [];
            while(true) {
                const nameExpr = this.parseSimpleNameExpr();
                if(nameExpr === undefined) { return undefined; }
                names.push(nameExpr);
                
                if(this.q.pollIfS('=')) { break; }
                if(this.expectPollS('|', '=') === undefined) { return undefined; }
            }
           
            if(isParam && names.length !== 1) {
                this.diagnostics.syntaxError(`'let param' must only declare one name`, pos);
            }
            */
            
            let name: AST.SimpleNameExpr | undefined, rhs: AST.Expression | undefined;
            return (name = this.parseSimpleNameExpr())
                && this.expectPollS('=')
                && (rhs = this.expr())
                && {kind: 'decl.let', name, rhs, pos, isParam};
        }
        
        /**
         * ```none
         * LegendDecl = 'legend' PatternLiteralExpr
         * SymmetryDecl = 'symmetry' StrLiteralExpr
         * ```
         */
        private parseSymmetryDecl(): AST.SymmetryDecl | undefined {
            const {pos} = this.assertPollS('symmetry');
            const expr = this.parseStrLiteralExpr();
            return expr && {kind: 'decl.symmetry', expr, pos};
        }
        
        /**
         * ```none
         * UnionDecl = 'union' PatternLiteralExpr '=' Expression
         * ```
         */
        private parseUnionDecl(): AST.UnionDecl | undefined {
            const {pos} = this.assertPollS('union');
            
            let label: AST.PatternLiteralExpr | undefined, chars: AST.Expression | undefined;
            return (label = this.parsePatternLiteralExpr())
                && this.expectPollS('=')
                && (chars = this.expr())
                && {kind: 'decl.union', label, chars, pos};
        }
        
        // rules
        
        /**
         * ```none
         * FieldRule = 'field' Args NEWLINE
         * ```
         */
        private parseFieldRule(): AST.FieldRule | undefined {
            const {pos} = this.assertPollS('field');
            
            const args = this.parseArgs('field');
            return args
                && this.expectPoll('NEWLINE')
                && ({kind: 'rule.field', ...args, pos});
        }
        
        /**
         * ObserveRule = 'observe' Expression ('->' Expression)? '->' Expression ('if' Expression)? NEWLINE
         */
        private parseObserveRule(): AST.ObserveRule | undefined {
            const {pos} = this.assertPollS('observe');
            
            const from = this.expr(Precedence.OR);
            if(!from) { return undefined; }
            
            this.expectPollS('->');
            let via = this.expr(Precedence.OR);
            if(via === undefined) { return undefined; }
            
            let to: AST.Expression | undefined;
            if(this.q.pollIfS('->')) {
                to = this.expr(Precedence.OR);
                if(to === undefined) { return undefined; }
            } else {
                to = via;
                via = undefined;
            }
            
            let condition: AST.Expression | undefined;
            return (this.q.pollIfS('if') ? condition = this.expr(Precedence.OR) : true)
                && this.expectPoll('NEWLINE')
                && {kind: 'rule.observe', from, via, to, condition, pos};
        }
        
        /**
         * ```none
         * RewriteRule = Expression '->' Expression ('if' Expression)? NEWLINE
         * ```
         */
        private parseRewriteRule(): AST.RewriteRule | undefined {
            let from: AST.Expression | undefined, to: AST.Expression | undefined, condition: AST.Expression | undefined = undefined;
            return (from = this.expr(Precedence.OR))
                && this.expectPollS('->')
                && (to = this.expr(Precedence.OR))
                && (this.q.pollIfS('if') ? condition = this.expr(Precedence.OR) : true)
                && this.expectPoll('NEWLINE')
                && {kind: 'rule.rewrite', from, to, condition, pos: from.pos};
        }
        
        // statements
        
        /**
         * ```none
         * ModifiedStmt = '@' 'limit' Expression NEWLINE ModifiableStmt
         * ```
         */
        private parseModifiedStmt(): AST.ModifiedStmt | undefined {
            const {pos} = this.assertPollS('@');
            let name: Tokenizer.Token & {s: AST.ModifiedStmtName} | undefined, arg: AST.Expression | undefined, child: Exclude<AST.Statement, AST.DeclarationStmt> | undefined;
            return (name = this.expectPollS('limit'))
                && (arg = this.expr())
                && this.expectPoll('NEWLINE')
                && (child = this.parseModifiableStmt())
                && {kind: `stmt.modified.${name.s}`, arg, child, pos};
        }
        
        /**
         * ```none
         * BlockStmt = MarkovStmt | SequenceStmt
         * MarkovStmt = 'markov' BlockChildren<Statement>
         * SequenceStmt = 'sequence' BlockChildren<Statement>
         * ```
         */
        private parseBlockStmt(): AST.BlockStmt | undefined {
            const {s: kind, pos} = this.assertPollS('markov', 'sequence');
            const children = this.parseBlockChildren('stmt');
            return {kind: `stmt.block.${kind}`, children, pos};
        }
        
        /**
         * ```none
         * RuleBlockStmt = AllStmt | ConvolutionStmt | MapStmt | OnceStmt | OneStmt | PrlStmt
         * AllStmt = 'all' Args BlockChildren<Rule>
         * ConvolutionStmt = 'convolution' Args BlockChildren<Rule>
         * MapStmt = 'map' Args BlockChildren<Rule>
         * OnceStmt = 'once' BlockChildren<Rule>
         * OneStmt = 'one' Args BlockChildren<Rule>
         * PrlStmt = 'prl' BlockChildren<Rule>
         * ```
         */
        private parseRuleBlockStmt(): AST.RuleBlockStmt | undefined {
            const {s: kind, pos} = this.assertPollS('all', 'convolution', 'map', 'once', 'one', 'prl');
            const args = kind === 'prl' || kind === 'once' ? {} : this.parseArgs(kind);
            const rules = this.parseBlockChildren('rule');
            return args && ({kind: `stmt.rules.${kind}`, ...args, rules, pos} as AST.RuleBlockStmt);
        }
        
        /**
         * ```none
         * PassStmt = 'pass' NEWLINE
         * ```
         */
        private parsePassStmt(): AST.PassStmt {
            const {pos} = this.assertPollS('pass');
            return {kind: 'stmt.pass', pos};
        }
        
        /**
         * ```none
         * PutStmt = 'put' Expression 'at' Expression ('if' Expression)? NEWLINE
         * ```
         */
        private parsePutStmt(): AST.PutStmt | undefined {
            const {pos} = this.assertPollS('put');
            let pattern: AST.Expression | undefined, at: AST.Expression | undefined, condition: AST.Expression | undefined;
            return (pattern = this.expr(Precedence.OR))
                && this.expectPollS('at')
                && (at = this.expr(Precedence.OR))
                && (this.q.pollIfS('if') ? condition = this.expr(Precedence.OR) : true)
                && this.expectPoll('NEWLINE')
                && {kind: 'stmt.put', pattern, at, condition, pos};
        }
        
        /**
         * ```none
         * ConvChainStmt = 'convchain' Args NEWLINE
         * PathStmt = 'path' Args NEWLINE
         * ```
         */
        private parseLineArgsStmt(): AST.LineStmt | undefined {
            const {s: kind, pos} = this.assertPollS('convchain', 'path');
            const args = this.parseArgs(kind);
            return args
                && this.expectPoll('NEWLINE')
                && ({kind: `stmt.${kind}`, ...args, pos} as AST.LineStmt);
        }
        
        /**
         * ```none
         * BareUseStmt = GridExpr NEWLINE
         * ```
         */
        private parseBareUseStmt(): AST.UseExprStmt | undefined {
            const expr = this.parseGridExpr();
            return expr
                && this.expectPoll('NEWLINE')
                && {kind: 'stmt.use.expr', expr, pos: expr.pos};
        }
        
        /**
         * LogStmt = 'log' Expression NEWLINE
         */
        private parseLogStmt(): AST.LogStmt | undefined {
            const {pos, s: kind} = this.assertPollS('log');
            let expr: AST.Expression | undefined;
            return (expr = this.expr())
                && this.expectPoll('NEWLINE')
                && {kind: `stmt.${kind}`, expr, pos};
        }
        /**
         * UseStmt = UseExprStmt | UseLetStmt
         * UseExprStmt = 'use' Expression NEWLINE
         * UseLetStmt = 'use' LetDecl NEWLINE Statement*
         */
        private parseUseStmt(): AST.UseExprStmt | AST.UseLetStmt | undefined {
            const {pos} = this.assertPollS('use');
            if(this.q.hasNextS('let')) {
                let decl: AST.LetDecl | undefined;
                return (decl = this.parseLetDecl())
                    && this.expectPoll('NEWLINE')
                    && {kind: 'stmt.use.let', decl, children: this.parseUntil('stmt', 'DEDENT'), pos};
            } else {
                let expr: AST.Expression | undefined;
                return (expr = this.expr())
                    && this.expectPoll('NEWLINE')
                    && {kind: 'stmt.use.expr', expr, pos};
            }
        }
        
        // expressions
        
        private parseBracketedExpr(): AST.Expression | undefined {
            this.assertPollS('(');
            const expr = this.expr(Precedence.DECLARATION);
            this.expectPollS(')');
            return expr;
        }
        
        /**
         * ```none
         * DeclarationExpr = Declaration 'in' Expression
         * ```
         */
        private parseDeclarationExpr(): AST.DeclarationExpr | undefined {
            let declaration: AST.Declaration | undefined, child: AST.Expression | undefined;
            return (declaration = this.decl())
                && (this.expectPollIfS('in'), child = this.expr(Precedence.DECLARATION))
                && {kind: 'expr.decl', declaration, child, pos: declaration.pos};
        }
        
        /**
         * ```none
         * DictExpr = NameValuePairs
         * ```
         */
        private parseDictExpr(): AST.DictExpr | undefined {
            const {pos} = this.q.peek();
            const pairs = this.parseNameValuePairs();
            return pairs && {kind: 'expr.dict', pairs, pos};
        }
        
        /**
         * ```none
         * BoolLiteralExpr = 'false' | 'true'
         * ```
         */
        private parseBoolLiteralExpr(): AST.BoolLiteralExpr | undefined {
            const tok = this.expectPollS('false', 'true');
            return tok && {kind: 'expr.literal.bool', value: tok.s === 'true', pos: tok.pos};
        }
        
        /**
         * ```none
         * FloatLiteralExpr = FLOAT
         * ```
         */
        private parseFloatLiteralExpr(): AST.FloatLiteralExpr | undefined {
            const tok = this.expectPoll('FLOAT');
            return tok && {kind: 'expr.literal.float', value: parseFloat(tok.s), pos: tok.pos};
        }
        
        /**
         * ```none
         * IntLiteralExpr = INT
         * ```
         */
        private parseIntLiteralExpr(): AST.IntLiteralExpr | undefined {
            const tok = this.expectPoll('INT');
            if(tok === undefined) { return undefined; }
            const value = parseInt(tok.s) | 0;
            if(`${value}` !== tok.s) { this.diagnostics.syntaxError(`int literal '${tok.s}' out of range`, tok.pos); }
            return {kind: 'expr.literal.int', value, pos: tok.pos};
        }
        
        /**
         * ```none
         * PatternLiteralExpr = '[' (Char | CharSet)+ ('/' (Char | CharSet)+)* ']'
         * Char = PATTERN_CHAR
         * ```
         */
        private parsePatternLiteralExpr(): AST.PatternLiteralExpr | undefined {
            const beginTok = this.expectPollS('[');
            if(beginTok === undefined) { return undefined; }
            const {pos} = beginTok;
            
            let row: (AST.Char | AST.CharSet)[] = [];
            const rows: (AST.Char | AST.CharSet)[][] = [row];
            while(!this.q.pollIfS(']')) {
                const tok = this.q.poll();
                switch(tok.s) {
                    case '[':
                        row.push(this.parseCharSet(tok));
                        break;
                    case '/':
                        rows.push(row = []);
                        break;
                    default:
                        row.push(tok as AST.Char);
                        break;
                }
            }
            
            const width = row.length, height = rows.length;
            if(rows.some(row => row.length !== width)) {
                this.diagnostics.syntaxError('pattern must be rectangular', pos);
            } else if(width === 0) {
                this.diagnostics.syntaxError('empty pattern', pos);
            }
            return {kind: 'expr.literal.pattern', width, height, value: rows.flat(), pos};
        }
        
        /**
         * ```none
         * CharSet = '[' Char+ ']'
         * ```
         */
        private parseCharSet(beginTok: Tokenizer.Token): AST.CharSet {
            const {pos} = beginTok;
            const inverted = this.q.pollIfS('^');
            
            const chars: AST.Char[] = [];
            while(!this.q.pollIfS(']')) {
                const tok = this.expectPoll('PATTERN_CHAR');
                if(tok !== undefined) { chars.push(tok); }
            }
            if(chars.length === 0 && !inverted) { this.diagnostics.syntaxError('empty charset', pos); }
            
            return {kind: 'CHARSET', chars, inverted, pos};
        }
        
        /**
         * ```none
         * StrLiteralExpr = STRING
         * ```
         */
        private parseStrLiteralExpr(): AST.StrLiteralExpr | undefined {
            const {pos} = this.assertPoll('QUOTE');
            const s: string[] = [];
            while(!this.q.pollIf('QUOTE')) {
                const tok = this.q.poll();
                if(tok.kind === 'ESCAPED_CHAR') {
                    s.push(UNESCAPE_STRING_CHAR[tok.s] ?? tok.s.substring(1));
                } else {
                    s.push(tok.s);
                }
            }
            return {kind: 'expr.literal.str', value: s.join(''), pos};
        }
        
        /**
         * ```none
         * NameExpr = AttributeExpr | KeywordNameExpr | SimpleNameExpr
         * AttributeExpr = NameExpr '.' NAME
         * KeywordNameExpr = 'at' | 'origin' | 'random'
         * ```
         */
        private parseNameExpr(): AST.NameExpr | undefined {
            let expr: AST.AttributeExpr | AST.NameExpr | undefined;
            if(this.q.hasNext('NAME')) {
                expr = this.parseSimpleNameExpr();
            } else {
                const {s: name, pos} = this.assertPollS('at', 'origin', 'random');
                expr = {kind: 'expr.name.keyword', name, pos};
            }
            while(expr !== undefined && this.q.pollIfS('.')) {
                const attr = this.expectPoll('NAME');
                expr = attr && {kind: 'expr.attr', left: expr, attr: attr.s, pos: expr.pos};
            }
            return expr;
        }
        
        /**
         * ```none
         * SimpleNameExpr = NAME
         * ```
         */
        private parseSimpleNameExpr(): AST.SimpleNameExpr | undefined {
            const tok = this.expectPoll('NAME');
            return tok && {kind: 'expr.name.simple', name: tok.s, pos: tok.pos};
        }
        
        /**
         * ```none
         * GridExpr = 'grid' Args PatternLiteralExpr
         * ```
         */
        private parseGridExpr(): AST.GridExpr | undefined {
            const {pos} = this.assertPollS('grid');
            const args = this.parseArgs('grid');
            
            const alphabet = this.parsePatternLiteralExpr();
            if(alphabet === undefined) { return undefined; }
            
            if(alphabet.height > 1) {
                this.diagnostics.syntaxError(`alphabet must be a single row`, alphabet.pos);
            } else if(alphabet.width < 2) {
                this.diagnostics.syntaxError(`alphabet size must be at least 2`, alphabet.pos);
            } else if(alphabet.width > MAX_ALPHABET_SIZE) {
                this.diagnostics.syntaxError(`alphabet size cannot exceed ${MAX_ALPHABET_SIZE}`, alphabet.pos);
            }
            
            const alphabetKey: string[] = [];
            for(const c of alphabet.value) {
                if(c.kind === 'CHARSET') {
                    this.diagnostics.syntaxError(`alphabet cannot have unions`, c.pos);
                } else if(c.s === '.') {
                    this.diagnostics.syntaxError(`'.' cannot be an alphabet symbol`, c.pos);
                } else if(alphabetKey.includes(c.s)) {
                    this.diagnostics.syntaxError(`repeated alphabet symbol '${c.s}'`, c.pos);
                } else {
                    alphabetKey.push(c.s);
                }
            }
            
            return {kind: 'expr.grid', alphabetKey: alphabetKey.join(''), ...args, pos};
        }
    }
    
    export function parse(src: string): AST.CompilationUnit;
    export function parse<K extends Part>(src: string, part: K): PartMap[K] | undefined;
    export function parse(src: string, part: 'root' | Part = 'root'): AST.Node | undefined {
        const parser = new Parser(src);
        const ast = parser[part]();
        parser.diagnostics.throwIfAnyErrors();
        return ast;
    }
}
