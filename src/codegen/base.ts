namespace CodeGen {
    export type DeclWriteFuncs<T> = {
        readonly [K in Exclude<IR.Decl['kind'], 'decl.init' | 'decl.multi' | 'decl.none' | 'decl.var.loop'>]:
            (out: T, decl: Extract<IR.Decl, {readonly kind: K}>) => void
    }
    export type StmtWriteFuncs<T> = {
        readonly [K in Exclude<IR.Stmt['kind'], 'stmt.blankline' | 'stmt.decl' | 'stmt.sequence'>]:
            (out: T, stmt: Extract<IR.Stmt, {readonly kind: K}>) => void
    }
    export type ExprWriteFuncs<T> = {
        readonly [K in Exclude<IR.Expr['kind'], 'expr.op.binary' | 'expr.op.unary' | 'expr.unused.deferred' | 'expr.unused.error'>]:
            readonly [number, (out: T, expr: Extract<IR.Expr, {readonly kind: K}>) => void]
    }
    
    export const NOOP = Symbol();
    export interface BinaryOpSpec extends Readonly<{p: number, before: string, lMinP: number, between: string, rMinP: number, after: string}> {}
    export interface UnaryOpSpec extends Readonly<{p: number, before: string, cMinP: number, after: string}> {}
    export type BinaryOpSpecs = IRecord<IR.BinaryOp, BinaryOpSpec>
    export type UnaryOpSpecs = IRecord<IR.UnaryOp, UnaryOpSpec | typeof NOOP>
    
    export const enum Associativity {
        NEITHER = 0,
        LEFT = 1,
        RIGHT = 2,
        BOTH = LEFT | RIGHT,
    }
    
    export function binaryOp(p: number, before: string, lMinP: number, between: string, rMinP: number, after: string): BinaryOpSpec {
        return {p, before, lMinP, between, rMinP, after};
    }
    export function unaryOp(p: number, before: string, cMinP: number, after: string): UnaryOpSpec {
        return {p, before, cMinP, after};
    }
    export function infixOp(p: number, op: string, associativity = Associativity.LEFT): BinaryOpSpec {
        const lMinP = (associativity & Associativity.LEFT) !== 0 ? p : p + 1;
        const rMinP = (associativity & Associativity.RIGHT) !== 0 ? p : p + 1;
        return binaryOp(p, '', lMinP, ` ${op} `, rMinP, '');
    }
    export function prefixOp(p: number, op: string): UnaryOpSpec {
        return unaryOp(p, op, p, '');
    }
    
    export function statements(stmt: IR.Stmt): readonly IR.Stmt[] {
        return stmt.kind === 'stmt.sequence' ? stmt.children : [stmt];
    }
    
    type AllowedNode = Exclude<IR.Stmt | IR.Expr | IR.Decl, IR.MultiDecl | IR.NoDecl | IR.InitDecl | IR.DeclInStmt>
    function _getImmediatelyDeclaredNames(node: AllowedNode): IR.NameExpr[] {
        switch(node.kind) {
            case 'decl.func':
                return [node.name, ...node.params.map(p => p.name)];
            
            case 'decl.var.const':
            case 'decl.var.loop':
            case 'decl.var.mut':
            case 'decl.var.param':
                return [node.name];
            
            case 'stmt.for.range':
                return [node.index.name];
            case 'expr.letin':
                return [node.decl.name];
            
            default:
                return [];
        }
    }
    
    export abstract class Base {
        public readonly diagnostics = new Diagnostics();
        
        private readonly _out: string[] = [];
        private _indentationLevel: number = 0;
        private _indent: string = '';
        private readonly _toAssign = new Map<number, IR.Expr>();
        
        protected abstract readonly RESERVED_WORDS: string;
        protected abstract readonly STMT_WRITE_FUNCS: StmtWriteFuncs<this>;
        protected abstract readonly DECL_WRITE_FUNCS: DeclWriteFuncs<this>;
        protected abstract readonly EXPR_WRITE_FUNCS: ExprWriteFuncs<this>;
        protected abstract readonly BINARY_OPS: BinaryOpSpecs;
        protected abstract readonly UNARY_OPS: UnaryOpSpecs;
        protected readonly LPAREN: string = '(';
        protected readonly RPAREN: string = ')';
        protected readonly LBLOCK: string = '{';
        protected readonly RBLOCK: string = '}';
        
        private readonly _reserved = new Set<string>();
        private readonly _namesInUse: string[] = [];
        private readonly _namesMap = new Map<number, string>();
        
        public constructor(
            public readonly config: Compiler.Config,
        ) {}
        
        public beginGenerating(): void {
            const {_reserved} = this;
            for(const word of this.RESERVED_WORDS.split(/\s+/)) {
                _reserved.add(word);
            }
        }
        
        public beginLine(): void {
            const {_out} = this;
            if(_out.length > 0 && !_out[_out.length - 1].endsWith('\n')) { _out.push('\n'); }
            _out.push(this._indent);
        }
        public write(s: string): void {
            this._out.push(s);
        }
        public indent(): void {
            const i = ++this._indentationLevel;
            this._indent = ' '.repeat(this.config.indentSpaces * i);
        }
        public dedent(): void {
            const i = --this._indentationLevel;
            this._indent = ' '.repeat(this.config.indentSpaces * i);
        }
        
        public getName(expr: IR.NameExpr): string {
            return this._namesMap.get(expr.id) ?? fail(`use of name (${expr.namePart}) outside of its declared scope`, expr);
        }
        public getLibName(name: string): string {
            while(this._reserved.has(name)) {
                name += '_';
            }
            return name;
        }
        private declareNames(node: AllowedNode): void {
            const {_reserved, _namesInUse, _namesMap} = this;
            
            const nameExprs = _getImmediatelyDeclaredNames(node);
            for(const expr of nameExprs) {
                //if(_namesMap.has(expr.id)) { fail(`redeclared name`, expr); }
                
                let name = expr.namePart, suffix = 1;
                while(_reserved.has(name) || _namesInUse.includes(name)) {
                    ++suffix;
                    name = `${expr.namePart}${suffix}`;
                }
                _namesInUse.push(name);
                _namesMap.set(expr.id, name);
            }
        }
        public withScope(f: () => void): void {
            const {_namesInUse} = this;
            const n = _namesInUse.length;
            f();
            _namesInUse.length = n;
        }
        
        public writeBlockScope(stmt: IR.Stmt): void {
            this.withScope(() => this.writeIndentedBlock(stmt));
        }
        
        public writeIndentedBlock(stmt: IR.Stmt): void {
            this.write(this.LBLOCK);
            this.indent();
            this.writeStmt(stmt);
            this.dedent();
            this.beginLine();
            this.write(this.RBLOCK);
        }
        
        public writeStmt(stmt: IR.Stmt): void {
            while(stmt.kind === 'stmt.decl') {
                this.beginLine();
                this.writeDecl(stmt.decl);
                stmt = stmt.child;
            }
            
            switch(stmt.kind) {
                case 'stmt.blankline': {
                    this.beginLine();
                    return;
                }
                case 'stmt.comment': {
                    if(!this.config.emitComments) { return; }
                    break;
                }
                case 'stmt.sequence': {
                    for(const child of stmt.children) {
                        this.writeStmt(child);
                    }
                    return;
                }
            }
            
            this.declareNames(stmt);
            const f = this.STMT_WRITE_FUNCS[stmt.kind];
            f(this, stmt as never);
        }
        
        public writeDecl(decl: IR.Decl): void {
            switch(decl.kind) {
                case 'decl.var.loop':
                case 'decl.init': {
                    fail(`'${decl.kind}' was not replaced`);
                }
                case 'decl.none': {
                    return;
                }
                case 'decl.multi': {
                    for(const d of decl.children) {
                        this.writeDecl(d);
                    }
                    return;
                }
            }
            
            this.declareNames(decl);
            const f = this.DECL_WRITE_FUNCS[decl.kind];
            f(this, decl as never);
        }
        
        public writeExpr(expr: IR.Expr, minPrecedence: number = 0): void {
            switch(expr.kind) {
                case 'expr.unused.deferred': {
                    fail('deferred expression was not substituted', expr.purpose);
                }
                case 'expr.unused.error': {
                    fail('unused expression was not eliminated', expr.error);
                }
                case 'expr.letin': {
                    // moving initialiser forwards is only sound if it commutes
                    if(this.writeAssignExpr !== undefined && expr.decl.initialiser.info.commutesWith(expr.child)) {
                        this.declareNames(expr.decl);
                        this._toAssign.set(expr.decl.name.id, expr.decl.initialiser);
                        this.writeExpr(expr.child, minPrecedence);
                        return;
                    }
                    break;
                }
                case 'expr.name': {
                    const rhs = this._toAssign.get(expr.id);
                    if(this.writeAssignExpr !== undefined && rhs !== undefined) {
                        this._toAssign.delete(expr.id);
                        if(minPrecedence > 0) { this.write(this.LPAREN); }
                        this.writeAssignExpr(expr, rhs);
                        if(minPrecedence > 0) { this.write(this.RPAREN); }
                        return;
                    }
                    break;
                }
                case 'expr.op.unary': {
                    const spec = this.UNARY_OPS[expr.op];
                    if(spec === NOOP) {
                        this.writeExpr(expr.child, minPrecedence);
                    } else {
                        const {p, before, cMinP, after} = spec;
                        if(p < minPrecedence) { this.write(this.LPAREN); }
                        this.write(before);
                        this.writeExpr(expr.child, cMinP);
                        this.write(after);
                        if(p < minPrecedence) { this.write(this.RPAREN); }
                    }
                    return;
                }
                case 'expr.op.binary': {
                    const {p, before, lMinP, between, rMinP, after} = this.BINARY_OPS[expr.op];
                    if(p < minPrecedence) { this.write(this.LPAREN); }
                    this.write(before);
                    this.writeExpr(expr.left, lMinP);
                    this.write(between);
                    this.writeExpr(expr.right, rMinP)
                    this.write(after);
                    if(p < minPrecedence) { this.write(this.RPAREN); }
                    return;
                }
            }
            
            this.declareNames(expr);
            
            const [p, f] = this.EXPR_WRITE_FUNCS[expr.kind];
            if(p < minPrecedence) { this.write(this.LPAREN); }
            f(this, expr as never);
            if(p < minPrecedence) { this.write(this.RPAREN); }
        }
        
        public writeAssignExpr?(left: IR.NameExpr, right: IR.Expr): void;
        
        public writeList(writer: (i: number) => void, n: number, rowLength: number = IR.DEFAULT_ROW_LENGTH, compact: boolean = false, sep: string = ','): void {
            const multiline = rowLength < n;
            if(multiline) {
                this.indent();
                if(!compact) { this.beginLine(); }
            }
            
            for(let i = 0; i < n; ++i) {
                writer(i);
                if(i < n - 1) {
                    this.write(sep);
                    if(i % rowLength < rowLength - 1) {
                        this.write(' ');
                    } else {
                        this.beginLine();
                    }
                }
            }
            
            if(multiline) {
                this.dedent();
                if(!compact) { this.beginLine(); }
            }
        }
        
        public writeExprList(exprs: readonly IR.Expr[], rowLength: number = IR.DEFAULT_ROW_LENGTH, compact: boolean = false, sep: string = ','): void {
            this.writeList(i => this.writeExpr(exprs[i]), exprs.length, rowLength, compact, sep);
        }
        
        public writeLongStringLiteral(s: string, rowLength: number, concatOp: string = ' +'): void {
            if(s.length === 0) { this.write(`""`); return; }
            
            rowLength *= Math.ceil(4 * IR.DEFAULT_ROW_LENGTH / rowLength);
            this.writeList(i => {
                const start = i * rowLength;
                this.write(JSON.stringify(s.slice(start, start + rowLength)));
            }, Math.ceil(s.length / rowLength), 1, false, concatOp);
        }
        
        public render(): string {
            return this._out.join('');
        }
    }
    
    export function signedIntBits(domainSize: number): 8 | 16 | 32 {
        return domainSize <= (1 << 7) ? 8
            : domainSize <= (1 << 15) ? 16
            : 32;
    }
    export function uintBits(domainSize: number): 8 | 16 | 32 {
        return domainSize <= (1 << 8) ? 8
            : domainSize <= (1 << 16) ? 16
            : 32;
    }
    export function uintBitsFours(domainSize: number): 4 | 8 | 12 | 16 | 20 | 24 | 28 | 32 {
        return domainSize <= (1 << 4) ? 4
            : domainSize <= (1 << 8) ? 8
            : domainSize <= (1 << 12) ? 12
            : domainSize <= (1 << 16) ? 16
            : domainSize <= (1 << 20) ? 20
            : domainSize <= (1 << 24) ? 24
            : domainSize <= (1 << 28) ? 28
            : 32;
    }
    
    export function arrayToHex(arr: readonly number[], bitsPerElement: number): string {
        const digitsPerElement = bitsPerElement >> 2;
        return arr.map(x => x.toString(16).padStart(digitsPerElement, '0')).join('');
    }
}
