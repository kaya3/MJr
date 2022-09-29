namespace CodeGen {
    export type StmtWriteFuncs<T> = {
        readonly [K in IR.Stmt['kind']]: (out: T, stmt: Extract<IR.Stmt, {readonly kind: K}>) => void
    }
    export type ExprWriteFuncs<T> = {
        readonly [K in Exclude<IR.Expr['kind'], 'expr.op.binary' | 'expr.op.unary'>]: readonly [number, (out: T, expr: Extract<IR.Expr, {readonly kind: K}>) => void]
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
    
    export abstract class Base {
        public readonly diagnostics = new Diagnostics();
        
        private readonly _out: string[] = [];
        private _indentationLevel: number = 0;
        private _indent: string = '';
        private readonly _toAssign = new Map<string, IR.Expr>();
        
        protected abstract readonly STMT_WRITE_FUNCS: StmtWriteFuncs<this>;
        protected abstract readonly EXPR_WRITE_FUNCS: ExprWriteFuncs<this>;
        protected abstract readonly BINARY_OPS: BinaryOpSpecs;
        protected abstract readonly UNARY_OPS: UnaryOpSpecs;
        protected LPAREN = '(';
        protected RPAREN = ')';
        
        public constructor(public readonly config: Compiler.Config) {}
        
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
        
        public writeStmt(stmt: IR.Stmt): void {
            const f = this.STMT_WRITE_FUNCS[stmt.kind];
            f(this, stmt as never);
        }
        
        public writeExpr(expr: IR.Expr, minPrecedence: number = 0): void {
            switch(expr.kind) {
                case 'expr.letin': {
                    if(this.writeAssignExpr !== undefined) {
                        for(const decl of expr.decls) {
                            this._toAssign.set(decl.name.name, decl.initialiser);
                        }
                        this.writeExpr(expr.child, minPrecedence);
                        return;
                    }
                    break;
                }
                case 'expr.name': {
                    const rhs = this._toAssign.get(expr.name);
                    if(this.writeAssignExpr !== undefined && rhs !== undefined) {
                        this._toAssign.delete(expr.name);
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
            const [p, f] = this.EXPR_WRITE_FUNCS[expr.kind];
            if(p < minPrecedence) { this.write(this.LPAREN); }
            f(this, expr as never);
            if(p < minPrecedence) { this.write(this.RPAREN); }
            return;
        }
        
        public writeAssignExpr?(left: IR.NameExpr, right: IR.Expr): void;
        
        public writeIndentedBlock(stmt: IR.Stmt): void {
            this.writeStmt(stmt.kind !== 'stmt.block' ? {kind: 'stmt.block', children: [stmt]} : stmt);
        }
        
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
    
    export function arrayToHex(arr: readonly number[], bitsPerElement: number): string {
        const digitsPerElement = bitsPerElement >> 2;
        return arr.map(x => x.toString(16).padStart(digitsPerElement, '0')).join('');
    }
}
