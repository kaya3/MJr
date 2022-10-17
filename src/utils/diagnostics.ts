class Diagnostics {
    public static readonly MAX_ERRORS = 100;
    
    readonly errors: string[] = [];
    
    public throwIfAnyErrors(): void {
        if(this.errors.length > 0) { throw this; }
    }
    
    private error(prefix: string, msg: string, pos?: SourcePosition): void {
        if(pos !== undefined) {
            msg += ` at line ${pos.line}, col ${pos.col}`;
        }
        this.errors.push(`${prefix}: ${msg}`);
        if(this.errors.length >= Diagnostics.MAX_ERRORS) { throw this; }
    }
    
    public configError(msg: string): void {
        this.error('Configuration error', msg);
    }
    
    public syntaxError(msg: string, pos: SourcePosition): void {
        this.error('Syntax error', msg, pos);
    }
    
    public compilationError(msg: string, pos: SourcePosition): void {
        this.error('Compilation error', msg, pos);
    }
    
    public typeError(msg: string, pos: SourcePosition): void {
        this.error('Type error', msg, pos);
    }
}
