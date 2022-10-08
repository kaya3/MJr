namespace IR {
    function _bit(i: number): Expr {
        return int(1 << (i & 31));
    }
    
    export class Flags {
        private vars: readonly NameExpr[];
        
        constructor(private readonly numFlags: number) {
            const numVars = (numFlags + 31) >> 5;
            this.vars = makeArray(numVars, NAMES.flag);
        }
        
        private _var(i: number): NameExpr {
            return this.vars[i >> 5];
        }
        
        public declare(): Stmt {
            const initialiser = this.numFlags === 1 ? FALSE : ZERO;
            return declVars(this.vars.map(v => ({
                name: v,
                type: INT_TYPE,
                initialiser,
            })), true);
        }
        
        public set(i: number): Stmt {
            const v = this._var(i);
            return this.numFlags === 1
                ? assign(v, '=', TRUE)
                : assign(v, '|=', _bit(i));
        }
        
        public clear(i: number): Stmt {
            const v =  this._var(i);
            return this.numFlags === 1
                ? assign(v, '=', FALSE)
                : assign(v, '&=', OP.bitwiseNot(_bit(i)));
        }
        
        public check(i: number): Expr {
            const v = this._var(i);
            return this.numFlags === 1
                ? v
                : OP.ne(OP.bitwiseAnd(this._var(i), _bit(i)), ZERO);
        }
    }
}
