namespace IR {
    export interface ConstArray {
        decl: VarDeclWithInitialiser[];
        get(index: Expr): Expr;
    }
    
    export interface MutableArray extends ConstArray {
        readonly name: NameExpr;
        set(index: Expr, op: AssignOp, value: Expr): Stmt;
    }
    
    export interface ConstArray2D {
        readonly decl: VarDeclWithInitialiser[];
        get(j: Expr, i: Expr): Expr;
    }
    
    export interface MutableArray2D extends ConstArray2D {
        readonly name: NameExpr;
        set(j: Expr, i: Expr, op: AssignOp, value: Expr): Stmt;
    }
    
    export function makeConstArray(name: NameExpr, from: readonly number[], domainSize: number): ConstArray {
        if(from.length === 0) {
            return {decl: [], get: () => NULL};
        }
        
        // check if table is constant
        const start = from[0];
        if(from.every(x => x === start)) {
            return {decl: [], get: () => int(start)};
        }
        
        // check if table is an arithmetic progression
        // only consider domain sizes where loose int arithmetic is safe
        const step = from[1] - start;
        if(domainSize < 2 ** 31 && from.every((x, i) => x === start + i * step)) {
            return step >= 0
                ? {decl: [], get: index => OP.add(int(start), OP.multConstant(index, step))}
                : {decl: [], get: index => OP.minus(int(start), OP.multConstant(index, -step))};
        }
        
        return {
            decl: [constArrayDecl(name, from, domainSize)],
            get: index => access(name, index),
        };
    }
    
    export function makeConstArray2D(name: NameExpr, from: readonly number[], rowLength: number, domainSize: number): ConstArray2D {
        if(from.every((x, i) => x === from[i % rowLength])) {
            const col = from.slice(0, rowLength);
            const table = makeConstArray(name, col, domainSize);
            return {decl: [], get: (j, i) => table.get(i)};
        } else if(from.every((x, i) => x === from[i - i % rowLength])) {
            const row = makeArray((from.length / rowLength) | 0, i => from[i * rowLength]);
            return makeConstArray(name, row, domainSize);
        }
        
        return {
            decl: [constArrayDecl(name, from, domainSize, rowLength)],
            get: (j, i) => access(name, OP.multAddConstant(j, rowLength, i)),
        };
    }
    
    export function makeMutableArray(name: NameExpr, length: Expr, domainSize: number): MutableArray {
        if(domainSize <= 1 || length === ZERO) {
            return {name, decl: [], get: () => ZERO, set: () => PASS};
        }
        
        const arr = {
            name,
            decl: [newArrayDecl(name, length, domainSize)],
            get(index: Expr): ArrayAccessExpr {
                return access(name, index);
            },
            set(index: Expr, op: AssignOp, value: Expr): Stmt {
                return assign(this.get(index), op, value);
            },
        };
        return arr;
    }
    
    export function makeMutableArray2D(name: NameExpr, numRows: Expr, rowLength: Expr, domainSize: number): MutableArray2D {
        if(domainSize <= 1 || rowLength === ZERO || numRows === ZERO) {
            return {name, decl: [], get: () => ZERO, set: () => PASS};
        }
        
        const arr = {
            name,
            decl: [newArrayDecl(name, OP.mult(numRows, rowLength), domainSize)],
            get: (j: Expr, i: Expr): ArrayAccessExpr => {
                return access(name, OP.add(OP.mult(j, rowLength), i));
            },
            set(j: Expr, i: Expr, op: AssignOp, value: Expr): Stmt {
                return assign(this.get(j, i), op, value);
            },
        };
        if(rowLength.kind === 'expr.literal.int') {
            const width = rowLength.value;
            arr.get = (j, i) => access(name, OP.multAddConstant(j, width, i));
        }
        return arr;
    }
}
