namespace IR {
    export interface ConstArray {
        readonly decl: StmtLevelDecl;
        get(index: Expr): Expr;
    }
    
    export interface MutableArray extends ConstArray {
        readonly array: ConstNameExpr;
        set(index: Expr, op: AssignOp, value: Expr): Stmt;
    }
    
    export interface ConstArray2D {
        readonly decl: StmtLevelDecl;
        get(j: Expr, i: Expr): Expr;
    }
    
    export interface MutableArray2D extends ConstArray2D {
        readonly name: ConstNameExpr;
        set(j: Expr, i: Expr, op: AssignOp, value: Expr): Stmt;
    }
    
    export function makeConstArray(ir: Factory, namePart: string, from: readonly number[], domainSize: number): ConstArray {
        if(from.length === 0) {
            return {decl: NO_DECL, get: () => unusedExpr('array of length zero')};
        }
        
        // check if table is constant
        const start = from[0];
        if(from.every(x => x === start)) {
            return {decl: NO_DECL, get: () => int(start)};
        }
        
        // check if table is an arithmetic progression
        // only consider domain sizes where loose int arithmetic is safe
        const step = from[1] - start;
        if(domainSize < 2 ** 31 && from.every((x, i) => x === start + i * step)) {
            return step >= 0
                ? {decl: NO_DECL, get: index => OP.add(int(start), OP.multConstant(index, step))}
                : {decl: NO_DECL, get: index => OP.minus(int(start), OP.multConstant(index, -step))};
        }
        
        const decl = ir.constArrayDecl(namePart, from, domainSize);
        return {
            decl,
            get: index => access(decl.name, index),
        };
    }
    
    export function makeConstArray2D(ir: Factory, namePart: string, from: readonly number[], rowLength: number, domainSize: number): ConstArray2D {
        if(from.every((x, i) => x === from[i % rowLength])) {
            const row = from.slice(0, rowLength);
            const table = makeConstArray(ir, namePart, row, domainSize);
            return {decl: table.decl, get: (j, i) => table.get(i)};
        } else if(from.every((x, i) => x === from[i - i % rowLength])) {
            const col = makeArray((from.length / rowLength) | 0, i => from[i * rowLength]);
            return makeConstArray(ir, namePart, col, domainSize);
        }
        
        const decl = ir.constArrayDecl(namePart, from, domainSize, rowLength);
        return {
            decl,
            get: (j, i) => access(decl.name, OP.multAddConstant(j, rowLength, i)),
        };
    }
    
    export function makeMutableArray(ir: Factory, namePart: string, length: Expr, domainSize: number): MutableArray {
        const decl = ir.mutArrayDecl(namePart, length, domainSize);
        
        if(domainSize <= 1 || length === ZERO) {
            return {array: decl.name, decl, get: () => ZERO, set: () => PASS};
        }
        
        const arr = {
            array: decl.name,
            decl,
            get(index: Expr): ArrayAccessExpr {
                return access(decl.name, index);
            },
            set(index: Expr, op: AssignOp, value: Expr): Stmt {
                return assign(this.get(index), op, value);
            },
        };
        return arr;
    }
    
    export function makeMutableArray2D(ir: Factory, namePart: string, numRows: Expr, rowLength: Expr, domainSize: number): MutableArray2D {
        const decl = ir.mutArrayDecl(namePart, OP.mult(numRows, rowLength), domainSize);
        
        if(domainSize <= 1 || rowLength === ZERO || numRows === ZERO) {
            return {name: decl.name, decl, get: () => ZERO, set: () => PASS};
        }
        
        const get = isInt(rowLength)
            ? (j: Expr, i: Expr) => access(decl.name, OP.multAddConstant(j, rowLength.value, i))
            : (j: Expr, i: Expr) => access(decl.name, OP.add(OP.mult(j, rowLength), i));
        return {
            name: decl.name,
            decl,
            get,
            set: (j, i, op, value) => assign(get(j, i), op, value),
        };
    }
}
