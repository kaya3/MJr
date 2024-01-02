///<reference path="./expr.ts"/>
///<reference path="./stmt.ts"/>
///<reference path="./types.ts"/>

namespace IR {
    export class Factory {
        private lastID: number = -1;
        private nextName<T extends boolean>(namePart: string, isMutable: T): NameExpr & {readonly isMutable: T} {
            const id = ++this.lastID;
            return {kind: 'expr.name', id, namePart, isMutable, info: isMutable ? Info.varGet(id) : Info.constGet(id)};
        }
        
        deferredExpr(purpose: string): DeferredExpr {
            return {kind: 'expr.unused.deferred', id: ++this.lastID, purpose, info: Info.DEFERRED}
        }
        
        func(namePart: string): ConstNameExpr {
            return this.nextName(namePart, false);
        }
        constDecl(namePart: string, type: IRType, initialiser: Expr): ConstDecl {
            const name = this.nextName(namePart, false);
            return {kind: 'decl.var.const', name, type, initialiser, info: initialiser.info};
        }
        loopVarDecl(namePart: string = 'i', type: IRType = INT_TYPE): LoopVarDecl {
            const name = this.nextName(namePart, false);
            return {kind: 'decl.var.loop', name, type, info: Info.DO_NOTHING};
        }
        paramDecl(namePart: string, type: IRType, isOptional: boolean = false): ParamDecl {
            const name = this.nextName(namePart, false);
            let initialiser = undefined;
            if(isOptional) {
                type = nullableType(type);
                initialiser = NULL;
            }
            return {kind: 'decl.var.param', name, type, isOptional, initialiser, info: Info.DO_NOTHING};
        }
        varDecl(namePart: string, type: IRType, initialiser?: Expr): MutVarDecl {
            const name = this.nextName(namePart, true);
            const info = initialiser !== undefined ? initialiser.info : Info.DO_NOTHING;
            return {kind: 'decl.var.mut', name, type, initialiser, info};
        }
        
        flag(): Readonly<{decl: MutVarDecl, set: Stmt, unset: Stmt, check: Expr}> {
            const decl = this.varDecl('flag', BOOL_TYPE, FALSE);
            return {
                decl,
                set: assign(decl.name, '=', TRUE),
                unset: assign(decl.name, '=', FALSE),
                check: decl.name,
            };
        }
        constArrayDecl(namePart: string, from: readonly number[], domainSize: number, rowLength: number = DEFAULT_ROW_LENGTH): ConstDecl {
            return this.constDecl(namePart, constArrayType(domainSize), constArray(from, domainSize, rowLength));
        }
        mutArrayDecl(namePart: string, length: Expr, domainSize: number): ConstDecl {
            return this.constDecl(namePart, mutArrayType(domainSize), newArray(length, domainSize));
        }
        
        withConst(namePart: string, type: IRType, initialiser: Expr, child: (name: ConstExpr) => Stmt): Stmt {
            if(isSimpleConstant(initialiser)) {
                return child(initialiser);
            }
            
            const name = this.nextName(namePart, false);
            return withConst(name, type, initialiser, child(name));
        }
        withVar(namePart: string, type: IRType, initialiser: Expr | undefined, child: (name: MutNameExpr) => Stmt): Stmt {
            const name = this.nextName(namePart, true);
            return withVar(name, type, initialiser, child(name));
        }
        
        forRange(namePart: string, low: Expr, high: Expr, body: (name: ConstExpr) => Stmt, reverse: boolean = false): Stmt {
            const decl = this.loopVarDecl(namePart);
            return forRange(decl, low, high, body(decl.name), reverse);
        }
        forRangeReverse(namePart: string, low: Expr, high: Expr, body: (name: ConstExpr) => Stmt): Stmt {
            return this.forRange(namePart, low, high, body, true);
        }
    }
}
