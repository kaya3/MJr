namespace IR {
    export class Constants {
        private readonly names: NameExpr[] = [];
        private readonly types: IRType[] = [];
        private readonly interned = IDMap.withKey<Expr>(key);
        
        public intern(expr: Expr, type: IRType): NameExpr {
            const {names, types, interned} = this;
            const id = interned.getOrCreateID(expr);
            if(id >= names.length) {
                names.push(NAMES.constant(id));
                types.push(type);
            }
            return names[id];
        }
        
        public declare(): Stmt {
            const {names, types, interned} = this;
            return IR.declVars(interned.map((expr, i) => ({
                name: names[i],
                type: types[i],
                initialiser: expr,
            })));
        }
    }
}
