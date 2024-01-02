///<reference path="info.ts"/>

namespace IR {
    export function funcDecl(name: ConstNameExpr, yields: IRType | undefined, params: readonly ParamDecl[], returnType: IRType, body: Stmt): FuncDecl {
        let info = body.info.asFuncDecl(name);
        for(const param of params) {
            info = info.asVarDecl(param.name);
        }
        return {kind: 'decl.func', name, yields, params, returnType, body, info};
    }
    
    export function initDecl(child: StmtLevelDecl, stmt: Stmt): InitDecl {
        const info = child.info.then(stmt.info);
        return {kind: 'decl.init', child, stmt, info};
    }
    
    export function multiDecl(decls: readonly StmtLevelDecl[]): StmtLevelDecl {
        const children: StmtLevelDecl[] = [];
        let info = Info.DO_NOTHING;
        for(const decl of decls) {
            if(decl.kind === 'decl.multi') {
                children.push(...decl.children);
            } else if(decl.kind !== 'decl.none') {
                children.push(decl);
            }
            info = info.then(decl.info);
        }
        return children.length === 0 ? NO_DECL
            : children.length === 1 ? children[0]
            : {kind: 'decl.multi', children, info};
    }
    
    export const NO_DECL: NoDecl = {kind: 'decl.none', info: Info.DO_NOTHING};
}
