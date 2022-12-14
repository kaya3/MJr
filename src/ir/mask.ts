///<reference path="names.ts"/>

namespace IR {
    const {
        MASK, MASK_CLEAR, MASK_HASNT, MASK_SET,
        WIDTH, HEIGHT,
        AT_X, AT_Y,
        G, I, N, P, S,
    } = NAMES;
    
    export class Mask {
        private scale: number = 0;
        
        public readonly name = MASK;
        
        private maskN(length: Expr): Expr {
            return OP.divConstant(OP.addConstant(length, 31), 32);
        }
        
        public use(g: Grid) {
            this.scale = Math.max(this.scale, g.grid.scaleX * g.grid.scaleY);
        }
        
        public declare(): Stmt[] {
            if(this.scale === 0) { return []; }
            
            const {name} = this;
            const arr = makeMutableArray(
                name,
                this.maskN(OP.multConstant(OP.mult(WIDTH, HEIGHT), this.scale)),
                INT32_ARRAY_TYPE.domainSize,
            );
            const index = OP.divConstant(I, 32);
            const bit = OP.lshift(ONE, OP.modConstant(I, 32));
            return [
                declVars(arr.decl),
                declFunc(
                    MASK_CLEAR,
                    undefined,
                    [N],
                    [INT_TYPE],
                    VOID_TYPE,
                    block([
                        assign(N, '=', this.maskN(N)),
                        forRange(I, ZERO, N, [arr.set(I, '=', ZERO)]),
                    ]),
                ),
                declFunc(
                    MASK_SET,
                    undefined,
                    [G, I, S],
                    [GRID_DATA_ARRAY_TYPE, INT_TYPE, BYTE_TYPE],
                    VOID_TYPE,
                    block([
                        assign(access(G, I), '=', S),
                        arr.set(index, '|=', bit),
                    ]),
                ),
                declFunc(
                    MASK_HASNT,
                    undefined,
                    [I],
                    [INT_TYPE],
                    BOOL_TYPE,
                    return_(OP.eq(OP.bitwiseAnd(arr.get(index), bit), ZERO)),
                ),
                BLANK_LINE,
            ];
        }
        
        public clear(g: Grid): Stmt {
            return localCallStmt(MASK_CLEAR, [g.n]);
        }
        
        public set(g: Grid, index: Expr, colour: Expr): Stmt {
            return localCallStmt(MASK_SET, [g.data.name, index, colour]);
        }
        
        public hasnt(index: Expr): Expr {
            return localCall(MASK_HASNT, [index]);
        }
        
        public patternFits(g: Grid, patternExpr: ASG.Prop<'pattern.out'>): Expr {
            return patternExpr.kind === 'expr.constant'
                ? patternExpr.constant.value.map((dx, dy) => this.hasnt(g.relativeIndex(dx, dy))).reduce(OP.and)
                : libMethodCall('Pattern', 'fitsMask', P, [g.useObj(), this.name, AT_X, AT_Y]);
        }
    }
}
