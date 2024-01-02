///<reference path="factory.ts"/>

namespace IR {
    function _maskN(length: Expr): Expr {
        return OP.divConstant(OP.addConstant(length, 31), 32);
    }
    
    export class Mask {
        public readonly arr: IR.MutableArray;
        private readonly clearFunc: IR.ConstNameExpr;
        private readonly setFunc: IR.ConstNameExpr;
        private readonly hasntFunc: IR.ConstNameExpr;
        private readonly capacity: IR.DeferredExpr;
        
        public constructor(private readonly ir: Factory) {
            const capacity = this.capacity = ir.deferredExpr('Mask.capacity');
            this.arr = makeMutableArray(ir, 'mask', capacity, INT32_ARRAY_TYPE.domainSize);
            this.clearFunc = ir.func('mask_clear');
            this.setFunc = ir.func('mask_set');
            this.hasntFunc = ir.func('mask_hasnt');
        }
        
        public declare(capacity: Expr): StmtLevelDecl {
            const {ir, arr} = this;
            const n = ir.paramDecl('n', INT_TYPE),
                grid = ir.paramDecl('g', GRID_DATA_ARRAY_TYPE),
                bitIndex = ir.paramDecl('j', INT_TYPE),
                setColour = ir.paramDecl('s', BYTE_TYPE);
            const index = OP.divConstant(bitIndex.name, 32);
            const bit = OP.lshift(ONE, OP.modConstant(bitIndex.name, 32));
            
            return multiDecl([
                replaceInDecl(arr.decl, this.capacity, _maskN(capacity)),
                funcDecl(
                    this.clearFunc,
                    undefined,
                    [n],
                    VOID_TYPE,
                    ir.withConst('len', INT_TYPE, _maskN(n.name), len =>
                        ir.forRange('i', ZERO, len, i => arr.set(i, '=', ZERO)),
                    ),
                ),
                funcDecl(
                    this.setFunc,
                    undefined,
                    [grid, bitIndex, setColour],
                    VOID_TYPE,
                    seq([
                        assign(access(grid.name, bitIndex.name), '=', setColour.name),
                        arr.set(index, '|=', bit),
                    ]),
                ),
                funcDecl(
                    this.hasntFunc,
                    undefined,
                    [bitIndex],
                    BOOL_TYPE,
                    return_(OP.eq(OP.bitwiseAnd(arr.get(index), bit), ZERO)),
                ),
            ]);
        }
        
        public clear(g: Grid): Stmt {
            return localCallStmt(this.clearFunc, [g.n]);
        }
        
        public set(g: Grid, index: Expr, colour: Expr): Stmt {
            return localCallStmt(this.setFunc, [g.data.array, index, colour]);
        }
        
        public hasnt(index: Expr): Expr {
            return localCall(this.hasntFunc, [index], true);
        }
        
        public patternFits(g: Grid, p: PatternResult, at: Location): Expr {
            return p.constant !== undefined
                ? OP.all(p.constant, (dx, dy) => this.hasnt(g.relativeIndex(at, dx, dy)))
                : libMethodCall('Pattern', 'fitsMask', p.expr, [g.obj, this.arr.array, at.x, at.y]);
        }
    }
}
