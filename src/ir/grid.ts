///<reference path="names.ts"/>

namespace IR {
    const {
        WIDTH, HEIGHT,
        AT, AT_X, AT_Y,
    } = NAMES;
    
    export class Grid {
        public readonly width: NameExpr;
        public readonly height: NameExpr;
        public readonly n: NameExpr;
        public readonly data: NameExpr;
        private obj: NameExpr | undefined = undefined;
        private origin: NameExpr | undefined = undefined;
        public readonly originX: Expr;
        public readonly originY: Expr;
        
        public readonly counters = new Map<string, NameExpr>();
        public readonly samplers = new Map<string, Sampler>();
        public readonly convBuffers = new Map<string, ConvBuffer>();
        public readonly matcher: Matcher;
        
        public constructor(public readonly grid: ASG.FormalGrid) {
            const {scaleX, scaleY} = grid;
            
            this.width = scaleX === 1 ? WIDTH : NAMES.gridVar(this, 'width');
            this.height = scaleY === 1 ? HEIGHT : NAMES.gridVar(this, 'height');
            this.n = NAMES.gridVar(this, 'n');
            this.data = NAMES.gridVar(this, 'data');
            
            this.originX = scaleX % 2 === 0 ? OP.multConstant(WIDTH, scaleX >> 1) : OP.divConstant(this.width, 2);
            this.originY = scaleY % 2 === 0 ? OP.multConstant(HEIGHT, scaleY >> 1) : OP.divConstant(this.height, 2);
            
            // TODO: multiple matchers per grid?
            this.matcher = new Matcher(this, 0);
        }
        
        public makeCounter(patterns: readonly Pattern[]): Expr {
            const {counters, samplers, matcher} = this;
            
            const key = patterns.map(Pattern.key).join('\n');
            
            // TODO: this is order-dependent, a matching sampler might be declared later
            const sampler = samplers.get(key);
            if(sampler !== undefined) { return sampler.count; }
            
            let counter = counters.get(key);
            if(counter !== undefined) { return counter; }
            
            counters.set(key, counter = NAMES.counter(this, counters.size));
            for(const pattern of patterns) {
                matcher.addMatchHandler({kind: 'counter', pattern, counter});
            }
            return counter;
        }
        
        public makeSampler(patterns: readonly Pattern[]): Sampler {
            const {samplers, matcher} = this;
            
            const key = patterns.map(Pattern.key).join('\n');
            let sampler = samplers.get(key);
            if(sampler !== undefined) { return sampler; }
            
            samplers.set(key, sampler = new Sampler(samplers.size, this, patterns.length));
            
            for(let i = 0; i < patterns.length; ++i) {
                const pattern = patterns[i];
                matcher.addMatchHandler({kind: 'sampler', pattern, sampler, i});
            }
            return sampler;
        }
        
        public makeConvBuffer(p: ASG.ConvPattern): ConvBuffer {
            const {convBuffers} = this;
            const key = Convolution.Kernel.key(p.kernel);
            let buffer = convBuffers.get(key);
            if(buffer !== undefined) { return buffer; }
            
            const charsets: ISet[] = [];
            this.grid.convPatterns.forEach(q => {
                if(p.kernel.equals(q.kernel)) { charsets.push(q.chars); }
            });
            
            convBuffers.set(key, buffer = new ConvBuffer(convBuffers.size, this, charsets, p.kernel));
            return buffer;
        }
        
        public useOrigin(): NameExpr {
            return this.origin ??= NAMES.gridVar(this, 'origin');
        }
        public useObj(): NameExpr {
            return this.obj ??= NAMES.gridVar(this, 'obj');
        }
        public declareVars(): Stmt[] {
            const {width, height, n, data, obj, origin, originX, originY, grid} = this;
            const alphabetKey = grid.alphabet.key;
            const consts: VarDeclWithInitialiser[] = [];
            const vars: VarDeclWithInitialiser[] = [];
            
            if(width !== WIDTH) {
                consts.push({name: width, type: INT_TYPE, initialiser: OP.multConstant(WIDTH, grid.scaleX)});
            }
            if(height !== HEIGHT) {
                consts.push({name: height, type: INT_TYPE, initialiser: OP.multConstant(HEIGHT, grid.scaleY)});
            }
            consts.push(
                {name: n, type: INT_TYPE, initialiser: OP.mult(width, height)},
                {name: data, type: GRID_DATA_ARRAY_TYPE, initialiser: newGridDataArray(n)},
            );
            if(obj !== undefined) {
                const initialiser = libConstructorCall('Grid', [width, height, data, str(alphabetKey)]);
                consts.push({name: obj, type: GRID_TYPE, initialiser});
            }
            if(origin !== undefined) {
                consts.push({name: origin, type: INT_TYPE, initialiser: OP.add(originX, OP.mult(originY, width))});
            }
            
            for(const buffer of this.convBuffers.values()) {
                consts.push(...buffer.declare());
            }
            for(const sampler of this.samplers.values()) {
                consts.push(sampler.declare());
            }
            
            vars.push(...Array.from(this.counters.values(), counter => ({
                name: counter,
                type: INT_TYPE,
                initialiser: ZERO,
            })));
            
            return [
                declVars(consts),
                declVars(vars, true),
            ];
        }
        
        /**
         * Used internally for indices which are known to be in-bounds.
         * `x` and `y` must be non-negative.
         */
        public index(x: Expr, y: Expr): Expr {
            return OP.add(x, OP.mult(y, this.width));
        }
        
        /**
         * Used internally for relative indices which are known to be in-bounds.
         * The variables `AT`, `AT_X` and `AT_Y` must be declared in the IR,
         * and `dx` and `dy` must be non-negative.
         */
        public relativeIndex(dx: number, dy: number): Expr {
            if(this.grid.periodic) {
                const x = OP.mod(OP.add(AT_X, int(dx)), this.width);
                const y = OP.mod(OP.add(AT_Y, int(dy)), this.height);
                return OP.add(x, OP.mult(y, this.width));
            } else {
                return OP.add(OP.add(AT, int(dx)), OP.multConstant(this.width, dy));
            }
        }
        
        public checkedIndex(x: Expr, y: Expr): Expr {
            return libMethodCall('Grid', this.grid.periodic ? 'wrapIndex' : 'index', this.useObj(), [x, y]);
        }
        
        public access(index: Expr): ArrayAccessExpr {
            return access(this.data, index);
        }
        public write(index: Expr, colour: number, mask?: Mask): Stmt {
            return mask !== undefined ? mask.set(this, index, colour)
                : assign(this.access(index), '=', int(colour));
        }
        public update(x: Expr, y: Expr, w: Expr, h: Expr, doYield: boolean): Stmt[] {
            return [
                localCallStmt(this.matcher.updateFuncName, [x, y, w, h]),
                doYield ? this.yieldRewriteInfo(x, y, w, h) : PASS,
            ];
        }
        public yield_(): YieldStmt {
            return this.yieldRewriteInfo(ZERO, ZERO, this.width, this.height);
        }
        private yieldRewriteInfo(x: Expr, y: Expr, w: Expr, h: Expr): YieldStmt {
            return yield_(libConstructorCall('RewriteInfo', [this.useObj(), x, y, w, h]));
        }
    }
}
