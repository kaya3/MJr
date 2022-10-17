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
        public readonly data: MutableArray;
        private obj: NameExpr | undefined = undefined;
        private buffer: MutableArray | undefined = undefined;
        private origin: NameExpr | undefined = undefined;
        private lfsrFeedbackTerm: NameExpr | undefined = undefined;
        public readonly originX: Expr;
        public readonly originY: Expr;
        
        private readonly counters = new Map<string, NameExpr>();
        private readonly samplers = new Map<string, AbstractSampler>();
        private readonly convBuffers = new Map<string, ConvBuffer>();
        public readonly matcher: Matcher;
        
        private scale: number = 1;
        
        public constructor(public readonly grid: ASG.FormalGrid) {
            const {scaleX, scaleY} = grid;
            
            this.width = scaleX === 1 ? WIDTH : NAMES.gridVar(this, 'width');
            this.height = scaleY === 1 ? HEIGHT : NAMES.gridVar(this, 'height');
            this.n = NAMES.gridVar(this, 'n');
            this.data = makeMutableArray(NAMES.gridVar(this, 'data'), this.n, GRID_DATA_ARRAY_TYPE.domainSize);
            
            this.originX = scaleX % 2 === 0 ? OP.multConstant(WIDTH, scaleX >> 1) : OP.divConstant(this.width, 2);
            this.originY = scaleY % 2 === 0 ? OP.multConstant(HEIGHT, scaleY >> 1) : OP.divConstant(this.height, 2);
            
            // TODO: multiple matchers per grid?
            this.matcher = new Matcher(this, 0);
        }

        public getScale(): number {
            return this.grid.scaleX * this.grid.scaleY * this.scale;
        }
        
        public makeCounter(patterns: readonly PatternTree[]): Expr {
            const {counters, samplers, matcher} = this;
            
            const key = patterns.map(PatternTree.key).join('\n');
            
            // TODO: this is order-dependent, a matching sampler might be declared later
            const sampler = samplers.get(key);
            if(sampler !== undefined) { return sampler.count; }
            
            return getOrCompute(counters, key, () => {
                const counter = NAMES.counter(this, counters.size);
                for(const pattern of patterns) {
                    matcher.addMatchHandler({kind: 'counter', pattern, counter, weight: 1});
                }
                // need to set scale to avoid overflowing the counter
                this.scale = Math.max(this.scale, patterns.length);
                return counter;
            });
        }
        
        public makeWeightedCounter(weights: readonly ASG.WeightedPattern[]): Expr {
            const {counters, matcher} = this;
            
            const key = weights.map(w => `${PatternTree.key(w.pattern)} @ ${w.weight}`).join('\n');
            
            return getOrCompute(counters, key, () => {
                const counter = NAMES.counter(this, counters.size);
                let totalWeight = 0;
                for(const {pattern, weight} of weights) {
                    matcher.addMatchHandler({kind: 'counter', pattern, counter, weight});
                    totalWeight += weight;
                }
                // need to set scale to avoid overflowing the counter
                this.scale = Math.max(this.scale, totalWeight);
                return counter;
            });
        }
        
        public makeSampler(patterns: readonly PatternTree[]): AbstractSampler {
            const {samplers, matcher} = this;
            
            const key = patterns.map(PatternTree.key).join('\n');
            return getOrCompute(samplers, key, () => {
                if(patterns.length === 1 && patterns[0].kind === 'top') {
                    const {width, height} = patterns[0];
                    return new TrivialSampler(this, width, height);
                }
                
                const sampler = new Sampler(samplers.size, this, patterns.length);
                for(let i = 0; i < patterns.length; ++i) {
                    const pattern = patterns[i];
                    matcher.addMatchHandler({kind: 'sampler', pattern, sampler, i});
                }
                this.scale = Math.max(this.scale, patterns.length);
                return sampler;
            });
        }
        
        public makeConvBuffer(kernel: Convolution.Kernel): ConvBuffer {
            const {convBuffers} = this;
            const key = Convolution.Kernel.key(kernel);
            return getOrCompute(convBuffers, key, () => {
                const patterns = this.grid.convPatterns.filter(p => p.kernel.equals(kernel));
                this.scale = Math.max(this.scale, patterns.length);
                return new ConvBuffer(convBuffers.size, this, patterns, kernel);
            });
        }
        
        public useOrigin(): NameExpr {
            return this.origin ??= NAMES.gridVar(this, 'origin');
        }
        
        public useObj(): NameExpr {
            return this.obj ??= NAMES.gridVar(this, 'obj');
        }
        
        public useBuffer(): MutableArray {
            return this.buffer ??= makeMutableArray(
                NAMES.gridVar(this, 'buffer'),
                this.n,
                GRID_DATA_ARRAY_TYPE.domainSize,
            );
        }
        
        public useLsfrFeedbackTerm(): NameExpr {
            return this.lfsrFeedbackTerm ??= NAMES.gridVar(this, 'lfsrFeedbackTerm');
        }
        
        public declare(): Stmt[] {
            const {width, height, n, data, obj, buffer, origin, lfsrFeedbackTerm, grid} = this;
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
                ...data.decl,
            );
            if(obj !== undefined) {
                const initialiser = libConstructorCall('Grid', [width, height, data.name, str(alphabetKey)]);
                consts.push({name: obj, type: GRID_TYPE, initialiser});
            }
            if(buffer !== undefined) {
                consts.push(...buffer.decl);
            }
            if(origin !== undefined) {
                consts.push({name: origin, type: INT_TYPE, initialiser: OP.add(this.originX, OP.mult(this.originY, width))});
            }
            if(lfsrFeedbackTerm !== undefined) {
                consts.push({name: lfsrFeedbackTerm, type: INT_TYPE, initialiser: libFunctionCall('lfsrFeedbackTerm', [n])});
            }
            
            vars.push(...Array.from(this.counters.values(), counter => ({
                name: counter,
                type: INT_TYPE,
                initialiser: ZERO,
            })));
            
            return [
                declVars(consts),
                declVars(vars, true),
                ...Array.from(this.convBuffers.values(), buffer => buffer.declare()),
                ...Array.from(this.samplers.values(), sampler => sampler.declare()),
                BLANK_LINE,
                ...this.matcher.declareUpdateFunc(),
            ];
        }
        
        public attr(attr: Type.GridAttribute): Expr {
            switch(attr) {
                case 'area': return this.n;
                case 'width': return this.width;
                case 'height': return this.height;
            }
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
                const x = OP.mod(OP.addConstant(AT_X, dx), this.width);
                const y = OP.mod(OP.addConstant(AT_Y, dy), this.height);
                return OP.add(x, OP.mult(y, this.width));
            } else {
                return OP.add(OP.addConstant(AT, dx), OP.multConstant(this.width, dy));
            }
        }
        
        public checkedIndex(x: Expr, y: Expr): Expr {
            return libMethodCall('Grid', this.grid.periodic ? 'wrapIndex' : 'index', this.useObj(), [x, y]);
        }
        
        public declareAtIndex(index: Expr): Stmt {
            const decls: VarDeclWithInitialiser[] = [];
            if(index !== AT) { decls.push({name: AT, type: INT_TYPE, initialiser: index}); }
            decls.push(
                {name: AT_X, type: INT_TYPE, initialiser: OP.mod(AT, this.width)},
                {name: AT_Y, type: INT_TYPE, initialiser: OP.floordiv(AT, this.width)},
            );
            return declVars(decls);
        }
        
        public declareAtXY(x: Expr, y: Expr): Stmt {
            const decls: VarDeclWithInitialiser[] = [];
            if(x !== AT_X) { decls.push({name: AT_X, type: INT_TYPE, initialiser: x}); }
            if(y !== AT_Y) { decls.push({name: AT_Y, type: INT_TYPE, initialiser: y}); }
            decls.push({name: AT, type: INT_TYPE, initialiser: this.index(AT_X, AT_Y)});
            return declVars(decls);
        }
        
        public write(index: Expr, colour: Expr, mask?: Mask): Stmt {
            return mask !== undefined ? mask.set(this, index, colour)
                : this.data.set(index, '=', colour);
        }
        
        public update(x: Expr, y: Expr, w: Expr, h: Expr): Stmt {
            return this.matcher.update(x, y, w, h);
        }
        
        public yield_(): YieldStmt {
            return this.yieldRewriteInfo(ZERO, ZERO, this.width, this.height);
        }
        
        public yieldRewriteInfo(x: Expr, y: Expr, w: Expr, h: Expr): YieldStmt {
            return yield_(libConstructorCall('RewriteInfo', [this.useObj(), x, y, w, h]));
        }
    }
}
