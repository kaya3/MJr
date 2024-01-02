///<reference path="factory.ts"/>

namespace IR {
    export interface Location extends Readonly<{index: Expr, x: Expr, y: Expr}> {}
    
    export class Grid {
        public readonly width: ConstNameExpr;
        public readonly height: ConstNameExpr;
        public readonly n: ConstNameExpr;
        public readonly data: MutableArray;
        public readonly obj: ConstNameExpr;
        public readonly origin: ConstNameExpr;
        public readonly buffer: MutableArray;
        public readonly lfsrFeedbackTerm: ConstNameExpr;
        public readonly originX: Expr;
        public readonly originY: Expr;
        
        private readonly counters = new Map<string, MutVarDecl>();
        private readonly samplers = new Map<string, AbstractSampler>();
        private readonly convBuffers = new Map<string, ConvBuffer>();
        public readonly matcher: Matcher;
        
        private readonly decls: readonly StmtLevelDecl[];
        
        private scale: number = 1;
        
        public constructor(private readonly ir: Factory, public readonly grid: ASG.FormalGrid, baseWidth: ConstNameExpr, baseHeight: ConstNameExpr) {
            const {scaleX, scaleY} = grid;
            
            const width = ir.constDecl(`grid${grid.id}_width`, INT_TYPE, OP.multConstant(baseWidth, scaleX)),
                height = ir.constDecl(`grid${grid.id}_height`, INT_TYPE, OP.multConstant(baseHeight, scaleY)),
                w = width.name,
                h = height.name,
                area = ir.constDecl(`grid${grid.id}_n`, INT_TYPE, OP.mult(w, h)),
                n = area.name,
                data = makeMutableArray(ir, `grid${grid.id}_data`, n, GRID_DATA_ARRAY_TYPE.domainSize),
                obj = ir.constDecl(`grid${grid.id}_obj`, GRID_TYPE, libConstructorCall('Grid', [w, h, data.array, str(grid.alphabet.key)])),
                originX = scaleX % 2 === 0 ? OP.multConstant(baseWidth, scaleX >> 1) : OP.divConstant(w, 2),
                originY = scaleY % 2 === 0 ? OP.multConstant(baseHeight, scaleY >> 1) : OP.divConstant(h, 2),
                origin = ir.constDecl(`grid${grid.id}_origin`, INT_TYPE, OP.add(originX, OP.mult(originY, w))),
                lfsrFeedbackTerm = ir.constDecl(`grid${grid.id}_lfsrFeedbackTerm`, INT_TYPE, libFunctionCall('lfsrFeedbackTerm', [n])),
                buffer = makeMutableArray(ir, `grid${grid.id}_buffer`, n, GRID_DATA_ARRAY_TYPE.domainSize);
            
            this.width = width.name;
            this.height = height.name;
            this.n = n;
            this.data = data;
            this.obj = obj.name;
            this.origin = origin.name;
            this.originX = originX;
            this.originY = originY;
            this.lfsrFeedbackTerm = lfsrFeedbackTerm.name;
            this.buffer = buffer;
            
            // TODO: multiple matchers per grid?
            this.matcher = new Matcher(ir, this);
            
            this.decls = [
                width,
                height,
                area,
                data.decl,
                obj,
                origin,
                lfsrFeedbackTerm,
                buffer.decl,
            ];
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
                const counterDecl = this.ir.varDecl(`grid${this.grid.id}_counter`, INT_TYPE, ZERO);
                for(const pattern of patterns) {
                    matcher.addMatchHandler({kind: 'counter', pattern, counter: counterDecl.name, weight: ONE});
                }
                // need to set scale to avoid overflowing the counter
                this.scale = Math.max(this.scale, patterns.length);
                return counterDecl;
            }).name;
        }
        
        public makeWeightedCounter(weights: readonly ASG.WeightedPattern[], useFloat: boolean): MutNameExpr {
            const {counters, matcher} = this;
            
            const key = weights.map(w => `${PatternTree.key(w.pattern)} @ ${w.weight}`).join('\n') + `\nuseFloat=${useFloat}`;
            
            return getOrCompute(counters, key, () => {
                const counterDecl = this.ir.varDecl(
                    `grid${this.grid.id}_counter`,
                    useFloat ? FLOAT_TYPE : INT_TYPE,
                    useFloat ? FLOAT_ZERO : ZERO,
                );
                let totalWeight = 0;
                for(const {pattern, weight} of weights) {
                    matcher.addMatchHandler({
                        kind: 'counter',
                        pattern,
                        counter: counterDecl.name,
                        weight: useFloat ? float(weight) : int(weight),
                    });
                    totalWeight += weight;
                }
                // need to set scale to avoid overflowing the counter
                if(!useFloat) {
                    this.scale = Math.max(this.scale, totalWeight);
                }
                return counterDecl;
            }).name;
        }
        
        public makeSampler(patterns: readonly PatternTree[]): AbstractSampler {
            const {samplers, matcher} = this;
            
            const key = patterns.map(PatternTree.key).join('\n');
            return getOrCompute(samplers, key, () => {
                if(patterns.length === 1 && patterns[0].kind === 'top') {
                    const {width, height} = patterns[0];
                    return new TrivialSampler(this.ir, this, width, height);
                } else if(patterns.length === 0 || (patterns.length === 1 && patterns[0].kind === 'bottom')) {
                    return new EmptySampler();
                }
                
                const sampler = new Sampler(this.ir, this, patterns.length);
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
                return new ConvBuffer(this.ir, this, patterns, kernel);
            });
        }
        
        public declare(): StmtLevelDecl {
            const decls: StmtLevelDecl[] = [
                ...this.decls,
                ...this.counters.values(),
            ];
            for(const sampler of this.samplers.values()) {
                decls.push(sampler.declare());
            }
            for(const buffer of this.convBuffers.values()) {
                decls.push(buffer.declare());
            }
            decls.push(this.matcher.declare());
            return multiDecl(decls);
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
         * The variables `dx` and `dy` must be non-negative.
         */
        public relativeIndex(at: Location, dx: number, dy: number): Expr {
            if(this.grid.periodic) {
                const x = OP.mod(OP.addConstant(at.x, dx), this.width);
                const y = OP.mod(OP.addConstant(at.y, dy), this.height);
                return OP.add(x, OP.mult(y, this.width));
            } else {
                return OP.add(OP.addConstant(at.index, dx), OP.multConstant(this.width, dy));
            }
        }
        
        public checkedIndex(x: Expr, y: Expr): Expr {
            return libMethodCall('Grid', this.grid.periodic ? 'wrapIndex' : 'index', this.obj, [x, y]);
        }
        
        public declareAtIndex(index: Expr, child: (at: Location) => Stmt): Stmt {
            const ir = this.ir;
            return ir.withConst('at', INT_TYPE, index, index =>
                ir.withConst('x', INT_TYPE, OP.mod(index, this.width), x =>
                    ir.withConst('y', INT_TYPE, OP.floordiv(index, this.width), y =>
                        child({index, x, y})
                    ),
                ),
            );
        }
        
        public declareAtXY(x: Expr, y: Expr, child: (at: Location) => Stmt): Stmt {
            const ir = this.ir;
            return ir.withConst('x', INT_TYPE, x, x =>
                ir.withConst('y', INT_TYPE, y, y =>
                    ir.withConst('at', INT_TYPE, this.index(x, y), index =>
                        child({index, x, y})
                    ),
                ),
            );
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
            return yield_(libConstructorCall('RewriteInfo', [this.obj, x, y, w, h]));
        }
    }
}
