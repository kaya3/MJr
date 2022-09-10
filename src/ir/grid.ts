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
            this.data = NAMES.gridVar(this, 'data');
            
            this.originX = scaleX % 2 === 0 ? OP.multConstant(WIDTH, scaleX >> 1) : OP.divConstant(this.width, 2);
            this.originY = scaleY % 2 === 0 ? OP.multConstant(HEIGHT, scaleY >> 1) : OP.divConstant(this.height, 2);
            
            // TODO: multiple matchers per grid?
            this.matcher = new Matcher(this, 0);
        }

        public getScale(): number {
            return this.grid.scaleX * this.grid.scaleY * this.scale;
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
        
        public makeSampler(patterns: readonly Pattern[]): AbstractSampler {
            const {samplers, matcher} = this;
            
            const key = patterns.map(Pattern.key).join('\n');
            const cached = samplers.get(key);
            if(cached !== undefined) { return cached; }
            
            if(patterns.length === 1 && patterns[0].masks.every(mask => ISet.size(mask) === this.grid.alphabet.key.length)) {
                const {width, height} = patterns[0];
                const sampler = new TrivialSampler(this, width, height);
                samplers.set(key, sampler);
                return sampler;
            }
            
            const sampler = new Sampler(samplers.size, this, patterns.length);
            samplers.set(key, sampler);
            
            for(let i = 0; i < patterns.length; ++i) {
                const pattern = patterns[i];
                matcher.addMatchHandler({kind: 'sampler', pattern, sampler, i});
            }
            this.scale = Math.max(this.scale, patterns.length);
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
            this.scale = Math.max(this.scale, charsets.length);
            return buffer;
        }
        
        public useOrigin(): NameExpr {
            return this.origin ??= NAMES.gridVar(this, 'origin');
        }
        
        public useObj(): NameExpr {
            return this.obj ??= NAMES.gridVar(this, 'obj');
        }
        
        public useLsfrFeedbackTerm(): NameExpr {
            return this.lfsrFeedbackTerm ??= NAMES.gridVar(this, 'lfsrFeedbackTerm');
        }
        
        public declareVars(): Stmt[] {
            const {width, height, n, data, obj, origin, lfsrFeedbackTerm, grid} = this;
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
                consts.push({name: origin, type: INT_TYPE, initialiser: OP.add(this.originX, OP.mult(this.originY, width))});
            }
            if(lfsrFeedbackTerm !== undefined) {
                consts.push({name: lfsrFeedbackTerm, type: INT_TYPE, initialiser: libFunctionCall('lfsrFeedbackTerm', [n])});
            }
            
            for(const buffer of this.convBuffers.values()) {
                consts.push(...buffer.declare());
            }
           
            vars.push(...Array.from(this.counters.values(), counter => ({
                name: counter,
                type: INT_TYPE,
                initialiser: ZERO,
            })));
            
            return [
                declVars(consts),
                declVars(vars, true),
                ...Array.from(this.samplers.values(), sampler => sampler.declare()),
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
