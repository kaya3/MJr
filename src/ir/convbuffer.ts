namespace IR {
    export class ConvBuffer {
        private readonly name: NameExpr;
        private readonly width: Expr;
        private readonly height: Expr;
        private readonly n: NameExpr;
        private readonly k: number;
        private readonly values: ReadonlyMap<PrimitiveKey, Expr>;
        
        public constructor(
            id: number,
            g: Grid,
            charsets: readonly ISet[],
            private readonly kernel: Convolution.Kernel,
        ) {
            this.width = OP.add(g.width, int(kernel.width - 1));
            this.height = OP.add(g.height, int(kernel.height - 1));
            const n = this.n = NAMES.convBufferN(g, id);
            const name = this.name = NAMES.convBufferArray(g, id);
            
            // partition the alphabet, so that each alphabet symbol contributes to at most one gBuffer
            const alphabetKey = g.grid.alphabet.key;
            const alphabetPartition = new Partition(alphabetKey.length);
            charsets.forEach(set => alphabetPartition.refine(set));
            
            const repMap = IDMap.withKey<number>(i => alphabetPartition.getRepresentative(i));
            const mappedBuffers: readonly ReadonlySet<number>[] = charsets.map(chars => {
                const out = new Set<number>();
                ISet.forEach(chars, i => out.add(repMap.getOrCreateID(i)));
                return out;
            });
            repMap.forEach((rep, i) => {
                const chars = alphabetPartition.getSet(rep);
                const pattern = new Pattern(1, 1, alphabetKey, [-2], [chars], true);
                g.matcher.addMatchHandler({kind: 'convolution', buffer: this, pattern, i});
            });
            this.k = repMap.size();
            
            const values = this.values = new Map<PrimitiveKey, Expr>();
            charsets.forEach((chars, i) => {
                const exprs: Expr[] = Array.from(mappedBuffers[i], j => {
                    const index = OP.add(OP.multConstant(n, j), NAMES.AT_CONV);
                    return access(name, index);
                });
                // sanity check
                if(exprs.length === 0) { fail(); }
                values.set(ISet.key(chars), exprs.reduce(OP.add));
            });
        }
        
        public declare(): VarDeclWithInitialiser[] {
            const {n, name, k, width, height} = this;
            return [
                {name: n, type: INT_TYPE, initialiser: OP.mult(width, height)},
                {name, type: GRID_DATA_ARRAY_TYPE, initialiser: newGridDataArray(OP.multConstant(n, k))},
            ];
        }
        
        public get(chars: ISet): Expr {
            return this.values.get(ISet.key(chars)) ?? fail();
        }
        
        public update(i: number, xVar: NameExpr, yVar: NameExpr, op: '+=' | '-='): Stmt {
            const {name, width, n, kernel} = this;
            const out: Stmt[] = [];
            for(let dy = 0; dy < kernel.height; ++dy) {
                for(let dx = 0; dx < kernel.width; ++dx) {
                    const delta = kernel.data[dx + kernel.width * dy];
                    if(delta !== 0) {
                        const index = OP.add(
                            OP.multConstant(n, i),
                            OP.add(
                                OP.add(xVar, int(dx)),
                                OP.mult(OP.add(yVar, int(dy)), width),
                            ),
                        );
                        out.push(assign(access(name, index), op, int(delta)));
                    }
                }
            }
            return block(out);
        }
    }
}
