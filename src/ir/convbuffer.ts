namespace IR {
    export class ConvBuffer {
        private readonly buffer: MutableArray2D;
        private readonly width: NameExpr;
        private readonly n: NameExpr;
        private readonly values: ReadonlyMap<PrimitiveKey, Expr>;
        
        public constructor(
            id: number,
            private readonly g: Grid,
            charsets: readonly ISet[],
            private readonly kernel: Convolution.Kernel,
        ) {
            this.width = kernel.width === 1 ? g.width : NAMES.convBufferVar(g, id, 'width');
            const n = this.n = kernel.width === 1 && kernel.height === 1 ? g.n : NAMES.convBufferVar(g, id, 'n');
            
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
            
            const buffer = this.buffer = makeMutableArray2D(
                NAMES.convBufferVar(g, id, 'buffer'),
                int(repMap.size()),
                n,
                kernel.width * kernel.height,
            );
            
            const values = this.values = new Map<PrimitiveKey, Expr>();
            charsets.forEach((chars, i) => {
                const exprs: Expr[] = Array.from(
                    mappedBuffers[i],
                    j => buffer.get(int(j), NAMES.AT_CONV),
                );
                // sanity check
                if(exprs.length === 0) { fail(); }
                values.set(ISet.key(chars), exprs.reduce(OP.add));
            });
        }
        
        public declare(): VarDeclWithInitialiser[] {
            const {g, kernel, n, width, buffer} = this;
            
            const out: VarDeclWithInitialiser[] = []
            if(width !== g.width) {
                const w = OP.addConstant(g.width, kernel.width - 1);
                out.push({name: width, type: INT_TYPE, initialiser: w});
            }
            
            const h = OP.addConstant(g.height, kernel.height - 1);
            out.push(
                {name: n, type: INT_TYPE, initialiser: OP.mult(width, h)},
                ...buffer.decl
            );
            return out;
        }
        
        public get(chars: ISet): Expr {
            return this.values.get(ISet.key(chars)) ?? fail();
        }
        
        public update(i: number, xVar: NameExpr, yVar: NameExpr, op: '+=' | '-='): Stmt {
            const {buffer, width, kernel} = this;
            const out: Stmt[] = [];
            for(let dy = 0; dy < kernel.height; ++dy) {
                for(let dx = 0; dx < kernel.width; ++dx) {
                    const delta = kernel.data[dx + kernel.width * dy];
                    if(delta !== 0) {
                        const index = OP.add(
                            OP.addConstant(xVar, dx),
                            OP.mult(OP.addConstant(yVar, dy), width),
                        );
                        out.push(buffer.set(int(i), index, op, int(delta)));
                    }
                }
            }
            return block(out);
        }
    }
}
