///<reference path="./factory.ts"/>

namespace IR {
    function _key(p: ASG.ConvPattern): string {
        return `${ISet.key(p.chars)}:${p.includeBoundary ? 1 : 0}`;
    }
    
    export class ConvBuffer {
        private readonly buffer: MutableArray2D;
        public readonly width: ConstNameExpr;
        public readonly height: ConstNameExpr;
        private readonly n: ConstNameExpr;
        private readonly patternIndices: ReadonlyMap<string, readonly number[]>;
        private readonly repsWithBoundary: ISet;
        
        private readonly constDecls: readonly ConstDecl[];
        
        public constructor(
            private readonly ir: Factory,
            private readonly g: Grid,
            patterns: readonly ASG.ConvPattern[],
            private readonly kernel: Convolution.Kernel,
        ) {
            const constDecls: ConstDecl[] = this.constDecls = [];
            
            if(kernel.width === 1) {
                this.width = g.width;
            } else {
                const width = ir.constDecl('convBufferWidth', IR.INT_TYPE, OP.addConstant(g.width, kernel.width - 1));
                constDecls.push(width);
                this.width = width.name;
            }
            if(kernel.height === 1) {
                this.height = g.height;
            } else {
                const height = ir.constDecl('convBufferHeight', IR.INT_TYPE, OP.addConstant(g.height, kernel.height - 1));
                constDecls.push(height);
                this.height = height.name;
            }
            
            if(kernel.width === 1 && kernel.height === 1) {
                this.n = g.n;
            } else {
                const n = ir.constDecl('convBufferN', IR.INT_TYPE, OP.mult(this.width, this.height));
                constDecls.push(n);
                this.n = n.name;
            }
            
            // partition the alphabet, so that each alphabet symbol contributes to at most one gBuffer
            const alphabetKey = g.grid.alphabet.key;
            const alphabetPartition = new Partition(alphabetKey.length);
            patterns.forEach(p => alphabetPartition.refine(p.chars));
            
            const repMap = IDMap.withKey<number>(i => alphabetPartition.getRepresentative(i));
            const mappedReps = makeArray(patterns.length, () => ISet.empty(alphabetPartition.countSubsets()));
            for(let i = 0; i < patterns.length; ++i) {
                ISet.forEach(patterns[i].chars, c => {
                    ISet.add(mappedReps[i], repMap.getOrCreateID(c));
                });
            }
            const numReps = repMap.size();
            
            repMap.forEach((rep, i) => {
                const chars = alphabetPartition.getSet(rep);
                const pattern = new Pattern(1, 1, alphabetKey, [-2], [chars], true);
                g.matcher.addMatchHandler({kind: 'convolution', buffer: this, pattern, i});
            });
            
            // find a minimal subset of reps such that:
            // - no rep in the subset occurs in a pattern `p` for which `p.includesBoundary` is false, and
            // - for each `p` where `p.includeBoundary` is true, `p.chars` contains at most one rep from the subset;
            // - the number of patterns `p` for which `p.includeBoundary` is true and `p.chars` contains a rep is maximised
            // then add those reps to `repsWithBoundary`.
            const allowedReps = ISet.full(alphabetPartition.countSubsets());
            const toCover: ISet[] = [];
            for(let i = 0; i < patterns.length; ++i) {
                const p = patterns[i],
                    m = mappedReps[i];
                if(p.includeBoundary) {
                    toCover.push(m);
                } else {
                    ISet.removeAll(allowedReps, m);
                }
            }
            const repsWithBoundary = this.repsWithBoundary = _findCover(ISet.empty(numReps + 1), allowedReps, toCover);
            
            const anyExtraBoundary = patterns.some((p, i) => p.includeBoundary && ISet.isDisjoint(repsWithBoundary, mappedReps[i]));
            if(anyExtraBoundary) {
                ISet.add(repsWithBoundary, numReps);
            }
            
            this.buffer = makeMutableArray2D(
                ir,
                'convBuffer',
                int(numReps + (anyExtraBoundary ? 1 : 0)),
                this.n,
                kernel.width * kernel.height,
            );
            
            const patternIndices = this.patternIndices = new Map<string, readonly number[]>();
            patterns.forEach((p, i) => {
                const reps = mappedReps[i];
                const indices: number[] = ISet.toArray(reps);
                // sanity check
                if(indices.length === 0) { fail(); }
                
                if(p.includeBoundary && ISet.isDisjoint(reps, repsWithBoundary)) {
                    indices.push(numReps);
                }
                
                patternIndices.set(_key(p), indices);
            });
        }
        
        public declare(): StmtLevelDecl {
            const {ir, g, kernel, buffer} = this;
            const {centreX, centreY} = kernel;
            
            const atX = ir.loopVarDecl('x'), atY = ir.loopVarDecl('y');
            
            const boundaryValues = kernel.boundaryValues();
            const xLoop: Stmt[] = [],
                yLoop: Stmt[] = [],
                noLoop: Stmt[] = [];
            ISet.forEach(this.repsWithBoundary, j => {
                for(let dy = -centreY; dy <= centreY; ++dy) {
                    const y = dy < 0 ? int(-dy - 1)
                        : dy === 0 ? atY.name
                        : OP.minusConstant(g.height, dy);
                    for(let dx = -centreX; dx <= centreX; ++dx) {
                        const value = boundaryValues[(centreX + dx) + (centreY + dy) * kernel.width];
                        if(value === 0) { continue; }
                        
                        const x = dx < 0 ? int(-dx - 1)
                            : dx === 0 ? atX.name
                            : OP.minusConstant(g.width, dx);
                        const arr = dx === 0 ? xLoop
                            : dy === 0 ? yLoop
                            : noLoop;
                        arr.push(buffer.set(int(j), this.index(x, y), '=', int(value)));
                    }
                }
            });
            
            return initDecl(
                multiDecl([...this.constDecls, buffer.decl]),
                seq([
                    ...noLoop,
                    forRange(atX, int(centreX), OP.minusConstant(g.width, centreX), IR.seq(xLoop)),
                    forRange(atY, int(centreY), OP.minusConstant(g.height, centreY), IR.seq(yLoop)),
                ]),
            );
        }
        
        public get(p: ASG.ConvPattern, at: Expr): Expr {
            return (this.patternIndices.get(_key(p)) ?? fail())
                .map(j => this.buffer.get(int(j), at))
                .reduce(OP.add);
        }
        
        public indexRaw(x: Expr, y: Expr): Expr {
            return OP.add(x, OP.mult(y, this.width));
        }
        
        public index(x: Expr, y: Expr): Expr {
            const {centreX, centreY} = this.kernel;
            return this.indexRaw(
                OP.addConstant(x, centreX),
                OP.addConstant(y, centreY),
            );
        }
        
        public update(i: number, at: Location, op: '+=' | '-='): Stmt {
            // x, y must be simple constants since they are repeated
            const {buffer, kernel} = this;
            const out: Stmt[] = [];
            const iExpr = int(i);
            kernel.forEach((dx, dy, value) => {
                const index = this.indexRaw(
                    OP.addConstant(at.x, dx),
                    OP.addConstant(at.y, dy),
                );
                out.push(buffer.set(iExpr, index, op, int(value)));
            });
            return seq(out);
        }
    }
    
    interface CoverSearchState extends Readonly<{
        allowedReps: ISet,
        toCover: readonly ISet[],
        covered: number,
        countUsed: number,
        repsUsed: MutableISet,
    }> {}
    
    function _findCover(repsUsed: MutableISet, allowedReps: ISet, toCover: readonly ISet[]): MutableISet {
        // fast path for convolutions without boundary
        if(toCover.length === 0) { return repsUsed; }
        
        const initialState: CoverSearchState = {
            allowedReps,
            toCover,
            covered: 0,
            countUsed: 0,
            repsUsed: repsUsed,
        };
        
        // depth-first search loop
        let best = initialState;
        const stack = [initialState];
        while(stack.length > 0) {
            const cur = stack.pop()!;
            if(cur.covered > best.covered || (cur.covered === best.covered && cur.countUsed < best.countUsed)) {
                best = cur;
                // break if this set can't be improved on; fast path for common case where only one `p.includesBoundary` is true
                if(best.covered === toCover.length && best.countUsed === 1) { break; }
            }
            
            const filteredToCover = cur.toCover.filter(s => !ISet.isDisjoint(cur.allowedReps, s));
            const upperBound = cur.covered + filteredToCover.length;
            if(filteredToCover.length === 0 || upperBound < best.covered || (upperBound === best.covered && cur.countUsed >= best.countUsed)) { continue; }
            
            const x = ISet.first(ISet.intersection(filteredToCover[0], cur.allowedReps));
            if(x < 0) { fail(); }
            
            // add child state where x is not used
            const xNotAllowed = ISet.copy(cur.allowedReps);
            ISet.remove(xNotAllowed, x);
            stack.push({
                allowedReps: xNotAllowed,
                toCover: filteredToCover.filter(s => !ISet.isDisjoint(xNotAllowed, s)),
                covered: cur.covered,
                countUsed: cur.countUsed,
                repsUsed: cur.repsUsed,
            });
            
            // add child state where x is used
            const newRepsUsed = ISet.copy(cur.repsUsed);
            ISet.add(newRepsUsed, x);
            
            const newAllowedReps = ISet.copy(cur.allowedReps);
            let newCovered = cur.covered;
            const newToCover = filteredToCover.filter(s => {
                if(ISet.has(s, x)) {
                    ISet.removeAll(newAllowedReps, s);
                    ++newCovered;
                    return false;
                } else {
                    return true;
                }
            });
            
            stack.push({
                allowedReps: newAllowedReps,
                toCover: newToCover,
                covered: newCovered,
                countUsed: cur.countUsed + 1,
                repsUsed: newRepsUsed,
            });
        }
        
        return best.repsUsed;
    }
}
