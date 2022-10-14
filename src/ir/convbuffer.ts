///<reference path="./names.ts"/>

namespace IR {
    function _key(p: ASG.ConvPattern): string {
        return `${ISet.key(p.chars)}:${p.includeBoundary}`;
    }
    
    const {
        AT_CONV, AT_X, AT_Y,
    } = NAMES;
    
    export class ConvBuffer {
        private readonly buffer: MutableArray2D;
        public readonly width: NameExpr;
        public readonly height: NameExpr;
        private readonly n: NameExpr;
        private readonly values: ReadonlyMap<string, Expr>;
        private readonly repsWithBoundary: ISet;
        
        public constructor(
            id: number,
            private readonly g: Grid,
            patterns: readonly ASG.ConvPattern[],
            private readonly kernel: Convolution.Kernel,
        ) {
            this.width = kernel.width === 1 ? g.width : NAMES.convBufferVar(g, id, 'width');
            this.height = kernel.height === 1 ? g.height : NAMES.convBufferVar(g, id, 'height');
            const n = this.n = kernel.width === 1 && kernel.height === 1 ? g.n : NAMES.convBufferVar(g, id, 'n');
            
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
                const p = patterns[i], m = mappedReps[i];
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
            
            const buffer = this.buffer = makeMutableArray2D(
                NAMES.convBufferVar(g, id, 'buffer'),
                int(numReps + (anyExtraBoundary ? 1 : 0)),
                n,
                kernel.width * kernel.height,
            );
            
            const boundaryExpr = anyExtraBoundary ? buffer.get(int(numReps), AT_CONV) : ZERO;
            
            const values = this.values = new Map<string, Expr>();
            patterns.forEach((p, i) => {
                const reps = mappedReps[i];
                const exprs: Expr[] = ISet.map(
                    reps,
                    j => buffer.get(int(j), AT_CONV),
                );
                // sanity check
                if(exprs.length === 0) { fail(); }
                
                if(p.includeBoundary && ISet.isDisjoint(reps, repsWithBoundary)) {
                    exprs.push(boundaryExpr);
                }
                
                values.set(_key(p), exprs.reduce(OP.add));
            });
        }
        
        public declare(): Stmt {
            const {g, kernel, width, height, buffer} = this;
            const {centreX, centreY} = kernel;
            
            const decls: VarDeclWithInitialiser[] = [];
            if(width !== g.width) {
                const w = OP.addConstant(g.width, kernel.width - 1);
                decls.push({name: width, type: INT_TYPE, initialiser: w});
            }
            if(height !== g.height) {
                const h = OP.addConstant(g.height, kernel.height - 1);
                decls.push({name: height, type: INT_TYPE, initialiser: h});
            }
            decls.push(
                {name: this.n, type: INT_TYPE, initialiser: OP.mult(width, height)},
                ...buffer.decl,
            );
            
            const out: Stmt[] = [declVars(decls)];
            
            const boundaryValues = kernel.boundaryValues();
            const xLoop: Stmt[] = [];
            const yLoop: Stmt[] = [];
            ISet.forEach(this.repsWithBoundary, j => {
                for(let dy = -centreY; dy <= centreY; ++dy) {
                    const y = dy < 0 ? int(-dy - 1)
                        : dy === 0 ? AT_Y
                        : OP.minusConstant(g.height, dy);
                    for(let dx = -centreX; dx <= centreX; ++dx) {
                        const value = boundaryValues[(centreX + dx) + (centreY + dy) * kernel.width];
                        if(value === 0) { continue; }
                        
                        const x = dx < 0 ? int(-dx - 1)
                            : dx === 0 ? AT_X
                            : OP.minusConstant(g.width, dx);
                        const arr = dx === 0 ? xLoop
                            : dy === 0 ? yLoop
                            : out;
                        arr.push(buffer.set(int(j), this.index(x, y), '=', int(value)));
                    }
                }
            });
            out.push(
                forRange(AT_X, int(centreX), OP.minusConstant(g.width, centreX), xLoop),
                forRange(AT_Y, int(centreY), OP.minusConstant(g.height, centreY), yLoop),
            );
            
            return block(out);
        }
        
        public get(p: ASG.ConvPattern): Expr {
            return this.values.get(_key(p)) ?? fail();
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
        
        public update(i: number, xVar: NameExpr, yVar: NameExpr, op: '+=' | '-='): Stmt {
            const {buffer, kernel} = this;
            const out: Stmt[] = [];
            kernel.forEach((dx, dy, value) => {
                const index = this.indexRaw(
                    OP.addConstant(xVar, dx),
                    OP.addConstant(yVar, dy),
                );
                out.push(buffer.set(int(i), index, op, int(value)));
            });
            return block(out);
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
