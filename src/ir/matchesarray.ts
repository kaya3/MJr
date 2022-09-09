///<reference path="names.ts"/>

namespace IR {
    const {
        MATCH, MATCHES, MATCH_COUNT,
        WIDTH, HEIGHT, RNG,
        J,
    } = NAMES;
    
    export class MatchesArray {
        private scale: number = 0;
        
        readonly array = MATCHES;
        readonly count = MATCH_COUNT;
        readonly getAtCount = this.get(this.count);
        readonly isNotEmpty = OP.gt(this.count, ZERO);
        readonly incrementCount = assign(this.count, '+=', ONE);
        readonly decrementCount = assign(this.count, '-=', ONE);
        
        use(g: Grid, k: number): void {
            // TODO: in principle, we can get a better estimate for the maximum number of matches if we know some patterns cannot overlap
            this.scale = Math.max(this.scale, g.grid.scaleX * g.grid.scaleY * k);
        }
        declare(): Stmt {
            if(this.scale === 0) { return PASS; }
            
            const n = OP.multConstant(OP.mult(WIDTH, HEIGHT), this.scale);
            return declVar(this.array, INT32_ARRAY_TYPE, newInt32Array(n));
        }
        declareCount(initial: Expr, mutable: boolean): Stmt {
            return declVar(this.count, INT_TYPE, initial, mutable);
        }
        copyFrom(sampler: NameExpr, shuffle: boolean): Stmt {
            return shuffle
                ? libMethodCallStmt('Sampler', 'shuffleInto', sampler, [this.array, RNG])
                : libMethodCallStmt('Sampler', 'copyInto', sampler, [this.array]);
        }
        get(index: Expr): ArrayAccessExpr {
            return access(this.array, index);
        }
        add(match: Expr, shuffle: boolean): Stmt {
            return block(shuffle ? [
                declVar(J, INT_TYPE, libMethodCall('PRNG', 'nextInt', RNG, [OP.add(this.count, ONE)])),
                assign(this.getAtCount, '=', this.get(J)),
                assign(this.get(J), '=', match),
                this.incrementCount,
            ] : [
                assign(this.getAtCount, '=', match),
                this.incrementCount,
            ]);
        }
        forEach(indexVar: NameExpr, then: readonly Stmt[]): Stmt {
            return forRange(indexVar, ZERO, this.count, block([
                declVar(MATCH, INT_TYPE, this.get(indexVar)),
                ...then,
            ]));
        }
    }
}
