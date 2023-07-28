///<reference path="../ir/names.ts"/>
///<reference path="../ir/ops.ts"/>

namespace Compiler {
    const {
        NAMES: {
            AT, AT_X, AT_Y,
            I,
            OLD_S,
            ANY,
        },
        OP,
        PRNG,
    } = IR;
    
    export class Stmt_ConvChain implements StmtCompiler {
        private readonly colourArray: IR.ConstArray;
        private readonly c2iArray: IR.ConstArray;
        private readonly c2iOffset: number;
        private readonly matchesArray: IR.MutableArray | undefined;
        private readonly matchesCount: IR.NameExpr;
        private readonly score: IR.NameExpr;
        private readonly temperature: IR.NameExpr | undefined;
        private readonly anneal: IR.NameExpr | undefined;
        
        constructor(
            readonly stmt: ASG.ConvChainStmt,
            stmtID: number,
            c: Compiler,
        ) {
            const g = c.grids[stmt.inGrid];
            const alphabetSize = g.grid.alphabet.key.length;
            const colours = stmt.output;
            
            this.c2iOffset = colours[0];
            const colourRange = colours[colours.length - 1] + 1 - colours[0];
            const colourToIndex = emptyArray(colourRange, 0);
            for(let i = 0; i < colours.length; ++i) {
                const d = colours[i] - colours[0];
                colourToIndex[d] = i + 1;
            }
            
            this.colourArray = IR.makeConstArray(IR.NAMES.otherVar(stmtID, 'colours'), colours, alphabetSize);
            this.c2iArray = IR.makeConstArray(IR.NAMES.otherVar(stmtID, 'c2i'), colourToIndex, colours.length + 1);
            
            this.matchesArray = stmt.on.kind === 'top' ? undefined : IR.makeMutableArray(IR.NAMES.otherVar(stmtID, 'matches'), g.n, IR.INT32_ARRAY_TYPE.domainSize);
            this.matchesCount = IR.NAMES.otherVar(stmtID, 'count');
            
            const logEpsilon = Math.log2(stmt.epsilon);
            this.score = g.makeWeightedCounter(stmt.weights.map(w => ({
                pattern: w.pattern,
                weight: Math.log2(w.weight) - logEpsilon,
            })), true);
            
            this.temperature = (stmt.temperature !== undefined && stmt.temperature.kind !== 'expr.constant') || stmt.anneal !== undefined ? IR.NAMES.otherVar(stmtID, 'temperature') : undefined;
            this.anneal = stmt.anneal !== undefined && stmt.anneal.kind !== 'expr.constant' ? IR.NAMES.otherVar(stmtID, 'anneal') : undefined;
            
            for(const op of [
                'float_plus', 'float_minus', 'float_mult', 'float_log2',
                'float_eq', 'float_ne', 'float_lt', 'float_le', 'float_gt', 'float_ge',
            ] as const) {
                c.opsUsed.add(op);
            }
        }
        
        private getFromMatchesArray(index: IR.Expr): IR.Expr {
            const {matchesArray} = this;
            return matchesArray !== undefined ? matchesArray.get(index) : index;
        }
        
        declare(): IR.Stmt {
            return IR.block([
                IR.declVars([
                    ...this.colourArray.decl,
                    ...this.c2iArray.decl,
                    ...this.matchesArray?.decl ?? [],
                ]),
                IR.declVar(this.matchesCount, IR.INT_TYPE, IR.MINUS_ONE, true),
                this.temperature !== undefined ? IR.declVar(this.temperature, IR.FLOAT_TYPE, undefined, true) : IR.PASS,
                this.anneal !== undefined ? IR.declVar(this.anneal, IR.FLOAT_TYPE, undefined, true) : IR.PASS,
            ]);
        }
        
        compileReset(c: Compiler): IR.Stmt {
            return IR.assign(this.matchesCount, '=', IR.MINUS_ONE);
        }
        
        compile(c: Compiler, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
            const {stmt} = this;
            const g = c.grids[stmt.inGrid];
            const alphabetKey = g.grid.alphabet.key;
            
            const on = stmt.on.masks[0];
            const output = stmt.output;
            const outputMask = ISet.of(alphabetKey.length, output);
            const initIsDefinitelyEffective = ISet.isDisjoint(on, outputMask);
            const initIsDefinitelyIneffective = ISet.isSubset(on, outputMask);
            
            const sampler = g.makeSampler([stmt.on]);
            const initSampler = initIsDefinitelyIneffective ? new IR.EmptySampler() : g.makeSampler([
                new Pattern(1, 1, alphabetKey, [-2], [ISet.difference(on, outputMask)], true),
            ]);
            const temperatureInit = stmt.temperature !== undefined ? c.expr(stmt.temperature) : IR.FLOAT_ONE;
            const annealInit = stmt.anneal !== undefined ? c.expr(stmt.anneal) : IR.FLOAT_ZERO;
            
            const {colourArray, c2iArray, c2iOffset, matchesArray, matchesCount, score, temperature, anneal} = this;
            // TODO: can this use integer arithmetic?
            const keepWithProbability = temperatureInit === IR.FLOAT_ZERO ? IR.FALSE
                : IR.binaryOp(
                    'float_lt',
                    IR.binaryOp(
                        'float_mult',
                        temperature ?? temperatureInit,
                        OP.log2(IR.binaryOp('float_minus', IR.FLOAT_ONE, PRNG.NEXT_DOUBLE))
                    ),
                    score,
                );
            const getRandomColour = colourArray.get(PRNG.nextInt(IR.int(output.length)));
            const getRandomDifferentColour = colourArray.get(
                OP.modConstant(
                    OP.add(
                        c2iArray.get(OP.minusConstant(OLD_S, c2iOffset)),
                        PRNG.nextInt(IR.int(output.length - 1))
                    ),
                    output.length,
                )
            );
            const update1x1 = g.update(AT_X, AT_Y, IR.ONE, IR.ONE);
            
            if(c.config.animate) {
                ifChanged = IR.block([g.yield_(), ifChanged]);
            }
            
            const declareFlag = IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true);
            const setFlag = IR.assign(ANY, '=', IR.TRUE);
            const branchOnFlag = IR.if_(ANY, ifChanged, then);
            
            const shouldInit = OP.lt(matchesCount, IR.ZERO);
            const doInit = IR.block([
                matchesArray !== undefined ? sampler.copyInto(matchesArray.name) : IR.PASS,
                IR.assign(matchesCount, '=', sampler.count),
                temperature !== undefined ? IR.block([
                    IR.assign(temperature, '=', temperatureInit),
                    temperatureInit.kind !== 'expr.literal.float' ? IR.if_(IR.binaryOp('float_lt', temperature, IR.FLOAT_ZERO), IR.throw_(`'temperature' must be non-negative`)) : IR.PASS,
                ]): IR.PASS,
                anneal !== undefined ? IR.block([
                    IR.assign(anneal, '=', annealInit),
                    IR.if_(IR.binaryOp('float_lt', anneal, IR.FLOAT_ZERO), IR.throw_(`'anneal' must be non-negative`)),
                ]): IR.PASS,
                initIsDefinitelyIneffective ? IR.PASS : IR.if_(
                    initSampler === sampler ? OP.gt(matchesCount, IR.ZERO) : initSampler.isNotEmpty,
                    IR.block([
                        initSampler === sampler
                            ? IR.forRange(I, IR.ZERO, matchesCount, [g.write(this.getFromMatchesArray(I), getRandomColour)])
                            : initSampler.forEach([g.write(AT, getRandomColour)]),
                        g.update(IR.ZERO, IR.ZERO, g.width, g.height),
                        setFlag,
                    ]),
                ),
            ]);
            const doMain = IR.block([
                IR.forRange(I, IR.ZERO, matchesCount, [
                    g.declareAtIndex(this.getFromMatchesArray(PRNG.nextInt(matchesCount))),
                    IR.declVar(OLD_S, IR.BYTE_TYPE, g.data.get(AT)),
                    IR.assign(score, '=', IR.FLOAT_ZERO),
                    g.write(AT, getRandomDifferentColour),
                    update1x1,
                    IR.if_(
                        OP.or(IR.binaryOp('float_ge', score, IR.FLOAT_ZERO), keepWithProbability),
                        IR.assign(ANY, '=', IR.TRUE),
                        IR.block([
                            g.write(AT, OLD_S),
                            update1x1,
                        ]),
                    ),
                ]),
                temperature !== undefined && annealInit !== IR.FLOAT_ZERO ? IR.assign(temperature, '=', IR.ternary(
                    IR.binaryOp('float_gt', temperature, anneal ?? annealInit),
                    IR.binaryOp('float_minus', temperature, anneal ?? annealInit),
                    IR.FLOAT_ZERO,
                )) : IR.PASS,
            ]);
            
            return initIsDefinitelyEffective ? IR.block([
                    declareFlag,
                    IR.if_(shouldInit, doInit, doMain),
                    branchOnFlag,
                ])
                : initIsDefinitelyIneffective ? IR.block([
                    IR.if_(shouldInit, doInit),
                    declareFlag,
                    doMain,
                    branchOnFlag,
                ])
                : IR.block([
                    declareFlag,
                    IR.if_(shouldInit, doInit),
                    IR.if_(OP.not(ANY), doMain),
                    branchOnFlag,
                ]);
        }
    }
}
