///<reference path="../ir/factory.ts"/>
///<reference path="../ir/expr.ts"/>

namespace Compiler {
    const {
        OP,
    } = IR;
    
    export class Stmt_ConvChain extends StmtCompiler<ASG.ConvChainStmt> {
        private readonly colourArray: IR.ConstArray;
        private readonly c2iArray: IR.ConstArray;
        private readonly c2iOffset: number;
        private readonly matchesCount: IR.MutNameExpr;
        private readonly matchesArray: IR.MutableArray | undefined;
        private readonly score: IR.MutNameExpr;
        private readonly temperature: IR.MutNameExpr | undefined;
        private readonly anneal: IR.MutNameExpr | undefined;
        
        private readonly rootDecl: IR.StmtLevelDecl;
        private readonly parentDecl: IR.StmtLevelDecl;
        
        constructor(
            stmt: ASG.ConvChainStmt,
            c: Compiler,
        ) {
            super(stmt);
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
            
            this.colourArray = IR.makeConstArray(c.ir, 'colours', colours, alphabetSize);
            this.c2iArray = IR.makeConstArray(c.ir, 'c2i', colourToIndex, colours.length + 1);
            
            const matchesCount = c.ir.varDecl('convchainCount', IR.INT_TYPE);
            this.matchesCount = matchesCount.name;
            
            const constDecls = [
                this.colourArray.decl,
                this.c2iArray.decl,
                (this.matchesArray?.decl ?? IR.NO_DECL)
            ];
            const varDecls = [
                matchesCount,
            ];
            
            if(stmt.on.kind !== 'top') {
                const matches = this.matchesArray = IR.makeMutableArray(c.ir, 'convchainMatches', g.n, IR.INT32_ARRAY_TYPE.domainSize);
                constDecls.push(matches.decl);
            } else {
                this.matchesArray = undefined;
            }
            
            const logEpsilon = Math.log2(stmt.epsilon);
            this.score = g.makeWeightedCounter(stmt.weights.map(w => ({
                pattern: w.pattern,
                weight: Math.log2(w.weight) - logEpsilon,
            })), true);
            
            if((stmt.temperature !== undefined && stmt.temperature.kind !== 'expr.constant') || stmt.anneal !== undefined) {
                const temperature = c.ir.varDecl('temperature', IR.FLOAT_TYPE);
                this.temperature = temperature.name;
                varDecls.push(temperature);
            } else {
                this.temperature = undefined;
            }
            
            if(stmt.anneal !== undefined && stmt.anneal.kind !== 'expr.constant') {
                const anneal = c.ir.varDecl('anneal', IR.FLOAT_TYPE);
                this.anneal = anneal.name;
                varDecls.push(anneal);
            } else {
                this.anneal = undefined;
            }
            
            this.rootDecl = IR.multiDecl(constDecls);
            this.parentDecl = IR.multiDecl(varDecls);
            
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
        
        declareRoot(ctx: CompilerContext): IR.StmtLevelDecl {
            return this.rootDecl;
        }
        declareParent(ctx: CompilerContext): IR.StmtLevelDecl {
            return this.parentDecl;
        }
        
        compileReset(ctx: CompilerContext): IR.Stmt {
            return IR.assign(this.matchesCount, '=', IR.MINUS_ONE);
        }
        
        compile(ctx: CompilerContext, ifTrue: IR.Stmt, ifFalse: IR.Stmt): StmtCompileResult {
            const {stmt} = this;
            const c = ctx.c;
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
            const temperatureInit = stmt.temperature !== undefined ? ctx.expr(stmt.temperature) : IR.FLOAT_ONE;
            const annealInit = stmt.anneal !== undefined ? ctx.expr(stmt.anneal) : IR.FLOAT_ZERO;
            
            const {colourArray, c2iArray, c2iOffset, matchesArray, matchesCount, score, temperature, anneal} = this;
            // TODO: can this use integer arithmetic?
            const keepWithProbability = temperatureInit === IR.FLOAT_ZERO ? IR.FALSE
                : IR.binaryOp(
                    'float_lt',
                    IR.binaryOp(
                        'float_mult',
                        temperature ?? temperatureInit,
                        // Need `1 - nextDouble()`, to avoid `math.log2(0)` domain error in Python
                        OP.log2(IR.binaryOp('float_minus', IR.FLOAT_ONE, c.prng.nextDouble()))
                    ),
                    score,
                );
            const getRandomColour = colourArray.get(c.prng.nextInt(IR.int(output.length)));
            const getRandomDifferentColour = (oldS: IR.ConstExpr) => colourArray.get(
                OP.modConstant(
                    OP.add(
                        c2iArray.get(OP.minusConstant(oldS, c2iOffset)),
                        c.prng.nextInt(IR.int(output.length - 1))
                    ),
                    output.length,
                )
            );
            const update1x1 = (at: IR.Location) => g.update(at.x, at.y, IR.ONE, IR.ONE);
            
            if(c.config.animate) {
                ifTrue = IR.seq([g.yield_(), ifTrue]);
            }
            
            const flag = c.ir.flag();
            const branchOnFlag = IR.if_(flag.check, ifTrue, ifFalse);
            
            const shouldInit = OP.lt(matchesCount, IR.ZERO);
            const doInit = IR.seq([
                matchesArray !== undefined ? sampler.copyInto(matchesArray) : IR.PASS,
                IR.assign(matchesCount, '=', sampler.count),
                temperature !== undefined ? IR.seq([
                    IR.assign(temperature, '=', temperatureInit),
                    temperatureInit.kind !== 'expr.literal.float' ? IR.if_(IR.binaryOp('float_lt', temperature, IR.FLOAT_ZERO), IR.throw_(`'temperature' must be non-negative`)) : IR.PASS,
                ]): IR.PASS,
                anneal !== undefined ? IR.seq([
                    IR.assign(anneal, '=', annealInit),
                    IR.if_(IR.binaryOp('float_lt', anneal, IR.FLOAT_ZERO), IR.throw_(`'anneal' must be non-negative`)),
                ]): IR.PASS,
                initIsDefinitelyIneffective ? IR.PASS : IR.if_(
                    initSampler === sampler ? OP.gt(matchesCount, IR.ZERO) : initSampler.isNotEmpty,
                    IR.seq([
                        initSampler === sampler
                            ? c.ir.forRange('i', IR.ZERO, matchesCount, i => g.write(this.getFromMatchesArray(i), getRandomColour))
                            : initSampler.forEach(at => g.write(at.index, getRandomColour)),
                        g.update(IR.ZERO, IR.ZERO, g.width, g.height),
                        flag.set,
                    ]),
                ),
            ]);
            const doMain = IR.seq([
                c.ir.forRange('i', IR.ZERO, matchesCount, () =>
                    g.declareAtIndex(this.getFromMatchesArray(c.prng.nextInt(matchesCount)), at =>
                        c.ir.withConst('oldS', IR.BYTE_TYPE, g.data.get(at.index), oldS => IR.seq([
                            IR.assign(score, '=', IR.FLOAT_ZERO),
                            g.write(at.index, getRandomDifferentColour(oldS)),
                            update1x1(at),
                            IR.if_(
                                OP.or(IR.binaryOp('float_gt', score, IR.FLOAT_ZERO), keepWithProbability),
                                flag.set,
                                IR.seq([
                                    g.write(at.index, oldS),
                                    update1x1(at),
                                ]),
                            ),
                        ])),
                    ),
                ),
                temperature !== undefined && annealInit !== IR.FLOAT_ZERO ? IR.assign(temperature, '=', IR.ternary(
                    IR.binaryOp('float_gt', temperature, anneal ?? annealInit),
                    IR.binaryOp('float_minus', temperature, anneal ?? annealInit),
                    IR.FLOAT_ZERO,
                )) : IR.PASS,
            ]);
            
            const r = initIsDefinitelyIneffective ? IR.seq([
                    IR.if_(shouldInit, doInit),
                    IR.withDecl(flag.decl, IR.seq([
                        doMain,
                        branchOnFlag,
                    ])),
                ])
                : IR.withDecl(flag.decl,
                    initIsDefinitelyEffective ? IR.seq([
                        IR.if_(shouldInit, doInit, doMain),
                        branchOnFlag,
                    ])
                    : IR.seq([
                        IR.if_(shouldInit, doInit),
                        IR.if_(OP.not(flag.check), doMain),
                        branchOnFlag,
                    ])
                );
            
            return [r, ctx];
        }
    }
}
