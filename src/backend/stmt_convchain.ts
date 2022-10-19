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
        private readonly matchesArray: IR.MutableArray;
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
            
            this.matchesArray = IR.makeMutableArray(IR.NAMES.otherVar(stmtID, 'matches'), g.n, IR.INT32_ARRAY_TYPE.domainSize);
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
        
        declare(): IR.Stmt {
            return IR.block([
                IR.declVars([
                    ...this.colourArray.decl,
                    ...this.c2iArray.decl,
                    ...this.matchesArray.decl,
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
            
            const output = stmt.output;
            if(output.some(x => ISet.has(stmt.on.masks[0], x))) {
                // TODO
                c.notSupported(`'convchain' with overlapping 'on' and output`, stmt.pos);
            }
            
            const sampler = g.makeSampler([stmt.on]);
            const temperatureInit = stmt.temperature !== undefined ? c.expr(stmt.temperature) : IR.FLOAT_ONE;
            const annealInit = stmt.anneal !== undefined ? c.expr(stmt.anneal) : IR.FLOAT_ZERO;
            
            const {colourArray, c2iArray, c2iOffset, matchesArray, matchesCount, score, temperature, anneal} = this;
            // TODO
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
            
            const shouldInit = OP.lt(matchesCount, IR.ZERO);
            const doInit = IR.block([
                IR.assign(matchesCount, '=', sampler.count),
                temperature !== undefined ? IR.block([
                    IR.assign(temperature, '=', temperatureInit),
                    temperatureInit.kind !== 'expr.literal.float' ? IR.if_(IR.binaryOp('float_lt', temperature, IR.FLOAT_ZERO), IR.throw_(`'temperature' must be non-negative`)) : IR.PASS,
                ]): IR.PASS,
                anneal !== undefined ? IR.block([
                    IR.assign(anneal, '=', annealInit),
                    IR.if_(IR.binaryOp('float_lt', anneal, IR.FLOAT_ZERO), IR.throw_(`'anneal' must be non-negative`)),
                ]): IR.PASS,
                IR.if_(
                    OP.gt(matchesCount, IR.ZERO),
                    IR.block([
                        sampler.copyInto(matchesArray.name),
                        IR.forRange(I, IR.ZERO, matchesCount, [
                            // TODO: if `on` and `output` overlap, only randomise if existing colour is not in `output`
                            g.write(matchesArray.get(I), getRandomColour),
                        ]),
                        ifChanged,
                    ]),
                    then,
                ),
            ]);
            const doMain = IR.block([
                IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true),
                IR.forRange(I, IR.ZERO, matchesCount, [
                    g.declareAtIndex(matchesArray.get(PRNG.nextInt(matchesCount))),
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
                IR.if_(ANY, ifChanged, then),
            ]);
            
            return IR.if_(shouldInit, doInit, doMain);
        }
    }
}
