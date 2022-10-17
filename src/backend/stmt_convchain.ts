///<reference path="../ir/names.ts"/>
///<reference path="../ir/ops.ts"/>

namespace Compiler {
    const {
        NAMES: {
            AT, AT_X, AT_Y,
            I,
            OLD_S, OLD_T,
            ANY,
        },
        OP,
        PRNG,
    } = IR;
    
    export class stmt_convchain implements StmtCompiler {
        constructor(
            readonly stmt: ASG.ConvChainStmt,
            readonly ifChanged: IR.Stmt,
            readonly then: IR.Stmt,
        ) {}
        
        compile(c: Compiler): IR.Stmt {
            const {stmt} = this;
            const g = c.grids[stmt.inGrid];
            
            const sampler = g.makeSampler([stmt.on]);
            const counter = g.makeWeightedCounter(stmt.weights);
            const output = stmt.output;
            const temperature = stmt.temperature !== undefined ? c.expr(stmt.temperature) : IR.FLOAT_ONE;
            
            // TODO
            const colourArrayName = null!;
            const matchesArrayName = null!;
            const matchesCount = null!;
            const doReset = null!;
            const setFlagToNotDoResetNextTime = null!;
            const keepProbability = null!;
            
            const colourArray = IR.makeConstArray(colourArrayName, output, g.grid.alphabet.key.length);
            const matchesArray = IR.makeMutableArray(matchesArrayName, g.n, IR.INT32_ARRAY_TYPE.domainSize);
            
            const getRandomColour = colourArray.get(PRNG.nextInt(IR.int(output.length)));
            const update1x1 = g.update(AT_X, AT_Y, IR.ONE, IR.ONE);
            
            return IR.if_(
                doReset,
                IR.if_(
                    OP.gt(matchesCount, IR.ZERO),
                    IR.block([
                        sampler.copyInto(matchesArrayName),
                        IR.assign(matchesCount, '=', sampler.count),
                        IR.forRange(I, IR.ZERO, matchesCount, [
                            g.write(matchesArray.get(I), getRandomColour),
                        ]),
                        setFlagToNotDoResetNextTime,
                        this.ifChanged,
                    ]),
                    this.then,
                ),
                IR.block([
                    IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true),
                    IR.forRange(I, IR.ZERO, matchesCount, [
                        g.declareAtIndex(matchesArray.get(PRNG.nextInt(matchesCount))),
                        IR.declVar(OLD_S, IR.BYTE_TYPE, g.data.get(AT)),
                        IR.declVar(OLD_T, IR.INT_TYPE, counter),
                        g.write(AT, getRandomColour),
                        update1x1,
                        IR.if_(
                            OP.and(OP.lt(counter, OLD_T), OP.ge(PRNG.NEXT_DOUBLE, keepProbability)),
                            IR.block([
                                g.write(AT, OLD_S),
                                update1x1,
                                IR.assign(ANY, '=', IR.TRUE),
                            ]),
                        ),
                    ]),
                    IR.if_(
                        ANY,
                        IR.block([c.config.animate ? g.yield_() : IR.PASS, this.ifChanged]),
                        this.then,
                    ),
                ]),
            );
        }
    }
}
