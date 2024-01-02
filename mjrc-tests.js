"use strict";
const Assert = {
    equals(expected) {
        expected = expected.trim();
        return { kind: 'equals', expected };
    },
    count(colour, expected) {
        return { kind: 'count', colour, expected };
    },
};
function dedent(src) {
    const lines = src.split('\n');
    const firstLine = lines.find(line => line.trim().length > 0) ?? fail();
    const initialIndent = firstLine.split(/[^\s]/, 2)[0];
    return lines.map(line => line.replace(initialIndent, ''))
        .join('\n')
        .trim();
}
const Benchmark = {
    _all: new Map(),
    _baselines: new Map(),
    add(benchmarks) {
        for (const [name, src] of Object.entries(benchmarks)) {
            if (Benchmark._all.has(name)) {
                throw new Error(`Benchmark '${name}' already exists`);
            }
            Benchmark._all.set(name, dedent(src));
        }
    },
    setBaselines(baselines) {
        for (const [name, baseline] of Object.entries(baselines)) {
            Benchmark._baselines.set(name, baseline);
        }
    },
    _time(f) {
        const MAX_TOTAL_TIME = 2000;
        const MAX_ITERATIONS = 2000;
        const startTime = performance.now();
        const maxEndTime = startTime + MAX_TOTAL_TIME;
        let i = 0, endTime;
        do {
            f();
            ++i;
        } while ((endTime = performance.now()) < maxEndTime && i < MAX_ITERATIONS);
        return (endTime - startTime) / i;
    },
    _timeBestOfThree(f) {
        return Math.min(Benchmark._time(f), Benchmark._time(f), Benchmark._time(f));
    },
    _run(src, gridSize) {
        const [result, f] = Test._compile(src, { indentSpaces: 0, emitComments: false, maxIterations: 100000 });
        const rng = new Test.PRNG();
        return {
            codeSize: result.length,
            compileTime: Benchmark._timeBestOfThree(() => Compiler.compile(src, 'JavaScript')),
            runTime: Benchmark._timeBestOfThree(() => f(gridSize, gridSize, rng)),
        };
    },
    runAll() {
        const SMALL_CHANGE_PERCENT = 5;
        const BENCHMARK_GRID_SIZE = 127;
        const benchmarkPanelElem = document.getElementById('benchmark_panel') ?? fail();
        const titleElem = document.createElement('h2');
        titleElem.appendChild(document.createTextNode('Benchmarks'));
        benchmarkPanelElem.appendChild(titleElem);
        const tableElem = document.createElement('table');
        const tableHeaderElem = document.createElement('tr');
        tableElem.appendChild(tableHeaderElem);
        for (const heading of ['Model', 'Compile time', 'Output size', 'Run time']) {
            const headingElem = document.createElement('th');
            headingElem.innerText = heading;
            tableHeaderElem.append(headingElem);
        }
        benchmarkPanelElem.appendChild(tableElem);
        const allResults = {};
        let improvements = 0, regressions = 0;
        function _makeTDElem(f, toStr, baseline, result) {
            const tdElem = document.createElement('td');
            const changeElem = document.createElement('span');
            const after = f(result);
            if (baseline === undefined) {
                changeElem.innerText = 'no baseline';
            }
            else {
                const before = f(baseline);
                const changePercent = ((after / before) - 1) * 100;
                changeElem.innerText = `${changePercent >= 0 ? '+' : ''}${changePercent.toFixed(1)}%`;
                if (changePercent > SMALL_CHANGE_PERCENT) {
                    ++regressions;
                    changeElem.className = 'fail';
                }
                else if (changePercent < -SMALL_CHANGE_PERCENT) {
                    ++improvements;
                    changeElem.className = 'pass';
                }
            }
            tdElem.appendChild(document.createTextNode(toStr(after) + ' ('));
            tdElem.appendChild(changeElem);
            tdElem.appendChild(document.createTextNode(')'));
            return tdElem;
        }
        const cases = Array.from(Benchmark._all.entries());
        function runCase(i) {
            if (i >= cases.length) {
                const improvementsElem = document.createElement('span');
                const regressionsElem = document.createElement('span');
                improvementsElem.innerText = `${improvements} improvement${improvements === 1 ? '' : 's'}`;
                if (improvements > 0) {
                    improvementsElem.className = 'pass';
                }
                regressionsElem.innerText = `${regressions} regression${regressions === 1 ? '' : 's'}`;
                regressionsElem.className = regressions === 0 ? 'pass' : 'fail';
                titleElem.appendChild(document.createTextNode(' ('));
                titleElem.appendChild(improvementsElem);
                titleElem.appendChild(document.createTextNode(', '));
                titleElem.appendChild(regressionsElem);
                titleElem.appendChild(document.createTextNode(')'));
                console.log('Benchmark.setBaselines(' + JSON.stringify(allResults, undefined, 4) + ');');
                return;
            }
            setTimeout(() => {
                const [name, src] = cases[i];
                const baseline = Benchmark._baselines.get(name);
                const result = Benchmark._run(src, BENCHMARK_GRID_SIZE);
                allResults[name] = result;
                const rowElem = document.createElement('tr');
                const nameElem = document.createElement('td');
                nameElem.innerText = name;
                rowElem.appendChild(nameElem);
                rowElem.appendChild(_makeTDElem(r => r.compileTime, x => `${x.toFixed(1)} ms`, baseline, result));
                rowElem.appendChild(_makeTDElem(r => r.codeSize, x => `${(x / 1024).toFixed(1)} KiB`, baseline, result));
                rowElem.appendChild(_makeTDElem(r => r.runTime, x => `${x.toFixed(1)} ms`, baseline, result));
                tableElem.appendChild(rowElem);
                runCase(i + 1);
            }, 50);
        }
        runCase(0);
    }
};
function Test(src, width, height, assertions) {
    return { src: dedent(src), width, height, assertions };
}
Test._all = [];
Test.add = (title, tests) => {
    Test._all.push({ title, tests });
};
// Translated from https://prng.di.unimi.it/xoroshiro64starstar.c
Test.PRNG = class PRNG {
    s0 = 1234;
    s1 = 5678;
    nextUint32() {
        const s0 = this.s0;
        let s1 = this.s1;
        const r0 = Math.imul(s0, 0x9E3779BB);
        const r1 = (r0 << 5) | (r0 >>> 27);
        const r2 = Math.imul(r1, 5);
        s1 ^= s0;
        this.s0 = ((s0 << 26) | (s0 >>> 6)) ^ s1 ^ (s1 << 9);
        this.s1 = (s1 << 13) | (s1 >>> 19);
        return r2 >>> 0;
    }
    nextInt(n) {
        return this.nextUint32() % n;
    }
    nextDouble() {
        return this.nextUint32() * (2 ** -32);
    }
};
Test._compile = (src, config) => {
    const compiled = Compiler.compile(src, 'JavaScript', config);
    return [compiled, Function(`return ${compiled};`)()];
};
Test._run = (test) => {
    let compiled = undefined;
    try {
        const [_compiled, f] = Test._compile(test.src);
        compiled = _compiled;
        const actual = f(test.width, test.height, new Test.PRNG()).toString();
        const failures = test.assertions.filter((assertion) => {
            switch (assertion.kind) {
                case 'count': {
                    const count = [...actual].reduce((acc, char) => acc + (assertion.colour === char ? 1 : 0), 0);
                    return count !== assertion.expected;
                }
                case 'equals': {
                    return actual !== assertion.expected;
                }
            }
        });
        return failures.length === 0
            ? { result: 'pass' }
            : { result: 'fail.assert', failures, actual };
    }
    catch (e) {
        if (e instanceof Diagnostics) {
            return { result: 'fail.diagnostics', diagnostics: e };
        }
        else {
            console.log(e);
            return {
                result: compiled === undefined ? 'fail.ice' : 'fail.runtime',
                msg: e instanceof Error ? (e.stack ?? e.toString()) : `${e}`,
            };
        }
    }
};
Test.runAll = () => {
    const testPanelElem = document.getElementById('test_panel') ?? fail();
    let totalRun = 0, totalPassed = 0;
    const totalSectionElem = document.createElement('h2');
    const totalSummaryElem = document.createElement('span');
    totalSectionElem.appendChild(document.createTextNode('Tests ('));
    totalSectionElem.appendChild(totalSummaryElem);
    totalSectionElem.appendChild(document.createTextNode(')'));
    testPanelElem.appendChild(totalSectionElem);
    for (const { title, tests } of Test._all.sort((a, b) => a.title.localeCompare(b.title))) {
        const sectionElem = document.createElement('div');
        const titleElem = document.createElement('h3');
        const summaryElem = document.createElement('span');
        titleElem.appendChild(document.createTextNode(`${title} (`));
        titleElem.appendChild(summaryElem);
        titleElem.appendChild(document.createTextNode(`)`));
        sectionElem.appendChild(titleElem);
        testPanelElem.appendChild(sectionElem);
        function _assertionToString(a) {
            switch (a.kind) {
                case 'count':
                    return `Expected count [${a.colour}] == ${a.expected}`;
                case 'equals':
                    return `Expected:\n${a.expected}`;
            }
        }
        const cases = Object.entries(tests);
        let passes = 0, total = 0;
        for (const [name, test] of cases) {
            ++total;
            const r = Test._run(test);
            const rowElem = document.createElement('div');
            const passFailElem = document.createElement('span');
            passFailElem.className = r.result === 'pass' ? 'pass' : 'fail';
            passFailElem.innerText = r.result;
            rowElem.appendChild(document.createTextNode(`${title}.${name}: `));
            rowElem.appendChild(passFailElem);
            sectionElem.appendChild(rowElem);
            if (r.result === 'pass') {
                ++passes;
                continue;
            }
            const failSrcElem = document.createElement('pre');
            failSrcElem.className = 'src';
            failSrcElem.innerText = test.src;
            rowElem.appendChild(failSrcElem);
            const failInfoElem = document.createElement('pre');
            failInfoElem.className = 'err';
            rowElem.appendChild(failInfoElem);
            switch (r.result) {
                case 'fail.assert':
                    failInfoElem.appendChild(document.createTextNode(`${r.actual}\n\n`));
                    for (const a of r.failures.map(_assertionToString)) {
                        failInfoElem.appendChild(document.createTextNode(`${a}\n`));
                    }
                    break;
                case 'fail.diagnostics':
                    for (const d of r.diagnostics.errors) {
                        failInfoElem.appendChild(document.createTextNode(`${d}\n`));
                    }
                    break;
                case 'fail.ice':
                case 'fail.runtime':
                    failInfoElem.innerText = r.msg;
                    break;
            }
        }
        summaryElem.className = passes === total ? 'pass' : 'fail';
        summaryElem.innerText = `${passes}/${total} passed`;
        totalRun += total;
        totalPassed += passes;
    }
    totalSummaryElem.className = totalPassed === totalRun ? 'pass' : 'fail';
    totalSummaryElem.innerText = `${totalPassed}/${totalRun} passed`;
};
///<reference path="../framework.ts"/>
Benchmark.setBaselines({
    "GameOfLife": {
        "codeSize": 3290,
        "compileTime": 0.7521500000003726,
        "runTime": 85.01250000003104
    },
    "River": {
        "codeSize": 10832,
        "compileTime": 2.5895213454065393,
        "runTime": 9.04774774775446
    },
    "BasicDungeonGrowth": {
        "codeSize": 312817,
        "compileTime": 59.69117647058823,
        "runTime": 4.8474576271186445
    },
    "NystromDungeon": {
        "codeSize": 20177,
        "compileTime": 5.338563829789216,
        "runTime": 6.396485623007956
    },
    "MazesAndLakes": {
        "codeSize": 16161,
        "compileTime": 3.4143344709910326,
        "runTime": 6.089969604856428
    }
});
///<reference path="../framework.ts"/>
Benchmark.add({
    GameOfLife: `
        # Implementation of John Conway's "Game of Life"
        # https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

        grid [DA]

        @limit 1
        prl: [D] -> [A] if random < 0.5
        
        @limit 1000
        convolution {kernel="Moore"}:
            [D] -> [A] if sum [A] == 3
            [A] -> [D] if sum [A] < 2 or sum [A] > 3
    `,
    River: `
        # Translated from Maxim Gumin's 'River' model
        # https://github.com/mxgmn/MarkovJunior/blob/main/models/River.xml

        grid [BWRUGE]

        once: [B] -> [W]
        once: [B] -> [R]

        one:
            [RB] -> [RR]
            [WB] -> [WW]

        all: [RW] -> [UU]
        all: [[WR]] -> [B]

        @limit 1
        all: [UB] -> [UU]

        all: [BU/UB] -> [U./..]
        all: [UB] -> [.G]

        @limit 13
        one: [B] -> [E]

        one:
            [EB] -> [.E]
            [GB] -> [.G]
    `,
    BasicDungeonGrowth: `
        # Translated from Maxim Gumin's 'BasicDungeonGrowth' model
        # https://github.com/mxgmn/MarkovJunior/blob/main/models/BasicDungeonGrowth.xml

        grid [BRACDG]
        union [?] = [[BR]]

        put [R] at origin

        one: [..?../.BBB./.BBB?/.BBB./..R..] -> [AARAA/ADDDA/ADDDR/ADDDA/AACAA]
        one: [ACA/BBB] -> [ARA/BBB]

        all: [C] -> [D]
        all: [R] -> [D]

        all: [BD] -> [.A]
        all: [DDD/ADA/DDD] -> [.../D.D/...]
        all: [DDD/DAD/DDD] -> [.../.D./...]
    `,
    NystromDungeon: `
        # Translated from Maxim Gumin's 'NystromDungeon' model, which
        # is based on Bob Nystrom's dungeon generation algorithm
        # https://github.com/mxgmn/MarkovJunior/blob/main/models/NystromDungeon.xml
        # http://journal.stuffwithstuff.com/2014/12/21/rooms-and-mazes/
        
        let NUM_CYCLES = 5
        grid [BPWRG]
        put [P] at origin
        all: [PBB] -> [..P]
        
        let roomIn = [
            PBPBPBPBP /
            BBBBBBBBB /
            PBPBPBPBP /
            BBBBBBBBB /
            PBPBPBPBP /
            BBBBBBBBB /
            PBPBPBPBP
        ]
        let roomOut = [
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW
        ]
        
        one: roomIn -> roomOut
        markov:
            one: [RBP] -> [GGR]
            one: [GGR] -> [RWW]
            one: [P] -> [R]
        
        once: [R] -> [G]
        all: [R] -> [W]
        markov:
            all: [GWW] -> [..G]
            one: [GBW] -> [.WG]
        
        @limit NUM_CYCLES
        one: [GBG] -> [.W.]
        
        all: [G] -> [W]
        all: [BBB/BWB] -> [BBB/BBB]    
    `,
    MazesAndLakes: `
        # Author: Andrew Kay
        
        use let g = grid [BWREI]
        
        let LAKE_SEEDS = 4
        let LAKE_SIZE = g.width * g.height // 4
        let LAND_SEEDS = 32
        let ANIMATE_WATER = false
        
        @limit LAKE_SEEDS
        one: [B] -> [I]
        
        @limit LAKE_SIZE - LAKE_SEEDS
        one: [IB] -> [.I]
        
        @limit LAND_SEEDS
        one: [B] -> [R]
        
        markov:
            one: [RBB] -> [WWR]
            one: [RWW] -> [EER]
        
        one: [R] -> [E]
        
        one: [BBWBB] -> [..B..]
        
        @limit LAKE_SIZE // 2
        one: [II] -> [BB]
        
        markov:
            # fill unused space with a water texture
            one:
                [BB./BBB/.B.] -> [.../.I./...]
                [.I./IBI/.I.] -> [.../.I./...]
        
            # delete water pixels at random, for an animated effect
            one: [I] -> [B] if ANIMATE_WATER
    `,
});
///<reference path="../framework.ts"/>
Test.add('Convolution', {
    emptySum: Test(`
        grid [BW]
        convolution {kernel="Moore"}:
            [B] -> [W] if sum [W] == 0
    `, 3, 3, [
        Assert.equals('WWW\nWWW\nWWW'),
    ]),
    oneSum: Test(`
        grid [BW]
        put [W] at origin
        convolution {kernel="Moore"}:
            [B] -> [W] if sum [W] == 1
            [W] -> [B] if sum [B] == 8
    `, 3, 3, [
        Assert.equals('WWW\nWBW\nWWW'),
    ]),
    boundary: Test(`
        grid [BW035]
        convolution {kernel="Moore", boundary=[W]}:
            [B] -> (let s = sum [W] in
                [0] if s == 0 else [3] if s == 3 else [5] if s == 5 else [.])
    `, 4, 4, [
        Assert.equals('5335\n3003\n3003\n5335'),
    ]),
});
///<reference path="../framework.ts"/>
Test.add('Basic', {
    blank: Test(`grid [BW]`, 3, 3, [
        Assert.equals('BBB\nBBB\nBBB'),
    ]),
    put: Test(`
        grid [BW]
        put [W] at origin
    `, 3, 3, [
        Assert.equals('BBB\nBWB\nBBB'),
    ]),
    one: Test(`
        grid [BW]
        one: [B] -> [W]
    `, 3, 3, [
        Assert.equals('WWW\nWWW\nWWW'),
    ]),
    all: Test(`
        grid [BW]
        all: [B] -> [W]
    `, 3, 3, [
        Assert.equals('WWW\nWWW\nWWW'),
    ]),
    neighbours: Test(`
        grid [BWR]
        put [W] at origin
        one: [WB] -> [.R]
    `, 3, 3, [
        Assert.equals('BRB\nRWR\nBRB'),
    ]),
});
