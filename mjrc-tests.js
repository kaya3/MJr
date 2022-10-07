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
const Benchmark = {
    _all: new Map(),
    _baselines: new Map(),
    add(benchmarks) {
        for (const [name, src] of Object.entries(benchmarks)) {
            if (Benchmark._all.has(name)) {
                throw new Error(`Benchmark '${name}' already exists`);
            }
            Benchmark._all.set(name, src);
        }
    },
    setBaselines(baselines) {
        for (const [name, baseline] of Object.entries(baselines)) {
            Benchmark._baselines.set(name, baseline);
        }
    },
    _time(f) {
        const MAX_TOTAL_TIME = 1000;
        const MAX_ITERATIONS = 1000;
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
    _run(src) {
        const result = Compiler.compile(src, 'JavaScript', { indentSpaces: 0 });
        const compileTime = Benchmark._timeBestOfThree(() => Compiler.compile(src, 'JavaScript'));
        return {
            codeSize: result.length,
            compileTime,
        };
    },
    _runAll(sectionElem) {
        const SMALL_CHANGE_PERCENT = 3;
        const titleElem = document.createElement('h3');
        titleElem.appendChild(document.createTextNode('Benchmarks'));
        sectionElem.appendChild(titleElem);
        const tableElem = document.createElement('table');
        const tableHeaderElem = document.createElement('tr');
        tableElem.appendChild(tableHeaderElem);
        for (const heading of ['Model', 'Compile time', 'Output size']) {
            const headingElem = document.createElement('th');
            headingElem.innerText = heading;
            tableHeaderElem.append(headingElem);
        }
        sectionElem.appendChild(tableElem);
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
            const [name, src] = cases[i];
            setTimeout(() => {
                const baseline = Benchmark._baselines.get(name);
                const result = Benchmark._run(src);
                allResults[name] = result;
                const rowElem = document.createElement('tr');
                const nameElem = document.createElement('td');
                nameElem.innerText = name;
                rowElem.appendChild(nameElem);
                rowElem.appendChild(_makeTDElem(r => r.compileTime, x => `${x.toFixed(1)} ms`, baseline, result));
                rowElem.appendChild(_makeTDElem(r => r.codeSize, x => `${(x / 1024).toFixed(1)} KiB`, baseline, result));
                tableElem.appendChild(rowElem);
                runCase(i + 1);
            }, 50);
        }
        runCase(0);
    }
};
function Test(src, width, height, assertions) {
    return { src, width, height, assertions };
}
Test._all = [];
Test.add = (title, tests) => {
    Test._all.push({ title, tests });
};
Test._run = (test) => {
    let compiled = undefined;
    try {
        compiled = Compiler.compile(test.src, 'JavaScript');
        const f = Function(`return ${compiled};`)();
        const actual = f(test.width, test.height).toString();
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
    let totalRun = 0, totalPassed = 0;
    const totalSectionElem = document.createElement('h2');
    const totalSummaryElem = document.createElement('span');
    totalSectionElem.appendChild(document.createTextNode('Tests ('));
    totalSectionElem.appendChild(totalSummaryElem);
    totalSectionElem.appendChild(document.createTextNode(')'));
    document.body.appendChild(totalSectionElem);
    const benchmarkSectionElem = document.createElement('div');
    document.body.appendChild(benchmarkSectionElem);
    for (const { title, tests } of Test._all) {
        const sectionElem = document.createElement('div');
        const titleElem = document.createElement('h3');
        const summaryElem = document.createElement('span');
        titleElem.appendChild(document.createTextNode(`${title} (`));
        titleElem.appendChild(summaryElem);
        titleElem.appendChild(document.createTextNode(`)`));
        sectionElem.appendChild(titleElem);
        document.body.appendChild(sectionElem);
        function _assertionToString(a) {
            switch (a.kind) {
                case 'count':
                    return `Expected count [${a.colour}] == ${a.expected}`;
                case 'equals':
                    return `Expected:\n${a.expected}`;
            }
        }
        let passes = 0, total = 0;
        for (const [name, test] of Object.entries(tests)) {
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
            const failInfoElem = document.createElement('pre');
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
    setTimeout(() => Benchmark._runAll(benchmarkSectionElem), 50);
};
///<reference path="../framework.ts"/>
Benchmark.setBaselines({
    "GameOfLife": {
        "codeSize": 3679,
        "compileTime": 0.3083000000002794
    },
    "River": {
        "codeSize": 13548,
        "compileTime": 1.427674750357032
    },
    "BasicDungeonGrowth": {
        "codeSize": 408748,
        "compileTime": 69.32666666666046
    },
    "PacMan": {
        "codeSize": 240832,
        "compileTime": 59.26470588235294
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
    PacMan: `
# Author: Andrew Kay

grid [BWDPYIERO]

# draw maze
put [P] at origin
all: [PBB] -> [..P]
put [W] at origin
all: [WBPBP] -> [....W]
put [D] at origin
all: [DBPBW] -> [..W.D]
one: [DBPBDBP/BBBBBBB/.BPBPBP] -> [..W..../......./.......]

@limit 5
one: [DBPBD] -> [..W..]
all: [PBP] -> [.P.]
all: [D] -> [W]
one: [BBBBBB/BPBWBP/BBBBBB] -> [....../..PPP./......]

# add player and enemies
put [Y] at origin

@limit 4
one: [W] -> [O]

# play!

union [?] = [[WB]]

sequence:
    # mark unsafe moves
    all: [.BBB/[RO]B?B/.BBB] -> [..I./.I.I/..I.] if count [Y] > 0
    
    # allow player to backtrack if they have no safe move
    one: [E] -> [B] if count [YB?] == 0
    
    # allow player to make an unsafe move if they have no safe move
    one: [I] -> [B] if count [YB?] == 0
    
    # player moves
    @limit 1
    all:
        [YBW] -> [BEY]
        [YBB] -> [BEY] if count [YBW] == 0
        [[IE]] -> [B]
    
    # enemy eats player, if possible
    once:
        [RBY] -> [BBR]
        [OBY] -> [WBR]
    
    # enemies move
    @limit 1
    all:
        let alive = count [Y] > 0
        [RBW] -> [BDO] if alive
        [OBW] -> [WDO] if alive
        [RBB] -> [BDR] if alive
        [OBB] -> [WDR] if alive
        [D] -> [B]    
`,
});
///<reference path="../framework.ts"/>
Test.add('Basic', {
    blank: Test(`grid [BW]`, 3, 3, [
        Assert.equals('BBB\nBBB\nBBB'),
    ]),
    put: Test(`
grid [BW]
put [W] at origin`, 3, 3, [
        Assert.equals('BBB\nBWB\nBBB'),
    ]),
    one: Test(`
grid [BW]
one: [B] -> [W]`, 3, 3, [
        Assert.equals('WWW\nWWW\nWWW'),
    ]),
    all: Test(`
grid [BW]
all: [B] -> [W]`, 3, 3, [
        Assert.equals('WWW\nWWW\nWWW'),
    ]),
    neighbours: Test(`
grid [BWR]
put [W] at origin
one: [WB] -> [.R]`, 3, 3, [
        Assert.equals('BRB\nRWR\nBRB'),
    ]),
});
