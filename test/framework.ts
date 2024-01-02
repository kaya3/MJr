interface TestCase extends Readonly<{
    src: string,
    width: number,
    height: number,
    assertions: readonly Assertion[],
}> {}

type Assertion = Readonly<
    | {kind: 'count', colour: string, expected: number}
    | {kind: 'equals', expected: string}
>

type TestResult = Readonly<
    | {result: 'pass'}
    | {result: 'fail.assert', failures: readonly Assertion[], actual: string}
    | {result: 'fail.diagnostics', diagnostics: Diagnostics}
    | {result: 'fail.ice' | 'fail.runtime', msg: string}
>

interface Benchmark extends Readonly<{
    src: string,
    baseline: BenchmarkResult,
}> {}

interface BenchmarkResult extends Readonly<{
    codeSize: number,
    compileTime: number,
    runTime: number,
}> {}

const Assert = {
    equals(expected: string): Assertion {
        expected = expected.trim();
        return {kind: 'equals', expected};
    },
    count(colour: string, expected: number): Assertion {
        return {kind: 'count', colour, expected};
    },
};

function dedent(src: string): string {
    const lines = src.split('\n');
    const firstLine = lines.find(line => line.trim().length > 0) ?? fail();
    const initialIndent = firstLine.split(/[^\s]/, 2)[0];
    return lines.map(line => line.replace(initialIndent, ''))
        .join('\n')
        .trim();
}

const Benchmark = {
    _all: new Map<string, string>(),
    _baselines: new Map<string, BenchmarkResult>(),
    add(benchmarks: Record<string, string>): void {
        for(const [name, src] of Object.entries(benchmarks)) {
            if(Benchmark._all.has(name)) { throw new Error(`Benchmark '${name}' already exists`); }
            Benchmark._all.set(name, dedent(src));
        }
    },
    setBaselines(baselines: Record<string, BenchmarkResult>): void {
        for(const [name, baseline] of Object.entries(baselines)) {
            Benchmark._baselines.set(name, baseline);
        }
    },
    
    _time(f: () => void): number {
        const MAX_TOTAL_TIME = 2000;
        const MAX_ITERATIONS = 2000;
        
        const startTime = performance.now();
        const maxEndTime = startTime + MAX_TOTAL_TIME;
        let i = 0, endTime: number;
        do {
            f();
            ++i;
        } while((endTime = performance.now()) < maxEndTime && i < MAX_ITERATIONS);
        
        return (endTime - startTime) / i;
    },
    _timeBestOfThree(f: () => void): number {
        return Math.min(
            Benchmark._time(f),
            Benchmark._time(f),
            Benchmark._time(f),
        );
    },
    _run(src: string, gridSize: number): BenchmarkResult {
        const [result, f] = Test._compile(src, {indentSpaces: 0, emitComments: false, maxIterations: 100000});
        const rng = new Test.PRNG();
        return {
            codeSize: result.length,
            compileTime: Benchmark._timeBestOfThree(() => Compiler.compile(src, 'JavaScript')),
            runTime: Benchmark._timeBestOfThree(() => f(gridSize, gridSize, rng)),
        };
    },
    
    runAll(): void {
        const SMALL_CHANGE_PERCENT = 5;
        const BENCHMARK_GRID_SIZE = 127;
        
        const benchmarkPanelElem = document.getElementById('benchmark_panel') ?? fail();
        const titleElem = document.createElement('h2');
        titleElem.appendChild(document.createTextNode('Benchmarks'));
        benchmarkPanelElem.appendChild(titleElem);

        const tableElem = document.createElement('table');
        const tableHeaderElem = document.createElement('tr');
        tableElem.appendChild(tableHeaderElem);
        for(const heading of ['Model', 'Compile time', 'Output size', 'Run time']) {
            const headingElem = document.createElement('th');
            headingElem.innerText = heading;
            tableHeaderElem.append(headingElem);
        }
        benchmarkPanelElem.appendChild(tableElem);
        
        const allResults: Record<string, BenchmarkResult> = {};
        let improvements = 0, regressions = 0;
        function _makeTDElem(f: (r: BenchmarkResult) => number, toStr: (x: number) => string, baseline: BenchmarkResult | undefined, result: BenchmarkResult): HTMLTableCellElement {
            const tdElem = document.createElement('td');
            const changeElem = document.createElement('span');
            const after = f(result);
            if(baseline === undefined) {
                changeElem.innerText = 'no baseline';
            } else {
                const before = f(baseline);
                const changePercent = ((after / before) - 1) * 100;
                changeElem.innerText = `${changePercent >= 0 ? '+' : ''}${changePercent.toFixed(1)}%`;
                if(changePercent > SMALL_CHANGE_PERCENT) {
                    ++regressions;
                    changeElem.className = 'fail';
                } else if(changePercent < -SMALL_CHANGE_PERCENT) {
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
        function runCase(i: number): void {
            if(i >= cases.length) {
                const improvementsElem = document.createElement('span');
                const regressionsElem = document.createElement('span');
                improvementsElem.innerText = `${improvements} improvement${improvements === 1 ? '' : 's'}`;
                if(improvements > 0) { improvementsElem.className = 'pass'; }
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

function Test(src: string, width: number, height: number, assertions: readonly Assertion[]): TestCase {
    return {src: dedent(src), width, height, assertions};
}

Test._all = [] as Readonly<{title: string, tests: IRecord<string, TestCase>}>[];

Test.add = (title: string, tests: IRecord<string, TestCase>): void => {
    Test._all.push({title, tests});
};

// Translated from https://prng.di.unimi.it/xoroshiro64starstar.c
Test.PRNG = class PRNG {
    s0: number = 1234;
    s1: number = 5678;
    
    private nextUint32(): number {
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
    
    nextInt(n: number): number {
        return this.nextUint32() % n;
    }
    nextDouble(): number {
        return this.nextUint32() * (2 ** -32);
    }
};

Test._compile = (src: string, config?: Partial<Compiler.Config>): [string, (width: number, height: number, rng: MJr.PRNG) => MJr.Grid] => {
    const compiled = Compiler.compile(src, 'JavaScript', config);
    return [compiled, Function(`return ${compiled};`)()];
};
Test._run = (test: TestCase): TestResult => {
    let compiled: string | undefined = undefined;
    try {
        const [_compiled, f] = Test._compile(test.src);
        compiled = _compiled;
        const actual = f(test.width, test.height, new Test.PRNG()).toString();
        
        const failures = test.assertions.filter((assertion: Assertion): boolean => {
            switch(assertion.kind) {
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
            ? {result: 'pass'}
            : {result: 'fail.assert', failures, actual};
    } catch(e: unknown) {
        if(e instanceof Diagnostics) {
            return {result: 'fail.diagnostics', diagnostics: e};
        } else {
            console.log(e);
            return {
                result: compiled === undefined ? 'fail.ice' : 'fail.runtime',
                msg: e instanceof Error ? (e.stack ?? e.toString()) : `${e}`,
            };
        }
    }
};

Test.runAll = (): void => {
    const testPanelElem = document.getElementById('test_panel') ?? fail();
    
    let totalRun = 0, totalPassed = 0;
    const totalSectionElem = document.createElement('h2');
    const totalSummaryElem = document.createElement('span');
    totalSectionElem.appendChild(document.createTextNode('Tests ('));
    totalSectionElem.appendChild(totalSummaryElem);
    totalSectionElem.appendChild(document.createTextNode(')'));
    testPanelElem.appendChild(totalSectionElem);
    
    for(const {title, tests} of Test._all.sort((a, b) => a.title.localeCompare(b.title))) {
        const sectionElem = document.createElement('div');
        const titleElem = document.createElement('h3');
        const summaryElem = document.createElement('span');
        titleElem.appendChild(document.createTextNode(`${title} (`));
        titleElem.appendChild(summaryElem);
        titleElem.appendChild(document.createTextNode(`)`));
        sectionElem.appendChild(titleElem);
        testPanelElem.appendChild(sectionElem);
        
        function _assertionToString(a: Assertion): string {
            switch(a.kind) {
                case 'count':
                    return `Expected count [${a.colour}] == ${a.expected}`;
                case 'equals':
                    return `Expected:\n${a.expected}`;
            }
        }
        
        const cases: [string, TestCase][] = Object.entries(tests);
        let passes = 0, total = 0;
        for(const [name, test] of cases) {
            ++total;
            const r = Test._run(test);
            const rowElem = document.createElement('div');
            const passFailElem = document.createElement('span');
            passFailElem.className = r.result === 'pass' ? 'pass' : 'fail';
            passFailElem.innerText = r.result;
            rowElem.appendChild(document.createTextNode(`${title}.${name}: `));
            rowElem.appendChild(passFailElem);
            sectionElem.appendChild(rowElem);
            
            if(r.result === 'pass') {
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
            switch(r.result) {
                case 'fail.assert':
                    failInfoElem.appendChild(document.createTextNode(`${r.actual}\n\n`));
                    for(const a of r.failures.map(_assertionToString)) {
                        failInfoElem.appendChild(document.createTextNode(`${a}\n`));
                    }
                    break;
                case 'fail.diagnostics':
                    for(const d of r.diagnostics.errors) {
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
