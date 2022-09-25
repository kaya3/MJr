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

const A = {
    equals(expected: string): Assertion {
        expected = expected.trim();
        return {kind: 'equals', expected};
    },
    count(colour: string, expected: number): Assertion {
        return {kind: 'count', colour, expected};
    },
};

function T(src: string, width: number, height: number, assertions: readonly Assertion[]): TestCase {
    return {src, width, height, assertions};
}

function runTest(test: TestCase): TestResult {
    let compiled: string | undefined = undefined;
    try {
        compiled = Compiler.compile(test.src, 'JavaScript');
        const f: (width: number, height: number, rng?: MJr.PRNG) => MJr.Grid = Function(`return ${compiled};`)();
        const actual = f(test.width, test.height).toString();
        
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
}

function runTests(title: string, tests: IRecord<string, TestCase>): void {
    const sectionElem = document.createElement('div');
    const titleElem = document.createElement('h2');
    const summaryElem = document.createElement('span');
    titleElem.appendChild(document.createTextNode(`${title} (`));
    titleElem.appendChild(summaryElem);
    titleElem.appendChild(document.createTextNode(`)`));
    sectionElem.appendChild(titleElem);
    document.body.appendChild(sectionElem);
    
    function _assertionToString(a: Assertion): string {
        switch(a.kind) {
            case 'count':
                return `Expected count [${a.colour}] == ${a.expected}`;
            case 'equals':
                return `Expected:\n${a.expected}`;
        }
    }
    
    let passes = 0, total = 0;
    for(const [name, test] of Object.entries(tests) as [string, TestCase][]) {
        ++total;
        const r = runTest(test);
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
        
        const failInfoElem = document.createElement('pre');
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
}
