"use strict";
const A = {
    equals(expected) {
        expected = expected.trim();
        return { kind: 'equals', expected };
    },
    count(colour, expected) {
        return { kind: 'count', colour, expected };
    },
};
function T(src, width, height, assertions) {
    return { src, width, height, assertions };
}
function runTest(test) {
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
}
function runTests(title, tests) {
    const sectionElem = document.createElement('div');
    const titleElem = document.createElement('h2');
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
        const r = runTest(test);
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
}
///<reference path="../framework.ts"/>
runTests('Basic', {
    blank: T(`grid [BW]`, 3, 3, [
        A.equals('BBB\nBBB\nBBB'),
    ]),
    put: T(`
grid [BW]
put [W] at origin`, 3, 3, [
        A.equals('BBB\nBWB\nBBB'),
    ]),
    one: T(`
grid [BW]
one: [B] -> [W]`, 3, 3, [
        A.equals('WWW\nWWW\nWWW'),
    ]),
    all: T(`
grid [BW]
all: [B] -> [W]`, 3, 3, [
        A.equals('WWW\nWWW\nWWW'),
    ]),
});
