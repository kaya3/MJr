type TestResult = Readonly<
    | {result: 'pass'}
    | {result: 'fail', expected: string, actual: string}
    | {result: 'error', msg: string}
    | {result: 'ice', msg: string}
>

function runTest(src: string, expected: string): TestResult {
    expected = expected.trim();
    const height = expected.split('\n').length;
    const width = height > 1 ? expected.indexOf('\n') : expected.length;
    
    try {
        const compiled = Compiler.compile(src, 'JavaScript');
        const f: (width: number, height: number) => MJr.Grid = Function(`return ${compiled};`)();
        const actual = f(width, height).toString();
        if(actual === expected) {
            return {result: 'pass'};
        } else {
            return {result: 'fail', expected, actual};
        }
    } catch(e: unknown) {
        if(e instanceof Diagnostics) {
            return {result: 'error', msg: e.errors.join('\n')};
        } else {
            console.log(e);
            return {result: 'ice', msg: e instanceof Error ? (e.stack ?? e.toString()) : `${e}`};
        }
    }
}
