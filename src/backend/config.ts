namespace Compiler {
    export type Config = typeof DEFAULT_CONFIG
    export const DEFAULT_CONFIG = {
        indentSpaces: 4,
        emitComments: true,
        emitChecks: true,
        entryPointName: 'main',
        maxIterations: 0,
        animate: false,
    };
}
