///<reference path="ir.ts"/>

/**
 * Compiles MJr source code to a specified target language.
 */
namespace Compiler {
    export const COMPILER_VERSION = '0.1 (unstable)';
    export const REQUIRED_RUNTIME_LIB_VERSION = 0;
    
    export type Config = typeof DEFAULT_CONFIG
    const DEFAULT_CONFIG = {
        indentSpaces: 4,
        emitChecks: true,
        entryPointName: 'main',
        animate: false,
    };
    
    const ALWAYS_USED_OPS: readonly IR.Op[] = [
        'bool_and', 'bool_or', 'bool_not',
        'int_eq', 'int_ne', 'int_lt', 'int_le', 'int_gt', 'int_ge',
        'int_and', 'int_or', 'int_not', 'int_lshift', 'int_rshift',
        'loose_int_plus', 'loose_int_mult', 'loose_int_floordiv', 'loose_int_mod',
    ];
    
    // names
    const WIDTH = IR.name('width'),
        HEIGHT = IR.name('height'),
        RNG = IR.name('rng'),
        STATE = IR.name('state'),
        AT = IR.name('at'),
        AT_X = IR.name('atX'),
        AT_Y = IR.name('atY'),
        MATCHES = 'matches',
        MATCH_COUNT = 'count',
        MATCH = IR.name('m'),
        ANY = IR.name('any'),
        G = IR.name('g'),
        I = IR.name('i'),
        J = IR.name('j'),
        N = IR.name('n'),
        P = IR.name('p'),
        START_X = IR.name('startX'),
        START_Y = IR.name('startY'),
        END_X = IR.name('endX'),
        END_Y = IR.name('endY'),
        EFFECTIVE_WIDTH = IR.name('w'),
        EFFECTIVE_HEIGHT = IR.name('h'),
        X = IR.name('x'),
        Y = IR.name('y'),
        S = IR.name('s'),
        OLD_S = IR.name('oldS'),
        MASK = 'mask',
        MASK_CLEAR = 'mask_clear',
        MASK_SET = 'mask_set',
        MASK_HASNT = 'mask_hasnt';
    
    // helpers
    const OP = IR.OP;
    
    class CGrid {
        readonly width: IR.NameExpr;
        readonly height: IR.NameExpr;
        readonly n: IR.NameExpr;
        readonly data: IR.NameExpr;
        private obj: IR.NameExpr | undefined = undefined;
        private origin: IR.NameExpr | undefined = undefined;
        readonly originX: IR.Expr;
        readonly originY: IR.Expr;
        
        readonly counters = new Map<string, IR.NameExpr>();
        readonly samplers = new Map<string, CSampler>();
        readonly matcher: CMatcher;
        
        constructor(readonly grid: ASG.FormalGrid) {
            const {id, scaleX, scaleY} = grid;
            
            this.width = scaleX === 1 ? WIDTH : IR.name(`grid${id}_width`);
            this.height = scaleY === 1 ? HEIGHT : IR.name(`grid${id}_height`);
            this.n = IR.name(`grid${id}_n`);
            this.data = IR.name(`grid${id}_data`);
            
            this.originX = scaleX % 2 === 0 ? OP.multConstant(WIDTH, scaleX >> 1) : OP.divConstant(this.width, 2);
            this.originY = scaleY % 2 === 0 ? OP.multConstant(HEIGHT, scaleY >> 1) : OP.divConstant(this.height, 2);
            
            this.matcher = new CMatcher(this);
        }
        
        makeCounter(patterns: readonly Pattern[]): IR.Expr {
            const {counters, samplers, matcher} = this;
            
            const key = patterns.map(Pattern.key).join('\n');
            
            // TODO: this is order-dependent, a matching sampler might be declared later
            const sampler = samplers.get(key);
            if(sampler !== undefined) { return sampler.count; }
            
            let counter = counters.get(key);
            if(counter !== undefined) { return counter; }
            
            counters.set(key, counter = IR.name(`grid${this.grid.id}_counter${counters.size}`));
            for(const pattern of patterns) {
                matcher.addMatchHandler({kind: 'counter', pattern, counter});
            }
            return counter;
        }
        
        makeSampler(patterns: readonly Pattern[]): CSampler {
            const {samplers, matcher} = this;
            
            const key = patterns.map(Pattern.key).join('\n');
            let sampler = samplers.get(key);
            if(sampler !== undefined) { return sampler; }
            
            samplers.set(key, sampler = new CSampler(samplers.size, this, patterns.length));
            
            for(let i = 0; i < patterns.length; ++i) {
                const pattern = patterns[i];
                matcher.addMatchHandler({kind: 'sampler', pattern, sampler, i});
            }
            return sampler;
        }
        
        useOrigin(): IR.NameExpr {
            return this.origin ??= IR.name(`grid${this.grid.id}_origin`);
        }
        useObj(): IR.NameExpr {
            return this.obj ??= IR.name(`grid${this.grid.id}_obj`);
        }
        declareVars(): IR.Stmt[] {
            const {width, height, n, data, obj, origin, originX, originY, grid} = this;
            const alphabetKey = grid.alphabet.key;
            const consts: IR.VarDeclWithInitialiser[] = [];
            const vars: IR.VarDeclWithInitialiser[] = [];
            
            if(width !== WIDTH) {
                consts.push({name: width.name, type: IR.INT_TYPE, initialiser: OP.multConstant(WIDTH, grid.scaleX)});
            }
            if(height !== HEIGHT) {
                consts.push({name: height.name, type: IR.INT_TYPE, initialiser: OP.multConstant(HEIGHT, grid.scaleY)});
            }
            consts.push(
                {name: n.name, type: IR.INT_TYPE, initialiser: OP.mult(width, height)},
                {name: data.name, type: IR.GRID_DATA_ARRAY_TYPE, initialiser: IR.newArray(n, alphabetKey.length)},
            );
            if(obj !== undefined) {
                const initialiser = IR.libConstructorCall('Grid', [width, height, data, IR.str(alphabetKey)]);
                consts.push({name: obj.name, type: IR.GRID_TYPE, initialiser});
            }
            if(origin !== undefined) {
                consts.push({name: origin.name, type: IR.INT_TYPE, initialiser: OP.add(originX, OP.mult(originY, width))});
            }
            
            for(const sampler of this.samplers.values()) {
                consts.push(sampler.declare());
            }
            
            vars.push(...Array.from(this.counters.values(), counter => ({
                name: counter.name,
                type: IR.INT_TYPE,
                initialiser: IR.ZERO,
            })));
            
            return [
                IR.declVars(consts),
                IR.declVars(vars, true),
            ];
        }
        
        /**
         * Used internally for indices which are known to be in-bounds.
         * `x` and `y` must be non-negative.
         */
        index(x: IR.Expr, y: IR.Expr): IR.Expr {
            return OP.add(x, OP.mult(y, this.width));
        }
        
        /**
         * Used internally for relative indices which are known to be in-bounds.
         * The variables `AT`, `AT_X` and `AT_Y` must be declared in the IR,
         * and `dx` and `dy` must be non-negative.
         */
        relativeIndex(dx: number, dy: number): IR.Expr {
            if(this.grid.periodic) {
                const x = OP.mod(OP.add(AT_X, IR.int(dx)), this.width);
                const y = OP.mod(OP.add(AT_Y, IR.int(dy)), this.height);
                return OP.add(x, OP.mult(y, this.width));
            } else {
                return OP.add(OP.add(AT, IR.int(dx)), OP.multConstant(this.width, dy));
            }
        }
        
        checkedIndex(x: IR.Expr, y: IR.Expr): IR.Expr {
            return IR.libMethodCall('Grid', this.grid.periodic ? 'wrapIndex' : 'index', this.useObj(), [x, y]);
        }
        
        access(index: IR.Expr): IR.ArrayAccessExpr {
            return IR.access(this.data, index);
        }
        write(index: IR.Expr, colour: number, mask?: CMask): IR.Stmt {
            return mask !== undefined ? mask.set(this, index, colour)
                : IR.assign(this.access(index), '=', IR.int(colour));
        }
        update(x: IR.Expr, y: IR.Expr, w: IR.Expr, h: IR.Expr, doYield: boolean): IR.Stmt[] {
            return [
                IR.localCallStmt(this.matcher.updateFuncName, [x, y, w, h]),
                doYield ? this.yieldRewriteInfo(x, y, w, h) : IR.PASS,
            ];
        }
        yield_(): IR.YieldStmt {
            return this.yieldRewriteInfo(IR.ZERO, IR.ZERO, this.width, this.height);
        }
        private yieldRewriteInfo(x: IR.Expr, y: IR.Expr, w: IR.Expr, h: IR.Expr): IR.YieldStmt {
            return IR.yield_(IR.libConstructorCall('RewriteInfo', [this.useObj(), x, y, w, h]));
        }
    }
    
    // TODO: more kinds of match handler (e.g. conditional on another pattern not being
    // present, conditional on a boolean expression)
    type MatchHandler = Readonly<
        | {kind: 'sampler', pattern: Pattern, sampler: CSampler, i: number}
        | {kind: 'counter', pattern: Pattern, counter: IR.NameExpr}
    >
    
    class CMatcher {
        private readonly matchHandlers: MatchHandler[] = [];
        
        readonly updateFuncName: string;
        
        constructor(readonly g: CGrid) {
            // TODO: multiple matchers per grid? could depend on CFG
            this.updateFuncName = `grid${g.grid.id}_update`;
        }
        
        addMatchHandler(handler: MatchHandler): void {
            this.matchHandlers.push(handler);
        }
        
        declareUpdateFunc(): IR.Stmt[] {
            const {g, updateFuncName} = this;
            const {id, alphabet} = g.grid;
            
            const rowDFA = IR.name(`grid${id}_rowDFA`);
            const colDFA = IR.name(`grid${id}_colDFA`);
            const rowAcceptSets = IR.name(`grid${id}_rowAcceptSets`);
            const colAcceptSets = IR.name(`grid${id}_colAcceptSets`);
            const rowStates = IR.name(`grid${id}_rowStates`);
            const colStates = IR.name(`grid${id}_colStates`);
            
            const patternMap = IDMap.ofWithKey(this.matchHandlers.map(h => h.pattern), Pattern.key);
            const [_rowDFA, _colDFA] = makePatternMatcherDFAs(alphabet.key.length, patternMap);
            
            const handlersByPattern = makeArray<MatchHandler[]>(patternMap.size(), () => []);
            for(const handler of this.matchHandlers) {
                const patternID = patternMap.getID(handler.pattern);
                handlersByPattern[patternID].push(handler);
            }
            
            // would be possible to compute a table of deltas for each pair of states, but this results in quadratic size code output
            function makeStateChangeHandlers(f: 'add' | 'del'): IR.Stmt[] {
                return _colDFA.acceptSetMap.map(patternIDs => {
                    const out: IR.Stmt[] = [];
                    for(const patternID of patternIDs) {
                        for(const h of handlersByPattern[patternID]) switch(h.kind) {
                            case 'sampler': {
                                const match = OP.multAddConstant(I, h.sampler.numPatterns, IR.int(h.i));
                                out.push(IR.libMethodCallStmt('Sampler', f, h.sampler.name, [match]));
                                break;
                            }
                            case 'counter': {
                                out.push(IR.assign(h.counter, f === 'add' ? '+=' : '-=', IR.ONE));
                                break;
                            }
                        }
                    }
                    return IR.block(out);
                });
            }
            
            const rowDFAType = IR.constArrayType(_rowDFA.size()),
                rowAcceptSetsType = IR.constArrayType(_rowDFA.acceptSetMap.size()),
                colDFAType = IR.constArrayType(_colDFA.size()),
                colAcceptSetsType = IR.constArrayType(_colDFA.acceptSetMap.size()),
                rowStatesType = IR.mutableArrayType(rowDFAType.domainSize),
                colStatesType = IR.mutableArrayType(colDFAType.domainSize);
            
            return [
                IR.declVars([
                    {name: rowDFA.name, type: rowDFAType, initialiser: IR.constArray(_rowDFA.toFlatArray(), rowDFAType.domainSize, _rowDFA.alphabetSize)},
                    {name: rowAcceptSets.name, type: rowAcceptSetsType, initialiser: IR.constArray(_rowDFA.getAcceptSetIDs(), rowAcceptSetsType.domainSize)},
                    {name: colDFA.name, type: colDFAType, initialiser: IR.constArray(_colDFA.toFlatArray(), colDFAType.domainSize, _colDFA.alphabetSize)},
                    {name: colAcceptSets.name, type: colAcceptSetsType, initialiser: IR.constArray(_colDFA.getAcceptSetIDs(), colAcceptSetsType.domainSize)},
                    {name: rowStates.name, type: rowStatesType, initialiser: IR.newArray(g.n, rowDFAType.domainSize)},
                    {name: colStates.name, type: colStatesType, initialiser: IR.newArray(g.n, colDFAType.domainSize)},
                ]),
                IR.declFunc(
                    updateFuncName,
                    undefined,
                    [START_X.name, START_Y.name, EFFECTIVE_WIDTH.name, EFFECTIVE_HEIGHT.name],
                    [IR.INT_TYPE, IR.INT_TYPE, IR.INT_TYPE, IR.INT_TYPE],
                    IR.VOID_TYPE,
                    IR.block([
                        IR.declVars([
                            {name: END_X.name, type: IR.INT_TYPE, initialiser: OP.add(START_X, EFFECTIVE_WIDTH)},
                            {name: END_Y.name, type: IR.INT_TYPE, initialiser: OP.add(START_Y, EFFECTIVE_HEIGHT)},
                        ]),
                        IR.BLANK_LINE,
                        
                        IR.comment('recompute row states'),
                        IR.forRange(Y.name, START_Y, END_Y, IR.block([
                            IR.declVar(S.name, IR.INT_TYPE, IR.ternary(
                                OP.lt(END_X, g.width),
                                IR.access(rowStates, g.index(END_X, Y)),
                                IR.ZERO,
                            ), true),
                            IR.forRangeReverse(X.name, IR.ZERO, END_X, IR.block([
                                IR.declVars([
                                    {name: I.name, type: IR.INT_TYPE, initialiser: g.index(X, Y)},
                                    {name: OLD_S.name, type: IR.INT_TYPE, initialiser: IR.access(rowStates, I)},
                                ]),
                                IR.assign(S, '=', IR.access(
                                    rowDFA,
                                    OP.multAddConstant(S, _rowDFA.alphabetSize, g.access(I))
                                )),
                                IR.if_(
                                    OP.ne(S, OLD_S),
                                    IR.block([
                                        IR.assign(IR.access(rowStates, I), '=', S),
                                        IR.if_(OP.lt(X, START_X), IR.assign(START_X, '=', X)),
                                    ]),
                                    IR.if_(OP.lt(X, START_X), IR.BREAK),
                                ),
                            ])),
                        ])),
                        IR.BLANK_LINE,
                        
                        IR.comment('recompute col states'),
                        IR.forRange(X.name, START_X, END_X, IR.block([
                            IR.declVar(S.name, IR.INT_TYPE, IR.ternary(
                                OP.lt(END_Y, g.height),
                                IR.access(colStates, g.index(X, END_Y)),
                                IR.ZERO,
                            ), true),
                            IR.forRangeReverse(Y.name, IR.ZERO, END_Y, IR.block([
                                IR.declVars([
                                    {name: I.name, type: IR.INT_TYPE, initialiser: g.index(X, Y)},
                                    {name: OLD_S.name, type: IR.INT_TYPE, initialiser: IR.access(colStates, I)},
                                ]),
                                IR.assign(S, '=', IR.access(
                                    colDFA,
                                    OP.multAddConstant(S, _colDFA.alphabetSize, IR.access(rowAcceptSets, IR.access(rowStates, I))),
                                )),
                                IR.if_(
                                    OP.ne(S, OLD_S),
                                    IR.block([
                                        IR.assign(IR.access(colStates, I), '=', S),
                                        
                                        // update samplers
                                        IR.switch_(IR.access(colAcceptSets, OLD_S), makeStateChangeHandlers('del')),
                                        IR.switch_(IR.access(colAcceptSets, S), makeStateChangeHandlers('add')),
                                    ]),
                                    IR.if_(OP.lt(Y, START_Y), IR.BREAK),
                                ),
                            ])),
                        ])),
                    ]),
                ),
                IR.localCallStmt(updateFuncName, [IR.ZERO, IR.ZERO, g.width, g.height]),
                IR.BLANK_LINE,
            ];
        }
    }
    
    class CFlags {
        private vars: readonly IR.NameExpr[];
        constructor(private readonly numFlags: number) {
            const numVars = (numFlags + 31) >> 5;
            this.vars = makeArray(numVars, i => IR.name(`f${i}`));
        }
        
        private _var(i: number): IR.NameExpr {
            return this.vars[i >> 5];
        }
        private _bit(i: number): IR.Expr {
            return OP.lshift(IR.ONE, IR.int(i & 31));
        }
        declare(): IR.Stmt {
            const initialiser = this.numFlags === 1 ? IR.FALSE : IR.ZERO;
            return IR.declVars(this.vars.map(v => ({
                name: v.name,
                type: IR.INT_TYPE,
                initialiser,
            })), true);
        }
        set(i: number): IR.Stmt {
            const v = this._var(i);
            return this.numFlags === 1
                ? IR.assign(v, '=', IR.TRUE)
                : IR.assign(v, '|=', this._bit(i));
        }
        clear(i: number): IR.Stmt {
            const v =  this._var(i);
            return this.numFlags === 1
                ? IR.assign(v, '=', IR.FALSE)
                : IR.assign(v, '&=', OP.bitwiseNot(this._bit(i)));
        }
        check(i: number): IR.Expr {
            const v = this._var(i);
            return this.numFlags === 1
                ? v
                : OP.ne(OP.bitwiseAnd(this._var(i), this._bit(i)), IR.ZERO);
        }
    }
    
    class CLimits {
        private readonly vars: readonly IR.NameExpr[];
        private readonly checks: readonly IR.Expr[];
        private readonly decrements: readonly IR.Stmt[];
        
        constructor(private readonly limits: readonly ASG.FormalLimit[]) {
            const vars = this.vars = makeArray(limits.length, i => IR.name(`limit${i}`));
            this.checks = vars.map(v => OP.gt(v, IR.ZERO));
            this.decrements = vars.map(v => IR.assign(v, '-=', IR.ONE));
        }
        
        declare(c: {expr(e: ASG.Expression): IR.Expr}): IR.Stmt {
            const {vars} = this;
            return IR.declVars(this.limits.map((limit, i) => ({
                name: vars[i].name,
                type: IR.INT_TYPE,
                initialiser: limit.canReset ? undefined : c.expr(limit.initialiser),
            })), true);
        }
        reset(limitID: number, c: {expr(e: ASG.Expression): IR.Expr}): IR.Stmt {
            const limit = this.limits[limitID];
            if(!limit.canReset) { throw new Error(); }
            return IR.assign(this.vars[limitID], '=', c.expr(limit.initialiser));
        }
        check(limitID: number): IR.Expr {
            return this.checks[limitID];
        }
        decrement(limitID: number): IR.Stmt {
            return this.decrements[limitID];
        }
    }
    
    class CMask {
        private scale: number = 0;
        
        readonly name = IR.name(MASK);
        
        private maskN(length: IR.Expr): IR.Expr {
            return OP.divConstant(OP.add(length, IR.int(31)), 32);
        }
        use(g: CGrid) {
            this.scale = Math.max(this.scale, g.grid.scaleX * g.grid.scaleY);
        }
        declare(): IR.Stmt[] {
            if(this.scale === 0) { return []; }
            
            const arrayComponent = IR.access(this.name, OP.divConstant(I, 32));
            const bit = OP.lshift(IR.ONE, OP.modConstant(I, 32));
            return [
                IR.declVar(
                    this.name.name,
                    IR.INT32_ARRAY_TYPE,
                    IR.newInt32Array(this.maskN(OP.multConstant(OP.mult(WIDTH, HEIGHT), this.scale))),
                ),
                IR.declFunc(
                    MASK_CLEAR,
                    undefined,
                    [N.name],
                    [IR.INT_TYPE],
                    IR.VOID_TYPE,
                    IR.block([
                        IR.assign(N, '=', this.maskN(N)),
                        IR.forRange(I.name, IR.ZERO, N, IR.assign(IR.access(this.name, I), '=', IR.ZERO)),
                    ]),
                ),
                IR.declFunc(
                    MASK_SET,
                    undefined,
                    [G.name, I.name, S.name],
                    [IR.GRID_DATA_ARRAY_TYPE, IR.INT_TYPE, IR.BYTE_TYPE],
                    IR.VOID_TYPE,
                    IR.block([
                        IR.assign(IR.access(G, I), '=', S),
                        IR.assign(arrayComponent, '|=', bit),
                    ]),
                ),
                IR.declFunc(
                    MASK_HASNT,
                    undefined,
                    [I.name],
                    [IR.INT_TYPE],
                    IR.BOOL_TYPE,
                    IR.return_(OP.eq(OP.bitwiseAnd(arrayComponent, bit), IR.ZERO)),
                ),
                IR.BLANK_LINE,
            ];
        }
        clear(g: CGrid): IR.Stmt {
            return IR.localCallStmt(MASK_CLEAR, [g.n]);
        }
        set(g: CGrid, index: IR.Expr, colour: number): IR.Stmt {
            return IR.localCallStmt(MASK_SET, [g.data, index, IR.int(colour)]);
        }
        hasnt(index: IR.Expr): IR.Expr {
            return IR.localCall(MASK_HASNT, [index]);
        }
        patternFits(g: CGrid, patternExpr: ASG.Prop<'pattern.out'>): IR.Expr {
            return patternExpr.kind === 'expr.constant'
                ? patternExpr.constant.value.map((dx, dy) => this.hasnt(g.relativeIndex(dx, dy))).reduce(OP.and)
                : IR.libMethodCall('Pattern', 'fitsMask', P, [g.useObj(), this.name, AT_X, AT_Y]);
        }
    }
    
    class CMatches {
        private scale: number = 0;
        
        readonly array = IR.name(MATCHES);
        readonly count = IR.name(MATCH_COUNT);
        readonly getAtCount = this.get(this.count);
        readonly isNotEmpty = OP.gt(this.count, IR.ZERO);
        readonly incrementCount = IR.assign(this.count, '+=', IR.ONE);
        readonly decrementCount = IR.assign(this.count, '-=', IR.ONE);
        
        use(g: CGrid, k: number): void {
            // TODO: in principle, we can get a better estimate for the maximum number of matches if we know some patterns cannot overlap
            this.scale = Math.max(this.scale, g.grid.scaleX * g.grid.scaleY * k);
        }
        declare(): IR.Stmt {
            if(this.scale === 0) { return IR.PASS; }
            
            const n = OP.multConstant(OP.mult(WIDTH, HEIGHT), this.scale);
            return IR.declVar(this.array.name, IR.INT32_ARRAY_TYPE, IR.newInt32Array(n));
        }
        declareCount(initial: IR.Expr, mutable: boolean): IR.Stmt {
            return IR.declVar(this.count.name, IR.INT_TYPE, initial, mutable);
        }
        copyFrom(sampler: IR.NameExpr, shuffle: boolean): IR.Stmt {
            return shuffle
                ? IR.libMethodCallStmt('Sampler', 'shuffleInto', sampler, [this.array, RNG])
                : IR.libMethodCallStmt('Sampler', 'copyInto', sampler, [this.array]);
        }
        get(index: IR.Expr): IR.ArrayAccessExpr {
            return IR.access(this.array, index);
        }
        add(match: IR.Expr, shuffle: boolean): IR.Stmt {
            return IR.block(shuffle ? [
                IR.declVar(J.name, IR.INT_TYPE, IR.libMethodCall('PRNG', 'nextInt', RNG, [OP.add(this.count, IR.ONE)])),
                IR.assign(this.getAtCount, '=', this.get(J)),
                IR.assign(this.get(J), '=', match),
                this.incrementCount,
            ] : [
                IR.assign(this.getAtCount, '=', match),
                this.incrementCount,
            ]);
        }
        forEach(indexVar: IR.NameExpr, then: readonly IR.Stmt[]): IR.Stmt {
            return IR.forRange(indexVar.name, IR.ZERO, this.count, IR.block([
                IR.declVar(MATCH.name, IR.INT_TYPE, this.get(indexVar)),
                ...then,
            ]));
        }
    }
    
    class CSampler {
        readonly name: IR.NameExpr;
        readonly count: IR.Expr;
        readonly arr: IR.Expr;
        readonly isNotEmpty: IR.Expr;
        
        constructor(
            readonly id: number,
            readonly inGrid: CGrid,
            readonly numPatterns: number,
        ) {
            this.name = IR.name(`grid${inGrid.grid.id}_sampler${id}`);
            this.count = IR.attr(this.name, 'count');
            this.arr = IR.attr(this.name, 'arr');
            this.isNotEmpty = OP.gt(this.count, IR.ZERO);
        }
        
        declare(): IR.VarDeclWithInitialiser {
            return {
                name: this.name.name,
                type: IR.SAMPLER_TYPE,
                initialiser: IR.libConstructorCall('Sampler', [OP.multConstant(this.inGrid.n, this.numPatterns)]),
            };
        }
        
        get(index: IR.Expr): IR.Expr {
            return IR.access(this.arr, index);
        }
        sample(withReplacement: boolean): IR.Expr {
            return withReplacement
                ? this.get(IR.libMethodCall('PRNG', 'nextInt', RNG, [this.count]))
                : IR.libMethodCall('Sampler', 'sample', this.name, [this.count, RNG]);
        }
        forEach(indexVar: IR.NameExpr, then: readonly IR.Stmt[]): IR.Stmt {
            return IR.forRange(indexVar.name, IR.ZERO, this.count, IR.block(then));
        }
    }
    
    class CVariables {
        private readonly names: readonly IR.NameExpr[];
        
        constructor(private readonly variables: readonly ASG.FormalVariable[]) {
            const counts = new Map<string, number>();
            this.names = variables.map(v => {
                const count = counts.get(v.name) ?? 0;
                counts.set(v.name, count + 1);
                return IR.name(count > 0 ? `_${v.name}_${count}` : `_${v.name}`);
            });
        }
        
        declare(c: {type(t: Type.Type): IR.IRType, expr(e: ASG.Expression): IR.Expr}): readonly IR.Stmt[] {
            const {names} = this;
            // this also filters out compile-time constants, since the resolver folds them instead of referencing them
            return this.variables
                .filter(v => v.references > 0)
                .map(v => IR.declVar(
                    names[v.id].name,
                    c.type(v.type),
                    v.initialiser && c.expr(v.initialiser),
                    v.initialiser === undefined,
                ));
        }
        name(variableID: number): IR.NameExpr {
            return this.names[variableID];
        }
        type(variableID: number, c: {type(t: Type.Type): IR.IRType}): IR.IRType {
            return c.type(this.variables[variableID].type);
        }
    }
    
    /**
     * Compiles MJr source code to a high-level intermediate representation.
     */
    class Compiler {
        readonly diagnostics = new Diagnostics();
        
        private readonly cfg: CFG.CFG;
        private readonly stateIDs = IDMap.empty<number>();
        readonly opsUsed = new Set(ALWAYS_USED_OPS);
        private readonly internedLiterals = IDMap.withKey<{expr: IR.Expr, type: IR.IRType}>(entry => IR.key(entry.expr));
        
        readonly grids: readonly CGrid[];
        readonly mask = new CMask();
        private readonly flags: CFlags;
        private readonly limits: CLimits;
        readonly matches = new CMatches();
        readonly variables: CVariables;
        
        constructor(
            readonly asg: ASG.ASG,
            readonly config: Readonly<Config>,
        ) {
            this.cfg = CFG.build(asg.root, config.animate);
            this.grids = asg.grids.map(g => new CGrid(g));
            this.flags = new CFlags(this.cfg.numFlags);
            this.limits = new CLimits(asg.limits);
            this.variables = new CVariables(asg.variables);
        }
        
        private internLiteral(expr: IR.Expr, type: IR.IRType): IR.Expr {
            const id = this.internedLiterals.getOrCreateID({expr, type});
            return IR.name(`constant${id}`);
        }
        
        notSupported(s: string, pos: SourcePosition): void {
            this.diagnostics.compilationError(`${s} is not currently supported`, pos);
        }
        
        compile(): IR.Stmt {
            // YYYY-MM-DD HH:mm:ss GMT+offset
            const date = new Date().toLocaleString('en-SE', {timeZoneName: 'short'});
            
            const {params, endGridID} = this.asg;
            
            const switchCases: IR.Stmt[] = [];
            for(const node of this.cfg.nodes) {
                // only `stmt` and `reset` nodes can be jumped to
                if(node.kind !== 'stmt.branching' && node.kind !== 'stmt.nonbranching' && node.kind !== 'reset') { continue; }
                
                const thisState = this.stateIDs.getOrCreateID(node.id);
                // sanity check
                if(thisState !== switchCases.length) { throw new Error(); }
                
                switchCases.push(
                    node.kind === 'stmt.branching' ? this.compileBranchingStmtNode(node)
                    : node.kind === 'stmt.nonbranching' ? this.compileNonBranchingStmtNode(node)
                    : this.compileResetNode(node)
                );
            }
            
            // TODO: potentials
            
            const gridDecls: IR.Stmt[] = [];
            const gridUpdateDecls: IR.Stmt[] = [];
            const endGridObj = this.grids[endGridID].useObj();
            for(const g of this.grids) {
                if(g.grid.periodic) { this.notSupported('periodic grid', g.grid.pos); }
                
                gridDecls.push(...g.declareVars());
                gridUpdateDecls.push(...g.matcher.declareUpdateFunc());
            }
            
            const mainParams: string[] = [WIDTH.name, HEIGHT.name];
            const mainParamTypes: IR.IRType[] = [IR.INT_TYPE, IR.INT_TYPE];
            if(params.size > 0) {
                mainParams.push('params');
                mainParamTypes.push(IR.nullableType({
                    kind: 'dict',
                    keys: Array.from(params.keys()),
                    values: Array.from(params.values(), t => IR.nullableType(this.type(t))),
                }));
            }
            mainParams.push(RNG.name);
            mainParamTypes.push(IR.nullableType(IR.PRNG_TYPE));
            
            // need to compile everything before preamble, so that `this.opsUsed` is complete
            const matchesDecl = this.matches.declare(),
                maskDecl = this.mask.declare(),
                constDecls = IR.declVars(this.internedLiterals.map((s, i) => ({name: `constant${i}`, type: s.type, initialiser: s.expr}))),
                varDecls = this.variables.declare(this),
                flagDecls = this.flags.declare(),
                limitDecls = this.limits.declare(this);
            
            return IR.declFunc(this.config.entryPointName, this.config.animate ? IR.REWRITE_INFO_TYPE : undefined, mainParams, mainParamTypes, IR.GRID_TYPE, IR.block([
                IR.comment(`compiled by mjrc-${COMPILER_VERSION} on ${date}`),
                // TODO: compute and pass max width/height, to ensure no overflow of "loose" integer operations
                this.config.emitChecks ? IR.if_(OP.or(OP.le(WIDTH, IR.ZERO), OP.le(HEIGHT, IR.ZERO)), IR.throw_("Grid dimensions must be positive")) : IR.PASS,
                
                IR.preamble(this.dictType(params), this.config.emitChecks, REQUIRED_RUNTIME_LIB_VERSION, Array.from(this.opsUsed)),
                IR.BLANK_LINE,
                
                ...gridDecls,
                IR.BLANK_LINE,
                
                ...gridUpdateDecls,
                
                matchesDecl,
                ...maskDecl,
                constDecls,
                ...varDecls,
                flagDecls,
                limitDecls,
                ...this.goto(-1, 0),
                IR.while_(OP.ge(STATE, IR.ZERO), IR.switch_(STATE, switchCases)),
                IR.return_(endGridObj),
            ]));
        }
        
        private compileBranchingStmtNode(node: CFG.BranchingStmtNode): IR.Stmt {
            const {stmt} = node;
            
            const ifTrue = this.goto(node.id, node.ifChanged.nodeID),
                ifFalse = this.goto(node.id, node.then.nodeID),
                eitherWay: IR.Stmt[] = [];
            while(ifTrue.length > 0 && ifFalse.length > 0 && IR.equals(ifTrue[ifTrue.length - 1], ifFalse[ifFalse.length - 1])) {
                const s = ifTrue.pop()!;
                ifFalse.pop();
                eitherWay.push(s);
            }
            
            const f = STMT_COMPILE_FUNCS[stmt.kind];
            return IR.block([
                IR.comment(`${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                f(this, stmt as never, IR.block(ifTrue), IR.block(ifFalse)),
                ...eitherWay,
            ]);
        }
        
        private compileNonBranchingStmtNode(node: CFG.NonBranchingStmtNode): IR.Stmt {
            const {stmt} = node;
            const f = STMT_COMPILE_FUNCS[stmt.kind];
            return IR.block([
                IR.comment(`${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                f(this, stmt as never),
                ...this.goto(node.id, node.then.nodeID),
            ]);
        }
        
        private compileResetNode(node: CFG.ResetNode): IR.Stmt {
            const out: IR.Stmt[] = [IR.comment(`reset ${node.stmt.kind} at line ${node.stmt.pos.line}, col ${node.stmt.pos.col}`)];
            if(node.reset !== undefined) {
                const {limitIDs} = node.reset;
                out.push(...limitIDs.map(limitID => this.limits.reset(limitID, this)));
            }
            out.push(
                this.flags.clear(node.flagID),
                ...this.goto(node.id, node.then.nodeID),
            );
            return IR.block(out);
        }
        
        expr(expr: ASG.Expression): IR.Expr {
            const f = EXPR_COMPILE_FUNCS[expr.kind] as ExprCompileFunc<typeof expr['kind']>
            return f(this, expr);
        }
        
        literal(c: Type.ConstantValue): IR.Expr {
            switch(c.kind) {
                case 'bool': return c.value ? IR.TRUE : IR.FALSE;
                case 'float': return IR.float(c.value);
                case 'int': return IR.int(c.value);
                case 'str': return IR.str(c.value);
                
                case 'dict': {
                    const type = this.dictType(c.type.entryTypes);
                    const values = Array.from(type.keys, k => this.literal(c.value.get(k)!));
                    return this.internLiteral(IR.dict(type, values), type);
                }
                case 'fraction': {
                    const expr = OP.fraction(IR.int(c.value.p), IR.int(c.value.q));
                    this.opsUsed.add(expr.op);
                    return this.internLiteral(expr, this.type(c.type));
                }
                case 'grid': {
                    return this.grids[c.value].useObj();
                }
                case 'pattern': {
                    const {width, height, pattern} = c.value;
                    const patternExpr = IR.constArray(pattern, 256, width);
                    // TODO: if masks are needed, flatten them into one array
                    return this.internLiteral(IR.libConstructorCall('Pattern', [IR.int(width), IR.int(height), patternExpr]), this.type(c.type));
                }
                case 'position': {
                    const {x, y, inGrid} = c.value;
                    const g = this.grids[inGrid];
                    return this.internLiteral(g.checkedIndex(IR.int(x), IR.int(y)), IR.INT_TYPE);
                }
            }
        }
        
        type(type: Type.Type): IR.IRType {
            return type.kind === 'dict'
                ? this.dictType(type.entryTypes)
                : TYPES_TO_IR[type.kind];
        }
        dictType(entryTypes: ReadonlyMap<string, Type.Type>): IR.DictType {
            const keys = Array.from(entryTypes.keys()).sort();
            // TODO: declare type aliases for dict types
            return {kind: 'dict', keys, values: keys.map(k => this.type(entryTypes.get(k)!))};
        }
        
        goto(fromNodeID: number, toNodeID: number): IR.Stmt[] {
            const initial = fromNodeID < 0;
            
            const out: IR.Stmt[] = [];
            const {nodes} = this.cfg;
            let cur: CFG.Node | undefined;
            while(true) {
                cur = nodes[toNodeID];
                if(cur.kind === 'stmt.branching' || cur.kind === 'stmt.nonbranching' || (cur.kind === 'reset' && cur.reset !== undefined) || cur.kind === 'stop') {
                    break;
                } else if(cur.kind === 'checklimit') {
                    if(initial) { toNodeID = cur.ifTrue.nodeID; continue; }
                    break;
                } else if(cur.kind === 'checkflag' || cur.kind === 'reset') {
                    if(initial) { toNodeID = cur.then.nodeID; continue; }
                    break;
                } else if(cur.kind === 'decrementlimit') {
                    if(initial) { throw new Error(); }
                    out.push(this.limits.decrement(cur.limitID));
                } else if(cur.kind === 'setflag') {
                    if(initial) { throw new Error(); }
                    out.push(this.flags.set(cur.flagID));
                }
                toNodeID = cur.then.nodeID;
            }
            
            if(cur.kind === 'checkflag' || cur.kind === 'checklimit') {
                if(initial) { throw new Error(); }
                
                out.push(IR.if_(
                    cur.kind === 'checkflag' ? this.flags.check(cur.flagID) : this.limits.check(cur.limitID),
                    IR.block(this.goto(fromNodeID, cur.ifTrue.nodeID)),
                    IR.block(this.goto(fromNodeID, cur.then.nodeID)),
                ));
            } else {
                const nextState = IR.int(cur.kind === 'stop' ? -1 : this.stateIDs.getOrCreateID(toNodeID));
                if(initial) {
                    out.push(IR.declVar(STATE.name, IR.INT_TYPE, nextState, true));
                } else if(fromNodeID !== toNodeID) {
                    out.push(IR.assign(STATE, '=', nextState));
                }
            }
            return out;
        }
    }
    
    const TYPES_TO_IR: IRecord<Exclude<Type.Kind, 'dict'>, IR.IRType> = {
        bool: IR.BOOL_TYPE,
        float: IR.FLOAT_TYPE,
        fraction: IR.FRACTION_TYPE,
        grid: IR.GRID_TYPE,
        int: IR.INT_TYPE,
        pattern: IR.PATTERN_TYPE,
        position: IR.INT_TYPE,
        str: IR.STR_TYPE,
    };
    
    type ExprCompileFunc<K extends ASG.Expression['kind']> = (c: Compiler, expr: Extract<ASG.Expression, {readonly kind: K}>) => IR.Expr
    const EXPR_COMPILE_FUNCS: {readonly [K in ASG.Expression['kind']]: ExprCompileFunc<K>} = {
        'expr.attr.dict': (c, expr) => IR.attr(c.expr(expr.left), expr.attr),
        'expr.attr.grid': (c, expr) => c.grids[expr.grid][expr.attr],
        'expr.attr.position': (c, expr) => {
            const g = c.grids[expr.left.type.inGrid];
            
            // optimise common cases
            if(expr.left.kind === 'expr.name.keyword') {
                switch(expr.left.name) {
                    case 'at':
                        return {x: AT_X, y: AT_Y}[expr.attr];
                    case 'origin':
                        return {x: g.originX, y: g.originY}[expr.attr];
                }
            }
            
            const pos = c.expr(expr.left);
            switch(expr.attr) {
                case 'x': return OP.mod(pos, g.width);
                case 'y': return OP.floordiv(pos, g.width);
            }
            // exhaustivity check
            const _: never = expr.attr;
        },
        'expr.constant': (c, expr) => c.literal(expr.constant),
        'expr.count': (c, expr) => c.grids[expr.inGrid].makeCounter(expr.patterns),
        'expr.decl': (c, expr) => {
            // TODO: get rid of `expr.decl` in ASG and `expr.letin` in IR, by hoisting assign statements?
            const decls: IR.VarDeclWithInitialiser[] = [];
            let cur: ASG.Expression = expr;
            while(cur.kind === 'expr.decl') {
                const {variable, rhs} = expr.decl;
                if(variable.references > 0) {
                    decls.push({
                        name: c.variables.name(variable.id).name,
                        // need to pass the type, in case the code generator wants to use a lambda requiring a type annotation
                        type: c.variables.type(variable.id, c),
                        initialiser: c.expr(rhs),
                    });
                }
                cur = cur.child;
            }
            return IR.letIn(decls, c.expr(cur));
        },
        'expr.dict': (c, expr) => {
            const type = c.dictType(expr.type.entryTypes);
            return IR.dict(type, type.keys.map(k => c.expr(expr.entryExprs.get(k)!)));
        },
        'expr.name.keyword': (c, expr) => {
            switch(expr.name) {
                case 'at':
                    return AT;
                case 'origin':
                    if(expr.type.kind !== 'position') { throw new Error(); }
                    return c.grids[expr.type.inGrid].useOrigin();
                case 'random':
                    return IR.libMethodCall('PRNG', 'nextDouble', RNG, []);
            }
        },
        'expr.name.simple': (c, expr) => c.variables.name(expr.variableID),
        'expr.op.binary': (c, expr) => {
            const r = IR.binaryOp(expr.op, c.expr(expr.left), c.expr(expr.right));
            // `IR.binaryOp` may optimise e.g. `0 - x` to `-x`
            if(r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') { c.opsUsed.add(r.op); }
            return r;
        },
        'expr.op.ternary': (c, expr) => IR.ternary(c.expr(expr.condition), c.expr(expr.then), c.expr(expr.otherwise)),
        'expr.op.unary': (c, expr) => {
            const r = IR.unaryOp(expr.op, c.expr(expr.child));
            // `IR.unaryOp` may optimise e.g. `not x == y` to `x != y`
            if(r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') { c.opsUsed.add(r.op); }
            return r;
        },
        'expr.param': (c, expr) => IR.param(expr.name, c.expr(expr.otherwise)),
        'expr.randint': (c, expr) => {
            const max = c.expr(expr.max);
            return max.kind === 'expr.literal.int' && max.value > 0
                ? IR.libMethodCall('PRNG', 'nextInt', RNG, [max])
                : IR.libFunctionCall('nextIntChecked', [RNG, max]);
        },
        'expr.sum': (c, expr) => {
            c.notSupported(`'sum' expression`, expr.pos);
            return IR.NULL;
        },
    };
    
    type StmtCompileFunc<K extends ASG.Statement['kind']>
        = K extends ASG.BranchingStmt['kind']
        ? (c: Compiler, stmt: Extract<ASG.Statement, {readonly kind: K}>, ifChanged: IR.Stmt, then: IR.Stmt) => IR.Stmt
        : (c: Compiler, stmt: Extract<ASG.Statement, {readonly kind: K}>) => IR.Stmt
    const STMT_COMPILE_FUNCS: {readonly [K in (ASG.BranchingStmt | ASG.NonBranchingStmt)['kind']]: StmtCompileFunc<K>} = {
        // non-branching
        'stmt.assign': (c, stmt) => IR.assign(c.variables.name(stmt.variable.id), '=', c.expr(stmt.rhs)),
        'stmt.log': (c, stmt) => IR.log(c.expr(stmt.expr)),
        'stmt.rules.map': _stmtNotSupported,
        'stmt.put': (c, stmt) => {
            const g = c.grids[stmt.inGrid];
            const {pattern} = stmt;
            return IR.block([
                // TODO: check bounds, given size of pattern
                _declareAt(g, c.expr(stmt.at)),
                pattern.kind !== 'expr.constant' ? IR.declVar(P.name, IR.PATTERN_TYPE, c.expr(pattern)) : IR.PASS,
                IR.if_(
                    _writeCondition(c, g, pattern, P, stmt.uncertainties, stmt.condition),
                    _doWrite(c, g, undefined, pattern, false, undefined, c.config.animate),
                ),
            ]);
        },
        'stmt.use': (c, stmt) => {
            if(!c.config.animate) { throw new Error(); }
            return c.grids[stmt.grid].yield_();
        },
        
        // branching
        'stmt.convchain': _stmtNotSupported,
        'stmt.path': _stmtNotSupported,
        'stmt.rules.basic.all': _basicAllPrl,
        'stmt.rules.basic.one': _basicOne,
        'stmt.rules.basic.prl': _basicAllPrl,
        'stmt.rules.biased.all': _stmtNotSupported,
        'stmt.rules.biased.one': _stmtNotSupported,
        'stmt.rules.search.all': _stmtNotSupported,
        'stmt.rules.search.one': _stmtNotSupported,
    };
    
    function _stmtNotSupported(c: Compiler, stmt: ASG.Statement): IR.Stmt {
        c.notSupported(`'${stmt.kind}'`, stmt.pos);
        return IR.PASS;
    }
    
    function _declareAt(g: CGrid, at: IR.Expr): IR.Stmt {
        return IR.declVars([
            {name: AT.name, type: IR.INT_TYPE, initialiser: at},
            {name: AT_X.name, type: IR.INT_TYPE, initialiser: OP.mod(AT, g.width)},
            {name: AT_Y.name, type: IR.INT_TYPE, initialiser: OP.floordiv(AT, g.width)},
        ]);
    }
    
    function _doWrite(c: Compiler, outGrid: CGrid, from: Pattern | undefined, to: ASG.Prop<'pattern.out'>, useMask: boolean, flagVar: IR.NameExpr | undefined, doYield: boolean): IR.Stmt {
        const out: IR.Stmt[] = [];
        let mX: IR.Expr, mY: IR.Expr, eW: IR.Expr, eH: IR.Expr;
        
        if(to.kind === 'expr.constant') {
            // constant output pattern
            const {value} = to.constant;
            
            let minX = value.width, maxX = 0, minY = value.height, maxY = 0;
            value.forEach((dx, dy, colour) => {
                const ineffective = from !== undefined && from.pattern[dx + from.width * dy] === colour;
                if(useMask || !ineffective) {
                    out.push(outGrid.write(
                        outGrid.relativeIndex(dx, dy),
                        colour,
                        useMask ? c.mask : undefined,
                    ));
                }
                if(!ineffective) {
                    minX = Math.min(minX, dx);
                    maxX = Math.max(maxX, dx + 1);
                    minY = Math.min(minY, dy);
                    maxY = Math.max(maxY, dy + 1);
                }
            });
            
            if(minX > maxX || minY > maxY) { throw new Error(); }
            
            mX = IR.int(minX);
            mY = IR.int(minY);
            eW = IR.int(maxX - minX);
            eH = IR.int(maxY - minY);
        } else {
            // output pattern determined at runtime
            // this requires that `p` is already declared
            out.push(IR.libMethodCallStmt(
                'Pattern', 'put', P,
                [outGrid.useObj(), useMask ? c.mask.name : IR.NULL, AT_X, AT_Y],
            ));
            
            mX = IR.attr(P, 'minX');
            mY = IR.attr(P, 'minY');
            eW = IR.attr(P, 'effectiveWidth');
            eH = IR.attr(P, 'effectiveHeight');
        }
        
        out.push(...outGrid.update(OP.add(AT_X, mX), OP.add(AT_Y, mY), eW, eH, doYield));
        if(flagVar !== undefined) { out.push(IR.assign(flagVar, '=', IR.TRUE)); }
        return IR.block(out);
    }
    
    function _writeCondition(c: Compiler, g: CGrid, patternExpr: ASG.Prop<'pattern.out'> | undefined, patternVar: IR.NameExpr | undefined, uncertainties: readonly number[] | undefined, conditionExpr: ASG.Prop<'bool?'>): IR.Expr {
        let out: IR.Expr[] = [];
        if(patternExpr !== undefined) {
            if(patternExpr.kind !== 'expr.constant') {
                if(patternVar === undefined) { throw new Error(); }
                out.push(IR.libMethodCall('Pattern', 'hasEffect', patternVar, [g.useObj(), AT_X, AT_Y]));
            } else if(uncertainties !== undefined) {
                // uncertainties array should be non-empty, so `reduce` doesn't need an initial value
                if(uncertainties.length === 0) { throw new Error(); }
                
                const {pattern, width} = patternExpr.constant.value;
                const isEffective = uncertainties.map(i => OP.ne(
                    g.access(g.relativeIndex(i % width, (i / width) | 0)),
                    IR.int(pattern[i]),
                ));
                out.push(isEffective.reduce(OP.or));
            }
        }
        if(conditionExpr !== undefined) { out.push(c.expr(conditionExpr)); }
        return out.length > 0 ? out.reduce(OP.and) : IR.TRUE;
    }
    
    function _basicOne(c: Compiler, stmt: ASG.BasicRulesStmt, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
        const {rewrites} = stmt;
        const g = c.grids[stmt.inGrid];
        const k = rewrites.length;
        const sampler = g.makeSampler(rewrites.map(rule => rule.from));
        
        const writeConditions = rewrites.map(rule => _writeCondition(c, g, rule.to, P, rule.toUncertainties, rule.condition));
        
        // optimisation for common case: all rewrites are unconditional and definitely effective
        const allUnconditionalAndEffective = writeConditions.every(c => c === IR.TRUE);
        
        const randomMatch = IR.declVar(MATCH.name, IR.INT_TYPE, sampler.sample(allUnconditionalAndEffective));
        const switchWrites = IR.block([
            _declareAt(g, OP.divConstant(MATCH, k)),
            IR.switch_(
                OP.modConstant(MATCH, k),
                rewrites.map((rule, i) => IR.block([
                    rule.to.kind !== 'expr.constant' ? IR.declVar(P.name, IR.PATTERN_TYPE, c.expr(rule.to)) : IR.PASS,
                    IR.if_(
                        writeConditions[i],
                        _doWrite(c, g, rule.from, rule.to, false, allUnconditionalAndEffective ? undefined : ANY, c.config.animate),
                    ),
                ])),
            ),
        ]);
        
        return allUnconditionalAndEffective
            ? IR.if_(
                sampler.isNotEmpty,
                IR.block([randomMatch, switchWrites, ifChanged]),
                then,
            )
            : IR.block([
                c.matches.declareCount(sampler.count, true),
                IR.declVar(ANY.name, IR.BOOL_TYPE, IR.FALSE, true),
                IR.while_(
                    OP.and(c.matches.isNotEmpty, OP.not(ANY)),
                    IR.block([randomMatch, switchWrites, c.matches.decrementCount]),
                ),
                IR.if_(ANY, ifChanged, then),
            ]);
    }
    
    function _basicAllPrl(c: Compiler, stmt: ASG.BasicRulesStmt, ifChanged: IR.Stmt, then: IR.Stmt): IR.Stmt {
        const {rewrites} = stmt;
        const g = c.grids[stmt.inGrid];
        const k = rewrites.length;
        const out: IR.Stmt[] = [];
        
        const conditionIsSameEverywhere = rewrites.map(rule =>
            (rule.condition.flags & ExprFlags.SAME_EVERYWHERE) === ExprFlags.SAME_EVERYWHERE
            && rule.to.kind === 'expr.constant'
            && rule.toUncertainties === undefined
        );
        const outPatternIsConstant = rewrites.map(rule => rule.to.kind === 'expr.constant');
        const outPatternIsSameEverywhere = rewrites.map(rule => (rule.to.flags & ExprFlags.SAME_EVERYWHERE) === ExprFlags.SAME_EVERYWHERE);
        
        // TODO: if rule.to is grid-dependent and not same-everywhere, need to do rewrites on a temporary buffer then copy over afterwards
        const patternIsGridDependent = rewrites.map(rule => (rule.to.flags & ExprFlags.GRID_INDEPENDENT) === 0);
        rewrites.forEach((rule, i) => {
            if(patternIsGridDependent[i] && !outPatternIsSameEverywhere[i]) {
                c.notSupported('output pattern dependent on both grid state and match position', rule.pos);
            }
        });
        
        c.matches.use(g, k);
        const useMask = stmt.kind === 'stmt.rules.basic.all' && (!stmt.commutative || rewrites.some(rule => {
            if(rule.to.kind === 'expr.constant') {
                const {value} = rule.to.constant;
                return value.effectiveWidth > 1 || value.effectiveHeight > 1;
            } else {
                const {type} = rule.to;
                return type.width > 1 || type.height > 1;
            }
        }));
        if(useMask) { c.mask.use(g); }
        const shuffle = useMask || !stmt.commutative;
        
        out.push(IR.declVars(rewrites.flatMap((rule, i) =>
            !outPatternIsConstant[i] && outPatternIsSameEverywhere[i]
            ? [{name: `p${i}`, type: IR.PATTERN_TYPE, initialiser: c.expr(rule.to)}]
            : []
        )));
        
        const firstPassConditions = rewrites.map((rule, i) => _writeCondition(
            c,
            g,
            outPatternIsSameEverywhere[i] ? rule.to : undefined,
            IR.name(`p${i}`),
            outPatternIsSameEverywhere[i] ? rule.toUncertainties : undefined,
            rule.condition,
        ));
        const secondPassConditions = rewrites.map((rule, i) => _writeCondition(
            c,
            g,
            outPatternIsSameEverywhere[i] ? undefined : rule.to,
            P,
            outPatternIsSameEverywhere[i] ? undefined : rule.toUncertainties,
            undefined,
        ));
        
        // if any second-pass conditions do more than just check the mask, use a flag for whether any rewrites were done
        // but no flag needed if this statement isn't branching anyway
        const useFlag = (ifChanged.kind !== 'stmt.pass' || then.kind !== 'stmt.pass') && outPatternIsSameEverywhere.includes(false)
        
        // optimisation for common case: all rewrites are unconditional and definitely effective
        if(firstPassConditions.every(c => c === IR.TRUE)) {
            const sampler = g.makeSampler(rewrites.map(rule => rule.from));
            out.push(
                c.matches.copyFrom(sampler.name, shuffle),
                c.matches.declareCount(sampler.count, false),
            );
        } else {
            out.push(c.matches.declareCount(IR.ZERO, true));
            for(let i = 0; i < k; ++i) {
                const rule = rewrites[i];
                const sampler = g.makeSampler([rule.from]);
                const declareAt = _declareAt(g, sampler.get(I));
                const condition = firstPassConditions[i];
                const addMatch = c.matches.add(OP.multAddConstant(AT, k, IR.int(i)), shuffle);
                
                out.push(
                    // if condition is same-everywhere, then we only need to check it once for all matches of this rule
                    conditionIsSameEverywhere[i]
                    ? IR.if_(condition, sampler.forEach(I, [declareAt, addMatch]))
                    // otherwise, need to check the condition separately for each match
                    : sampler.forEach(I, [declareAt, IR.if_(condition, addMatch)])
                );
            }
        }
        
        const doWrites = rewrites.map((rule, i) => IR.block([
            outPatternIsConstant[i] ? IR.PASS : IR.declVar(
                P.name,
                IR.PATTERN_TYPE,
                // TODO: `c.expr(rule.to)` is only correct when `rule.to` is grid-independent (see above)
                outPatternIsSameEverywhere[i] ? IR.name(`p${i}`) : c.expr(rule.to),
            ),
            IR.if_(
                OP.and(
                    secondPassConditions[i],
                    useMask ? c.mask.patternFits(g, rule.to) : IR.TRUE,
                ),
                _doWrite(c, g, rule.from, rule.to, useMask, useFlag ? ANY : undefined, false),
            ),
        ]));
        
        if(c.config.animate) {
            ifChanged = IR.block([g.yield_(), ifChanged]);
        }
        
        out.push(
            useFlag ? IR.declVar(ANY.name, IR.BOOL_TYPE, IR.FALSE, true) : IR.PASS,
            IR.if_(
                c.matches.isNotEmpty,
                IR.block([
                    useMask ? c.mask.clear(g) : IR.PASS,
                    c.matches.forEach(I, [
                        _declareAt(g, OP.divConstant(MATCH, k)),
                        IR.switch_(OP.modConstant(MATCH, k), doWrites),
                    ]),
                    useFlag ? IR.PASS : ifChanged,
                ]),
                useFlag ? undefined : then,
            ),
            useFlag ? IR.if_(ANY, ifChanged, then) : IR.PASS,
        );
        return IR.block(out);
    }
    
    type CodeGenClass = new (config: Config) => CodeGen.Base
    export type CodeGenName = KeysMatching<typeof CodeGen, CodeGenClass>
    
    export function compile(src: string, targetLanguage: CodeGenName, config?: Partial<Config>): string {
        const ast = Parser.parse(src);
        const asg = Resolver.resolve(ast);
        
        const compiler = new Compiler(asg, config !== undefined ? {...DEFAULT_CONFIG, ...config} : DEFAULT_CONFIG);
        const result = compiler.compile();
        compiler.diagnostics.throwIfAnyErrors();
        
        const cls: CodeGenClass = CodeGen[targetLanguage];
        const gen = new cls(compiler.config);
        gen.writeStmt(result);
        
        gen.diagnostics.throwIfAnyErrors();
        return gen.render();
    }
}
