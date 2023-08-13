///<reference path="../runtime/mjr.ts"/>

namespace Resolver {
    const FRACTION_ONE = MJr.fraction(1, 1);
    
    type PropsKeys<K extends AST.Kind> = KeysMatching<AST.Node & {kind: K}, AST.Expression | undefined>
    
    /**
     * Specifies types and other static checks for properties of AST nodes.
     * A spec of '?' indicates that the property should not be present, but
     * enables some nodes to have the same property names and therefore be
     * resolved by the same functions.
     */
    const PROP_SPECS = (<T extends {[K in AST.Kind]?: {[J in PropsKeys<K>]?: ASG.PropSpec}}>(specs: T) => specs)({
        'expr.grid': {
            scaleX: 'const int?',
            scaleY: 'const int?',
            periodic: 'const bool?',
        },
        
        'rule.field': {
            for_: 'charset.in',
            on: 'charset.in',
            from: 'charset.in?',
            to: 'charset.in?',
            essential: 'const bool?',
            recompute: 'const bool?',
        },
        
        'stmt.convchain': {
            sample: 'const pattern.out',
            n: 'const int',
            temperature: 'float?',
            anneal: 'float?',
            on: 'const charset.in?',
            epsilon: 'const float?',
            periodic: 'const bool?',
        },
        'stmt.log': {
            expr: 'str~',
        },
        'stmt.path': {
            from: 'charset.in',
            to: 'charset.in',
            input: 'charset.in',
            output: 'charset.out',
            longest: 'bool?',
            inertia: 'bool?',
        },
        'stmt.put': {
            // the pattern and condition are rule contexts, but the position is not
            pattern: 'pattern.out',
            condition: 'bool?',
        },
        'stmt.rules.all': {
            temperature: 'float?',
            search: 'const bool?',
            maxStates: 'int?', 
            depthCoefficient: 'float?',
        },
        'stmt.rules.one': {
            temperature: 'float?',
            search: 'const bool?',
            maxStates: 'int?', 
            depthCoefficient: 'float?',
        },
    } as const);
    
    type PropTypesMap = typeof PROP_SPECS
    type ASTProps<K extends keyof PropTypesMap> = {[J in keyof PropTypesMap[K]]?: AST.Expression}
    type ResolvedProps<K extends keyof PropTypesMap> = {[J in keyof PropTypesMap[K]]: ASG.Prop<PropTypesMap[K][J] & ASG.PropSpec>}
    
    function _convPatternKey(p: ASG.ConvPattern): string {
        return `${Convolution.Kernel.key(p.kernel)}:${ISet.key(p.chars)}`;
    }
    
    type DeclResolveResult<T> = [decl: ASG.AssignStmt | undefined, result: T | undefined]
    type ExprResolveResult<K extends AST.Expression['kind']> = (
        K extends `expr.literal.${string}` ? ASG.ConstantExpr
        : K extends `expr.grid` ? ASG.ConstantExpr<'grid'>
        : ASG.Expression
    ) | undefined
    type RuleResolveResult = {assigns?: ASG.AssignStmt[], rules: ASG.Rule[]} | undefined
    type StmtResolveResult<K extends AST.Statement['kind']> = (
        K extends 'stmt.decl' | 'stmt.use.let' ? {kind: 'stmts', stmts: ASG.Statement[]}
        : {kind: 'stmt', assigns?: ASG.AssignStmt[], stmt: Exclude<ASG.Statement, ASG.AssignStmt>}
    ) | undefined
    
    type DeclResolveFunc<K extends AST.Declaration['kind']> = <T>(node: AST.Node & {kind: K}, ctx: Context, f: () => T) => DeclResolveResult<T>
    type ExprResolveFunc<K extends AST.Expression['kind']> = (node: AST.Node & {kind: K}, ctx: Context) => ExprResolveResult<K>
    type RuleResolveFunc<K extends AST.Rule['kind']> = (node: AST.Node & {kind: K}, ctx: GridContext, outGrid: FormalGrid) => RuleResolveResult
    type StmtResolveFunc<K extends AST.Statement['kind']> = (node: AST.Node & {kind: K}, ctx: Context) => StmtResolveResult<K>
    
    interface GlobalDeclarations extends Readonly<{
        grids: FormalGrid[],
        params: Map<string, Type.Type>,
        potentials: ASG.FormalPotential[],
        variables: FormalVariable[],
    }> {}
    type FormalGrid = ASG.FormalGrid & Readonly<{alphabet: Alphabet, convPatterns: IDMap<ASG.ConvPattern>}>
    type FormalVariable = ASG.FormalVariable & {references: number}
    
    class Alphabet {
        public readonly map: IDMap<string>;
        public readonly charsets = new Map<string, ISet>();
        public readonly wildcard: ISet;
        public legend: Pattern | undefined = undefined;
        
        constructor(
            public readonly key: string,
        ) {
            this.map = IDMap.of(key);
            
            const n = key.length;
            const {charsets} = this;
            for(let i = 0; i < n; ++i) {
                charsets.set(key[i], ISet.of(n, [i]));
            }
            charsets.set('.', this.wildcard = ISet.full(n));
        }
        
        withUnion<T>(label: string, set: ISet, f: () => T): T {
            const {charsets} = this;
            
            const oldSet = charsets.get(label);
            charsets.set(label, set);
            const result = f();
            if(oldSet !== undefined) { charsets.set(label, oldSet); } else { charsets.delete(label); }
            return result;
        }
    }
    
    interface GridContext extends Context {
        grid: FormalGrid;
    }
    
    class Context {
        public readonly diagnostics = new Diagnostics();
        public readonly globals: GlobalDeclarations = {
            grids: [],
            params: new Map(),
            potentials: [],
            variables: [],
        };
        private readonly uniqueVariableNames = new Set<string>();
        
        public symmetryName: Symmetry.SymmetryName = 'all';
        public readonly variables = new Map<string, FormalVariable>();
        public readonly errorVariables = new Set<string>();
        public kernel: Convolution.Kernel | undefined = undefined;
        public boundaryMask: ISet | undefined = undefined;
        
        public grid: FormalGrid | undefined = undefined;
        public inputPattern: Readonly<{width: number, height: number}> | undefined = undefined;
        public isRuleContext = false;
        public rewriteScaleX = FRACTION_ONE;
        public rewriteScaleY = FRACTION_ONE;
        
        resolveRoot(root: AST.CompilationUnit): ASG.SequenceStmt {
            const children = this.resolveStmts(root.stmts);
            return {kind: 'stmt.block.sequence', children, pos: root.pos};
        }
        
        resolveDecl<K extends AST.Declaration['kind'], T>(node: AST.Declaration & {kind: K}, callback: () => T): DeclResolveResult<T> {
            const f = DECL_RESOLVE_FUNCS[node.kind] as DeclResolveFunc<K>;
            return f(node, this, callback);
        }
        
        resolveExpr<K extends AST.Expression['kind']>(node: AST.Expression & {kind: K}): ExprResolveResult<K> {
            const f = EXPR_RESOLVE_FUNCS[node.kind] as ExprResolveFunc<K>;
            return f(node, this);
        }
        
        resolveRule<K extends AST.Rule['kind']>(node: AST.Rule & {kind: K}, outGrid: FormalGrid): RuleResolveResult {
            if(!this.expectGrid(node.pos)) { return undefined; }
            
            const f = RULE_RESOLVE_FUNCS[node.kind] as RuleResolveFunc<K>;
            return f(node, this, outGrid);
        }
        
        resolveStmt<K extends AST.Statement['kind']>(node: AST.Statement & {kind: K}): StmtResolveResult<K> {
            const f = STMT_RESOLVE_FUNCS[node.kind] as StmtResolveFunc<K>;
            return f(node, this);
        }
        
        resolveChar(c: AST.Char): ISet | undefined {
            const grid = this.grid ?? fail();
            const set = grid.alphabet.charsets.get(c.s);
            if(set === undefined) { this.error(`'${c.s}' is not an alphabet symbol or union label`, c.pos); }
            return set;
        }
        
        resolveCharSet(charset: AST.CharSet): ISet | undefined {
            const grid = this.grid ?? fail();
            const k = grid.alphabet.key.length;
            
            const mask = charset.inverted ? ISet.full(k) : ISet.empty(k);
            for(const c of charset.chars) {
                const cMask = this.resolveChar(c);
                if(cMask === undefined) {
                    return undefined;
                } else if(charset.inverted) {
                    ISet.removeAll(mask, cMask);
                } else {
                    ISet.addAll(mask, cMask);
                }
            }
            return mask;
        }
        
        resolveStmts(children: readonly AST.Statement[]): ASG.Statement[] {
            return children.flatMap(c => {
                const r = this.resolveStmt(c);
                return r === undefined ? []
                    : r.kind === 'stmts' ? r.stmts
                    : r.assigns !== undefined ? [...r.assigns, r.stmt]
                    : [r.stmt];
            });
        }
        
        expectGrid(pos: SourcePosition): this is GridContext {
            if(this.grid !== undefined) {
                return true;
            } else {
                this.error(this.globals.grids.length === 0 ? `no grid declared` : `no grid in use`, pos);
                return false;
            }
        }
        
        checkType(expected: Type.InternalType, expr: ASG.Expression): boolean {
            if(Type.isSubtype(expr.type, expected)) {
                return true;
            } else {
                this.typeError((expected.kind === 'union' ? expected.options : [expected]).map(Type.toStr), expr);
                return false;
            }
        }
        
        error(msg: string, pos: SourcePosition): void {
            this.diagnostics.compilationError(msg, pos);
        }
        
        typeError(expected: string[], expr: ASG.Expression): void {
            this.error(`expected ${quoteJoin(expected, ' | ')}, was '${Type.toStr(expr.type)}'`, expr.pos);
        }
        
        makeVariable(name: string, type: Type.Type, flags: ASG.ExprFlags, initialiser: ASG.Expression | undefined, isParam: boolean, pos: SourcePosition): FormalVariable {
            if(this.variables.has(name) || this.errorVariables.has(name)) {
                this.error(`cannot redeclare name '${name}'`, pos);
            } else if(isParam && this.globals.params.has(name)) {
                this.error(`cannot redeclare parameter '${name}'`, pos);
            }
            
            const {uniqueVariableNames} = this;
            let uniqueName = name, i = 1;
            while(uniqueVariableNames.has(uniqueName)) {
                uniqueName = `${name}_${++i}`;
            }
            uniqueVariableNames.add(uniqueName);
            
            if(isParam) { this.globals.params.set(name, type); }
            flags |= ASG.ExprFlags.LOCALLY_DETERMINISTIC | ASG.ExprFlags.POSITION_INDEPENDENT | ASG.ExprFlags.GRID_INDEPENDENT;
            return withNextID(this.globals.variables, {name, uniqueName, type, initialiser, flags, references: 0});
        }
        
        withOutGrid<T>(outGrid: FormalGrid, inputPatternPos: SourcePosition, f: () => T): T | undefined {
            const {grid: inGrid, inputPattern} = this;
            if(inGrid === undefined || inputPattern === undefined) { fail(); }
            
            if(outGrid.id === inGrid.id) { return f(); }
            
            // if scale is not positive, `makeGrid` already reported the error
            if(inGrid.scaleX <= 0 || inGrid.scaleY <= 0 || outGrid.scaleX <= 0 || outGrid.scaleY <= 0) { return undefined; }
            const scaleX = MJr.fraction(outGrid.scaleX, inGrid.scaleX);
            const scaleY = MJr.fraction(outGrid.scaleY, inGrid.scaleY);
            
            if((inputPattern.width * scaleX.p) % scaleX.q !== 0 || (inputPattern.height * scaleY.p) % scaleY.q !== 0) {
                this.error(`input pattern size must be a multiple of ${scaleX.q}x${scaleY.q}, was ${inputPattern.width}x${inputPattern.height}`, inputPatternPos);
                return undefined;
            }
            
            this.grid = outGrid; this.rewriteScaleX = scaleX; this.rewriteScaleY = scaleY;
            const result = f();
            this.grid = inGrid; this.rewriteScaleX = this.rewriteScaleY = FRACTION_ONE;
            return result;
        }
        
        withLegend<T>(decl: AST.LegendDecl, f: () => T): T | undefined {
            if(!this.expectGrid(decl.pos)) { return undefined; }
            
            const {alphabet} = this.grid;
            const oldLegend = alphabet.legend;
            if(oldLegend !== undefined) { this.error(`'legend' already declared`, decl.pos); }
            
            const legend = _resolvePatternLiteralExpr(decl.expr, this);
            if(legend === undefined) { return f(); }
            if(legend.height !== 1) { this.error(`'legend' pattern cannot have multiple rows`, decl.expr.pos); }
            
            alphabet.legend = legend;
            const result = f();
            alphabet.legend = oldLegend;
            return result;
        }
        
        withKernel<T>(stmt: AST.ConvolutionStmt, f: (kernel: Convolution.Kernel) => T): T | undefined {
            if(this.kernel !== undefined) { fail(); }
            
            const kernel = _resolveProp(stmt, 'kernel', 'const str', this);
            if(objHasKey(Convolution.KERNELS, kernel)) {
                const k = this.kernel = Convolution.KERNELS[kernel];
                const result = f(k);
                this.kernel = undefined;
                return result;
            } else {
                // this also handles PROP_ERROR
                this.error(`convolution kernel must be one of ${quoteJoin(Object.keys(Convolution.KERNELS))}`, stmt.kernel.pos);
                return undefined;
            }
        }
        
        withSymmetry<T>(decl: AST.SymmetryDecl, f: () => T): T {
            const symmetryName = _resolveProp(decl, 'expr', 'const str', this);
            if(objHasKey(Symmetry.SYMMETRY_GROUPS, symmetryName)) {
                const oldSymmetryName = this.symmetryName;
                this.symmetryName = symmetryName;
                const result = f();
                this.symmetryName = oldSymmetryName;
                return result;
            } else {
                // this also handles PROP_ERROR
                this.error(`symmetry group must be one of ${quoteJoin(Object.keys(Symmetry.SYMMETRY_GROUPS))}`, decl.expr.pos);
                return f();
            }
        }
        
        withUnion<T>(decl: AST.UnionDecl, f: () => T): T | undefined {
            if(!this.expectGrid(decl.pos)) { return undefined; }
            const {alphabet} = this.grid;
            
            if(decl.label.width !== 1 || decl.label.height !== 1) { this.error(`union label must be a single character`, decl.label.pos); return f(); }
            const label = decl.label.value[0];
            if(label.kind === 'CHARSET') { this.error(`union label cannot be a charset`, label.pos); return f(); }
            
            if(alphabet.map.has(label.s)) { this.error(`alphabet symbol '${label.s}' cannot be union label`, decl.label.pos); return f(); }
            if(alphabet.charsets.has(label.s)) { this.error(`union label '${label.s}' already declared`, decl.label.pos); }
            
            const charset = _resolveProp(decl, 'chars', 'const charset.in', this);
            if(charset === PROP_ERROR) { return undefined; }
            
            switch(charset.kind) {
                case 'leaf':
                case 'top':
                    return alphabet.withUnion(label.s, charset.masks[0], f);
                case 'bottom':
                    this.error(`empty charset`, decl.chars.pos); 
                    return undefined;
                default:
                    this.error(`union must be a constant charset`, decl.chars.pos); 
                    return undefined;
            }
        }
        
        withVariable<T>(variable: ASG.FormalVariable, f: () => T): T {
            const {variables} = this;
            
            const {name} = variable;
            const oldVariable = variables.get(name);
            variables.set(name, variable);
            const result = f();
            if(oldVariable !== undefined) {
                variables.set(name, oldVariable);
            } else {
                variables.delete(name);
            }
            return result;
        }
        
        withErrorVariable<T>(name: string, f: () => T): T {
            const {errorVariables} = this;
            const alreadyHad = errorVariables.has(name);
            errorVariables.add(name);
            const result = f();
            if(!alreadyHad) { errorVariables.delete(name); }
            return result;
        }
    }
    
    // helper functions
    
    function _makeConstantValue<K extends Type.Kind>(type: Type.OfKind<K> & {readonly kind: K}, value: Type.Value<K>): Type.ConstantValue<K>;
    function _makeConstantValue(type: Type.Type, value: Type.Value | undefined): Type.ConstantValue | undefined;
    function _makeConstantValue(type: Type.Type, value: Type.Value | undefined): {kind: Type.Kind, type: Type.Type, value: Type.Value} | undefined {
        return value !== undefined ? {kind: type.kind, type, value} : undefined;
    }
    function _makeConstantExpr<K extends Type.Kind>(type: Type.OfKind<K> & {kind: K}, value: Type.Value<K>, pos: SourcePosition): ASG.ConstantExpr<K> {
        return {kind: 'expr.constant', type, constant: _makeConstantValue(type, value), flags: ASG.ExprFlags.CONSTANT, pos} as ASG.ConstantExpr<K>;
    }
    function _coerceFromInt(expr: ASG.Expression, type: Type.OfKind<'float' | 'fraction'>): ASG.Expression {
        const op = expr.type.kind === 'int'
            ? `int_to_${type.kind}` as const
            : fail();
        const f = Op.UNARY_FUNCS[op] ?? fail();
        if(expr.kind === 'expr.constant') {
            const value = f(expr.constant.value as number);
            if(value !== undefined) { return _makeConstantExpr(type, value, expr.pos); }
        }
        return {kind: 'expr.op.unary', type, op, child: expr, flags: expr.flags, pos: expr.pos};
    }
    function _coerceToStr(expr: ASG.Expression): ASG.Expression {
        switch(expr.type.kind) {
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int': {
                const op = `${expr.type.kind}_to_str` as const;
                const f = Op.UNARY_FUNCS[op];
                if(expr.kind === 'expr.constant' && f !== undefined) {
                    const value = f(expr.constant.value as never);
                    if(value !== undefined) { return _makeConstantExpr(Type.STR, value, expr.pos); }
                }
                return {kind: 'expr.op.unary', type: Type.STR, flags: expr.flags, op, child: expr, pos: expr.pos};
            }
            default:
                return expr;
        }
    }
    
    function _parsePropTypeSpec(typeSpec: ASG.PropTypeSpec, ctx: Context): Type.InternalType {
        switch(typeSpec) {
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int':
            case 'str':
                return Type.PRIMITIVES[typeSpec];
            
            case 'str~':
                return Type.STR;
            case 'dict':
                return Type.ANY_DICT;
            case 'object':
                return Type.OBJECT;
        }
        
        const grid = ctx.grid ?? fail();
        const alphabetKey = grid.alphabet.key;
        
        switch(typeSpec) {
            case 'charset.in':
                return {kind: 'pattern.in', width: 1, height: 1, alphabetKey};
            case 'charset.out':
                return {kind: 'pattern.out', width: 1, height: 1, alphabetKey};
            case 'position':
                return {kind: 'position', inGrid: grid.id};
            
            case 'pattern.in':
                const {inputPattern} = ctx;
                if(inputPattern === undefined) {
                    return {kind: 'any_pattern', alphabetKey, allowUnions: true};
                } else {
                    const {width, height} = inputPattern;
                    return {kind: 'pattern.in', alphabetKey, width, height};
                }
            case 'pattern.out': {
                const {inputPattern, rewriteScaleX, rewriteScaleY} = ctx;
                if(inputPattern === undefined) {
                    return {kind: 'any_pattern', alphabetKey, allowUnions: false};
                } else {
                    const w = MJr.fraction(inputPattern.width * rewriteScaleX.p, rewriteScaleX.q);
                    const h = MJr.fraction(inputPattern.height * rewriteScaleY.p, rewriteScaleY.q);
                    
                    // ensure integer results
                    if(w.q !== 1 || h.q !== 1) { fail(); }
                    
                    return {kind: 'pattern.out', alphabetKey, width: w.p, height: h.p};
                }
            }
        }
    }
    function _parsePropSpec(spec: ASG.PropSpec, ctx: Context): {isConst: boolean, expectedType: Type.InternalType, coerceToStr: boolean, isRequired: boolean} {
        const isConst = spec.startsWith('const ');
        const typeSpec = spec.replace(/^const /, '').replace(/\?$/, '') as ASG.PropTypeSpec;
        const expectedType = _parsePropTypeSpec(typeSpec, ctx);
        const coerceToStr = typeSpec === 'str~';
        const isRequired = !spec.endsWith('?');
        
        return {isConst, expectedType, coerceToStr, isRequired};
    }
    
    /**
     * Sentinel value indicating that an error occurred when resolving a prop.
     * The value `undefined` cannot be used for this purpose, as it is valid
     * for an optional const prop to be `undefined`.
     */
    const PROP_ERROR = Symbol();
    function _resolveProp<T extends AST.Node, K extends string & keyof T, S extends ASG.PropSpec>(node: T & {[J in K]?: AST.Expression}, propName: K, spec: S, ctx: Context): ASG.Prop<S> | typeof PROP_ERROR {
        const ast: AST.Expression | undefined = node[propName];
        const {isConst, expectedType, coerceToStr, isRequired} = _parsePropSpec(spec, ctx);
        
        if(ast === undefined && isRequired) { fail(); }
        
        let resolved = ast && ctx.resolveExpr(ast);
        if(resolved === undefined) { return isRequired ? PROP_ERROR : undefined as ASG.Prop<S>; }
        
        if(resolved.type.kind === 'int' && (expectedType.kind === 'float' || expectedType.kind === 'fraction')) {
            resolved = _coerceFromInt(resolved, expectedType);
        } else if(coerceToStr) {
            resolved = _coerceToStr(resolved);
        }
        
        if(!ctx.checkType(expectedType, resolved)) {
            return PROP_ERROR;
        } else if(!isConst) {
            return resolved as ASG.Prop<S>;
        } else if(resolved.kind === 'expr.constant') {
            return resolved.constant.value as ASG.Prop<S>;
        } else {
            ctx.error(`expected compile-time constant`, resolved.pos);
            return PROP_ERROR;
        }
    }
    function _resolveProps<T extends AST.Node, K extends keyof PropTypesMap>(node: T & {kind: K} & ASTProps<K>, ctx: Context): ResolvedProps<K> | undefined {
        const specs = PROP_SPECS[node.kind];
        const props: Record<string, unknown> = {};
        let allOK = true;
        for(const [propName, spec] of Object.entries(specs)) {
            const resolved = _resolveProp(node, propName as string & keyof PropTypesMap[K], spec, ctx);
            if(resolved === PROP_ERROR) {
                allOK = false;
            } else {
                props[propName] = resolved;
            }
        }
        return allOK ? props as ResolvedProps<K> : undefined;
    }
    
    // check AST instead of resolved, because we want to allow rules with constant false conditions
    function _hasAtLeastOneRewriteRule(rules: readonly AST.Rule[]): boolean {
        return rules.some(rule =>
            rule.kind === 'rule.rewrite'
            || (rule.kind === 'rule.decl' && _hasAtLeastOneRewriteRule(rule.children))
        );
    }
    
    // TODO: check that there aren't multiple `fields` for the same color? or can this be allowed?
    type RuleArrays = {assigns: ASG.AssignStmt[], rewrites: ASG.RewriteRule[], fields: ASG.FieldRule[], observations: ASG.ObserveRule[]}
    function _resolveRules(node: AST.RuleBlockStmt, ctx: GridContext, outGrid: FormalGrid, allowFieldObserve: true): RuleArrays;
    function _resolveRules(node: AST.RuleBlockStmt, ctx: GridContext, outGrid: FormalGrid, allowFieldObserve: false): Pick<RuleArrays, 'rewrites' | 'assigns'>;
    function _resolveRules(node: AST.RuleBlockStmt, ctx: GridContext, outGrid: FormalGrid, allowFieldObserve: boolean): Partial<RuleArrays> {
        const out: Pick<RuleArrays, 'rewrites'> & Partial<RuleArrays> = {
            assigns: [],
            rewrites: [],
            fields: allowFieldObserve ? [] : undefined,
            observations: allowFieldObserve ? [] : undefined,
        };
        
        if(!_hasAtLeastOneRewriteRule(node.rules)) {
            ctx.error(`'${node.kind}' block must have at least one rewrite rule`, node.pos);
        }
        
        for(const rule of node.rules) {
            const r = ctx.resolveRule(rule, outGrid);
            if(r === undefined) { continue; }
            
            if(r.assigns !== undefined) {
                (out.assigns ??= []).push(...r.assigns);
            }
            
            for(const resolved of r.rules) {
                const arr: ASG.Rule[] | undefined
                    = resolved.kind === 'rule.field' ? out.fields
                    : resolved.kind === 'rule.observe' ? out.observations
                    : resolved.kind === 'rule.rewrite' ? out.rewrites
                    : undefined;
                if(arr !== undefined) {
                    arr.push(resolved);
                } else {
                    ctx.error(`'${resolved.kind}' not allowed in '${node.kind}' block`, resolved.pos);
                }
            }
        }
        
        // sorting makes it more likely that samplers can be reused
        out.rewrites.sort((a, b) => _cmpPatternKey(a.from, b.from));
        out.observations?.sort((a, b) => _cmpPatternKey(a.from, b.from));
        return out;
    }
    function _cmpPatternKey(a: PatternTree, b: PatternTree): number {
        const aKey = PatternTree.key(a), bKey = PatternTree.key(b);
        return aKey < bKey ? -1 : aKey === bKey ? 0 : 1;
    }
    
    /**
     * Conservatively estimates whether the given rewrite rules commute.
     */
    function _rewritesCommute(rewrites: readonly ASG.RewriteRule[]): boolean {
        if(rewrites.length === 0) { return true; }
        
        const mapping = emptyArray(rewrites[0].to.type.alphabetKey.length, -1);
        
        return rewrites.every(({from, to}) =>
            to.kind === 'expr.constant' && from.kind === 'leaf' && to.constant.value.every((x, y, toColour) => ISet.every(
                from.masks[x + y * from.width],
                fromColour => {
                    if(mapping[fromColour] >= 0 && mapping[fromColour] !== toColour) { return false; }
                    mapping[fromColour] = toColour;
                    return true;
                },
            )
        ));
    }
    
    // resolver functions which can handle multiple kinds of node
    
    function _resolveSimpleLiteralExpr(expr: Exclude<AST.LiteralExpr, AST.PatternLiteralExpr>): ASG.ConstantExpr {
        const kind = expr.kind.replace(/^expr\.literal\./, '') as Type.PrimitiveKind;
        return _makeConstantExpr(Type.PRIMITIVES[kind], expr.value, expr.pos);
    }
    function _resolvePatternLiteralExpr(expr: AST.PatternLiteralExpr, ctx: Context): Pattern | undefined {
        if(!ctx.expectGrid(expr.pos)) { return undefined; }
        const {alphabet} = ctx.grid;
        
        const {width, height, value} = expr;
        const pattern: number[] = [];
        const masks: ISet[] = [];
        
        let ok = true, hasUnions = false;
        for(const c of value) {
            let charID = -1, isUnion = false;
            let mask: ISet | undefined;
            if(c.kind === 'CHARSET') {
                mask = ctx.resolveCharSet(c);
                if(mask !== undefined) {
                    const size = ISet.size(mask);
                    if(size === 0) {
                        ctx.error('empty charset', c.pos);
                        ok = false;
                    } else if(size === 1) {
                        charID = ISet.first(mask);
                    } else {
                        isUnion = size < alphabet.key.length;
                    }
                }
            } else {
                mask = ctx.resolveChar(c);
                if(mask !== undefined) {
                    charID = alphabet.map.getIDOrDefault(c.s);
                    isUnion = charID < 0 && c.s !== '.';
                }
            }
            
            if(mask !== undefined) {
                pattern.push(charID >= 0 ? charID : isUnion ? PatternValue.UNION : PatternValue.WILDCARD);
                masks.push(mask);
                hasUnions ||= isUnion;
            } else {
                ok = false;
            }
        }
        
        return ok ? new Pattern(width, height, alphabet.key, pattern, masks, hasUnions) : undefined;
    }
    
    function _resolveCountExpr(expr: AST.UnaryOpExpr, ctx: Context): ASG.CountExpr | undefined {
        const {pos} = expr;
        if(!ctx.expectGrid(pos)) { return undefined; }
        
        const pattern = _resolveProp(expr, 'child', 'const pattern.in', ctx);
        if(pattern === PROP_ERROR) { return undefined; }
        
        const patterns = Symmetry.generate(pattern, ctx.symmetryName, PatternTree.rotate, PatternTree.reflect, PatternTree.key);
        
        // sorting makes it more likely that samplers can be reused
        patterns.sort(_cmpPatternKey);
        
        const inGrid = ctx.grid.id;
        const flags = ASG.ExprFlags.DETERMINISTIC | ASG.ExprFlags.LOCALLY_DETERMINISTIC | ASG.ExprFlags.POSITION_INDEPENDENT;
        return {kind: 'expr.count', type: Type.INT, flags, inGrid, patterns, pos};
    }
    function _resolveLoadExpr(expr: AST.UnaryOpExpr, ctx: Context): ASG.Expression | undefined {
        if(!ctx.expectGrid(expr.pos)) { return undefined; }
        
        const {legend, key: alphabetKey} = ctx.grid.alphabet;
        if(legend === undefined) { ctx.error(`missing 'legend' declaration`, expr.pos); return undefined; }
        
        const path = _resolveProp(expr, 'child', 'const str', ctx);
        if(path === PROP_ERROR) { return undefined; }
        
        const width = -1, height = -1, hasUnions = legend.hasUnions;
        
        const type: Type.Type = {kind: hasUnions ? 'pattern.in' : 'pattern.out', alphabetKey, width, height};
        // TODO
        ctx.error(`'load' expression is currently unsupported`, expr.pos);
        return undefined;
    }
    function _resolveRandIntExpr(expr: AST.UnaryOpExpr, ctx: Context): ASG.Expression | undefined {
        const {pos} = expr;
        const max = _resolveProp(expr, 'child', 'int', ctx);
        if(max === PROP_ERROR) { return undefined; }
        if(max.kind === 'expr.constant') {
            if(max.constant.value <= 0) { ctx.error(MJr.SAMPLE_EMPTY_MESSAGE, pos); return undefined; }
            if(max.constant.value === 1) { return _makeConstantExpr(Type.INT, 0, expr.pos); }
        }
        
        const flags = ASG.ExprFlags.GRID_INDEPENDENT | ASG.ExprFlags.POSITION_INDEPENDENT;
        return {kind: 'expr.randint', type: Type.INT, flags, max, pos};
    }
    function _resolveSumExpr(expr: AST.UnaryOpExpr, ctx: Context): ASG.Expression | undefined {
        const {pos} = expr;
        if(!ctx.expectGrid(expr.pos)) { return undefined; }
        
        const {kernel, grid} = ctx;
        if(kernel === undefined) { ctx.error(`'sum' expression may only be used 'convolution' statement`, pos); return undefined; }
        if(!ctx.isRuleContext) { ctx.error(`'sum' expression may only be used in a rule condition or output pattern`, pos); return undefined; }
        
        const charsPattern = _resolveProp(expr, 'child', 'const charset.in', ctx);
        if(charsPattern === PROP_ERROR) { return undefined; }
        
        switch(charsPattern.kind) {
            case 'bottom': {
                return _makeConstantExpr(Type.INT, 0, pos);
            }
            case 'top':
            case 'leaf': {
                const chars = charsPattern.masks[0];
                const includeBoundary = ctx.boundaryMask !== undefined && !ISet.isDisjoint(ctx.boundaryMask, chars);
                if(charsPattern.kind === 'top' && includeBoundary) {
                    return _makeConstantExpr(Type.INT, kernel.total(), pos);
                }
                
                const patternID = grid.convPatterns.getOrCreateID({chars, kernel, includeBoundary});
                const flags = ASG.ExprFlags.DETERMINISTIC | ASG.ExprFlags.LOCALLY_DETERMINISTIC;
                return {kind: 'expr.sum', type: Type.INT, flags, inGrid: grid.id, patternID, pos};
            }
            default: {
                // logical ops on const 1x1 patterns should already be folded
                fail();
            }
        }
    }
    function _checkZero(ctx: Context, child: ASG.Expression): ASG.Expression | undefined {
        if(child.kind !== 'expr.constant') {
            const type = child.type as Type.OfKind<'float' | 'fraction' | 'int'>;
            return {kind: 'expr.op.unary', type, flags: child.flags, op: `${type.kind}_checkzero`, child, pos: child.pos};
        } else if(child.constant.value !== 0) {
            return child;
        } else {
            ctx.diagnostics.compilationError('division by zero in constant expression', child.pos)
            return undefined;
        }
    }
    
    function _resolveObserveOrRewriteRule(rule: AST.ObserveRule | AST.RewriteRule, ctx: GridContext, outGrid: FormalGrid): RuleResolveResult {
        const {pos} = rule;
        
        let from = _resolveProp(rule, 'from', 'const pattern.in', ctx);
        if(from === PROP_ERROR) { return undefined; }
        
        ctx.inputPattern = from;
        ctx.isRuleContext = true;
        const via = _resolveProp(rule, 'via', 'pattern.out?', ctx);
        const to = ctx.withOutGrid(outGrid, rule.from.pos, () => _resolveProp(rule, 'to', 'pattern.out', ctx));
        ctx.inputPattern = undefined;
        const condition = _resolveProp(rule, 'condition', 'bool?', ctx) ?? _makeConstantExpr(Type.BOOL, true, pos);
        ctx.isRuleContext = false;
        if(via === PROP_ERROR || to === undefined || to === PROP_ERROR || condition === PROP_ERROR) { return undefined; }
        
        if(rule.kind === 'rule.rewrite' && to.kind === 'expr.constant') {
            from = PatternTree.and(from, PatternTree.not(to.constant.value, to.type.alphabetKey));
        }
        if(from.kind === 'bottom') {
            ctx.error('input pattern has no effect', rule.from.pos);
            return {rules: []};
        } else if(to.kind === 'expr.constant' && to.constant.value.kind === 'top') {
            ctx.error('output pattern has no effect', rule.to.pos);
            return {rules: []};
        } else if(via !== undefined && via.kind === 'expr.constant' && via.constant.value.kind === 'top') {
            ctx.error(`'via' pattern has no effect`, rule.via!.pos);
            return {rules: []};
        } else if(condition.kind === 'expr.constant' && !condition.constant.value) {
            // condition is constant `false` expression
            return {rules: []};
        }
        
        const rules: ASG.Rule[] = [];
        
        const makeRule = (from: PatternTree, via: ASG.Prop<'pattern.out?'>, to: ASG.Prop<'pattern.out'>): void => {
            rules.push({kind: rule.kind, from, via, to, condition, pos});
        };
        
        if((via === undefined || via.kind === 'expr.constant') && to.kind === 'expr.constant') {
            const symmetries = Symmetry.generate<{from: PatternTree, via?: Pattern, to: Pattern}>(
                {from, via: via?.constant!.value, to: to.constant.value},
                ctx.symmetryName,
                s => ({from: PatternTree.rotate(s.from), via: s.via && Pattern.rotate(s.via), to: Pattern.rotate(s.to)}),
                s => ({from: PatternTree.reflect(s.from), via: s.via && Pattern.reflect(s.via), to: Pattern.reflect(s.to)}),
                s => `${PatternTree.key(s.from)} -> ${s.via && Pattern.key(s.via)} -> ${Pattern.key(s.to)}`,
            );
            
            function makeExpr(p: Pattern, original: ASG.Prop<'pattern.out'>): ASG.Prop<'pattern.out'> {
                const {width, height} = p;
                const {alphabetKey} = original.type;
                return _makeConstantExpr({kind: 'pattern.out', width, height, alphabetKey}, p, original.pos);
            }
            
            for(const s of symmetries) {
                makeRule(s.from, via && s.via && makeExpr(s.via, via), makeExpr(s.to, to));
            }
        } else {
            // TODO: need to apply symmetries as ASG ops
            if(ctx.symmetryName === 'none' || (from.width === 1 && from.height === 1 && to.type.width === 1 && to.type.height === 1)) {
                makeRule(from, via, to);
            } else {
                ctx.error(`non-constant pattern requires 'symmetry "none"'`, rule.pos);
            }
        }
        
        return {rules};
    }
    
    function _resolveBlockStmt(stmt: AST.BlockStmt, ctx: Context): StmtResolveResult<AST.BlockStmt['kind']> {
        const {kind, pos} = stmt;
        
        const children = ctx.resolveStmts(stmt.children);
        
        if(children.length === 0) { return undefined; }
        return {kind: 'stmt', stmt: {kind, children, pos}};
    }
    function _resolvePropsStmt<T extends AST.Node, K extends keyof PropTypesMap>(stmt: T & {kind: K} & ASTProps<K>, ctx: Context): {kind: 'stmt', stmt: {kind: K, inGrid: number, pos: SourcePosition} & ResolvedProps<K>} | undefined {
        if(!ctx.expectGrid(stmt.pos)) { return undefined; }
        
        const props = _resolveProps<T, K>(stmt, ctx);
        if(props === undefined) { return undefined; }
        
        return {kind: 'stmt', stmt: {kind: stmt.kind, inGrid: ctx.grid.id, pos: stmt.pos, ...props}};
    }
    function _resolveAllOnceOneStmt(stmt: AST.AllStmt | AST.OnceStmt | AST.OneStmt, ctx: Context): StmtResolveResult<'stmt.rules.all' | 'stmt.rules.once' | 'stmt.rules.one'> {
        const {pos} = stmt;
        if(!ctx.expectGrid(pos)) { return undefined; }
        
        const props = stmt.kind !== 'stmt.rules.once' ? _resolveProps(stmt, ctx) : {search: undefined, maxStates: undefined, depthCoefficient: undefined, temperature: undefined};
        if(props === undefined) { return undefined; }
        
        const {search: isSearch = false, maxStates, depthCoefficient, temperature} = props;
        const {rewrites, fields, observations, assigns} = _resolveRules(stmt, ctx, ctx.grid, true);
        if(rewrites.length === 0) { return undefined; }
        
        const inGrid = ctx.grid.id;
        const kind = stmt.kind === 'stmt.rules.all' ? 'all' : 'one';
        const isBasic = fields.length === 0 && observations.length === 0;
        
        if(isSearch) {
            for(const field of fields) { ctx.error(`'field' cannot be used with 'search'`, field.pos); }
            if(observations.length === 0) { ctx.error(`'search' requires at least one 'observe'`, pos); }
        } else {
            if(maxStates !== undefined) { ctx.error(`argument 'maxStates' can only be used with 'search'`, maxStates.pos); }
            if(depthCoefficient !== undefined) { ctx.error(`argument 'depthCoefficient' can only be used with 'search'`, depthCoefficient.pos); }
        }
        
        let r: ASG.BranchingRulesStmt | ASG.LimitStmt;
        if(isBasic) {
            if(temperature !== undefined) { ctx.error(`'temperature' requires at least one 'field' or 'observe'`, temperature.pos); }
            r = {kind: `stmt.rules.basic.${kind}`, inGrid, rewrites, commutative: _rewritesCommute(rewrites), pos};
        } else if(isSearch) {
            r = {kind: `stmt.rules.search.${kind}`, inGrid, temperature, maxStates, depthCoefficient, rewrites, observations, pos};
        } else {
            if(fields.length > 0 && observations.length > 0) { ctx.error(`cannot have 'field' and 'observe' rules in the same block`, pos); }
            r = {kind: `stmt.rules.biased.${kind}`, inGrid, temperature, rewrites, fields, observations, pos};
        }
        
        if(stmt.kind === 'stmt.rules.once') {
            const limit = _makeConstantExpr(Type.INT, 1, pos);
            r = {kind: 'stmt.modified.limit', limit, child: r, pos};
        }
        
        return {kind: 'stmt', assigns, stmt: r};
    }
    
    const DECL_RESOLVE_FUNCS: {readonly [K in AST.Declaration['kind']]: DeclResolveFunc<K>} = {
        'decl.legend': (decl, ctx, f) => [
            undefined,
            ctx.withLegend(decl, f),
        ],
        'decl.let': (decl, ctx, f) => {
            const {name, pos} = decl.name;
            
            let rhs = ctx.resolveExpr(decl.rhs);
            if(rhs === undefined) {
                return [undefined, ctx.withErrorVariable(name, f)];
            }
            
            const {type, flags} = rhs;
            if(decl.isParam) {
                rhs = {kind: 'expr.param', type, flags, otherwise: rhs, name, pos};
            }
            
            const isMutable = (flags & ASG.ExprFlags.RUNTIME_CONSTANT) === 0;
            const variable = ctx.makeVariable(name, type, flags, isMutable ? undefined : rhs, decl.isParam, decl.name.pos);
            return [
                // constants will be folded, or assigned in preamble; not assigned in program body
                isMutable ? {kind: 'stmt.assign', variable, rhs, pos} : undefined,
                ctx.withVariable(variable, f),
            ];
        },
        'decl.symmetry': (decl, ctx, f) => [
            undefined,
            ctx.withSymmetry(decl, f),
        ],
        'decl.union': (decl, ctx, f) => [
            undefined,
            ctx.withUnion(decl, f),
        ],
    };
    
    const EXPR_RESOLVE_FUNCS: {readonly [K in AST.Expression['kind']]: ExprResolveFunc<K>} = {
        'expr.attr': (expr, ctx) => {
            const left = _resolveProp(expr, 'left', 'object', ctx);
            if(left === PROP_ERROR) { return undefined; }
            
            const {attr, pos} = expr;
            
            const {kind} = left.type;
            const entryTypes
                = kind === 'grid' ? Type.GRID_ATTRS
                : kind === 'position' ? Type.POSITION_ATTRS
                : left.type.entryTypes;
            
            const type = entryTypes.get(attr);
            if(type === undefined) {
                ctx.diagnostics.typeError(`type '${Type.toStr(left.type)}' has no attribute named '${attr}'`, pos);
                return undefined;
            }
            
            if(kind === 'grid') {
                const grid = _resolveProp(expr, 'left', 'const grid', ctx);
                if(grid === PROP_ERROR) { return undefined; }
                return {kind: 'expr.attr.grid', type, flags: ASG.ExprFlags.CONSTANT, grid, attr: attr as Type.GridAttribute, pos};
            } else if(left.kind === 'expr.constant') {
                if(kind === 'dict') {
                    const constant = (left.constant.value as Type.Value<'dict'>).get(attr) ?? fail();
                    return _makeConstantExpr(type, constant.value, pos);
                } else if(kind === 'position') {
                    const value = (left.constant.value as Type.Value<'position'>)[attr as Type.PositionAttribute];
                    return _makeConstantExpr(type, value, pos);
                }
            }
            
            return {kind: `expr.attr.${kind}`, type, flags: left.flags, left, attr, pos} as ASG.DictAttributeExpr | ASG.PositionAttributeExpr;
        },
        'expr.decl': (expr, ctx) => {
            const [decl, child] = ctx.resolveDecl(expr.declaration, () => ctx.resolveExpr(expr.child));
            return decl === undefined || child === undefined || child.kind === 'expr.constant'
                ? child
                : {kind: 'expr.decl', type: child.type, flags: decl.rhs.flags & child.flags, decl, child, pos: decl.pos};
        },
        'expr.dict': (expr, ctx) => {
            const {pos} = expr;
            
            const entryExprs = new Map<string, ASG.Expression>();
            const entryTypes = new Map<string, Type.Type>();
            const value = new Map<string, Type.ConstantValue>();
            let ok = true, flags = ASG.ExprFlags.ALL;
            for(const [{name}, v] of expr.pairs) {
                const resolved = ctx.resolveExpr(v);
                if(resolved !== undefined) {
                    entryExprs.set(name, resolved);
                    entryTypes.set(name, resolved.type);
                    if(resolved.kind === 'expr.constant') { value.set(name, resolved.constant); }
                    flags &= resolved.flags;
                } else {
                    ok = false;
                }
            }
            if(!ok) { return undefined; }
            
            const type: Type.Type = {kind: 'dict', entryTypes};
            if(value.size === entryExprs.size) { return _makeConstantExpr(type, value, pos); }
            return {kind: 'expr.dict', type, flags, entryExprs, pos};
        },
        'expr.grid': (expr, ctx) => {
            const {pos} = expr;
            const props = _resolveProps(expr, ctx);
            if(props === undefined) { return undefined; }
            
            const {scaleX = 1, scaleY = 1} = props;
            if(scaleX <= 0) { ctx.error(`'scaleX' must be positive`, expr.scaleX!.pos); }
            if(scaleY <= 0) { ctx.error(`'scaleY' must be positive`, expr.scaleY!.pos); }
            
            const alphabet = new Alphabet(expr.alphabetKey);
            const grid = withNextID(ctx.globals.grids, {
                alphabet,
                scaleX,
                scaleY,
                periodic: props.periodic ?? false,
                convPatterns: IDMap.withKey(_convPatternKey),
                pos,
            });
            return _makeConstantExpr(Type.GRID, grid.id, pos);
        },
        'expr.literal.bool': _resolveSimpleLiteralExpr,
        'expr.literal.float': _resolveSimpleLiteralExpr,
        'expr.literal.int': _resolveSimpleLiteralExpr,
        'expr.literal.pattern': (expr, ctx) => {
            const p = _resolvePatternLiteralExpr(expr, ctx);
            if(p === undefined) { return undefined; }
            
            const {width, height, alphabetKey} = p;
            return _makeConstantExpr(
                {kind: p.hasUnions ? 'pattern.in' : 'pattern.out', alphabetKey, width, height},
                p,
                expr.pos,
            );
        },
        'expr.literal.str': _resolveSimpleLiteralExpr,
        'expr.name.keyword': (expr, ctx) => {
            const {name, pos} = expr;
            
            const flags: ASG.ExprFlags = {
                at: ASG.ExprFlags.DETERMINISTIC | ASG.ExprFlags.LOCALLY_DETERMINISTIC | ASG.ExprFlags.GRID_INDEPENDENT,
                origin: ASG.ExprFlags.DETERMINISTIC | ASG.ExprFlags.LOCALLY_DETERMINISTIC | ASG.ExprFlags.GRID_INDEPENDENT | ASG.ExprFlags.RUNTIME_CONSTANT | ASG.ExprFlags.POSITION_INDEPENDENT,
                random: ASG.ExprFlags.GRID_INDEPENDENT | ASG.ExprFlags.POSITION_INDEPENDENT,
            }[name];
            
            switch(name) {
                case 'random':
                    return {kind: 'expr.name.keyword', type: Type.FLOAT, flags, name, pos};
            }
            
            if(!ctx.expectGrid(pos)) { return undefined; }
            switch(name) {
                case 'at':
                    if(!ctx.isRuleContext) { ctx.error(`'at' expression may only be used in a rule condition or output pattern`, pos); }
                    break;
            }
            const type: Type.Type = {kind: 'position', inGrid: ctx.grid.id};
            return {kind: 'expr.name.keyword', type, flags, name, pos};
        },
        'expr.name.simple': (expr, ctx) => {
            const {name, pos} = expr;
            if(ctx.errorVariables.has(name)) { return undefined; }
            
            const variable = ctx.variables.get(name);
            if(variable === undefined) { ctx.error(`no such variable '${name}'`, pos); return undefined; }
            
            const {type, initialiser} = variable;
            if(initialiser !== undefined && initialiser.kind === 'expr.constant') {
                return _makeConstantExpr(type, initialiser.constant.value, pos);
            } else {
                ++variable.references;
                return {kind: 'expr.name.simple', type, flags: variable.flags, variable, pos};
            }
        },
        'expr.op.binary': (expr, ctx) => {
            let left = ctx.resolveExpr(expr.left), right = ctx.resolveExpr(expr.right);
            if(left === undefined || right === undefined) { return undefined; }
            
            const {pos} = expr;
            
            if((expr.op === 'and' || expr.op === 'or') && (left.type.kind === 'pattern.in' || left.type.kind === 'pattern.out')) {
                const type: Type.OfKind<'pattern.in'> = {...left.type, kind: 'pattern.in'};
                if(!ctx.checkType(type, right)) { return undefined; }
                
                if(left.kind !== 'expr.constant' || right.kind !== 'expr.constant') {
                    // TODO?
                    ctx.error(`'${expr.op}' may only be used on constant patterns`, pos);
                    return undefined;
                }
                
                const value = PatternTree[expr.op](left.constant.value as PatternTree, right.constant.value as PatternTree);
                return _makeConstantExpr(type, value, pos);
            }
            
            // type coercion, from int to float or fraction
            if(left.type.kind === 'int' && (right.type.kind === 'float' || right.type.kind === 'fraction')) {
                left = _coerceFromInt(left, right.type);
            } else if(right.type.kind === 'int' && (left.type.kind === 'float' || left.type.kind === 'fraction')) {
                right = _coerceFromInt(right, left.type);
            } else if(left.type.kind === 'str' && expr.op === '+') {
                right = _coerceToStr(right);
            } else if(right.type.kind === 'str' && expr.op === '+') {
                left = _coerceToStr(left);
            }
            
            const spec = Op.BINARY_OP_TYPES[expr.op];
            for(const [leftType, rightType, outType, op] of spec) {
                // OK to only check `kind`, since it's a primitive type
                if(left.type.kind !== leftType) { continue; }
                
                // binary operators don't have multiple `rightType` options for the same `left.type`
                if(!ctx.checkType(Type.PRIMITIVES[rightType], right)) { return undefined; }
                
                switch(op) {
                    case 'float_truediv':
                    case 'float_mod':
                    case 'fraction_truediv':
                    case 'int_truediv':
                    case 'int_floordiv':
                    case 'int_mod':
                        right = _checkZero(ctx, right);
                        if(right === undefined) { return undefined; }
                        break;
                }
                
                const type = Type.PRIMITIVES[outType];
                const f = Op.BINARY_FUNCS[op] as Op.BinaryFunc;
                if(left.kind === 'expr.constant' && right.kind === 'expr.constant') {
                    const value = f(left.constant.value, right.constant.value);
                    if(value !== undefined) { return _makeConstantExpr(type, value, pos); }
                }
                const flags = left.flags & right.flags;
                return {kind: 'expr.op.binary', type, flags, op, left, right, pos};
            }
            
            ctx.typeError(spec.map(opt => opt[0]), left);
            return undefined;
        },
        'expr.op.ternary': (expr, ctx) => {
            const condition = _resolveProp(expr, 'condition', 'bool', ctx);
            let then = ctx.resolveExpr(expr.then);
            let otherwise = ctx.resolveExpr(expr.otherwise);
            if(condition === PROP_ERROR || then === undefined || otherwise === undefined) { return undefined; }
            
            if(then.type.kind === 'int' && (otherwise.type.kind === 'float' || otherwise.type.kind === 'fraction')) {
                then = _coerceFromInt(then, otherwise.type);
            } else if(otherwise.type.kind === 'int' && (then.type.kind === 'float' || then.type.kind === 'fraction')) {
                otherwise = _coerceFromInt(otherwise, then.type);
            } else if(then.type.kind === 'str') {
                otherwise = _coerceToStr(otherwise);
            } else if(otherwise.type.kind === 'str') {
                then = _coerceToStr(then);
            }
            
            const type = Type.leastUpperBound(then.type, otherwise.type);
            if(type === undefined) { ctx.typeError([Type.toStr(then.type)], otherwise); return undefined; }
            
            const flags = condition.flags & then.flags & otherwise.flags;
            if(condition.kind === 'expr.constant') {
                return condition.constant.value ? then : otherwise;
            }
            return {kind: 'expr.op.ternary', type, flags, condition, then, otherwise, pos: expr.pos};
        },
        'expr.op.unary': (expr, ctx) => {
            switch(expr.op) {
                case 'count':
                    return _resolveCountExpr(expr, ctx);
                case 'load':
                    return _resolveLoadExpr(expr, ctx);
                case 'randint':
                    return _resolveRandIntExpr(expr, ctx);
                case 'sum':
                    return _resolveSumExpr(expr, ctx);
            }
            
            const child = ctx.resolveExpr(expr.child);
            if(child === undefined) { return undefined; }
            
            const {pos} = expr;
            
            if(expr.op === 'not' && (child.type.kind === 'pattern.in' || child.type.kind === 'pattern.out')) {
                const type: Type.OfKind<'pattern.in'> = {...child.type, kind: 'pattern.in'};
                
                if(child.kind !== 'expr.constant') {
                    // TODO?
                    ctx.error(`'not' may only be used on constant patterns`, pos);
                    return undefined;
                }
                const value = PatternTree.not(child.constant.value as PatternTree, type.alphabetKey);
                return _makeConstantExpr(type, value, pos);
            }
            
            // unary + is a NOOP
            if(expr.op === '+') {
                return ctx.checkType(Type.NUMERIC, child) ? child : undefined;
            }
            
            const spec = Op.UNARY_OP_TYPES[expr.op];
            for(const [inType, outType, op] of spec) {
                // OK to only check `kind`, since it's a primitive type
                if(child.type.kind !== inType) { continue; }
                
                const type = Type.PRIMITIVES[outType];
                const f = Op.UNARY_FUNCS[op] as Op.UnaryFunc | undefined;
                if(child.kind === 'expr.constant' && f !== undefined) {
                    const value = f(child.constant.value);
                    if(value !== undefined) { return _makeConstantExpr(type, value, pos); }
                }
                return {kind: 'expr.op.unary', type, flags: child.flags, op, child, pos};
            }
            
            ctx.typeError(spec.map(opt => opt[0]), child);
            return undefined;
        },
    };
    
    const RULE_RESOLVE_FUNCS: {readonly [K in AST.Rule['kind']]: RuleResolveFunc<K>} = {
        'rule.decl': (rule, ctx, outGrid) => {
            const assigns: ASG.AssignStmt[] = [];
            const rules: ASG.Rule[] = [];
            let [decl, _] = ctx.resolveDecl(rule.declaration, () => {
                for(const c of rule.children) {
                    const r = ctx.resolveRule(c, outGrid);
                    if(r === undefined) { continue; }
                    
                    if(r.assigns !== undefined) { assigns.push(...r.assigns); }
                    rules.push(...r.rules);
                }
            });
            if(decl !== undefined) { assigns.unshift(decl); }
            return {assigns, rules};
        },
        'rule.field': (rule, ctx, outGrid) => {
            const {pos} = rule;
            
            const props = _resolveProps(rule, ctx);
            if(props === undefined) { return undefined; }
            
            const {for_, on, from, to, recompute = false, essential = false} = props;
            const zero = from ?? to;
            const inversed = from !== undefined;
            
            if(zero === undefined) {
                ctx.error(`'field' must have either 'from' or 'to'`, pos);
                return undefined;
            } else if(from !== undefined && to !== undefined) {
                ctx.error(`'field' cannot have both 'from' and 'to'`, pos);
            }
            
            const potential = withNextID(ctx.globals.potentials, {inGrid: ctx.grid.id, for_});
            return {rules: [{kind: 'rule.field', potential, for_, on, zero, inversed, recompute, essential, pos}]};
        },
        'rule.observe': _resolveObserveOrRewriteRule,
        'rule.rewrite': _resolveObserveOrRewriteRule,
    };
    
    const STMT_RESOLVE_FUNCS: {readonly [K in AST.Statement['kind']]: StmtResolveFunc<K>} = {
        'stmt.block.markov': _resolveBlockStmt,
        'stmt.block.sequence': _resolveBlockStmt,
        'stmt.convchain': (stmt, ctx) => {
            const {pos} = stmt;
            if(!ctx.expectGrid(pos)) { return undefined; }
            
            const props = _resolveProps(stmt, ctx);
            if(props === undefined) { return undefined; }
            
            const {on, sample, n, periodic = true, temperature, anneal, epsilon = 0.125} = props;
            // 1x1 patterns should be folded
            if(on !== undefined && on.kind !== 'leaf' && on.kind !== 'top') { fail(); }
            
            if(sample.pattern.some(c => c === PatternValue.WILDCARD || c === PatternValue.UNION)) {
                ctx.error(`'sample' must not have wildcards or unions`, stmt.sample.pos);
            }
            if(n < 1 || n > sample.width || n > sample.height) {
                ctx.error(`'n' must be at least 1 and at most the sample dimensions`, stmt.n.pos);
                return undefined;
            }
            if(epsilon <= 0) {
                ctx.error(`'epsilon' must be positive`, stmt.epsilon!.pos);
            }
            if(temperature !== undefined && temperature.kind === 'expr.constant' && temperature.constant.value < 0) {
                ctx.error(`'temperature' must be non-negative`, stmt.temperature!.pos);
            }
            if(anneal !== undefined && anneal.kind === 'expr.constant' && anneal.constant.value < 0) {
                ctx.error(`'anneal' must be non-negative`, stmt.anneal!.pos);
            }
            
            const output = ISet.toArray(ISet.of(ctx.grid.alphabet.key.length, sample.pattern));
            if(output.length < 2) {
                ctx.error(`'sample' must use at least two different alphabet symbols`, stmt.sample.pos);
                return undefined;
            }
            
            const samplePatterns = IDMap.withKey(Pattern.key);
            const sampleWeights: number[] = [];
            for(const symmetry of Symmetry.generate(sample, ctx.symmetryName, Pattern.rotate, Pattern.reflect, Pattern.key)) {
                for(const p of Pattern.windowsOf(symmetry, n, periodic)) {
                    const id = samplePatterns.getOrCreateID(p);
                    if(id === sampleWeights.length) { sampleWeights.push(0); }
                    ++sampleWeights[id];
                }
            }
            
            const weightMap = new Map<number, PatternTree[]>();
            samplePatterns.forEach((p, i) => {
                getOrCompute(weightMap, sampleWeights[i], () => []).push(p);
            });
            const weights = Array.from(weightMap.entries(), ([weight, v]) => ({
                pattern: v.reduce(PatternTree.or),
                weight,
            }));
            
            return {kind: 'stmt', stmt: {
                kind: 'stmt.convchain',
                inGrid: ctx.grid.id,
                on: on ?? PatternTree.top(1, 1, ctx.grid.alphabet.key),
                weights, output, temperature, anneal, epsilon,
                pos,
            }};
        },
        'stmt.decl': (stmt, ctx) => {
            let [decl, stmts] = ctx.resolveDecl(stmt.declaration, () => ctx.resolveStmts(stmt.children));
            stmts ??= [];
            if(decl !== undefined) { stmts.unshift(decl); }
            return {kind: 'stmts', stmts};
        },
        'stmt.log': _resolvePropsStmt,
        'stmt.modified.limit': (stmt, ctx) => {
            const limit = _resolveProp(stmt, 'arg', 'int', ctx);
            const r = ctx.resolveStmt(stmt.child);
            if(limit === PROP_ERROR) { return undefined; }
            if(limit.kind === 'expr.constant' && limit.constant.value <= 0) { ctx.error(`limit must be positive (was ${limit})`, stmt.arg.pos); }
            
            // TODO: loosen this to allow e.g. random limits; problem is that limit initialisers will be hoisted
            // to the start of their parent blocks, where referenced variables might not yet be assigned
            if((limit.flags & ASG.ExprFlags.RUNTIME_CONSTANT) === 0) { ctx.error(`limit must be a runtime constant`, stmt.arg.pos); }
            if(r === undefined) {
                return undefined;
            } else if(r.kind === 'stmts' || r.stmt.kind === 'stmt.log' || r.stmt.kind === 'stmt.rules.map' || r.stmt.kind === 'stmt.put' || r.stmt.kind === 'stmt.use') {
                ctx.error(`'@limit' cannot modify '${stmt.child.kind}'`, stmt.child.pos);
                return undefined;
            } else if(r.stmt.kind === 'stmt.modified.limit') {
                ctx.error(`statement cannot have multiple limits`, stmt.child.pos);
            }
            
            const {assigns, stmt: child} = r;
            return {kind: 'stmt', assigns, stmt: {kind: 'stmt.modified.limit', limit, child, pos: stmt.pos}};
        },
        'stmt.path': _resolvePropsStmt,
        'stmt.pass': (stmt, ctx) => undefined,
        'stmt.put': (stmt, ctx) => {
            const {pos} = stmt;
            if(!ctx.expectGrid(pos)) { return undefined; }
            
            const at = _resolveProp(stmt, 'at', 'position', ctx);
            ctx.isRuleContext = true;
            const props = _resolveProps(stmt, ctx);
            ctx.isRuleContext = false;
            
            if(at === PROP_ERROR || props === undefined) { return undefined; }
            
            const {pattern, condition} = props;
            const inGrid = ctx.grid.id;
            return {kind: 'stmt', stmt: {kind: 'stmt.put', inGrid, at, pattern, condition, pos}};
        },
        'stmt.rules.all': _resolveAllOnceOneStmt,
        'stmt.rules.convolution': (stmt, ctx) => ctx.withKernel(stmt, kernel => {
            const {pos} = stmt;
            if(!ctx.expectGrid(pos)) { return undefined; }
            
            let boundary = _resolveProp(stmt, 'boundary', 'const charset.in?', ctx);
            if(boundary !== undefined && ctx.grid.periodic) { ctx.error(`periodic grid has no boundary`, stmt.boundary!.pos); }
            if(boundary === PROP_ERROR) { boundary = undefined; }
            
            ctx.boundaryMask
                = boundary === undefined ? undefined
                : PatternTree.isLeafOrTop(boundary) ? boundary.masks[0]
                : fail();
            const {rewrites, assigns} = _resolveRules(stmt, ctx, ctx.grid, false);
            ctx.boundaryMask = undefined;
            if(rewrites.length === 0) { return undefined; }
            
            const charsUsed = ISet.empty(ctx.grid.alphabet.key.length);
            for(const rule of rewrites) {
                if(rule.from.width !== 1 || rule.from.height !== 1) { ctx.error(`'convolution' rule patterns must be 1x1`, rule.pos); }
                
                if(rule.from.kind === 'bottom') { continue; }
                
                // logical ops on const 1x1 patterns should already be folded
                const chars = PatternTree.isLeafOrTop(rule.from) ? rule.from.masks[0] : fail();
                if(!ISet.isDisjoint(chars, charsUsed)) {
                    ctx.error(`'convolution' input patterns must be disjoint`, rule.pos);
                }
                ISet.addAll(charsUsed, chars);
            }
            
            return {
                kind: 'stmt',
                assigns,
                stmt: {kind: 'stmt.rules.convolution', inGrid: ctx.grid.id, rewrites, kernel, boundary, pos},
            };
        }),
        'stmt.rules.map': (stmt, ctx) => {
            const {pos} = stmt;
            if(!ctx.expectGrid(pos)) { return undefined; }
            
            const inGrid = ctx.grid.id;
            const outGrid = _resolveProp(stmt, 'outGrid', 'const grid', ctx);
            if(outGrid === PROP_ERROR) { return undefined; }
            if(outGrid === inGrid) { ctx.error(`'outGrid' must be different to the input grid`, stmt.outGrid.pos); }
            
            const formalOutGrid = ctx.globals.grids[outGrid];
            const {assigns, rewrites} = _resolveRules(stmt, ctx, formalOutGrid, false);
            ctx.grid = formalOutGrid;
            if(rewrites.length === 0) { return undefined; }
            
            const commutative = rewrites.every(rule => rule.from.width === 1 && rule.from.height === 1);
            return {kind: 'stmt', assigns, stmt: {kind: 'stmt.rules.map', inGrid, outGrid, rewrites, commutative, pos}};
        },
        'stmt.rules.once': _resolveAllOnceOneStmt,
        'stmt.rules.one': _resolveAllOnceOneStmt,
        'stmt.rules.prl': (stmt, ctx) => {
            const {pos} = stmt;
            if(!ctx.expectGrid(pos)) { return undefined; }
            
            const {rewrites, assigns} = _resolveRules(stmt, ctx, ctx.grid, false);
            if(rewrites.length === 0) { return undefined; }
            
            return {
                kind: 'stmt',
                assigns,
                stmt: {kind: 'stmt.rules.basic.prl', inGrid: ctx.grid.id, rewrites, commutative: _rewritesCommute(rewrites), pos},
            };
        },
        
        'stmt.use.expr': (stmt, ctx) => {
            const grid = _resolveProp(stmt, 'expr', 'const grid', ctx);
            if(grid === PROP_ERROR) { return undefined; }
            
            ctx.grid = ctx.globals.grids[grid];
            return {kind: 'stmt', stmt: {kind: 'stmt.use', grid, pos: stmt.pos}};
        },
        'stmt.use.let': (stmt, ctx) => {
            if(stmt.decl.isParam) { ctx.error(`'use let' declaration cannot be a 'param'`, stmt.pos); }
            
            const grid = _resolveProp(stmt.decl, 'rhs', 'const grid', ctx);
            if(grid === PROP_ERROR) { return undefined; }
            
            ctx.grid = ctx.globals.grids[grid];
            
            const {name, rhs, pos} = stmt.decl;
            const variable = ctx.makeVariable(name.name, Type.GRID, ASG.ExprFlags.CONSTANT, _makeConstantExpr(Type.GRID, grid, rhs.pos), false, name.pos);
            const stmts = ctx.withVariable(variable, () => ctx.resolveStmts(stmt.children));
            stmts.unshift(
                {kind: 'stmt.assign', variable, rhs: _makeConstantExpr(Type.GRID, grid, rhs.pos), pos},
                {kind: 'stmt.use', grid, pos: stmt.pos}
            );
            return {kind: 'stmts', stmts};
        },
    };
    
    export function resolve(ast: AST.CompilationUnit): ASG.ASG {
        const ctx = new Context();
        const root = ctx.resolveRoot(ast);
        const endGrid = ctx.grid;
        if(endGrid === undefined) { ctx.error('program uses no grid', ast.pos); }
        ctx.diagnostics.throwIfAnyErrors();
        
        const {grids, params, potentials, variables} = ctx.globals;
        return {
            root,
            grids,
            params,
            potentials,
            variables,
            endGridID: endGrid!.id,
        };
    }
}
