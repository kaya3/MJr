"use strict";
var CodeGen;
(function (CodeGen) {
    CodeGen.NOOP = Symbol();
    function binaryOp(p, before, lMinP, between, rMinP, after) {
        return { p, before, lMinP, between, rMinP, after };
    }
    CodeGen.binaryOp = binaryOp;
    function unaryOp(p, before, cMinP, after) {
        return { p, before, cMinP, after };
    }
    CodeGen.unaryOp = unaryOp;
    function infixOp(p, op, associativity = 1 /* Associativity.LEFT */) {
        const lMinP = (associativity & 1 /* Associativity.LEFT */) !== 0 ? p : p + 1;
        const rMinP = (associativity & 2 /* Associativity.RIGHT */) !== 0 ? p : p + 1;
        return binaryOp(p, '', lMinP, ` ${op} `, rMinP, '');
    }
    CodeGen.infixOp = infixOp;
    function prefixOp(p, op) {
        return unaryOp(p, op, p, '');
    }
    CodeGen.prefixOp = prefixOp;
    class Base {
        config;
        diagnostics = new Diagnostics();
        _out = [];
        _indentationLevel = 0;
        _indent = '';
        _toAssign = new Map();
        LPAREN = '(';
        RPAREN = ')';
        constructor(config) {
            this.config = config;
        }
        beginLine() {
            const { _out } = this;
            if (_out.length > 0 && !_out[_out.length - 1].endsWith('\n')) {
                _out.push('\n');
            }
            _out.push(this._indent);
        }
        write(s) {
            this._out.push(s);
        }
        indent() {
            const i = ++this._indentationLevel;
            this._indent = ' '.repeat(this.config.indentSpaces * i);
        }
        dedent() {
            const i = --this._indentationLevel;
            this._indent = ' '.repeat(this.config.indentSpaces * i);
        }
        writeStmt(stmt) {
            const f = this.STMT_WRITE_FUNCS[stmt.kind];
            f(this, stmt);
        }
        writeExpr(expr, minPrecedence = 0) {
            switch (expr.kind) {
                case 'expr.letin': {
                    if (this.writeAssignExpr !== undefined) {
                        for (const decl of expr.decls) {
                            this._toAssign.set(decl.name.name, decl.initialiser);
                        }
                        this.writeExpr(expr.child, minPrecedence);
                        return;
                    }
                    break;
                }
                case 'expr.name': {
                    const rhs = this._toAssign.get(expr.name);
                    if (this.writeAssignExpr !== undefined && rhs !== undefined) {
                        this._toAssign.delete(expr.name);
                        if (minPrecedence > 0) {
                            this.write(this.LPAREN);
                        }
                        this.writeAssignExpr(expr, rhs);
                        if (minPrecedence > 0) {
                            this.write(this.RPAREN);
                        }
                        return;
                    }
                    break;
                }
                case 'expr.op.unary': {
                    const spec = this.UNARY_OPS[expr.op];
                    if (spec === CodeGen.NOOP) {
                        this.writeExpr(expr.child, minPrecedence);
                    }
                    else {
                        const { p, before, cMinP, after } = spec;
                        if (p < minPrecedence) {
                            this.write(this.LPAREN);
                        }
                        this.write(before);
                        this.writeExpr(expr.child, cMinP);
                        this.write(after);
                        if (p < minPrecedence) {
                            this.write(this.RPAREN);
                        }
                    }
                    return;
                }
                case 'expr.op.binary': {
                    const { p, before, lMinP, between, rMinP, after } = this.BINARY_OPS[expr.op];
                    if (p < minPrecedence) {
                        this.write(this.LPAREN);
                    }
                    this.write(before);
                    this.writeExpr(expr.left, lMinP);
                    this.write(between);
                    this.writeExpr(expr.right, rMinP);
                    this.write(after);
                    if (p < minPrecedence) {
                        this.write(this.RPAREN);
                    }
                    return;
                }
            }
            const [p, f] = this.EXPR_WRITE_FUNCS[expr.kind];
            if (p < minPrecedence) {
                this.write(this.LPAREN);
            }
            f(this, expr);
            if (p < minPrecedence) {
                this.write(this.RPAREN);
            }
            return;
        }
        writeIndentedBlock(stmt) {
            this.writeStmt(stmt.kind !== 'stmt.block' ? { kind: 'stmt.block', children: [stmt] } : stmt);
        }
        writeList(writer, n, rowLength = IR.DEFAULT_ROW_LENGTH, compact = false, sep = ',') {
            const multiline = rowLength < n;
            if (multiline) {
                this.indent();
                if (!compact) {
                    this.beginLine();
                }
            }
            for (let i = 0; i < n; ++i) {
                writer(i);
                if (i < n - 1) {
                    this.write(sep);
                    if (i % rowLength < rowLength - 1) {
                        this.write(' ');
                    }
                    else {
                        this.beginLine();
                    }
                }
            }
            if (multiline) {
                this.dedent();
                if (!compact) {
                    this.beginLine();
                }
            }
        }
        writeExprList(exprs, rowLength = IR.DEFAULT_ROW_LENGTH, compact = false, sep = ',') {
            this.writeList(i => this.writeExpr(exprs[i]), exprs.length, rowLength, compact, sep);
        }
        writeLongStringLiteral(s, rowLength, concatOp = ' +') {
            if (s.length === 0) {
                this.write(`""`);
                return;
            }
            rowLength *= Math.ceil(4 * IR.DEFAULT_ROW_LENGTH / rowLength);
            this.writeList(i => {
                const start = i * rowLength;
                this.write(JSON.stringify(s.slice(start, start + rowLength)));
            }, Math.ceil(s.length / rowLength), 1, false, concatOp);
        }
        render() {
            return this._out.join('');
        }
    }
    CodeGen.Base = Base;
    function signedIntBits(domainSize) {
        return domainSize <= (1 << 7) ? 8
            : domainSize <= (1 << 15) ? 16
                : 32;
    }
    CodeGen.signedIntBits = signedIntBits;
    function uintBits(domainSize) {
        return domainSize <= (1 << 8) ? 8
            : domainSize <= (1 << 16) ? 16
                : 32;
    }
    CodeGen.uintBits = uintBits;
    function arrayToHex(arr, bitsPerElement) {
        const digitsPerElement = bitsPerElement >> 2;
        return arr.map(x => x.toString(16).padStart(digitsPerElement, '0')).join('');
    }
    CodeGen.arrayToHex = arrayToHex;
})(CodeGen || (CodeGen = {}));
///<reference path="base.ts"/>
var CodeGen;
(function (CodeGen) {
    const RUNTIME_LIB_NAME = 'MJr';
    class JavaScript extends CodeGen.Base {
        STMT_WRITE_FUNCS = {
            'stmt.assign': (out, stmt) => {
                const { left, op, right } = stmt;
                out.beginLine();
                if ((op === '+=' || op === '-=') && right === IR.ONE) {
                    out.write(op === '+=' ? '++' : '--');
                    out.writeExpr(left);
                }
                else {
                    out.writeExpr(left);
                    out.write(` ${op} `);
                    out.writeExpr(right);
                }
                out.write(';');
            },
            'stmt.block': (out, stmt) => {
                out.write(' {');
                out.indent();
                for (const c of stmt.children) {
                    out.writeStmt(c);
                }
                out.dedent();
                out.beginLine();
                out.write('}');
            },
            'stmt.blankline': (out, stmt) => {
                out.beginLine();
            },
            'stmt.break': (out, stmt) => {
                out.beginLine();
                out.write('break;');
            },
            'stmt.comment': (out, stmt) => {
                out.beginLine();
                out.write(`// ${stmt.comment}`);
            },
            'stmt.continue': (out, stmt) => {
                out.beginLine();
                out.write('continue;');
            },
            'stmt.decl.func': (out, stmt) => {
                const { params, paramTypes } = stmt;
                out.beginLine();
                out.write('function');
                if (stmt.yields !== undefined) {
                    out.write('*');
                }
                out.write(' ');
                out.writeExpr(stmt.name);
                out.write('(');
                out.writeList(i => out.writeParamDecl(params[i].name, paramTypes[i]), params.length);
                out.write(')');
                out.writeReturnType(stmt.returnType, stmt.yields);
                out.writeIndentedBlock(stmt.body);
            },
            'stmt.decl.vars': (out, stmt) => {
                const { decls, mutable } = stmt;
                out.beginLine();
                out.write(mutable ? 'let ' : 'const ');
                out.writeList(i => out.writeVarDecl(decls[i]), decls.length, 1, true);
                out.write(';');
            },
            'stmt.expr': (out, stmt) => {
                out.beginLine();
                out.writeExpr(stmt.expr);
                out.write(';');
            },
            'stmt.for.range': (out, stmt) => {
                const { index: { name: i }, low, high, reverse, body } = stmt;
                out.beginLine();
                out.write(`for(let ${i} = `);
                if (reverse) {
                    out.writeExpr(IR.OP.minusOne(high));
                    out.write(`; ${i} >= `);
                    out.writeExpr(low, 9 /* Precedence.CMP */);
                    out.write(`; --${i})`);
                }
                else {
                    out.writeExpr(low);
                    out.write(`; ${i} < `);
                    out.writeExpr(high, 9 /* Precedence.CMP */);
                    out.write(`; ++${i})`);
                }
                out.writeIndentedBlock(body);
            },
            'stmt.if': (out, stmt) => {
                let cur = stmt;
                out.beginLine();
                while (true) {
                    out.write('if(');
                    out.writeExpr(cur.condition);
                    out.write(')');
                    // intented block has braces, avoiding parsing hazard of `if(...) if(...) ... else ...`
                    out.writeIndentedBlock(cur.then);
                    cur = cur.otherwise;
                    if (cur === undefined) {
                        break;
                    }
                    out.write(' else');
                    if (cur.kind === 'stmt.if') {
                        out.write(' ');
                    }
                    else {
                        out.writeIndentedBlock(cur);
                        break;
                    }
                }
            },
            'stmt.log': (out, stmt) => {
                out.beginLine();
                out.write('console.log(');
                out.writeExpr(stmt.expr);
                out.write(')');
            },
            'stmt.pass': (out, stmt) => {
                out.beginLine();
                out.write(';');
            },
            'stmt.preamble': (out, stmt) => {
                out.beginLine();
                out.write('width = width | 0;');
                out.beginLine();
                out.write('height = height | 0;');
                if (stmt.emitChecks) {
                    // TODO: add code to check params are valid at runtime
                    const { libVersion } = stmt;
                    out.beginLine();
                    out.beginLine();
                    out.write(`if(typeof ${RUNTIME_LIB_NAME} !== "object" || typeof ${RUNTIME_LIB_NAME}.VERSION !== "number") throw new Error("${RUNTIME_LIB_NAME} runtime library not found");`);
                    out.beginLine();
                    out.write(`if(${RUNTIME_LIB_NAME}.VERSION !== ${libVersion}) throw new Error("Requires ${RUNTIME_LIB_NAME} runtime library version ${libVersion}");`);
                }
                out.beginLine();
                out.write(`rng ??= ${RUNTIME_LIB_NAME}.DEFAULT_PRNG;`);
                for (const op of stmt.opsUsed) {
                    if (objHasKey(MJr.OPS, op)) {
                        out.beginLine();
                        out.write(`const ${op} = ${RUNTIME_LIB_NAME}.OPS.${op};`);
                    }
                }
            },
            'stmt.return': (out, stmt) => {
                out.beginLine();
                out.write('return');
                if (stmt.expr !== undefined) {
                    out.write(' ');
                    out.writeExpr(stmt.expr);
                }
                out.write(';');
            },
            'stmt.switch': (out, stmt) => {
                out.beginLine();
                out.writeSwitch(stmt);
            },
            'stmt.throw': (out, stmt) => {
                out.beginLine();
                out.write(`throw new Error(${JSON.stringify(stmt.message)});`);
            },
            'stmt.while': (out, stmt) => {
                const { then } = stmt;
                out.beginLine();
                out.write('while(');
                out.writeExpr(stmt.condition);
                out.write(')');
                if (then.kind === 'stmt.switch') {
                    // main loop is like `while(...) switch(...) { ... }`, don't need two levels of indentation for switch body
                    out.write(' ');
                    out.writeSwitch(then);
                }
                else {
                    out.writeIndentedBlock(then);
                }
            },
            'stmt.yield': (out, stmt) => {
                out.beginLine();
                this.write('yield');
                if (stmt.expr !== undefined) {
                    this.write(' ');
                    this.writeExpr(stmt.expr);
                }
                this.write(';');
            },
        };
        EXPR_WRITE_FUNCS = {
            'expr.array.const': [18 /* Precedence.MAX */, (out, expr) => {
                    const { from } = expr;
                    const bits = CodeGen.uintBits(expr.domainSize);
                    const s = CodeGen.arrayToHex(from, bits);
                    out.write(`${RUNTIME_LIB_NAME}.HEX.u${bits}(`);
                    out.writeLongStringLiteral(s, expr.rowLength * s.length / from.length);
                    out.write(')');
                }],
            'expr.array.new': [18 /* Precedence.MAX */, (out, expr) => {
                    const bits = CodeGen.uintBits(expr.domainSize);
                    out.write(`new Uint${bits}Array(`);
                    out.writeExpr(expr.length);
                    out.write(')');
                }],
            'expr.attr': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.left, 17 /* Precedence.ATTR_ACCESS_CALL */);
                    out.write(`.${expr.attr}`);
                }],
            'expr.dict': [18 /* Precedence.MAX */, (out, expr) => {
                    const { type: { keys }, values } = expr;
                    out.write('{');
                    out.writeList(i => {
                        out.write(`${keys[i]}: `);
                        out.writeExpr(values[i]);
                    }, keys.length, 1);
                    out.write('}');
                }],
            'expr.letin': [0, fail],
            'expr.literal.bool': _literal,
            'expr.literal.float': _literal,
            'expr.literal.int': _literal,
            'expr.literal.str': _literal,
            'expr.literal.null': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write('undefined');
                }],
            'expr.name': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write(expr.name);
                }],
            'expr.op.access': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.left, 17 /* Precedence.ATTR_ACCESS_CALL */);
                    out.write('[');
                    out.writeExpr(expr.right);
                    out.write(']');
                }],
            'expr.op.call.lib.constructor': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write(`new ${RUNTIME_LIB_NAME}.${expr.className}`);
                    out.write(`(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.lib.function': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.write(`${RUNTIME_LIB_NAME}.${expr.name}`);
                    out.write(`(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.lib.method': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.obj);
                    out.write(`.${expr.name}(`);
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.op.call.local': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.name);
                    out.write('(');
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.param': [3 /* Precedence.NULL_COALESCE */, (out, expr) => {
                    out.write(`params?.${expr.name} ?? `);
                    out.writeExpr(expr.otherwise, 3 /* Precedence.NULL_COALESCE */ + 1);
                }],
            'expr.op.ternary': [2 /* Precedence.TERNARY */, (out, expr) => {
                    out.writeExpr(expr.condition, 2 /* Precedence.TERNARY */ + 1);
                    out.write(' ? ');
                    out.writeExpr(expr.then, 2 /* Precedence.TERNARY */);
                    out.write(' : ');
                    out.writeExpr(expr.otherwise, 2 /* Precedence.TERNARY */);
                }],
        };
        BINARY_OPS = (function () {
            function _intOp(p, op) {
                // all of these ops are left-associative
                return CodeGen.binaryOp(5 /* Precedence.BITWISE_OR */, '(', p, ` ${op} `, p + 1, ') | 0');
            }
            function _func(name) {
                return CodeGen.binaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `${name}(`, 0 /* Precedence.MIN */, ', ', 0 /* Precedence.MIN */, ')');
            }
            const PLUS = CodeGen.infixOp(11 /* Precedence.PLUS_MINUS */, '+', 3 /* Associativity.BOTH */), MINUS = CodeGen.infixOp(11 /* Precedence.PLUS_MINUS */, '-'), EQ = CodeGen.infixOp(8 /* Precedence.EQ */, '==='), NE = CodeGen.infixOp(8 /* Precedence.EQ */, '!=='), LT = CodeGen.infixOp(9 /* Precedence.CMP */, '<'), LE = CodeGen.infixOp(9 /* Precedence.CMP */, '<='), GT = CodeGen.infixOp(9 /* Precedence.CMP */, '>'), GE = CodeGen.infixOp(9 /* Precedence.CMP */, '>=');
            return {
                bool_and: CodeGen.infixOp(4 /* Precedence.BOOL_AND */, '&&', 3 /* Associativity.BOTH */),
                bool_or: CodeGen.infixOp(3 /* Precedence.BOOL_OR */, '||', 3 /* Associativity.BOTH */),
                bool_eq: EQ,
                bool_ne: NE,
                // + and * are not strictly right-associative for floats
                float_plus: CodeGen.infixOp(11 /* Precedence.PLUS_MINUS */, '+'),
                float_minus: MINUS,
                float_mult: CodeGen.infixOp(12 /* Precedence.MULT_DIV_MOD */, '*'),
                float_truediv: CodeGen.infixOp(12 /* Precedence.MULT_DIV_MOD */, '/'),
                float_mod: _func('float_mod'),
                float_eq: EQ,
                float_ne: NE,
                float_lt: LT,
                float_le: LE,
                float_gt: GT,
                float_ge: GE,
                fraction_plus: _func('fraction_plus'),
                fraction_minus: _func('fraction_minus'),
                fraction_mult: _func('fraction_mult'),
                fraction_truediv: _func('fraction_truediv'),
                fraction_eq: _func('fraction_eq'),
                fraction_ne: _func('fraction_ne'),
                fraction_lt: _func('fraction_lt'),
                fraction_le: _func('fraction_le'),
                fraction_gt: _func('fraction_gt'),
                fraction_ge: _func('fraction_ge'),
                int_plus: _intOp(11 /* Precedence.PLUS_MINUS */, '+'),
                int_minus: _intOp(11 /* Precedence.PLUS_MINUS */, '-'),
                int_mult: _func('Math.imul'),
                int_truediv: _func('int_truediv'),
                int_floordiv: _func('int_floordiv'),
                int_mod: _func('int_mod'),
                int_eq: EQ,
                int_ne: NE,
                int_lt: LT,
                int_le: LE,
                int_gt: GT,
                int_ge: GE,
                int_and: CodeGen.infixOp(7 /* Precedence.BITWISE_AND */, '&', 3 /* Associativity.BOTH */),
                int_or: CodeGen.infixOp(5 /* Precedence.BITWISE_OR */, '|', 3 /* Associativity.BOTH */),
                int_xor: CodeGen.infixOp(6 /* Precedence.BITWISE_XOR */, '^', 3 /* Associativity.BOTH */),
                int_lshift: CodeGen.infixOp(10 /* Precedence.BITWISE_SHIFT */, '<<'),
                int_rshift: CodeGen.infixOp(10 /* Precedence.BITWISE_SHIFT */, '>>'),
                str_concat: PLUS,
                str_eq: EQ,
                str_ne: NE,
                loose_int_plus: PLUS,
                loose_int_minus: MINUS,
                loose_int_mult: CodeGen.infixOp(12 /* Precedence.MULT_DIV_MOD */, '*', 3 /* Associativity.BOTH */),
                loose_int_floordiv: _intOp(12 /* Precedence.MULT_DIV_MOD */, '/'),
                loose_int_mod: CodeGen.infixOp(12 /* Precedence.MULT_DIV_MOD */, '%'),
            };
        })();
        UNARY_OPS = (function () {
            function _intOp(p, op) {
                return CodeGen.unaryOp(5 /* Precedence.BITWISE_OR */, `(${op}`, p, ') | 0');
            }
            function _func(name) {
                return CodeGen.unaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `${name}(`, 0 /* Precedence.MIN */, ')');
            }
            const TO_STR = CodeGen.unaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, '', 17 /* Precedence.ATTR_ACCESS_CALL */, '.toString()');
            return {
                bool_not: CodeGen.prefixOp(14 /* Precedence.BOOL_NOT */, '!'),
                // need space to avoid incorrect parse of `- - x`
                float_uminus: CodeGen.prefixOp(14 /* Precedence.UPLUS_UMINUS */, '- '),
                float_checkzero: _func('float_checkzero'),
                fraction_uminus: _func('fraction_uminus'),
                fraction_checkzero: CodeGen.NOOP,
                // need space to avoid incorrect parse of `- - x`
                int_uminus: _intOp(14 /* Precedence.UPLUS_UMINUS */, '- '),
                int_checkzero: _func('int_checkzero'),
                int_not: CodeGen.prefixOp(14 /* Precedence.BITWISE_NOT */, '~'),
                int_ctz: _func('int_ctz'),
                int_to_float: CodeGen.NOOP,
                int_to_fraction: _func('int_to_fraction'),
                bool_to_str: TO_STR,
                float_to_str: TO_STR,
                fraction_to_str: _func('fraction_to_str'),
                grid_to_str: TO_STR,
                int_to_str: TO_STR,
            };
        })();
        writeAssignExpr(left, right) {
            this.writeExpr(left);
            this.write(' = ');
            this.writeExpr(right, 2 /* Precedence.ASSIGN */);
        }
        writeSwitch(stmt) {
            this.write('switch(');
            this.writeExpr(stmt.expr);
            this.write(') {');
            this.indent();
            for (const c of stmt.cases) {
                for (const value of c.values) {
                    this.beginLine();
                    this.write(`case ${value}:`);
                }
                this.writeIndentedBlock(IR.block([c.then, IR.BREAK]));
            }
            this.dedent();
            this.beginLine();
            this.write('}');
        }
        writeParamDecl(name, type) {
            this.write(name);
        }
        writeVarDecl(decl) {
            this.writeExpr(decl.name);
            if (decl.initialiser !== undefined) {
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type, yields) { }
    }
    CodeGen.JavaScript = JavaScript;
    class TypeScript extends JavaScript {
        writeParamDecl(name, type) {
            this.write(name);
            if (type.kind === 'nullable') {
                this.write('?');
                type = type.componentType;
            }
            this.write(': ');
            this.writeType(type);
        }
        writeVarDecl(decl) {
            this.writeExpr(decl.name);
            if (decl.initialiser === undefined) {
                this.write('!: ');
                this.writeType(decl.type);
            }
            else {
                // otherwise, type should be inferred from initialiser
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type, yields) {
            this.write(': ');
            if (yields === undefined) {
                this.writeType(type);
            }
            else {
                this.write(`Generator<`);
                this.writeType(yields);
                this.write(`, `);
                this.writeType(type);
                this.write(`>`);
            }
        }
        writeType(type) {
            switch (type.kind) {
                case 'dict': {
                    const { keys, values } = type;
                    this.write('{');
                    this.writeList(i => this.writeParamDecl(keys[i], values[i]), keys.length);
                    this.write('}');
                    return;
                }
                case 'array.const':
                case 'array.mutable': {
                    this.write(`Uint${CodeGen.uintBits(type.domainSize)}Array`);
                    return;
                }
                case 'nullable': {
                    this.writeType(type.componentType);
                    this.write(' | undefined');
                    return;
                }
                default: {
                    this.write(TYPES_TO_TS[type.kind]);
                    return;
                }
            }
        }
    }
    CodeGen.TypeScript = TypeScript;
    const TYPES_TO_TS = {
        bool: 'boolean',
        byte: 'number',
        float: 'number',
        fraction: `${RUNTIME_LIB_NAME}.Fraction`,
        grid: `${RUNTIME_LIB_NAME}.Grid`,
        int: 'number',
        pattern: `${RUNTIME_LIB_NAME}.Pattern`,
        prng: `${RUNTIME_LIB_NAME}.PRNG`,
        rewriteinfo: `${RUNTIME_LIB_NAME}.RewriteInfo`,
        sampler: `${RUNTIME_LIB_NAME}.Sampler`,
        str: 'string',
        void: 'void',
    };
    const _literal = [16 /* Precedence.LITERAL */, (out, expr) => {
            out.write(JSON.stringify(expr.value));
        }];
})(CodeGen || (CodeGen = {}));
///<reference path="base.ts"/>
var CodeGen;
(function (CodeGen) {
    const RUNTIME_LIB_NAME = 'MJr';
    class Python extends CodeGen.Base {
        constructor(config) {
            super(config);
            if (config.indentSpaces === 0) {
                this.diagnostics.configError(`Python output requires 'indentSpaces' to be non-zero`);
            }
        }
        STMT_WRITE_FUNCS = {
            'stmt.assign': (out, stmt) => {
                const { left, op, right } = stmt;
                out.beginLine();
                out.writeExpr(left);
                out.write(` ${op} `);
                out.writeExpr(right);
            },
            'stmt.block': (out, stmt) => {
                out.write(':');
                out.indent();
                for (const c of stmt.children) {
                    out.writeStmt(c);
                }
                out.dedent();
            },
            'stmt.blankline': (out, stmt) => {
                out.beginLine();
            },
            'stmt.break': (out, stmt) => {
                out.beginLine();
                out.write('break');
            },
            'stmt.comment': (out, stmt) => {
                out.beginLine();
                out.write(`# ${stmt.comment}`);
            },
            'stmt.continue': (out, stmt) => {
                out.beginLine();
                out.write('continue');
            },
            'stmt.decl.func': (out, stmt) => {
                const { params, paramTypes } = stmt;
                out.beginLine();
                out.write('def ');
                out.writeExpr(stmt.name);
                out.write('(');
                out.writeList(i => out.writeParamDecl(params[i].name, paramTypes[i]), params.length);
                out.write(')');
                out.writeReturnType(stmt.returnType);
                out.writeIndentedBlock(stmt.body);
            },
            'stmt.decl.vars': (out, stmt) => {
                for (const decl of stmt.decls) {
                    out.writeVarDecl(decl);
                }
            },
            'stmt.expr': (out, stmt) => {
                out.beginLine();
                out.writeExpr(stmt.expr);
            },
            'stmt.for.range': (out, stmt) => {
                const { low, high } = stmt;
                out.beginLine();
                out.write(`for ${stmt.index.name} in range(`);
                if (stmt.reverse) {
                    out.writeExpr(IR.OP.minusOne(high));
                    out.write(`, `);
                    out.writeExpr(IR.OP.minusOne(low));
                    out.write(`, -1)`);
                }
                else {
                    if (low !== IR.ZERO) {
                        out.writeExpr(low);
                        out.write(`, `);
                    }
                    out.writeExpr(high);
                    out.write(`)`);
                }
                out.writeIndentedBlock(stmt.body);
            },
            'stmt.if': (out, stmt) => {
                let cur = stmt;
                out.beginLine();
                out.write('if ');
                while (true) {
                    out.writeExpr(cur.condition);
                    out.writeIndentedBlock(cur.then);
                    cur = cur.otherwise;
                    if (cur === undefined) {
                        break;
                    }
                    out.beginLine();
                    if (cur.kind === 'stmt.if') {
                        out.write('elif ');
                    }
                    else {
                        out.write('else');
                        out.writeIndentedBlock(cur);
                        break;
                    }
                }
            },
            'stmt.log': (out, stmt) => {
                out.beginLine();
                out.write('print(');
                out.writeExpr(stmt.expr);
                out.write(')');
            },
            'stmt.pass': (out, stmt) => {
                out.beginLine();
                out.write('pass');
            },
            'stmt.preamble': (out, stmt) => {
                out.beginLine();
                out.write(`import ${RUNTIME_LIB_NAME}`);
                if (stmt.emitChecks) {
                    // TODO: add code to check params are valid at runtime
                    const { libVersion } = stmt;
                    out.beginLine();
                    out.write(`if ${RUNTIME_LIB_NAME}.VERSION !== ${libVersion}: raise Error("Requires ${RUNTIME_LIB_NAME} runtime library version ${libVersion}")`);
                }
                out.beginLine();
                out.write(`if rng is None: rng = ${RUNTIME_LIB_NAME}.DefaultPRNG()`);
                out.beginLine();
                out.write(`import array`);
                out.beginLine();
                out.write(`int32 = ${RUNTIME_LIB_NAME}.int32`);
                out.write(`int_ctz = ${RUNTIME_LIB_NAME}.int_ctz`);
                if (stmt.opsUsed.includes('int_truediv') || stmt.opsUsed.includes('int_to_fraction')) {
                    out.beginLine();
                    out.write('from fractions import Fraction');
                }
            },
            'stmt.return': (out, stmt) => {
                out.beginLine();
                out.write('return');
                if (stmt.expr !== undefined) {
                    out.write(' ');
                    out.writeExpr(stmt.expr);
                }
            },
            'stmt.switch': (out, stmt) => {
                out.beginLine();
                out.write('match ');
                out.writeExpr(stmt.expr);
                out.write(':');
                out.indent();
                for (const c of stmt.cases) {
                    out.beginLine();
                    out.write(`case ${c.values.join(' | ')}`);
                    out.writeIndentedBlock(c.then);
                }
                out.dedent();
            },
            'stmt.throw': (out, stmt) => {
                out.beginLine();
                out.write(`raise Error(${JSON.stringify(stmt.message)})`);
            },
            'stmt.while': (out, stmt) => {
                out.beginLine();
                out.write('while ');
                out.writeExpr(stmt.condition);
                out.writeIndentedBlock(stmt.then);
            },
            'stmt.yield': (out, stmt) => {
                out.beginLine();
                out.write('yield');
                if (stmt.expr !== undefined) {
                    out.write(' ');
                    out.writeExpr(stmt.expr);
                }
            },
        };
        EXPR_WRITE_FUNCS = {
            'expr.array.const': [18 /* Precedence.MAX */, (out, expr) => {
                    const { from, domainSize, rowLength } = expr;
                    const bits = CodeGen.uintBits(domainSize);
                    const s = CodeGen.arrayToHex(from, bits);
                    const f = bits === 8 ? 'bytes.fromhex' : `${RUNTIME_LIB_NAME}.hex_to_u${bits}`;
                    out.write(`${f}(`);
                    out.writeLongStringLiteral(s, rowLength * s.length / from.length, '');
                    out.write(')');
                }],
            'expr.array.new': [13 /* Precedence.MULT_DIV_MOD */, (out, expr) => {
                    const bits = CodeGen.uintBits(expr.domainSize);
                    if (bits === 8) {
                        out.write(`bytearray(`);
                        out.writeExpr(expr.length);
                        out.write(')');
                    }
                    else {
                        // https://docs.python.org/3/library/array.html
                        out.write(`array.array("${bits === 16 ? 'H' : 'L'}", (0,)) * `);
                        out.writeExpr(expr.length, 13 /* Precedence.MULT_DIV_MOD */);
                    }
                }],
            'expr.attr': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.left, 17 /* Precedence.ATTR_ACCESS_CALL */);
                    out.write(`.${expr.attr}`);
                }],
            'expr.dict': [18 /* Precedence.MAX */, (out, expr) => {
                    const { type: { keys }, values } = expr;
                    out.write('{');
                    out.writeList(i => {
                        out.write(`"${keys[i]}": `);
                        out.writeExpr(values[i]);
                    }, keys.length, 1);
                    out.write('}');
                }],
            'expr.letin': [0, fail],
            'expr.literal.bool': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write(expr.value ? 'True' : 'False');
                }],
            'expr.literal.float': _literal,
            'expr.literal.int': _literal,
            'expr.literal.str': _literal,
            'expr.literal.null': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write('None');
                }],
            'expr.name': [18 /* Precedence.MAX */, (out, expr) => {
                    out.write(expr.name);
                }],
            'expr.op.access': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.left, 17 /* Precedence.ATTR_ACCESS_CALL */);
                    out.write('[');
                    out.writeExpr(expr.right);
                    out.write(']');
                }],
            'expr.op.call.lib.constructor': [17 /* Precedence.ATTR_ACCESS_CALL */, (js, expr) => {
                    js.write(`${RUNTIME_LIB_NAME}.${expr.className}`);
                    js.write(`(`);
                    js.writeExprList(expr.args);
                    js.write(')');
                }],
            'expr.op.call.lib.function': [17 /* Precedence.ATTR_ACCESS_CALL */, (js, expr) => {
                    js.write(`${RUNTIME_LIB_NAME}.${expr.name}`);
                    js.write(`(`);
                    js.writeExprList(expr.args);
                    js.write(')');
                }],
            'expr.op.call.lib.method': [17 /* Precedence.ATTR_ACCESS_CALL */, (js, expr) => {
                    js.writeExpr(expr.obj);
                    js.write(`.${expr.name}(`);
                    js.writeExprList(expr.args);
                    js.write(')');
                }],
            'expr.op.call.local': [17 /* Precedence.ATTR_ACCESS_CALL */, (out, expr) => {
                    out.writeExpr(expr.name);
                    out.write('(');
                    out.writeExprList(expr.args);
                    out.write(')');
                }],
            'expr.param': [3 /* Precedence.TERNARY */, (out, expr) => {
                    // TODO: take params as **kwargs
                    out.write(`params['${expr.name}'] if params is not None and '${expr.name}' in params else `);
                    out.writeExpr(expr.otherwise, 3 /* Precedence.TERNARY */);
                }],
            'expr.op.ternary': [3 /* Precedence.TERNARY */, (out, expr) => {
                    out.writeExpr(expr.then, 3 /* Precedence.TERNARY */ + 1);
                    out.write(' if ');
                    out.writeExpr(expr.condition, 3 /* Precedence.TERNARY */ + 1);
                    out.write(' else ');
                    out.writeExpr(expr.otherwise, 3 /* Precedence.TERNARY */);
                }],
        };
        BINARY_OPS = (function () {
            function _cmpOp(op) {
                // Python's comparison ops are neither left- nor right-associative; need to avoid them chaining
                return CodeGen.infixOp(7 /* Precedence.CMP_EQ */, op, 0 /* Associativity.NEITHER */);
            }
            function _intOp(p, op) {
                // all of these ops are left-associative
                return CodeGen.binaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, 'int32(', p, ` ${op} `, p + 1, ')');
            }
            function _func(name) {
                return CodeGen.binaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `${name}(`, 0 /* Precedence.MIN */, ', ', 0 /* Precedence.MIN */, ')');
            }
            // PLUS and MULT are not strictly right-associative for floats
            const PLUS = CodeGen.infixOp(12 /* Precedence.PLUS_MINUS */, '+'), MINUS = CodeGen.infixOp(12 /* Precedence.PLUS_MINUS */, '-'), MULT = CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '*'), DIV = CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '/'), FLOORDIV = CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '//'), MOD = CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '%'), EQ = _cmpOp('=='), NE = _cmpOp('!='), LT = _cmpOp('<'), LE = _cmpOp('<='), GT = _cmpOp('>'), GE = _cmpOp('>=');
            return {
                bool_and: CodeGen.infixOp(5 /* Precedence.BOOL_AND */, 'and', 3 /* Associativity.BOTH */),
                bool_or: CodeGen.infixOp(4 /* Precedence.BOOL_OR */, 'or', 3 /* Associativity.BOTH */),
                bool_eq: EQ,
                bool_ne: NE,
                // + and * are not strictly right-associative for floats
                float_plus: CodeGen.infixOp(12 /* Precedence.PLUS_MINUS */, '+'),
                float_minus: MINUS,
                float_mult: CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '*'),
                float_truediv: CodeGen.infixOp(13 /* Precedence.MULT_DIV_MOD */, '/'),
                float_mod: MOD,
                float_eq: EQ,
                float_ne: NE,
                float_lt: LT,
                float_le: LE,
                float_gt: GT,
                float_ge: GE,
                fraction_plus: PLUS,
                fraction_minus: MINUS,
                fraction_mult: MULT,
                fraction_truediv: DIV,
                fraction_eq: EQ,
                fraction_ne: NE,
                fraction_lt: LT,
                fraction_le: LE,
                fraction_gt: GT,
                fraction_ge: GE,
                int_plus: _intOp(12 /* Precedence.PLUS_MINUS */, '+'),
                int_minus: _intOp(12 /* Precedence.PLUS_MINUS */, '-'),
                int_mult: _intOp(13 /* Precedence.MULT_DIV_MOD */, '*'),
                int_truediv: _func('Fraction'),
                int_floordiv: FLOORDIV,
                int_mod: MOD,
                int_eq: EQ,
                int_ne: NE,
                int_lt: LT,
                int_le: LE,
                int_gt: GT,
                int_ge: GE,
                int_and: CodeGen.infixOp(10 /* Precedence.BITWISE_AND */, '&', 3 /* Associativity.BOTH */),
                int_or: CodeGen.infixOp(8 /* Precedence.BITWISE_OR */, '|', 3 /* Associativity.BOTH */),
                int_xor: CodeGen.infixOp(9 /* Precedence.BITWISE_XOR */, '^', 3 /* Associativity.BOTH */),
                int_lshift: CodeGen.infixOp(11 /* Precedence.BITWISE_SHIFT */, '<<'),
                int_rshift: CodeGen.infixOp(11 /* Precedence.BITWISE_SHIFT */, '>>'),
                str_concat: PLUS,
                str_eq: EQ,
                str_ne: NE,
                loose_int_plus: PLUS,
                loose_int_minus: MINUS,
                loose_int_mult: MULT,
                loose_int_floordiv: FLOORDIV,
                loose_int_mod: MOD,
            };
        })();
        UNARY_OPS = (function () {
            function _intOp(p, op) {
                return CodeGen.unaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `int32(${op}`, p, ')');
            }
            function _func(name) {
                return CodeGen.unaryOp(17 /* Precedence.ATTR_ACCESS_CALL */, `${name}(`, 0 /* Precedence.MIN */, ')');
            }
            const UMINUS = CodeGen.prefixOp(14 /* Precedence.UPLUS_UMINUS */, '-'), TO_STR = _func('str');
            // 'checkzero' ops are NOOPs in Python; all of the relevant operations already raise errors for divzero
            return {
                bool_not: CodeGen.prefixOp(6 /* Precedence.BOOL_NOT */, 'not '),
                float_uminus: UMINUS,
                float_checkzero: CodeGen.NOOP,
                fraction_uminus: UMINUS,
                fraction_checkzero: CodeGen.NOOP,
                int_uminus: _intOp(14 /* Precedence.UPLUS_UMINUS */, '-'),
                int_checkzero: CodeGen.NOOP,
                int_not: CodeGen.prefixOp(14 /* Precedence.BITWISE_NOT */, '~'),
                int_ctz: _func('int_ctz'),
                int_to_float: _func('float'),
                int_to_fraction: _func('Fraction'),
                bool_to_str: TO_STR,
                float_to_str: TO_STR,
                fraction_to_str: TO_STR,
                grid_to_str: TO_STR,
                int_to_str: TO_STR,
            };
        })();
        writeAssignExpr(left, right) {
            this.writeExpr(left);
            this.write(' := ');
            this.writeExpr(right, 2 /* Precedence.ASSIGN */);
        }
        writeParamDecl(name, type) {
            this.write(name);
            if (type.kind === 'nullable') {
                this.write('=None');
            }
        }
        writeVarDecl(decl) {
            if (decl.initialiser === undefined) {
                return;
            }
            this.beginLine();
            this.writeExpr(decl.name);
            this.write(' = ');
            this.writeExpr(decl.initialiser);
        }
        writeReturnType(type) { }
    }
    CodeGen.Python = Python;
    class PythonWithTypes extends Python {
        writeParamDecl(name, type) {
            this.write(name);
            this.write(': ');
            this.writeType(type);
            if (type.kind === 'nullable') {
                this.write(' = None');
            }
        }
        writeVarDecl(decl) {
            this.beginLine();
            this.writeExpr(decl.name);
            this.write(': ');
            this.writeType(decl.type);
            if (decl.initialiser !== undefined) {
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type) {
            this.write(' -> ');
            this.writeType(type);
        }
        writeType(type) {
            switch (type.kind) {
                case 'dict': {
                    const { keys, values } = type;
                    // TODO: need `stmt.typedecl` or otherwise; declare namedtuple types
                    this.write('{');
                    this.writeList(i => this.writeParamDecl(keys[i], values[i]), keys.length);
                    this.write('}');
                    return;
                }
                case 'array.const': {
                    this.write(type.domainSize <= 256 ? 'bytes' : 'array.array');
                    return;
                }
                case 'array.mutable': {
                    this.write(type.domainSize <= 256 ? 'bytearray' : 'array.array');
                    return;
                }
                case 'nullable': {
                    this.writeType(type.componentType);
                    this.write(' | None');
                    return;
                }
                default: {
                    this.write(TYPES_TO_PY[type.kind]);
                    return;
                }
            }
        }
    }
    CodeGen.PythonWithTypes = PythonWithTypes;
    const TYPES_TO_PY = {
        bool: 'bool',
        byte: 'int',
        float: 'float',
        fraction: `Fraction`,
        grid: `${RUNTIME_LIB_NAME}.Grid`,
        int: 'int',
        pattern: `${RUNTIME_LIB_NAME}.Pattern`,
        prng: `${RUNTIME_LIB_NAME}.PRNG`,
        rewriteinfo: `${RUNTIME_LIB_NAME}.RewriteInfo`,
        sampler: `${RUNTIME_LIB_NAME}.Sampler`,
        str: 'str',
        void: 'None',
    };
    const _literal = [18 /* Precedence.MAX */, (out, expr) => {
            out.write(JSON.stringify(expr.value));
        }];
})(CodeGen || (CodeGen = {}));
var CFG;
(function (CFG) {
    // only `stmt` and `reset` nodes will be compiled to cases in the main `switch` statement
    function isSwitchNode(node) {
        return node.kind === 'stmt.branching'
            || node.kind === 'stmt.nonbranching'
            || node.kind === 'reset';
    }
    CFG.isSwitchNode = isSwitchNode;
    const newLabel = () => ({ nodeID: -1 });
    class CFGBuilder {
        animate;
        nodes = [];
        numFlags = 0;
        constructor(animate) {
            this.animate = animate;
        }
        makeNode(partialNode) {
            return withNextID(this.nodes, partialNode);
        }
        buildBlock(stmt, flagID, then) {
            const { kind, children } = stmt;
            if (children.length === 0) {
                return this.makeNode({ kind: 'pass', then });
            }
            const isMarkov = kind === 'stmt.block.markov';
            const labels = makeArray(children.length, newLabel);
            labels.push(then);
            for (let i = 0; i < children.length; ++i) {
                const child = children[i];
                const thenTrue = labels[isMarkov ? 0 : i];
                const thenFalse = labels[i + 1];
                labels[i].nodeID = this.buildChild(child, kind, flagID, thenTrue, thenFalse).id;
            }
            return this.nodes[labels[0].nodeID];
        }
        buildChild(stmt, parentKind, parentFlagID, ifTrue, then) {
            switch (stmt.kind) {
                case 'stmt.block.markov':
                case 'stmt.block.sequence': {
                    if (stmt.children.length === 0) {
                        return this.makeNode({ kind: 'pass', then });
                    }
                    else if (stmt.kind === 'stmt.block.markov' && parentKind === 'stmt.block.sequence' && stmt.reset === undefined) {
                        // optimisation: don't need a separate reset node for this
                        return this.buildBlock(stmt, parentFlagID, then);
                    }
                    const flagID = this.numFlags++;
                    const beginLabel = newLabel();
                    const endLabel = newLabel();
                    const begin = this.makeNode({ kind: 'reset', stmt, flagID, reset: stmt.reset, then: beginLabel });
                    beginLabel.nodeID = this.buildBlock(stmt, flagID, endLabel).id;
                    if (parentFlagID >= 0) {
                        const setFlagLabel = newLabel();
                        endLabel.nodeID = this.makeNode({ kind: 'checkflag', flagID, ifTrue: setFlagLabel, then }).id;
                        setFlagLabel.nodeID = this.makeNode({ kind: 'setflag', flagID: parentFlagID, then: ifTrue }).id;
                    }
                    else {
                        endLabel.nodeID = this.makeNode({ kind: 'checkflag', flagID, ifTrue, then }).id;
                    }
                    return begin;
                }
                case 'stmt.modified.limit': {
                    const { limit } = stmt;
                    if (limit.isTransparent) {
                        // optimisation for common case
                        return this.buildChild(stmt.child, parentKind, parentFlagID, then, then);
                    }
                    const limitID = limit.id;
                    const childLabel = newLabel(), decrementLimitLabel = newLabel();
                    const r = this.makeNode({ kind: 'checklimit', limitID, ifTrue: childLabel, then });
                    childLabel.nodeID = this.buildChild(stmt.child, parentKind, parentFlagID, decrementLimitLabel, then).id;
                    decrementLimitLabel.nodeID = this.makeNode({ kind: 'decrementlimit', limitID, then: ifTrue }).id;
                    return r;
                }
                case 'stmt.assign':
                case 'stmt.log':
                case 'stmt.put':
                case 'stmt.rules.map':
                case 'stmt.use': {
                    if ((stmt.kind === 'stmt.assign' && stmt.variable.references === 0) || (stmt.kind === 'stmt.use' && !this.animate)) {
                        return this.makeNode({ kind: 'pass', then });
                    }
                    return this.makeNode({ kind: 'stmt.nonbranching', stmt, then });
                }
                case 'stmt.convchain':
                case 'stmt.path':
                case 'stmt.rules.basic.all':
                case 'stmt.rules.basic.one':
                case 'stmt.rules.basic.prl':
                case 'stmt.rules.biased.all':
                case 'stmt.rules.biased.one':
                case 'stmt.rules.convolution':
                case 'stmt.rules.search.all':
                case 'stmt.rules.search.one': {
                    if (parentFlagID >= 0) {
                        const setFlagLabel = newLabel();
                        const child = this.makeNode({ kind: 'stmt.branching', stmt, ifChanged: setFlagLabel, then });
                        setFlagLabel.nodeID = this.makeNode({ kind: 'setflag', flagID: parentFlagID, then: ifTrue }).id;
                        return child;
                    }
                    else {
                        return this.makeNode({ kind: 'stmt.branching', stmt, ifChanged: ifTrue, then });
                    }
                }
            }
        }
    }
    function build(root, animate) {
        const builder = new CFGBuilder(animate);
        const stopLabel = newLabel();
        const rootNode = builder.buildBlock(root, -1, stopLabel);
        stopLabel.nodeID = builder.makeNode({ kind: 'stop' }).id;
        // sanity check
        if (rootNode.id !== 0) {
            fail();
        }
        return builder;
    }
    CFG.build = build;
})(CFG || (CFG = {}));
/**
 * Abstract syntax tree for a high-level intermediate representation (IR) which
 * is output by the compiler. The IR is converted into output source code by a
 * class in the `CodeGen` namespace.
 *
 * The IR should be convertible to most imperative languages, so long as they
 * allow local function declarations to access constants from the outer scope.
 */
var IR;
(function (IR) {
    IR.DEFAULT_ROW_LENGTH = 16;
    // all type declarations here must be JSON serialisable, since `JSON.stringify` is used to detect repeated IR code and simplify it
    const JSON_KEY = Symbol();
    function key(a) {
        return a[JSON_KEY] ??= JSON.stringify(a);
    }
    IR.key = key;
    function equals(a, b) {
        if (a === b) {
            return true;
        }
        if (a === undefined || b === undefined || a.kind !== b.kind) {
            return false;
        }
        return key(a) === key(b);
    }
    IR.equals = equals;
    function float(value) {
        return value === 0 ? IR.FLOAT_ZERO
            : value === 1 ? IR.FLOAT_ONE
                : value === -1 ? IR.FLOAT_MINUS_ONE
                    : { kind: 'expr.literal.float', value };
    }
    IR.float = float;
    function int(value) {
        return value === 0 ? IR.ZERO
            : value === 1 ? IR.ONE
                : value === -1 ? IR.MINUS_ONE
                    : { kind: 'expr.literal.int', value };
    }
    IR.int = int;
    function str(value) {
        return { kind: 'expr.literal.str', value };
    }
    IR.str = str;
    // singleton objects for common values
    IR.TRUE = { kind: 'expr.literal.bool', value: true };
    IR.FALSE = { kind: 'expr.literal.bool', value: false };
    IR.NULL = { kind: 'expr.literal.null' };
    IR.ZERO = { kind: 'expr.literal.int', value: 0 };
    IR.ONE = { kind: 'expr.literal.int', value: 1 };
    IR.MINUS_ONE = { kind: 'expr.literal.int', value: -1 };
    IR.FLOAT_ZERO = { kind: 'expr.literal.float', value: 0 };
    IR.FLOAT_ONE = { kind: 'expr.literal.float', value: 1 };
    IR.FLOAT_MINUS_ONE = { kind: 'expr.literal.float', value: -1 };
    function attr(left, attr) {
        return { kind: 'expr.attr', left, attr };
    }
    IR.attr = attr;
    function letIn(decls, child) {
        return decls.length === 0 ? child
            : { kind: 'expr.letin', decls, child };
    }
    IR.letIn = letIn;
    function nameExpr(name) {
        return { kind: 'expr.name', name };
    }
    IR.nameExpr = nameExpr;
    function param(name, otherwise) {
        return { kind: 'expr.param', name, otherwise };
    }
    IR.param = param;
    function dict(type, values) {
        return { kind: 'expr.dict', type, values };
    }
    IR.dict = dict;
    function newArray(length, domainSize) {
        return { kind: 'expr.array.new', length, domainSize };
    }
    IR.newArray = newArray;
    function newGridDataArray(length) {
        return newArray(length, IR.GRID_DATA_ARRAY_TYPE.domainSize);
    }
    IR.newGridDataArray = newGridDataArray;
    function newInt32Array(length) {
        return newArray(length, IR.INT32_ARRAY_TYPE.domainSize);
    }
    IR.newInt32Array = newInt32Array;
    function constArray(from, domainSize, rowLength = IR.DEFAULT_ROW_LENGTH) {
        return { kind: 'expr.array.const', from, domainSize, rowLength };
    }
    IR.constArray = constArray;
    function constArrayDecl(name, from, domainSize, rowLength = IR.DEFAULT_ROW_LENGTH) {
        return {
            name,
            type: IR.constArrayType(domainSize),
            initialiser: constArray(from, domainSize, rowLength),
        };
    }
    IR.constArrayDecl = constArrayDecl;
    function newArrayDecl(name, length, domainSize) {
        return {
            name,
            type: IR.mutableArrayType(domainSize),
            initialiser: newArray(length, domainSize),
        };
    }
    IR.newArrayDecl = newArrayDecl;
    function access(left, right) {
        return { kind: 'expr.op.access', left, right };
    }
    IR.access = access;
    function libConstructorCall(className, args) {
        return { kind: 'expr.op.call.lib.constructor', className, args };
    }
    IR.libConstructorCall = libConstructorCall;
    function libFunctionCall(name, args) {
        return { kind: 'expr.op.call.lib.function', name, args };
    }
    IR.libFunctionCall = libFunctionCall;
    function libMethodCall(className, name, obj, args) {
        return { kind: 'expr.op.call.lib.method', className, name, obj, args };
    }
    IR.libMethodCall = libMethodCall;
    function localCall(name, args) {
        return { kind: 'expr.op.call.local', name, args };
    }
    IR.localCall = localCall;
    function ternary(condition, then, otherwise) {
        if (condition === IR.TRUE) {
            return then;
        }
        else if (condition === IR.FALSE) {
            return otherwise;
        }
        else if (condition.kind === 'expr.op.unary' && condition.op === 'bool_not') {
            condition = condition.child;
            const tmp = then;
            then = otherwise;
            otherwise = tmp;
        }
        return { kind: 'expr.op.ternary', condition, then, otherwise };
    }
    IR.ternary = ternary;
    // singletons
    IR.BLANK_LINE = { kind: 'stmt.blankline' };
    IR.BREAK = { kind: 'stmt.break' };
    IR.CONTINUE = { kind: 'stmt.continue' };
    IR.PASS = { kind: 'stmt.pass' };
    function assign(left, op, right) {
        return { kind: 'stmt.assign', op, left, right };
    }
    IR.assign = assign;
    function block(children) {
        children = children.flatMap(c => c.kind === 'stmt.block' ? c.children
            : c === IR.PASS ? []
                : [c]);
        return children.length === 0 ? IR.PASS
            : children.length === 1 ? children[0]
                : { kind: 'stmt.block', children };
    }
    IR.block = block;
    function comment(comment) {
        return { kind: 'stmt.comment', comment };
    }
    IR.comment = comment;
    function declFunc(name, yields, params, paramTypes, returnType, body) {
        return { kind: 'stmt.decl.func', name, yields, params, paramTypes, returnType, body };
    }
    IR.declFunc = declFunc;
    function declVar(name, type, initialiser, mutable = false) {
        return declVars([{ name, type, initialiser }], mutable);
    }
    IR.declVar = declVar;
    function declVars(decls, mutable = false) {
        return decls.length > 0 ? { kind: 'stmt.decl.vars', decls, mutable } : IR.PASS;
    }
    IR.declVars = declVars;
    function forRange(index, low, high, body) {
        return { kind: 'stmt.for.range', index, low, high, reverse: false, body: block(body) };
    }
    IR.forRange = forRange;
    function forRangeReverse(index, low, high, body) {
        return { kind: 'stmt.for.range', index, low, high, reverse: true, body: block(body) };
    }
    IR.forRangeReverse = forRangeReverse;
    function if_(condition, then, otherwise) {
        if (otherwise === IR.PASS) {
            otherwise = undefined;
        }
        if (condition === IR.TRUE) {
            return then;
        }
        else if (condition === IR.FALSE) {
            return otherwise ?? IR.PASS;
        }
        else if (equals(then, otherwise)) {
            return then;
        }
        else if (then === IR.PASS) {
            return otherwise === undefined ? IR.PASS : if_(IR.OP.not(condition), otherwise);
        }
        else if (then.kind === 'stmt.assign' && otherwise !== undefined && otherwise.kind === 'stmt.assign' && equals(then.left, otherwise.left) && then.op === otherwise.op) {
            // replace `if(c) { x = a; } else { x = b; }` with `x = c ? a : b;`
            return assign(then.left, then.op, ternary(condition, then.right, otherwise.right));
        }
        else if (then.kind === 'stmt.for.range' && then.low === IR.ZERO && (equals(condition, IR.OP.gt(then.high, IR.ZERO)) || equals(condition, IR.OP.lt(IR.ZERO, then.high))) && otherwise === undefined) {
            // omit redundant `if` statement guarding a `for` loop
            return then;
        }
        else if (then.kind === 'stmt.if' && otherwise === undefined && then.otherwise === undefined) {
            // collapse nested `if` statements
            return if_(IR.OP.and(condition, then.condition), then.then);
        }
        else {
            return { kind: 'stmt.if', condition, then, otherwise };
        }
    }
    IR.if_ = if_;
    function libFunctionCallStmt(f, args) {
        return { kind: 'stmt.expr', expr: libFunctionCall(f, args) };
    }
    IR.libFunctionCallStmt = libFunctionCallStmt;
    function libMethodCallStmt(className, name, obj, args) {
        return { kind: 'stmt.expr', expr: libMethodCall(className, name, obj, args) };
    }
    IR.libMethodCallStmt = libMethodCallStmt;
    function localCallStmt(f, args) {
        return { kind: 'stmt.expr', expr: localCall(f, args) };
    }
    IR.localCallStmt = localCallStmt;
    function log(expr) {
        return { kind: 'stmt.log', expr };
    }
    IR.log = log;
    function preamble(paramTypes, emitChecks, libVersion, opsUsed) {
        return { kind: 'stmt.preamble', paramTypes, emitChecks, libVersion, opsUsed };
    }
    IR.preamble = preamble;
    function return_(expr) {
        return { kind: 'stmt.return', expr };
    }
    IR.return_ = return_;
    function switch_(expr, casesByIndex) {
        if (casesByIndex.length === 0) {
            return IR.PASS;
        }
        else if (casesByIndex.length === 1) {
            return casesByIndex[0];
        }
        else if (casesByIndex.length === 2) {
            return if_(IR.OP.eq(expr, IR.ZERO), casesByIndex[0], casesByIndex[1]);
        }
        const firstCase = casesByIndex[0];
        if (firstCase.kind === 'stmt.if' && casesByIndex.every((c) => c.kind === 'stmt.if' && equals(c.condition, firstCase.condition))) {
            // factor out common condition; the `otherwise` part will generally be trivial
            return if_(firstCase.condition, switch_(expr, casesByIndex.map(c => c.then)), switch_(expr, casesByIndex.map(c => c.otherwise ?? IR.PASS)));
        }
        // de-duplicate cases
        const map = new Map();
        let exhaustive = true;
        for (let i = 0; i < casesByIndex.length; ++i) {
            const c = casesByIndex[i];
            if (c === IR.PASS) {
                exhaustive = false;
                continue;
            }
            const k = key(c);
            getOrCompute(map, k, () => ({ values: [], then: c })).values.push(i);
        }
        const cases = Array.from(map.values());
        return cases.length === 0 ? IR.PASS
            : cases.length === 1 && exhaustive ? firstCase
                : { kind: 'stmt.switch', expr, cases };
    }
    IR.switch_ = switch_;
    function throw_(message) {
        return { kind: 'stmt.throw', message };
    }
    IR.throw_ = throw_;
    function while_(condition, then) {
        // the compiler won't ever output an infinite loop that does nothing; if the loop body is empty, then the condition must be false
        if (then === IR.PASS) {
            return IR.PASS;
        }
        return { kind: 'stmt.while', condition, then };
    }
    IR.while_ = while_;
    function yield_(expr) {
        return { kind: 'stmt.yield', expr };
    }
    IR.yield_ = yield_;
})(IR || (IR = {}));
var IR;
(function (IR) {
    IR.NAMES = {
        WIDTH: IR.nameExpr('width'),
        HEIGHT: IR.nameExpr('height'),
        PARAMS: IR.nameExpr('params'),
        RNG: IR.nameExpr('rng'),
        STATE: IR.nameExpr('state'),
        AT: IR.nameExpr('at'),
        AT_X: IR.nameExpr('atX'),
        AT_Y: IR.nameExpr('atY'),
        AT_CONV: IR.nameExpr('atConv'),
        MATCHES: IR.nameExpr('matches'),
        MATCH_COUNT: IR.nameExpr('count'),
        MATCH: IR.nameExpr('m'),
        ANY: IR.nameExpr('any'),
        G: IR.nameExpr('g'),
        I: IR.nameExpr('i'),
        J: IR.nameExpr('j'),
        N: IR.nameExpr('n'),
        P: IR.nameExpr('p'),
        START_X: IR.nameExpr('startX'),
        START_Y: IR.nameExpr('startY'),
        END_X: IR.nameExpr('endX'),
        END_Y: IR.nameExpr('endY'),
        EFFECTIVE_WIDTH: IR.nameExpr('w'),
        EFFECTIVE_HEIGHT: IR.nameExpr('h'),
        S: IR.nameExpr('s'),
        OLD_S: IR.nameExpr('oldS'),
        T: IR.nameExpr('t'),
        OLD_T: IR.nameExpr('oldT'),
        U: IR.nameExpr('u'),
        MASK: IR.nameExpr('mask'),
        MASK_CLEAR: IR.nameExpr('mask_clear'),
        MASK_SET: IR.nameExpr('mask_set'),
        MASK_HASNT: IR.nameExpr('mask_hasnt'),
        constant(id) {
            return IR.nameExpr(`constant${id}`);
        },
        counter(g, id) {
            return IR.nameExpr(`grid${g.grid.id}_counter${id}`);
        },
        flag(id) {
            return IR.nameExpr(`f${id}`);
        },
        limit(id) {
            return IR.nameExpr(`limit${id}`);
        },
        sampler(g, id) {
            return IR.nameExpr(`grid${g.grid.id}_sampler${id}`);
        },
        tmpPattern(id) {
            return IR.nameExpr(`p${id}`);
        },
        variable(v, n) {
            return IR.nameExpr(n > 0 ? `_${v}_${n}` : `_${v}`);
        },
        convBufferVar(g, id, v) {
            return IR.nameExpr(`grid${g.grid.id}_conv${id}_${v}`);
        },
        gridVar(g, v) {
            return IR.nameExpr(`grid${g.grid.id}_${v}`);
        },
        matcherVar(g, id, v) {
            return IR.nameExpr(`grid${g.grid.id}_matcher${id}_${v}`);
        },
    };
})(IR || (IR = {}));
var IR;
(function (IR) {
    function binaryOp(op, left, right) {
        switch (op) {
            case 'int_plus':
                if (left === IR.ZERO) {
                    return right;
                }
                if (left.kind === 'expr.op.unary' && left.op === 'int_uminus') {
                    return binaryOp('int_minus', right, left.child);
                }
                if (right === IR.ZERO) {
                    return left;
                }
                if (right.kind === 'expr.op.unary' && right.op === 'int_uminus') {
                    return binaryOp('int_minus', left, right.child);
                }
                break;
            case 'int_minus':
                if (left === IR.ZERO) {
                    return unaryOp('int_uminus', right);
                }
                if (right === IR.ZERO) {
                    return left;
                }
                if (right.kind === 'expr.op.unary' && right.op === 'int_uminus') {
                    return binaryOp('int_plus', left, right.child);
                }
                break;
            case 'float_plus':
                if (left === IR.FLOAT_ZERO) {
                    return right;
                }
                if (left.kind === 'expr.op.unary' && left.op === 'float_uminus') {
                    return binaryOp('float_minus', right, left.child);
                }
                if (right === IR.FLOAT_ZERO) {
                    return left;
                }
                if (right.kind === 'expr.op.unary' && right.op === 'float_uminus') {
                    return binaryOp('float_minus', left, right.child);
                }
                break;
            case 'float_minus':
                if (left === IR.FLOAT_ZERO) {
                    return unaryOp('float_uminus', right);
                }
                if (right === IR.FLOAT_ZERO) {
                    return left;
                }
                if (right.kind === 'expr.op.unary' && right.op === 'float_uminus') {
                    return binaryOp('float_plus', left, right.child);
                }
                break;
            case 'int_mult':
                if (left === IR.ZERO || right === IR.ZERO) {
                    return IR.ZERO;
                }
                if (left === IR.ONE) {
                    return right;
                }
                if (right === IR.ONE) {
                    return left;
                }
                if (left === IR.MINUS_ONE) {
                    return unaryOp('int_uminus', right);
                }
                if (right === IR.MINUS_ONE) {
                    return unaryOp('int_uminus', left);
                }
                break;
            case 'int_truediv':
                if (right === IR.ONE) {
                    return _unOp('int_to_fraction', left);
                }
                break;
            case 'int_floordiv':
                if (right === IR.ONE) {
                    return left;
                }
                if (right === IR.MINUS_ONE) {
                    return unaryOp('int_uminus', left);
                }
                break;
            case 'float_mult':
                // `0 * x === 0` is not strictly correct, due to infinity and NaN
                if (left === IR.FLOAT_ONE) {
                    return right;
                }
                if (right === IR.FLOAT_ONE) {
                    return left;
                }
                if (left === IR.FLOAT_MINUS_ONE) {
                    return unaryOp('float_uminus', right);
                }
                if (right === IR.FLOAT_MINUS_ONE) {
                    return unaryOp('float_uminus', left);
                }
                break;
            case 'float_truediv':
                // `0 / y === 0` is not strictly correct, due to NaN
                if (right === IR.FLOAT_ONE) {
                    return left;
                }
                if (right === IR.FLOAT_MINUS_ONE) {
                    return unaryOp('float_uminus', left);
                }
                break;
        }
        return _binOp(op, left, right);
    }
    IR.binaryOp = binaryOp;
    function unaryOp(op, child) {
        switch (op) {
            case 'bool_not':
                return IR.OP.not(child);
            case 'float_uminus':
            case 'fraction_uminus':
            case 'int_uminus':
                // optimisation for self-inverse ops
                if (child.kind === 'expr.op.unary' && child.op === op) {
                    return child.child;
                }
                break;
        }
        return _unOp(op, child);
    }
    IR.unaryOp = unaryOp;
    const OP_NEGATIONS = ((x) => x)({
        bool_eq: 'bool_ne',
        bool_ne: 'bool_eq',
        float_eq: 'float_ne',
        float_ne: 'float_eq',
        // float comparison ops cannot be swapped like this, due to NaN
        // https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN
        // TODO: disallow Infinity/NaN completely?
        /*
        float_lt: 'float_ge',
        float_le: 'float_gt',
        float_gt: 'float_le',
        float_ge: 'float_lt',
        */
        fraction_eq: 'fraction_ne',
        fraction_ne: 'fraction_eq',
        fraction_lt: 'fraction_ge',
        fraction_le: 'fraction_gt',
        fraction_gt: 'fraction_le',
        fraction_ge: 'fraction_lt',
        int_eq: 'int_ne',
        int_ne: 'int_eq',
        int_lt: 'int_ge',
        int_le: 'int_gt',
        int_gt: 'int_le',
        int_ge: 'int_lt',
        str_eq: 'str_ne',
        str_ne: 'str_eq',
    });
    function _binOp(op, left, right) {
        return { kind: 'expr.op.binary', op, left, right };
    }
    function _unOp(op, child) {
        return { kind: 'expr.op.unary', op, child };
    }
    function _isPowerOfTwo(x) {
        return x > 0 && (x & (x - 1)) === 0;
    }
    function _log2(x) {
        return IR.int(31 - Math.clz32(x));
    }
    /**
     * Functions for constructing `uint` and `bool` operations in the IR.
     */
    IR.OP = {
        and(left, right) {
            return left === IR.TRUE || right === IR.FALSE ? right
                : left === IR.FALSE || right === IR.TRUE ? left
                    : _binOp('bool_and', left, right);
        },
        or(left, right) {
            return left === IR.FALSE || right === IR.TRUE ? right
                : left === IR.TRUE || right === IR.FALSE ? left
                    : _binOp('bool_or', left, right);
        },
        not(expr) {
            if (expr.kind === 'expr.op.binary') {
                const { op } = expr;
                if (op === 'bool_and' || op === 'bool_or') {
                    // https://en.wikipedia.org/wiki/De_Morgan's_laws
                    return _binOp(op === 'bool_and' ? 'bool_or' : 'bool_and', IR.OP.not(expr.left), IR.OP.not(expr.right));
                }
                else if (objHasKey(OP_NEGATIONS, op)) {
                    return _binOp(OP_NEGATIONS[op], expr.left, expr.right);
                }
            }
            else if (expr.kind === 'expr.op.unary' && expr.op === 'bool_not') {
                return expr.child;
            }
            return _unOp('bool_not', expr);
        },
        add(left, right) {
            return left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int' ? IR.int(left.value + right.value)
                : left === IR.ZERO ? right
                    : right === IR.ZERO ? left
                        : _binOp('loose_int_plus', left, right);
        },
        addOne(expr) {
            return IR.OP.addConstant(expr, 1);
        },
        addConstant(left, right) {
            return right === 0 ? left
                : left.kind === 'expr.literal.int' ? IR.int(left.value + right)
                    : right > 0 ? IR.OP.add(left, IR.int(right))
                        : IR.OP.minus(left, IR.int(-right));
        },
        minus(left, right) {
            return left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int' ? IR.int(left.value - right.value)
                : right === IR.ZERO ? left
                    : _binOp('loose_int_minus', left, right);
        },
        minusOne(expr) {
            return IR.OP.addConstant(expr, -1);
        },
        minusConstant(left, right) {
            return IR.OP.addConstant(left, -right);
        },
        mult(left, right) {
            return left.kind === 'expr.literal.int' ? IR.OP.multConstant(right, left.value)
                : right.kind === 'expr.literal.int' ? IR.OP.multConstant(left, right.value)
                    : _binOp('loose_int_mult', left, right);
        },
        fraction(left, right) {
            return right === IR.ONE ? _unOp('int_to_fraction', left)
                : _binOp('int_truediv', left, right);
        },
        floordiv(left, right) {
            return right.kind === 'expr.literal.int' && right.value > 0 ? IR.OP.divConstant(left, right.value)
                : _binOp('loose_int_floordiv', left, right);
        },
        mod(left, right) {
            return right.kind === 'expr.literal.int' && right.value > 0 ? IR.OP.modConstant(left, right.value)
                : _binOp('loose_int_mod', left, right);
        },
        multConstant(left, right) {
            return right === 1 ? left
                : right === 0 ? IR.ZERO
                    : _isPowerOfTwo(right) ? IR.OP.lshift(left, _log2(right))
                        : right > 0 ? _binOp('loose_int_mult', IR.int(right), left)
                            : fail();
        },
        /**
         * For packing two numbers into one int; requires `0 <= y < scale`.
         */
        multAddConstant(x, scale, y) {
            return _isPowerOfTwo(scale) ? IR.OP.bitwiseXor(IR.OP.lshift(x, _log2(scale)), y)
                : scale > 0 ? IR.OP.add(IR.OP.multConstant(x, scale), y)
                    : fail();
        },
        divConstant(left, right) {
            return right === 1 ? left
                : _isPowerOfTwo(right) ? IR.OP.rshift(left, _log2(right))
                    : right > 0 ? _binOp('loose_int_floordiv', left, IR.int(right))
                        : fail();
        },
        modConstant(left, right) {
            return right === 1 ? IR.ZERO
                : _isPowerOfTwo(right) ? IR.OP.bitwiseAnd(left, IR.int(right - 1))
                    : right > 0 ? _binOp('loose_int_mod', left, IR.int(right))
                        : fail();
        },
        lshift(left, right) {
            return left === IR.ZERO || right === IR.ZERO ? left
                : _binOp('int_lshift', left, right);
        },
        rshift(left, right) {
            return left === IR.ZERO || right === IR.ZERO ? left
                : _binOp('int_rshift', left, right);
        },
        bitwiseAnd(left, right) {
            return left === IR.ZERO || right === IR.ZERO ? IR.ZERO
                : _binOp('int_and', left, right);
        },
        bitwiseOr(left, right) {
            return left === IR.ZERO ? right
                : right === IR.ZERO ? left
                    : _binOp('int_or', left, right);
        },
        bitwiseXor(left, right) {
            return left === IR.ZERO ? right
                : right === IR.ZERO ? left
                    : _binOp('int_xor', left, right);
        },
        bitwiseNot(expr) {
            return expr.kind === 'expr.op.unary' && expr.op === 'int_not' ? expr.child
                : _unOp('int_not', expr);
        },
        countTrailingZeros(expr) {
            return _unOp('int_ctz', expr);
        },
        eq(left, right) {
            if (left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value === right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_eq', left, right);
        },
        ne(left, right) {
            if (left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value !== right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_ne', left, right);
        },
        lt(left, right) {
            if (left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value < right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_lt', left, right);
        },
        le(left, right) {
            if (left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value <= right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_le', left, right);
        },
        gt(left, right) {
            if (left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value > right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_gt', left, right);
        },
        ge(left, right) {
            if (left.kind === 'expr.literal.int' && right.kind === 'expr.literal.int') {
                return left.value >= right.value ? IR.TRUE : IR.FALSE;
            }
            return _binOp('int_ge', left, right);
        },
    };
})(IR || (IR = {}));
///<reference path="../ir/ir.ts"/>
///<reference path="../ir/names.ts"/>
///<reference path="../ir/ops.ts"/>
/**
 * Compiles MJr source code to a specified target language.
 */
var Compiler;
(function (Compiler_1) {
    Compiler_1.COMPILER_VERSION = '0.1 (unstable)';
    Compiler_1.REQUIRED_RUNTIME_LIB_VERSION = 0;
    const DEFAULT_CONFIG = {
        indentSpaces: 4,
        emitChecks: true,
        entryPointName: 'main',
        animate: false,
    };
    const ALWAYS_USED_OPS = [
        'bool_and', 'bool_or', 'bool_not',
        'int_eq', 'int_ne', 'int_lt', 'int_le', 'int_gt', 'int_ge',
        'int_and', 'int_or', 'int_xor', 'int_not', 'int_lshift', 'int_rshift', 'int_ctz',
        'loose_int_plus', 'loose_int_mult', 'loose_int_floordiv', 'loose_int_mod',
    ];
    // helpers
    const { OP, NAMES: { WIDTH, HEIGHT, PARAMS, RNG, STATE, AT, AT_X, AT_Y, AT_CONV, I, P, ANY, MATCH, }, } = IR;
    /**
     * Compiles MJr source code to a high-level intermediate representation.
     */
    class Compiler {
        asg;
        config;
        diagnostics = new Diagnostics();
        cfg;
        switchNodes;
        opsUsed = new Set(ALWAYS_USED_OPS);
        internedLiterals = IDMap.withKey(entry => IR.key(entry.expr));
        grids;
        mask = new IR.Mask();
        flags;
        limits;
        matches = new IR.MatchesArray();
        variables;
        constructor(asg, config) {
            this.asg = asg;
            this.config = config;
            this.cfg = CFG.build(asg.root, config.animate);
            this.switchNodes = IDMap.ofWithKey(this.cfg.nodes.filter(CFG.isSwitchNode), node => node.id);
            this.grids = asg.grids.map(g => new IR.Grid(g));
            this.flags = new IR.Flags(this.cfg.numFlags);
            this.limits = new IR.Limits(asg.limits);
            this.variables = new IR.Variables(asg.variables);
        }
        internLiteral(expr, type) {
            const id = this.internedLiterals.getOrCreateID({ expr, type });
            return IR.NAMES.constant(id);
        }
        notSupported(s, pos) {
            this.diagnostics.compilationError(`${s} is not currently supported`, pos);
        }
        compile() {
            // YYYY-MM-DD HH:mm:ss GMT+offset
            const date = new Date().toLocaleString('en-SE', { timeZoneName: 'short' });
            const { params, endGridID } = this.asg;
            const switchCases = this.switchNodes.map(node => node.kind === 'stmt.branching' ? this.compileBranchingStmtNode(node)
                : node.kind === 'stmt.nonbranching' ? this.compileNonBranchingStmtNode(node)
                    : node.kind === 'reset' ? this.compileResetNode(node)
                        : fail());
            // TODO: potentials
            const gridDecls = [];
            const gridUpdateDecls = [];
            const endGridObj = this.grids[endGridID].useObj();
            for (const g of this.grids) {
                if (g.grid.periodic) {
                    this.notSupported('periodic grid', g.grid.pos);
                }
                gridDecls.push(...g.declareVars());
                gridUpdateDecls.push(...g.matcher.declareUpdateFunc());
            }
            const mainParams = [WIDTH, HEIGHT];
            const mainParamTypes = [IR.INT_TYPE, IR.INT_TYPE];
            if (params.size > 0) {
                mainParams.push(PARAMS);
                mainParamTypes.push(IR.nullableType({
                    kind: 'dict',
                    keys: Array.from(params.keys()),
                    values: Array.from(params.values(), t => IR.nullableType(this.type(t))),
                }));
            }
            mainParams.push(RNG);
            mainParamTypes.push(IR.nullableType(IR.PRNG_TYPE));
            // need to compile everything before preamble, so that `maxScale` and `this.opsUsed` are correct
            const matchesDecl = this.matches.declare(), maskDecl = this.mask.declare(), constDecls = IR.declVars(this.internedLiterals.map((s, i) => ({ name: IR.NAMES.constant(i), type: s.type, initialiser: s.expr }))), varDecls = this.variables.declare(this), flagDecls = this.flags.declare(), limitDecls = this.limits.declare(this);
            // compute maximum grid dimensions, to ensure that arrays aren't over-allocated and loose int operations don't overflow
            // 0x3FFFFFFE is the magic number for the largest allowed array length for a LFSR
            // don't need to include mask.scale here; mask array length is at most 1/32 of any grid array length
            const maxScale = Math.max(this.matches.scale, ...this.grids.map(g => g.getScale()));
            const maxDim = IR.int(Math.sqrt(0x3FFFFFFE / maxScale) | 0);
            return IR.declFunc(IR.nameExpr(this.config.entryPointName), this.config.animate ? IR.REWRITE_INFO_TYPE : undefined, mainParams, mainParamTypes, IR.GRID_TYPE, IR.block([
                IR.comment(`compiled by mjrc-${Compiler_1.COMPILER_VERSION} on ${date}`),
                this.config.emitChecks ? IR.if_(OP.or(OP.le(WIDTH, IR.ZERO), OP.le(HEIGHT, IR.ZERO)), IR.throw_('Grid dimensions must be positive'), IR.if_(OP.or(OP.gt(WIDTH, maxDim), OP.gt(HEIGHT, maxDim)), IR.throw_(`Grid dimensions cannot exceed ${maxDim.value}`))) : IR.PASS,
                IR.preamble(this.dictType(params), this.config.emitChecks, Compiler_1.REQUIRED_RUNTIME_LIB_VERSION, Array.from(this.opsUsed)),
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
        compileBranchingStmtNode(node) {
            const { stmt } = node;
            const ifTrue = this.goto(node.id, node.ifChanged.nodeID), ifFalse = this.goto(node.id, node.then.nodeID), eitherWay = [];
            while (ifTrue.length > 0 && ifFalse.length > 0 && IR.equals(ifTrue[ifTrue.length - 1], ifFalse[ifFalse.length - 1])) {
                const s = ifTrue.pop();
                ifFalse.pop();
                eitherWay.push(s);
            }
            const f = STMT_COMPILE_FUNCS[stmt.kind];
            return IR.block([
                IR.comment(`${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                f(this, stmt, IR.block(ifTrue), IR.block(ifFalse)),
                ...eitherWay,
            ]);
        }
        compileNonBranchingStmtNode(node) {
            const { stmt } = node;
            const f = STMT_COMPILE_FUNCS[stmt.kind];
            return IR.block([
                IR.comment(`${stmt.kind} at line ${stmt.pos.line}, col ${stmt.pos.col}`),
                f(this, stmt),
                ...this.goto(node.id, node.then.nodeID),
            ]);
        }
        compileResetNode(node) {
            const out = [IR.comment(`reset ${node.stmt.kind} at line ${node.stmt.pos.line}, col ${node.stmt.pos.col}`)];
            if (node.reset !== undefined) {
                const { limitIDs } = node.reset;
                out.push(...limitIDs.map(limitID => this.limits.reset(limitID, this)));
            }
            out.push(this.flags.clear(node.flagID), ...this.goto(node.id, node.then.nodeID));
            return IR.block(out);
        }
        expr(expr) {
            const f = EXPR_COMPILE_FUNCS[expr.kind];
            return f(this, expr);
        }
        literal(c, pos) {
            switch (c.kind) {
                case 'bool': return c.value ? IR.TRUE : IR.FALSE;
                case 'float': return IR.float(c.value);
                case 'int': return IR.int(c.value);
                case 'str': return IR.str(c.value);
                case 'dict': {
                    const type = this.dictType(c.type.entryTypes);
                    const values = Array.from(type.keys, k => this.literal(c.value.get(k), pos));
                    return this.internLiteral(IR.dict(type, values), type);
                }
                case 'fraction': {
                    const expr = OP.fraction(IR.int(c.value.p), IR.int(c.value.q));
                    this.opsUsed.add(expr.op);
                    return this.internLiteral(expr, IR.FRACTION_TYPE);
                }
                case 'grid': {
                    return this.grids[c.value].useObj();
                }
                case 'pattern.in': {
                    this.notSupported('input pattern in non-constant expression', pos);
                    return IR.NULL;
                }
                case 'pattern.out': {
                    const { width, height, pattern } = c.value;
                    const patternExpr = IR.constArray(pattern, 256, width);
                    return this.internLiteral(IR.libConstructorCall('Pattern', [IR.int(width), IR.int(height), patternExpr]), IR.PATTERN_TYPE);
                }
                case 'position': {
                    const { x, y, inGrid } = c.value;
                    const g = this.grids[inGrid];
                    return this.internLiteral(g.checkedIndex(IR.int(x), IR.int(y)), IR.INT_TYPE);
                }
            }
        }
        type(type) {
            return type.kind === 'dict' ? this.dictType(type.entryTypes)
                : type.kind !== 'pattern.in' ? TYPES_TO_IR[type.kind]
                    : fail();
        }
        dictType(entryTypes) {
            const keys = Array.from(entryTypes.keys()).sort();
            // TODO: declare type aliases for dict types
            return { kind: 'dict', keys, values: keys.map(k => this.type(entryTypes.get(k))) };
        }
        goto(fromNodeID, toNodeID) {
            const initial = fromNodeID < 0;
            const out = [];
            const { nodes } = this.cfg;
            let cur;
            while (true) {
                cur = nodes[toNodeID];
                if (cur.kind === 'stmt.branching' || cur.kind === 'stmt.nonbranching' || (cur.kind === 'reset' && cur.reset !== undefined) || cur.kind === 'stop') {
                    break;
                }
                else if (cur.kind === 'checklimit') {
                    if (initial) {
                        toNodeID = cur.ifTrue.nodeID;
                        continue;
                    }
                    break;
                }
                else if (cur.kind === 'checkflag' || cur.kind === 'reset') {
                    if (initial) {
                        toNodeID = cur.then.nodeID;
                        continue;
                    }
                    break;
                }
                else if (cur.kind === 'decrementlimit') {
                    if (initial) {
                        fail();
                    }
                    out.push(this.limits.decrement(cur.limitID));
                }
                else if (cur.kind === 'setflag') {
                    if (initial) {
                        fail();
                    }
                    out.push(this.flags.set(cur.flagID));
                }
                toNodeID = cur.then.nodeID;
            }
            if (cur.kind === 'checkflag' || cur.kind === 'checklimit') {
                if (initial) {
                    fail();
                }
                out.push(IR.if_(cur.kind === 'checkflag' ? this.flags.check(cur.flagID) : this.limits.check(cur.limitID), IR.block(this.goto(fromNodeID, cur.ifTrue.nodeID)), IR.block(this.goto(fromNodeID, cur.then.nodeID))));
            }
            else {
                const nextState = IR.int(cur.kind === 'stop' ? -1 : this.switchNodes.getID(cur));
                if (initial) {
                    out.push(IR.declVar(STATE, IR.INT_TYPE, nextState, true));
                }
                else if (fromNodeID !== toNodeID) {
                    out.push(IR.assign(STATE, '=', nextState));
                }
            }
            return out;
        }
    }
    const TYPES_TO_IR = {
        bool: IR.BOOL_TYPE,
        float: IR.FLOAT_TYPE,
        fraction: IR.FRACTION_TYPE,
        grid: IR.GRID_TYPE,
        int: IR.INT_TYPE,
        'pattern.out': IR.PATTERN_TYPE,
        position: IR.INT_TYPE,
        str: IR.STR_TYPE,
    };
    const EXPR_COMPILE_FUNCS = {
        'expr.attr.dict': (c, expr) => IR.attr(c.expr(expr.left), expr.attr),
        'expr.attr.grid': (c, expr) => c.grids[expr.grid].attr(expr.attr),
        'expr.attr.position': (c, expr) => {
            const g = c.grids[expr.left.type.inGrid];
            // optimise common cases
            if (expr.left.kind === 'expr.name.keyword') {
                switch (expr.left.name) {
                    case 'at':
                        return { x: AT_X, y: AT_Y }[expr.attr];
                    case 'origin':
                        return { x: g.originX, y: g.originY }[expr.attr];
                }
            }
            const pos = c.expr(expr.left);
            switch (expr.attr) {
                case 'x': return OP.mod(pos, g.width);
                case 'y': return OP.floordiv(pos, g.width);
            }
            // exhaustivity check
            const _ = expr.attr;
        },
        'expr.constant': (c, expr) => c.literal(expr.constant, expr.pos),
        'expr.count': (c, expr) => c.grids[expr.inGrid].makeCounter(expr.patterns),
        'expr.decl': (c, expr) => {
            // TODO: get rid of `expr.decl` in ASG and `expr.letin` in IR, by hoisting assign statements?
            const decls = [];
            let cur = expr;
            while (cur.kind === 'expr.decl') {
                const { variable, rhs } = cur.decl;
                if (variable.references > 0) {
                    decls.push({
                        name: c.variables.name(variable.id),
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
            return IR.dict(type, type.keys.map(k => c.expr(expr.entryExprs.get(k))));
        },
        'expr.name.keyword': (c, expr) => {
            switch (expr.name) {
                case 'at':
                    return AT;
                case 'origin':
                    return expr.type.kind === 'position'
                        ? c.grids[expr.type.inGrid].useOrigin()
                        : fail();
                case 'random':
                    return IR.libMethodCall('PRNG', 'nextDouble', RNG, []);
            }
        },
        'expr.name.simple': (c, expr) => c.variables.name(expr.variableID),
        'expr.op.binary': (c, expr) => {
            const r = IR.binaryOp(expr.op, c.expr(expr.left), c.expr(expr.right));
            // `IR.binaryOp` may optimise e.g. `0 - x` to `-x`
            if (r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') {
                c.opsUsed.add(r.op);
            }
            return r;
        },
        'expr.op.ternary': (c, expr) => IR.ternary(c.expr(expr.condition), c.expr(expr.then), c.expr(expr.otherwise)),
        'expr.op.unary': (c, expr) => {
            const r = IR.unaryOp(expr.op, c.expr(expr.child));
            // `IR.unaryOp` may optimise e.g. `not x == y` to `x != y`
            if (r.kind === 'expr.op.binary' || r.kind === 'expr.op.unary') {
                c.opsUsed.add(r.op);
            }
            return r;
        },
        'expr.param': (c, expr) => IR.param(expr.name, c.expr(expr.otherwise)),
        'expr.randint': (c, expr) => {
            const max = c.expr(expr.max);
            return max.kind !== 'expr.literal.int' ? IR.libFunctionCall('nextIntChecked', [RNG, max])
                : max.value > 0 ? IR.libMethodCall('PRNG', 'nextInt', RNG, [max])
                    : fail();
        },
        'expr.sum': (c, expr) => {
            const g = c.grids[expr.inGrid];
            const p = g.grid.convPatterns.getByID(expr.patternID);
            return g.makeConvBuffer(p).get(p.chars);
        },
    };
    const STMT_COMPILE_FUNCS = {
        // non-branching
        'stmt.assign': (c, stmt) => IR.assign(c.variables.name(stmt.variable.id), '=', c.expr(stmt.rhs)),
        'stmt.log': (c, stmt) => IR.log(c.expr(stmt.expr)),
        'stmt.rules.map': _stmtNotSupported,
        'stmt.put': (c, stmt) => {
            const g = c.grids[stmt.inGrid];
            const { pattern } = stmt;
            // check whether pattern is effective
            let isEffective;
            if (pattern.kind === 'expr.constant') {
                const checks = pattern.constant.value.map((dx, dy, c) => OP.ne(g.data.get(g.relativeIndex(dx, dy)), IR.int(c)));
                isEffective = checks.length > 0 ? checks.reduce(OP.or) : fail();
            }
            else {
                // non-constant pattern will be checked in _writeCondition
                isEffective = IR.TRUE;
            }
            return IR.block([
                g.declareAtIndex(c.expr(stmt.at)),
                // check bounds, given size of pattern
                g.grid.periodic ? IR.PASS : IR.if_(OP.or(pattern.type.width > 1 ? OP.ge(OP.addConstant(AT_X, pattern.type.width - 1), g.width) : IR.FALSE, pattern.type.height > 1 ? OP.ge(OP.addConstant(AT_Y, pattern.type.height - 1), g.height) : IR.FALSE), IR.throw_('pattern would be out of bounds')),
                pattern.kind !== 'expr.constant' ? IR.declVar(P, IR.PATTERN_TYPE, c.expr(pattern)) : IR.PASS,
                IR.if_(OP.and(isEffective, _writeCondition(c, g, pattern, P, stmt.condition)), _doWrite(c, g, undefined, pattern, false, undefined, true, c.config.animate)),
            ]);
        },
        'stmt.use': (c, stmt) => {
            return c.config.animate ? c.grids[stmt.grid].yield_() : fail();
        },
        // branching
        'stmt.convchain': _stmtNotSupported,
        'stmt.path': _stmtNotSupported,
        'stmt.rules.basic.all': _basicAllPrl,
        'stmt.rules.basic.one': _basicOne,
        'stmt.rules.basic.prl': _basicAllPrl,
        'stmt.rules.biased.all': _stmtNotSupported,
        'stmt.rules.biased.one': _stmtNotSupported,
        'stmt.rules.convolution': _stmtConvolution,
        'stmt.rules.search.all': _stmtNotSupported,
        'stmt.rules.search.one': _stmtNotSupported,
    };
    function _stmtNotSupported(c, stmt) {
        c.notSupported(`'${stmt.kind}'`, stmt.pos);
        return IR.PASS;
    }
    function _doWrite(c, outGrid, from, to, useMask, flagVar, doUpdate, doYield) {
        const out = [];
        let mX, mY, eW, eH;
        if (to.kind === 'expr.constant') {
            // constant output pattern
            const { value } = to.constant;
            let minX = value.width, maxX = 0, minY = value.height, maxY = 0;
            value.forEach((dx, dy, colour) => {
                const maybeEffective = from === undefined || from.kind !== 'leaf' || from.pattern[dx + from.width * dy] !== colour;
                if (useMask || maybeEffective) {
                    out.push(outGrid.write(outGrid.relativeIndex(dx, dy), colour, useMask ? c.mask : undefined));
                }
                if (maybeEffective) {
                    minX = Math.min(minX, dx);
                    maxX = Math.max(maxX, dx + 1);
                    minY = Math.min(minY, dy);
                    maxY = Math.max(maxY, dy + 1);
                }
            });
            if (minX > maxX || minY > maxY) {
                fail();
            }
            mX = IR.int(minX);
            mY = IR.int(minY);
            eW = IR.int(maxX - minX);
            eH = IR.int(maxY - minY);
        }
        else {
            // output pattern determined at runtime
            // this requires that `p` is already declared
            out.push(IR.libMethodCallStmt('Pattern', 'put', P, [outGrid.useObj(), useMask ? c.mask.name : IR.NULL, AT_X, AT_Y]));
            mX = IR.attr(P, 'minX');
            mY = IR.attr(P, 'minY');
            eW = IR.attr(P, 'effectiveWidth');
            eH = IR.attr(P, 'effectiveHeight');
        }
        if (doUpdate) {
            out.push(...outGrid.update(OP.add(AT_X, mX), OP.add(AT_Y, mY), eW, eH, doYield));
        }
        if (flagVar !== undefined) {
            out.push(IR.assign(flagVar, '=', IR.TRUE));
        }
        return IR.block(out);
    }
    function _writeCondition(c, g, patternExpr, patternVar, conditionExpr) {
        const hasEffect = patternExpr === undefined || patternExpr.kind === 'expr.constant'
            ? IR.TRUE
            : IR.libMethodCall('Pattern', 'hasEffect', patternVar ?? fail(), [g.useObj(), AT_X, AT_Y]);
        return conditionExpr === undefined ? hasEffect : OP.and(hasEffect, c.expr(conditionExpr));
    }
    function _basicOne(c, stmt, ifChanged, then) {
        const { rewrites } = stmt;
        const g = c.grids[stmt.inGrid];
        const sampler = g.makeSampler(rewrites.map(rule => rule.from));
        const writeConditions = rewrites.map(rule => _writeCondition(c, g, rule.to, P, rule.condition));
        // optimisation for common case: all rewrites are unconditional and definitely effective
        const allUnconditionalAndEffective = writeConditions.every(c => c === IR.TRUE);
        const cases = rewrites.map((rule, i) => IR.block([
            rule.to.kind !== 'expr.constant' ? IR.declVar(P, IR.PATTERN_TYPE, c.expr(rule.to)) : IR.PASS,
            IR.if_(writeConditions[i], _doWrite(c, g, rule.from, rule.to, false, allUnconditionalAndEffective ? undefined : ANY, true, c.config.animate)),
        ]));
        return allUnconditionalAndEffective
            ? IR.if_(OP.gt(sampler.count, IR.ZERO), IR.block([
                sampler.sampleWithReplacement(cases),
                ifChanged,
            ]), then)
            : IR.block([
                sampler.beginSamplingWithoutReplacement(),
                c.matches.declareCount(sampler.count, true),
                IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true),
                IR.while_(OP.and(c.matches.isNotEmpty, OP.not(ANY)), IR.block([
                    sampler.sampleWithoutReplacement(cases, c.matches.count),
                    c.matches.decrementCount,
                ])),
                IR.if_(ANY, ifChanged, then),
            ]);
    }
    function _exprIs(expr, flags) {
        return (expr.flags & flags) === flags;
    }
    function _basicAllPrl(c, stmt, ifChanged, then) {
        const { rewrites } = stmt;
        const g = c.grids[stmt.inGrid];
        const k = rewrites.length;
        const out = [];
        const conditionIsSameEverywhere = rewrites.map(rule => _exprIs(rule.condition, 12 /* ExprFlags.SAME_EVERYWHERE */)
            && rule.to.kind === 'expr.constant');
        const outPatternIsConstant = rewrites.map(rule => rule.to.kind === 'expr.constant');
        const outPatternIsSameEverywhere = rewrites.map(rule => _exprIs(rule.to, 12 /* ExprFlags.SAME_EVERYWHERE */));
        // TODO: if rule.to is grid-dependent and not same-everywhere, need to do rewrites on a temporary buffer then copy over afterwards
        const patternIsGridDependent = rewrites.map(rule => !_exprIs(rule.to, 16 /* ExprFlags.GRID_INDEPENDENT */));
        rewrites.forEach((rule, i) => {
            if (patternIsGridDependent[i] && !outPatternIsSameEverywhere[i]) {
                c.notSupported('output pattern dependent on both grid state and match position', rule.pos);
            }
        });
        c.matches.use(g, k);
        const useMask = stmt.kind === 'stmt.rules.basic.all' && (!stmt.commutative || rewrites.some(rule => {
            if (rule.to.kind === 'expr.constant') {
                const { value } = rule.to.constant;
                return value.effectiveWidth > 1 || value.effectiveHeight > 1;
            }
            else {
                const { type } = rule.to;
                return type.width > 1 || type.height > 1;
            }
        }));
        if (useMask) {
            c.mask.use(g);
        }
        const shuffle = useMask || !stmt.commutative;
        out.push(IR.declVars(rewrites.flatMap((rule, i) => !outPatternIsConstant[i] && outPatternIsSameEverywhere[i]
            ? [{ name: IR.NAMES.tmpPattern(i), type: IR.PATTERN_TYPE, initialiser: c.expr(rule.to) }]
            : [])));
        const firstPassConditions = rewrites.map((rule, i) => _writeCondition(c, g, outPatternIsSameEverywhere[i] ? rule.to : undefined, IR.NAMES.tmpPattern(i), rule.condition));
        const secondPassConditions = rewrites.map((rule, i) => _writeCondition(c, g, outPatternIsSameEverywhere[i] ? undefined : rule.to, P, undefined));
        // if any second-pass conditions do more than just check the mask, use a flag for whether any rewrites were done
        // but no flag needed if this statement isn't branching anyway
        const useFlag = (ifChanged !== IR.PASS || then !== IR.PASS) && outPatternIsSameEverywhere.includes(false);
        // optimisation for common case: all rewrites are unconditional and definitely effective
        if (firstPassConditions.every(c => c === IR.TRUE)) {
            const sampler = g.makeSampler(rewrites.map(rule => rule.from));
            out.push(...sampler.copyInto(c.matches, shuffle));
        }
        else {
            out.push(c.matches.declareCount(IR.ZERO, true));
            for (let i = 0; i < k; ++i) {
                const rule = rewrites[i];
                const sampler = g.makeSampler([rule.from]);
                const condition = firstPassConditions[i];
                const addMatch = c.matches.add(OP.multAddConstant(AT, k, IR.int(i)), shuffle);
                out.push(
                // if condition is same-everywhere, then we only need to check it once for all matches of this rule
                conditionIsSameEverywhere[i]
                    ? IR.if_(condition, sampler.forEach(addMatch))
                    // otherwise, need to check the condition separately for each match
                    : sampler.forEach([IR.if_(condition, IR.block(addMatch))]));
            }
        }
        const doWrites = rewrites.map((rule, i) => IR.block([
            outPatternIsConstant[i] ? IR.PASS : IR.declVar(P, IR.PATTERN_TYPE, 
            // TODO: `c.expr(rule.to)` is only correct when `rule.to` is grid-independent (see above)
            outPatternIsSameEverywhere[i] ? IR.NAMES.tmpPattern(i) : c.expr(rule.to)),
            IR.if_(OP.and(secondPassConditions[i], useMask ? c.mask.patternFits(g, rule.to) : IR.TRUE), _doWrite(c, g, rule.from, rule.to, useMask, useFlag ? ANY : undefined, true, false)),
        ]));
        if (c.config.animate) {
            ifChanged = IR.block([g.yield_(), ifChanged]);
        }
        out.push(useFlag ? IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true) : IR.PASS, IR.if_(c.matches.isNotEmpty, IR.block([
            useMask ? c.mask.clear(g) : IR.PASS,
            IR.forRange(I, IR.ZERO, c.matches.count, [
                IR.declVar(MATCH, IR.INT_TYPE, c.matches.get(I)),
                g.declareAtIndex(OP.divConstant(MATCH, k)),
                IR.switch_(OP.modConstant(MATCH, k), doWrites),
            ]),
            useFlag ? IR.PASS : ifChanged,
        ]), useFlag ? undefined : then), useFlag ? IR.if_(ANY, ifChanged, then) : IR.PASS);
        return IR.block(out);
    }
    function _stmtConvolution(c, stmt, ifChanged, then) {
        const { kernel } = stmt;
        const g = c.grids[stmt.inGrid];
        const cases = emptyArray(g.grid.alphabet.key.length, IR.PASS);
        for (const rule of stmt.rewrites) {
            const mask = rule.from.kind === 'leaf' ? rule.from.masks[0]
                : rule.from.kind === 'top' ? g.grid.alphabet.wildcard
                    : fail();
            const caseHandler = IR.if_(_writeCondition(c, g, rule.to, undefined, rule.condition), _doWrite(c, g, rule.from, rule.to, false, ANY, false, false));
            ISet.forEach(mask, i => {
                if (cases[i] !== IR.PASS) {
                    fail();
                }
                cases[i] = caseHandler;
            });
        }
        const bufferWidth = OP.addConstant(g.width, kernel.width - 1), atConvInitialiser = OP.add(OP.addConstant(AT_X, kernel.centreX), OP.mult(OP.addConstant(AT_Y, kernel.centreY), bufferWidth));
        // TODO: different strategy if rules don't cover the whole alphabet?
        return IR.block([
            IR.declVar(ANY, IR.BOOL_TYPE, IR.FALSE, true),
            IR.forRange(AT_Y, IR.ZERO, g.height, [IR.forRange(AT_X, IR.ZERO, g.width, [
                    g.declareAtXY(AT_X, AT_Y),
                    IR.declVar(AT_CONV, IR.INT_TYPE, atConvInitialiser),
                    IR.switch_(g.data.get(AT), cases),
                ])]),
            IR.if_(ANY, IR.block([
                // TODO: this is suboptimal, but need to defer update until all `sum` expressions are evaluated
                ...g.update(IR.ZERO, IR.ZERO, g.width, g.height, c.config.animate),
                ifChanged,
            ]), then),
        ]);
    }
    function compile(src, targetLanguage, config) {
        const ast = Parser.parse(src);
        const asg = Resolver.resolve(ast);
        const compiler = new Compiler(asg, config !== undefined ? { ...DEFAULT_CONFIG, ...config } : DEFAULT_CONFIG);
        const result = compiler.compile();
        compiler.diagnostics.throwIfAnyErrors();
        const cls = CodeGen[targetLanguage];
        const gen = new cls(compiler.config);
        gen.writeStmt(result);
        gen.diagnostics.throwIfAnyErrors();
        return gen.render();
    }
    Compiler_1.compile = compile;
})(Compiler || (Compiler = {}));
class Diagnostics {
    static MAX_ERRORS = 100;
    errors = [];
    throwIfAnyErrors() {
        if (this.errors.length > 0) {
            throw this;
        }
    }
    error(prefix, msg, pos) {
        if (pos !== undefined) {
            msg += ` at line ${pos.line}, col ${pos.col}`;
        }
        this.errors.push(`${prefix}: ${msg}`);
        if (this.errors.length >= Diagnostics.MAX_ERRORS) {
            throw this;
        }
    }
    configError(msg) {
        this.error('Configuration error', msg);
    }
    syntaxError(msg, pos) {
        this.error('Syntax error', msg, pos);
    }
    compilationError(msg, pos) {
        this.error('Compilation error', msg, pos);
    }
    typeError(msg, pos) {
        this.error('Type error', msg, pos);
    }
}
var MJr;
(function (MJr) {
    // MJr runtime version 0 is unstable!
    MJr.VERSION = 0;
    MJr.DIV_ZERO_MESSAGE = 'division by zero';
    function checkZero(y) {
        if (y === 0) {
            throw new Error(MJr.DIV_ZERO_MESSAGE);
        }
    }
    function modulo(x, y) {
        return x - y * Math.floor(x / y);
    }
    function fraction(p, q) {
        checkZero(q);
        // Euclid's algorithm
        let x = p, y = q;
        while (y !== 0) {
            x %= y;
            const tmp = x;
            x = y;
            y = tmp;
        }
        return { p: p / x, q: q / x };
    }
    MJr.fraction = fraction;
    MJr.OPS = {
        float_mod: modulo,
        float_checkzero: checkZero,
        fraction_plus: (x, y) => fraction(x.p * y.q + y.p * x.q, x.q * y.q),
        fraction_minus: (x, y) => fraction(x.p * y.q - y.p * x.q, x.q * y.q),
        fraction_mult: (x, y) => fraction(x.p * y.p, x.q * y.q),
        fraction_truediv: (x, y) => fraction(x.p * y.q, x.q * y.p),
        fraction_uminus: (x) => ({ p: -x.p, q: x.q }),
        fraction_eq: (x, y) => x.p === y.p && x.q === y.q,
        fraction_ne: (x, y) => x.p !== y.p || x.q !== y.q,
        fraction_lt: (x, y) => x.p * y.q < y.p * x.q,
        fraction_le: (x, y) => x.p * y.q <= y.p * x.q,
        fraction_gt: (x, y) => x.p * y.q > y.p * x.q,
        fraction_ge: (x, y) => x.p * y.q >= y.p * x.q,
        fraction_to_str: (x) => x.q === 1 ? `${x.p}` : `${x.p}/${x.q}`,
        int_truediv: fraction,
        int_floordiv: (x, y) => Math.floor(x / y) | 0,
        int_mod: (x, y) => modulo(x, y) | 0,
        int_ctz: (x) => 31 - Math.clz32(x & -x),
        int_checkzero: checkZero,
        int_to_fraction: (x) => ({ p: x, q: 1 }),
    };
    MJr.HEX = {
        u8(s) {
            const arr = new Uint8Array(s.length >> 1);
            for (let i = 0; i < s.length; i += 2) {
                arr[i >> 1] = parseInt(s.substring(i, i + 2), 16);
            }
            return arr;
        },
        u16(s) {
            const arr = new Uint16Array(s.length >> 2);
            for (let i = 0; i < s.length; i += 4) {
                arr[i >> 2] = parseInt(s.substring(i, i + 4), 16);
            }
            return arr;
        },
        u32(s) {
            const arr = new Uint32Array(s.length >> 3);
            for (let i = 0; i < s.length; i += 8) {
                arr[i >> 3] = parseInt(s.substring(i, i + 8), 16);
            }
            return arr;
        },
    };
    MJr.DEFAULT_PRNG = {
        nextDouble: Math.random,
        nextInt: n => (Math.random() * n) | 0,
    };
    MJr.SAMPLE_EMPTY_MESSAGE = 'sample from empty range';
    function nextIntChecked(rng, n) {
        if (n <= 0) {
            throw new Error(MJr.SAMPLE_EMPTY_MESSAGE);
        }
        return rng.nextInt(n);
    }
    MJr.nextIntChecked = nextIntChecked;
    function lfsrFeedbackTerm(n) {
        // http://users.ece.cmu.edu/~koopman/lfsr/
        if (n < 0xFF) {
            return 0xA6;
        }
        else if (n < 0x3FF) {
            return 0x344;
        }
        else if (n < 0xFFF) {
            return 0xAF5;
        }
        else if (n < 0x3FFF) {
            return 0x243F;
        }
        else if (n < 0xFFFF) {
            return 0x8580;
        }
        else if (n < 0x3FFFF) {
            return 0x204C9;
        }
        else if (n < 0xFFFFF) {
            return 0x80534;
        }
        else if (n < 0x3FFFFF) {
            return 0x200634;
        }
        else if (n < 0xFFFFFF) {
            return 0x8009F8;
        }
        else if (n < 0x3FFFFFF) {
            return 0x20006B9;
        }
        else if (n < 0xFFFFFFF) {
            return 0x8000893;
        }
        else {
            return 0x20000A46;
        }
    }
    MJr.lfsrFeedbackTerm = lfsrFeedbackTerm;
    class Grid {
        width;
        height;
        data;
        alphabet;
        constructor(width, height, data, alphabet) {
            this.width = width;
            this.height = height;
            this.data = data;
            this.alphabet = alphabet;
        }
        index(x, y) {
            if (x < 0 || x >= this.width || y < 0 || y >= this.height) {
                throw new Error(`position out of bounds: (${x}, ${y})`);
            }
            return x + y * this.width;
        }
        wrapIndex(x, y) {
            return MJr.OPS.int_mod(x, this.width) + MJr.OPS.int_mod(y, this.height) * this.width;
        }
        toString() {
            const { width, data, alphabet } = this;
            const out = [];
            for (let i = 0; i < data.length; ++i) {
                if (i > 0 && i % width === 0) {
                    out.push('\n');
                }
                out.push(alphabet[data[i]]);
            }
            return out.join('');
        }
    }
    MJr.Grid = Grid;
    class Pattern {
        width;
        height;
        pattern;
        vectorData;
        minX;
        minY;
        effectiveWidth;
        effectiveHeight;
        constructor(width, height, pattern) {
            this.width = width;
            this.height = height;
            this.pattern = pattern;
            let minX = width, minY = height, maxX = 0, maxY = 0;
            const v = [];
            for (let y = 0; y < height; ++y) {
                for (let x = 0; x < width; ++x) {
                    const c = pattern[x + width * y];
                    if (c < 0) {
                        continue;
                    }
                    v.push(x, y, c);
                    minX = Math.min(minX, x);
                    minY = Math.min(minY, y);
                    maxX = Math.max(maxX, x + 1);
                    maxY = Math.max(maxY, y + 1);
                }
            }
            this.vectorData = v;
            this.minX = minX;
            this.minY = minY;
            this.effectiveWidth = Math.max(maxX - minX, 0);
            this.effectiveHeight = Math.max(maxY - minY, 0);
        }
        fitsMask(grid, mask, atX, atY) {
            const v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                if ((mask[index >> 5] & 1 << (index & 31)) !== 0) {
                    return false;
                }
            }
            return true;
        }
        hasEffect(grid, atX, atY) {
            const g = grid.data, v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                if (g[index] !== v[i + 2]) {
                    return true;
                }
            }
            return false;
        }
        put(grid, mask, atX, atY) {
            const g = grid.data, v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                g[index] = v[i + 2];
                if (mask !== undefined) {
                    mask[index >> 5] |= 1 << (index & 31);
                }
            }
        }
    }
    MJr.Pattern = Pattern;
    class RewriteInfo {
        grid;
        x;
        y;
        width;
        height;
        constructor(grid, x, y, width, height) {
            this.grid = grid;
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
        }
    }
    MJr.RewriteInfo = RewriteInfo;
    class Sampler {
        arr;
        indices;
        count = 0;
        constructor(domainSize) {
            const arr = new Uint32Array(domainSize);
            const indices = new Uint32Array(domainSize);
            for (let i = 0; i < domainSize; ++i) {
                arr[i] = indices[i] = i;
            }
            this.arr = arr;
            this.indices = indices;
        }
        copyInto(out) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                out[i] = arr[i];
            }
        }
        shuffleInto(out, rng) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                const j = rng.nextInt(i + 1);
                out[i] = out[j];
                out[j] = arr[i];
            }
        }
        has(x) {
            return this.indices[x] < this.count;
        }
        add(x) {
            const { arr, indices, count } = this;
            const i = indices[x];
            if (i >= count) {
                const j = count;
                const y = arr[j];
                arr[j] = x;
                indices[x] = j;
                arr[i] = y;
                indices[y] = i;
                ++this.count;
            }
        }
        del(x) {
            const { arr, indices, count } = this;
            const i = indices[x];
            if (i < count) {
                const j = count - 1;
                const y = arr[j];
                arr[j] = x;
                indices[x] = j;
                arr[i] = y;
                indices[y] = i;
                --this.count;
            }
        }
        sample(max, rng) {
            const { arr, indices } = this;
            const i = rng.nextInt(max);
            const j = max - 1;
            const x = arr[i];
            const y = arr[j];
            arr[i] = y;
            indices[y] = i;
            arr[j] = x;
            indices[x] = j;
            return x;
        }
    }
    MJr.Sampler = Sampler;
})(MJr || (MJr = {}));
///<reference path="../runtime/mjr.ts"/>
var Op;
(function (Op) {
    Op.UNARY_FUNCS = {
        bool_not: x => !x,
        float_uminus: x => -x,
        fraction_uminus: x => ({ p: -x.p, q: x.q }),
        int_uminus: x => (-x) | 0,
        int_to_float: x => +x,
        int_to_fraction: MJr.OPS.int_to_fraction,
        bool_to_str: x => x.toString(),
        float_to_str: x => x.toString(),
        fraction_to_str: MJr.OPS.fraction_to_str,
        int_to_str: x => x.toString(),
    };
    Op.BINARY_FUNCS = ((funcs) => funcs)({
        bool_and: (x, y) => x && y,
        bool_or: (x, y) => x || y,
        bool_eq: (x, y) => x === y,
        bool_ne: (x, y) => x !== y,
        float_plus: (x, y) => x + y,
        float_minus: (x, y) => x - y,
        float_mult: (x, y) => x * y,
        float_truediv: (x, y) => x / y,
        float_mod: MJr.OPS.float_mod,
        float_eq: (x, y) => x === y,
        float_ne: (x, y) => x !== y,
        float_lt: (x, y) => x < y,
        float_le: (x, y) => x <= y,
        float_gt: (x, y) => x > y,
        float_ge: (x, y) => x >= y,
        fraction_plus: MJr.OPS.fraction_plus,
        fraction_minus: MJr.OPS.fraction_minus,
        fraction_mult: MJr.OPS.fraction_mult,
        fraction_truediv: MJr.OPS.fraction_truediv,
        fraction_eq: MJr.OPS.fraction_eq,
        fraction_ne: MJr.OPS.fraction_ne,
        fraction_lt: MJr.OPS.fraction_lt,
        fraction_le: MJr.OPS.fraction_le,
        fraction_gt: MJr.OPS.fraction_gt,
        fraction_ge: MJr.OPS.fraction_ge,
        int_plus: (x, y) => (x + y) | 0,
        int_minus: (x, y) => (x - y) | 0,
        int_mult: Math.imul,
        int_truediv: MJr.OPS.int_truediv,
        int_floordiv: MJr.OPS.int_floordiv,
        int_mod: MJr.OPS.int_mod,
        int_eq: (x, y) => x === y,
        int_ne: (x, y) => x !== y,
        int_lt: (x, y) => x < y,
        int_le: (x, y) => x <= y,
        int_gt: (x, y) => x > y,
        int_ge: (x, y) => x >= y,
        str_concat: (x, y) => x + y,
        str_eq: (x, y) => x === y,
        str_ne: (x, y) => x !== y,
    });
    Op.UNARY_OP_TYPES = {
        '-': [['int', 'int', 'int_uminus'], ['float', 'float', 'float_uminus']],
        'not': [['bool', 'bool', 'bool_not']],
    };
    Op.BINARY_OP_TYPES = {
        '+': [['int', 'int', 'int', 'int_plus'], ['float', 'float', 'float', 'float_plus'], ['fraction', 'fraction', 'fraction', 'fraction_plus'], ['str', 'str', 'str', 'str_concat']],
        '-': [['int', 'int', 'int', 'int_minus'], ['float', 'float', 'float', 'float_minus'], ['fraction', 'fraction', 'fraction', 'fraction_minus']],
        '*': [['int', 'int', 'int', 'int_mult'], ['float', 'float', 'float', 'float_mult'], ['fraction', 'fraction', 'fraction', 'fraction_mult']],
        '/': [['int', 'int', 'fraction', 'int_truediv'], ['float', 'float', 'float', 'float_truediv'], ['fraction', 'fraction', 'fraction', 'fraction_truediv']],
        '//': [['int', 'int', 'int', 'int_floordiv']],
        '%': [['int', 'int', 'int', 'int_mod'], ['float', 'float', 'float', 'float_mod']],
        '==': [['int', 'int', 'bool', 'int_eq'], ['float', 'float', 'bool', 'float_eq'], ['fraction', 'fraction', 'bool', 'fraction_eq'], ['str', 'str', 'bool', 'str_eq']],
        '!=': [['int', 'int', 'bool', 'int_ne'], ['float', 'float', 'bool', 'float_ne'], ['fraction', 'fraction', 'bool', 'fraction_ne'], ['str', 'str', 'bool', 'str_ne']],
        '<': [['int', 'int', 'bool', 'int_lt'], ['float', 'float', 'bool', 'float_lt'], ['fraction', 'fraction', 'bool', 'fraction_lt']],
        '<=': [['int', 'int', 'bool', 'int_le'], ['float', 'float', 'bool', 'float_le'], ['fraction', 'fraction', 'bool', 'fraction_le']],
        '>': [['int', 'int', 'bool', 'int_gt'], ['float', 'float', 'bool', 'float_gt'], ['fraction', 'fraction', 'bool', 'fraction_gt']],
        '>=': [['int', 'int', 'bool', 'int_ge'], ['float', 'float', 'bool', 'float_ge'], ['fraction', 'fraction', 'bool', 'fraction_ge']],
        'and': [['bool', 'bool', 'bool', 'bool_and']],
        'or': [['bool', 'bool', 'bool', 'bool_or']],
    };
})(Op || (Op = {}));
var Parser;
(function (Parser_1) {
    /**
     * Maximum size for a grid alphabet; this is set at 128 so that alphabet
     * symbols can be represented by signed byte values.
     */
    const MAX_ALPHABET_SIZE = 128;
    /**
     * Precedence table for binary operators.
     */
    Parser_1.BINARY_OPS = {
        'or': 2 /* Precedence.OR */,
        'and': 3 /* Precedence.AND */,
        '<': 5 /* Precedence.CMP */,
        '<=': 5 /* Precedence.CMP */,
        '>': 5 /* Precedence.CMP */,
        '>=': 5 /* Precedence.CMP */,
        '==': 5 /* Precedence.CMP */,
        '!=': 5 /* Precedence.CMP */,
        '+': 6 /* Precedence.PLUS_MINUS */,
        '-': 6 /* Precedence.PLUS_MINUS */,
        '*': 7 /* Precedence.MULT_DIV_MOD */,
        '/': 7 /* Precedence.MULT_DIV_MOD */,
        '//': 7 /* Precedence.MULT_DIV_MOD */,
        '%': 7 /* Precedence.MULT_DIV_MOD */,
    };
    /**
     * Precedence table for unary operators.
     */
    Parser_1.UNARY_OPS = {
        'not': 4 /* Precedence.NOT */,
        '+': 8 /* Precedence.UPLUS_UMINUS */,
        '-': 8 /* Precedence.UPLUS_UMINUS */,
        'count': 9 /* Precedence.FUNC */,
        'load': 9 /* Precedence.FUNC */,
        'randint': 9 /* Precedence.FUNC */,
        'sum': 9 /* Precedence.FUNC */,
    };
    const UNESCAPE_STRING_CHAR = {
        '\\n': '\n',
        '\\t': '\t',
    };
    /**
     * Arguments for each node type; `true` means a required argument.
     */
    const ARGS = ((specs) => specs)({
        all: { temperature: false, search: false, maxStates: false, depthCoefficient: false },
        convchain: { sample: true, n: true, on: true, temperature: false, periodic: false },
        convolution: { kernel: true },
        field: { for_: true, on: true, from: false, to: false, essential: false, recompute: false },
        grid: { scaleX: false, scaleY: false, periodic: false },
        map: { outGrid: true },
        one: { temperature: false, search: false, maxStates: false, depthCoefficient: false },
        path: { from: true, to: true, input: true, output: true, longest: false, inertia: false },
    });
    /**
     * Lookup table for which nodes have each argument; used to generate hints
     * in error messages.
     */
    const ARGS_TO_NODES = {};
    for (const [k, args] of Object.entries(ARGS)) {
        for (const j of Object.keys(args)) {
            (ARGS_TO_NODES[j] ??= []).push(k);
        }
    }
    function quoteNode(tok) {
        return tok.kind === 'KEYWORD' ? `keyword '${tok.s}'`
            : tok.kind === 'OP' || tok.kind === 'PUNCTUATION' ? `'${tok.s}'`
                : `'${tok.kind}'`;
    }
    class Parser {
        diagnostics = new Diagnostics();
        q;
        constructor(src) {
            const tokens = Tokenizer.tokenize(src, true);
            this.q = new Tokenizer.TokenQueue(tokens);
        }
        // error reporting and checking
        errorUnexpected(part) {
            const tok = this.q.poll();
            this.diagnostics.syntaxError(`unexpected ${quoteNode(tok)}, expected ${part}`, tok.pos);
        }
        errorExpected(was, ...hints) {
            this.diagnostics.syntaxError(`expected ${quoteJoin(hints, ' | ')}, was ${quoteNode(was)}`, was.pos);
        }
        errorOperatorPrecedence(tok) {
            this.diagnostics.syntaxError(`${quoteNode(tok)} not allowed here due to operator precedence`, tok.pos);
        }
        expectPoll(kind) {
            const tok = this.q.poll();
            if (tok.kind === kind) {
                return tok;
            }
            else {
                this.errorExpected(tok, kind);
                return undefined;
            }
        }
        expectPollS(...strings) {
            const tok = this.q.poll();
            if (strings.includes(tok.s)) {
                return tok;
            }
            else {
                this.errorExpected(tok, ...strings);
                return undefined;
            }
        }
        expectPollIf(kind) {
            return this.q.pollIf(kind)
                || (this.errorExpected(this.q.peek(), kind), false);
        }
        expectPollIfS(s) {
            if (!this.q.pollIfS(s)) {
                this.errorExpected(this.q.peek(), s);
            }
        }
        assertPoll(kind) {
            return this.q.hasNext(kind) ? this.q.poll() : fail();
        }
        assertPollS(...strings) {
            return this.q.hasNextS(...strings) ? this.q.poll() : fail();
        }
        // entry points
        /**
         * ```none
         * CompilationUnit = Statement* EOF
         * ```
         */
        root = () => {
            const stmts = this.parseUntil('stmt');
            this.assertPoll('EOF');
            return { kind: 'root', stmts, pos: { line: 1, col: 0 } };
        };
        /**
         * ```none
         * Declaration = LegendDecl | LetDecl | SymmetryDecl | UnionDecl
         * ```
         */
        decl = () => {
            const tok = this.q.peek();
            if (tok.kind === 'KEYWORD') {
                switch (tok.s) {
                    case 'legend':
                        return this.parseLegendDecl();
                    case 'let':
                        return this.parseLetDecl();
                    case 'symmetry':
                        return this.parseSymmetryDecl();
                    case 'union':
                        return this.parseUnionDecl();
                }
            }
            this.errorUnexpected('declaration');
            this.q.skipLine();
            return undefined;
        };
        /**
         * ```none
         * Rule = FieldRule | ObserveRule | RewriteRule | WithDeclaration<Rule>
         * ```
         */
        rule = () => {
            const tok = this.q.peek();
            if (tok.kind === 'KEYWORD') {
                switch (tok.s) {
                    case 'legend':
                    case 'let':
                    case 'symmetry':
                    case 'union':
                        const declaration = this.decl();
                        return declaration && this.parseDeclChildren('rule', declaration);
                    case 'field':
                        return this.parseFieldRule();
                    case 'observe':
                        return this.parseObserveRule();
                }
            }
            const rule = this.parseRewriteRule();
            if (rule !== undefined) {
                return rule;
            }
            this.errorUnexpected('rule');
            this.q.skipLine();
            return undefined;
        };
        /**
         * ```none
         * Statement = BaseUseStmt | LogStmt | ModifiableStmt | PassStmt | UseStmt | WithDeclaration<Statement>
         * ```
         */
        stmt = () => {
            const tok = this.q.peek();
            if (tok.kind === 'KEYWORD') {
                switch (tok.s) {
                    case 'legend':
                    case 'let':
                    case 'symmetry':
                    case 'union':
                        const declaration = this.decl();
                        return declaration && this.parseDeclChildren('stmt', declaration);
                    case 'grid':
                        return this.parseBareUseStmt();
                    case 'pass':
                        return this.parsePassStmt();
                    case 'log':
                        return this.parseLogStmt();
                    case 'use':
                        return this.parseUseStmt();
                }
            }
            return this.parseModifiableStmt();
        };
        /**
         * ```none
         * ModifiableStmt = BlockStmt | ConvChainStmt | ModifiedStmt | PathStmt | PutStmt | RuleBlockStmt
         * ```
         */
        parseModifiableStmt() {
            const tok = this.q.peek();
            if (tok.kind === 'KEYWORD') {
                switch (tok.s) {
                    case 'all':
                    case 'convolution':
                    case 'map':
                    case 'once':
                    case 'one':
                    case 'prl':
                        return this.parseRuleBlockStmt();
                    case 'markov':
                    case 'sequence':
                        return this.parseBlockStmt();
                    case 'convchain':
                    case 'path':
                        return this.parseLineArgsStmt();
                    case 'put':
                        return this.parsePutStmt();
                }
            }
            else if (tok.s === '@') {
                return this.parseModifiedStmt();
            }
            this.errorUnexpected('statement');
            this.q.skipLine();
            return undefined;
        }
        /**
         * ```none
         * Expression = TernaryExpr
         * ```
         */
        expr = (minPrecedence = 1 /* Precedence.IF_ELSE */) => {
            const unaryExpr = this.parseUnaryOpExpr(minPrecedence);
            const binaryExpr = this.parseBinaryOpExpr(unaryExpr, minPrecedence);
            return this.parseTernaryExpr(binaryExpr, minPrecedence);
        };
        /**
         * ```none
         * TernaryExpr = BinaryOpExpr ('if' Expression 'else' Expression)?
         * ```
         */
        parseTernaryExpr(expr, minPrecedence) {
            if (expr === undefined || minPrecedence > 1 /* Precedence.IF_ELSE */ || !this.q.pollIfS('if')) {
                return expr;
            }
            let condition, otherwise;
            return (condition = this.expr(2 /* Precedence.OR */))
                && this.expectPollS('else')
                && (otherwise = this.expr(1 /* Precedence.IF_ELSE */))
                && { kind: 'expr.op.ternary', condition, then: expr, otherwise, pos: expr.pos };
        }
        hasNextBinaryOp(minPrecedence) {
            const op = this.q.peek().s;
            return objHasKey(Parser_1.BINARY_OPS, op) && Parser_1.BINARY_OPS[op] >= minPrecedence;
        }
        /**
         * ```none
         * BinaryOpExpr = UnaryOpExpr (BinaryOp UnaryOpExpr)*
         * BinaryOp = OP | 'and' | 'or'
         * ```
         */
        parseBinaryOpExpr(left, minPrecedence) {
            // https://en.wikipedia.org/wiki/Operator-precedence_parser#Pseudocode
            let prevWasComparison = false;
            while (left !== undefined && this.hasNextBinaryOp(minPrecedence)) {
                const opTok = this.q.poll();
                const op = opTok.s;
                const opPrecedence = Parser_1.BINARY_OPS[op];
                const isComparison = opPrecedence === 5 /* Precedence.CMP */;
                // MJr syntax is similar to Python's, but `x < y < z` chains in Python and not in
                // MJr; such expressions are mistakes, so report a useful error message now rather
                // than a type error later. If the programmer wrote `x < y == z` intending to
                // compare the result of `x < y` with a boolean `z`, they should use brackets.
                if (prevWasComparison && isComparison) {
                    this.diagnostics.syntaxError(`comparison operators do not chain; use 'and' to do multiple comparisons, or brackets to disambiguate`, opTok.pos);
                }
                let right = this.parseUnaryOpExpr(opPrecedence);
                // none of the binary ops are right-associative
                while (right !== undefined && this.hasNextBinaryOp(opPrecedence + 1)) {
                    right = this.parseBinaryOpExpr(right, opPrecedence + 1);
                }
                left = right && { kind: 'expr.op.binary', op, left, right, pos: left.pos };
                prevWasComparison = isComparison;
            }
            return left;
        }
        /**
         * ```none
         * UnaryOpExpr = DeclarationExpr | UnaryOp Expression | PrimaryExpr
         * UnaryOp = '+' | '-' | 'count' | 'load' | 'not' | 'randint' | 'sum'
         * ```
         */
        parseUnaryOpExpr(minPrecedence) {
            if (this.q.hasNextS('legend', 'let', 'symmetry', 'union')) {
                if (minPrecedence > 0 /* Precedence.DECLARATION */) {
                    this.errorOperatorPrecedence(this.q.peek());
                }
                return this.parseDeclarationExpr();
            }
            const { s: op, pos } = this.q.peek();
            if (!objHasKey(Parser_1.UNARY_OPS, op)) {
                return this.parsePrimaryExpression();
            }
            const tok = this.q.poll();
            const opPrecedence = Parser_1.UNARY_OPS[op];
            if (opPrecedence < minPrecedence) {
                this.errorOperatorPrecedence(tok);
            }
            const child = this.expr(opPrecedence);
            return child && { kind: 'expr.op.unary', op, child, pos };
        }
        /**
         * ```none
         * PrimaryExpr = DictExpr | GridExpr | LiteralExpr | NameExpr | '(' Expression ')'
         * LiteralExpr = BoolLiteralExpr | FloatLiteralExpr | IntLiteralExpr | PatternLiteralExpr | StrLiteralExpr
         * ```
         */
        parsePrimaryExpression() {
            const tok = this.q.peek();
            switch (tok.kind) {
                case 'KEYWORD':
                    switch (tok.s) {
                        case 'at':
                        case 'origin':
                        case 'random':
                            return this.parseNameExpr();
                        case 'false':
                        case 'true':
                            return this.parseBoolLiteralExpr();
                        case 'grid':
                            return this.parseGridExpr();
                    }
                    break;
                case 'FLOAT':
                    return this.parseFloatLiteralExpr();
                case 'INT':
                    return this.parseIntLiteralExpr();
                case 'NAME':
                    return this.parseNameExpr();
                case 'QUOTE':
                    return this.parseStrLiteralExpr();
                case 'PUNCTUATION':
                    switch (tok.s) {
                        case '(':
                            return this.parseBracketedExpr();
                        case '[':
                            return this.parsePatternLiteralExpr();
                        case '{':
                            return this.parseDictExpr();
                    }
                    break;
            }
            this.errorUnexpected('expression');
            return undefined;
        }
        // helpers
        /**
         * Parses `T*`. `stopBefore` must only contain tokens which `T` cannot begin with.
         */
        parseUntil(part, ...stopBefore) {
            const parseChild = this[part];
            const children = [];
            while (!this.q.hasNext('EOF', ...stopBefore)) {
                const child = parseChild();
                if (child !== undefined) {
                    children.push(child);
                }
            }
            return children;
        }
        /**
         * ```none
         * BlockChildren<T> = ':' (T | NEWLINE INDENT T+ DEDENT)
         * ```
         */
        parseBlockChildren(kind) {
            this.expectPollIfS(':');
            if (this.q.pollIf('NEWLINE')) {
                if (!this.expectPollIf('INDENT')) {
                    return [];
                }
                const children = this.parseUntil(kind, 'DEDENT');
                this.assertPoll('DEDENT');
                return children;
            }
            else {
                const child = this[kind]();
                return child !== undefined ? [child] : [];
            }
        }
        /**
         * ```none
         * WithDeclaration<T> = Declaration (NEWLINE T* | 'in' BlockChildren<T>)
         * ```
         */
        parseDeclChildren(kind, declaration) {
            const children = this.q.pollIf('NEWLINE')
                ? this.parseUntil(kind, 'DEDENT')
                : (this.expectPollIfS('in'), this.parseBlockChildren(kind));
            return { kind: `${kind}.decl`, declaration, children, pos: declaration.pos };
        }
        /**
         * ```none
         * Args = NameValuePairs?
         * ```
         */
        parseArgs(kind) {
            const pairs = this.q.hasNextS('{') ? this.parseNameValuePairs() : [];
            if (pairs === undefined) {
                return undefined;
            }
            const spec = ARGS[kind];
            const args = {};
            for (const [name, expr] of pairs) {
                // sanitise JS keyword
                const argName = name.name === 'for' ? 'for_' : name.name;
                args[argName] = expr;
                if (!objHasKey(spec, argName)) {
                    const hints = ARGS_TO_NODES[argName];
                    const msg = hints !== undefined ? `argument '${name.name}' only valid for ${quoteJoin(hints)}` : `invalid argument '${name.name}'`;
                    this.diagnostics.syntaxError(msg, name.pos);
                }
            }
            const missing = Object.keys(spec).filter(k => spec[k] && args[k] === undefined);
            if (missing.length > 0) {
                this.diagnostics.syntaxError(`missing required argument${missing.length > 1 ? 's' : ''} ${quoteJoin(missing)}`, this.q.peek().pos);
                return undefined;
            }
            return args;
        }
        /**
         * ```none
         * NameValuePairs = '{' (NameValuePair ',')* NameValuePair? '}'
         * NameValuePair = SimpleNameExpr '=' Expression
         * ```
         */
        parseNameValuePairs() {
            const pairs = [];
            const names = new Set();
            if (this.q.pollIfS('{')) {
                while (true) {
                    const name = this.parseSimpleNameExpr();
                    if (name === undefined) {
                        return undefined;
                    }
                    if (names.has(name.name)) {
                        this.diagnostics.syntaxError(`duplicate name '${name.name}'`, name.pos);
                    }
                    const arg = this.expectPollS('=') && this.expr();
                    if (arg === undefined) {
                        return undefined;
                    }
                    ;
                    pairs.push([name, arg]);
                    names.add(name.name);
                    if (this.q.pollIfS(',')) {
                        if (this.q.pollIfS('}')) {
                            break;
                        }
                    }
                    else if (this.expectPollS('}')) {
                        break;
                    }
                    else {
                        return undefined;
                    }
                }
            }
            return pairs;
        }
        // declarations
        /**
         * ```none
         * LegendDecl = 'legend' PatternLiteralExpr
         * SymmetryDecl = 'symmetry' StrLiteralExpr
         * ```
         */
        parseLegendDecl() {
            const { pos } = this.assertPollS('legend', 'symmetry');
            const expr = this.parsePatternLiteralExpr();
            return expr && { kind: 'decl.legend', expr, pos };
        }
        /**
         * ```none
         * LetDecl = 'let' 'param'? SimpleNameExpr '=' Expression
         * ```
         */
        parseLetDecl() {
            const { pos } = this.assertPollS('let');
            const isParam = this.q.pollIfS('param');
            /*
            const names: AST.SimpleNameExpr[] = [];
            while(true) {
                const nameExpr = this.parseSimpleNameExpr();
                if(nameExpr === undefined) { return undefined; }
                names.push(nameExpr);
                
                if(this.q.pollIfS('=')) { break; }
                if(this.expectPollS('|', '=') === undefined) { return undefined; }
            }
           
            if(isParam && names.length !== 1) {
                this.diagnostics.syntaxError(`'let param' must only declare one name`, pos);
            }
            */
            let name, rhs;
            return (name = this.parseSimpleNameExpr())
                && this.expectPollS('=')
                && (rhs = this.expr())
                && { kind: 'decl.let', name, rhs, pos, isParam };
        }
        /**
         * ```none
         * LegendDecl = 'legend' PatternLiteralExpr
         * SymmetryDecl = 'symmetry' StrLiteralExpr
         * ```
         */
        parseSymmetryDecl() {
            const { pos } = this.assertPollS('symmetry');
            const expr = this.parseStrLiteralExpr();
            return expr && { kind: 'decl.symmetry', expr, pos };
        }
        /**
         * ```none
         * UnionDecl = 'union' PatternLiteralExpr '=' Expression
         * ```
         */
        parseUnionDecl() {
            const { pos } = this.assertPollS('union');
            let label, chars;
            return (label = this.parsePatternLiteralExpr())
                && this.expectPollS('=')
                && (chars = this.expr())
                && { kind: 'decl.union', label, chars, pos };
        }
        // rules
        /**
         * ```none
         * FieldRule = 'field' Args NEWLINE
         * ```
         */
        parseFieldRule() {
            const { pos } = this.assertPollS('field');
            const args = this.parseArgs('field');
            return args
                && this.expectPoll('NEWLINE')
                && ({ kind: 'rule.field', ...args, pos });
        }
        /**
         * ObserveRule = 'observe' Expression ('->' Expression)? '->' Expression ('if' Expression)? NEWLINE
         */
        parseObserveRule() {
            const { pos } = this.assertPollS('observe');
            const from = this.expr(2 /* Precedence.OR */);
            if (!from) {
                return undefined;
            }
            this.expectPollS('->');
            let via = this.expr(2 /* Precedence.OR */);
            if (via === undefined) {
                return undefined;
            }
            let to;
            if (this.q.pollIfS('->')) {
                to = this.expr(2 /* Precedence.OR */);
                if (to === undefined) {
                    return undefined;
                }
            }
            else {
                to = via;
                via = undefined;
            }
            let condition;
            return (this.q.pollIfS('if') ? condition = this.expr(2 /* Precedence.OR */) : true)
                && this.expectPoll('NEWLINE')
                && { kind: 'rule.observe', from, via, to, condition, pos };
        }
        /**
         * ```none
         * RewriteRule = Expression '->' Expression ('if' Expression)? NEWLINE
         * ```
         */
        parseRewriteRule() {
            let from, to, condition = undefined;
            return (from = this.expr(2 /* Precedence.OR */))
                && this.expectPollS('->')
                && (to = this.expr(2 /* Precedence.OR */))
                && (this.q.pollIfS('if') ? condition = this.expr(2 /* Precedence.OR */) : true)
                && this.expectPoll('NEWLINE')
                && { kind: 'rule.rewrite', from, to, condition, pos: from.pos };
        }
        // statements
        /**
         * ```none
         * ModifiedStmt = '@' 'limit' Expression NEWLINE ModifiableStmt
         * ```
         */
        parseModifiedStmt() {
            const { pos } = this.assertPollS('@');
            let name, arg, child;
            return (name = this.expectPollS('limit'))
                && (arg = this.expr())
                && this.expectPoll('NEWLINE')
                && (child = this.parseModifiableStmt())
                && { kind: `stmt.modified.${name.s}`, arg, child, pos };
        }
        /**
         * ```none
         * BlockStmt = MarkovStmt | SequenceStmt
         * MarkovStmt = 'markov' BlockChildren<Statement>
         * SequenceStmt = 'sequence' BlockChildren<Statement>
         * ```
         */
        parseBlockStmt() {
            const { s: kind, pos } = this.assertPollS('markov', 'sequence');
            const children = this.parseBlockChildren('stmt');
            return { kind: `stmt.block.${kind}`, children, pos };
        }
        /**
         * ```none
         * RuleBlockStmt = AllStmt | ConvolutionStmt | MapStmt | OnceStmt | OneStmt | PrlStmt
         * AllStmt = 'all' Args BlockChildren<Rule>
         * ConvolutionStmt = 'convolution' Args BlockChildren<Rule>
         * MapStmt = 'map' Args BlockChildren<Rule>
         * OnceStmt = 'once' BlockChildren<Rule>
         * OneStmt = 'one' Args BlockChildren<Rule>
         * PrlStmt = 'prl' BlockChildren<Rule>
         * ```
         */
        parseRuleBlockStmt() {
            const { s: kind, pos } = this.assertPollS('all', 'convolution', 'map', 'once', 'one', 'prl');
            const args = kind === 'prl' || kind === 'once' ? {} : this.parseArgs(kind);
            const rules = this.parseBlockChildren('rule');
            return args && { kind: `stmt.rules.${kind}`, ...args, rules, pos };
        }
        /**
         * ```none
         * PassStmt = 'pass' NEWLINE
         * ```
         */
        parsePassStmt() {
            const { pos } = this.assertPollS('pass');
            return { kind: 'stmt.pass', pos };
        }
        /**
         * ```none
         * PutStmt = 'put' Expression 'at' Expression ('if' Expression)? NEWLINE
         * ```
         */
        parsePutStmt() {
            const { pos } = this.assertPollS('put');
            let pattern, at, condition;
            return (pattern = this.expr(2 /* Precedence.OR */))
                && this.expectPollS('at')
                && (at = this.expr(2 /* Precedence.OR */))
                && (this.q.pollIfS('if') ? condition = this.expr(2 /* Precedence.OR */) : true)
                && this.expectPoll('NEWLINE')
                && { kind: 'stmt.put', pattern, at, condition, pos };
        }
        /**
         * ```none
         * ConvChainStmt = 'convchain' Args NEWLINE
         * PathStmt = 'path' Args NEWLINE
         * ```
         */
        parseLineArgsStmt() {
            const { s: kind, pos } = this.assertPollS('convchain', 'path');
            const args = this.parseArgs(kind);
            return args
                && this.expectPoll('NEWLINE')
                && { kind: `stmt.${kind}`, ...args, pos };
        }
        /**
         * ```none
         * BareUseStmt = GridExpr NEWLINE
         * ```
         */
        parseBareUseStmt() {
            const expr = this.parseGridExpr();
            return expr
                && this.expectPoll('NEWLINE')
                && { kind: 'stmt.use.expr', expr, pos: expr.pos };
        }
        /**
         * LogStmt = 'log' Expression NEWLINE
         */
        parseLogStmt() {
            const { pos, s: kind } = this.assertPollS('log');
            let expr;
            return (expr = this.expr())
                && this.expectPoll('NEWLINE')
                && { kind: `stmt.${kind}`, expr, pos };
        }
        /**
         * UseStmt = UseExprStmt | UseLetStmt
         * UseExprStmt = 'use' Expression NEWLINE
         * UseLetStmt = 'use' LetDecl NEWLINE Statement*
         */
        parseUseStmt() {
            const { pos } = this.assertPollS('use');
            if (this.q.hasNextS('let')) {
                let decl;
                return (decl = this.parseLetDecl())
                    && this.expectPoll('NEWLINE')
                    && { kind: 'stmt.use.let', decl, children: this.parseUntil('stmt', 'DEDENT'), pos };
            }
            else {
                let expr;
                return (expr = this.expr())
                    && this.expectPoll('NEWLINE')
                    && { kind: 'stmt.use.expr', expr, pos };
            }
        }
        // expressions
        parseBracketedExpr() {
            this.assertPollS('(');
            const expr = this.expr(0 /* Precedence.DECLARATION */);
            this.expectPollS(')');
            return expr;
        }
        /**
         * ```none
         * DeclarationExpr = Declaration 'in' Expression
         * ```
         */
        parseDeclarationExpr() {
            let declaration, child;
            return (declaration = this.decl())
                && (this.expectPollIfS('in'), child = this.expr(0 /* Precedence.DECLARATION */))
                && { kind: 'expr.decl', declaration, child, pos: declaration.pos };
        }
        /**
         * ```none
         * DictExpr = NameValuePairs
         * ```
         */
        parseDictExpr() {
            const { pos } = this.q.peek();
            const pairs = this.parseNameValuePairs();
            return pairs && { kind: 'expr.dict', pairs, pos };
        }
        /**
         * ```none
         * BoolLiteralExpr = 'false' | 'true'
         * ```
         */
        parseBoolLiteralExpr() {
            const tok = this.expectPollS('false', 'true');
            return tok && { kind: 'expr.literal.bool', value: tok.s === 'true', pos: tok.pos };
        }
        /**
         * ```none
         * FloatLiteralExpr = FLOAT
         * ```
         */
        parseFloatLiteralExpr() {
            const tok = this.expectPoll('FLOAT');
            return tok && { kind: 'expr.literal.float', value: parseFloat(tok.s), pos: tok.pos };
        }
        /**
         * ```none
         * IntLiteralExpr = INT
         * ```
         */
        parseIntLiteralExpr() {
            const tok = this.expectPoll('INT');
            if (tok === undefined) {
                return undefined;
            }
            const value = parseInt(tok.s) | 0;
            if (`${value}` !== tok.s) {
                this.diagnostics.syntaxError(`int literal '${tok.s}' out of range`, tok.pos);
            }
            return { kind: 'expr.literal.int', value, pos: tok.pos };
        }
        /**
         * ```none
         * PatternLiteralExpr = '[' (Char | CharSet)+ ('/' (Char | CharSet)+)* ']'
         * Char = PATTERN_CHAR
         * ```
         */
        parsePatternLiteralExpr() {
            const beginTok = this.expectPollS('[');
            if (beginTok === undefined) {
                return undefined;
            }
            const { pos } = beginTok;
            let row = [];
            const rows = [row];
            while (!this.q.pollIfS(']')) {
                const tok = this.q.poll();
                switch (tok.s) {
                    case '[':
                        row.push(this.parseCharSet(tok));
                        break;
                    case '/':
                        rows.push(row = []);
                        break;
                    default:
                        row.push(tok);
                        break;
                }
            }
            const width = row.length, height = rows.length;
            if (rows.some(row => row.length !== width)) {
                this.diagnostics.syntaxError('pattern must be rectangular', pos);
            }
            else if (width === 0) {
                this.diagnostics.syntaxError('empty pattern', pos);
            }
            return { kind: 'expr.literal.pattern', width, height, value: rows.flat(), pos };
        }
        /**
         * ```none
         * CharSet = '[' Char+ ']'
         * ```
         */
        parseCharSet(beginTok) {
            const { pos } = beginTok;
            const inverted = this.q.pollIfS('^');
            const chars = [];
            while (!this.q.pollIfS(']')) {
                const tok = this.expectPoll('PATTERN_CHAR');
                if (tok !== undefined) {
                    chars.push(tok);
                }
            }
            if (chars.length === 0 && !inverted) {
                this.diagnostics.syntaxError('empty charset', pos);
            }
            return { kind: 'CHARSET', chars, inverted, pos };
        }
        /**
         * ```none
         * StrLiteralExpr = STRING
         * ```
         */
        parseStrLiteralExpr() {
            const { pos } = this.assertPoll('QUOTE');
            const s = [];
            while (!this.q.pollIf('QUOTE')) {
                const tok = this.q.poll();
                if (tok.kind === 'ESCAPED_CHAR') {
                    s.push(UNESCAPE_STRING_CHAR[tok.s] ?? tok.s.substring(1));
                }
                else {
                    s.push(tok.s);
                }
            }
            return { kind: 'expr.literal.str', value: s.join(''), pos };
        }
        /**
         * ```none
         * NameExpr = AttributeExpr | KeywordNameExpr | SimpleNameExpr
         * AttributeExpr = NameExpr '.' NAME
         * KeywordNameExpr = 'at' | 'origin' | 'random'
         * ```
         */
        parseNameExpr() {
            let expr;
            if (this.q.hasNext('NAME')) {
                expr = this.parseSimpleNameExpr();
            }
            else {
                const { s: name, pos } = this.assertPollS('at', 'origin', 'random');
                expr = { kind: 'expr.name.keyword', name, pos };
            }
            while (expr !== undefined && this.q.pollIfS('.')) {
                const attr = this.expectPoll('NAME');
                expr = attr && { kind: 'expr.attr', left: expr, attr: attr.s, pos: expr.pos };
            }
            return expr;
        }
        /**
         * ```none
         * SimpleNameExpr = NAME
         * ```
         */
        parseSimpleNameExpr() {
            const tok = this.expectPoll('NAME');
            return tok && { kind: 'expr.name.simple', name: tok.s, pos: tok.pos };
        }
        /**
         * ```none
         * GridExpr = 'grid' Args PatternLiteralExpr
         * ```
         */
        parseGridExpr() {
            const { pos } = this.assertPollS('grid');
            const args = this.parseArgs('grid');
            const alphabet = this.parsePatternLiteralExpr();
            if (alphabet === undefined) {
                return undefined;
            }
            if (alphabet.height > 1) {
                this.diagnostics.syntaxError(`alphabet must be a single row`, alphabet.pos);
            }
            else if (alphabet.width < 2) {
                this.diagnostics.syntaxError(`alphabet size must be at least 2`, alphabet.pos);
            }
            else if (alphabet.width > MAX_ALPHABET_SIZE) {
                this.diagnostics.syntaxError(`alphabet size cannot exceed ${MAX_ALPHABET_SIZE}`, alphabet.pos);
            }
            const alphabetKey = [];
            for (const c of alphabet.value) {
                if (c.kind === 'CHARSET') {
                    this.diagnostics.syntaxError(`alphabet cannot have unions`, c.pos);
                }
                else if (c.s === '.') {
                    this.diagnostics.syntaxError(`'.' cannot be an alphabet symbol`, c.pos);
                }
                else if (alphabetKey.includes(c.s)) {
                    this.diagnostics.syntaxError(`repeated alphabet symbol '${c.s}'`, c.pos);
                }
                else {
                    alphabetKey.push(c.s);
                }
            }
            return { kind: 'expr.grid', alphabetKey: alphabetKey.join(''), ...args, pos };
        }
    }
    function parse(src, part = 'root') {
        const parser = new Parser(src);
        const ast = parser[part]();
        parser.diagnostics.throwIfAnyErrors();
        return ast;
    }
    Parser_1.parse = parse;
})(Parser || (Parser = {}));
///<reference path="../runtime/mjr.ts"/>
var Resolver;
(function (Resolver) {
    const FRACTION_ONE = MJr.fraction(1, 1);
    /**
     * Specifies types and other static checks for properties of AST nodes.
     * A spec of '?' indicates that the property should not be present, but
     * enables some nodes to have the same property names and therefore be
     * resolved by the same functions.
     */
    const PROP_SPECS = ((specs) => specs)({
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
            on: 'charset.in',
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
    });
    function _convPatternKey(p) {
        return `${Convolution.Kernel.key(p.kernel)}:${ISet.key(p.chars)}`;
    }
    class Alphabet {
        key;
        map;
        charsets = new Map();
        wildcard;
        legend = undefined;
        constructor(key) {
            this.key = key;
            this.map = IDMap.of(key);
            const n = key.length;
            const { charsets } = this;
            for (let i = 0; i < n; ++i) {
                charsets.set(key[i], ISet.of(n, [i]));
            }
            charsets.set('.', this.wildcard = ISet.full(n));
        }
        withUnion(label, set, f) {
            const { charsets } = this;
            const oldSet = charsets.get(label);
            charsets.set(label, set);
            const result = f();
            if (oldSet !== undefined) {
                charsets.set(label, oldSet);
            }
            else {
                charsets.delete(label);
            }
            return result;
        }
    }
    class Context {
        diagnostics = new Diagnostics();
        globals = {
            grids: [],
            limits: [],
            params: new Map(),
            potentials: [],
            variables: [],
        };
        reset = undefined;
        symmetryName = 'all';
        variables = new Map();
        errorVariables = new Set();
        kernel = undefined;
        grid = undefined;
        inputPattern = undefined;
        isRuleContext = false;
        rewriteScaleX = FRACTION_ONE;
        rewriteScaleY = FRACTION_ONE;
        resolveRoot(root) {
            const children = this.resolveStmts(root.stmts, false);
            return { kind: 'stmt.block.sequence', children, reset: undefined, pos: root.pos };
        }
        resolveDecl(node, callback) {
            const f = DECL_RESOLVE_FUNCS[node.kind];
            return f(node, this, callback);
        }
        resolveExpr(node) {
            const f = EXPR_RESOLVE_FUNCS[node.kind];
            return f(node, this);
        }
        resolveRule(node, outGrid) {
            if (!this.expectGrid(node.pos)) {
                return undefined;
            }
            const f = RULE_RESOLVE_FUNCS[node.kind];
            return f(node, this, outGrid);
        }
        resolveStmt(node, canReset = true) {
            const f = STMT_RESOLVE_FUNCS[node.kind];
            return f(node, this, canReset);
        }
        resolveChar(c) {
            const grid = this.grid ?? fail();
            const set = grid.alphabet.charsets.get(c.s);
            if (set === undefined) {
                this.error(`'${c.s}' is not an alphabet symbol or union label`, c.pos);
            }
            return set;
        }
        resolveCharSet(charset) {
            const grid = this.grid ?? fail();
            const k = grid.alphabet.key.length;
            const mask = charset.inverted ? ISet.full(k) : ISet.empty(k);
            for (const c of charset.chars) {
                const cMask = this.resolveChar(c);
                if (cMask === undefined) {
                    return undefined;
                }
                else if (charset.inverted) {
                    ISet.removeAll(mask, cMask);
                }
                else {
                    ISet.addAll(mask, cMask);
                }
            }
            return mask;
        }
        resolveStmts(children, canReset) {
            return children.flatMap(c => {
                const r = this.resolveStmt(c, canReset);
                return r === undefined ? []
                    : r.kind === 'stmts' ? r.stmts
                        : r.assigns !== undefined ? [...r.assigns, r.stmt]
                            : [r.stmt];
            });
        }
        expectGrid(pos) {
            if (this.grid !== undefined) {
                return true;
            }
            else {
                this.error(this.globals.grids.length === 0 ? `no grid declared` : `no grid in use`, pos);
                return false;
            }
        }
        checkType(expected, expr) {
            if (Type.isSubtype(expr.type, expected)) {
                return true;
            }
            else {
                this.typeError((expected.kind === 'union' ? expected.options : [expected]).map(Type.toStr), expr);
                return false;
            }
        }
        error(msg, pos) {
            this.diagnostics.compilationError(msg, pos);
        }
        typeError(expected, expr) {
            this.error(`expected ${quoteJoin(expected, ' | ')}, was '${Type.toStr(expr.type)}'`, expr.pos);
        }
        makeLimit(initialiser) {
            const { reset } = this;
            const canReset = reset !== undefined;
            const isTransparent = (!canReset || reset.kind === 'stmt.block.sequence')
                && initialiser.kind === 'expr.constant'
                && initialiser.constant.value === 1;
            let limit = { id: -1, initialiser, canReset, isTransparent };
            if (!isTransparent) {
                limit = withNextID(this.globals.limits, limit);
                this.reset?.limitIDs.push(limit.id);
            }
            return limit;
        }
        makeVariable(name, type, flags, initialiser, isParam, pos) {
            if (this.variables.has(name) || this.errorVariables.has(name)) {
                this.error(`cannot redeclare name '${name}'`, pos);
            }
            else if (isParam && this.globals.params.has(name)) {
                this.error(`cannot redeclare parameter '${name}'`, pos);
            }
            if (isParam) {
                this.globals.params.set(name, type);
            }
            flags |= 4 /* ExprFlags.LOCALLY_DETERMINISTIC */ | 8 /* ExprFlags.POSITION_INDEPENDENT */ | 16 /* ExprFlags.GRID_INDEPENDENT */;
            return withNextID(this.globals.variables, { name, type, initialiser, flags, references: 0 });
        }
        withOutGrid(outGrid, inputPatternPos, f) {
            const { grid: inGrid, inputPattern } = this;
            if (inGrid === undefined || inputPattern === undefined) {
                fail();
            }
            if (outGrid.id === inGrid.id) {
                return f();
            }
            // if scale is not positive, `makeGrid` already reported the error
            if (inGrid.scaleX <= 0 || inGrid.scaleY <= 0 || outGrid.scaleX <= 0 || outGrid.scaleY <= 0) {
                return undefined;
            }
            const scaleX = MJr.fraction(outGrid.scaleX, inGrid.scaleX);
            const scaleY = MJr.fraction(outGrid.scaleY, inGrid.scaleY);
            if ((inputPattern.width * scaleX.p) % scaleX.q !== 0 || (inputPattern.height * scaleY.p) % scaleY.q !== 0) {
                this.error(`input pattern size must be a multiple of ${scaleX.q}x${scaleY.q}, was ${inputPattern.width}x${inputPattern.height}`, inputPatternPos);
                return undefined;
            }
            this.grid = outGrid;
            this.rewriteScaleX = scaleX;
            this.rewriteScaleY = scaleY;
            const result = f();
            this.grid = inGrid;
            this.rewriteScaleX = this.rewriteScaleY = FRACTION_ONE;
            return result;
        }
        withLegend(decl, f) {
            if (!this.expectGrid(decl.pos)) {
                return undefined;
            }
            const { alphabet } = this.grid;
            const oldLegend = alphabet.legend;
            if (oldLegend !== undefined) {
                this.error(`'legend' already declared`, decl.pos);
            }
            const legend = _resolvePatternLiteralExpr(decl.expr, this);
            if (legend === undefined) {
                return f();
            }
            if (legend.height !== 1) {
                this.error(`'legend' pattern cannot have multiple rows`, decl.expr.pos);
            }
            alphabet.legend = legend;
            const result = f();
            alphabet.legend = oldLegend;
            return result;
        }
        withKernel(stmt, f) {
            if (this.kernel !== undefined) {
                fail();
            }
            const kernel = _resolveProp(stmt, 'kernel', 'const str', this);
            if (objHasKey(Convolution.KERNELS, kernel)) {
                const k = this.kernel = Convolution.KERNELS[kernel];
                const result = f(k);
                this.kernel = undefined;
                return result;
            }
            else {
                // this also handles PROP_ERROR
                this.error(`convolution kernel must be one of ${quoteJoin(Object.keys(Convolution.KERNELS))}`, stmt.kernel.pos);
                return undefined;
            }
        }
        withSymmetry(decl, f) {
            const symmetryName = _resolveProp(decl, 'expr', 'const str', this);
            if (objHasKey(Symmetry.SYMMETRY_GROUPS, symmetryName)) {
                const oldSymmetryName = this.symmetryName;
                this.symmetryName = symmetryName;
                const result = f();
                this.symmetryName = oldSymmetryName;
                return result;
            }
            else {
                // this also handles PROP_ERROR
                this.error(`symmetry group must be one of ${quoteJoin(Object.keys(Symmetry.SYMMETRY_GROUPS))}`, decl.expr.pos);
                return f();
            }
        }
        withUnion(decl, f) {
            if (!this.expectGrid(decl.pos)) {
                return undefined;
            }
            const { alphabet } = this.grid;
            if (decl.label.width !== 1 || decl.label.height !== 1) {
                this.error(`union label must be a single character`, decl.label.pos);
                return f();
            }
            const label = decl.label.value[0];
            if (label.kind === 'CHARSET') {
                this.error(`union label cannot be a charset`, label.pos);
                return f();
            }
            if (alphabet.map.has(label.s)) {
                this.error(`alphabet symbol '${label.s}' cannot be union label`, decl.label.pos);
                return f();
            }
            if (alphabet.charsets.has(label.s)) {
                this.error(`union label '${label.s}' already declared`, decl.label.pos);
            }
            const charset = _resolveProp(decl, 'chars', 'const charset.in', this);
            if (charset === PROP_ERROR) {
                return undefined;
            }
            let mask = undefined;
            switch (charset.kind) {
                case 'leaf':
                    mask = charset.masks[0];
                    break;
                case 'top':
                    mask = alphabet.wildcard;
                    break;
                case 'bottom':
                    this.error(`empty charset`, decl.chars.pos);
                    break;
                default:
                    this.error(`union must be a constant charset`, decl.chars.pos);
                    break;
            }
            return mask && alphabet.withUnion(label.s, mask, f);
        }
        withVariable(variable, f) {
            const { variables } = this;
            const { name } = variable;
            const oldVariable = variables.get(name);
            variables.set(name, variable);
            const result = f();
            if (oldVariable !== undefined) {
                variables.set(name, oldVariable);
            }
            else {
                variables.delete(name);
            }
            return result;
        }
        withErrorVariable(name, f) {
            const { errorVariables } = this;
            const alreadyHad = errorVariables.has(name);
            errorVariables.add(name);
            const result = f();
            if (!alreadyHad) {
                errorVariables.delete(name);
            }
            return result;
        }
    }
    function _makeConstantValue(type, value) {
        return value !== undefined ? { kind: type.kind, type, value } : undefined;
    }
    function _makeConstantExpr(type, value, pos) {
        return { kind: 'expr.constant', type, constant: _makeConstantValue(type, value), flags: 31 /* ExprFlags.CONSTANT */, pos };
    }
    function _coerceFromInt(expr, type) {
        const op = expr.type.kind === 'int'
            ? `int_to_${type.kind}`
            : fail();
        const f = Op.UNARY_FUNCS[op] ?? fail();
        if (expr.kind === 'expr.constant') {
            const value = f(expr.constant.value);
            if (value !== undefined) {
                return _makeConstantExpr(type, value, expr.pos);
            }
        }
        return { kind: 'expr.op.unary', type, op, child: expr, flags: expr.flags, pos: expr.pos };
    }
    function _coerceToStr(expr) {
        switch (expr.type.kind) {
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int': {
                const op = `${expr.type.kind}_to_str`;
                const f = Op.UNARY_FUNCS[op];
                if (expr.kind === 'expr.constant' && f !== undefined) {
                    const value = f(expr.constant.value);
                    if (value !== undefined) {
                        return _makeConstantExpr(Type.STR, value, expr.pos);
                    }
                }
                return { kind: 'expr.op.unary', type: Type.STR, flags: expr.flags, op, child: expr, pos: expr.pos };
            }
            default:
                return expr;
        }
    }
    function _parsePropTypeSpec(typeSpec, ctx) {
        switch (typeSpec) {
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
        switch (typeSpec) {
            case 'charset.in':
                return { kind: 'pattern.in', width: 1, height: 1, alphabetKey };
            case 'charset.out':
                return { kind: 'pattern.out', width: 1, height: 1, alphabetKey };
            case 'position':
                return { kind: 'position', inGrid: grid.id };
            case 'pattern.in':
                const { inputPattern } = ctx;
                if (inputPattern === undefined) {
                    return { kind: 'any_pattern', alphabetKey, allowUnions: true };
                }
                else {
                    const { width, height } = inputPattern;
                    return { kind: 'pattern.in', alphabetKey, width, height };
                }
            case 'pattern.out': {
                const { inputPattern, rewriteScaleX, rewriteScaleY } = ctx;
                if (inputPattern === undefined) {
                    return { kind: 'any_pattern', alphabetKey, allowUnions: false };
                }
                else {
                    const w = MJr.fraction(inputPattern.width * rewriteScaleX.p, rewriteScaleX.q);
                    const h = MJr.fraction(inputPattern.height * rewriteScaleY.p, rewriteScaleY.q);
                    // ensure integer results
                    if (w.q !== 1 || h.q !== 1) {
                        fail();
                    }
                    return { kind: 'pattern.out', alphabetKey, width: w.p, height: h.p };
                }
            }
        }
    }
    function _parsePropSpec(spec, ctx) {
        const isConst = spec.startsWith('const ');
        const typeSpec = spec.replace(/^const /, '').replace(/\?$/, '');
        const expectedType = _parsePropTypeSpec(typeSpec, ctx);
        const coerceToStr = typeSpec === 'str~';
        const isRequired = !spec.endsWith('?');
        return { isConst, expectedType, coerceToStr, isRequired };
    }
    /**
     * Sentinel value indicating that an error occurred when resolving a prop.
     * The value `undefined` cannot be used for this purpose, as it is valid
     * for an optional const prop to be `undefined`.
     */
    const PROP_ERROR = Symbol();
    function _resolveProp(node, propName, spec, ctx) {
        const ast = node[propName];
        const { isConst, expectedType, coerceToStr, isRequired } = _parsePropSpec(spec, ctx);
        if (ast === undefined && isRequired) {
            fail();
        }
        let resolved = ast && ctx.resolveExpr(ast);
        if (resolved === undefined) {
            return isRequired ? PROP_ERROR : undefined;
        }
        if (resolved.type.kind === 'int' && (expectedType.kind === 'float' || expectedType.kind === 'fraction')) {
            resolved = _coerceFromInt(resolved, expectedType);
        }
        else if (coerceToStr) {
            resolved = _coerceToStr(resolved);
        }
        if (!ctx.checkType(expectedType, resolved)) {
            return PROP_ERROR;
        }
        else if (!isConst) {
            return resolved;
        }
        else if (resolved.kind === 'expr.constant') {
            return resolved.constant.value;
        }
        else {
            ctx.error(`expected compile-time constant`, resolved.pos);
            return PROP_ERROR;
        }
    }
    function _resolveProps(node, ctx) {
        const specs = PROP_SPECS[node.kind];
        const props = {};
        let allOK = true;
        for (const [propName, spec] of Object.entries(specs)) {
            const resolved = _resolveProp(node, propName, spec, ctx);
            if (resolved === PROP_ERROR) {
                allOK = false;
            }
            else {
                props[propName] = resolved;
            }
        }
        return allOK ? props : undefined;
    }
    // check AST instead of resolved, because we want to allow rules with constant false conditions
    function _hasAtLeastOneRewriteRule(rules) {
        return rules.some(rule => rule.kind === 'rule.rewrite'
            || (rule.kind === 'rule.decl' && _hasAtLeastOneRewriteRule(rule.children)));
    }
    function _resolveRules(node, ctx, outGrid, allowFieldObserve) {
        const out = {
            assigns: [],
            rewrites: [],
            fields: allowFieldObserve ? [] : undefined,
            observations: allowFieldObserve ? [] : undefined,
        };
        if (!_hasAtLeastOneRewriteRule(node.rules)) {
            ctx.error(`'${node.kind}' block must have at least one rewrite rule`, node.pos);
        }
        for (const rule of node.rules) {
            const r = ctx.resolveRule(rule, outGrid);
            if (r === undefined) {
                continue;
            }
            if (r.assigns !== undefined) {
                (out.assigns ??= []).push(...r.assigns);
            }
            for (const resolved of r.rules) {
                const arr = resolved.kind === 'rule.field' ? out.fields
                    : resolved.kind === 'rule.observe' ? out.observations
                        : resolved.kind === 'rule.rewrite' ? out.rewrites
                            : undefined;
                if (arr !== undefined) {
                    arr.push(resolved);
                }
                else {
                    ctx.error(`'${resolved.kind}' not allowed in '${node.kind}' block`, resolved.pos);
                }
            }
        }
        // sorting makes it more likely that samplers can be reused
        out.rewrites.sort((a, b) => _cmpPatternKey(a.from, b.from));
        out.observations?.sort((a, b) => _cmpPatternKey(a.from, b.from));
        return out;
    }
    function _cmpPatternKey(a, b) {
        const aKey = PatternTree.key(a), bKey = PatternTree.key(b);
        return aKey < bKey ? -1 : aKey === bKey ? 0 : 1;
    }
    /**
     * Conservatively estimates whether the given rewrite rules commute.
     */
    function _rewritesCommute(rewrites) {
        if (rewrites.length === 0) {
            return true;
        }
        const mapping = emptyArray(rewrites[0].to.type.alphabetKey.length, -1);
        return rewrites.every(({ from, to }) => to.kind === 'expr.constant' && from.kind === 'leaf' && to.constant.value.every((x, y, toColour) => ISet.every(from.masks[x + y * from.width], fromColour => {
            if (mapping[fromColour] >= 0 && mapping[fromColour] !== toColour) {
                return false;
            }
            mapping[fromColour] = toColour;
            return true;
        })));
    }
    // resolver functions which can handle multiple kinds of node
    function _resolveSimpleLiteralExpr(expr) {
        const kind = expr.kind.replace(/^expr\.literal\./, '');
        return _makeConstantExpr(Type.PRIMITIVES[kind], expr.value, expr.pos);
    }
    function _resolvePatternLiteralExpr(expr, ctx) {
        if (!ctx.expectGrid(expr.pos)) {
            return undefined;
        }
        const { alphabet } = ctx.grid;
        const { width, height, value } = expr;
        const pattern = [];
        const masks = [];
        let ok = true, hasUnions = false;
        for (const c of value) {
            let charID = -1, isUnion = false;
            let mask;
            if (c.kind === 'CHARSET') {
                mask = ctx.resolveCharSet(c);
                if (mask !== undefined) {
                    const size = ISet.size(mask);
                    if (size === 0) {
                        ctx.error('empty charset', c.pos);
                        ok = false;
                    }
                    else if (size === 1) {
                        charID = ISet.toArray(mask)[0];
                    }
                    else {
                        isUnion = size < alphabet.key.length;
                    }
                }
            }
            else {
                mask = ctx.resolveChar(c);
                if (mask !== undefined) {
                    charID = alphabet.map.getIDOrDefault(c.s);
                    isUnion = charID < 0 && c.s !== '.';
                }
            }
            if (mask !== undefined) {
                pattern.push(isUnion ? -2 : charID);
                masks.push(mask);
                hasUnions ||= isUnion;
            }
            else {
                ok = false;
            }
        }
        return ok ? new Pattern(width, height, alphabet.key, pattern, masks, hasUnions) : undefined;
    }
    function _resolveCountExpr(expr, ctx) {
        const { pos } = expr;
        if (!ctx.expectGrid(pos)) {
            return undefined;
        }
        const pattern = _resolveProp(expr, 'child', 'const pattern.in', ctx);
        if (pattern === PROP_ERROR) {
            return undefined;
        }
        const patterns = Symmetry.generate(pattern, ctx.symmetryName, PatternTree.rotate, PatternTree.reflect, PatternTree.key);
        // sorting makes it more likely that samplers can be reused
        patterns.sort(_cmpPatternKey);
        const inGrid = ctx.grid.id;
        const flags = 2 /* ExprFlags.DETERMINISTIC */ | 4 /* ExprFlags.LOCALLY_DETERMINISTIC */ | 8 /* ExprFlags.POSITION_INDEPENDENT */;
        return { kind: 'expr.count', type: Type.INT, flags, inGrid, patterns, pos };
    }
    function _resolveLoadExpr(expr, ctx) {
        if (!ctx.expectGrid(expr.pos)) {
            return undefined;
        }
        const { legend, key: alphabetKey } = ctx.grid.alphabet;
        if (legend === undefined) {
            ctx.error(`missing 'legend' declaration`, expr.pos);
            return undefined;
        }
        const path = _resolveProp(expr, 'child', 'const str', ctx);
        if (path === PROP_ERROR) {
            return undefined;
        }
        const width = -1, height = -1, hasUnions = legend.hasUnions;
        const type = { kind: hasUnions ? 'pattern.in' : 'pattern.out', alphabetKey, width, height };
        // TODO
        ctx.error(`'load' expression is currently unsupported`, expr.pos);
        return undefined;
    }
    function _resolveRandIntExpr(expr, ctx) {
        const { pos } = expr;
        const max = _resolveProp(expr, 'child', 'int', ctx);
        if (max === PROP_ERROR) {
            return undefined;
        }
        if (max.kind === 'expr.constant') {
            if (max.constant.value <= 0) {
                ctx.error(MJr.SAMPLE_EMPTY_MESSAGE, pos);
                return undefined;
            }
            if (max.constant.value === 1) {
                return _makeConstantExpr(Type.INT, 0, expr.pos);
            }
        }
        const flags = 16 /* ExprFlags.GRID_INDEPENDENT */ | 8 /* ExprFlags.POSITION_INDEPENDENT */;
        return { kind: 'expr.randint', type: Type.INT, flags, max, pos };
    }
    function _resolveSumExpr(expr, ctx) {
        const { pos } = expr;
        if (!ctx.expectGrid(expr.pos)) {
            return undefined;
        }
        const { kernel, grid } = ctx;
        if (kernel === undefined) {
            ctx.error(`'sum' expression may only be used 'convolution' statement`, pos);
            return undefined;
        }
        if (!ctx.isRuleContext) {
            ctx.error(`'sum' expression may only be used in a rule condition or output pattern`, pos);
            return undefined;
        }
        const chars = _resolveProp(expr, 'child', 'const charset.in', ctx);
        if (chars === PROP_ERROR) {
            return undefined;
        }
        switch (chars.kind) {
            case 'top': {
                const flags = 31 /* ExprFlags.CONSTANT */;
                return { kind: 'expr.attr.grid', type: Type.INT, flags, grid: grid.id, attr: 'area', pos };
            }
            case 'bottom': {
                return _makeConstantExpr(Type.INT, 0, pos);
            }
            case 'leaf': {
                const patternID = grid.convPatterns.getOrCreateID({ chars: chars.masks[0], kernel });
                const flags = 2 /* ExprFlags.DETERMINISTIC */ | 4 /* ExprFlags.LOCALLY_DETERMINISTIC */;
                return { kind: 'expr.sum', type: Type.INT, flags, inGrid: grid.id, patternID, pos };
            }
            default: {
                // logical ops on const 1x1 patterns should already be folded
                fail();
            }
        }
    }
    function _checkZero(ctx, child) {
        if (child.kind !== 'expr.constant') {
            const type = child.type;
            return { kind: 'expr.op.unary', type, flags: child.flags, op: `${type.kind}_checkzero`, child, pos: child.pos };
        }
        else if (child.constant.value !== 0) {
            return child;
        }
        else {
            ctx.diagnostics.compilationError('division by zero in constant expression', child.pos);
            return undefined;
        }
    }
    function _resolveObserveOrRewriteRule(rule, ctx, outGrid) {
        const { pos } = rule;
        let from = _resolveProp(rule, 'from', 'const pattern.in', ctx);
        if (from === PROP_ERROR) {
            return undefined;
        }
        ctx.inputPattern = from;
        ctx.isRuleContext = true;
        const via = _resolveProp(rule, 'via', 'pattern.out?', ctx);
        const to = ctx.withOutGrid(outGrid, rule.from.pos, () => _resolveProp(rule, 'to', 'pattern.out', ctx));
        ctx.inputPattern = undefined;
        const condition = _resolveProp(rule, 'condition', 'bool?', ctx) ?? _makeConstantExpr(Type.BOOL, true, pos);
        ctx.isRuleContext = false;
        if (via === PROP_ERROR || to === undefined || to === PROP_ERROR || condition === PROP_ERROR) {
            return undefined;
        }
        if (rule.kind === 'rule.rewrite' && to.kind === 'expr.constant') {
            from = PatternTree.and(from, PatternTree.not(to.constant.value));
        }
        if (from.kind === 'bottom') {
            ctx.error('rule has no effect', pos);
            return { rules: [] };
        }
        else if (condition.kind === 'expr.constant' && !condition.constant.value) {
            // condition is constant `false` expression
            return { rules: [] };
        }
        const rules = [];
        const makeRule = (from, via, to) => {
            rules.push({ kind: rule.kind, from, via, to, condition, pos });
        };
        if ((via === undefined || via.kind === 'expr.constant') && to.kind === 'expr.constant') {
            const symmetries = Symmetry.generate({ from, via: via?.constant?.value, to: to.constant.value }, ctx.symmetryName, s => ({ from: PatternTree.rotate(s.from), via: s.via && Pattern.rotate(s.via), to: Pattern.rotate(s.to) }), s => ({ from: PatternTree.reflect(s.from), via: s.via && Pattern.reflect(s.via), to: Pattern.reflect(s.to) }), s => `${PatternTree.key(s.from)} -> ${s.via && Pattern.key(s.via)} -> ${Pattern.key(s.to)}`);
            function makeExpr(p, original) {
                const { width, height } = p;
                const { alphabetKey } = original.type;
                return _makeConstantExpr({ kind: 'pattern.out', width, height, alphabetKey }, p, original.pos);
            }
            for (const s of symmetries) {
                makeRule(s.from, via && s.via && makeExpr(s.via, via), makeExpr(s.to, to));
            }
        }
        else {
            // TODO: need to apply symmetries as ASG ops
            if (ctx.symmetryName === 'none' || (from.width === 1 && from.height === 1 && to.type.width === 1 && to.type.height === 1)) {
                makeRule(from, via, to);
            }
            else {
                ctx.error(`non-constant pattern requires 'symmetry "none"'`, rule.pos);
            }
        }
        return { rules };
    }
    function _resetIsTrivial(reset) {
        return reset === undefined || reset.limitIDs.length === 0;
    }
    function _resolveBlockStmt(stmt, ctx, canReset) {
        const { kind, pos } = stmt;
        const oldReset = ctx.reset;
        const reset = canReset ? { kind, limitIDs: [] } : undefined;
        ctx.reset = reset;
        const children = ctx.resolveStmts(stmt.children, true);
        ctx.reset = oldReset;
        if (children.length === 0) {
            return undefined;
        }
        return { kind: 'stmt', stmt: { kind, children, reset: _resetIsTrivial(reset) ? undefined : reset, pos } };
    }
    function _resolvePropsStmt(stmt, ctx) {
        if (!ctx.expectGrid(stmt.pos)) {
            return undefined;
        }
        const props = _resolveProps(stmt, ctx);
        if (props === undefined) {
            return undefined;
        }
        return { kind: 'stmt', stmt: { kind: stmt.kind, inGrid: ctx.grid.id, pos: stmt.pos, ...props } };
    }
    function _resolveAllOnceOneStmt(stmt, ctx) {
        const { pos } = stmt;
        if (!ctx.expectGrid(pos)) {
            return undefined;
        }
        const props = stmt.kind !== 'stmt.rules.once' ? _resolveProps(stmt, ctx) : { search: undefined, maxStates: undefined, depthCoefficient: undefined, temperature: undefined };
        if (props === undefined) {
            return undefined;
        }
        const { search: isSearch = false, maxStates, depthCoefficient, temperature } = props;
        const { rewrites, fields, observations, assigns } = _resolveRules(stmt, ctx, ctx.grid, true);
        if (rewrites.length === 0) {
            return undefined;
        }
        const inGrid = ctx.grid.id;
        const kind = stmt.kind === 'stmt.rules.all' ? 'all' : 'one';
        const isBasic = fields.length === 0 && observations.length === 0;
        if (isSearch) {
            for (const field of fields) {
                ctx.error(`'field' cannot be used with 'search'`, field.pos);
            }
            if (observations.length === 0) {
                ctx.error(`'search' requires at least one 'observe'`, pos);
            }
        }
        else {
            if (maxStates !== undefined) {
                ctx.error(`argument 'maxStates' can only be used with 'search'`, maxStates.pos);
            }
            if (depthCoefficient !== undefined) {
                ctx.error(`argument 'depthCoefficient' can only be used with 'search'`, depthCoefficient.pos);
            }
        }
        let r;
        if (isBasic) {
            if (temperature !== undefined) {
                ctx.error(`'temperature' requires at least one 'field' or 'observe'`, temperature.pos);
            }
            r = { kind: `stmt.rules.basic.${kind}`, inGrid, rewrites, commutative: _rewritesCommute(rewrites), pos };
        }
        else if (isSearch) {
            r = { kind: `stmt.rules.search.${kind}`, inGrid, temperature, maxStates, depthCoefficient, rewrites, observations, pos };
        }
        else {
            if (fields.length > 0 && observations.length > 0) {
                ctx.error(`cannot have 'field' and 'observe' rules in the same block`, pos);
            }
            r = { kind: `stmt.rules.biased.${kind}`, inGrid, temperature, rewrites, fields, observations, pos };
        }
        if (stmt.kind === 'stmt.rules.once') {
            const limit = ctx.makeLimit(_makeConstantExpr(Type.INT, 1, pos));
            r = { kind: 'stmt.modified.limit', limit, child: r, pos };
        }
        return { kind: 'stmt', assigns, stmt: r };
    }
    const DECL_RESOLVE_FUNCS = {
        'decl.legend': (decl, ctx, f) => [
            undefined,
            ctx.withLegend(decl, f),
        ],
        'decl.let': (decl, ctx, f) => {
            const { name, pos } = decl.name;
            let rhs = ctx.resolveExpr(decl.rhs);
            if (rhs === undefined) {
                return [undefined, ctx.withErrorVariable(name, f)];
            }
            const { type, flags } = rhs;
            if (decl.isParam) {
                rhs = { kind: 'expr.param', type, flags, otherwise: rhs, name, pos };
            }
            const isMutable = (flags & 1 /* ExprFlags.RUNTIME_CONSTANT */) === 0;
            const variable = ctx.makeVariable(name, type, flags, isMutable ? undefined : rhs, decl.isParam, decl.name.pos);
            return [
                // constants will be folded, or assigned in preamble; not assigned in program body
                isMutable ? { kind: 'stmt.assign', variable, rhs, pos } : undefined,
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
    const EXPR_RESOLVE_FUNCS = {
        'expr.attr': (expr, ctx) => {
            const left = _resolveProp(expr, 'left', 'object', ctx);
            if (left === PROP_ERROR) {
                return undefined;
            }
            const { attr, pos } = expr;
            const { kind } = left.type;
            const entryTypes = kind === 'grid' ? Type.GRID_ATTRS
                : kind === 'position' ? Type.POSITION_ATTRS
                    : left.type.entryTypes;
            const type = entryTypes.get(attr);
            if (type === undefined) {
                ctx.diagnostics.typeError(`type '${Type.toStr(left.type)}' has no attribute named '${attr}'`, pos);
                return undefined;
            }
            if (kind === 'grid') {
                const grid = _resolveProp(expr, 'left', 'const grid', ctx);
                if (grid === PROP_ERROR) {
                    return undefined;
                }
                return { kind: 'expr.attr.grid', type, flags: 31 /* ExprFlags.CONSTANT */, grid, attr: attr, pos };
            }
            else if (left.kind === 'expr.constant') {
                if (kind === 'dict') {
                    const constant = left.constant.value.get(attr) ?? fail();
                    return _makeConstantExpr(type, constant.value, pos);
                }
                else if (kind === 'position') {
                    const value = left.constant.value[attr];
                    return _makeConstantExpr(type, value, pos);
                }
            }
            return { kind: `expr.attr.${kind}`, type, flags: left.flags, left, attr, pos };
        },
        'expr.decl': (expr, ctx) => {
            const [decl, child] = ctx.resolveDecl(expr.declaration, () => ctx.resolveExpr(expr.child));
            return decl === undefined || child === undefined || child.kind === 'expr.constant'
                ? child
                : { kind: 'expr.decl', type: child.type, flags: decl.rhs.flags & child.flags, decl, child, pos: decl.pos };
        },
        'expr.dict': (expr, ctx) => {
            const { pos } = expr;
            const entryExprs = new Map();
            const entryTypes = new Map();
            const value = new Map();
            let ok = true, flags = 31 /* ExprFlags.ALL */;
            for (const [{ name }, v] of expr.pairs) {
                const resolved = ctx.resolveExpr(v);
                if (resolved !== undefined) {
                    entryExprs.set(name, resolved);
                    entryTypes.set(name, resolved.type);
                    if (resolved.kind === 'expr.constant') {
                        value.set(name, resolved.constant);
                    }
                    flags &= resolved.flags;
                }
                else {
                    ok = false;
                }
            }
            if (!ok) {
                return undefined;
            }
            const type = { kind: 'dict', entryTypes };
            if (value.size === entryExprs.size) {
                return _makeConstantExpr(type, value, pos);
            }
            return { kind: 'expr.dict', type, flags, entryExprs, pos };
        },
        'expr.grid': (expr, ctx) => {
            const { pos } = expr;
            const props = _resolveProps(expr, ctx);
            if (props === undefined) {
                return undefined;
            }
            const { scaleX = 1, scaleY = 1 } = props;
            if (scaleX <= 0) {
                ctx.error(`'scaleX' must be positive`, expr.scaleX.pos);
            }
            if (scaleY <= 0) {
                ctx.error(`'scaleY' must be positive`, expr.scaleY.pos);
            }
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
            if (p === undefined) {
                return undefined;
            }
            const { width, height, alphabetKey } = p;
            return _makeConstantExpr({ kind: p.hasUnions ? 'pattern.in' : 'pattern.out', alphabetKey, width, height }, p.isTop() ? PatternTree.top(width, height) : p, expr.pos);
        },
        'expr.literal.str': _resolveSimpleLiteralExpr,
        'expr.name.keyword': (expr, ctx) => {
            const { name, pos } = expr;
            const flags = {
                at: 2 /* ExprFlags.DETERMINISTIC */ | 4 /* ExprFlags.LOCALLY_DETERMINISTIC */ | 16 /* ExprFlags.GRID_INDEPENDENT */,
                origin: 2 /* ExprFlags.DETERMINISTIC */ | 4 /* ExprFlags.LOCALLY_DETERMINISTIC */ | 16 /* ExprFlags.GRID_INDEPENDENT */ | 1 /* ExprFlags.RUNTIME_CONSTANT */ | 8 /* ExprFlags.POSITION_INDEPENDENT */,
                random: 16 /* ExprFlags.GRID_INDEPENDENT */ | 8 /* ExprFlags.POSITION_INDEPENDENT */,
            }[name];
            switch (name) {
                case 'random':
                    return { kind: 'expr.name.keyword', type: Type.FLOAT, flags, name, pos };
            }
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            switch (name) {
                case 'at':
                    if (!ctx.isRuleContext) {
                        ctx.error(`'at' expression may only be used in a rule condition or output pattern`, pos);
                    }
                    break;
            }
            const type = { kind: 'position', inGrid: ctx.grid.id };
            return { kind: 'expr.name.keyword', type, flags, name, pos };
        },
        'expr.name.simple': (expr, ctx) => {
            const { name, pos } = expr;
            if (ctx.errorVariables.has(name)) {
                return undefined;
            }
            const variable = ctx.variables.get(name);
            if (variable === undefined) {
                ctx.error(`no such variable '${name}'`, pos);
                return undefined;
            }
            const { id: variableID, type, initialiser } = variable;
            if (initialiser !== undefined && initialiser.kind === 'expr.constant') {
                return _makeConstantExpr(type, initialiser.constant.value, pos);
            }
            else {
                ++variable.references;
                return { kind: 'expr.name.simple', type, flags: variable.flags, variableID, pos };
            }
        },
        'expr.op.binary': (expr, ctx) => {
            let left = ctx.resolveExpr(expr.left), right = ctx.resolveExpr(expr.right);
            if (left === undefined || right === undefined) {
                return undefined;
            }
            const { pos } = expr;
            if ((expr.op === 'and' || expr.op === 'or') && (left.type.kind === 'pattern.in' || left.type.kind === 'pattern.out')) {
                const type = { ...left.type, kind: 'pattern.in' };
                if (!ctx.checkType(type, right)) {
                    return undefined;
                }
                if (left.kind !== 'expr.constant' || right.kind !== 'expr.constant') {
                    // TODO?
                    ctx.error(`'${expr.op}' may only be used on constant patterns`, pos);
                    return undefined;
                }
                const value = PatternTree[expr.op](left.constant.value, right.constant.value);
                return _makeConstantExpr(type, value, pos);
            }
            // type coercion, from int to float or fraction
            if (left.type.kind === 'int' && (right.type.kind === 'float' || right.type.kind === 'fraction')) {
                left = _coerceFromInt(left, right.type);
            }
            else if (right.type.kind === 'int' && (left.type.kind === 'float' || left.type.kind === 'fraction')) {
                right = _coerceFromInt(right, left.type);
            }
            else if (left.type.kind === 'str' && expr.op === '+') {
                right = _coerceToStr(right);
            }
            else if (right.type.kind === 'str' && expr.op === '+') {
                left = _coerceToStr(left);
            }
            const spec = Op.BINARY_OP_TYPES[expr.op];
            for (const [leftType, rightType, outType, op] of spec) {
                // OK to only check `kind`, since it's a primitive type
                if (left.type.kind !== leftType) {
                    continue;
                }
                // binary operators don't have multiple `rightType` options for the same `left.type`
                if (!ctx.checkType(Type.PRIMITIVES[rightType], right)) {
                    return undefined;
                }
                switch (op) {
                    case 'float_truediv':
                    case 'float_mod':
                    case 'fraction_truediv':
                    case 'int_truediv':
                    case 'int_floordiv':
                    case 'int_mod':
                        right = _checkZero(ctx, right);
                        if (right === undefined) {
                            return undefined;
                        }
                        break;
                }
                const type = Type.PRIMITIVES[outType];
                const f = Op.BINARY_FUNCS[op];
                if (left.kind === 'expr.constant' && right.kind === 'expr.constant') {
                    const value = f(left.constant.value, right.constant.value);
                    if (value !== undefined) {
                        return _makeConstantExpr(type, value, pos);
                    }
                }
                const flags = left.flags & right.flags;
                return { kind: 'expr.op.binary', type, flags, op, left, right, pos };
            }
            ctx.typeError(spec.map(opt => opt[0]), left);
            return undefined;
        },
        'expr.op.ternary': (expr, ctx) => {
            const condition = _resolveProp(expr, 'condition', 'bool', ctx);
            let then = ctx.resolveExpr(expr.then);
            let otherwise = ctx.resolveExpr(expr.otherwise);
            if (condition === PROP_ERROR || then === undefined || otherwise === undefined) {
                return undefined;
            }
            if (then.type.kind === 'int' && (otherwise.type.kind === 'float' || otherwise.type.kind === 'fraction')) {
                then = _coerceFromInt(then, otherwise.type);
            }
            else if (otherwise.type.kind === 'int' && (then.type.kind === 'float' || then.type.kind === 'fraction')) {
                otherwise = _coerceFromInt(otherwise, then.type);
            }
            else if (then.type.kind === 'str') {
                otherwise = _coerceToStr(otherwise);
            }
            else if (otherwise.type.kind === 'str') {
                then = _coerceToStr(then);
            }
            const type = Type.leastUpperBound(then.type, otherwise.type);
            if (type === undefined) {
                ctx.typeError([Type.toStr(then.type)], otherwise);
                return undefined;
            }
            const flags = condition.flags & then.flags & otherwise.flags;
            if (condition.kind === 'expr.constant') {
                return condition.constant.value ? then : otherwise;
            }
            return { kind: 'expr.op.ternary', type, flags, condition, then, otherwise, pos: expr.pos };
        },
        'expr.op.unary': (expr, ctx) => {
            switch (expr.op) {
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
            if (child === undefined) {
                return undefined;
            }
            const { pos } = expr;
            if (expr.op === 'not' && (child.type.kind === 'pattern.in' || child.type.kind === 'pattern.out')) {
                const type = { ...child.type, kind: 'pattern.in' };
                if (child.kind !== 'expr.constant') {
                    // TODO?
                    ctx.error(`'not' may only be used on constant patterns`, pos);
                    return undefined;
                }
                const value = PatternTree.not(child.constant.value);
                return _makeConstantExpr(type, value, pos);
            }
            // unary + is a NOOP
            if (expr.op === '+') {
                return ctx.checkType(Type.NUMERIC, child) ? child : undefined;
            }
            const spec = Op.UNARY_OP_TYPES[expr.op];
            for (const [inType, outType, op] of spec) {
                // OK to only check `kind`, since it's a primitive type
                if (child.type.kind !== inType) {
                    continue;
                }
                const type = Type.PRIMITIVES[outType];
                const f = Op.UNARY_FUNCS[op];
                if (child.kind === 'expr.constant' && f !== undefined) {
                    const value = f(child.constant.value);
                    if (value !== undefined) {
                        return _makeConstantExpr(type, value, pos);
                    }
                }
                return { kind: 'expr.op.unary', type, flags: child.flags, op, child, pos };
            }
            ctx.typeError(spec.map(opt => opt[0]), child);
            return undefined;
        },
    };
    const RULE_RESOLVE_FUNCS = {
        'rule.decl': (rule, ctx, outGrid) => {
            const assigns = [];
            const rules = [];
            let [decl, _] = ctx.resolveDecl(rule.declaration, () => {
                for (const c of rule.children) {
                    const r = ctx.resolveRule(c, outGrid);
                    if (r === undefined) {
                        continue;
                    }
                    if (r.assigns !== undefined) {
                        assigns.push(...r.assigns);
                    }
                    rules.push(...r.rules);
                }
            });
            if (decl !== undefined) {
                assigns.unshift(decl);
            }
            return { assigns, rules };
        },
        'rule.field': (rule, ctx, outGrid) => {
            const { pos } = rule;
            const props = _resolveProps(rule, ctx);
            if (props === undefined) {
                return undefined;
            }
            const { for_, on, from, to, recompute = false, essential = false } = props;
            const zero = from ?? to;
            const inversed = from !== undefined;
            if (zero === undefined) {
                ctx.error(`'field' must have either 'from' or 'to'`, pos);
                return undefined;
            }
            else if (from !== undefined && to !== undefined) {
                ctx.error(`'field' cannot have both 'from' and 'to'`, pos);
            }
            const potential = withNextID(ctx.globals.potentials, { inGrid: ctx.grid.id, for_ });
            return { rules: [{ kind: 'rule.field', potential, for_, on, zero, inversed, recompute, essential, pos }] };
        },
        'rule.observe': _resolveObserveOrRewriteRule,
        'rule.rewrite': _resolveObserveOrRewriteRule,
    };
    const STMT_RESOLVE_FUNCS = {
        'stmt.block.markov': _resolveBlockStmt,
        'stmt.block.sequence': _resolveBlockStmt,
        'stmt.convchain': _resolvePropsStmt,
        'stmt.decl': (stmt, ctx, canReset) => {
            let [decl, stmts] = ctx.resolveDecl(stmt.declaration, () => ctx.resolveStmts(stmt.children, canReset));
            stmts ??= [];
            if (decl !== undefined) {
                stmts.unshift(decl);
            }
            return { kind: 'stmts', stmts };
        },
        'stmt.log': _resolvePropsStmt,
        'stmt.modified.limit': (stmt, ctx, canReset) => {
            const value = _resolveProp(stmt, 'arg', 'int', ctx);
            const r = ctx.resolveStmt(stmt.child, canReset);
            if (value === PROP_ERROR) {
                return undefined;
            }
            if (value.kind === 'expr.constant' && value.constant.value <= 0) {
                ctx.error(`limit must be positive (was ${value})`, stmt.arg.pos);
            }
            // TODO: loosen this to allow e.g. random limits; problem is that limit initialisers will be hoisted
            // to the start of their parent blocks, where referenced variables might not yet be assigned
            if ((value.flags & 1 /* ExprFlags.RUNTIME_CONSTANT */) === 0) {
                ctx.error(`limit must be a runtime constant`, stmt.arg.pos);
            }
            if (r === undefined) {
                return undefined;
            }
            else if (r.kind === 'stmts' || r.stmt.kind === 'stmt.log' || r.stmt.kind === 'stmt.rules.map' || r.stmt.kind === 'stmt.put' || r.stmt.kind === 'stmt.use') {
                ctx.error(`'@limit' cannot modify '${stmt.child.kind}'`, stmt.child.pos);
                return undefined;
            }
            else if (r.stmt.kind === 'stmt.modified.limit') {
                ctx.error(`statement cannot have multiple limits`, stmt.child.pos);
            }
            const { assigns, stmt: child } = r;
            const limit = ctx.makeLimit(value);
            return { kind: 'stmt', assigns, stmt: { kind: 'stmt.modified.limit', limit, child, pos: stmt.pos } };
        },
        'stmt.path': _resolvePropsStmt,
        'stmt.pass': (stmt, ctx) => undefined,
        'stmt.put': (stmt, ctx) => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            const at = _resolveProp(stmt, 'at', 'position', ctx);
            ctx.isRuleContext = true;
            const props = _resolveProps(stmt, ctx);
            ctx.isRuleContext = false;
            if (at === PROP_ERROR || props === undefined) {
                return undefined;
            }
            const { pattern, condition } = props;
            const inGrid = ctx.grid.id;
            return { kind: 'stmt', stmt: { kind: 'stmt.put', inGrid, at, pattern, condition, pos } };
        },
        'stmt.rules.all': _resolveAllOnceOneStmt,
        'stmt.rules.convolution': (stmt, ctx) => ctx.withKernel(stmt, kernel => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            const { rewrites, assigns } = _resolveRules(stmt, ctx, ctx.grid, false);
            if (rewrites.length === 0) {
                return undefined;
            }
            const charsUsed = ISet.empty(ctx.grid.alphabet.key.length);
            for (const rule of rewrites) {
                if (rule.from.width !== 1 || rule.from.height !== 1) {
                    ctx.error(`'convolution' rule patterns must be 1x1`, rule.pos);
                }
                if (rule.from.kind === 'bottom') {
                    continue;
                }
                // logical ops on const 1x1 patterns should already be folded
                if (rule.from.kind !== 'leaf' && rule.from.kind !== 'top') {
                    fail();
                }
                const chars = rule.from.kind === 'top' ? ctx.grid.alphabet.wildcard : rule.from.masks[0];
                if (!ISet.isDisjoint(chars, charsUsed)) {
                    ctx.error(`'convolution' input patterns must be disjoint`, rule.pos);
                }
                ISet.addAll(charsUsed, chars);
            }
            return {
                kind: 'stmt',
                assigns,
                stmt: { kind: 'stmt.rules.convolution', inGrid: ctx.grid.id, rewrites, kernel, pos },
            };
        }),
        'stmt.rules.map': (stmt, ctx) => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            const inGrid = ctx.grid.id;
            const outGrid = _resolveProp(stmt, 'outGrid', 'const grid', ctx);
            if (outGrid === PROP_ERROR) {
                return undefined;
            }
            if (outGrid === inGrid) {
                ctx.error(`'outGrid' must be different to the input grid`, stmt.outGrid.pos);
            }
            const formalOutGrid = ctx.globals.grids[outGrid];
            const { assigns, rewrites } = _resolveRules(stmt, ctx, formalOutGrid, false);
            ctx.grid = formalOutGrid;
            if (rewrites.length === 0) {
                return undefined;
            }
            const commutative = rewrites.every(rule => rule.from.width === 1 && rule.from.height === 1);
            return { kind: 'stmt', assigns, stmt: { kind: 'stmt.rules.map', inGrid, outGrid, rewrites, commutative, pos } };
        },
        'stmt.rules.once': _resolveAllOnceOneStmt,
        'stmt.rules.one': _resolveAllOnceOneStmt,
        'stmt.rules.prl': (stmt, ctx) => {
            const { pos } = stmt;
            if (!ctx.expectGrid(pos)) {
                return undefined;
            }
            const { rewrites, assigns } = _resolveRules(stmt, ctx, ctx.grid, false);
            if (rewrites.length === 0) {
                return undefined;
            }
            return {
                kind: 'stmt',
                assigns,
                stmt: { kind: 'stmt.rules.basic.prl', inGrid: ctx.grid.id, rewrites, commutative: _rewritesCommute(rewrites), pos },
            };
        },
        'stmt.use.expr': (stmt, ctx) => {
            const grid = _resolveProp(stmt, 'expr', 'const grid', ctx);
            if (grid === PROP_ERROR) {
                return undefined;
            }
            ctx.grid = ctx.globals.grids[grid];
            return { kind: 'stmt', stmt: { kind: 'stmt.use', grid, pos: stmt.pos } };
        },
        'stmt.use.let': (stmt, ctx, canReset) => {
            if (stmt.decl.isParam) {
                ctx.error(`'use let' declaration cannot be a 'param'`, stmt.pos);
            }
            const grid = _resolveProp(stmt.decl, 'rhs', 'const grid', ctx);
            if (grid === PROP_ERROR) {
                return undefined;
            }
            ctx.grid = ctx.globals.grids[grid];
            const { name, rhs, pos } = stmt.decl;
            const variable = ctx.makeVariable(name.name, Type.GRID, 31 /* ExprFlags.CONSTANT */, _makeConstantExpr(Type.GRID, grid, rhs.pos), false, name.pos);
            const stmts = ctx.withVariable(variable, () => ctx.resolveStmts(stmt.children, canReset));
            stmts.unshift({ kind: 'stmt.assign', variable, rhs: _makeConstantExpr(Type.GRID, grid, rhs.pos), pos }, { kind: 'stmt.use', grid, pos: stmt.pos });
            return { kind: 'stmts', stmts };
        },
    };
    function resolve(ast) {
        const ctx = new Context();
        const root = ctx.resolveRoot(ast);
        const endGrid = ctx.grid;
        if (endGrid === undefined) {
            ctx.error('program uses no grid', ast.pos);
        }
        ctx.diagnostics.throwIfAnyErrors();
        const { grids, limits, params, potentials, variables } = ctx.globals;
        return {
            root,
            grids,
            limits,
            params,
            potentials,
            variables,
            endGridID: endGrid.id,
        };
    }
    Resolver.resolve = resolve;
})(Resolver || (Resolver = {}));
var Symmetry;
(function (Symmetry) {
    Symmetry.SYMMETRY_GROUPS = ((groups) => groups)({
        all: [true, true, true, true, true, true, true, true],
        none: [true, false, false, false, false, false, false, false],
        rot90: [true, true, true, true, false, false, false, false],
        rot180: [true, false, true, false, false, false, false, false],
        x: [true, false, false, false, false, false, true, false],
        y: [true, false, false, false, true, false, false, false],
        xy: [true, false, true, false, true, false, true, false],
    });
    const TRANSFORMS = [
        { a: 1, b: 0, c: 0, d: 1 },
        { a: 0, b: 1, c: -1, d: 0 },
        { a: -1, b: 0, c: 0, d: -1 },
        { a: 0, b: -1, c: 1, d: 0 },
        { a: 1, b: 0, c: 0, d: -1 },
        { a: 0, b: 1, c: 1, d: 0 },
        { a: -1, b: 0, c: 0, d: 1 },
        { a: 0, b: -1, c: -1, d: 0 }, // flip_yx
    ];
    function transformAll(p, groupName) {
        const group = Symmetry.SYMMETRY_GROUPS[groupName];
        const out = [];
        for (let i = 0; i < group.length; ++i) {
            if (group[i]) {
                out.push(transform(p, TRANSFORMS[i]));
            }
        }
        return out;
    }
    Symmetry.transformAll = transformAll;
    /**
     * Applies a transformation matrix to a pattern.
     */
    function transform(p, m) {
        const { width, height, pattern, masks } = p;
        const newData = [];
        const newMasks = [];
        const newWidth = m.b === 0 ? width : height;
        const newHeight = m.b === 0 ? height : width;
        const xOffset = m.a < 0 || m.b < 0 ? newWidth - 1 : 0;
        const yOffset = m.c < 0 || m.d < 0 ? newHeight - 1 : 0;
        for (let y = 0; y < newHeight; ++y) {
            for (let x = 0; x < newWidth; ++x) {
                const px = m.a * x + m.b * y + xOffset;
                const py = m.c * x + m.d * y + yOffset;
                const index = px + width * py;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(newWidth, newHeight, p.alphabetKey, newData, newMasks, p.hasUnions);
    }
    Symmetry.transform = transform;
    function generate(original, groupName, rotate, reflect, keyFunc) {
        const r1 = rotate(original), r2 = rotate(r1), r3 = rotate(r2), s0 = reflect(original), s1 = reflect(r1), s2 = reflect(r2), s3 = reflect(r3);
        const group = Symmetry.SYMMETRY_GROUPS[groupName];
        const out = [original, r1, r2, r3, s0, s1, s2, s3].filter((x, i) => group[i]);
        return keyFunc !== undefined ? IDMap.distinctByKey(out, keyFunc) : out;
    }
    Symmetry.generate = generate;
})(Symmetry || (Symmetry = {}));
var Tokenizer;
(function (Tokenizer) {
    Tokenizer.KEYWORDS = [
        'all',
        'and',
        'at',
        'convchain',
        'convolution',
        'count',
        'else',
        'false',
        'field',
        'grid',
        'if',
        'in',
        'legend',
        'let',
        'limit',
        'load',
        'log',
        'map',
        'markov',
        'not',
        'observe',
        'once',
        'one',
        'or',
        'origin',
        'param',
        'pass',
        'path',
        'prl',
        'put',
        'randint',
        'random',
        'sequence',
        'sum',
        'symmetry',
        'true',
        'union',
        'use',
    ];
    const C_MAP = (function (...pairs) {
        const arr = emptyArray(128, 21 /* C.OTHER */);
        for (const [chars, c] of pairs) {
            for (let i = 0; i < chars.length; ++i) {
                arr[chars.charCodeAt(i)] = c;
            }
        }
        return arr;
    })([' \t', 0 /* C.WHITESPACE */], ['abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_', 1 /* C.LETTER_OR_UNDERSCORE */], ['0123456789', 2 /* C.DIGIT */], ['({', 3 /* C.LPAREN */], [')}', 4 /* C.RPAREN */], ['[', 5 /* C.LSQB */], [']', 6 /* C.RSQB */], ['<', 7 /* C.LANGLE */], ['>', 8 /* C.RANGLE */], ["'", 9 /* C.QUOTE */], ['"', 10 /* C.DBLQUOTE */], ['=', 11 /* C.EQUALS */], ['!', 12 /* C.EXCLAMATION_MARK */], ['-', 13 /* C.MINUS */], ['/', 14 /* C.SLASH */], ['#', 15 /* C.HASH */], ['\\', 16 /* C.BACKSLASH */], ['^', 17 /* C.CARET */], ['.', 18 /* C.DOT */], ['+*%', 19 /* C.OTHER_OP */], ['@|,:', 20 /* C.OTHER_PUNCTUATION */]);
    class LineTokenizer {
        lineString;
        lineCodes;
        constructor(lineString) {
            this.lineString = lineString;
            this.lineCodes = makeArray(lineString.length, i => {
                const charCode = lineString.charCodeAt(i);
                return charCode >= 0 && charCode < C_MAP.length ? C_MAP[charCode] : 21 /* C.OTHER */;
            });
        }
        scan(i, cs) {
            const { lineCodes } = this;
            while (i < lineCodes.length && cs.includes(lineCodes[i])) {
                ++i;
            }
            return i;
        }
        has(i, c) {
            const { lineCodes } = this;
            return i < lineCodes.length && lineCodes[i] === c;
        }
        getNextToken(mode, col, depth) {
            const { lineCodes } = this;
            const c = lineCodes[col];
            switch (mode) {
                case 0 /* Mode.NORMAL */:
                    switch (c) {
                        case 0 /* C.WHITESPACE */:
                            return ['WHITESPACE', this.scan(col, [0 /* C.WHITESPACE */]), depth, mode];
                        case 15 /* C.HASH */:
                            return ['COMMENT', lineCodes.length, depth, mode];
                        case 3 /* C.LPAREN */:
                            return ['PUNCTUATION', col + 1, depth + 1, mode];
                        case 4 /* C.RPAREN */:
                            return ['PUNCTUATION', col + 1, depth - 1, mode];
                        case 5 /* C.LSQB */:
                            return ['PUNCTUATION', col + 1, depth + 1, 1 /* Mode.PATTERN */];
                        case 9 /* C.QUOTE */:
                            return ['QUOTE', col + 1, depth, 3 /* Mode.QUOTE_STRING */];
                        case 10 /* C.DBLQUOTE */:
                            return ['QUOTE', col + 1, depth, 4 /* Mode.DBLQUOTE_STRING */];
                        case 1 /* C.LETTER_OR_UNDERSCORE */: {
                            const end = this.scan(col, [1 /* C.LETTER_OR_UNDERSCORE */, 2 /* C.DIGIT */]);
                            const s = this.lineString.substring(col, end);
                            return [KEYWORDS_SET.has(s) ? 'KEYWORD' : 'NAME', end, depth, mode];
                        }
                        case 11 /* C.EQUALS */:
                        case 12 /* C.EXCLAMATION_MARK */:
                        case 7 /* C.LANGLE */:
                        case 8 /* C.RANGLE */:
                            // '=', '!', '<', '>', '==', '!=', '<=' or '>='
                            // note that '!' is not a valid operator, but this will be caught by the parser
                            return ['OP', this.has(col + 1, 11 /* C.EQUALS */) ? col + 2 : col + 1, depth, mode];
                        case 14 /* C.SLASH */:
                            // '/' or '//'
                            return ['OP', this.has(col + 1, 14 /* C.SLASH */) ? col + 2 : col + 1, depth, mode];
                        case 13 /* C.MINUS */:
                            // INT, FLOAT, '-' or '->'
                            if (!this.has(col + 1, 2 /* C.DIGIT */)) {
                                return ['OP', this.has(col + 1, 8 /* C.RANGLE */) ? col + 2 : col + 1, depth, mode];
                            }
                            ++col;
                        // intentional fall-through
                        case 2 /* C.DIGIT */: {
                            const end = this.scan(col, [2 /* C.DIGIT */]);
                            if (!this.has(end, 18 /* C.DOT */)) {
                                return ['INT', end, depth, mode];
                            }
                            else if (this.has(end + 1, 2 /* C.DIGIT */)) {
                                return ['FLOAT', this.scan(end + 1, [2 /* C.DIGIT */]), depth, mode];
                            }
                            else {
                                return ['ERROR', end + 1, depth, mode];
                            }
                        }
                        case 18 /* C.DOT */:
                        case 19 /* C.OTHER_OP */:
                            // '.', '+', '*' or '%'
                            return ['OP', col + 1, depth, mode];
                        case 20 /* C.OTHER_PUNCTUATION */:
                            return ['PUNCTUATION', col + 1, depth, mode];
                        default:
                            return ['ERROR', col + 1, depth, mode];
                    }
                case 1 /* Mode.PATTERN */:
                case 2 /* Mode.CHARSET */:
                    switch (c) {
                        case 0 /* C.WHITESPACE */:
                            return ['WHITESPACE', this.scan(col, [0 /* C.WHITESPACE */]), depth, mode];
                        case 15 /* C.HASH */:
                            return ['COMMENT', lineCodes.length, depth, mode];
                        case 5 /* C.LSQB */:
                            return mode === 1 /* Mode.PATTERN */
                                ? ['PUNCTUATION', col + 1, depth + 1, 2 /* Mode.CHARSET */]
                                : ['ERROR', col + 1, depth, mode];
                        case 6 /* C.RSQB */:
                            return ['PUNCTUATION', col + 1, depth - 1, mode === 1 /* Mode.PATTERN */ ? 0 /* Mode.NORMAL */ : 1 /* Mode.PATTERN */];
                        case 17 /* C.CARET */:
                            return [mode === 2 /* Mode.CHARSET */ ? 'PUNCTUATION' : 'ERROR', col + 1, depth, mode];
                        case 18 /* C.DOT */:
                            return [mode === 1 /* Mode.PATTERN */ ? 'PATTERN_CHAR' : 'ERROR', col + 1, depth, mode];
                        case 1 /* C.LETTER_OR_UNDERSCORE */:
                        case 2 /* C.DIGIT */:
                        case 11 /* C.EQUALS */:
                        case 12 /* C.EXCLAMATION_MARK */:
                        case 13 /* C.MINUS */:
                        case 19 /* C.OTHER_OP */:
                        case 20 /* C.OTHER_PUNCTUATION */:
                        case 21 /* C.OTHER */:
                            return ['PATTERN_CHAR', col + 1, depth, mode];
                        default:
                            return ['ERROR', col + 1, depth, mode];
                    }
                case 3 /* Mode.QUOTE_STRING */:
                case 4 /* Mode.DBLQUOTE_STRING */:
                    switch (c) {
                        case 9 /* C.QUOTE */:
                        case 10 /* C.DBLQUOTE */:
                            return (mode === 3 /* Mode.QUOTE_STRING */ && c === 9 /* C.QUOTE */) || (mode === 4 /* Mode.DBLQUOTE_STRING */ && c === 10 /* C.DBLQUOTE */)
                                ? ['QUOTE', col + 1, depth, 0 /* Mode.NORMAL */]
                                : ['STRING_CHAR', col + 1, depth, mode];
                        case 16 /* C.BACKSLASH */:
                            return ['ESCAPED_CHAR', Math.min(col + 2, lineCodes.length), depth, mode];
                        default:
                            return ['STRING_CHAR', col + 1, depth, mode];
                    }
            }
        }
    }
    const KEYWORDS_SET = new Set(Tokenizer.KEYWORDS);
    function tokenize(src, skipWhitespace = false) {
        const lines = src.split('\n');
        const tokens = [];
        const indentation = [''];
        const diagnostics = new Diagnostics();
        function _makeToken(kind, s, pos) {
            if ((kind === 'WHITESPACE' || kind === 'COMMENT') && skipWhitespace) {
                return;
            }
            if (kind === 'NAME' && KEYWORDS_SET.has(s)) {
                kind = 'KEYWORD';
            }
            tokens.push({ kind, s, pos });
        }
        let depth = 0;
        let mode = 0 /* Mode.NORMAL */;
        for (let line = 1; line <= lines.length; ++line) {
            const lineString = lines[line - 1];
            // ignore lines with only whitespace and comments
            if (/^\s*(?:#.*)?$/.test(lineString)) {
                const i = lineString.indexOf('#');
                if (i >= 0) {
                    if (i > 0) {
                        _makeToken('WHITESPACE', lineString.substring(0, i), { line, col: 0 });
                    }
                    _makeToken('COMMENT', lineString.substring(i), { line, col: i });
                }
                else if (lineString.length > 0) {
                    _makeToken('WHITESPACE', lineString, { line, col: 0 });
                }
                if (!skipWhitespace) {
                    _makeToken('NEWLINE', '\n', { line, col: lineString.length });
                }
                continue;
            }
            let col = 0;
            // check for indent or dedents
            if (mode === 0 /* Mode.NORMAL */ && depth === 0) {
                const initialWhitespace = /^\s*/.exec(lineString)[0];
                const currentIndentation = indentation[indentation.length - 1];
                if (initialWhitespace.startsWith(currentIndentation)) {
                    if (initialWhitespace.length > currentIndentation.length) {
                        indentation.push(initialWhitespace);
                        _makeToken('INDENT', '', { line, col });
                    }
                }
                else {
                    while (true) {
                        const c = indentation[indentation.length - 1];
                        if (c === initialWhitespace) {
                            break;
                        }
                        indentation.pop();
                        _makeToken('DEDENT', '', { line, col });
                        if (!c.startsWith(initialWhitespace)) {
                            indentation.push(initialWhitespace);
                            diagnostics.syntaxError('inconsistent indentation', { line, col });
                            break;
                        }
                    }
                }
                col = initialWhitespace.length;
            }
            const lineTokenizer = new LineTokenizer(lineString);
            while (col < lineString.length) {
                const [kind, nextCol, nextDepth, nextMode] = lineTokenizer.getNextToken(mode, col, depth);
                _makeToken(kind, lineString.substring(col, nextCol), { line, col });
                col = nextCol;
                depth = nextDepth;
                mode = nextMode;
            }
            if (mode === 3 /* Mode.QUOTE_STRING */ || mode === 4 /* Mode.DBLQUOTE_STRING */) {
                diagnostics.syntaxError('unexpected end of line in string literal', { line, col });
                mode = 0 /* Mode.NORMAL */;
            }
            if (depth === 0 || !skipWhitespace) {
                _makeToken('NEWLINE', '\n', { line, col });
            }
        }
        const pos = { line: lines.length, col: lines[lines.length - 1].length };
        for (let i = 1; i < indentation.length; ++i) {
            _makeToken('DEDENT', '', pos);
        }
        _makeToken('EOF', '', pos);
        if (mode !== 0 /* Mode.NORMAL */) {
            diagnostics.syntaxError('unexpected end of source', pos);
        }
        diagnostics.throwIfAnyErrors();
        return tokens;
    }
    Tokenizer.tokenize = tokenize;
    class TokenQueue {
        tokens;
        i = 0;
        constructor(tokens) {
            this.tokens = tokens;
        }
        peek() {
            return this.tokens[this.i];
        }
        poll() {
            return this.tokens[this.i++];
        }
        hasNext(...kinds) {
            return kinds.includes(this.peek().kind);
        }
        hasNextS(...strings) {
            return strings.includes(this.peek().s);
        }
        pollIf(kind) {
            return this.hasNext(kind) && (++this.i, true);
        }
        pollIfS(s) {
            return this.hasNextS(s) && (++this.i, true);
        }
        skipLine() {
            while (!this.hasNext('EOF') && this.poll().kind !== 'NEWLINE') { }
        }
    }
    Tokenizer.TokenQueue = TokenQueue;
})(Tokenizer || (Tokenizer = {}));
var Type;
(function (Type) {
    Type.BOOL = { kind: 'bool' };
    Type.FLOAT = { kind: 'float' };
    Type.FRACTION = { kind: 'fraction' };
    Type.GRID = { kind: 'grid' };
    Type.INT = { kind: 'int' };
    Type.STR = { kind: 'str' };
    Type.PRIMITIVES = {
        bool: Type.BOOL,
        float: Type.FLOAT,
        fraction: Type.FRACTION,
        grid: Type.GRID,
        int: Type.INT,
        str: Type.STR,
    };
    Type.ANY_DICT = { kind: 'any_dict' };
    Type.ANY_POSITION = { kind: 'any_position' };
    Type.OBJECT = { kind: 'union', options: [Type.ANY_DICT, Type.ANY_POSITION, Type.GRID] };
    Type.NUMERIC = { kind: 'union', options: [Type.FLOAT, Type.FRACTION, Type.INT] };
    Type.GRID_ATTRS = new Map([
        ['width', Type.INT],
        ['height', Type.INT],
        ['area', Type.INT],
    ]);
    Type.POSITION_ATTRS = new Map([
        ['x', Type.INT],
        ['y', Type.INT],
    ]);
    function toStr(type) {
        switch (type.kind) {
            case 'any_dict':
                return 'dict';
            case 'any_pattern':
                return `pattern.${type.allowUnions ? 'in' : 'out'}[${type.alphabetKey}]`;
            case 'any_position':
                return 'position';
            case 'union':
                return type.options.map(toStr).join(' | ');
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int':
            case 'str':
                return type.kind;
            case 'dict':
                return `{${Array.from(type.entryTypes, ([k, t]) => `${k}: ${toStr(t)}`).join(', ')}}`;
            case 'pattern.in':
            case 'pattern.out':
                return `${type.kind}.${type.width}x${type.height}[${type.alphabetKey}]`;
            case 'position':
                return `position.grid${type.inGrid}`;
        }
    }
    Type.toStr = toStr;
    function equals(t1, t2) {
        switch (t1.kind) {
            case 'bool':
            case 'float':
            case 'fraction':
            case 'grid':
            case 'int':
            case 'str':
                return t1.kind === t2.kind;
            case 'dict':
                return t2.kind === 'dict' && t1.entryTypes.size === t2.entryTypes.size && [...t1.entryTypes].every(([k, v1]) => {
                    const v2 = t2.entryTypes.get(k);
                    return v2 !== undefined && equals(v1, v2);
                });
            case 'pattern.in':
            case 'pattern.out':
                return t1.kind === t2.kind
                    && t1.alphabetKey === t2.alphabetKey
                    && t1.width === t2.width && t1.height === t2.height;
            case 'position':
                return t2.kind === 'position' && t1.inGrid === t2.inGrid;
        }
    }
    Type.equals = equals;
    function isSubtype(t1, t2) {
        switch (t2.kind) {
            case 'any_dict':
                return t1.kind === 'dict';
            case 'any_pattern':
                return (t1.kind === 'pattern.out' || (t1.kind === 'pattern.in' && t2.allowUnions))
                    && t1.alphabetKey === t2.alphabetKey;
            case 'any_position':
                return t1.kind === 'position';
            case 'union':
                return t2.options.some(t => isSubtype(t1, t));
            case 'dict':
                return t1.kind === 'dict' && t1.entryTypes.size === t2.entryTypes.size && [...t1.entryTypes].every(([k, v1]) => {
                    const v2 = t2.entryTypes.get(k);
                    return v2 !== undefined && isSubtype(v1, v2);
                });
            case 'pattern.in':
            case 'pattern.out':
                return (t1.kind === 'pattern.in' || t1.kind === 'pattern.out')
                    && t1.alphabetKey === t2.alphabetKey
                    && t1.width === t2.width && t1.height === t2.height
                    && (t1.kind === 'pattern.out' || t2.kind === 'pattern.in');
            default:
                return equals(t1, t2);
        }
    }
    Type.isSubtype = isSubtype;
    function leastUpperBound(t1, t2) {
        return isSubtype(t1, t2) ? t2
            : isSubtype(t2, t1) ? t1
                : undefined;
    }
    Type.leastUpperBound = leastUpperBound;
})(Type || (Type = {}));
var IR;
(function (IR) {
    function makeConstArray(name, from, domainSize) {
        if (from.length === 0) {
            return { decl: [], get: () => IR.NULL };
        }
        // check if table is constant
        const start = from[0];
        if (from.every(x => x === start)) {
            return { decl: [], get: () => IR.int(start) };
        }
        // check if table is an arithmetic progression
        // only consider domain sizes where loose int arithmetic is safe
        const step = from[1] - start;
        if (domainSize < 2 ** 31 && from.every((x, i) => x === start + i * step)) {
            return step >= 0
                ? { decl: [], get: index => IR.OP.add(IR.int(start), IR.OP.multConstant(index, step)) }
                : { decl: [], get: index => IR.OP.minus(IR.int(start), IR.OP.multConstant(index, -step)) };
        }
        return {
            decl: [IR.constArrayDecl(name, from, domainSize)],
            get: index => IR.access(name, index),
        };
    }
    IR.makeConstArray = makeConstArray;
    function makeConstArray2D(name, from, rowLength, domainSize) {
        if (from.every((x, i) => x === from[i % rowLength])) {
            const row = from.slice(0, rowLength);
            const table = makeConstArray(name, row, domainSize);
            return { decl: [], get: (j, i) => table.get(i) };
        }
        else if (from.every((x, i) => x === from[i - i % rowLength])) {
            const col = makeArray((from.length / rowLength) | 0, i => from[i * rowLength]);
            return makeConstArray(name, col, domainSize);
        }
        return {
            decl: [IR.constArrayDecl(name, from, domainSize, rowLength)],
            get: (j, i) => IR.access(name, IR.OP.multAddConstant(j, rowLength, i)),
        };
    }
    IR.makeConstArray2D = makeConstArray2D;
    function makeMutableArray(name, length, domainSize) {
        if (domainSize <= 1 || length === IR.ZERO) {
            return { name, decl: [], get: () => IR.ZERO, set: () => IR.PASS };
        }
        const arr = {
            name,
            decl: [IR.newArrayDecl(name, length, domainSize)],
            get(index) {
                return IR.access(name, index);
            },
            set(index, op, value) {
                return IR.assign(this.get(index), op, value);
            },
        };
        return arr;
    }
    IR.makeMutableArray = makeMutableArray;
    function makeMutableArray2D(name, numRows, rowLength, domainSize) {
        if (domainSize <= 1 || rowLength === IR.ZERO || numRows === IR.ZERO) {
            return { name, decl: [], get: () => IR.ZERO, set: () => IR.PASS };
        }
        const arr = {
            name,
            decl: [IR.newArrayDecl(name, IR.OP.mult(numRows, rowLength), domainSize)],
            get: (j, i) => {
                return IR.access(name, IR.OP.add(IR.OP.mult(j, rowLength), i));
            },
            set(j, i, op, value) {
                return IR.assign(this.get(j, i), op, value);
            },
        };
        if (rowLength.kind === 'expr.literal.int') {
            const width = rowLength.value;
            arr.get = (j, i) => IR.access(name, IR.OP.multAddConstant(j, width, i));
        }
        return arr;
    }
    IR.makeMutableArray2D = makeMutableArray2D;
})(IR || (IR = {}));
var IR;
(function (IR) {
    class ConvBuffer {
        g;
        kernel;
        buffer;
        width;
        n;
        values;
        constructor(id, g, charsets, kernel) {
            this.g = g;
            this.kernel = kernel;
            this.width = kernel.width === 1 ? g.width : IR.NAMES.convBufferVar(g, id, 'width');
            const n = this.n = kernel.width === 1 && kernel.height === 1 ? g.n : IR.NAMES.convBufferVar(g, id, 'n');
            // partition the alphabet, so that each alphabet symbol contributes to at most one gBuffer
            const alphabetKey = g.grid.alphabet.key;
            const alphabetPartition = new Partition(alphabetKey.length);
            charsets.forEach(set => alphabetPartition.refine(set));
            const repMap = IDMap.withKey(i => alphabetPartition.getRepresentative(i));
            const mappedBuffers = charsets.map(chars => {
                const out = new Set();
                ISet.forEach(chars, i => out.add(repMap.getOrCreateID(i)));
                return out;
            });
            repMap.forEach((rep, i) => {
                const chars = alphabetPartition.getSet(rep);
                const pattern = new Pattern(1, 1, alphabetKey, [-2], [chars], true);
                g.matcher.addMatchHandler({ kind: 'convolution', buffer: this, pattern, i });
            });
            const buffer = this.buffer = IR.makeMutableArray2D(IR.NAMES.convBufferVar(g, id, 'buffer'), IR.int(repMap.size()), n, kernel.width * kernel.height);
            const values = this.values = new Map();
            charsets.forEach((chars, i) => {
                const exprs = Array.from(mappedBuffers[i], j => buffer.get(IR.int(j), IR.NAMES.AT_CONV));
                // sanity check
                if (exprs.length === 0) {
                    fail();
                }
                values.set(ISet.key(chars), exprs.reduce(IR.OP.add));
            });
        }
        declare() {
            const { g, kernel, n, width, buffer } = this;
            const out = [];
            if (width !== g.width) {
                const w = IR.OP.addConstant(g.width, kernel.width - 1);
                out.push({ name: width, type: IR.INT_TYPE, initialiser: w });
            }
            const h = IR.OP.addConstant(g.height, kernel.height - 1);
            out.push({ name: n, type: IR.INT_TYPE, initialiser: IR.OP.mult(width, h) }, ...buffer.decl);
            return out;
        }
        get(chars) {
            return this.values.get(ISet.key(chars)) ?? fail();
        }
        update(i, xVar, yVar, op) {
            const { buffer, width, kernel } = this;
            const out = [];
            for (let dy = 0; dy < kernel.height; ++dy) {
                for (let dx = 0; dx < kernel.width; ++dx) {
                    const delta = kernel.data[dx + kernel.width * dy];
                    if (delta !== 0) {
                        const index = IR.OP.add(IR.OP.addConstant(xVar, dx), IR.OP.mult(IR.OP.addConstant(yVar, dy), width));
                        out.push(buffer.set(IR.int(i), index, op, IR.int(delta)));
                    }
                }
            }
            return IR.block(out);
        }
    }
    IR.ConvBuffer = ConvBuffer;
})(IR || (IR = {}));
var IR;
(function (IR) {
    class Flags {
        numFlags;
        vars;
        constructor(numFlags) {
            this.numFlags = numFlags;
            const numVars = (numFlags + 31) >> 5;
            this.vars = makeArray(numVars, IR.NAMES.flag);
        }
        _var(i) {
            return this.vars[i >> 5];
        }
        _bit(i) {
            return IR.OP.lshift(IR.ONE, IR.int(i & 31));
        }
        declare() {
            const initialiser = this.numFlags === 1 ? IR.FALSE : IR.ZERO;
            return IR.declVars(this.vars.map(v => ({
                name: v,
                type: IR.INT_TYPE,
                initialiser,
            })), true);
        }
        set(i) {
            const v = this._var(i);
            return this.numFlags === 1
                ? IR.assign(v, '=', IR.TRUE)
                : IR.assign(v, '|=', this._bit(i));
        }
        clear(i) {
            const v = this._var(i);
            return this.numFlags === 1
                ? IR.assign(v, '=', IR.FALSE)
                : IR.assign(v, '&=', IR.OP.bitwiseNot(this._bit(i)));
        }
        check(i) {
            const v = this._var(i);
            return this.numFlags === 1
                ? v
                : IR.OP.ne(IR.OP.bitwiseAnd(this._var(i), this._bit(i)), IR.ZERO);
        }
    }
    IR.Flags = Flags;
})(IR || (IR = {}));
///<reference path="names.ts"/>
var IR;
(function (IR) {
    const { WIDTH, HEIGHT, AT, AT_X, AT_Y, } = IR.NAMES;
    class Grid {
        grid;
        width;
        height;
        n;
        data;
        obj = undefined;
        buffer = undefined;
        origin = undefined;
        lfsrFeedbackTerm = undefined;
        originX;
        originY;
        counters = new Map();
        samplers = new Map();
        convBuffers = new Map();
        matcher;
        scale = 1;
        constructor(grid) {
            this.grid = grid;
            const { scaleX, scaleY } = grid;
            this.width = scaleX === 1 ? WIDTH : IR.NAMES.gridVar(this, 'width');
            this.height = scaleY === 1 ? HEIGHT : IR.NAMES.gridVar(this, 'height');
            this.n = IR.NAMES.gridVar(this, 'n');
            this.data = IR.makeMutableArray(IR.NAMES.gridVar(this, 'data'), this.n, IR.GRID_DATA_ARRAY_TYPE.domainSize);
            this.originX = scaleX % 2 === 0 ? IR.OP.multConstant(WIDTH, scaleX >> 1) : IR.OP.divConstant(this.width, 2);
            this.originY = scaleY % 2 === 0 ? IR.OP.multConstant(HEIGHT, scaleY >> 1) : IR.OP.divConstant(this.height, 2);
            // TODO: multiple matchers per grid?
            this.matcher = new IR.Matcher(this, 0);
        }
        getScale() {
            return this.grid.scaleX * this.grid.scaleY * this.scale;
        }
        makeCounter(patterns) {
            const { counters, samplers, matcher } = this;
            const key = patterns.map(PatternTree.key).join('\n');
            // TODO: this is order-dependent, a matching sampler might be declared later
            const sampler = samplers.get(key);
            if (sampler !== undefined) {
                return sampler.count;
            }
            return getOrCompute(counters, key, () => {
                const counter = IR.NAMES.counter(this, counters.size);
                for (const pattern of patterns) {
                    matcher.addMatchHandler({ kind: 'counter', pattern, counter });
                }
                return counter;
            });
        }
        makeSampler(patterns) {
            const { samplers, matcher } = this;
            const key = patterns.map(PatternTree.key).join('\n');
            return getOrCompute(samplers, key, () => {
                if (patterns.length === 1 && patterns[0].kind === 'top') {
                    const { width, height } = patterns[0];
                    return new IR.TrivialSampler(this, width, height);
                }
                const sampler = new IR.Sampler(samplers.size, this, patterns.length);
                for (let i = 0; i < patterns.length; ++i) {
                    const pattern = patterns[i];
                    matcher.addMatchHandler({ kind: 'sampler', pattern, sampler, i });
                }
                this.scale = Math.max(this.scale, patterns.length);
                return sampler;
            });
        }
        makeConvBuffer(p) {
            const { convBuffers } = this;
            const key = Convolution.Kernel.key(p.kernel);
            return getOrCompute(convBuffers, key, () => {
                const charsets = [];
                this.grid.convPatterns.forEach(q => {
                    if (p.kernel.equals(q.kernel)) {
                        charsets.push(q.chars);
                    }
                });
                this.scale = Math.max(this.scale, charsets.length);
                return new IR.ConvBuffer(convBuffers.size, this, charsets, p.kernel);
            });
        }
        useOrigin() {
            return this.origin ??= IR.NAMES.gridVar(this, 'origin');
        }
        useObj() {
            return this.obj ??= IR.NAMES.gridVar(this, 'obj');
        }
        useBuffer() {
            return this.buffer ??= IR.makeMutableArray(IR.NAMES.gridVar(this, 'buffer'), this.n, IR.GRID_DATA_ARRAY_TYPE.domainSize);
        }
        useLsfrFeedbackTerm() {
            return this.lfsrFeedbackTerm ??= IR.NAMES.gridVar(this, 'lfsrFeedbackTerm');
        }
        declareVars() {
            const { width, height, n, data, obj, buffer, origin, lfsrFeedbackTerm, grid } = this;
            const alphabetKey = grid.alphabet.key;
            const consts = [];
            const vars = [];
            if (width !== WIDTH) {
                consts.push({ name: width, type: IR.INT_TYPE, initialiser: IR.OP.multConstant(WIDTH, grid.scaleX) });
            }
            if (height !== HEIGHT) {
                consts.push({ name: height, type: IR.INT_TYPE, initialiser: IR.OP.multConstant(HEIGHT, grid.scaleY) });
            }
            consts.push({ name: n, type: IR.INT_TYPE, initialiser: IR.OP.mult(width, height) }, ...data.decl);
            if (obj !== undefined) {
                const initialiser = IR.libConstructorCall('Grid', [width, height, data.name, IR.str(alphabetKey)]);
                consts.push({ name: obj, type: IR.GRID_TYPE, initialiser });
            }
            if (buffer !== undefined) {
                consts.push(...buffer.decl);
            }
            if (origin !== undefined) {
                consts.push({ name: origin, type: IR.INT_TYPE, initialiser: IR.OP.add(this.originX, IR.OP.mult(this.originY, width)) });
            }
            if (lfsrFeedbackTerm !== undefined) {
                consts.push({ name: lfsrFeedbackTerm, type: IR.INT_TYPE, initialiser: IR.libFunctionCall('lfsrFeedbackTerm', [n]) });
            }
            for (const buffer of this.convBuffers.values()) {
                consts.push(...buffer.declare());
            }
            vars.push(...Array.from(this.counters.values(), counter => ({
                name: counter,
                type: IR.INT_TYPE,
                initialiser: IR.ZERO,
            })));
            return [
                IR.declVars(consts),
                IR.declVars(vars, true),
                ...Array.from(this.samplers.values(), sampler => sampler.declare()),
            ];
        }
        attr(attr) {
            switch (attr) {
                case 'area': return this.n;
                case 'width': return this.width;
                case 'height': return this.height;
            }
        }
        /**
         * Used internally for indices which are known to be in-bounds.
         * `x` and `y` must be non-negative.
         */
        index(x, y) {
            return IR.OP.add(x, IR.OP.mult(y, this.width));
        }
        /**
         * Used internally for relative indices which are known to be in-bounds.
         * The variables `AT`, `AT_X` and `AT_Y` must be declared in the IR,
         * and `dx` and `dy` must be non-negative.
         */
        relativeIndex(dx, dy) {
            if (this.grid.periodic) {
                const x = IR.OP.mod(IR.OP.addConstant(AT_X, dx), this.width);
                const y = IR.OP.mod(IR.OP.addConstant(AT_Y, dy), this.height);
                return IR.OP.add(x, IR.OP.mult(y, this.width));
            }
            else {
                return IR.OP.add(IR.OP.addConstant(AT, dx), IR.OP.multConstant(this.width, dy));
            }
        }
        checkedIndex(x, y) {
            return IR.libMethodCall('Grid', this.grid.periodic ? 'wrapIndex' : 'index', this.useObj(), [x, y]);
        }
        declareAtIndex(index) {
            const decls = [];
            if (index !== AT) {
                decls.push({ name: AT, type: IR.INT_TYPE, initialiser: index });
            }
            decls.push({ name: AT_X, type: IR.INT_TYPE, initialiser: IR.OP.mod(AT, this.width) }, { name: AT_Y, type: IR.INT_TYPE, initialiser: IR.OP.floordiv(AT, this.width) });
            return IR.declVars(decls);
        }
        declareAtXY(x, y) {
            const decls = [];
            if (x !== AT_X) {
                decls.push({ name: AT_X, type: IR.INT_TYPE, initialiser: x });
            }
            if (y !== AT_Y) {
                decls.push({ name: AT_Y, type: IR.INT_TYPE, initialiser: y });
            }
            decls.push({ name: AT, type: IR.INT_TYPE, initialiser: this.index(AT_X, AT_Y) });
            return IR.declVars(decls);
        }
        write(index, colour, mask) {
            return mask !== undefined ? mask.set(this, index, colour)
                : this.data.set(index, '=', IR.int(colour));
        }
        update(x, y, w, h, doYield) {
            return [
                this.matcher.update(x, y, w, h),
                doYield ? this.yieldRewriteInfo(x, y, w, h) : IR.PASS,
            ];
        }
        yield_() {
            return this.yieldRewriteInfo(IR.ZERO, IR.ZERO, this.width, this.height);
        }
        yieldRewriteInfo(x, y, w, h) {
            return IR.yield_(IR.libConstructorCall('RewriteInfo', [this.useObj(), x, y, w, h]));
        }
    }
    IR.Grid = Grid;
})(IR || (IR = {}));
var IR;
(function (IR) {
    class Limits {
        limits;
        vars;
        checks;
        decrements;
        constructor(limits) {
            this.limits = limits;
            const vars = this.vars = makeArray(limits.length, IR.NAMES.limit);
            this.checks = vars.map(v => IR.OP.gt(v, IR.ZERO));
            this.decrements = vars.map(v => IR.assign(v, '-=', IR.ONE));
        }
        declare(c) {
            const { vars } = this;
            return IR.declVars(this.limits.map((limit, i) => ({
                name: vars[i],
                type: IR.INT_TYPE,
                initialiser: limit.canReset ? undefined : c.expr(limit.initialiser),
            })), true);
        }
        reset(limitID, c) {
            const limit = this.limits[limitID];
            return limit.canReset
                ? IR.assign(this.vars[limitID], '=', c.expr(limit.initialiser))
                : fail();
        }
        check(limitID) {
            return this.checks[limitID];
        }
        decrement(limitID) {
            return this.decrements[limitID];
        }
    }
    IR.Limits = Limits;
})(IR || (IR = {}));
///<reference path="names.ts"/>
var IR;
(function (IR) {
    const { MASK, MASK_CLEAR, MASK_HASNT, MASK_SET, WIDTH, HEIGHT, AT_X, AT_Y, G, I, N, P, S, } = IR.NAMES;
    class Mask {
        scale = 0;
        name = MASK;
        maskN(length) {
            return IR.OP.divConstant(IR.OP.addConstant(length, 31), 32);
        }
        use(g) {
            this.scale = Math.max(this.scale, g.grid.scaleX * g.grid.scaleY);
        }
        declare() {
            if (this.scale === 0) {
                return [];
            }
            const { name } = this;
            const arr = IR.makeMutableArray(name, this.maskN(IR.OP.multConstant(IR.OP.mult(WIDTH, HEIGHT), this.scale)), IR.INT32_ARRAY_TYPE.domainSize);
            const index = IR.OP.divConstant(I, 32);
            const bit = IR.OP.lshift(IR.ONE, IR.OP.modConstant(I, 32));
            return [
                IR.declVars(arr.decl),
                IR.declFunc(MASK_CLEAR, undefined, [N], [IR.INT_TYPE], IR.VOID_TYPE, IR.block([
                    IR.assign(N, '=', this.maskN(N)),
                    IR.forRange(I, IR.ZERO, N, [arr.set(I, '=', IR.ZERO)]),
                ])),
                IR.declFunc(MASK_SET, undefined, [G, I, S], [IR.GRID_DATA_ARRAY_TYPE, IR.INT_TYPE, IR.BYTE_TYPE], IR.VOID_TYPE, IR.block([
                    IR.assign(IR.access(G, I), '=', S),
                    arr.set(index, '|=', bit),
                ])),
                IR.declFunc(MASK_HASNT, undefined, [I], [IR.INT_TYPE], IR.BOOL_TYPE, IR.return_(IR.OP.eq(IR.OP.bitwiseAnd(arr.get(index), bit), IR.ZERO))),
                IR.BLANK_LINE,
            ];
        }
        clear(g) {
            return IR.localCallStmt(MASK_CLEAR, [g.n]);
        }
        set(g, index, colour) {
            return IR.localCallStmt(MASK_SET, [g.data.name, index, IR.int(colour)]);
        }
        hasnt(index) {
            return IR.localCall(MASK_HASNT, [index]);
        }
        patternFits(g, patternExpr) {
            return patternExpr.kind === 'expr.constant'
                ? patternExpr.constant.value.map((dx, dy) => this.hasnt(g.relativeIndex(dx, dy))).reduce(IR.OP.and)
                : IR.libMethodCall('Pattern', 'fitsMask', P, [g.useObj(), this.name, AT_X, AT_Y]);
        }
    }
    IR.Mask = Mask;
})(IR || (IR = {}));
///<reference path="names.ts"/>
var IR;
(function (IR) {
    const { START_X, START_Y, END_X, END_Y, EFFECTIVE_WIDTH, EFFECTIVE_HEIGHT, AT, AT_X, AT_Y, S, OLD_S, T, OLD_T, U, } = IR.NAMES;
    class Matcher {
        g;
        id;
        matchHandlers = [];
        updateFuncName;
        constructor(g, id) {
            this.g = g;
            this.id = id;
            this.updateFuncName = IR.NAMES.matcherVar(g, id, 'update');
        }
        addMatchHandler(handler) {
            this.matchHandlers.push(handler);
        }
        makeMatchHandler(h, f) {
            switch (h.kind) {
                case 'sampler':
                    return h.sampler.handleMatch(f, h.i);
                case 'counter':
                    return IR.assign(h.counter, f === 'add' ? '+=' : '-=', IR.ONE);
                case 'convolution':
                    return h.buffer.update(h.i, AT_X, AT_Y, f === 'add' ? '+=' : '-=');
            }
        }
        update(x, y, w, h) {
            return IR.localCallStmt(this.updateFuncName, [x, y, w, h]);
        }
        declareUpdateFunc() {
            const { g, id } = this;
            const gridAlphabetSize = g.grid.alphabet.key.length;
            const dfas = makePatternMatcherDFAs(gridAlphabetSize, this.matchHandlers.map(h => h.pattern));
            const colsAlphabetSize = dfas.colDFA.alphabetSize, rowDFASize = dfas.rowDFA.size(), colDFASize = dfas.colDFA.size(), numRowPatterns = dfas.rowsAcceptMap.size(), numColPatterns = dfas.colsAcceptMap.size(), rowAcceptSetMasks = dfas.rowsAcceptSetMap.flatMap(entry => [...dfas.rowsAcceptMap.getIDSet(entry)]), colAcceptSetMasks = dfas.colsAcceptSetMap.flatMap(entry => [...dfas.colsAcceptMap.getIDSet(entry)]);
            const rowDFA = IR.makeConstArray2D(IR.NAMES.matcherVar(g, id, 'rowDFA'), dfas.rowDFA.toFlatArray(), gridAlphabetSize, rowDFASize);
            const rowAcceptSetIDs = IR.makeConstArray(IR.NAMES.matcherVar(g, id, 'rowAcceptSetIDs'), dfas.rowsAcceptSetIDs, dfas.rowsAcceptSetMap.size());
            const rowAcceptSets = IR.makeConstArray2D(IR.NAMES.matcherVar(g, id, 'rowAcceptSets'), rowAcceptSetMasks, (numRowPatterns + 31) >> 5, numRowPatterns <= 16 ? 1 << numRowPatterns : IR.INT32_ARRAY_TYPE.domainSize);
            const rowsToCols = IR.makeConstArray(IR.NAMES.matcherVar(g, id, 'rowsToCols'), dfas.rowsToCols, colsAlphabetSize);
            const colDFA = IR.makeConstArray2D(IR.NAMES.matcherVar(g, id, 'colDFA'), dfas.colDFA.toFlatArray(), colsAlphabetSize, colDFASize);
            const colAcceptSetIDs = IR.makeConstArray(IR.NAMES.matcherVar(g, id, 'colAcceptSetIDs'), dfas.colsAcceptSetIDs, dfas.colsAcceptSetMap.size());
            const colAcceptSets = IR.makeConstArray2D(IR.NAMES.matcherVar(g, id, 'colAcceptSets'), colAcceptSetMasks, (numColPatterns + 31) >> 5, numColPatterns <= 16 ? 1 << numColPatterns : IR.INT32_ARRAY_TYPE.domainSize);
            const rowStates = IR.makeMutableArray(IR.NAMES.matcherVar(g, id, 'rowStates'), g.n, rowDFASize);
            const colStates = IR.makeMutableArray(IR.NAMES.matcherVar(g, id, 'colStates'), g.n, colDFASize);
            const handlersByPattern = new Map();
            for (const handler of this.matchHandlers) {
                const key = PatternTree.key(handler.pattern);
                getOrCompute(handlersByPattern, key, () => []).push(handler);
            }
            const maskDiff = (acceptSets, t1, t2, index) => {
                const indexExpr = IR.int(index);
                return IR.OP.bitwiseAnd(acceptSets.get(t1, indexExpr), IR.OP.bitwiseNot(acceptSets.get(t2, indexExpr)));
            };
            const makeStateChangeHandlers = (t1, t2, acceptSets, acceptMap, f) => {
                const maskSize = (acceptMap.size() + 31) >> 5;
                const out = [];
                for (let index = 0; index < maskSize; ++index) {
                    const cases = [];
                    const minAcceptID = index << 5, maxAcceptID = Math.min(minAcceptID + 32, acceptMap.size());
                    for (let acceptID = minAcceptID; acceptID < maxAcceptID; ++acceptID) {
                        const key = PatternTree.key(acceptMap.getByID(acceptID));
                        const handlers = handlersByPattern.get(key) ?? fail();
                        cases.push(IR.block(handlers.map(h => this.makeMatchHandler(h, f))));
                    }
                    out.push(IR.assign(U, '=', maskDiff(acceptSets, t1, t2, index)), 
                    // special cases for short switches, when there is no need for `countTrailingZeros`
                    cases.length === 1 ? IR.if_(IR.OP.ne(U, IR.ZERO), cases[0])
                        : cases.length === 2 ? IR.block(cases.map((c, i) => IR.if_(IR.OP.ne(IR.OP.bitwiseAnd(U, IR.int(1 << i)), IR.ZERO), c)))
                            : IR.while_(IR.OP.ne(U, IR.ZERO), IR.block([
                                IR.switch_(IR.OP.countTrailingZeros(U), cases),
                                IR.assign(U, '&=', IR.OP.minusOne(U)),
                            ])));
                }
                return out;
            };
            const makeUpdateSamplers = (acceptSetIDs, acceptSets, acceptMap) => {
                let t = acceptSetIDs.get(S), oldT = acceptSetIDs.get(OLD_S);
                if (t.kind === 'expr.literal.int' && oldT.kind === 'expr.literal.int') {
                    return t.value === oldT.value ? [] : fail();
                }
                const out = [];
                if (t !== S || oldT !== OLD_S) {
                    out.push(IR.declVars([
                        { name: T, type: IR.INT_TYPE, initialiser: acceptSetIDs.get(S) },
                        { name: OLD_T, type: IR.INT_TYPE, initialiser: acceptSetIDs.get(OLD_S) },
                    ]), IR.if_(IR.OP.eq(T, OLD_T), IR.CONTINUE));
                    t = T;
                    oldT = OLD_T;
                }
                out.push(IR.declVar(U, IR.INT_TYPE, undefined, true), ...makeStateChangeHandlers(oldT, t, acceptSets, acceptMap, 'del'), ...makeStateChangeHandlers(t, oldT, acceptSets, acceptMap, 'add'));
                return out;
            };
            const recomputeRowStates = [
                IR.BLANK_LINE,
                IR.comment('recompute row states'),
                IR.forRange(AT_Y, START_Y, END_Y, [
                    IR.declVar(S, IR.INT_TYPE, IR.ternary(IR.OP.lt(END_X, g.width), rowStates.get(g.index(END_X, AT_Y)), IR.ZERO), true),
                    IR.forRangeReverse(AT_X, IR.ZERO, END_X, [
                        g.declareAtXY(AT_X, AT_Y),
                        IR.declVar(OLD_S, IR.INT_TYPE, rowStates.get(AT)),
                        IR.assign(S, '=', rowDFA.get(S, g.data.get(AT))),
                        IR.if_(IR.OP.eq(S, OLD_S), IR.if_(IR.OP.lt(AT_X, START_X), IR.BREAK, IR.CONTINUE)),
                        rowStates.set(AT, '=', S),
                        numColPatterns === 0 ? IR.PASS : IR.if_(IR.OP.lt(AT_X, START_X), IR.assign(START_X, '=', AT_X)),
                        // must occur last, since it uses `continue`
                        ...makeUpdateSamplers(rowAcceptSetIDs, rowAcceptSets, dfas.rowsAcceptMap),
                    ]),
                ]),
            ];
            const recomputeColStates = numColPatterns === 0 ? [] : [
                IR.BLANK_LINE,
                IR.comment('recompute col states'),
                IR.forRange(AT_X, START_X, END_X, [
                    IR.declVar(S, IR.INT_TYPE, IR.ternary(IR.OP.lt(END_Y, g.height), colStates.get(g.index(AT_X, END_Y)), IR.ZERO), true),
                    IR.forRangeReverse(AT_Y, IR.ZERO, END_Y, [
                        g.declareAtXY(AT_X, AT_Y),
                        IR.declVar(OLD_S, IR.INT_TYPE, colStates.get(AT)),
                        IR.assign(S, '=', colDFA.get(S, rowsToCols.get(rowStates.get(AT)))),
                        IR.if_(IR.OP.eq(S, OLD_S), IR.if_(IR.OP.lt(AT_Y, START_Y), IR.BREAK, IR.CONTINUE)),
                        colStates.set(AT, '=', S),
                        // must occur last, since it uses `continue`
                        ...makeUpdateSamplers(colAcceptSetIDs, colAcceptSets, dfas.colsAcceptMap),
                    ]),
                ]),
            ];
            return [
                IR.declVars([
                    rowDFA,
                    rowAcceptSetIDs,
                    rowAcceptSets,
                    rowsToCols,
                    colDFA,
                    colAcceptSetIDs,
                    colAcceptSets,
                    rowStates,
                    colStates
                ].flatMap(arr => arr.decl)),
                IR.declFunc(this.updateFuncName, undefined, [START_X, START_Y, EFFECTIVE_WIDTH, EFFECTIVE_HEIGHT], [IR.INT_TYPE, IR.INT_TYPE, IR.INT_TYPE, IR.INT_TYPE], IR.VOID_TYPE, numRowPatterns === 0 && numColPatterns === 0 ? IR.PASS : IR.block([
                    IR.declVars([
                        { name: END_X, type: IR.INT_TYPE, initialiser: IR.OP.add(START_X, EFFECTIVE_WIDTH) },
                        { name: END_Y, type: IR.INT_TYPE, initialiser: IR.OP.add(START_Y, EFFECTIVE_HEIGHT) },
                    ]),
                    ...recomputeRowStates,
                    ...recomputeColStates,
                ])),
                this.update(IR.ZERO, IR.ZERO, g.width, g.height),
                IR.BLANK_LINE,
            ];
        }
    }
    IR.Matcher = Matcher;
    /**
     * Builds a pair of DFAs which can be used to match 2D patterns. The `rowDFA`
     * recognises pattern rows, and the `colDFA` recognises sequences of pattern
     * rows matched by the `rowDFA`.
     *
     * The DFAs recognise the patterns in reverse order, for convenience so that
     * matches are reported where the patterns start rather than where they end.
     */
    function makePatternMatcherDFAs(alphabetSize, allPatterns) {
        const rowPatterns = allPatterns.filter(p => p.height === 1);
        const colPatterns = allPatterns.filter(p => p.height > 1);
        const rowsAcceptMap = IDMap.ofWithKey(rowPatterns, PatternTree.key);
        const rowsKeepMap = IDMap.withKey(Pattern.key);
        const rowsAcceptOrKeepMap = IDMap.ofWithKey(rowPatterns, PatternTree.key);
        const colsAcceptMap = IDMap.ofWithKey(colPatterns, PatternTree.key);
        const allLeaves = IDMap.ofWithKey(allPatterns.flatMap(PatternTree.getLeaves), PatternTree.key);
        const allLeafRows = IDMap.withKey(Pattern.key);
        const leafRowMap = [];
        allLeaves.forEach(pattern => {
            const rows = Pattern.rowsOf(pattern);
            // determine which rows to keep when mapping from rowDFA to colDFA
            if (rows.length > 1) {
                rowsAcceptOrKeepMap.addAll(rows);
                rowsKeepMap.addAll(rows);
            }
            // associate this pattern with the IDs of its rows in `allLeafRows`
            leafRowMap.push(rows.map(row => allLeafRows.getOrCreateID(row)));
        });
        const rowDFA = Regex.compile(alphabetSize, _makeRegex(allLeafRows.map(pattern => ({
            pattern,
            seq: pattern.kind === 'top'
                ? emptyArray(pattern.width, Regex.WILDCARD)
                : pattern.masks.map(ISet.toArray).map(Regex.letters),
        }))))
            .map(literalRows => {
            const acceptSet = new Set(literalRows.map(Pattern.key));
            return rowsAcceptOrKeepMap.filter(pattern => PatternTree.matches(pattern, p => acceptSet.has(Pattern.key(p))));
        })
            .minimise(PatternTree.key);
        const [rowsAcceptSetIDs, rowsAcceptSetMap] = rowDFA.getAcceptSetMap(rowsAcceptOrKeepMap, row => rowsAcceptMap.has(row));
        // reduce alphabet size of colDFA by not distinguishing rows which aren't part of taller patterns
        const [rowsToCols, rowsToColsMap] = rowDFA.getAcceptSetMap(rowsAcceptOrKeepMap, (row) => rowsKeepMap.has(row));
        const acceptingSetIDs = makeArray(allLeafRows.size(), () => []);
        rowsToColsMap.forEach((rowSet, rowSetID) => {
            for (const row of rowSet) {
                const rowID = allLeafRows.getID(row);
                acceptingSetIDs[rowID].push(rowSetID);
            }
        });
        const colRegexLetters = acceptingSetIDs.map(Regex.letters);
        const colRegexPatterns = [];
        allLeaves.forEach((pattern, i) => {
            // patterns with only one row will be matched by the rowDFA directly
            if (pattern.height === 1) {
                return;
            }
            colRegexPatterns.push({
                pattern,
                seq: leafRowMap[i].map(rowID => colRegexLetters[rowID]),
            });
        });
        const colDFA = Regex.compile(rowsToColsMap.size(), _makeRegex(colRegexPatterns))
            .map(literals => {
            const acceptSet = new Set(literals.map(Pattern.key));
            return allPatterns.filter(pattern => PatternTree.matches(pattern, p => acceptSet.has(Pattern.key(p))));
        })
            .minimise(PatternTree.key);
        const [colsAcceptSetIDs, colsAcceptSetMap] = colDFA.getAcceptSetMap(colsAcceptMap);
        return {
            rowDFA,
            rowsAcceptMap,
            rowsAcceptSetIDs,
            rowsAcceptSetMap,
            rowsToCols,
            colDFA,
            colsAcceptMap,
            colsAcceptSetIDs,
            colsAcceptSetMap,
        };
    }
    function _makeRegex(patterns) {
        return Regex.concat([
            Regex.DOT_STAR,
            Regex.union(patterns.map(p => Regex.concat([
                Regex.accept(p.pattern),
                ...p.seq,
            ].reverse()))),
        ]);
    }
})(IR || (IR = {}));
///<reference path="names.ts"/>
var IR;
(function (IR) {
    const { MATCHES, MATCH_COUNT, WIDTH, HEIGHT, RNG, J, } = IR.NAMES;
    class MatchesArray {
        scale = 0;
        array = MATCHES;
        count = MATCH_COUNT;
        getAtCount = this.get(this.count);
        isNotEmpty = IR.OP.gt(this.count, IR.ZERO);
        incrementCount = IR.assign(this.count, '+=', IR.ONE);
        decrementCount = IR.assign(this.count, '-=', IR.ONE);
        use(g, k) {
            // TODO: in principle, we can get a better estimate for the maximum number of matches if we know some patterns cannot overlap
            this.scale = Math.max(this.scale, g.grid.scaleX * g.grid.scaleY * k);
        }
        declare() {
            if (this.scale === 0) {
                return IR.PASS;
            }
            const n = IR.OP.multConstant(IR.OP.mult(WIDTH, HEIGHT), this.scale);
            return IR.declVar(this.array, IR.INT32_ARRAY_TYPE, IR.newInt32Array(n));
        }
        declareCount(initial, mutable) {
            return IR.declVar(this.count, IR.INT_TYPE, initial, mutable);
        }
        get(index) {
            return IR.access(this.array, index);
        }
        add(match, shuffle) {
            return shuffle ? [
                IR.declVar(J, IR.INT_TYPE, IR.libMethodCall('PRNG', 'nextInt', RNG, [IR.OP.addOne(this.count)])),
                IR.assign(this.getAtCount, '=', this.get(J)),
                IR.assign(this.get(J), '=', match),
                this.incrementCount,
            ] : [
                IR.assign(this.getAtCount, '=', match),
                this.incrementCount,
            ];
        }
    }
    IR.MatchesArray = MatchesArray;
})(IR || (IR = {}));
///<reference path="names.ts"/>
var IR;
(function (IR) {
    const { RNG, I, S, MATCH, AT, AT_X, AT_Y } = IR.NAMES;
    class Sampler {
        g;
        numPatterns;
        name;
        count;
        arr;
        capacity;
        declareMatchAt;
        matchPatternIndex;
        constructor(id, g, numPatterns) {
            this.g = g;
            this.numPatterns = numPatterns;
            this.name = IR.NAMES.sampler(g, id);
            this.count = IR.attr(this.name, 'count');
            this.arr = IR.attr(this.name, 'arr');
            this.capacity = IR.OP.multConstant(g.n, numPatterns);
            this.declareMatchAt = this.g.declareAtIndex(IR.OP.divConstant(MATCH, this.numPatterns));
            this.matchPatternIndex = IR.OP.modConstant(MATCH, this.numPatterns);
        }
        declare() {
            return IR.declVar(this.name, IR.SAMPLER_TYPE, IR.libConstructorCall('Sampler', [this.capacity]));
        }
        handleMatch(f, patternIndex) {
            const match = IR.OP.multAddConstant(AT, this.numPatterns, IR.int(patternIndex));
            return IR.libMethodCallStmt('Sampler', f, this.name, [match]);
        }
        sampleWithReplacement(cases) {
            return IR.block([
                IR.declVar(MATCH, IR.INT_TYPE, IR.access(this.arr, IR.libMethodCall('PRNG', 'nextInt', RNG, [this.count]))),
                this.declareMatchAt,
                IR.switch_(this.matchPatternIndex, cases),
            ]);
        }
        beginSamplingWithoutReplacement() {
            return IR.PASS;
        }
        sampleWithoutReplacement(cases, count) {
            return IR.block([
                IR.declVar(MATCH, IR.INT_TYPE, IR.libMethodCall('Sampler', 'sample', this.name, [count, RNG])),
                this.declareMatchAt,
                IR.switch_(this.matchPatternIndex, cases),
            ]);
        }
        copyInto(matches, shuffle) {
            return [
                shuffle ? IR.libMethodCallStmt('Sampler', 'shuffleInto', this.name, [matches.array, RNG])
                    : IR.libMethodCallStmt('Sampler', 'copyInto', this.name, [matches.array]),
                IR.declVar(matches.count, IR.INT_TYPE, this.count),
            ];
        }
        forEach(then) {
            return this.numPatterns === 1
                ? IR.forRange(I, IR.ZERO, this.count, [
                    this.g.declareAtIndex(IR.access(this.arr, I)),
                    ...then,
                ])
                : fail();
        }
    }
    IR.Sampler = Sampler;
    class TrivialSampler {
        g;
        count;
        width;
        height;
        is1x1;
        constructor(g, patternWidth, patternHeight) {
            this.g = g;
            this.width = IR.OP.minusConstant(g.width, patternWidth - 1);
            this.height = IR.OP.minusConstant(g.height, patternHeight - 1);
            this.is1x1 = patternWidth === 1 && patternHeight === 1;
            this.count = this.is1x1 ? g.n : IR.OP.mult(this.width, this.height);
        }
        declare() {
            return IR.PASS;
        }
        handleMatch(f, patternIndex) {
            fail();
        }
        sampleWithReplacement(cases) {
            return IR.block([
                this.is1x1 ? this.g.declareAtIndex(IR.libMethodCall('PRNG', 'nextInt', RNG, [this.count]))
                    : this.g.declareAtXY(IR.libMethodCall('PRNG', 'nextInt', RNG, [this.width]), IR.libMethodCall('PRNG', 'nextInt', RNG, [this.height])),
                cases.length === 1 ? cases[0] : fail(),
            ]);
        }
        beginSamplingWithoutReplacement() {
            return IR.declVar(S, IR.INT_TYPE, IR.OP.addOne(IR.libMethodCall('PRNG', 'nextInt', RNG, [this.count])), true);
        }
        sampleWithoutReplacement(cases, count) {
            const declAt = this.is1x1
                ? this.g.declareAtIndex(IR.OP.minusOne(S))
                : this.g.declareAtXY(IR.OP.mod(S, this.width), IR.OP.floordiv(IR.OP.minusOne(S), this.width));
            return IR.block([
                IR.while_(IR.TRUE, IR.block([
                    IR.assign(S, '=', IR.ternary(IR.OP.ne(IR.OP.bitwiseAnd(S, IR.ONE), IR.ZERO), IR.OP.bitwiseXor(IR.OP.rshift(S, IR.ONE), this.g.useLsfrFeedbackTerm()), IR.OP.rshift(S, IR.ONE))),
                    IR.if_(IR.OP.le(S, this.count), IR.BREAK),
                ])),
                declAt,
                cases.length === 1 ? cases[0] : fail(),
            ]);
        }
        copyInto(matches, shuffle) {
            return [
                IR.declVar(matches.count, IR.INT_TYPE, IR.ZERO, true),
                this.is1x1 ? IR.forRange(AT, IR.ZERO, this.count, matches.add(AT, shuffle))
                    : IR.forRange(AT_Y, IR.ZERO, this.height, [
                        IR.forRange(AT_X, IR.ZERO, this.width, matches.add(this.g.index(AT_X, AT_Y), shuffle)),
                    ]),
            ];
        }
        forEach(then) {
            return this.is1x1
                ? IR.forRange(AT, IR.ZERO, this.count, [
                    this.g.declareAtIndex(AT),
                    ...then,
                ])
                : IR.forRange(AT_Y, IR.ZERO, this.height, [
                    IR.forRange(AT_X, IR.ZERO, this.width, [
                        this.g.declareAtXY(AT_X, AT_Y),
                        ...then,
                    ])
                ]);
        }
    }
    IR.TrivialSampler = TrivialSampler;
})(IR || (IR = {}));
var IR;
(function (IR) {
    IR.BOOL_TYPE = { kind: 'bool' };
    IR.BYTE_TYPE = { kind: 'byte' };
    IR.FLOAT_TYPE = { kind: 'float' };
    IR.FRACTION_TYPE = { kind: 'fraction' };
    IR.GRID_TYPE = { kind: 'grid' };
    IR.INT_TYPE = { kind: 'int' };
    IR.PATTERN_TYPE = { kind: 'pattern' };
    IR.PRNG_TYPE = { kind: 'prng' };
    IR.REWRITE_INFO_TYPE = { kind: 'rewriteinfo' };
    IR.SAMPLER_TYPE = { kind: 'sampler' };
    IR.STR_TYPE = { kind: 'str' };
    IR.VOID_TYPE = { kind: 'void' };
    IR.GRID_DATA_ARRAY_TYPE = mutableArrayType(128);
    IR.INT32_ARRAY_TYPE = mutableArrayType(2 ** 32);
    function mutableArrayType(domainSize) {
        return { kind: 'array.mutable', domainSize };
    }
    IR.mutableArrayType = mutableArrayType;
    function constArrayType(domainSize) {
        return { kind: 'array.const', domainSize };
    }
    IR.constArrayType = constArrayType;
    function nullableType(componentType) {
        return { kind: 'nullable', componentType };
    }
    IR.nullableType = nullableType;
})(IR || (IR = {}));
var IR;
(function (IR) {
    class Variables {
        variables;
        names;
        constructor(variables) {
            this.variables = variables;
            const counts = new Map();
            this.names = variables.map(v => {
                const count = counts.get(v.name) ?? 0;
                counts.set(v.name, count + 1);
                return IR.NAMES.variable(v.name, count);
            });
        }
        declare(c) {
            const { names } = this;
            // this also filters out compile-time constants, since the resolver folds them instead of referencing them
            return this.variables
                .filter(v => v.references > 0)
                .map(v => IR.declVar(names[v.id], c.type(v.type), v.initialiser && c.expr(v.initialiser), v.initialiser === undefined));
        }
        name(variableID) {
            return this.names[variableID];
        }
        type(variableID, c) {
            return c.type(this.variables[variableID].type);
        }
    }
    IR.Variables = Variables;
})(IR || (IR = {}));
var Convolution;
(function (Convolution) {
    class Kernel {
        width;
        height;
        data;
        static key(kernel) {
            return kernel._key ??= `${kernel.width}x${kernel.height}:${kernel.data.join('')}`;
        }
        _key = undefined;
        centreX;
        centreY;
        constructor(width, height, data) {
            this.width = width;
            this.height = height;
            this.data = data;
            this.centreX = width >> 1;
            this.centreY = height >> 1;
        }
        equals(other) {
            if (this === other) {
                return true;
            }
            return this.width === other.width
                && this.height === other.height
                && this.data.every((x, i) => x === other.data[i]);
        }
    }
    Convolution.Kernel = Kernel;
    Convolution.KERNELS = {
        Moore: new Kernel(3, 3, [1, 1, 1, 1, 0, 1, 1, 1, 1]),
        VonNeumann: new Kernel(3, 3, [0, 1, 0, 1, 0, 1, 0, 1, 0]),
    };
})(Convolution || (Convolution = {}));
/**
 * Data structure representing a partition of the natural numbers from 0 to n - 1,
 * for use in the `DFA.minimise` algorithm. The main operations are `refine` and
 * `pollUnprocessed`.
 *
 * https://en.wikipedia.org/wiki/Partition_refinement#Data_structure
 */
class Partition {
    /**
     * The numbers from 0 to n - 1, ordered so that each subset in the partition
     * is a contiguous range.
     *
     * Invariant: `arr` is a permutation of the numbers from 0 to n - 1
     */
    arr;
    /**
     * Maps the numbers from 0 to n - 1 to their indices in `arr`.
     *
     * Invariant: `arr[i] === x` if and only if `indices[x] === i`
     */
    indices;
    /**
     * The boundaries in `arr` for each subset in the partition.
     *
     * Invariant: `subsets[i].index === i`
     * Invariant: `subsets[i].start < subsets[i].end`
     * Invariant: `subsets[i].start === 0` or there is a unique `j` such that `subsets[i].start === subsets[j].end`
     * Invariant: `subsets[i].end === n` or there is a unique `j` such that `subsets[i].end === subsets[j].start`
     */
    subsets = [];
    /**
     * The subsets which have yet to be processed by the `DFA.minimise` algorithm,
     * plus possibly some empty subsets which do not need to be processed.
     *
     * Invariant: if `subset.isUnprocessed` then `unprocessed.includes(subset)`
     * Invariant: if `unprocessed.includes(subset)` and not `subset.isUnprocessed`, then `subset.start === subset.end`
     */
    unprocessed = [];
    /**
     * Maps each number from 0 to n - 1 to the subset it is a member of.
     *
     * Invariant: `map[x].start <= indices[x] && indices[x] < map[x].end`
     */
    map;
    /**
     * Constructs a new instance representing a partition of the numbers from
     * 0 to n - 1. The partition initially contains only a single subset (the
     * whole range).
     */
    constructor(n) {
        this.arr = makeArray(n, i => i);
        this.indices = makeArray(n, i => i);
        const initialSubset = this.makeSubset(0, n, true);
        this.map = emptyArray(n, initialSubset);
    }
    /**
     * Returns the number of subsets in this partition.
     */
    countSubsets() {
        return this.subsets.length;
    }
    makeSubset(start, end, isUnprocessed) {
        const { subsets } = this;
        const subset = {
            index: subsets.length,
            start,
            end,
            isUnprocessed,
            sibling: undefined,
        };
        subsets.push(subset);
        if (isUnprocessed) {
            this.unprocessed.push(subset);
        }
        return subset;
    }
    deleteSubset(subset) {
        // sanity check
        if (subset.start !== subset.end) {
            fail();
        }
        const { index } = subset;
        const removed = this.subsets.pop();
        if (removed.index !== index) {
            this.subsets[removed.index = index] = removed;
        }
        subset.isUnprocessed = false;
    }
    /**
     * Returns a subset which needs to be processed, and marks it as processed.
     * The elements are in no particular order.
     *
     * If no subsets remain to be processed, `undefined` is returned.
     */
    pollUnprocessed() {
        const { unprocessed } = this;
        while (unprocessed.length > 0) {
            const subset = unprocessed.pop();
            // have to check `isUnprocessed` because deleted subsets may still be in the stack
            if (subset.isUnprocessed) {
                subset.isUnprocessed = false;
                return this.arr.slice(subset.start, subset.end);
            }
        }
        return undefined;
    }
    /**
     * Returns a representative element from the subset in the partition which
     * contains the number `x`.
     */
    getRepresentative(x) {
        return this.arr[this.map[x].start];
    }
    /**
     * Calls the provided callback function with a representative element
     * from each subset in the partition.
     */
    forEachRepresentative(f) {
        const { arr } = this;
        for (const subset of this.subsets) {
            f(arr[subset.start]);
        }
    }
    getSet(x) {
        const { arr } = this;
        const { start, end } = this.map[x];
        const out = ISet.empty(arr.length);
        for (let i = start; i < end; ++i) {
            ISet.add(out, arr[i]);
        }
        return out;
    }
    /**
     * Refines this partition by splitting any subsets which partly intersect
     * with the given set. If an unprocessed subset is split, both parts are
     * marked unprocessed; otherwise, the smaller part is marked.
     */
    refine(set) {
        const { unprocessed, map } = this;
        const splits = [];
        ISet.forEach(set, x => {
            const subset = map[x];
            if (subset.sibling === undefined) {
                splits.push(subset);
                subset.sibling = this.makeSubset(subset.end, subset.end, subset.isUnprocessed);
            }
            this.moveToSibling(x, subset);
        });
        for (const subset of splits) {
            if (subset.start === subset.end) {
                this.deleteSubset(subset);
            }
            else if (!subset.isUnprocessed) {
                const sibling = subset.sibling;
                const min = subset.end - subset.start <= sibling.end - sibling.start ? subset : sibling;
                min.isUnprocessed = true;
                unprocessed.push(min);
            }
            subset.sibling = undefined;
        }
    }
    /**
     * Moves the element x from `subset` to `subset.sibling`, in O(1) time. The
     * sibling appears immediately afterwards in `arr`, so `x` is swapped with
     * the last member of `subset` and then the boundary is adjusted.
     */
    moveToSibling(x, subset) {
        const { arr, map, indices } = this;
        const sibling = subset.sibling;
        const i = indices[x];
        const j = subset.end = --sibling.start;
        const y = arr[j];
        arr[i] = y;
        indices[y] = i;
        arr[j] = x;
        indices[x] = j;
        map[x] = sibling;
    }
}
///<reference path="partition.ts"/>
var Regex;
(function (Regex) {
    function letters(letterIDs) {
        return { kind: 0 /* Kind.LETTERS */, letterIDs };
    }
    Regex.letters = letters;
    Regex.WILDCARD = { kind: 1 /* Kind.WILDCARD */ };
    function concat(children) {
        return { kind: 2 /* Kind.CONCAT */, children };
    }
    Regex.concat = concat;
    function union(children) {
        return { kind: 3 /* Kind.UNION */, children };
    }
    Regex.union = union;
    function kleeneStar(child) {
        return { kind: 4 /* Kind.KLEENESTAR */, child };
    }
    Regex.kleeneStar = kleeneStar;
    function accept(accept) {
        return { kind: 5 /* Kind.ACCEPT */, accept };
    }
    Regex.accept = accept;
    Regex.DOT_STAR = kleeneStar(Regex.WILDCARD);
    function compile(alphabetSize, regex) {
        return new NFA(alphabetSize, regex).toDFA();
    }
    Regex.compile = compile;
})(Regex || (Regex = {}));
class NFA {
    alphabetSize;
    nodes = [];
    startID;
    constructor(alphabetSize, regex) {
        this.alphabetSize = alphabetSize;
        this.startID = this.makeFromRegex(regex, this.makeNode([]));
        //console.log(`NFA with ${this.nodes.length} nodes on alphabet of size ${alphabetSize}`);
    }
    makeNode(epsilons, letters = [], nextID = -1) {
        const { nodes } = this;
        const id = nodes.length;
        nodes.push({ epsilons, letters, nextID, acceptSet: [], epsilonClosure: undefined });
        return id;
    }
    makeFromRegex(regex, outID) {
        // https://en.wikipedia.org/wiki/Thompson's_construction
        switch (regex.kind) {
            case 0 /* Regex.Kind.LETTERS */: {
                return this.makeNode([], regex.letterIDs, outID);
            }
            case 1 /* Regex.Kind.WILDCARD */: {
                return this.makeNode([], makeArray(this.alphabetSize, i => i), outID);
            }
            case 2 /* Regex.Kind.CONCAT */: {
                const { children } = regex;
                for (let i = children.length - 1; i >= 0; --i) {
                    outID = this.makeFromRegex(children[i], outID);
                }
                return outID;
            }
            case 3 /* Regex.Kind.UNION */: {
                const epsilons = regex.children.map(child => this.makeFromRegex(child, this.makeNode([outID])));
                return this.makeNode(epsilons);
            }
            case 4 /* Regex.Kind.KLEENESTAR */: {
                const childOutID = this.makeNode([outID]);
                const childInID = this.makeFromRegex(regex.child, childOutID);
                this.nodes[childOutID].epsilons.push(childInID);
                return this.makeNode([childInID, outID]);
            }
            case 5 /* Regex.Kind.ACCEPT */: {
                const node = this.nodes[outID];
                node.acceptSet.push(regex.accept);
                return outID;
            }
        }
    }
    getEpsilonClosure(nodeID) {
        const { nodes } = this;
        // epsilon closure, by depth-first search
        // use ISet instead of Set<number> or bigint for the state, for performance
        const cached = nodes[nodeID].epsilonClosure;
        if (cached !== undefined) {
            return cached;
        }
        const out = ISet.empty(nodes.length);
        const stack = [nodeID];
        while (stack.length > 0) {
            const id = stack.pop();
            if (ISet.has(out, id)) {
                continue;
            }
            ISet.add(out, id);
            for (const eps of nodes[id].epsilons) {
                if (!ISet.has(out, eps)) {
                    stack.push(eps);
                }
            }
        }
        nodes[nodeID].epsilonClosure = out;
        return out;
    }
    toDFA() {
        // https://en.wikipedia.org/wiki/Powerset_construction
        const { alphabetSize, nodes } = this;
        const nfaStates = IDMap.withKey(ISet.key);
        const dfaNodes = [];
        const cache = new Map();
        const getNodeID = (nfaState) => getOrCompute(cache, ISet.key(nfaState), () => {
            for (let nfaNodeID of ISet.toArray(nfaState)) {
                while (true) {
                    const epsilons = nodes[nfaNodeID].epsilons;
                    if (epsilons.length === 0) {
                        break;
                    }
                    else if (epsilons.length === 1) {
                        const nfaNodeID = epsilons[0];
                        if (ISet.has(nfaState, nfaNodeID)) {
                            break;
                        }
                        ISet.add(nfaState, nfaNodeID);
                    }
                    else {
                        ISet.addAll(nfaState, this.getEpsilonClosure(nfaNodeID));
                        break;
                    }
                }
            }
            return nfaStates.getOrCreateID(nfaState);
        });
        const startID = getNodeID(ISet.of(nodes.length, [this.startID]));
        // sanity check
        if (startID !== 0) {
            fail();
        }
        // this loop iterates over `nfaStates`, while adding to it via `getNodeID`
        for (let nfaStateID = 0; nfaStateID < nfaStates.size(); ++nfaStateID) {
            const transitionStates = makeArray(alphabetSize, () => ISet.empty(nodes.length));
            const accepts = [];
            ISet.forEach(nfaStates.getByID(nfaStateID), nfaNodeID => {
                const nfaNode = nodes[nfaNodeID];
                for (const letterID of nfaNode.letters) {
                    ISet.add(transitionStates[letterID], nfaNode.nextID);
                }
                accepts.push(...nfaNode.acceptSet);
            });
            dfaNodes.push({
                transitions: transitionStates.map(getNodeID),
                accepts,
            });
        }
        return new DFA(alphabetSize, dfaNodes);
    }
}
class DFA {
    alphabetSize;
    nodes;
    constructor(alphabetSize, nodes) {
        this.alphabetSize = alphabetSize;
        this.nodes = nodes;
        //console.log(`DFA with ${nodes.length} nodes on alphabet of size ${alphabetSize} and ${acceptSetMap.size()} accept sets`);
    }
    /**
     * Returns the number of distinct states of this DFA.
     */
    size() {
        return this.nodes.length;
    }
    toFlatArray() {
        return this.nodes.flatMap(node => node.transitions);
    }
    getAcceptSetMap(acceptMap, predicate) {
        const { nodes } = this;
        const acceptSets = nodes.map(predicate !== undefined
            ? node => node.accepts.filter(predicate)
            : node => node.accepts);
        const acceptSetMap = IDMap.ofWithKey(acceptSets, set => ISet.key(acceptMap.getIDSet(set)));
        return [
            acceptSetMap.getIDs(acceptSets),
            acceptSetMap,
        ];
    }
    /**
     * Returns an iterable mapping each accept to the set of node IDs which
     * accept it.
     */
    computeAcceptingStates(keyFunc) {
        const { nodes } = this;
        const n = nodes.length;
        const map = new Map();
        for (let id = 0; id < n; ++id) {
            for (const accept of nodes[id].accepts) {
                const key = keyFunc(accept);
                const set = getOrCompute(map, key, () => ISet.empty(n));
                ISet.add(set, id);
            }
        }
        return map.values();
    }
    /**
     * Returns an equivalent DFA with the minimum possible number of states.
     */
    minimise(keyFunc) {
        // https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft's_algorithm
        const { alphabetSize, nodes } = this;
        const n = nodes.length;
        const inverseTransitions = emptyArray(alphabetSize * n, undefined);
        for (let id = 0; id < n; ++id) {
            const { transitions } = nodes[id];
            for (let c = 0; c < alphabetSize; ++c) {
                (inverseTransitions[c * n + transitions[c]] ??= []).push(id);
            }
        }
        const partition = new Partition(n);
        for (const d of this.computeAcceptingStates(keyFunc)) {
            partition.refine(d);
        }
        // pre-allocate
        const refinement = ISet.empty(n);
        while (true) {
            const a = partition.pollUnprocessed();
            if (a === undefined) {
                break;
            }
            for (let c = 0; c < alphabetSize; ++c) {
                ISet.clear(refinement);
                for (const id of a) {
                    const arr = inverseTransitions[c * n + id];
                    if (arr !== undefined) {
                        // `ISet.addAll` would be faster for dense sets, but most of these sets are small
                        for (const x of arr) {
                            ISet.add(refinement, x);
                        }
                    }
                }
                partition.refine(refinement);
                // shortcut if the DFA cannot be minimised
                if (partition.countSubsets() === n) {
                    return this;
                }
            }
        }
        const reps = IDMap.withKey(id => partition.getRepresentative(id));
        // ensure id(rep(0)) === 0, so that 0 is still the starting state
        reps.getOrCreateID(0);
        partition.forEachRepresentative(x => reps.getOrCreateID(x));
        const repNodes = reps.map(rep => {
            const { transitions, accepts } = this.nodes[rep];
            return {
                transitions: reps.getIDs(transitions),
                accepts,
            };
        });
        return new DFA(alphabetSize, repNodes);
    }
    /**
     * Returns a new DFA, with the accept sets replaced using the function `f`.
     */
    map(f) {
        const mappedNodes = this.nodes.map(node => {
            const { transitions, accepts } = node;
            return {
                transitions,
                accepts: f(accepts),
            };
        });
        return new DFA(this.alphabetSize, mappedNodes);
    }
}
/**
 * Assigns unique, incremental IDs to a set of values.
 */
class IDMap {
    keyFunc;
    static IDENTITY = (x) => x;
    static empty() {
        return new IDMap(IDMap.IDENTITY);
    }
    static withKey(keyFunc) {
        return new IDMap(keyFunc);
    }
    /**
     * Creates a new IDMap with the distinct elements from `iterable`, with IDs
     * in order of first occurrence.
     */
    static of(iterable) {
        return IDMap.ofWithKey(iterable, IDMap.IDENTITY);
    }
    static ofWithKey(iterable, keyFunc) {
        const map = new IDMap(keyFunc);
        map.addAll(iterable);
        return map;
    }
    /**
     * Returns a new array of the distinct elements from `iterable`, in order
     * of first occurrence.
     */
    static distinct(iterable) {
        return IDMap.of(iterable).arr;
    }
    /**
     * Returns a new array of the elements from `iterable`, deduplicated using
     * the given key function, in order of first occurrence. If multiple values
     * have the same key, only the first is included.
     */
    static distinctByKey(iterable, keyFunc) {
        return IDMap.ofWithKey(iterable, keyFunc).arr;
    }
    /**
     * The distinct elements in this map, in insertion order.
     */
    arr = [];
    /**
     * Maps elements to their indices in `arr`.
     *
     * Invariant: `ids.get(keyFunc(x)) === i` if and only if `arr[i] === x`
     */
    ids = new Map();
    constructor(keyFunc) {
        this.keyFunc = keyFunc;
    }
    /**
     * Returns the number of elements in the map.
     */
    size() {
        return this.arr.length;
    }
    /**
     * Adds an element to the map if it is not already present, and returns the
     * element's ID, in O(1) time.
     */
    getOrCreateID(x) {
        const key = this.keyFunc(x);
        return getOrCompute(this.ids, key, () => {
            const id = this.arr.length;
            this.arr.push(x);
            return id;
        });
    }
    addAll(xs) {
        for (const x of xs) {
            this.getOrCreateID(x);
        }
    }
    /**
     * Indicates whether the given element is associated with an ID, in O(1)
     * time.
     */
    has(x) {
        return this.ids.has(this.keyFunc(x));
    }
    /**
     * Returns the ID of the given element, in O(1) time. An error is thrown if
     * the element is not associated with an ID.
     */
    getID(x) {
        return this.ids.get(this.keyFunc(x)) ?? fail();
    }
    getIDs(xs) {
        return xs.map(x => this.getID(x));
    }
    getIDSet(xs) {
        const set = ISet.empty(this.arr.length);
        for (const x of xs) {
            ISet.add(set, this.getID(x));
        }
        return set;
    }
    /**
     * Returns the ID of the given element, or -1 if the given element is not
     * associated with an ID, in O(1) time.
     */
    getIDOrDefault(x) {
        return this.ids.get(this.keyFunc(x)) ?? -1;
    }
    /**
     * Returns the element associated with the given ID, in O(1) time. An error
     * is thrown if there is no element with the given ID.
     */
    getByID(id) {
        return id >= 0 && id < this.arr.length ? this.arr[id] : fail();
    }
    forEach(f) {
        this.arr.forEach(f);
    }
    filter(f) {
        return this.arr.filter(f);
    }
    map(f) {
        return this.arr.map(f);
    }
    flatMap(f) {
        return this.arr.flatMap(f);
    }
}
/**
 * Helper functions for using a typed array as a set of natural numbers.
 *
 * Aggregate operations `addAll`, `toArray` and `forEach` are O(N), where N is
 * the domain size; therefore they must not be used in the pattern matching loop.
 */
var ISet;
(function (ISet) {
    /**
     * Creates an empty set, which can contain numbers `0 <= x < domainSize`.
     */
    function empty(domainSize) {
        return new Uint32Array((domainSize + 31) >> 5);
    }
    ISet.empty = empty;
    /**
     * Creates a set containing the whole domain `0 <= x < domainSize`.
     */
    function full(domainSize) {
        const set = empty(domainSize);
        set.fill(-1);
        if ((domainSize & 31) !== 0) {
            set[set.length - 1] = (1 << (domainSize & 31)) - 1;
        }
        return set;
    }
    ISet.full = full;
    /**
     * Creates a set from an iterable of natural numbers, all of which must be
     * less than `domainSize`.
     */
    function of(domainSize, xs) {
        const set = empty(domainSize);
        for (const x of xs) {
            add(set, x);
        }
        return set;
    }
    ISet.of = of;
    /**
     * Indicates whether `set` contains the element `x`, in O(1) time.
     */
    function has(set, x) {
        return (set[x >> 5] & (1 << (x & 31))) !== 0;
    }
    ISet.has = has;
    /**
     * Returns the size of the set, in O(N) time.
     */
    function size(set) {
        let count = 0;
        for (let x of set) {
            while (x !== 0) {
                x &= x - 1;
                ++count;
            }
        }
        return count;
    }
    ISet.size = size;
    /**
     * Adds the element `x` to the set if it not already present, in O(1) time.
     */
    function add(set, x) {
        set[x >> 5] |= 1 << (x & 31);
    }
    ISet.add = add;
    /**
     * Adds all the members of the set `b` to the set `a`, in O(N) time.
     */
    function addAll(a, b) {
        if (a.length < b.length) {
            fail();
        }
        for (let i = 0; i < b.length; ++i) {
            a[i] |= b[i];
        }
    }
    ISet.addAll = addAll;
    /**
     * Removes the members from the set `a` which are not in `b`, in O(N) time.
     */
    function retainAll(a, b) {
        if (a.length < b.length) {
            fail();
        }
        for (let i = 0; i < b.length; ++i) {
            a[i] &= b[i];
        }
    }
    ISet.retainAll = retainAll;
    /**
     * Removes all the members of the set `b` to the set `a`, in O(N) time.
     */
    function removeAll(a, b) {
        if (a.length < b.length) {
            fail();
        }
        for (let i = 0; i < b.length; ++i) {
            a[i] &= ~b[i];
        }
    }
    ISet.removeAll = removeAll;
    /**
     * Removes all elements from the set, in O(N) time.
     */
    function clear(a) {
        for (let i = 0; i < a.length; ++i) {
            a[i] = 0;
        }
    }
    ISet.clear = clear;
    /**
     * Returns a new set which is the union of `a` and `b`, in O(N) time.
     */
    function union(a, b) {
        const out = new Uint32Array(a);
        addAll(out, b);
        return out;
    }
    ISet.union = union;
    /**
     * Returns a new set which is the intersection of `a` and `b`, in O(N) time.
     */
    function intersection(a, b) {
        const out = new Uint32Array(a);
        retainAll(out, b);
        return out;
    }
    ISet.intersection = intersection;
    /**
     * Determines whether the two sets are disjoint (i.e. they have no elements
     * in common).
     */
    function isDisjoint(a, b) {
        if (a.length < b.length) {
            fail();
        }
        for (let i = 0; i < b.length; ++i) {
            if ((a[i] & b[i]) !== 0) {
                return false;
            }
        }
        return true;
    }
    ISet.isDisjoint = isDisjoint;
    /**
     * Determines whether `a` is a subset of `b`, in O(N) time.
     */
    function isSubset(a, b) {
        if (a.length > b.length) {
            fail();
        }
        for (let i = 0; i < a.length; ++i) {
            if ((a[i] & ~b[i]) !== 0) {
                return false;
            }
        }
        return true;
    }
    ISet.isSubset = isSubset;
    /**
     * Converts an unordered array to a primitive type, suitable for use as a
     * Map key, in O(N) time.
     */
    function arrayToKey(xs) {
        if (xs.length === 0) {
            return 0;
        }
        const domainSize = Math.max(...xs) + 1;
        return key(of(domainSize, xs));
    }
    ISet.arrayToKey = arrayToKey;
    /**
     * Converts a set to a primitive type, suitable for use as a Map key, in
     * O(N) time.
     */
    function key(set) {
        // this function is part of the hot loop in `NFA.toDFA`, so it needs to be fast
        switch (set.length) {
            case 0:
                return 0;
            case 1:
                return set[0];
            case 2:
                return BigInt(set[0]) | BigInt(set[1]) << 32n;
            case 3:
                return BigInt(set[0]) | BigInt(set[1]) << 32n | BigInt(set[2]) << 64n;
            case 4:
                return (BigInt(set[0]) | BigInt(set[1]) << 32n)
                    | (BigInt(set[2]) | BigInt(set[3]) << 32n) << 64n;
            default:
                // O(N) time
                return String.fromCharCode(...new Uint16Array(set.buffer));
        }
    }
    ISet.key = key;
    /**
     * Sentinel value used to halt the `_forEach` function.
     */
    const STOP_ITERATION = Symbol();
    function _forEach(set, f) {
        for (let i = 0; i < set.length; ++i) {
            const x = i << 5;
            let setPart = set[i];
            while (setPart !== 0) {
                // position of the highest 1 bit
                const dx = 31 - Math.clz32(setPart);
                // 'x ^ dx' is equivalent to `x + dx` here
                if (f(x ^ dx) === STOP_ITERATION) {
                    return false;
                }
                // clear this bit
                setPart ^= 1 << dx;
            }
        }
        return true;
    }
    /**
     * Calls the function `f` for each element of the set, not necessarily in
     * order.
     */
    ISet.forEach = _forEach;
    /**
     * Returns a new array of the natural numbers in the given set, not
     * necessarily in order.
     */
    function toArray(set) {
        const arr = [];
        _forEach(set, x => arr.push(x));
        return arr;
    }
    ISet.toArray = toArray;
    /**
     * Determines whether the predicate is true for every element of the set.
     */
    function every(set, f) {
        return _forEach(set, x => !f(x) ? STOP_ITERATION : undefined);
    }
    ISet.every = every;
    /**
     * Determines whether the predicate is true for some element of the set.
     */
    function some(set, f) {
        return !_forEach(set, x => f(x) ? STOP_ITERATION : undefined);
    }
    ISet.some = some;
})(ISet || (ISet = {}));
function fail(...args) {
    console.log(args.length > 0 ? 'Assertion failed with arguments:' : 'Assertion failed', ...args);
    throw new Error('Internal compiler error: assertion failed');
}
function objHasKey(obj, key) {
    return Object.prototype.hasOwnProperty.call(obj, key);
}
/**
 * Creates an empty array of length `n`, filled with the given value.
 */
function emptyArray(n, value) {
    return makeArray(n, () => value);
}
/**
 * Creates an array of length `n`, initialised using the given callback function.
 */
function makeArray(n, f) {
    // equivalent to `Array(n).map((_, i) => f(i))`, but guarantees an array without holes, which may be more performant to use
    const arr = [];
    for (let i = 0; i < n; ++i) {
        arr.push(f(i));
    }
    return arr;
}
function withNextID(arr, obj) {
    const t = obj;
    t.id = arr.length;
    arr.push(t);
    return t;
}
function getOrCompute(map, key, f) {
    let v = map.get(key);
    if (v === undefined) {
        map.set(key, v = f());
    }
    return v;
}
function quoteJoin(hints, delimiter = ', ') {
    return hints.map(s => `'${s}'`).join(delimiter);
}
///<reference path="../runtime/mjr.ts"/>
var PatternTree;
(function (PatternTree) {
    /**
     * Returns the intersection of two patterns, or `undefined` if they are
     * mutually exclusive.
     */
    function _intersect(left, right) {
        const { alphabetKey, masks: leftMasks } = left;
        const pattern = [];
        const masks = [];
        for (let i = 0; i < leftMasks.length; ++i) {
            const mask = ISet.intersection(leftMasks[i], right.masks[i]);
            const maskSize = ISet.size(mask);
            if (maskSize === 0) {
                return undefined;
            }
            pattern.push(maskSize === 1 ? ISet.toArray(mask)[0]
                : maskSize === alphabetKey.length ? -1
                    : -2);
            masks.push(mask);
        }
        return new Pattern(left.width, left.height, alphabetKey, pattern, masks, true);
    }
    /**
     * Conservatively estimates of whether, if `left` matches, `right` must too.
     */
    function _entails(left, right) {
        switch (left.kind) {
            case 'leaf':
                return left.masks.every((mask, i) => ISet.isSubset(mask, right.masks[i]));
            case 'top':
                return false;
            case 'bottom':
                return true;
            case 'and':
                return _entails(left.left, right) || _entails(left.right, right);
            case 'or':
                return _entails(left.left, right) && _entails(left.right, right);
            case 'not':
                return false;
        }
    }
    function top(width, height) {
        return { kind: 'top', width, height, _key: undefined };
    }
    PatternTree.top = top;
    function bottom(width, height) {
        return { kind: 'bottom', width, height, _key: undefined };
    }
    PatternTree.bottom = bottom;
    function and(left, right) {
        const { width, height } = left;
        if (right.width !== width || right.height !== height) {
            fail();
        }
        if (left.kind === 'bottom' || right.kind === 'top') {
            return left;
        }
        else if (left.kind === 'top' || right.kind === 'bottom') {
            return right;
        }
        else if (left.kind === 'leaf' && right.kind === 'leaf') {
            return _intersect(left, right) ?? bottom(width, height);
        }
        else if (left.kind === 'leaf' && right.kind === 'not' && _intersect(left, right.child) === undefined) {
            return left;
        }
        else if (right.kind === 'leaf' && left.kind === 'not' && _intersect(left.child, right) === undefined) {
            return right;
        }
        else if ((left.kind === 'not' && _entails(right, left.child)) || (right.kind === 'not' && _entails(left, right.child))) {
            return bottom(width, height);
        }
        return { kind: 'and', left, right, width, height, _key: undefined };
    }
    PatternTree.and = and;
    function or(left, right) {
        const { width, height } = left;
        if (right.width !== width || right.height !== height) {
            fail();
        }
        if (left.kind === 'top' || right.kind === 'bottom') {
            return left;
        }
        else if (left.kind === 'bottom' || right.kind === 'top') {
            return right;
        }
        else if (left.kind === 'leaf' && right.kind === 'leaf' && width === 1 && height === 1) {
            const { alphabetKey } = left;
            const mask = ISet.union(left.masks[0], right.masks[0]);
            return ISet.size(mask) === alphabetKey.length
                ? top(1, 1)
                : new Pattern(1, 1, alphabetKey, [-2], [mask], true);
        }
        return { kind: 'or', left, right, width, height, _key: undefined };
    }
    PatternTree.or = or;
    function not(child) {
        const { width, height } = child;
        switch (child.kind) {
            case 'top':
                return bottom(width, height);
            case 'bottom':
                return top(width, height);
            case 'and':
                return or(not(child.left), not(child.right));
            case 'or':
                return and(not(child.left), not(child.right));
            case 'not':
                return child.child;
        }
        // leaf
        if (width === 1 && height === 1) {
            const { alphabetKey } = child;
            const mask = ISet.full(alphabetKey.length);
            ISet.removeAll(mask, child.masks[0]);
            return new Pattern(width, height, alphabetKey, [-2], [mask], true);
        }
        return { kind: 'not', child, width, height, _key: undefined };
    }
    PatternTree.not = not;
    function rotate(p) {
        switch (p.kind) {
            case 'leaf':
                return Pattern.rotate(p);
            case 'top':
                return top(p.height, p.width);
            case 'bottom':
                return bottom(p.height, p.width);
            case 'and':
                return and(rotate(p.left), rotate(p.right));
            case 'or':
                return or(rotate(p.left), rotate(p.right));
            case 'not':
                return not(Pattern.rotate(p.child));
        }
    }
    PatternTree.rotate = rotate;
    function reflect(p) {
        switch (p.kind) {
            case 'leaf':
                return Pattern.reflect(p);
            case 'top':
            case 'bottom':
                return p;
            case 'and':
                return and(reflect(p.left), reflect(p.right));
            case 'or':
                return or(reflect(p.left), reflect(p.right));
            case 'not':
                return not(Pattern.reflect(p.child));
        }
    }
    PatternTree.reflect = reflect;
    function getLeaves(p) {
        switch (p.kind) {
            case 'leaf':
            case 'top':
                return [p];
            case 'bottom':
                return [];
            case 'and':
            case 'or': {
                const out = getLeaves(p.left);
                out.push(...getLeaves(p.right));
                return out;
            }
            case 'not': {
                const out = getLeaves(p.child);
                out.push(top(p.width, p.height));
                return out;
            }
        }
    }
    PatternTree.getLeaves = getLeaves;
    /**
     * Evaluates a pattern tree as a boolean expression, using the given
     * predicate to evaluate the leaf and top nodes.
     */
    function matches(p, predicate) {
        switch (p.kind) {
            case 'leaf':
            case 'top':
                return predicate(p);
            case 'bottom':
                return false;
            case 'and':
                return matches(p.left, predicate) && matches(p.right, predicate);
            case 'or':
                return matches(p.left, predicate) || matches(p.right, predicate);
            case 'not':
                return predicate(top(p.width, p.height)) && !matches(p.child, predicate);
        }
    }
    PatternTree.matches = matches;
    function key(p) {
        switch (p.kind) {
            case 'leaf':
            case 'top':
                return Pattern.key(p);
            case 'bottom':
                return p._key ??= `${p.width}x${p.height}:${p.kind}`;
            case 'and':
            case 'or':
                return p._key ??= `(${key(p.left)}) ${p.kind} (${key(p.right)})`;
            case 'not':
                return p._key ??= `not (${key(p.child)})`;
        }
    }
    PatternTree.key = key;
})(PatternTree || (PatternTree = {}));
class Pattern extends MJr.Pattern {
    alphabetKey;
    masks;
    hasUnions;
    static rowsOf(p) {
        const { width, height } = p;
        if (height === 1) {
            return [p];
        }
        if (p.kind === 'top') {
            return emptyArray(height, PatternTree.top(width, 1));
        }
        const { pattern, masks } = p;
        const out = [];
        const n = width * height;
        for (let offset = 0; offset < n; offset += width) {
            const row = pattern.slice(offset, offset + width);
            const rowMasks = masks.slice(offset, offset + width);
            out.push(new Pattern(width, 1, p.alphabetKey, row, rowMasks, p.hasUnions));
        }
        return out;
    }
    static rotate(p) {
        const { width, height, pattern, masks } = p;
        const newData = [];
        const newMasks = [];
        for (let x = 0; x < width; ++x) {
            for (let y = height - 1; y >= 0; --y) {
                const index = x + width * y;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(height, width, p.alphabetKey, newData, newMasks, p.hasUnions);
    }
    static reflect(p) {
        const { width, height, pattern, masks } = p;
        const newData = [];
        const newMasks = [];
        for (let y = height - 1; y >= 0; --y) {
            for (let x = 0; x < width; ++x) {
                const index = x + width * y;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(width, height, p.alphabetKey, newData, newMasks, p.hasUnions);
    }
    static key(p) {
        return p._key ??= `${p.width}x${p.height}:${p.kind === 'leaf' ? p.masks.map(ISet.key).join(';') : 'top'}`;
    }
    kind = 'leaf';
    _key = undefined;
    constructor(
    /**
     * The pattern's width.
     */
    width, 
    /**
     * The pattern's height.
     */
    height, alphabetKey, 
    /**
     * The pattern, as a flat array. Wildcards are represented as -1, and
     * unions as -2.
     */
    pattern, 
    /**
     * The pattern, as a flat array of bitmasks.
     */
    masks, 
    /**
     * Indicates whether this pattern has any unions, i.e. cells which can
     * match multiple alphabet symbols, but are not wildcards.
     */
    hasUnions) {
        super(width, height, pattern);
        this.alphabetKey = alphabetKey;
        this.masks = masks;
        this.hasUnions = hasUnions;
    }
    /**
     * Indicates whether this pattern is tautological, i.e. it always matches
     * at any position.
     */
    isTop() {
        return this.pattern.every(p => p === -1);
    }
    /**
     * Calls the given function for each non-wildcard, non-union symbol in this
     * pattern.
     */
    forEach(f) {
        const v = this.vectorData;
        for (let i = 0; i < v.length; i += 3) {
            f(v[i], v[i + 1], v[i + 2]);
        }
    }
    map(f) {
        const out = [];
        this.forEach((dx, dy, c) => out.push(f(dx, dy, c)));
        return out;
    }
    every(f) {
        const v = this.vectorData;
        for (let i = 0; i < v.length; i += 3) {
            if (!f(v[i], v[i + 1], v[i + 2])) {
                return false;
            }
        }
        return true;
    }
    some(f) {
        return !this.every((dx, dy, c) => !f(dx, dy, c));
    }
}
