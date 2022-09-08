///<reference path="base.ts"/>

namespace CodeGen {
    const RUNTIME_LIB_NAME = 'MJr';
    
    // operator precedences from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table
    const enum Precedence {
        MAX = 18,
        ATTR_ACCESS_CALL = 17,
        LITERAL = 16, // e.g. `6.toString()` is a syntax error; this is the easiest way to avoid it
        BOOL_NOT = 14,
        BITWISE_NOT = 14,
        UPLUS_UMINUS = 14,
        MULT_DIV_MOD = 12,
        PLUS_MINUS = 11,
        BITWISE_SHIFT = 10,
        CMP = 9,
        EQ = 8,
        BITWISE_AND = 7,
        BITWISE_XOR = 6,
        BITWISE_OR = 5,
        BOOL_AND = 4,
        BOOL_OR = 3,
        NULL_COALESCE = 3,
        ASSIGN = 2,
        TERNARY = 2,
        MIN = 0,
    }
    
    export class JavaScript extends Base {
        readonly STMT_WRITE_FUNCS: StmtWriteFuncs<this> = {
            'stmt.assign': (out, stmt) => {
                const {left, op, right} = stmt;
                out.beginLine();
                if((op === '+=' || op === '-=') && right.kind === 'expr.literal.int' && right.value === 1) {
                    out.write(op === '+=' ? '++' : '--');
                    out.writeExpr(left);
                } else {
                    out.writeExpr(left);
                    out.write(` ${op} `);
                    out.writeExpr(right);
                }
                out.write(';');
            },
            'stmt.block': (out, stmt) => {
                out.write(' {');
                out.indent();
                for(const c of stmt.children) { out.writeStmt(c); }
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
            'stmt.decl.func': (out, stmt) => {
                const {params, paramTypes} = stmt;
                out.beginLine();
                out.write('function');
                if(stmt.yields !== undefined) { out.write('*'); }
                out.write(' ');
                out.writeExpr(stmt.name);
                out.write('(');
                out.writeList(i => out.writeParamDecl(params[i].name, paramTypes[i]), params.length);
                out.write(')');
                out.writeReturnType(stmt.returnType, stmt.yields);
                out.writeIndentedBlock(stmt.body);
            },
            'stmt.decl.vars': (out, stmt) => {
                const {decls, mutable} = stmt;
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
                const {index: {name: i}, low, high, reverse, body} = stmt;
                out.beginLine();
                out.write(`for(let ${i} = `);
                if(reverse) {
                    out.writeExpr(IR.OP.minusOne(high));
                    out.write(`; ${i} >= `);
                    out.writeExpr(low, Precedence.CMP);
                    out.write(`; --${i})`);
                } else {
                    out.writeExpr(low);
                    out.write(`; ${i} < `);
                    out.writeExpr(high, Precedence.CMP);
                    out.write(`; ++${i})`);
                }
                out.writeIndentedBlock(body);
            },
            'stmt.if': (out, stmt) => {
                let cur: IR.Stmt | undefined = stmt;
                out.beginLine();
                while(true) {
                    out.write('if(');
                    out.writeExpr(cur.condition);
                    out.write(')');
                    // intented block has braces, avoiding parsing hazard of `if(...) if(...) ... else ...`
                    out.writeIndentedBlock(cur.then);
                    
                    cur = cur.otherwise;
                    if(cur === undefined) { break; }
                    
                    out.write(' else');
                    if(cur.kind === 'stmt.if') {
                        out.write(' ');
                    } else {
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
                
                if(stmt.emitChecks) {
                    // TODO: add code to check params are valid at runtime
                    const {libVersion} = stmt;
                    out.beginLine();
                    out.beginLine();
                    out.write(`if(typeof ${RUNTIME_LIB_NAME} !== "object" || typeof ${RUNTIME_LIB_NAME}.VERSION !== "number") throw new Error("${RUNTIME_LIB_NAME} runtime library not found");`);
                    out.beginLine();
                    out.write(`if(${RUNTIME_LIB_NAME}.VERSION !== ${libVersion}) throw new Error("Requires ${RUNTIME_LIB_NAME} runtime library version ${libVersion}");`);
                }
                
                out.beginLine();
                out.write(`rng ??= ${RUNTIME_LIB_NAME}.DEFAULT_PRNG;`);
                
                for(const op of stmt.opsUsed) {
                    if(objHasKey(MJr.OPS, op)) {
                        out.beginLine();
                        out.write(`const ${op} = ${RUNTIME_LIB_NAME}.OPS.${op};`);
                    }
                }
            },
            'stmt.return': (out, stmt) => {
                out.beginLine();
                out.write('return');
                if(stmt.expr !== undefined) {
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
                const {then} = stmt;
                out.beginLine();
                out.write('while(');
                out.writeExpr(stmt.condition);
                out.write(')');
                
                if(then.kind === 'stmt.switch') {
                    // main loop is like `while(...) switch(...) { ... }`, don't need two levels of indentation for switch body
                    out.write(' ');
                    out.writeSwitch(then);
                } else {
                    out.writeIndentedBlock(then);
                }
            },
            'stmt.yield': (out, stmt) => {
                out.beginLine();
                this.write('yield');
                if(stmt.expr !== undefined) {
                    this.write(' ');
                    this.writeExpr(stmt.expr);
                }
                this.write(';');
            },
        };
        
        readonly EXPR_WRITE_FUNCS: ExprWriteFuncs<this> = {
            'expr.array.const': [Precedence.MAX, (out, expr) => {
                const {from} = expr;
                const bits = uintBits(expr.domainSize);
                const s = arrayToHex(from, bits);
                out.write(`${RUNTIME_LIB_NAME}.HEX.u${bits}(`);
                out.writeLongStringLiteral(s, expr.rowLength * s.length / from.length);
                out.write(')');
            }],
            'expr.array.new': [Precedence.MAX, (out, expr) => {
                const bits = uintBits(expr.domainSize);
                out.write(`new Uint${bits}Array(`);
                out.writeExpr(expr.length);
                out.write(')');
            }],
            'expr.attr': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                out.writeExpr(expr.left, Precedence.ATTR_ACCESS_CALL);
                out.write(`.${expr.attr}`);
            }],
            'expr.dict': [Precedence.MAX, (out, expr) => {
                const {type: {keys}, values} = expr;
                out.write('{');
                out.writeList(i => {
                    out.write(`${keys[i]}: `);
                    out.writeExpr(values[i]);
                }, keys.length, 1);
                out.write('}');
            }],
            'expr.letin': [Precedence.MAX, (out, expr) => {
               throw new Error();
            }],
            'expr.literal.bool': _literal,
            'expr.literal.float': _literal,
            'expr.literal.int': _literal,
            'expr.literal.str': _literal,
            'expr.literal.null': [Precedence.MAX, (out, expr) => {
                out.write('undefined');
            }],
            'expr.name': [Precedence.MAX, (out, expr) => {
                out.write(expr.name);
            }],
            'expr.op.access': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                out.writeExpr(expr.left, Precedence.ATTR_ACCESS_CALL);
                out.write('[');
                out.writeExpr(expr.right);
                out.write(']');
            }],
            'expr.op.call.lib.constructor': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                out.write(`new ${RUNTIME_LIB_NAME}.${expr.className}`);
                out.write(`(`);
                out.writeExprList(expr.args);
                out.write(')');
            }],
            'expr.op.call.lib.function': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                out.write(`${RUNTIME_LIB_NAME}.${expr.name}`);
                out.write(`(`);
                out.writeExprList(expr.args);
                out.write(')');
            }],
            'expr.op.call.lib.method': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                out.writeExpr(expr.obj);
                out.write(`.${expr.name}(`);
                out.writeExprList(expr.args);
                out.write(')');
            }],
            'expr.op.call.local': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                out.writeExpr(expr.name);
                out.write('(');
                out.writeExprList(expr.args);
                out.write(')');
            }],
            'expr.param': [Precedence.NULL_COALESCE, (out, expr) => {
                out.write(`params?.${expr.name} ?? `);
                out.writeExpr(expr.otherwise, Precedence.NULL_COALESCE + 1);
            }],
            'expr.op.ternary': [Precedence.TERNARY, (out, expr) => {
                out.writeExpr(expr.condition, Precedence.TERNARY + 1);
                out.write(' ? ');
                out.writeExpr(expr.then, Precedence.TERNARY);
                out.write(' : ');
                out.writeExpr(expr.otherwise, Precedence.TERNARY);
            }],
        };
        
        readonly BINARY_OPS = (function(): BinaryOpSpecs {
            function _intOp(p: Precedence, op: string): BinaryOpSpec {
                // all of these ops are left-associative
                return binaryOp(Precedence.BITWISE_OR, '(', p, ` ${op} `, p + 1, ') | 0');
            }
            function _func(name: 'Math.imul' | keyof typeof MJr.OPS): BinaryOpSpec {
                return binaryOp(Precedence.ATTR_ACCESS_CALL, `${name}(`, Precedence.MIN, ', ', Precedence.MIN, ')');
            }
            
            const PLUS = infixOp(Precedence.PLUS_MINUS, '+', Associativity.BOTH),
                MINUS = infixOp(Precedence.PLUS_MINUS, '-'),
                EQ = infixOp(Precedence.EQ, '==='),
                NE = infixOp(Precedence.EQ, '!=='),
                LT = infixOp(Precedence.CMP, '<'),
                LE = infixOp(Precedence.CMP, '<='),
                GT = infixOp(Precedence.CMP, '>'),
                GE = infixOp(Precedence.CMP, '>=');
            
            return {
                bool_and: infixOp(Precedence.BOOL_AND, '&&', Associativity.BOTH),
                bool_or: infixOp(Precedence.BOOL_OR, '||', Associativity.BOTH),
                bool_eq: EQ,
                bool_ne: NE,
                
                // + and * are not strictly right-associative for floats
                float_plus: infixOp(Precedence.PLUS_MINUS, '+'),
                float_minus: MINUS,
                float_mult: infixOp(Precedence.MULT_DIV_MOD, '*'),
                float_truediv: infixOp(Precedence.MULT_DIV_MOD, '/'),
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
                
                int_plus: _intOp(Precedence.PLUS_MINUS, '+'),
                int_minus: _intOp(Precedence.PLUS_MINUS, '-'),
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
                int_and: infixOp(Precedence.BITWISE_AND, '&', Associativity.BOTH),
                int_or: infixOp(Precedence.BITWISE_OR, '|', Associativity.BOTH),
                int_lshift: infixOp(Precedence.BITWISE_SHIFT, '<<'),
                int_rshift: infixOp(Precedence.BITWISE_SHIFT, '>>'),
                
                str_concat: PLUS,
                str_eq: EQ,
                str_ne: NE,
                
                loose_int_plus: PLUS,
                loose_int_minus: MINUS,
                loose_int_mult: infixOp(Precedence.MULT_DIV_MOD, '*', Associativity.BOTH),
                loose_int_floordiv: _intOp(Precedence.MULT_DIV_MOD, '/'),
                loose_int_mod: infixOp(Precedence.MULT_DIV_MOD, '%'),
            };
        })();
        
        readonly UNARY_OPS = (function(): {[K in IR.UnaryOp]: UnaryOpSpec | typeof NOOP} {
            function _intOp(p: Precedence, op: string): UnaryOpSpec {
                return unaryOp(Precedence.BITWISE_OR, `(${op}`, p, ') | 0');
            }
            function _func(name: keyof typeof MJr.OPS): UnaryOpSpec {
                return unaryOp(Precedence.ATTR_ACCESS_CALL, `${name}(`, Precedence.MIN, ')');
            }
            
            const TO_STR = unaryOp(Precedence.ATTR_ACCESS_CALL, '', Precedence.ATTR_ACCESS_CALL, '.toString()');
            
            return {
                bool_not: prefixOp(Precedence.BOOL_NOT, '!'),
                
                // need space to avoid incorrect parse of `- - x`
                float_uminus: prefixOp(Precedence.UPLUS_UMINUS, '- '),
                float_checkzero: _func('float_checkzero'),
                
                fraction_uminus: _func('fraction_uminus'),
                fraction_checkzero: NOOP,
                
                // need space to avoid incorrect parse of `- - x`
                int_uminus: _intOp(Precedence.UPLUS_UMINUS, '- '),
                int_checkzero: _func('int_checkzero'),
                int_to_float: NOOP,
                int_to_fraction: _func('int_to_fraction'),
                int_not: prefixOp(Precedence.BITWISE_NOT, '~'),
                
                bool_to_str: TO_STR,
                float_to_str: TO_STR,
                fraction_to_str: _func('fraction_to_str'),
                grid_to_str: TO_STR,
                int_to_str: TO_STR,
            };
        })();
        
        public writeAssignExpr(left: IR.NameExpr, right: IR.Expr): void {
            this.writeExpr(left);
            this.write(' = ');
            this.writeExpr(right, Precedence.ASSIGN);
        }
        
        private writeSwitch(stmt: IR.SwitchStmt): void {
            this.write('switch(');
            this.writeExpr(stmt.expr);
            this.write(') {');
            this.indent();
            for(const c of stmt.cases) {
                for(const value of c.values) {
                    this.beginLine();
                    this.write(`case ${value}:`);
                }
                this.writeIndentedBlock(IR.block([c.then, IR.BREAK]));
            }
            this.dedent();
            this.beginLine();
            this.write('}');
        }
        
        writeParamDecl(name: string, type: IR.IRType): void {
            this.write(name);
        }
        writeVarDecl(decl: IR.VarDecl): void {
            this.writeExpr(decl.name);
            if(decl.initialiser !== undefined) {
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type: IR.IRType, yields: IR.IRType | undefined): void {}
    }
    
    export class TypeScript extends JavaScript {
        writeParamDecl(name: string, type: IR.IRType): void {
            this.write(name);
            if(type.kind === 'nullable') {
                this.write('?');
                type = type.componentType;
            }
            this.write(': ');
            this.writeType(type);
        }
        writeVarDecl(decl: IR.VarDecl): void {
            const noInit = decl.initialiser === undefined;
            
            this.writeExpr(decl.name);
            if(noInit) {
                this.write(noInit ? '!: ' : ': ');
                this.writeType(decl.type);
            } else {
                // otherwise, type should be inferred from initialiser
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type: IR.IRType, yields: IR.IRType | undefined): void {
            this.write(': ');
            if(yields === undefined) {
                this.writeType(type);
            } else {
                this.write(`Generator<`);
                this.writeType(yields);
                this.write(`, `);
                this.writeType(type);
                this.write(`>`);
            }
        }
        
        private writeType(type: IR.IRType): void {
            switch(type.kind) {
                case 'dict': {
                    const {keys, values} = type;
                    this.write('{');
                    this.writeList(i => this.writeParamDecl(keys[i], values[i]), keys.length);
                    this.write('}');
                    return;
                }
                case 'array.const':
                case 'array.mutable': {
                    this.write(`Uint${uintBits(type.domainSize)}Array`);
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
    
    const _literal = [Precedence.LITERAL, (out: JavaScript, expr: IR.BoolLiteralExpr | IR.FloatLiteralExpr | IR.IntLiteralExpr | IR.StrLiteralExpr) => {
        out.write(JSON.stringify(expr.value));
    }] as const;
}
