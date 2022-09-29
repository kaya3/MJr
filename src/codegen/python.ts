///<reference path="base.ts"/>

namespace CodeGen {
    const RUNTIME_LIB_NAME = 'MJr';
    
    // operator precedences from https://docs.python.org/3/reference/expressions.html#operator-precedence
    const enum Precedence {
        MAX = 18,
        ATTR_ACCESS_CALL = 17,
        BITWISE_NOT = 14,
        UPLUS_UMINUS = 14,
        MULT_DIV_MOD = 13,
        PLUS_MINUS = 12,
        BITWISE_SHIFT = 11,
        BITWISE_AND = 10,
        BITWISE_XOR = 9,
        BITWISE_OR = 8,
        CMP_EQ = 7,
        BOOL_NOT = 6,
        BOOL_AND = 5,
        BOOL_OR = 4,
        TERNARY = 3,
        ASSIGN = 2,
        MIN = 0,
    }
    
    export class Python extends Base {
        readonly STMT_WRITE_FUNCS: StmtWriteFuncs<this> = {
            'stmt.assign': (out, stmt) => {
                const {left, op, right} = stmt;
                out.beginLine();
                out.writeExpr(left);
                out.write(` ${op} `);
                out.writeExpr(right);
            },
            'stmt.block': (out, stmt) => {
                out.write(':');
                out.indent();
                for(const c of stmt.children) { out.writeStmt(c); }
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
                const {params, paramTypes} = stmt;
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
                for(const decl of stmt.decls) { out.writeVarDecl(decl); }
            },
            'stmt.expr': (out, stmt) => {
                out.beginLine();
                out.writeExpr(stmt.expr);
            },
            'stmt.for.range': (out, stmt) => {
                const {low, high} = stmt;
                out.beginLine();
                out.write(`for ${stmt.index.name} in range(`);
                if(stmt.reverse) {
                    out.writeExpr(IR.OP.minusOne(high));
                    out.write(`, `);
                    out.writeExpr(IR.OP.minusOne(low));
                    out.write(`, -1)`);
                } else {
                    if(low !== IR.ZERO) {
                        out.writeExpr(low);
                        out.write(`, `);
                    }
                    out.writeExpr(high);
                    out.write(`)`);
                }
                out.writeIndentedBlock(stmt.body);
            },
            'stmt.if': (out, stmt) => {
                let cur: IR.Stmt | undefined = stmt;
                out.beginLine();
                out.write('if ');
                while(true) {
                    out.writeExpr(cur.condition);
                    out.writeIndentedBlock(cur.then);
                    
                    cur = cur.otherwise;
                    if(cur === undefined) { break; }
                    
                    out.beginLine();
                    if(cur.kind === 'stmt.if') {
                        out.write('elif ');
                    } else {
                        out.write('else')
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
                out.write(`import ${RUNTIME_LIB_NAME}`)
                
                if(stmt.emitChecks) {
                    // TODO: add code to check params are valid at runtime
                    const {libVersion} = stmt;
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
                if(stmt.opsUsed.includes('int_truediv') || stmt.opsUsed.includes('int_to_fraction')) {
                    out.beginLine();
                    out.write('from fractions import Fraction');
                }
            },
            'stmt.return': (out, stmt) => {
                out.beginLine();
                out.write('return');
                if(stmt.expr !== undefined) {
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
                for(const c of stmt.cases) {
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
                if(stmt.expr !== undefined) {
                    out.write(' ');
                    out.writeExpr(stmt.expr);
                }
            },
        };
        
        readonly EXPR_WRITE_FUNCS: ExprWriteFuncs<this> = {
            'expr.array.const': [Precedence.MAX, (out, expr) => {
                const {from, domainSize, rowLength} = expr;
                const bits = uintBits(domainSize);
                const s = arrayToHex(from, bits);
                const f = bits === 8 ? 'bytes.fromhex' : `${RUNTIME_LIB_NAME}.hex_to_u${bits}`;
                out.write(`${f}(`);
                out.writeLongStringLiteral(s, rowLength * s.length / from.length, '');
                out.write(')');
            }],
            'expr.array.new': [Precedence.MULT_DIV_MOD, (out, expr) => {
                const bits = uintBits(expr.domainSize);
                if(bits === 8) {
                    out.write(`bytearray(`);
                    out.writeExpr(expr.length);
                    out.write(')');
                } else {
                    // https://docs.python.org/3/library/array.html
                    out.write(`array.array("${bits === 16 ? 'H' : 'L'}", (0,)) * `);
                    out.writeExpr(expr.length, Precedence.MULT_DIV_MOD);
                }
            }],
            'expr.attr': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                out.writeExpr(expr.left, Precedence.ATTR_ACCESS_CALL);
                out.write(`.${expr.attr}`);
            }],
            'expr.dict': [Precedence.MAX, (out, expr) => {
                const {type: {keys}, values} = expr;
                out.write('{');
                out.writeList(i => {
                    out.write(`"${keys[i]}": `);
                    out.writeExpr(values[i]);
                }, keys.length, 1);
                out.write('}');
            }],
            'expr.letin': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                throw new Error();
            }],
            'expr.literal.bool': [Precedence.MAX, (out, expr) => {
                out.write(expr.value ? 'True' : 'False');
            }],
            'expr.literal.float': _literal,
            'expr.literal.int': _literal,
            'expr.literal.str': _literal,
            'expr.literal.null': [Precedence.MAX, (out, expr) => {
                out.write('None');
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
            'expr.op.call.lib.constructor': [Precedence.ATTR_ACCESS_CALL, (js, expr) => {
                js.write(`${RUNTIME_LIB_NAME}.${expr.className}`);
                js.write(`(`);
                js.writeExprList(expr.args);
                js.write(')');
            }],
            'expr.op.call.lib.function': [Precedence.ATTR_ACCESS_CALL, (js, expr) => {
                js.write(`${RUNTIME_LIB_NAME}.${expr.name}`);
                js.write(`(`);
                js.writeExprList(expr.args);
                js.write(')');
            }],
            'expr.op.call.lib.method': [Precedence.ATTR_ACCESS_CALL, (js, expr) => {
                js.writeExpr(expr.obj);
                js.write(`.${expr.name}(`);
                js.writeExprList(expr.args);
                js.write(')');
            }],
            'expr.op.call.local': [Precedence.ATTR_ACCESS_CALL, (out, expr) => {
                out.writeExpr(expr.name);
                out.write('(');
                out.writeExprList(expr.args);
                out.write(')');
            }],
            'expr.param': [Precedence.TERNARY, (out, expr) => {
                // TODO: take params as **kwargs
                out.write(`params['${expr.name}'] if params is not None and '${expr.name}' in params else `);
                out.writeExpr(expr.otherwise, Precedence.TERNARY);
            }],
            'expr.op.ternary': [Precedence.TERNARY, (out, expr) => {
                out.writeExpr(expr.then, Precedence.TERNARY + 1);
                out.write(' if ');
                out.writeExpr(expr.condition, Precedence.TERNARY + 1);
                out.write(' else ');
                out.writeExpr(expr.otherwise, Precedence.TERNARY);
            }],
        };
        
        readonly BINARY_OPS = (function(): BinaryOpSpecs {
            function _cmpOp(op: string): BinaryOpSpec {
                // Python's comparison ops are neither left- nor right-associative; need to avoid them chaining
                return infixOp(Precedence.CMP_EQ, op, Associativity.NEITHER);
            }
            function _intOp(p: Precedence, op: string): BinaryOpSpec {
                // all of these ops are left-associative
                return binaryOp(Precedence.ATTR_ACCESS_CALL, 'int32(', p, ` ${op} `, p + 1, ')');
            }
            function _func(name: 'Fraction'): BinaryOpSpec {
                return binaryOp(Precedence.ATTR_ACCESS_CALL, `${name}(`, Precedence.MIN, ', ', Precedence.MIN, ')');
            }
            
            // PLUS and MULT are not strictly right-associative for floats
            const PLUS = infixOp(Precedence.PLUS_MINUS, '+'),
                MINUS = infixOp(Precedence.PLUS_MINUS, '-'),
                MULT = infixOp(Precedence.MULT_DIV_MOD, '*'),
                DIV = infixOp(Precedence.MULT_DIV_MOD, '/'),
                FLOORDIV = infixOp(Precedence.MULT_DIV_MOD, '//'),
                MOD = infixOp(Precedence.MULT_DIV_MOD, '%'),
                EQ = _cmpOp('=='),
                NE = _cmpOp('!='),
                LT = _cmpOp('<'),
                LE = _cmpOp('<='),
                GT = _cmpOp('>'),
                GE = _cmpOp('>=');
            
            return {
                bool_and: infixOp(Precedence.BOOL_AND, 'and', Associativity.BOTH),
                bool_or: infixOp(Precedence.BOOL_OR, 'or', Associativity.BOTH),
                bool_eq: EQ,
                bool_ne: NE,
                
                // + and * are not strictly right-associative for floats
                float_plus: infixOp(Precedence.PLUS_MINUS, '+'),
                float_minus: MINUS,
                float_mult: infixOp(Precedence.MULT_DIV_MOD, '*'),
                float_truediv: infixOp(Precedence.MULT_DIV_MOD, '/'),
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
                
                int_plus: _intOp(Precedence.PLUS_MINUS, '+'),
                int_minus: _intOp(Precedence.PLUS_MINUS, '-'),
                int_mult: _intOp(Precedence.MULT_DIV_MOD, '*'),
                int_truediv: _func('Fraction'),
                int_floordiv: FLOORDIV,
                int_mod: MOD,
                int_eq: EQ,
                int_ne: NE,
                int_lt: LT,
                int_le: LE,
                int_gt: GT,
                int_ge: GE,
                int_and: infixOp(Precedence.BITWISE_AND, '&', Associativity.BOTH),
                int_or: infixOp(Precedence.BITWISE_OR, '|', Associativity.BOTH),
                int_xor: infixOp(Precedence.BITWISE_XOR, '^', Associativity.BOTH),
                int_lshift: infixOp(Precedence.BITWISE_SHIFT, '<<'),
                int_rshift: infixOp(Precedence.BITWISE_SHIFT, '>>'),
                
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
        
        readonly UNARY_OPS = (function(): UnaryOpSpecs {
            function _intOp(p: Precedence, op: string): UnaryOpSpec {
                return unaryOp(Precedence.ATTR_ACCESS_CALL, `int32(${op}`, p, ')');
            }
            function _func(name: 'float' | 'Fraction' | 'int_ctz' | 'str'): UnaryOpSpec {
                return unaryOp(Precedence.ATTR_ACCESS_CALL, `${name}(`, Precedence.MIN, ')');
            }
            
            const UMINUS = prefixOp(Precedence.UPLUS_UMINUS, '-'),
                TO_STR = _func('str');
            
            // 'checkzero' ops are NOOPs in Python; all of the relevant operations already raise errors for divzero
            return {
                bool_not: prefixOp(Precedence.BOOL_NOT, 'not '),
                
                float_uminus: UMINUS,
                float_checkzero: NOOP,
                
                fraction_uminus: UMINUS,
                fraction_checkzero: NOOP,
                
                int_uminus: _intOp(Precedence.UPLUS_UMINUS, '-'),
                int_checkzero: NOOP,
                int_not: prefixOp(Precedence.BITWISE_NOT, '~'),
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
        
        public writeAssignExpr(left: IR.NameExpr, right: IR.Expr): void {
            this.writeExpr(left);
            this.write(' := ');
            this.writeExpr(right, Precedence.ASSIGN);
        }
        
        writeParamDecl(name: string, type: IR.IRType): void {
            this.write(name);
            if(type.kind === 'nullable') { this.write('=None'); }
        }
        writeVarDecl(decl: IR.VarDecl): void {
            if(decl.initialiser === undefined) { return; }
            this.beginLine();
            this.writeExpr(decl.name);
            this.write(' = ');
            this.writeExpr(decl.initialiser);
        }
        writeReturnType(type: IR.IRType): void {}
    }
    
    export class PythonWithTypes extends Python {
        writeParamDecl(name: string, type: IR.IRType): void {
            this.write(name);
            this.write(': ');
            this.writeType(type);
            if(type.kind === 'nullable') { this.write(' = None'); }
        }
        writeVarDecl(decl: IR.VarDecl): void {
            this.beginLine();
            this.writeExpr(decl.name);
            this.write(': ');
            this.writeType(decl.type);
            if(decl.initialiser !== undefined) {
                this.write(' = ');
                this.writeExpr(decl.initialiser);
            }
        }
        writeReturnType(type: IR.IRType): void {
            this.write(' -> ');
            this.writeType(type);
        }
        
        private writeType(type: IR.IRType): void {
            switch(type.kind) {
                case 'dict': {
                    const {keys, values} = type;
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
    
    const _literal = [Precedence.MAX, (out: Python, expr: IR.FloatLiteralExpr | IR.IntLiteralExpr | IR.StrLiteralExpr) => {
        out.write(JSON.stringify(expr.value));
    }] as const;
}
