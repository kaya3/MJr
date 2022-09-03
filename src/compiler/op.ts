///<reference path="../runtime/mjr.ts"/>

namespace Op {
    export type UnaryOp =
        | 'bool_not'
        | 'float_uminus' | 'float_checkzero'
        | 'fraction_uminus' | 'fraction_checkzero'
        | 'int_uminus' | 'int_checkzero' | 'int_to_float' | 'int_to_fraction'
        | 'bool_to_str' /*| 'dict_to_str'*/ | 'float_to_str' | 'fraction_to_str' | 'grid_to_str' | 'int_to_str' /*| 'pattern_to_str'*/
    export type BinaryOp =
        | 'bool_and' | 'bool_or' | 'bool_eq' | 'bool_ne'
        | 'float_plus' | 'float_minus' | 'float_mult' | 'float_truediv' | 'float_mod' | 'float_eq' | 'float_ne' | 'float_lt' | 'float_le' | 'float_gt' | 'float_ge'
        | 'fraction_plus' | 'fraction_minus' | 'fraction_mult' | 'fraction_truediv' | 'fraction_eq' | 'fraction_ne' | 'fraction_lt' | 'fraction_le' | 'fraction_gt' | 'fraction_ge'
        | 'int_plus' | 'int_minus' | 'int_mult' | 'int_truediv' | 'int_floordiv' | 'int_mod' | 'int_eq' | 'int_ne' | 'int_lt' | 'int_le' | 'int_gt' | 'int_ge'
        | 'str_concat' | 'str_eq' | 'str_ne'
    
    export type UnaryFunc<K extends Type.Kind = Type.Kind> = (x: Type.Value<K>) => Type.Value | undefined
    export type BinaryFunc<K extends Type.Kind = Type.Kind> = (x: Type.Value<K>, y: Type.Value<K>) => Type.Value | undefined
    
    export const UNARY_FUNCS: {readonly [K in UnaryOp]?: K extends `${infer J extends Type.Kind}_${string}` ? UnaryFunc<J> : never} = {
        bool_not: x => !x,
        
        float_uminus: x => -x,
        
        fraction_uminus: x => ({p: -x.p, q: x.q}),
        
        int_uminus: x => (-x) | 0,
        int_to_float: x => +x,
        int_to_fraction: MJr.OPS.int_to_fraction,
        
        bool_to_str: x => x.toString(),
        float_to_str: x => x.toString(),
        fraction_to_str: MJr.OPS.fraction_to_str,
        int_to_str: x => x.toString(),
    };
    
    export const BINARY_FUNCS = (<T extends {readonly [K in BinaryOp]?: K extends `${infer T extends Type.Kind}_${string}` ? BinaryFunc<T> : never}>(funcs: T) => funcs)({
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
    
    type UnaryOpTypeSpec = readonly (readonly [inType: Type.PrimitiveKind, outType: Type.PrimitiveKind, op: UnaryOp])[]
    type BinaryOpTypeSpec = readonly (readonly [leftType: Type.PrimitiveKind, rightType: Type.PrimitiveKind, outType: Type.PrimitiveKind, op: BinaryOp])[]
    
    export const UNARY_OP_TYPES: IRecord<'-' | 'not', UnaryOpTypeSpec> = {
        '-': [['int', 'int', 'int_uminus'], ['float', 'float', 'float_uminus']],
        'not': [['bool', 'bool', 'bool_not']],
    };
    export const BINARY_OP_TYPES: IRecord<AST.BinaryOp, BinaryOpTypeSpec> = {
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
}
