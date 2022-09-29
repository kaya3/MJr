namespace IR {
    export type IRType = Readonly<
        | {kind: Type.PrimitiveKind | 'byte' | 'pattern' | 'prng' | 'rewriteinfo' | 'sampler' | 'void'}
        | ConstArrayType
        | MutableArrayType
        | DictType
        | {kind: 'nullable', componentType: IRType}
    >
    export interface ConstArrayType extends Readonly<{kind: 'array.const', domainSize: number}> {}
    export interface MutableArrayType extends Readonly<{kind: 'array.mutable', domainSize: number}> {}
    export interface DictType extends Readonly<{kind: 'dict', keys: readonly string[], values: readonly IRType[]}> {}
    
    export const BOOL_TYPE: IRType = {kind: 'bool'};
    export const BYTE_TYPE: IRType = {kind: 'byte'};
    export const FLOAT_TYPE: IRType = {kind: 'float'};
    export const FRACTION_TYPE: IRType = {kind: 'fraction'};
    export const GRID_TYPE: IRType = {kind: 'grid'};
    export const INT_TYPE: IRType = {kind: 'int'};
    export const PATTERN_TYPE: IRType = {kind: 'pattern'};
    export const PRNG_TYPE: IRType = {kind: 'prng'};
    export const REWRITE_INFO_TYPE: IRType = {kind: 'rewriteinfo'};
    export const SAMPLER_TYPE: IRType = {kind: 'sampler'};
    export const STR_TYPE: IRType = {kind: 'str'};
    export const VOID_TYPE: IRType = {kind: 'void'};
    
    export const GRID_DATA_ARRAY_TYPE = mutableArrayType(128);
    export const INT32_ARRAY_TYPE = mutableArrayType(2 ** 32);
    
    export function mutableArrayType(domainSize: number): MutableArrayType {
        return {kind: 'array.mutable', domainSize};
    }
    
    export function constArrayType(domainSize: number): ConstArrayType {
        return {kind: 'array.const', domainSize};
    }
    
    export function nullableType(componentType: IRType): IRType {
        return {kind: 'nullable', componentType};
    }
}
