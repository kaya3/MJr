namespace Type {
    export type Type = Readonly<
        | {kind: 'bool'}
        | {kind: 'dict', entryTypes: ReadonlyMap<string, Type>}
        | {kind: 'float'}
        | {kind: 'fraction'}
        | {kind: 'grid'}
        | {kind: 'int'}
        | {kind: 'pattern', alphabetKey: string, hasUnions: boolean, width: number, height: number}
        | {kind: 'position', inGrid: number}
        | {kind: 'str'}
    >
    
    export type Kind = Type['kind']
    export type PrimitiveKind = ValueOf<{[K in Kind]: {kind: K} extends Type ? K : never}>
    export type OfKind<K extends Kind> = Extract<Type, {kind: K}>
    
    export type Value<K extends Kind = Kind> = _ValueMap[K]
    type _ValueMap = {
        bool: boolean,
        dict: ReadonlyMap<string, ConstantValue>,
        float: number,
        fraction: MJr.Fraction,
        grid: number,
        int: number,
        pattern: Pattern,
        position: Readonly<{x: number, y: number, inGrid: number}>,
        str: string,
    }
    
    export type ConstantValue<K extends Kind = Kind> = K extends unknown ? Readonly<{kind: K, type: OfKind<K>, value: Value<K>}> : never
    
    export type InternalType = Type | Readonly<
        | {kind: 'any_dict'}
        | {kind: 'any_pattern', alphabetKey: string, allowUnions: boolean}
        | {kind: 'any_position'}
        | {kind: 'union', options: readonly InternalType[]}
    >
    
    export const BOOL: OfKind<'bool'> = {kind: 'bool'};
    export const FLOAT: OfKind<'float'> = {kind: 'float'};
    export const FRACTION: OfKind<'fraction'> = {kind: 'fraction'};
    export const GRID: OfKind<'grid'> = {kind: 'grid'};
    export const INT: OfKind<'int'> = {kind: 'int'};
    export const STR: OfKind<'str'> = {kind: 'str'};
    
    export const PRIMITIVES: {readonly [K in PrimitiveKind]: OfKind<K>} = {
        bool: BOOL,
        float: FLOAT,
        fraction: FRACTION,
        grid: GRID,
        int: INT,
        str: STR,
    };
    
    export const ANY_DICT: InternalType = {kind: 'any_dict'};
    export const ANY_POSITION: InternalType = {kind: 'any_position'};
    export const OBJECT: InternalType = {kind: 'union', options: [ANY_DICT, ANY_POSITION, GRID]};
    export const NUMERIC: InternalType = {kind: 'union', options: [FLOAT, FRACTION, INT]};
    
    export type GridAttribute = 'width' | 'height'
    export const GRID_ATTRS = new Map<GridAttribute, Type>([
        ['width', INT],
        ['height', INT],
    ]);
    export type PositionAttribute = 'x' | 'y'
    export const POSITION_ATTRS = new Map<PositionAttribute, Type>([
        ['x', INT],
        ['y', INT],
    ]);
    
    export function toStr(type: InternalType): string {
        switch(type.kind) {
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
            case 'pattern':
                return `pattern.${type.hasUnions ? 'in' : 'out'}.${type.width}x${type.height}[${type.alphabetKey}]`;
            case 'position':
                return `position.grid${type.inGrid}`;
        }
    }
    
    export function equals(t1: Type, t2: Type): boolean {
        switch(t1.kind) {
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
            
            case 'pattern':
                return t2.kind === 'pattern' && t1.alphabetKey === t2.alphabetKey && t1.hasUnions === t2.hasUnions
                    && t1.width === t2.width && t1.height === t2.height;
            
            case 'position':
                return t2.kind === 'position' && t1.inGrid === t2.inGrid;
        }
    }
    
    export function isSubtype(t1: Type, t2: InternalType): boolean {
        switch(t2.kind) {
            case 'any_dict':
                return t1.kind === 'dict';
            case 'any_pattern':
                return t1.kind === 'pattern' && t1.alphabetKey === t2.alphabetKey;
            case 'any_position':
                return t1.kind === 'position';
            case 'union':
                return t2.options.some(t => isSubtype(t1, t));
            
            case 'dict':
                return t1.kind === 'dict' && t1.entryTypes.size === t2.entryTypes.size && [...t1.entryTypes].every(([k, v1]) => {
                    const v2 = t2.entryTypes.get(k);
                    return v2 !== undefined && isSubtype(v1, v2);
                });
            case 'pattern':
                return t1.kind === 'pattern' && t1.alphabetKey === t2.alphabetKey
                    && t1.width === t2.width && t1.height === t2.height
                    && (!t1.hasUnions || t2.hasUnions);
            
            default:
                return equals(t1, t2);
        }
    }
    
    export function leastUpperBound(t1: Type, t2: Type): Type | undefined {
        return isSubtype(t1, t2) ? t2
            : isSubtype(t2, t1) ? t1
            : undefined;
    }
}
