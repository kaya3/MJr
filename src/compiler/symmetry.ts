namespace Symmetry {
    export const SYMMETRY_GROUPS = (<K extends string>(groups: IRecord<K, readonly boolean[]>) => groups)({
        all: [true, true, true, true, true, true, true, true],
        none: [true, false, false, false, false, false, false, false],
        rot90: [true, true, true, true, false, false, false, false],
        rot180: [true, false, true, false, false, false, false, false],
        x: [true, false, false, false, false, false, true, false],
        y: [true, false, false, false, true, false, false, false],
        xy: [true, false, true, false, true, false, true, false],
    });
    export type SymmetryName = keyof typeof SYMMETRY_GROUPS
    
    export interface TransformMatrix extends Readonly<{
        a: number, b: number,
        c: number, d: number,
    }> {}
    
    const TRANSFORMS: readonly TransformMatrix[] = [
        {a: 1, b: 0, c: 0, d: 1}, // identity
        {a: 0, b: 1, c: -1, d: 0}, // rot90
        {a: -1, b: 0, c: 0, d: -1}, // rot180
        {a: 0, b: -1, c: 1, d: 0}, // rot270
        {a: 1, b: 0, c: 0, d: -1}, // flip_y
        {a: 0, b: 1, c: 1, d: 0}, // flip_xy
        {a: -1, b: 0, c: 0, d: 1}, // flip_x
        {a: 0, b: -1, c: -1, d: 0}, // flip_yx
    ];
    
    export function transformAll(p: Pattern, groupName: SymmetryName): Pattern[] {
        const group = SYMMETRY_GROUPS[groupName];
        const out: Pattern[] = [];
        for(let i = 0; i < group.length; ++i) {
            if(group[i]) {
                out.push(transform(p, TRANSFORMS[i]));
            }
        }
        return out;
    }
    
    /**
     * Applies a transformation matrix to a pattern.
     */
    export function transform(p: Pattern, m: TransformMatrix): Pattern {
        const {width, height, pattern, masks} = p;
        const newData: number[] = [];
        const newMasks: ISet[] = [];
        const newWidth = m.b === 0 ? width : height;
        const newHeight = m.b === 0 ? height : width;
        const xOffset = m.a < 0 || m.b < 0 ? newWidth - 1 : 0;
        const yOffset = m.c < 0 || m.d < 0 ? newHeight - 1 : 0;
        for(let y = 0; y < newHeight; ++y) {
            for(let x = 0; x < newWidth; ++x) {
                const px = m.a * x + m.b * y + xOffset;
                const py = m.c * x + m.d * y + yOffset;
                const index = px + width * py;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(newWidth, newHeight, newData, newMasks, p.hasUnions);
    }
    
    export function generate<T>(original: T, groupName: SymmetryName, rotate: (x: T) => T, reflect: (x: T) => T, keyFunc?: (x: T) => PrimitiveKey): T[] {
        const r1 = rotate(original),
            r2 = rotate(r1),
            r3 = rotate(r2),
            s0 = reflect(original),
            s1 = reflect(r1),
            s2 = reflect(r2),
            s3 = reflect(r3);
        
        const group = SYMMETRY_GROUPS[groupName];
        const out: T[] = [original, r1, r2, r3, s0, s1, s2, s3].filter((x, i) => group[i]);
        return keyFunc !== undefined ? IDMap.distinctByKey(out, keyFunc) : out;
    }
}
