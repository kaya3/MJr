///<reference path="../runtime/mjr.ts"/>

class Pattern extends MJr.Pattern {
    public static rowsOf(p: Pattern): readonly Pattern[] {
        const {width, height, pattern, masks} = p;
        const out: Pattern[] = []
        const n = width * height;
        for(let offset = 0; offset < n; offset += width) {
            const row = pattern.slice(offset, offset + width);
            const rowMasks = masks.slice(offset, offset + width);
            out.push(new Pattern(width, 1, row, rowMasks, p.hasUnions));
        }
        return out;
    }
    
    public static rotate(p: Pattern): Pattern {
        const {width, height, pattern, masks} = p;
        const newData: number[] = [];
        const newMasks: ISet[] = [];
        for(let x = 0; x < width; ++x) {
            for(let y = height - 1; y >= 0; --y) {
                const index = x + width * y;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(height, width, newData, newMasks, p.hasUnions);
    }
    
    public static reflect(p: Pattern): Pattern {
        const {width, height, pattern, masks} = p;
        const newData: number[] = [];
        const newMasks: ISet[] = [];
        for(let y = height - 1; y >= 0; --y) {
            for(let x = 0; x < width; ++x) {
                const index = x + width * y;
                newData.push(pattern[index]);
                newMasks.push(masks[index]);
            }
        }
        return new Pattern(width, height, newData, newMasks, p.hasUnions);
    }
    
    public static key(p: Pattern): string {
        return p._key ??= `${p.width}x${p.height}:${p.masks.map(mask => Array.from(mask).join(',')).join(';')}`;
    }
    
    public _key: string | undefined = undefined;
    
    public constructor(
        width: number,
        height: number,
        pattern: readonly number[],
        public readonly masks: readonly ISet[],
        public readonly hasUnions: boolean,
    ) {
        super(width, height, pattern);
    }
    
    public forEach(f: (dx: number, dy: number, c: number) => void): void {
        const v = this.vectorData;
        for(let i = 0; i < v.length; i += 3) {
            f(v[i], v[i + 1], v[i + 2]);
        }
    }
    
    public map<T>(f: (dx: number, dy: number, c: number) => T): T[] {
        const out: T[] = [];
        this.forEach((dx, dy, c) => out.push(f(dx, dy, c)));
        return out;
    }
    
    public every(f: (dx: number, dy: number, c: number) => boolean): boolean {
        const v = this.vectorData;
        for(let i = 0; i < v.length; i += 3) {
            if(!f(v[i], v[i + 1], v[i + 2])) { return false; }
        }
        return true;
    }
    
    public some(f: (dx: number, dy: number, c: number) => boolean): boolean {
        return !this.every((dx, dy, c) => !f(dx, dy, c));
    }
}
