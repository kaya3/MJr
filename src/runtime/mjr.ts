namespace MJr {
    // MJr runtime version 0 is unstable!
    export const VERSION = 0;
    
    export const DIV_ZERO_MESSAGE: string = 'division by zero';
    function checkZero(y: number): void {
        if(y === 0) { throw new Error(DIV_ZERO_MESSAGE); }
    }
    function modulo(x: number, y: number): number {
        return x - y * Math.floor(x / y);
    }
    
    export interface Fraction extends Readonly<{p: number, q: number}> {}
    export function fraction(p: number, q: number): Fraction {
        checkZero(q);
        // Euclid's algorithm
        let x = p, y = q;
        while(y !== 0) {
            x %= y;
            const tmp = x; x = y; y = tmp;
        }
        return {p: p / x, q: q / x};
    }
    
    export const OPS = {
        float_mod: modulo,
        float_checkzero: checkZero,
        
        fraction_plus: (x: Fraction, y: Fraction): Fraction => fraction(x.p * y.q + y.p * x.q, x.q * y.q),
        fraction_minus: (x: Fraction, y: Fraction): Fraction => fraction(x.p * y.q - y.p * x.q, x.q * y.q),
        fraction_mult: (x: Fraction, y: Fraction): Fraction => fraction(x.p * y.p, x.q * y.q),
        fraction_truediv: (x: Fraction, y: Fraction): Fraction => fraction(x.p * y.q, x.q * y.p),
        fraction_uminus: (x: Fraction): Fraction => ({p: -x.p, q: x.q}),
        fraction_eq: (x: Fraction, y: Fraction): boolean => x.p === y.p && x.q === y.q,
        fraction_ne: (x: Fraction, y: Fraction): boolean => x.p !== y.p || x.q !== y.q,
        fraction_lt: (x: Fraction, y: Fraction): boolean => x.p * y.q < y.p * x.q,
        fraction_le: (x: Fraction, y: Fraction): boolean => x.p * y.q <= y.p * x.q,
        fraction_gt: (x: Fraction, y: Fraction): boolean => x.p * y.q > y.p * x.q,
        fraction_ge: (x: Fraction, y: Fraction): boolean => x.p * y.q >= y.p * x.q,
        fraction_to_str: (x: Fraction): string => x.q === 1 ? `${x.p}` : `${x.p}/${x.q}`,
        
        int_truediv: fraction,
        int_floordiv: (x: number, y: number): number => Math.floor(x / y) | 0,
        int_mod: (x: number, y: number): number => modulo(x, y) | 0,
        int_checkzero: checkZero,
        int_to_fraction: (x: number): Fraction => ({p: x, q: 1}),
    };
    
    export const HEX = {
        u8(s: string): Uint8Array {
            const arr = new Uint8Array(s.length >> 1);
            for(let i = 0; i < s.length; i += 2) {
                arr[i >> 1] = parseInt(s.substring(i, i + 2), 16);
            }
            return arr;
        },
        u16(s: string): Uint16Array {
            const arr = new Uint16Array(s.length >> 2);
            for(let i = 0; i < s.length; i += 4) {
                arr[i >> 2] = parseInt(s.substring(i, i + 4), 16);
            }
            return arr;
        },
        u32(s: string): Uint32Array {
            const arr = new Uint32Array(s.length >> 3);
            for(let i = 0; i < s.length; i += 8) {
                arr[i >> 3] = parseInt(s.substring(i, i + 8), 16);
            }
            return arr;
        },
    };
    
    export interface PRNG {
        nextDouble(): number;
        nextInt(n: number): number;
    }
    export const DEFAULT_PRNG: PRNG = {
        nextDouble: Math.random,
        nextInt: n => (Math.random() * n) | 0,
    };
    
    export const SAMPLE_EMPTY_MESSAGE = 'sample from empty range';
    export function nextIntChecked(rng: PRNG, n: number): number {
        if(n <= 0) { throw new Error(SAMPLE_EMPTY_MESSAGE); }
        return rng.nextInt(n);
    }
    
    export class Grid {
        public constructor(
            public readonly width: number,
            public readonly height: number,
            public readonly data: Int8Array,
            public readonly alphabet: string,
        ) {}
        
        public index(x: number, y: number): number {
            if(x < 0 || x >= this.width || y < 0 || y >= this.height) {
                throw new Error(`position out of bounds: (${x}, ${y})`);
            }
            return x + y * this.width;
        }
        
        public wrapIndex(x: number, y: number): number {
            return OPS.int_mod(x, this.width) + OPS.int_mod(y, this.height) * this.width;
        }
        
        public toString(): string {
            const {width, data, alphabet} = this;
            const out: string[] = [];
            for(let i = 0; i < data.length; ++i) {
                if(i > 0 && i % width === 0) { out.push('\n'); }
                out.push(alphabet[data[i]]);
            }
            return out.join('');
        }
    }
    
    export class Pattern {
        protected readonly vectorData: readonly number[];
        public readonly minX: number;
        public readonly minY: number;
        public readonly effectiveWidth: number;
        public readonly effectiveHeight: number;
        
        public constructor(
            public readonly width: number,
            public readonly height: number,
            public readonly pattern: readonly number[],
        ) {
            let minX = width, minY = height, maxX = 0, maxY = 0;
            const v: number[] = [];
            
            for(let y = 0; y < height; ++y) {
                for(let x = 0; x < width; ++x) {
                    const c = pattern[x + width * y];
                    if(c < 0) { continue; }
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
        
        public fitsMask(grid: Grid, mask: Uint32Array, atX: number, atY: number): boolean {
            const v = this.vectorData;
            for(let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                if((mask[index >> 5] & 1 << (index & 31)) !== 0) { return false; }
            }
            return true;
        }
        
        public hasEffect(grid: Grid, atX: number, atY: number): boolean {
            const g = grid.data, v = this.vectorData;
            for(let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                if(g[index] !== v[i + 2]) { return true; }
            }
            return false;
        }
        
        public put(grid: Grid, mask: Uint32Array | undefined, atX: number, atY: number): void {
            const g = grid.data, v = this.vectorData;
            for(let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                g[index] = v[i + 2];
                if(mask !== undefined) { mask[index >> 5] |= 1 << (index & 31); }
            }
        }
    }
    
    export class RewriteInfo {
        public constructor(
            public readonly grid: Grid,
            public readonly x: number,
            public readonly y: number,
            public readonly width: number,
            public readonly height: number,
        ) {}
    }
    
    export class Sampler {
        public readonly arr: Uint32Array;
        private readonly indices: Uint32Array;
        public count: number = 0;
        
        public constructor(domainSize: number) {
            const arr = new Uint32Array(domainSize);
            const indices = new Uint32Array(domainSize);
            for(let i = 0; i < domainSize; ++i) {
                arr[i] = indices[i] = i;
            }
            this.arr = arr;
            this.indices = indices;
        }
        
        public copyInto(out: Uint32Array): void {
            const {arr, count} = this;
            for(let i = 0; i < count; ++i) {
                out[i] = arr[i];
            }
        }
        
        public shuffleInto(out: Uint32Array, rng: PRNG): void {
            const {arr, count} = this;
            for(let i = 0; i < count; ++i) {
                const j = rng.nextInt(i + 1);
                out[i] = out[j];
                out[j] = arr[i];
            }
        }
        
        public has(x: number): boolean {
            return this.indices[x] < this.count;
        }
        
        public add(x: number): void {
            const {arr, indices, count} = this;
            const i = indices[x];
            if(i >= count) {
                const j = count;
                const y = arr[j];
                arr[j] = x; indices[x] = j;
                arr[i] = y; indices[y] = i;
                ++this.count;
            }
        }
        
        public del(x: number): void {
            const {arr, indices, count} = this;
            const i = indices[x];
            if(i < count) {
                const j = count - 1;
                const y = arr[j];
                arr[j] = x; indices[x] = j;
                arr[i] = y; indices[y] = i;
                --this.count;
            }
        }
        
        public sample(max: number, rng: PRNG): number {
            const {arr, indices} = this;
            const i = rng.nextInt(max);
            const j = max - 1;
            const x = arr[i];
            const y = arr[j];
            arr[i] = y; indices[y] = i;
            arr[j] = x; indices[x] = j;
            return x;
        }
    }
}
