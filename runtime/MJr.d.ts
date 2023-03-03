declare namespace MJr {
    export const VERSION = 0;
    export const DIV_ZERO_MESSAGE: string;
    function checkZero(y: number): number;
    function modulo(x: number, y: number): number;
    export interface Fraction extends Readonly<{
        p: number;
        q: number;
    }> {
    }
    export function fraction(p: number, q: number): Fraction;
    export const OPS: {
        float_mod: typeof modulo;
        float_checkzero: typeof checkZero;
        fraction_plus: (x: Fraction, y: Fraction) => Fraction;
        fraction_minus: (x: Fraction, y: Fraction) => Fraction;
        fraction_mult: (x: Fraction, y: Fraction) => Fraction;
        fraction_truediv: (x: Fraction, y: Fraction) => Fraction;
        fraction_uminus: (x: Fraction) => Fraction;
        fraction_eq: (x: Fraction, y: Fraction) => boolean;
        fraction_ne: (x: Fraction, y: Fraction) => boolean;
        fraction_lt: (x: Fraction, y: Fraction) => boolean;
        fraction_le: (x: Fraction, y: Fraction) => boolean;
        fraction_gt: (x: Fraction, y: Fraction) => boolean;
        fraction_ge: (x: Fraction, y: Fraction) => boolean;
        fraction_to_str: (x: Fraction) => string;
        int_truediv: typeof fraction;
        int_floordiv: (x: number, y: number) => number;
        int_mod: (x: number, y: number) => number;
        int_ctz: (x: number) => number;
        int_checkzero: typeof checkZero;
        int_to_fraction: (x: number) => Fraction;
    };
    export const HEX: {
        u4(s: string): Uint8Array;
        u8(s: string): Uint8Array;
        u12(s: string): Uint16Array;
        u16(s: string): Uint16Array;
        u20(s: string): Uint32Array;
        u24(s: string): Uint32Array;
        u28(s: string): Uint32Array;
        u32(s: string): Uint32Array;
    };
    export interface PRNG {
        nextDouble(): number;
        nextInt(n: number): number;
    }
    export const DEFAULT_PRNG: PRNG;
    export const SAMPLE_EMPTY_MESSAGE = "sample from empty range";
    export function nextIntChecked(rng: PRNG, n: number): number;
    export function lfsrFeedbackTerm(n: number): number;
    export class Grid {
        readonly width: number;
        readonly height: number;
        readonly data: Int8Array;
        readonly alphabet: string;
        constructor(width: number, height: number, data: Int8Array, alphabet: string);
        index(x: number, y: number): number;
        wrapIndex(x: number, y: number): number;
        toString(): string;
    }
    export class Pattern {
        readonly width: number;
        readonly height: number;
        readonly pattern: readonly number[];
        protected readonly vectorData: readonly number[];
        readonly minX: number;
        readonly minY: number;
        readonly effectiveWidth: number;
        readonly effectiveHeight: number;
        constructor(width: number, height: number, pattern: readonly number[]);
        fitsMask(grid: Grid, mask: Uint32Array, atX: number, atY: number): boolean;
        hasEffect(grid: Grid, atX: number, atY: number): boolean;
        put(grid: Grid, mask: Uint32Array | undefined, atX: number, atY: number): void;
    }
    export class RewriteInfo {
        readonly grid: Grid;
        readonly x: number;
        readonly y: number;
        readonly width: number;
        readonly height: number;
        constructor(grid: Grid, x: number, y: number, width: number, height: number);
    }
    export class Sampler {
        readonly arr: Uint32Array;
        private readonly indices;
        count: number;
        constructor(domainSize: number);
        copyInto(out: Uint32Array): void;
        copyIntoOffset(out: Uint32Array, offset: number, m: number, c: number): void;
        shuffleInto(out: Uint32Array, rng: PRNG): void;
        shuffleIntoOffset(out: Uint32Array, offset: number, m: number, c: number, rng: PRNG): void;
        has(x: number): boolean;
        add(x: number): void;
        del(x: number): void;
        sample(max: number, rng: PRNG): number;
    }
    export {};
}
