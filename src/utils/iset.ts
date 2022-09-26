/**
 * A set of natural numbers, represented using the bits of a typed array.
 */
interface ISet extends Omit<Uint32Array, Exclude<keyof unknown[], keyof readonly unknown[]>> {
    readonly [i: number]: number;
}

/**
 * A mutable set of natural numbers, represented using the bits of a typed array.
 */
type MutableISet = Uint32Array

/**
 * Helper functions for using a typed array as a set of natural numbers.
 * 
 * Aggregate operations `addAll`, `toArray` and `forEach` are O(N), where N is
 * the domain size; therefore they must not be used in the pattern matching loop.
 */
namespace ISet {
    /**
     * Creates an empty set, which can contain numbers `0 <= x < domainSize`.
     */
    export function empty(domainSize: number): MutableISet {
        return new Uint32Array((domainSize + 31) >> 5);
    }
    
    /**
     * Creates a set containing the whole domain `0 <= x < domainSize`.
     */
    export function full(domainSize: number): MutableISet {
        const set = empty(domainSize);
        set.fill(-1);
        if((domainSize & 31) !== 0) {
            set[set.length - 1] = (1 << (domainSize & 31)) - 1;
        }
        return set;
    }
    
    /**
     * Creates a set from an iterable of natural numbers, all of which must be
     * less than `domainSize`.
     */
    export function of(domainSize: number, xs: Iterable<number>): MutableISet {
        const set = empty(domainSize);
        for(const x of xs) { add(set, x); }
        return set;
    }
    
    /**
     * Indicates whether `set` contains the element `x`, in O(1) time.
     */
    export function has(set: ISet, x: number): boolean {
        return (set[x >> 5] & (1 << (x & 31))) !== 0;
    }
    
    /**
     * Returns the size of the set, in O(N) time.
     */
    export function size(set: ISet): number {
        let count = 0;
        for(let x of set) {
            while(x !== 0) {
                x &= x - 1;
                ++count;
            }
        }
        return count;
    }
    
    /**
     * Adds the element `x` to the set if it not already present, in O(1) time.
     */
    export function add(set: MutableISet, x: number): void {
        set[x >> 5] |= 1 << (x & 31);
    }
    
    /**
     * Adds all the members of the set `b` to the set `a`, in O(N) time.
     */
    export function addAll(a: MutableISet, b: ISet): void {
        if(a.length < b.length) { throw new Error(); }
        for(let i = 0; i < b.length; ++i) {
            a[i] |= b[i];
        }
    }
    
    /**
     * Removes all elements from the set.
     */
    export function clear(a: MutableISet): void {
        for(let i = 0; i < a.length; ++i) {
            a[i] = 0;
        }
    }
    
    /**
     * Determines whether the two sets are disjoint (i.e. they have no elements
     * in common).
     */
    export function isDisjoint(a: ISet, b: ISet): boolean {
        if(a.length < b.length) { throw new Error(); }
        for(let i = 0; i < b.length; ++i) {
            if((a[i] & b[i]) !== 0) { return false; }
        }
        return true;
    }
    
    /**
     * Converts an unordered array to a primitive type, suitable for use as a
     * Map key, in O(N) time.
     */
    export function arrayToKey(xs: readonly number[]): PrimitiveKey {
        if(xs.length === 0) { return 0n; }
        const domainSize = Math.max(...xs) + 1;
        return key(of(domainSize, xs));
    }
    
    function _toBigInt(set: ISet, a: number, b: number): bigint {
        if(a === b) {
            return 0n;
        } else if(a + 1 === b) {
            return BigInt(set[a]);
        } else {
            const halfRange = (b - a) >> 1;
            const mid = a + halfRange;
            return _toBigInt(set, a, mid) | _toBigInt(set, mid, b) << BigInt(halfRange << 5);
        }
    }
    
    /**
     * Converts a set to a primitive type, suitable for use as a Map key, in
     * O(N) time.
     */
    export function key(set: ISet): PrimitiveKey {
        if(set.length <= 4) {
            // O(N log N) time, but significantly faster for small domains, i.e. N <= 128
            return _toBigInt(set, 0, set.length);
        } else {
            // O(N) time
            return String.fromCharCode(...new Uint16Array(set.buffer));
        }
    }
    
    /**
     * Sentinel value used to halt the `_forEach` function.
     */
    const STOP_ITERATION = Symbol();
    
    function _forEach(set: ISet, f: (x: number) => unknown): boolean {
        for(let i = 0; i < set.length; ++i) {
            const x = i << 5;
            let setPart = set[i];
            while(setPart !== 0) {
                // position of the highest 1 bit
                const dx = 31 - Math.clz32(setPart);
                // 'x ^ dx' is equivalent to `x + dx` here
                if(f(x ^ dx) === STOP_ITERATION) { return false; }
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
    export const forEach: (set: ISet, f: (x: number) => void) => void = _forEach;
    
    /**
     * Returns a new array of the natural numbers in the given set, not
     * necessarily in order.
     */
    export function toArray(set: ISet): number[] {
        const arr: number[] = [];
        _forEach(set, x => arr.push(x));
        return arr;
    }
    
    /**
     * Determines whether the predicate is true for every element of the set.
     */
    export function every(set: ISet, f: (x: number) => boolean): boolean {
        return _forEach(set, x => !f(x) ? STOP_ITERATION : undefined);
    }
    
    /**
     * Determines whether the predicate is true for some element of the set.
     */
    export function some(set: ISet, f: (x: number) => boolean): boolean {
        return !_forEach(set, x => f(x) ? STOP_ITERATION : undefined);
    }
}
