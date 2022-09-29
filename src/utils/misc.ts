type Simplify<T> = T extends unknown ? {[K in keyof T]: T[K]} : never
type ValueOf<T> = T[keyof T]
type KeysMatching<T, V> = ValueOf<{[K in keyof T]: T[K] extends V ? K : never}> & string & keyof T
type IRecord<K extends PropertyKey, V> = {readonly [J in K]: V}
type Immutable<T> = {readonly [K in keyof T]:
    T[K] extends (infer U)[] ? readonly U[]
    : T[K] extends Map<infer K, infer V> ? ReadonlyMap<K, V>
    : Readonly<T[K]>
}

function fail(...args: unknown[]): never {
    console.log(args.length > 0 ? 'Assertion failed with arguments:' : 'Assertion failed', ...args);
    throw new Error('Internal compiler error: assertion failed');
}

function objHasKey<T extends object>(obj: T, key: PropertyKey): key is keyof T {
    return Object.prototype.hasOwnProperty.call(obj, key);
}

/**
 * Creates an empty array of length `n`, filled with the given value.
 */
function emptyArray<T>(n: number, value: T): T[] {
    return makeArray(n, () => value);
}

/**
 * Creates an array of length `n`, initialised using the given callback function.
 */
function makeArray<T>(n: number, f: (i: number) => T): T[] {
    // equivalent to `Array(n).map((_, i) => f(i))`, but guarantees an array without holes, which may be more performant to use
    const arr: T[] = [];
    for(let i = 0; i < n; ++i) { arr.push(f(i)); }
    return arr;
}

function withNextID<T extends {id: number}>(arr: T[], obj: Omit<T, 'id'>): T {
    const t = obj as T;
    t.id = arr.length;
    arr.push(t);
    return t;
}

function getOrCompute<K extends PrimitiveKey, V>(map: Map<K, V>, key: K, f: () => V): V {
    let v = map.get(key);
    if(v === undefined) { map.set(key, v = f()); }
    return v;
} 

function quoteJoin(hints: string[], delimiter: string = ', '): string {
    return hints.map(s => `'${s}'`).join(delimiter);
}
