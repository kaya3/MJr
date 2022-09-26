type PrimitiveKey = string | number | bigint

interface ReadonlyIDMap<T> {
    size(): number;
    has(x: T): boolean;
    getID(x: T): number;
    getIDOrDefault(x: T): number;
    getByID(id: number): T;
    forEach(f: (x: T, id: number) => void): void;
    filter(f: (x: T) => boolean): T[];
    map<S>(f: (x: T, id: number) => S): S[];
}

/**
 * Assigns unique, incremental IDs to a set of values.
 */
class IDMap<T> implements ReadonlyIDMap<T> {
    private static readonly IDENTITY = <T>(x: T): T => x;
    
    public static empty<T extends PrimitiveKey>(): IDMap<T> {
        return new IDMap(IDMap.IDENTITY);
    }
    
    public static withKey<T>(keyFunc: (x: T) => PrimitiveKey): IDMap<T> {
        return new IDMap(keyFunc);
    }
    
    /**
     * Creates a new IDMap with the distinct elements from `iterable`, with IDs
     * in order of first occurrence.
     */
    public static of<T extends PrimitiveKey>(iterable: Iterable<T>): IDMap<T> {
        return IDMap.ofWithKey(iterable, IDMap.IDENTITY);
    }
    
    public static ofWithKey<T>(iterable: Iterable<T>, keyFunc: (x: T) => PrimitiveKey): IDMap<T> {
        const map = new IDMap(keyFunc);
        for(const x of iterable) { map.getOrCreateID(x); }
        return map;
    }
    
    /**
     * Returns a new array of the distinct elements from `iterable`, in order
     * of first occurrence.
     */
    public static distinct<T extends PrimitiveKey>(iterable: Iterable<T>): T[] {
        return IDMap.of(iterable).arr;
    }
    
    /**
     * Returns a new array of the elements from `iterable`, deduplicated using
     * the given key function, in order of first occurrence. If multiple values
     * have the same key, only the first is included.
     */
    public static distinctByKey<T>(iterable: Iterable<T>, keyFunc: (x: T) => PrimitiveKey): T[] {
        return IDMap.ofWithKey(iterable, keyFunc).arr;
    }
    
    /**
     * The distinct elements in this map, in insertion order.
     */
    private readonly arr: T[] = [];
    
    /**
     * Maps elements to their indices in `arr`.
     * 
     * Invariant: `ids.get(keyFunc(x)) === i` if and only if `arr[i] === x`
     */
    private readonly ids = new Map<PrimitiveKey, number>();
    
    private constructor(private readonly keyFunc: (x: T) => PrimitiveKey) {}
    
    /**
     * Returns the number of elements in the map.
     */
    public size(): number {
        return this.arr.length;
    }
    
    /**
     * Adds an element to the map if it is not already present, and returns the
     * element's ID, in O(1) time.
     */
    public getOrCreateID(x: T): number {
        const key = this.keyFunc(x);
        return getOrCompute(this.ids, key, () => {
            const id = this.arr.length;
            this.arr.push(x);
            return id;
        });
    }
    
    /**
     * Indicates whether the given element is associated with an ID, in O(1)
     * time.
     */
    public has(x: T): boolean {
        return this.ids.has(this.keyFunc(x));
    }
    
    /**
     * Returns the ID of the given element, in O(1) time. An error is thrown if
     * the element is not associated with an ID.
     */
    public getID(x: T): number {
        const id = this.ids.get(this.keyFunc(x));
        if(id === undefined) { throw new Error(); }
        return id;
    }
    
    /**
     * Returns the ID of the given element, or -1 if the given element is not
     * associated with an ID, in O(1) time.
     */
    public getIDOrDefault(x: T): number {
        return this.ids.get(this.keyFunc(x)) ?? -1;
    }
    
    /**
     * Returns the element associated with the given ID, in O(1) time. An error
     * is thrown if there is no element with the given ID.
     */
    public getByID(id: number): T {
        if(id < 0 || id >= this.arr.length) { throw new Error(); }
        return this.arr[id];
    }
    
    public forEach(f: (x: T, id: number) => void): void {
        this.arr.forEach(f);
    }
    public filter(f: (x: T) => boolean): T[] {
        return this.arr.filter(f);
    }
    public map<S>(f: (x: T, id: number) => S): S[] {
        return this.arr.map(f);
    }
}
