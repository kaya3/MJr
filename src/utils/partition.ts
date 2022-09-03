type PartitionSubset = {
    /**
     * The index at which this subset occurs in the `Partition.subsets` array.
     * Used to delete subsets from that array in O(1) time.
     */
    index: number,
    /**
     * The start of the index range in `Partition.arr` for this subset (inclusive).
     */
    start: number,
    /**
     * The end of the index range in `Partition.arr` for this subset (exclusive).
     */
    end: number,
    /**
     * Indicates whether this subset needs to be processed by the `DFA.minimise`
     * algorithm.
     */
    isUnprocessed: boolean,
    /**
     * In the `Partition.refine` algorithm when this set is being split, the
     * sibling is the set which occurs immediately after this set in `arr`.
     */
    sibling: PartitionSubset | undefined,
}

/**
 * Data structure representing a partition of the natural numbers from 0 to n - 1,
 * for use in the `DFA.minimise` algorithm. The main operations are `refine` and
 * `pollUnprocessed`.
 * 
 * https://en.wikipedia.org/wiki/Partition_refinement#Data_structure
 */
class Partition {
    /**
     * The numbers from 0 to n - 1, ordered so that each subset in the partition
     * is a contiguous range.
     * 
     * Invariant: `arr` is a permutation of the numbers from 0 to n - 1
     */
    private readonly arr: number[];
    
    /**
     * Maps the numbers from 0 to n - 1 to their indices in `arr`.
     * 
     * Invariant: `arr[i] === x` if and only if `indices[x] === i`
     */
    private readonly indices: number[];
    
    /**
     * The boundaries in `arr` for each subset in the partition.
     * 
     * Invariant: `subsets[i].index === i`
     * Invariant: `subsets[i].start < subsets[i].end`
     * Invariant: `subsets[i].start === 0` or there is a unique `j` such that `subsets[i].start === subsets[j].end`
     * Invariant: `subsets[i].end === n` or there is a unique `j` such that `subsets[i].end === subsets[j].start`
     */
    private readonly subsets: PartitionSubset[] = [];
    
    /**
     * The subsets which have yet to be processed by the `DFA.minimise` algorithm,
     * plus possibly some empty subsets which do not need to be processed.
     * 
     * Invariant: if `subset.isUnprocessed` then `unprocessed.includes(subset)`
     * Invariant: if `unprocessed.includes(subset)` and not `subset.isUnprocessed`, then `subset.start === subset.end`
     */
    private readonly unprocessed: PartitionSubset[] = [];
    
    /**
     * Maps each number from 0 to n - 1 to the subset it is a member of.
     * 
     * Invariant: `map[x].start <= indices[x] && indices[x] < map[x].end`
     */
    private readonly map: PartitionSubset[];
    
    /**
     * Constructs a new instance representing a partition of the numbers from
     * 0 to n - 1. The partition initially contains only a single subset (the
     * whole range).
     */
    public constructor(n: number) {
        this.arr = makeArray(n, i => i);
        this.indices = makeArray(n, i => i);
        
        const initialSubset = this.makeSubset(0, n, true);
        this.map = emptyArray(n, initialSubset);
    }
    
    /**
     * Returns the number of subsets in this partition.
     */
    public countSubsets(): number {
        return this.subsets.length;
    }
    
    private makeSubset(start: number, end: number, isUnprocessed: boolean): PartitionSubset {
        const {subsets} = this;
        const subset: PartitionSubset = {
            index: subsets.length,
            start,
            end,
            isUnprocessed,
            sibling: undefined,
        };
        subsets.push(subset);
        if(isUnprocessed) { this.unprocessed.push(subset); }
        return subset;
    }
    
    private deleteSubset(subset: PartitionSubset): void {
        // sanity check
        if(subset.start !== subset.end) { throw new Error(); }
        
        const {index} = subset;
        const removed = this.subsets.pop()!;
        if(removed.index !== index) {
            this.subsets[removed.index = index] = removed;
        }
        subset.isUnprocessed = false;
    }
    
    /**
     * Returns a subset which needs to be processed, and marks it as processed.
     * The elements are in no particular order.
     * 
     * If no subsets remain to be processed, `undefined` is returned.
     */
    public pollUnprocessed(): readonly number[] | undefined {
        const {unprocessed} = this;
        while(unprocessed.length > 0) {
            const subset = unprocessed.pop()!;
            // have to check `isUnprocessed` because deleted subsets may still be in the stack
            if(subset.isUnprocessed) {
                subset.isUnprocessed = false;
                return this.arr.slice(subset.start, subset.end);
            }
        }
        return undefined;
    }
    
    /**
     * Returns a representative element from the subset in the partition which
     * contains the number `x`.
     */
    public getRepresentative(x: number): number {
        return this.arr[this.map[x].start];
    }
    
    /**
     * Calls the provided callback function with a representative element
     * from each subset in the partition.
     */
    public forEachRepresentative(f: (x: number) => void): void {
        const {arr} = this;
        for(const subset of this.subsets) {
            f(arr[subset.start]);
        }
    }
    
    /**
     * Refines this partition by splitting any subsets which partly intersect
     * with the given set. If an unprocessed subset is split, both parts are
     * marked unprocessed; otherwise, the smaller part is marked.
     */
    public refine(set: ISet): void {
        const {unprocessed, map} = this;
        const splits: PartitionSubset[] = [];
        ISet.forEach(set, x => {
            const subset = map[x];
            if(subset.sibling === undefined) {
                splits.push(subset);
                subset.sibling = this.makeSubset(subset.end, subset.end, subset.isUnprocessed);
            }
            this.moveToSibling(x, subset);
        });
        
        for(const subset of splits) {
            if(subset.start === subset.end) {
                this.deleteSubset(subset);
            } else if(!subset.isUnprocessed) {
                const sibling = subset.sibling!;
                const min = subset.end - subset.start <= sibling.end - sibling.start ? subset : sibling;
                min.isUnprocessed = true;
                unprocessed.push(min);
            }
            subset.sibling = undefined;
        }
    }
    
    /**
     * Moves the element x from `subset` to `subset.sibling`, in O(1) time. The
     * sibling appears immediately afterwards in `arr`, so `x` is swapped with
     * the last member of `subset` and then the boundary is adjusted.
     */
    private moveToSibling(x: number, subset: PartitionSubset): void {
        const {arr, map, indices} = this;
        const sibling = subset.sibling!;
        
        const i = indices[x];
        const j = subset.end = --sibling.start;
        
        const y = arr[j];
        arr[i] = y; indices[y] = i;
        arr[j] = x; indices[x] = j;
        
        map[x] = sibling;
    }
}
