///<reference path="../runtime/mjr.ts"/>

type PatternTree = PatternTree.Node

namespace PatternTree {
    export type Kind = Node['kind']
    export type Node =
        | Pattern
        | _Node<'top' | 'bottom', {}>
        | _Node<'and' | 'or', {left: OfKind<'and' | 'or' | 'not' | 'leaf'>, right: OfKind<'and' | 'or' | 'not' | 'leaf'>}>
        | _Node<'not', {child: Pattern}>
    
    type _Node<K extends string, T> = K extends unknown ? Readonly<{kind: K} & T & {width: number, height: number}> & {_key: string | undefined} : never
    
    type OfKind<K extends Kind> = Extract<Node, {kind: K}>
    export type LeafOrTop = Pattern | OfKind<'top'>
    
    /**
     * Returns the intersection of two patterns, or `undefined` if they are
     * mutually exclusive.
     */
    function _intersect(left: Pattern, right: Pattern): Pattern | undefined {
        const {alphabetKey, masks: leftMasks} = left;
        
        const pattern: number[] = [];
        const masks: ISet[] = [];
        for(let i = 0; i < leftMasks.length; ++i) {
            const mask = ISet.intersection(leftMasks[i], right.masks[i]);
            const maskSize = ISet.size(mask);
            if(maskSize === 0) { return undefined; }
            
            pattern.push(
                maskSize === 1 ? ISet.toArray(mask)[0]
                : maskSize === alphabetKey.length ? -1
                : -2
            );
            masks.push(mask);
        }
        
        return new Pattern(left.width, left.height, alphabetKey, pattern, masks, true);
    }
    
    /** 
     * Conservatively estimates of whether, if `left` matches, `right` must too.
     */
    function _entails(left: Node, right: Pattern): boolean {
        switch(left.kind) {
            case 'leaf':
                return left.masks.every((mask, i) => ISet.isSubset(mask, right.masks[i]));
            case 'top':
                return false;
            case 'bottom':
                return true;
            case 'and':
                return _entails(left.left, right) || _entails(left.right, right);
            case 'or':
                return _entails(left.left, right) && _entails(left.right, right);
            case 'not':
                return false;
        }
    }
    
    export function top(width: number, height: number): OfKind<'top'> {
        return {kind: 'top', width, height, _key: undefined};
    }
    
    export function bottom(width: number, height: number): OfKind<'bottom'> {
        return {kind: 'bottom', width, height, _key: undefined};
    }
    
    export function and(left: PatternTree, right: PatternTree): PatternTree {
        const {width, height} = left;
        if(right.width !== width || right.height !== height) { fail(); }
        
        if(left.kind === 'bottom' || right.kind === 'top') {
            return left;
        } else if(left.kind === 'top' || right.kind === 'bottom') {
            return right;
        } else if(left.kind === 'leaf' && right.kind === 'leaf') {
            return _intersect(left, right) ?? bottom(width, height);
        } else if(left.kind === 'leaf' && right.kind === 'not' && _intersect(left, right.child) === undefined) {
            return left;
        } else if(right.kind === 'leaf' && left.kind === 'not' && _intersect(left.child, right) === undefined) {
            return right;
        } else if((left.kind === 'not' && _entails(right, left.child)) || (right.kind === 'not' && _entails(left, right.child))) {
            return bottom(width, height);
        }
        
        return {kind: 'and', left, right, width, height, _key: undefined};
    }
    
    export function or(left: PatternTree, right: PatternTree): PatternTree {
        const {width, height} = left;
        if(right.width !== width || right.height !== height) { fail(); }
        
        if(left.kind === 'top' || right.kind === 'bottom') {
            return left;
        } else if(left.kind === 'bottom' || right.kind === 'top') {
            return right;
        } else if(left.kind === 'leaf' && right.kind === 'leaf' && width === 1 && height === 1) {
            const {alphabetKey} = left;
            const mask = ISet.union(left.masks[0], right.masks[0]);
            return ISet.size(mask) === alphabetKey.length
                ? top(1, 1)
                : new Pattern(1, 1, alphabetKey, [-2], [mask], true);
        }
        
        return {kind: 'or', left, right, width, height, _key: undefined};
    }
    
    export function not(child: PatternTree): PatternTree {
        const {width, height} = child;
        switch(child.kind) {
            case 'top':
                return bottom(width, height);
            case 'bottom':
                return top(width, height);
            case 'and':
                return or(not(child.left), not(child.right));
            case 'or':
                return and(not(child.left), not(child.right));
            case 'not':
                return child.child;
        }
        
        // leaf
        if(width === 1 && height === 1) {
            const {alphabetKey} = child;
            const mask = ISet.full(alphabetKey.length);
            ISet.removeAll(mask, child.masks[0]);
            return new Pattern(width, height, alphabetKey, [-2], [mask], true);
        }
        return {kind: 'not', child, width, height, _key: undefined};
    }
    
    export function rotate(p: PatternTree): PatternTree {
        switch(p.kind) {
            case 'leaf':
                return Pattern.rotate(p);
            case 'top':
                return top(p.height, p.width);
            case 'bottom':
                return bottom(p.height, p.width);
            case 'and':
                return and(rotate(p.left), rotate(p.right));
            case 'or':
                return or(rotate(p.left), rotate(p.right));
            case 'not':
                return not(Pattern.rotate(p.child));
        }
    }
    
    export function reflect(p: PatternTree): PatternTree {
        switch(p.kind) {
            case 'leaf':
                return Pattern.reflect(p);
            case 'top':
            case 'bottom':
                return p;
            case 'and':
                return and(reflect(p.left), reflect(p.right));
            case 'or':
                return or(reflect(p.left), reflect(p.right));
            case 'not':
                return not(Pattern.reflect(p.child));
        }
    }
    
    export function getLeaves(p: PatternTree): LeafOrTop[] {
        switch(p.kind) {
            case 'leaf':
            case 'top':
                return [p];
            case 'bottom':
                return [];
            case 'and':
            case 'or': {
                const out = getLeaves(p.left);
                out.push(...getLeaves(p.right));
                return out;
            }
            case 'not': {
                const out = getLeaves(p.child);
                out.push(top(p.width, p.height));
                return out;
            }
        }
    }
    
    /**
     * Evaluates a pattern tree as a boolean expression, using the given
     * predicate to evaluate the leaf and top nodes.
     */
    export function matches(p: PatternTree, predicate: (x: LeafOrTop) => boolean): boolean {
        switch(p.kind) {
            case 'leaf':
            case 'top':
                return predicate(p);
            case 'bottom':
                return false;
            case 'and':
                return matches(p.left, predicate) && matches(p.right, predicate);
            case 'or':
                return matches(p.left, predicate) || matches(p.right, predicate);
            case 'not':
                return predicate(top(p.width, p.height)) && !matches(p.child, predicate);
        }
    }
    
    export function key(p: PatternTree): string {
        switch(p.kind) {
            case 'leaf':
            case 'top':
                return Pattern.key(p);
            case 'bottom':
                return p._key ??= `${p.width}x${p.height}:${p.kind}`;
            case 'and':
            case 'or':
                return p._key ??= `(${key(p.left)}) ${p.kind} (${key(p.right)})`;
            case 'not':
                return p._key ??= `not (${key(p.child)})`;
        }
    }
}

class Pattern extends MJr.Pattern {
    public static rowsOf(p: PatternTree.LeafOrTop): PatternTree.LeafOrTop[] {
        const {width, height} = p;
        if(height === 1) { return [p]; }
        
        if(p.kind === 'top') {
            return emptyArray(height, PatternTree.top(width, 1))
        }
        
        const {pattern, masks} = p;
        const out: Pattern[] = []
        const n = width * height;
        for(let offset = 0; offset < n; offset += width) {
            const row = pattern.slice(offset, offset + width);
            const rowMasks = masks.slice(offset, offset + width);
            out.push(new Pattern(width, 1, p.alphabetKey, row, rowMasks, p.hasUnions));
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
        return new Pattern(height, width, p.alphabetKey, newData, newMasks, p.hasUnions);
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
        return new Pattern(width, height, p.alphabetKey, newData, newMasks, p.hasUnions);
    }
    
    public static key(p: PatternTree.LeafOrTop): string {
        return p._key ??= `${p.width}x${p.height}:${p.kind === 'leaf' ? p.masks.map(ISet.key).join(';') : 'top'}`;
    }
    
    public readonly kind: 'leaf' = 'leaf';
    _key: string | undefined = undefined;
    
    public constructor(
        /**
         * The pattern's width.
         */
        width: number,
        /**
         * The pattern's height.
         */
        height: number,
        public readonly alphabetKey: string,
        /**
         * The pattern, as a flat array. Wildcards are represented as -1, and
         * unions as -2.
         */
        pattern: readonly number[],
        /**
         * The pattern, as a flat array of bitmasks.
         */
        public readonly masks: readonly ISet[],
        /**
         * Indicates whether this pattern has any unions, i.e. cells which can
         * match multiple alphabet symbols, but are not wildcards.
         */
        public readonly hasUnions: boolean,
    ) {
        super(width, height, pattern);
    }
    
    /**
     * Indicates whether this pattern is tautological, i.e. it always matches
     * at any position.
     */
    public isTop(): boolean {
        return this.pattern.every(p => p === -1);
    }
    
    /**
     * Calls the given function for each non-wildcard, non-union symbol in this
     * pattern.
     */
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
