///<reference path="../runtime/mjr.ts"/>

type PatternTree = PatternTree.Node

namespace PatternTree {
    export type Kind = Node['kind']
    export type Node =
        | Pattern
        | _Node<'bottom', {}>
        | _Node<'and' | 'or', {left: OfKind<'and' | 'or' | 'not' | 'leaf'>, right: OfKind<'and' | 'or' | 'not' | 'leaf'>}>
        | _Node<'not', {child: Pattern}>
    
    type _Node<K extends string, T> = K extends unknown ? Readonly<{kind: K} & T & {width: number, height: number}> & {_key: string | undefined} : never
    
    type OfKind<K extends Kind> = K extends 'leaf' | 'top' ? Pattern : Extract<Node, {kind: K}>
    
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
                maskSize === 1 ? ISet.first(mask)
                : maskSize === alphabetKey.length ? PatternValue.WILDCARD
                : PatternValue.UNION
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
            case 'top':
                return left.masks.every((mask, i) => ISet.isSubset(mask, right.masks[i]));
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
    
    export function top(width: number, height: number, alphabetKey: string): OfKind<'top'> {
        const pattern = emptyArray(width * height, PatternValue.WILDCARD);
        const masks = emptyArray(width * height, ISet.full(alphabetKey.length));
        return new Pattern(width, height, alphabetKey, pattern, masks, false);
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
            const pattern = [ISet.size(mask) === alphabetKey.length ? PatternValue.WILDCARD : PatternValue.UNION];
            return new Pattern(1, 1, alphabetKey, pattern, [mask], true);
        }
        
        return {kind: 'or', left, right, width, height, _key: undefined};
    }
    
    export function not(child: PatternTree, alphabetKey: string): PatternTree {
        const {width, height} = child;
        switch(child.kind) {
            case 'top':
                return bottom(width, height);
            case 'bottom':
                return top(width, height, alphabetKey);
            case 'and':
                return or(not(child.left, alphabetKey), not(child.right, alphabetKey));
            case 'or':
                return and(not(child.left, alphabetKey), not(child.right, alphabetKey));
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
    
    export function isLeafOrTop(p: PatternTree): p is Pattern {
        return p.kind === 'leaf' || p.kind === 'top';
    }
    
    export function isDisjoint(p: PatternTree, q: Pattern): boolean {
        // double negation here due to and/or patterns
        return !matches(p, p => !p.isDisjoint(q));
    }
    
    export function rotate(p: PatternTree): PatternTree {
        switch(p.kind) {
            case 'leaf':
            case 'top':
                return Pattern.rotate(p);
            case 'bottom':
                return bottom(p.height, p.width);
            case 'and':
                return and(rotate(p.left), rotate(p.right));
            case 'or':
                return or(rotate(p.left), rotate(p.right));
            case 'not':
                return not(Pattern.rotate(p.child), p.child.alphabetKey);
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
                return not(Pattern.reflect(p.child), p.child.alphabetKey);
        }
    }
    
    export function getLeaves(p: PatternTree): Pattern[] {
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
                out.push(top(p.width, p.height, p.child.alphabetKey));
                return out;
            }
        }
    }
    
    /**
     * Evaluates a pattern tree as a boolean expression, using the given
     * predicate to evaluate the leaf and top nodes.
     */
    export function matches(p: PatternTree, predicate: (x: Pattern) => boolean): boolean {
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
                return predicate(top(p.width, p.height, p.child.alphabetKey)) && !matches(p.child, predicate);
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

const enum PatternValue {
    WILDCARD = 0xFF,
    UNION = 0xFE,
}

class Pattern extends MJr.Pattern {
    public static rowsOf(p: Pattern): Pattern[] {
        const {width, height} = p;
        if(height === 1) { return [p]; }
        
        if(p.kind === 'top') {
            return emptyArray(height, PatternTree.top(width, 1, p.alphabetKey))
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
    
    public static windowsOf(p: Pattern, n: number, periodic: boolean): Pattern[] {
        const {width, height, alphabetKey, pattern, masks, hasUnions} = p;
        
        const maxX = periodic ? width : width - n + 1;
        const maxY = periodic ? height : height - n + 1;
        const out: Pattern[] = [];
        
        for(let y = 0; y < maxY; ++y) {
            for(let x = 0; x < maxX; ++x) {
                const qPattern: number[] = [];
                const qMasks: ISet[] = [];
                for(let dy = 0; dy < n; ++dy) {
                    const py = (y + dy) % height;
                    for(let dx = 0; dx < n; ++dx) {
                        const px = (x + dx) % width;
                        qPattern.push(pattern[px + width * py]);
                        qMasks.push(masks[px + width * py]);
                    }
                }
                out.push(new Pattern(n, n, alphabetKey, qPattern, qMasks, hasUnions));
            }
        }
        
        return out;
    }
    
    public static rotate(p: Pattern): Pattern {
        const {width, height, pattern, masks} = p;
        if(p.kind === 'top') {
            return new Pattern(height, width, p.alphabetKey, pattern, masks, p.hasUnions);
        }
        
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
        if(p.kind === 'top') { return p; }
        
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
    
    public static key(p: Pattern): string {
        return p._key ??= `${p.width}x${p.height}:${p.masks.map(ISet.key).join(';')}`;
    }
    
    public readonly kind: 'leaf' | 'top';
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
         * The pattern, as a flat array. Wildcards are represented as 0xFF, and
         * unions as 0xFE.
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
        this.kind = pattern.every(p => p === PatternValue.WILDCARD) ? 'top' : 'leaf';
    }
    
    public canBe(dx: number, dy: number, c: number): boolean {
        return ISet.has(this.masks[dx + dy * this.width], c);
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
    
    public isDisjoint(other: Pattern): boolean {
        return this.masks.some((mask, i) => ISet.isDisjoint(mask, other.masks[i]));
    }
}
