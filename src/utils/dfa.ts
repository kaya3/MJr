///<reference path="partition.ts"/>

namespace Regex {
    export const enum Kind {
        LETTERS,
        WILDCARD,
        CONCAT,
        UNION,
        KLEENESTAR,
        ACCEPT,
    }
    
    export type Node<T> = Readonly<
        | {kind: Kind.LETTERS, letterIDs: readonly number[]}
        | {kind: Kind.WILDCARD}
        | {kind: Kind.CONCAT, children: readonly Node<T>[]}
        | {kind: Kind.UNION, children: readonly Node<T>[]}
        | {kind: Kind.KLEENESTAR, child: Node<T>}
        | {kind: Kind.ACCEPT, accept: T}
    >
    
    export function letters(letterIDs: readonly number[]): Node<never> {
        return {kind: Kind.LETTERS, letterIDs};
    }
    export const WILDCARD: Node<never> = {kind: Kind.WILDCARD};
    export function concat<T>(children: Node<T>[]): Node<T> {
        return {kind: Kind.CONCAT, children};
    }
    export function union<T>(children: Node<T>[]): Node<T> {
        return {kind: Kind.UNION, children};
    }
    export function kleeneStar<T>(child: Node<T>): Node<T> {
        return {kind: Kind.KLEENESTAR, child};
    }
    export function accept<T>(accept: T): Node<T> {
        return {kind: Kind.ACCEPT, accept};
    }
    
    export const DOT_STAR = kleeneStar<never>(WILDCARD);
    
    export function compile<T>(alphabetSize: number, regex: Node<T>): DFA<T> {
        return new NFA(alphabetSize, regex).toDFA();
    }
}

interface NFANode<T> extends Readonly<{
    epsilons: number[],
    letters: readonly number[],
    nextID: number,
    acceptSet: T[],
}> {
    epsilonClosure: ISet | undefined;
}

class NFA<T> {
    private readonly nodes: NFANode<T>[] = [];
    private readonly startID: number;
    public constructor(
        private readonly alphabetSize: number,
        regex: Regex.Node<T>,
    ) {
        this.startID = this.makeFromRegex(regex, this.makeNode([]));
        //console.log(`NFA with ${this.nodes.length} nodes on alphabet of size ${alphabetSize}`);
    }
    
    private makeNode(epsilons: number[]): number;
    private makeNode(epsilons: number[], letters: readonly number[], nextID: number): number;
    private makeNode(epsilons: number[], letters: readonly number[] = [], nextID: number = -1): number {
        const {nodes} = this;
        const id = nodes.length;
        nodes.push({epsilons, letters, nextID, acceptSet: [], epsilonClosure: undefined});
        return id;
    }
    
    private makeFromRegex(regex: Regex.Node<T>, outID: number): number {
        // https://en.wikipedia.org/wiki/Thompson's_construction
        switch(regex.kind) {
            case Regex.Kind.LETTERS: {
                return this.makeNode([], regex.letterIDs, outID);
            }
            case Regex.Kind.WILDCARD: {
                return this.makeNode([], makeArray(this.alphabetSize, i => i), outID);
            }
            case Regex.Kind.CONCAT: {
                const {children} = regex;
                for(let i = children.length - 1; i >= 0; --i) {
                    outID = this.makeFromRegex(children[i], outID);
                }
                return outID;
            }
            case Regex.Kind.UNION: {
                const epsilons = regex.children.map(child => this.makeFromRegex(child, this.makeNode([outID])));
                return this.makeNode(epsilons);
            }
            case Regex.Kind.KLEENESTAR: {
                const childOutID = this.makeNode([outID]);
                const childInID = this.makeFromRegex(regex.child, childOutID);
                this.nodes[childOutID].epsilons.push(childInID);
                return this.makeNode([childInID, outID]);
            }
            case Regex.Kind.ACCEPT: {
                const node = this.nodes[outID];
                node.acceptSet.push(regex.accept);
                return outID;
            }
        }
    }
    
    private getEpsilonClosure(nodeID: number): ISet {
        const {nodes} = this;
        
        // epsilon closure, by depth-first search
        // use ISet instead of Set<number> or bigint for the state, for performance
        const cached = nodes[nodeID].epsilonClosure;
        if(cached !== undefined) { return cached; }
        
        const out = ISet.empty(nodes.length);
        const stack = [nodeID];
        while(stack.length > 0) {
            const id = stack.pop()!;
            if(ISet.has(out, id)) { continue; }
            
            ISet.add(out, id);
            for(const eps of nodes[id].epsilons) {
                if(!ISet.has(out, eps)) {
                    stack.push(eps);
                }
            }
        }
        
        nodes[nodeID].epsilonClosure = out;
        return out;
    }
    
    public toDFA(): DFA<T> {
        // https://en.wikipedia.org/wiki/Powerset_construction
        
        const {alphabetSize, nodes} = this;
        const nfaStates: IDMap<ISet> = IDMap.withKey(ISet.key);
        const dfaNodes: DFANode<T>[] = [];
        
        const cache = new Map<PrimitiveKey, number>();
        const getNodeID = (nfaState: MutableISet): number => getOrCompute(cache, ISet.key(nfaState), () => {
            for(let nfaNodeID of ISet.toArray(nfaState)) {
                while(true) {
                    const epsilons = nodes[nfaNodeID].epsilons;
                    if(epsilons.length === 0) {
                        break;
                    } else if(epsilons.length === 1) {
                        const nfaNodeID = epsilons[0];
                        if(ISet.has(nfaState, nfaNodeID)) { break; }
                        ISet.add(nfaState, nfaNodeID);
                    } else {
                        ISet.addAll(nfaState, this.getEpsilonClosure(nfaNodeID));
                        break;
                    }
                }
            }
            return nfaStates.getOrCreateID(nfaState);
        });
        
        const startID = getNodeID(ISet.of(nodes.length, [this.startID]));
        // sanity check
        if(startID !== 0) { throw new Error(); }
        
        // this loop iterates over `nfaStates`, while adding to it via `getNodeID`
        for(let nfaStateID = 0; nfaStateID < nfaStates.size(); ++nfaStateID) {
            const transitionStates = makeArray(alphabetSize, () => ISet.empty(nodes.length));
            const accepts: T[] = [];
            ISet.forEach(nfaStates.getByID(nfaStateID), nfaNodeID => {
                const nfaNode = nodes[nfaNodeID];
                for(const letterID of nfaNode.letters) {
                    ISet.add(transitionStates[letterID], nfaNode.nextID);
                }
                accepts.push(...nfaNode.acceptSet);
            });
            
            dfaNodes.push({
                transitions: transitionStates.map(getNodeID),
                accepts,
            });
        }
        
        return new DFA(alphabetSize, dfaNodes);
    }
}

type DFANode<T> = Readonly<{
    transitions: readonly number[],
    accepts: readonly T[],
}>

class DFA<T> {
    public constructor(
        public readonly alphabetSize: number,
        private readonly nodes: readonly DFANode<T>[],
    ) {
        //console.log(`DFA with ${nodes.length} nodes on alphabet of size ${alphabetSize} and ${acceptSetMap.size()} accept sets`);
    }
    
    /**
     * Returns the number of distinct states of this DFA.
     */
    public size(): number {
        return this.nodes.length;
    }
    
    public toFlatArray(): readonly number[] {
        return this.nodes.flatMap(node => node.transitions);
    }
    
    /**
     * Returns a pair `[acceptSetIDs, acceptSetMap]` where `acceptSetMap` is an
     * IDMap of all distinct accept sets recognised by this DFA, and `acceptSetIDs`
     * maps each DFA state to its accept set's ID in `acceptSetMap`.
     */
    public getAcceptSetMap<S extends T>(acceptMap: ReadonlyIDMap<T>, predicate: (accept: T) => accept is S): [readonly number[], ReadonlyIDMap<readonly S[]>];
    public getAcceptSetMap(acceptMap: ReadonlyIDMap<T>, predicate?: (accept: T) => boolean): [readonly number[], ReadonlyIDMap<readonly T[]>];
    public getAcceptSetMap(acceptMap: ReadonlyIDMap<T>, predicate?: (accept: T) => boolean): [readonly number[], ReadonlyIDMap<readonly T[]>] {
        const {nodes} = this;
        const acceptSets = nodes.map(
            predicate !== undefined
            ? node => node.accepts.filter(predicate)
            : node => node.accepts
        );
        const acceptSetMap = IDMap.ofWithKey(
            acceptSets,
            set => ISet.key(acceptMap.getIDSet(set)),
        );
        return [
            acceptSetMap.getIDs(acceptSets),
            acceptSetMap,
        ];
    }
    
    /**
     * Returns an iterable mapping each accept to the set of node IDs which
     * accept it.
     */
    private computeAcceptingStates(keyFunc: (accept: T) => PrimitiveKey): Iterable<ISet> {
        const {nodes} = this;
        const n = nodes.length;
        const map = new Map<PrimitiveKey, MutableISet>();
        for(let id = 0; id < n; ++id) {
            for(const accept of nodes[id].accepts) {
                const key = keyFunc(accept);
                const set = getOrCompute(map, key, () => ISet.empty(n));
                ISet.add(set, id);
            }
        }
        return map.values();
    }
    
    /**
     * Returns an equivalent DFA with the minimum possible number of states.
     */
    public minimise(keyFunc: (accept: T) => PrimitiveKey): DFA<T> {
        // https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft's_algorithm
        
        const {alphabetSize, nodes} = this;
        const n = nodes.length;
        
        const inverseTransitions: (number[] | undefined)[] = emptyArray(alphabetSize * n, undefined);
        for(let id = 0; id < n; ++id) {
            const {transitions} = nodes[id];
            for(let c = 0; c < alphabetSize; ++c) {
                (inverseTransitions[c * n + transitions[c]] ??= []).push(id);
            }
        }
        
        const partition = new Partition(n);
        for(const d of this.computeAcceptingStates(keyFunc)) { partition.refine(d); }
        
        // pre-allocate
        const refinement = ISet.empty(n);
        while(true) {
            const a = partition.pollUnprocessed();
            if(a === undefined) { break; }
            
            for(let c = 0; c < alphabetSize; ++c) {
                ISet.clear(refinement);
                for(const id of a) {
                    const arr = inverseTransitions[c * n + id];
                    if(arr !== undefined) {
                        // `ISet.addAll` would be faster for dense sets, but most of these sets are small
                        for(const x of arr) { ISet.add(refinement, x); }
                    }
                }
                partition.refine(refinement);
                
                // shortcut if the DFA cannot be minimised
                if(partition.countSubsets() === n) { return this; }
            }
        }
        
        const reps: IDMap<number> = IDMap.withKey(id => partition.getRepresentative(id));
        // ensure id(rep(0)) === 0, so that 0 is still the starting state
        reps.getOrCreateID(0);
        partition.forEachRepresentative(x => reps.getOrCreateID(x));
        
        const repNodes: readonly DFANode<T>[] = reps.map(rep => {
            const {transitions, accepts} = this.nodes[rep];
            return {
                transitions: reps.getIDs(transitions),
                accepts,
            };
        });
        
        return new DFA(alphabetSize, repNodes);
    }
    
    /**
     * Returns a new DFA, with the accept sets replaced using the function `f`.
     */
    public map<S>(f: (acceptSet: readonly T[]) => readonly S[]): DFA<S> {
        const mappedNodes = this.nodes.map(node => {
            const {transitions, accepts} = node;
            return {
                transitions,
                accepts: f(accepts),
            };
        });
        return new DFA(this.alphabetSize, mappedNodes);
    }
}
