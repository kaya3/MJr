namespace CFG {
    export interface CFG extends Readonly<{
        nodes: readonly Node[],
        numFlags: number;
    }> {}
    
    export type Node = BranchingStmtNode | CheckFlagNode | CheckLimitNode | DecrementLimitNode | NonBranchingStmtNode | PassNode | ResetNode | SetFlagNode | StopNode
    type Kind = Node['kind']
    type NodeOfKind<K extends Kind> = Extract<Node, {kind: K}>
    
    export interface JumpLabel {readonly nodeID: number}
    
    type _Node<K extends string, T> = Readonly<{id: number, kind: K} & T>
    interface CheckFlagNode extends _Node<'checkflag', {flagID: number, ifTrue: JumpLabel, then: JumpLabel}> {}
    interface CheckLimitNode extends _Node<'checklimit', {limitID: number, ifTrue: JumpLabel, then: JumpLabel}> {}
    interface DecrementLimitNode extends _Node<'decrementlimit', {limitID: number, then: JumpLabel}> {}
    interface SetFlagNode extends _Node<'setflag', {flagID: number, then: JumpLabel}> {}
    interface PassNode extends _Node<'pass', {then: JumpLabel}> {}
    export interface ResetNode extends _Node<'reset', {stmt: ASG.BlockStmt, flagID: number, reset: ASG.BlockReset | undefined, then: JumpLabel}> {}
    export interface BranchingStmtNode extends _Node<'stmt.branching', {stmt: ASG.BranchingStmt, ifChanged: JumpLabel, then: JumpLabel}> {}
    export interface NonBranchingStmtNode extends _Node<'stmt.nonbranching', {stmt: ASG.NonBranchingStmt, then: JumpLabel}> {}
    interface StopNode extends _Node<'stop', {}> {}
    
    const newLabel = () => ({nodeID: -1});
    
    class CFGBuilder implements CFG {
        readonly nodes: Node[] = [];
        numFlags = 0;
        
        constructor(readonly animate: boolean) {}
        
        makeNode<K extends Kind>(partialNode: {kind: K} & Omit<NodeOfKind<K>, 'id'>): Node {
            return withNextID(this.nodes, partialNode);
        }
        
        buildBlock(stmt: ASG.BlockStmt, flagID: number, then: JumpLabel): Node {
            const {kind, children} = stmt;
            if(children.length === 0) {
                return this.makeNode({kind: 'pass', then});
            }
            
            const isMarkov = kind === 'stmt.block.markov';
            const labels = makeArray(children.length, newLabel);
            labels.push(then);
            
            for(let i = 0; i < children.length; ++i) {
                const child = children[i];
                const thenTrue = labels[isMarkov ? 0 : i];
                const thenFalse = labels[i + 1];
                labels[i].nodeID = this.buildChild(child, kind, flagID, thenTrue, thenFalse).id;
            }
            
            return this.nodes[labels[0].nodeID];
        }
        
        buildChild(stmt: ASG.Statement, parentKind: ASG.BlockStmt['kind'], parentFlagID: number, ifTrue: JumpLabel, then: JumpLabel): Node {
            switch(stmt.kind) {
                case 'stmt.block.markov':
                case 'stmt.block.sequence': {
                    if(stmt.children.length === 0) {
                        return this.makeNode({kind: 'pass', then});
                    } else if(stmt.kind === 'stmt.block.markov' && parentKind === 'stmt.block.sequence' && stmt.reset === undefined) {
                        // optimisation: don't need a separate reset node for this
                        return this.buildBlock(stmt, parentFlagID, then);
                    }
                    
                    const flagID = this.numFlags++;
                    const beginLabel = newLabel();
                    const endLabel = newLabel();
                    const begin = this.makeNode({kind: 'reset', stmt, flagID, reset: stmt.reset, then: beginLabel});
                    beginLabel.nodeID = this.buildBlock(stmt, flagID, endLabel).id;
                    if(parentFlagID >= 0) {
                        const setFlagLabel = newLabel();
                        endLabel.nodeID = this.makeNode({kind: 'checkflag', flagID, ifTrue: setFlagLabel, then}).id;
                        setFlagLabel.nodeID = this.makeNode({kind: 'setflag', flagID: parentFlagID, then: ifTrue}).id;
                    } else {
                        endLabel.nodeID = this.makeNode({kind: 'checkflag', flagID, ifTrue, then}).id;
                    }
                    return begin;
                }
                
                case 'stmt.modified.limit': {
                    const {limit} = stmt;
                    if(limit.isTransparent) {
                        // optimisation for common case
                        return this.buildChild(stmt.child, parentKind, parentFlagID, then, then);
                    }
                    const limitID = limit.id;
                    
                    const childLabel = newLabel(), decrementLimitLabel = newLabel();
                    const r = this.makeNode({kind: 'checklimit', limitID, ifTrue: childLabel, then});
                    childLabel.nodeID = this.buildChild(stmt.child, parentKind, parentFlagID, decrementLimitLabel, then).id;
                    decrementLimitLabel.nodeID = this.makeNode({kind: 'decrementlimit', limitID, then: ifTrue}).id;
                    return r;
                }
                
                case 'stmt.assign':
                case 'stmt.log':
                case 'stmt.put':
                case 'stmt.rules.map':
                case 'stmt.use': {
                    if((stmt.kind === 'stmt.assign' && stmt.variable.references === 0) || (stmt.kind === 'stmt.use' && !this.animate)) {
                        return this.makeNode({kind: 'pass', then});
                    }
                    return this.makeNode({kind: 'stmt.nonbranching', stmt, then});
                }
                
                case 'stmt.convchain':
                case 'stmt.path':
                case 'stmt.rules.basic.all':
                case 'stmt.rules.basic.one':
                case 'stmt.rules.basic.prl':
                case 'stmt.rules.biased.all':
                case 'stmt.rules.biased.one':
                case 'stmt.rules.search.all':
                case 'stmt.rules.search.one': {
                    if(parentFlagID >= 0) {
                        const setFlagLabel = newLabel();
                        const child = this.makeNode({kind: 'stmt.branching', stmt, ifChanged: setFlagLabel, then});
                        setFlagLabel.nodeID = this.makeNode({kind: 'setflag', flagID: parentFlagID, then: ifTrue}).id;
                        return child;
                    } else {
                        return this.makeNode({kind: 'stmt.branching', stmt, ifChanged: ifTrue, then});
                    }
                }
            }
        }
    }
    
    export function build(root: ASG.BlockStmt, animate: boolean): CFG {
        const builder = new CFGBuilder(animate);
        const stopLabel = newLabel();
        const rootNode = builder.buildBlock(root, -1, stopLabel);
        stopLabel.nodeID = builder.makeNode({kind: 'stop'}).id;
        
        // sanity check
        if(rootNode.id !== 0) { throw new Error(); }
        return builder;
    }
}
