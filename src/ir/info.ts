namespace IR {
    const enum Flags {
        NO_STATE_CHANGES = 1,
        NO_BREAKS = 2,
        NO_CONTINUES = 4,
        NO_RETURNS = 8,
        NO_THROWS = 16,
        NO_OUTPUT = 32,
        LOCALLY_DETERMINISTIC = 64,
        CONTEXT_INDEPENDENT = 128,
        CAN_COMPLETE_NORMALLY = 256,
        
        DO_NOTHING = 511,
        
        NO_BREAKS_OR_CONTINUES = NO_BREAKS | NO_CONTINUES,
        NO_CONTROL_FLOW_EFFECTS = NO_BREAKS_OR_CONTINUES | NO_THROWS | NO_RETURNS | CAN_COMPLETE_NORMALLY,
        NO_SIDE_EFFECTS = NO_STATE_CHANGES | NO_CONTROL_FLOW_EFFECTS | NO_OUTPUT,
        TIME_INDEPENDENT = NO_SIDE_EFFECTS | CONTEXT_INDEPENDENT,
        
        STATE_GET = NO_SIDE_EFFECTS | LOCALLY_DETERMINISTIC,
        STATE_UPDATE = NO_CONTROL_FLOW_EFFECTS | NO_OUTPUT,
    }
    
    function _addAll<T>(a: Set<T>, b?: ReadonlySet<T>): void {
        if(b !== undefined) {
            for(const x of b) { a.add(x); }
        }
    }
    
    export class Info {
        public static readonly DO_NOTHING = new Info(Flags.DO_NOTHING);
        public static readonly BREAK = new Info(Flags.DO_NOTHING & ~Flags.NO_BREAKS & ~Flags.CAN_COMPLETE_NORMALLY);
        public static readonly CONTINUE = new Info(Flags.DO_NOTHING & ~Flags.NO_CONTINUES & ~Flags.CAN_COMPLETE_NORMALLY);
        public static readonly RETURN = new Info(Flags.DO_NOTHING & ~Flags.NO_RETURNS & ~Flags.CAN_COMPLETE_NORMALLY);
        public static readonly THROW = new Info(Flags.DO_NOTHING & ~Flags.NO_THROWS & ~Flags.CAN_COMPLETE_NORMALLY);
        public static readonly OUTPUT = new Info(Flags.DO_NOTHING & ~Flags.NO_OUTPUT);
        public static readonly DEFERRED = new Info(Flags.CAN_COMPLETE_NORMALLY, new Set([-1]), new Set([-1]));
        public static readonly UNREACHABLE = new Info(Flags.DO_NOTHING & ~Flags.CAN_COMPLETE_NORMALLY);
        
        public static readonly FRESH_VALUE = new Info(Flags.NO_SIDE_EFFECTS | Flags.CONTEXT_INDEPENDENT);
        public static readonly STATE_GET = new Info(Flags.STATE_GET);
        public static readonly STATE_UPDATE = new Info(Flags.STATE_UPDATE);
        public static readonly PREAMBLE = new Info(Flags.NO_SIDE_EFFECTS & ~Flags.NO_THROWS);
        
        public static constGet(id: number): Info {
            return new Info(Flags.DO_NOTHING, new Set([id]));
        }
        public static varGet(id: number): Info {
            return new Info(Flags.STATE_GET, new Set([id]));
        }
        public static varSet(id: number): Info {
            return new Info(Flags.STATE_UPDATE, undefined, new Set([id]));
        }
        
        public static seq(nodes: readonly (Stmt | Expr)[]): Info {
            return Info.DO_NOTHING.thenSeq(nodes);
        }
        
        private constructor(
            private readonly flags: Flags,
            private readonly freeVarsGet?: ReadonlySet<number>,
            private readonly freeVarsSet?: ReadonlySet<number>,
        ) {
            if(freeVarsGet?.size === 0) { this.freeVarsGet = undefined; }
            if(freeVarsSet?.size === 0) { this.freeVarsSet = undefined; }
        }
        
        public toJSON(): void {}
        
        private withFlags(flags: Flags): Info {
            return new Info(flags, this.freeVarsGet, this.freeVarsSet)
        }
        private withUnionVars(flags: Flags, other: Info): Info {
            const freeVarsGet = new Set(this.freeVarsGet),
                freeVarsSet = new Set(this.freeVarsSet);
            _addAll(freeVarsGet, other.freeVarsGet);
            _addAll(freeVarsSet, other.freeVarsSet);
            return new Info(flags, freeVarsGet, freeVarsSet);
        }
        
        public then(other: Info): Info {
            return this.withUnionVars(this.flags & other.flags, other);
        }
        public thenSeq(others: readonly (Stmt | Expr)[]): Info {
            let flags = this.flags;
            const freeVarsGet = new Set(this.freeVarsGet),
                freeVarsSet = new Set(this.freeVarsSet);
            for(const other of others) {
                flags &= other.info.flags;
                _addAll(freeVarsGet, other.info.freeVarsGet);
                _addAll(freeVarsSet, other.info.freeVarsSet);
            }
            return new Info(flags, freeVarsGet, freeVarsSet);
        }
        public or(other: Info): Info {
            const a = this.flags,
                b = other.flags,
                c = (a | b) & Flags.CAN_COMPLETE_NORMALLY;
            return this.withUnionVars((a & b) | c, other);
        }
        public asLoopBody(infinite: boolean): Info {
            let flags = this.flags | Flags.NO_BREAKS_OR_CONTINUES;
            if(this.canBreak()) {
                flags |= Flags.CAN_COMPLETE_NORMALLY;
            } else if(infinite) {
                flags &= ~Flags.CAN_COMPLETE_NORMALLY;
            }
            return this.withFlags(flags);
        }
        public asVarDecl(name: NameExpr): Info {
            const freeVarsGet = new Set(this.freeVarsGet);
            const freeVarsSet = new Set(this.freeVarsSet);
            freeVarsGet.delete(name.id);
            freeVarsSet.delete(name.id);
            return new Info(this.flags, freeVarsGet, freeVarsSet);
        }
        public asFuncDecl(name: NameExpr): Info {
            return this.asVarDecl(name)
                .withFlags(Flags.DO_NOTHING);
        }
        
        public canGetVar(name: NameExpr): boolean {
            const s = this.freeVarsGet;
            return s !== undefined
                && (s.has(name.id) || s.has(-1));
        }
        public canSetVar(name: NameExpr): boolean {
            const s = this.freeVarsSet;
            return s !== undefined
                && (s.has(name.id) || s.has(-1));
        }
        public canUseVar(name: NameExpr): boolean {
            return this.canGetVar(name) || this.canSetVar(name);
        }
        public hasFreeVars(): boolean {
            return this.freeVarsGet !== undefined || this.freeVarsSet !== undefined;
        }
        
        public hasSideEffects(): boolean {
            return (this.flags & Flags.NO_SIDE_EFFECTS) !== Flags.NO_SIDE_EFFECTS;
        }
        public isTimeIndependent(): boolean {
            return (this.flags & Flags.TIME_INDEPENDENT) === Flags.TIME_INDEPENDENT;
        }
        public isLocallyDeterministic(): boolean {
            return (this.flags & Flags.LOCALLY_DETERMINISTIC) !== 0;
        }
        public canCompleteNormally(): boolean {
            return (this.flags & Flags.CAN_COMPLETE_NORMALLY) !== 0;
        }
        public canBreakOrContinue(): boolean {
            return (this.flags & Flags.NO_BREAKS_OR_CONTINUES) !== Flags.NO_BREAKS_OR_CONTINUES;
        }
        public canBreak(): boolean {
            return (this.flags & Flags.NO_BREAKS) === 0;
        }
        
        public commutesWith(other: Stmt | Expr): boolean {
            return (!this.hasSideEffects() || other.info.isTimeIndependent())
                && (!other.info.hasSideEffects() || this.isTimeIndependent());
        }
    }
    
    export const LOCAL_FUNCTION_INFO: IRecord<LocalFunction, Info> = {
        mask_clear: Info.STATE_UPDATE,
        mask_set: Info.STATE_UPDATE,
        mask_hasnt: Info.STATE_GET,
    };
    
    export const LIB_FUNCTION_INFO: IRecord<LibFunction, Info> = {
        lfsrFeedbackTerm: Info.DO_NOTHING,
        nextIntChecked: Info.FRESH_VALUE.or(Info.THROW),
    };
    
    export const LIB_METHOD_INFO: {readonly [K in LibInterface]: IRecord<LibMethod<K>, Info>} = {
        Grid: {
            index: Info.DO_NOTHING.or(Info.THROW),
            toString: Info.STATE_GET,
            wrapIndex: Info.DO_NOTHING,
        },
        PRNG: {
            nextDouble: Info.FRESH_VALUE,
            nextInt: Info.FRESH_VALUE,
        },
        Pattern: {
            fitsMask: Info.STATE_GET,
            hasEffect: Info.STATE_GET,
            put: Info.STATE_UPDATE,
        },
        RewriteInfo: {},
        Sampler: {
            add: Info.STATE_UPDATE,
            copyInto: Info.STATE_UPDATE,
            copyIntoOffset: Info.STATE_UPDATE,
            del: Info.STATE_UPDATE,
            has: Info.STATE_GET,
            sample: Info.STATE_GET,
            shuffleInto: Info.STATE_UPDATE,
            shuffleIntoOffset: Info.STATE_UPDATE,
        },
    };
    
    export const LIB_CONSTRUCTOR_INFO: {readonly [K in LibClass]: Info} = {
        Grid: Info.FRESH_VALUE,
        Pattern: Info.DO_NOTHING, // new immutable object
        RewriteInfo: Info.DO_NOTHING, // new immutable object
        Sampler: Info.FRESH_VALUE,
    };
}
