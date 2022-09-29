namespace IR {
    export const NAMES = {
        WIDTH: nameExpr('width'),
        HEIGHT: nameExpr('height'),
        PARAMS: nameExpr('params'),
        RNG: nameExpr('rng'),
        STATE: nameExpr('state'),
        AT: nameExpr('at'),
        AT_X: nameExpr('atX'),
        AT_Y: nameExpr('atY'),
        AT_CONV: nameExpr('atConv'),
        MATCHES: nameExpr('matches'),
        MATCH_COUNT: nameExpr('count'),
        MATCH: nameExpr('m'),
        ANY: nameExpr('any'),
        G: nameExpr('g'),
        I: nameExpr('i'),
        J: nameExpr('j'),
        N: nameExpr('n'),
        P: nameExpr('p'),
        START_X: nameExpr('startX'),
        START_Y: nameExpr('startY'),
        END_X: nameExpr('endX'),
        END_Y: nameExpr('endY'),
        EFFECTIVE_WIDTH: nameExpr('w'),
        EFFECTIVE_HEIGHT: nameExpr('h'),
        S: nameExpr('s'),
        OLD_S: nameExpr('oldS'),
        T: nameExpr('t'),
        OLD_T: nameExpr('oldT'),
        U: nameExpr('u'),
        MASK: nameExpr('mask'),
        MASK_CLEAR: nameExpr('mask_clear'),
        MASK_SET: nameExpr('mask_set'),
        MASK_HASNT: nameExpr('mask_hasnt'),
        
        convBufferN(g: Grid, id: number): NameExpr {
            return nameExpr(`grid${g.grid.id}_conv${id}_n`);
        },
        convBufferArray(g: Grid, id: number): NameExpr {
            return nameExpr(`grid${g.grid.id}_conv${id}`);
        },
        
        constant(id: number): NameExpr {
            return nameExpr(`constant${id}`);
        },
        counter(g: Grid, id: number): NameExpr {
            return nameExpr(`grid${g.grid.id}_counter${id}`);
        },
        flag(id: number): NameExpr {
            return nameExpr(`f${id}`);
        },
        limit(id: number): NameExpr {
            return nameExpr(`limit${id}`);
        },
        sampler(g: Grid, id: number): NameExpr {
            return nameExpr(`grid${g.grid.id}_sampler${id}`);
        },
        tmpPattern(id: number): NameExpr {
            return nameExpr(`p${id}`);
        },
        variable(v: string, n: number): NameExpr {
            return nameExpr(n > 0 ? `_${v}_${n}` : `_${v}`);
        },
        
        gridVar(g: Grid, v: GridVar): NameExpr {
            return nameExpr(`grid${g.grid.id}_${v}`);
        },
        matcherVar(g: Grid, id: number, v: MatcherVar): NameExpr {
            return nameExpr(`grid${g.grid.id}_matcher${id}_${v}`);
        },
    };
    
    type GridVar = 'width' | 'height' | 'n' | 'data' | 'obj' | 'origin' | 'lfsrFeedbackTerm'
    type MatcherVar = 'update' | 'rowsToCols' | `${'row' | 'col'}${'DFA' | 'States' | 'AcceptSetIDs' | 'AcceptSets'}`
}
