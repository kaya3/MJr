///<reference path="./expr.ts"/>
///<reference path="./stmt.ts"/>

namespace IR {
    export const NAMES = {
        WIDTH: nameExpr('width'),
        HEIGHT: nameExpr('height'),
        PARAMS: nameExpr('params'),
        RNG: nameExpr('rng'),
        STATE: nameExpr('state'),
        ITERATIONS: nameExpr('iterations'),
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
        variable(v: ASG.FormalVariable): NameExpr {
            return nameExpr(`_${v.uniqueName}`);
        },
        
        convBufferVar(g: Grid, id: number, v: ConvBufferVar): NameExpr {
            return nameExpr(`grid${g.grid.id}_conv${id}_${v}`);
        },
        gridVar(g: Grid, v: GridVar): NameExpr {
            return nameExpr(`grid${g.grid.id}_${v}`);
        },
        matcherVar(g: Grid, id: number, v: MatcherVar): NameExpr {
            return nameExpr(`grid${g.grid.id}_matcher${id}_${v}`);
        },
        
        otherVar(stmtID: number, name: string): NameExpr {
            return nameExpr(`stmt${stmtID}_${name}`);
        },
    };
    
    type ConvBufferVar = 'width' | 'height' | 'n' | 'buffer'
    type GridVar = 'width' | 'height' | 'n' | 'data' | 'obj' | 'origin' | 'buffer' | 'lfsrFeedbackTerm'
    type MatcherVar = 'update' | 'rowsToCols' | `${'row' | 'col'}${'DFA' | 'States' | 'AcceptSetIDs' | 'AcceptSets'}`
}
