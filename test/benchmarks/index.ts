///<reference path="../framework.ts"/>

Benchmark.add({
    GameOfLife: `
        # Implementation of John Conway's "Game of Life"
        # https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

        grid [DA]

        @limit 1
        prl: [D] -> [A] if random < 0.5
        
        @limit 1000
        convolution {kernel="Moore"}:
            [D] -> [A] if sum [A] == 3
            [A] -> [D] if sum [A] < 2 or sum [A] > 3
    `,

    River: `
        # Translated from Maxim Gumin's 'River' model
        # https://github.com/mxgmn/MarkovJunior/blob/main/models/River.xml

        grid [BWRUGE]

        once: [B] -> [W]
        once: [B] -> [R]

        one:
            [RB] -> [RR]
            [WB] -> [WW]

        all: [RW] -> [UU]
        all: [[WR]] -> [B]

        @limit 1
        all: [UB] -> [UU]

        all: [BU/UB] -> [U./..]
        all: [UB] -> [.G]

        @limit 13
        one: [B] -> [E]

        one:
            [EB] -> [.E]
            [GB] -> [.G]
    `,
    
    BasicDungeonGrowth: `
        # Translated from Maxim Gumin's 'BasicDungeonGrowth' model
        # https://github.com/mxgmn/MarkovJunior/blob/main/models/BasicDungeonGrowth.xml

        grid [BRACDG]
        union [?] = [[BR]]

        put [R] at origin

        one: [..?../.BBB./.BBB?/.BBB./..R..] -> [AARAA/ADDDA/ADDDR/ADDDA/AACAA]
        one: [ACA/BBB] -> [ARA/BBB]

        all: [C] -> [D]
        all: [R] -> [D]

        all: [BD] -> [.A]
        all: [DDD/ADA/DDD] -> [.../D.D/...]
        all: [DDD/DAD/DDD] -> [.../.D./...]
    `,
    
    NystromDungeon: `
        # Translated from Maxim Gumin's 'NystromDungeon' model, which
        # is based on Bob Nystrom's dungeon generation algorithm
        # https://github.com/mxgmn/MarkovJunior/blob/main/models/NystromDungeon.xml
        # http://journal.stuffwithstuff.com/2014/12/21/rooms-and-mazes/
        
        let NUM_CYCLES = 5
        grid [BPWRG]
        put [P] at origin
        all: [PBB] -> [..P]
        
        let roomIn = [
            PBPBPBPBP /
            BBBBBBBBB /
            PBPBPBPBP /
            BBBBBBBBB /
            PBPBPBPBP /
            BBBBBBBBB /
            PBPBPBPBP
        ]
        let roomOut = [
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW /
            WWWWWWWWW
        ]
        
        one: roomIn -> roomOut
        markov:
            one: [RBP] -> [GGR]
            one: [GGR] -> [RWW]
            one: [P] -> [R]
        
        once: [R] -> [G]
        all: [R] -> [W]
        markov:
            all: [GWW] -> [..G]
            one: [GBW] -> [.WG]
        
        @limit NUM_CYCLES
        one: [GBG] -> [.W.]
        
        all: [G] -> [W]
        all: [BBB/BWB] -> [BBB/BBB]    
    `,
    
    MazesAndLakes: `
        # Author: Andrew Kay
        
        use let g = grid [BWREI]
        
        let LAKE_SEEDS = 4
        let LAKE_SIZE = g.width * g.height // 4
        let LAND_SEEDS = 32
        let ANIMATE_WATER = false
        
        @limit LAKE_SEEDS
        one: [B] -> [I]
        
        @limit LAKE_SIZE - LAKE_SEEDS
        one: [IB] -> [.I]
        
        @limit LAND_SEEDS
        one: [B] -> [R]
        
        markov:
            one: [RBB] -> [WWR]
            one: [RWW] -> [EER]
        
        one: [R] -> [E]
        
        one: [BBWBB] -> [..B..]
        
        @limit LAKE_SIZE // 2
        one: [II] -> [BB]
        
        markov:
            # fill unused space with a water texture
            one:
                [BB./BBB/.B.] -> [.../.I./...]
                [.I./IBI/.I.] -> [.../.I./...]
        
            # delete water pixels at random, for an animated effect
            one: [I] -> [B] if ANIMATE_WATER
    `,
});
