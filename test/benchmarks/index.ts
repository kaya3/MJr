///<reference path="../framework.ts"/>

Benchmark.add({
    GameOfLife: `
# Implementation of John Conway's "Game of Life"
# https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

grid [DA]

@limit 1
prl: [D] -> [A] if random < 0.5

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

    PacMan: `
# Author: Andrew Kay

grid [BWDPYIERO]

# draw maze
put [P] at origin
all: [PBB] -> [..P]
put [W] at origin
all: [WBPBP] -> [....W]
put [D] at origin
all: [DBPBW] -> [..W.D]
one: [DBPBDBP/BBBBBBB/.BPBPBP] -> [..W..../......./.......]

@limit 5
one: [DBPBD] -> [..W..]
all: [PBP] -> [.P.]
all: [D] -> [W]
one: [BBBBBB/BPBWBP/BBBBBB] -> [....../..PPP./......]

# add player and enemies
put [Y] at origin

@limit 4
one: [W] -> [O]

# play!

union [?] = [[WB]]

sequence:
    # mark unsafe moves
    all: [.BBB/[RO]B?B/.BBB] -> [..I./.I.I/..I.] if count [Y] > 0
    
    # allow player to backtrack if they have no safe move
    one: [E] -> [B] if count [YB?] == 0
    
    # allow player to make an unsafe move if they have no safe move
    one: [I] -> [B] if count [YB?] == 0
    
    # player moves
    @limit 1
    all:
        [YBW] -> [BEY]
        [YBB] -> [BEY] if count [YBW] == 0
        [[IE]] -> [B]
    
    # enemy eats player, if possible
    once:
        [RBY] -> [BBR]
        [OBY] -> [WBR]
    
    # enemies move
    @limit 1
    all:
        let alive = count [Y] > 0
        [RBW] -> [BDO] if alive
        [OBW] -> [WDO] if alive
        [RBB] -> [BDR] if alive
        [OBB] -> [WDR] if alive
        [D] -> [B]    
`,
});
