///<reference path="../framework.ts"/>

Benchmark.setBaselines({
    "GameOfLife": {
        "codeSize": 3705,
        "compileTime": 0.656949999999255,
        "runTime": 157.95384615373152
    },
    "River": {
        "codeSize": 13142,
        "compileTime": 2.6124020887747914,
        "runTime": 17.435652173926
    },
    "BasicDungeonGrowth": {
        "codeSize": 408437,
        "compileTime": 139.92666666656731,
        "runTime": 8.52127659574468
    },
    "NystromDungeon": {
        "codeSize": 23056,
        "compileTime": 5.52011019284978,
        "runTime": 11.175
    },
    "MazesAndLakes": {
        "codeSize": 17462,
        "compileTime": 4.843099273614964,
        "runTime": 11.994610778407422
    }
});
