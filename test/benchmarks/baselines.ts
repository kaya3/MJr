///<reference path="../framework.ts"/>

Benchmark.setBaselines({
    "GameOfLife": {
        "codeSize": 3705,
        "compileTime": 0.68075,
        "runTime": 161.27692307693025
    },
    "River": {
        "codeSize": 11876,
        "compileTime": 2.69811320754717,
        "runTime": 15.692187499998909
    },
    "BasicDungeonGrowth": {
        "codeSize": 407405,
        "compileTime": 140.47333333333955,
        "runTime": 8.89155555555597
    },
    "NystromDungeon": {
        "codeSize": 22010,
        "compileTime": 5.7612068965521255,
        "runTime": 11.283146067415208
    },
    "MazesAndLakes": {
        "codeSize": 16722,
        "compileTime": 5.02788944723583,
        "runTime": 10.569473684210282
    }
});
