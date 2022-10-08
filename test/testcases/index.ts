///<reference path="../framework.ts"/>

Test.add('Basic', {
    blank: Test(`grid [BW]`, 3, 3, [
        Assert.equals('BBB\nBBB\nBBB'),
    ]),
    
    put: Test(`
        grid [BW]
        put [W] at origin
    `, 3, 3, [
        Assert.equals('BBB\nBWB\nBBB'),
    ]),
    
    one: Test(`
        grid [BW]
        one: [B] -> [W]
    `, 3, 3, [
        Assert.equals('WWW\nWWW\nWWW'),
    ]),
    
    all: Test(`
        grid [BW]
        all: [B] -> [W]
    `, 3, 3, [
        Assert.equals('WWW\nWWW\nWWW'),
    ]),
    
    neighbours: Test(`
        grid [BWR]
        put [W] at origin
        one: [WB] -> [.R]
    `, 3, 3, [
        Assert.equals('BRB\nRWR\nBRB'),
    ]),
});
