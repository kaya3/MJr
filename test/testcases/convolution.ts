///<reference path="../framework.ts"/>

Test.add('Convolution', {
    emptySum: Test(`
        grid [BW]
        convolution {kernel="Moore"}:
            [B] -> [W] if sum [W] == 0
    `, 3, 3, [
        Assert.equals('WWW\nWWW\nWWW'),
    ]),
    
    oneSum: Test(`
        grid [BW]
        put [W] at origin
        convolution {kernel="Moore"}:
            [B] -> [W] if sum [W] == 1
            [W] -> [B] if sum [B] == 8
    `, 3, 3, [
        Assert.equals('WWW\nWBW\nWWW'),
    ]),
    
    boundary: Test(`
        grid [BW035]
        convolution {kernel="Moore", boundary=[W]}:
            [B] -> (let s = sum [W] in
                [0] if s == 0 else [3] if s == 3 else [5] if s == 5 else [.])
    `, 4, 4, [
        Assert.equals('5335\n3003\n3003\n5335'),
    ]),
});
