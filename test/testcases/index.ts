///<reference path="../framework.ts"/>

runTests('Basic', {
    blank: T(`grid [BW]`, 3, 3, [
        A.equals('BBB\nBBB\nBBB'),
    ]),
    put: T(`
grid [BW]
put [W] at origin`, 3, 3, [
        A.equals('BBB\nBWB\nBBB'),
    ]),
    one: T(`
grid [BW]
one: [B] -> [W]`, 3, 3, [
        A.equals('WWW\nWWW\nWWW'),
    ]),
    all: T(`
grid [BW]
all: [B] -> [W]`, 3, 3, [
        A.equals('WWW\nWWW\nWWW'),
    ]),
});
