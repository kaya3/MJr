///<reference path="./names.ts"/>

namespace IR {
    const {RNG} = NAMES;
    
    export const PRNG = {
        nextInt(n: Expr): Expr {
            return isInt(n) && n.value === 1 ? ZERO
                : libMethodCall('PRNG', 'nextInt', RNG, [n]);
        },
        
        nextIntChecked(n: Expr): Expr {
            return libFunctionCall('nextIntChecked', [RNG, n]);
        },
        
        NEXT_DOUBLE: libMethodCall('PRNG', 'nextDouble', RNG, []),
    };
}
