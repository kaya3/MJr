var MJr;
(function (MJr) {
    // MJr runtime version 0 is unstable!
    MJr.VERSION = 0;
    MJr.DIV_ZERO_MESSAGE = 'division by zero';
    function checkZero(y) {
        if (y === 0) {
            throw new Error(MJr.DIV_ZERO_MESSAGE);
        }
        return y;
    }
    function modulo(x, y) {
        return x - y * Math.floor(x / y);
    }
    function fraction(p, q) {
        checkZero(q);
        // Euclid's algorithm
        let x = p, y = q;
        while (y !== 0) {
            x %= y;
            const tmp = x;
            x = y;
            y = tmp;
        }
        return { p: p / x, q: q / x };
    }
    MJr.fraction = fraction;
    MJr.OPS = {
        float_mod: modulo,
        float_checkzero: checkZero,
        fraction_plus: (x, y) => fraction(x.p * y.q + y.p * x.q, x.q * y.q),
        fraction_minus: (x, y) => fraction(x.p * y.q - y.p * x.q, x.q * y.q),
        fraction_mult: (x, y) => fraction(x.p * y.p, x.q * y.q),
        fraction_truediv: (x, y) => fraction(x.p * y.q, x.q * y.p),
        fraction_uminus: (x) => ({ p: -x.p, q: x.q }),
        fraction_eq: (x, y) => x.p === y.p && x.q === y.q,
        fraction_ne: (x, y) => x.p !== y.p || x.q !== y.q,
        fraction_lt: (x, y) => x.p * y.q < y.p * x.q,
        fraction_le: (x, y) => x.p * y.q <= y.p * x.q,
        fraction_gt: (x, y) => x.p * y.q > y.p * x.q,
        fraction_ge: (x, y) => x.p * y.q >= y.p * x.q,
        fraction_to_str: (x) => x.q === 1 ? `${x.p}` : `${x.p}/${x.q}`,
        int_truediv: fraction,
        int_floordiv: (x, y) => Math.floor(x / y) | 0,
        int_mod: (x, y) => modulo(x, y) | 0,
        int_ctz: (x) => 31 - Math.clz32(x & -x),
        int_checkzero: checkZero,
        int_to_fraction: (x) => ({ p: x, q: 1 }),
    };
    MJr.HEX = {
        u4(s) {
            const arr = new Uint8Array(s.length);
            for (let i = 0; i < s.length; ++i) {
                arr[i] = parseInt(s.substring(i, i + 1), 16);
            }
            return arr;
        },
        u8(s) {
            const arr = new Uint8Array(s.length >> 1);
            for (let i = 0; i < s.length; i += 2) {
                arr[i >> 1] = parseInt(s.substring(i, i + 2), 16);
            }
            return arr;
        },
        u12(s) {
            const n = (s.length / 3) | 0;
            const arr = new Uint16Array(n);
            for (let i = 0, j = 0; i < n; ++i, j += 3) {
                arr[i] = parseInt(s.substring(j, j + 3), 16);
            }
            return arr;
        },
        u16(s) {
            const arr = new Uint16Array(s.length >> 2);
            for (let i = 0; i < s.length; i += 4) {
                arr[i >> 2] = parseInt(s.substring(i, i + 4), 16);
            }
            return arr;
        },
        u20(s) {
            const n = (s.length / 5) | 0;
            const arr = new Uint32Array(n);
            for (let i = 0, j = 0; i < n; ++i, j += 5) {
                arr[i] = parseInt(s.substring(j, j + 5), 16);
            }
            return arr;
        },
        u24(s) {
            const n = (s.length / 6) | 0;
            const arr = new Uint32Array(n);
            for (let i = 0, j = 0; i < n; ++i, j += 6) {
                arr[i] = parseInt(s.substring(j, j + 6), 16);
            }
            return arr;
        },
        u28(s) {
            const n = (s.length / 7) | 0;
            const arr = new Uint32Array(n);
            for (let i = 0, j = 0; i < n; ++i, j += 7) {
                arr[i] = parseInt(s.substring(j, j + 7), 16);
            }
            return arr;
        },
        u32(s) {
            const arr = new Uint32Array(s.length >> 3);
            for (let i = 0; i < s.length; i += 8) {
                arr[i >> 3] = parseInt(s.substring(i, i + 8), 16);
            }
            return arr;
        },
    };
    MJr.DEFAULT_PRNG = {
        nextDouble: Math.random,
        nextInt: n => (Math.random() * n) | 0,
    };
    MJr.SAMPLE_EMPTY_MESSAGE = 'sample from empty range';
    function nextIntChecked(rng, n) {
        if (n <= 0) {
            throw new Error(MJr.SAMPLE_EMPTY_MESSAGE);
        }
        return rng.nextInt(n);
    }
    MJr.nextIntChecked = nextIntChecked;
    function lfsrFeedbackTerm(n) {
        // http://users.ece.cmu.edu/~koopman/lfsr/
        if (n < 0xFF) {
            return 0xA6;
        }
        else if (n < 0x3FF) {
            return 0x344;
        }
        else if (n < 0xFFF) {
            return 0xAF5;
        }
        else if (n < 0x3FFF) {
            return 0x243F;
        }
        else if (n < 0xFFFF) {
            return 0x8580;
        }
        else if (n < 0x3FFFF) {
            return 0x204C9;
        }
        else if (n < 0xFFFFF) {
            return 0x80534;
        }
        else if (n < 0x3FFFFF) {
            return 0x200634;
        }
        else if (n < 0xFFFFFF) {
            return 0x8009F8;
        }
        else if (n < 0x3FFFFFF) {
            return 0x20006B9;
        }
        else if (n < 0xFFFFFFF) {
            return 0x8000893;
        }
        else {
            return 0x20000A46;
        }
    }
    MJr.lfsrFeedbackTerm = lfsrFeedbackTerm;
    class Grid {
        width;
        height;
        data;
        alphabet;
        constructor(width, height, data, alphabet) {
            this.width = width;
            this.height = height;
            this.data = data;
            this.alphabet = alphabet;
        }
        index(x, y) {
            if (x < 0 || x >= this.width || y < 0 || y >= this.height) {
                throw new Error(`position out of bounds: (${x}, ${y})`);
            }
            return x + y * this.width;
        }
        wrapIndex(x, y) {
            return MJr.OPS.int_mod(x, this.width) + MJr.OPS.int_mod(y, this.height) * this.width;
        }
        toString() {
            const { width, data, alphabet } = this;
            const out = [];
            for (let i = 0; i < data.length; ++i) {
                if (i > 0 && i % width === 0) {
                    out.push('\n');
                }
                out.push(alphabet[data[i]]);
            }
            return out.join('');
        }
    }
    MJr.Grid = Grid;
    class Pattern {
        width;
        height;
        pattern;
        vectorData;
        minX;
        minY;
        effectiveWidth;
        effectiveHeight;
        constructor(width, height, pattern) {
            this.width = width;
            this.height = height;
            this.pattern = pattern;
            let minX = width, minY = height, maxX = 0, maxY = 0;
            const v = [];
            for (let y = 0; y < height; ++y) {
                for (let x = 0; x < width; ++x) {
                    const c = pattern[x + width * y];
                    if (c >= 128) {
                        continue;
                    }
                    v.push(x, y, c);
                    minX = Math.min(minX, x);
                    minY = Math.min(minY, y);
                    maxX = Math.max(maxX, x + 1);
                    maxY = Math.max(maxY, y + 1);
                }
            }
            this.vectorData = v;
            this.minX = minX;
            this.minY = minY;
            this.effectiveWidth = Math.max(maxX - minX, 0);
            this.effectiveHeight = Math.max(maxY - minY, 0);
        }
        fitsMask(grid, mask, atX, atY) {
            const v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                if ((mask[index >> 5] & 1 << (index & 31)) !== 0) {
                    return false;
                }
            }
            return true;
        }
        hasEffect(grid, atX, atY) {
            const g = grid.data, v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                if (g[index] !== v[i + 2]) {
                    return true;
                }
            }
            return false;
        }
        put(grid, mask, atX, atY) {
            const g = grid.data, v = this.vectorData;
            for (let i = 0; i < v.length; i += 3) {
                const index = grid.wrapIndex(atX + v[i], atY + v[i + 1]);
                g[index] = v[i + 2];
                if (mask !== undefined) {
                    mask[index >> 5] |= 1 << (index & 31);
                }
            }
        }
    }
    MJr.Pattern = Pattern;
    class RewriteInfo {
        grid;
        x;
        y;
        width;
        height;
        constructor(grid, x, y, width, height) {
            this.grid = grid;
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
        }
    }
    MJr.RewriteInfo = RewriteInfo;
    class Sampler {
        arr;
        indices;
        count = 0;
        constructor(domainSize) {
            const arr = new Uint32Array(domainSize);
            const indices = new Uint32Array(domainSize);
            for (let i = 0; i < domainSize; ++i) {
                arr[i] = indices[i] = i;
            }
            this.arr = arr;
            this.indices = indices;
        }
        copyInto(out) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                out[i] = arr[i];
            }
        }
        copyIntoOffset(out, offset, m, c) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                out[offset + i] = m * arr[i] + c;
            }
        }
        shuffleInto(out, rng) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                const j = rng.nextInt(i + 1);
                out[i] = out[j];
                out[j] = arr[i];
            }
        }
        shuffleIntoOffset(out, offset, m, c, rng) {
            const { arr, count } = this;
            for (let i = 0; i < count; ++i) {
                const j = rng.nextInt(offset + i + 1);
                out[offset + i] = out[j];
                out[j] = m * arr[i] + c;
            }
        }
        has(x) {
            return this.indices[x] < this.count;
        }
        add(x) {
            const { arr, indices, count } = this;
            const i = indices[x];
            if (i >= count) {
                const j = count;
                const y = arr[j];
                arr[j] = x;
                indices[x] = j;
                arr[i] = y;
                indices[y] = i;
                ++this.count;
            }
        }
        del(x) {
            const { arr, indices, count } = this;
            const i = indices[x];
            if (i < count) {
                const j = count - 1;
                const y = arr[j];
                arr[j] = x;
                indices[x] = j;
                arr[i] = y;
                indices[y] = i;
                --this.count;
            }
        }
        sample(max, rng) {
            const { arr, indices } = this;
            const i = rng.nextInt(max);
            const j = max - 1;
            const x = arr[i];
            const y = arr[j];
            arr[i] = y;
            indices[y] = i;
            arr[j] = x;
            indices[x] = j;
            return x;
        }
    }
    MJr.Sampler = Sampler;
})(MJr || (MJr = {}));
