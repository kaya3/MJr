import array
import random

# MJr runtime version 0 is unstable!
VERSION = 0

def int32(x):
    return ((x & 0xFFFFFFFF) ^ 0x80000000) - 0x80000000

def int_ctz(x):
    return (x & -x).bit_length() - 1

def hex_to_arr(typecode, chars_per_element, s):
    return array.array(typecode, [
        int(s[i:i + chars_per_element], 16)
        for i in range(0, len(s), chars_per_element)
    ])

class DefaultPRNG:
    def nextDouble(self):
        return random.random()
    def nextInt(self, n):
        return random.randint(0, n - 1)

SAMPLE_EMPTY_MESSAGE = 'sample from empty range';
def nextIntChecked(rng, n):
    if n <= 0:
        raise ValueError(SAMPLE_EMPTY_MESSAGE);
    return rng.nextInt(n)

def lfsrFeedbackTerm(n):
    # http://users.ece.cmu.edu/~koopman/lfsr/
    if n < 0xFF:
        return 0xA6
    elif n < 0x3FF:
        return 0x344
    elif n < 0xFFF:
        return 0xAF5
    elif n < 0x3FFF:
        return 0x243F
    elif n < 0xFFFF:
        return 0x8580
    elif n < 0x3FFFF:
        return 0x204C9
    elif n < 0xFFFFF:
        return 0x80534
    elif n < 0x3FFFFF:
        return 0x200634
    elif n < 0xFFFFFF:
        return 0x8009F8
    elif n < 0x3FFFFFF:
        return 0x20006B9
    elif n < 0xFFFFFFF:
        return 0x8000893
    else:
        return 0x20000A46

class Grid:
    def __init__(self, width, height, data, alphabet):
        self.width = width
        self.height = height
        self.data = data
        self.alphabet = alphabet
    
    def index(self, x, y):
        if x < 0 or x >= self.width or y < 0 or y >= self.height:
            raise ValueError(f'position out of bounds: ({x}, {y})');
        return x + y * self.width
    
    def wrapIndex(self, x, y):
        return (x % self.width) + (y % self.height) * self.width;
    
    def __str__(self):
        return '\n'.join([
            ''.join([
                self.alphabet[self.data[i]]
                for i in range(j, j + self.width)
            ])
            for j in range(0, self.width * self.height, self.width)
        ])

class Pattern:
    def __init__(self, width, height, pattern):
        self.width = width
        self.height = height
        self.pattern = pattern
        
        minX = width; minY = height; maxX = 0; maxY = 0
        v = []
        for y in range(height):
            for x in range(width):
                c = pattern[x + width * y]
                if c >= 128:
                    continue
                
                v.extend((x, y, c));
                minX = min(minX, x);
                minY = min(minY, y);
                maxX = max(maxX, x + 1);
                maxY = max(maxY, y + 1);
        
        self.vectorData = v
        self.minX = minX
        self.minY = minY
        self.effectiveWidth = max(maxX - minX, 0)
        self.effectiveHeight = max(maxY - minY, 0)
    
    def fitsMask(self, grid, mask, atX, atY):
        v = self.vectorData
        for i in range(0, len(v), 3):
            index = grid.wrapIndex(atX + v[i], atY + v[i + 1])
            if (mask[index >> 5] & 1 << (index & 31)) != 0:
                return False
        return True
    
    def hasEffect(self, grid, atX, atY):
        g = grid.data; v = self.vectorData
        for i in range(0, len(v), 3):
            index = grid.wrapIndex(atX + v[i], atY + v[i + 1])
            if g[index] != v[i + 2]:
                return True
        return False
    
    def put(self, grid, mask, atX, atY):
        g = grid.data; v = self.vectorData
        for i in range(0, len(v), 3):
            index = grid.wrapIndex(atX + v[i], atY + v[i + 1])
            g[index] = v[i + 2]
            if mask is not None:
                mask[index >> 5] |= 1 << (index & 31)

class RewriteInfo:
    def __init__(self, grid, x, y, width, height):
        self.grid = grid
        self.x = x
        self.y = y
        self.width = width
        self.height = height

class Sampler:
    def __init__(self, domainSize):
        self.count = 0
        self.arr = array.array('L', range(domainSize))
        self.indices = array.array('L', range(domainSize))
    
    def copyInto(self, out):
        out[:self.count] = self.arr[:self.count]
    
    def copyIntoOffset(self, out, offset, m, c):
        arr = self.arr
        for i in range(self.count):
            out[offset + i] = m * arr[i] + c
    
    def shuffleInto(self, out, rng):
        arr = self.arr
        for i in range(self.count):
            j = rng.nextInt(i + 1)
            out[i] = out[j]
            out[j] = arr[i]
    
    def shuffleIntoOffset(self, out, offset, m, c, rng):
        arr = self.arr
        for i in range(self.count):
            j = rng.nextInt(offset + i + 1)
            out[offset + i] = out[j]
            out[j] = m * arr[i] + c
    
    def has(self, x):
        return self.indices[x] < self.count;
    
    def add(self, x):
        arr = self.arr; indices = self.indices
        i = indices[x]
        if i >= self.count:
            j = self.count
            y = arr[j]
            arr[j] = x
            indices[x] = j
            arr[i] = y
            indices[y] = i
            self.count += 1
    
    def del_(self, x):
        arr = self.arr; indices = self.indices
        i = indices[x]
        if i < self.count:
            j = self.count - 1
            y = arr[j]
            arr[j] = x
            indices[x] = j
            arr[i] = y
            indices[y] = i
            self.count -= 1
    
    def sample(self, max_, rng):
        arr = self.arr; indices = self.indices
        i = rng.nextInt(max_)
        j = max_ - 1
        x = arr[i]
        y = arr[j]
        arr[i] = y
        indices[y] = i
        arr[j] = x
        indices[x] = j
        return x
