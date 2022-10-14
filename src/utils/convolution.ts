namespace Convolution {
    export class Kernel {
        public static key(kernel: Kernel): string {
            return kernel._key ??= `${kernel.width}x${kernel.height}:${kernel.data.join('')}`;
        }
        
        private _key: string | undefined = undefined;
        
        public readonly centreX: number;
        public readonly centreY: number;
        
        public constructor(
            public readonly width: number,
            public readonly height: number,
            public readonly data: readonly (0 | 1)[],
        ) {
            this.centreX = width >> 1;
            this.centreY = height >> 1;
        }
        
        public forEach(f: (dx: number, dy: number, value: number) => void): void {
            const {width, height, data} = this;
            for(let dy = 0; dy < height; ++dy) {
                for(let dx = 0; dx < width; ++dx) {
                    const value = data[dx + width * dy];
                    if(value !== 0) { f(dx, dy, value); }
                }
            }
        }
        
        public equals(other: Kernel): boolean {
            if(this === other) { return true; }
            return this.width === other.width
                && this.height === other.height
                && this.data.every((x, i) => x === other.data[i]);
        }
        
        public total(): number {
            let t = 0;
            for(const x of this.data) { t += x; }
            return t;
        }
        
        public boundaryValues(): number[] {
            const {width, height, centreX, centreY, data} = this;
            const out = emptyArray(data.length, 0);
            
            this.forEach((dx, dy) => {
                dx -= centreX; dy -= centreY;
                
                for(let y = 0; y < height; ++y) {
                    const diffY = y - centreY;
                    const yOK = dy < 0 && diffY <= dy
                        || dy > 0 && diffY >= dy;
                    for(let x = 0; x < width; ++x) {
                        const diffX = x - centreX;
                        const xOK = dx < 0 && diffX <= dx
                            || dx > 0 && diffX >= dx;
                        if(xOK || yOK) {
                            ++out[x + width * y];
                        }
                    }
                }
            });
            return out;
        }
    }
    
    export type KernelName = keyof typeof KERNELS
    
    export const KERNELS = {
        Moore: new Kernel(3, 3, [1, 1, 1, 1, 0, 1, 1, 1, 1]),
        VonNeumann: new Kernel(3, 3, [0, 1, 0, 1, 0, 1, 0, 1, 0]),
    };
}
