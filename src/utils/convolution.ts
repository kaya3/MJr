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
        
        public equals(other: Kernel): boolean {
            if(this === other) { return true; }
            return this.width === other.width
                && this.height === other.height
                && this.data.every((x, i) => x === other.data[i]);
        }
    }
    
    export type KernelName = keyof typeof KERNELS
    
    export const KERNELS = {
        Moore: new Kernel(3, 3, [1, 1, 1, 1, 0, 1, 1, 1, 1]),
        VonNeumann: new Kernel(3, 3, [0, 1, 0, 1, 0, 1, 0, 1, 0]),
    };
}
