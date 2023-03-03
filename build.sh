#!/bin/sh

tsc -p src/
tsc -p test/

mv out/*.js .

tsc src/runtime/mjr.ts --target ESNext --declaration --outFile runtime/MJr.js
