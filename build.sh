#!/bin/sh

tsc -p src/
tsc -p test/

mv out/*.js .

tsc src/runtime/mjr.ts --target ESNext --outFile mjr-runtime.js
