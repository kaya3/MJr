#!/bin/sh

tsc -p src/
tsc -p test/

mv out/*.js .
