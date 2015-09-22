#!/bin/bash
set -e
ghc -prof -rtsopts ./problem2.hs -o prob2
./prob2 +RTS -p
