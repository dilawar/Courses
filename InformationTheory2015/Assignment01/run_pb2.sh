#!/bin/bash
set -e
ghc -O2 --make ./problem2.hs -prof -auto-all -caf-all -fforce-recomp -o prob2
time ./prob2 +RTS -p
