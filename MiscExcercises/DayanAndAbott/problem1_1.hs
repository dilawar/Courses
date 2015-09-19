-- Chapter 1, Problem 1

import GSL.Random.Dist
import GSL.Random.Gen
import System.Random

-- Following uses GSL
rate = 100
time = 10
rng = newRNG mt19937
next =  rng >>= \x -> getFlat x 0.0 1.0 
    
samples = time * rate
main = do
    g <- getStdGen
    print $ take samples (randoms g :: [Double])
