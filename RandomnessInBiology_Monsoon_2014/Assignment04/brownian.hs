{-# LANGUAGE NoMonomorphismRestriction #-}
-- Brownian

import Statistics.Distribution.Normal
import System.Random.MWC
import Statistics.Distribution

alphaDist = normalDistr 0.0 1.0 

alpha gen 0 = []
alpha gen num = (genContinous alphaDist gen) : alpha gen (num-1)

getAlpha = do 
    gen <- create 
    let as = alpha gen 10
    return as
