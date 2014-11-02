-- Doing Gillespie simulation.
--
-- We have two reactions.
--  A -> A + 1, rate = kf
--  A -> A - 1, rate = kb

import System.Random

-- Global parameters.
data Global = Global {
    birthRate :: Double
    , deathRate :: Double
    , threshold :: Double
    } deriving Show 

data Event = Birth | Death deriving (Show, Eq)

global = Global 0.04 0.04 0.4

randomNums :: Int -> [Double]
randomNums n = randoms  (mkStdGen n)

-- This function determines which events should occur
birthOrDeath :: Double -> Event
birthOrDeath r | r < (threshold global) = Death
               | otherwise = Birth 

event :: Int -> Event -> Int
event n Birth = n + (floor $ (birthRate global) * fromIntegral n)
event n Death = n - (floor $ (deathRate global) * fromIntegral n)

reaction :: [Int] -> [Double] -> [Int]
reaction n [] = reverse n
reaction n rands 
    | head n <= 0 = reverse n
    | otherwise = reaction ((event (head n) e):n) (tail rands)
    where 
          e = birthOrDeath (head rands)

-- Run the equation with seed n. n controls the random numbers generated.
run n steps = reaction [100] (take steps $ randomNums n)

