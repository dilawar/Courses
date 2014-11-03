-- Doing Gillespie simulation.
--
-- We have two reactions.
--  A -> A + 1, rate = kf
--  A -> A - 1, rate = kb

import System.Random

-- Global parameters.
data Global = Global {
    threshold :: Double
    } deriving Show 

data Event = Birth | Death deriving (Show, Eq)

global = Global 0.4
fI = fromIntegral

randomNums :: Int -> [Double]
randomNums n = randoms  (mkStdGen n)

birthRate :: Int -> Double
birthRate x = (12.0 + 200*1e-4*(fI x^2))/(1 + 200*(fI x^2)) * (1 / fI x)
deathRate :: Int -> Double
deathRate x = 2/fI x
-- This function determines which events should occur
birthOrDeath :: Double -> Event
birthOrDeath r | r < (threshold global) = Death
               | otherwise = Birth 

event :: Int -> Event -> Int
event n Birth = n + (floor $ (birthRate n) * fI n)
event n Death = n - (floor $ (deathRate n) * fI n)

reaction :: [Int] -> [Double] -> [Int]
reaction n [] = reverse n
reaction n rands 
    | head n <= 0 = reverse n
    | otherwise = reaction ((event (head n) e):n) (tail rands)
    where 
          e = birthOrDeath (head rands)

-- Run the equation with seed n. n controls the random numbers generated.
run seed initX steps = reaction [initX] (take steps $ randomNums seed)

