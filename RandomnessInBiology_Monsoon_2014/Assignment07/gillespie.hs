-- Doing Gillespie simulation.
--
-- We have two reactions.
--  A -> A + 1, rate = kf
--  A -> A - 1, rate = kb

import System.Random
import Graphics.Gnuplot.Simple

-- Global parameters.
data Global = Global {
    threshold :: Double
    } deriving Show 

data Event = Birth | Death deriving (Show, Eq)

global = Global 0.4
fI = fromIntegral

f x = ((12.0 + 200*1e-4*(fI x^2))/(1 + 200*1e-4*(fI x^2))) / fI x
g x = 1.0 

randomNums :: Int -> [Double]
randomNums n = randoms  (mkStdGen n)

birthRate :: Int -> Double
birthRate x = f x
deathRate :: Int -> Double
deathRate x = g x

-- This function determines which events should occur
-- First argument is no of moleculares, second one is a random no.
birthOrDeath :: Int -> Double -> Event
birthOrDeath x r | r < (f x)  = Death
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
          e = birthOrDeath (head n) (head rands)

plot filename traj = do
    putStrLn $ "Plotting file " ++ filename
    case filename of 
        "" -> plotList [] traj
        _ -> plotList [PNG filename] traj

-- Run the equation with seed n. n controls the random numbers generated.
run seed initX steps = reaction [initX] (take steps $ randomNums seed)

main = do
    let traj = run 0 10000 100000
    print traj
    plot "gillespie.png" traj
    putStrLn $ "Done doing simulation you puny human."

