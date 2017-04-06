{-# LANGUAGE DuplicateRecordFields  #-}

-- Solution to problem 6.

import Data.Random.Normal -- from normaldistribution
import System.Random
import Data.List
import System.Environment

-- We keep the variables here.
mByGamma = 1
ktbyM = 1

data Molecule = Molecule { name :: String, n0 :: Int } deriving Show 

data Reac = Reac { name :: String
                , subs :: [ Molecule ],  prds :: [ Molecule ]
                , kf :: Double, kb :: Double 
                } deriving Show 

data System = System { name :: String, reacs :: [ Reac ] } deriving Show 

a = Molecule "a" 100
phi = Molecule "phi" 0
rf = Reac "forward" [a] [phi] 1.0 0.0
rb = Reac "backward" [phi] [a] 0.01 0.0

-- Create a system.
sys = System "Assign7" [rf, rb ]

gillespie :: System -> Double -> Double -> IO ( System )
gillespie sys dt t = do
    g <- getStdGen
    let n = floor ( t / dt )
    let rs = take n $ normals' (0,1.0) g
    step sys rs

-- whichReac r sys = do 
    

step :: System -> [ Double ] -> IO System 
step sys (r1:r2:rs) = do
    reac <- whichReac r1 sys
    return sys
    

-- Helper function. Write CSV file.
lineToStr :: [ Double ] -> String
lineToStr line =  intercalate "," $ map show line
writeCsv datafile xs = do 
    let text = "time,velocity,position\n" ++ (unlines $ map lineToStr xs)
    writeFile datafile text

toDouble :: String -> Double
toDouble x = read x

main = do
    args <- getArgs
    -- Args are stop, step, v0 and outfile name.
    let (t:dt:filename:[]) = args
    let step = toDouble dt
    let stop = toDouble t
    trajectory <- gillespie sys step stop
    print $ trajectory
    -- Save trajectory to a CSV file
    -- writeCsv filename trajectory
    -- putStrLn $ "Wrote data to " ++ filename
    putStrLn $ "All done"
