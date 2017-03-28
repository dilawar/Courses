-- Solution to problem 6.

import Data.Random.Normal -- from normaldistribution
import System.Random
import Data.List
import System.Environment

-- We keep the variables here.
mByGamma = 1
ktbyM = 1


simulateUsingLangevian v0 steps dt = do 
    g <- getStdGen
    let alphas = take steps $ normals' (0, 1.0) g 
    return $ simulate alphas 0.0 v0 0.0 dt

-- Compute next v
nextV alpha v dt = v - (v/mByGamma*dt) + alpha * ((2*ktbyM/mByGamma)**0.5) *(dt ** 0.5 )

-- Simulate
simulate [ ] _ _ _ _ = [ ]
simulate (a:as) x0 v0 t0 dt = [t0+dt,nv,nx] : simulate as nx nv (t0+dt) dt 
  where 
    nv :: Double
    nv = nextV a v0 dt
    nx :: Double
    nx = x0 + nv * dt


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
    let (stop:step:velocity:filename:[]) = args
    let nSteps = floor $ (toDouble stop) / (toDouble step)
    let v0 = toDouble velocity
    trajectory <- simulateUsingLangevian v0 nSteps (toDouble step)
    -- Save trajectory to a CSV file
    writeCsv filename trajectory
    putStrLn $ "Wrote data to " ++ filename
