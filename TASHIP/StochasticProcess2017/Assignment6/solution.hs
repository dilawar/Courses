-- Solution to problem 6.

import Data.Random.Normal -- from normaldistribution
import Data.List
import System.Environment

-- We keep the variables here.
mByGamma = 1
ktbyM = 1

-- Compute next v
nextV alpha v dt = v - (v/mByGamma*dt) + alpha * ((2*ktbyM/mByGamma)**0.5) *(dt ** 0.5 )

{-simulateUsingLangevian :: [ Float ]-}
simulateUsingLangevian steps v0 dt = simulate alphas v0 0.0 dt
  where 
    alphas :: [ Double ]
    alphas = take steps $ mkNormals' (0, 1.0) steps      -- Using steps as seed!

simulate [ ] _ _ _ = [ ]
simulate (alpha:alphas) v0 t0 dt = [t0+dt,nv] : simulate alphas nv (t0+dt) dt 
  where 
    nv :: Double
    nv = nextV alpha v0 dt


-- Helper function. Write CSV file.
lineToStr line =  intercalate "," $ map show line
writeCsv datafile xs = do 
    let text = "time,velocity\n" ++ (unlines $ map lineToStr xs)
    writeFile datafile text

main = do
    args <- getArgs
    -- Args are stop, step and outfile name.
    let (stop:step:filename:[]) = args
    let nSteps = floor ( (read stop) / (read step))
    let trajectory = simulateUsingLangevian nSteps 0.0 0.1
    -- Save trajectory to a CSV file
    writeCsv filename trajectory
    putStrLn $ "All done"
