{-
 - Monte Calro sampling.
 - It has nothing to do with Monte Carlo (code name given by Ulam and Neumann).
 -}

import qualified System.Random.MWC as R
import Data.Vector.Unboxed as U hiding ((++), mapM)
import Data.Function 
import qualified Data.List as L
import Statistics.Sample
import Data.CSV.Conduit
import System.IO
import Data.Double.Conversion.ByteString

divide :: Int -> Int -> Double
divide = (/) `on` fromIntegral

encodeDoubles x = L.map toShortest x

-- Very first thing we need is a random number generator (uniform distribution).
-- NOTE, it does not pick 0.0 
--random_floats :: Int -> IO (U.Vector )
random_floats n = R.createSystemRandom >>= \gen -> R.uniformVector gen n 

-- To scale the random floats in (0, 1.0) to (min, max)
-- y = (max - min) x + min 
scale_samples :: (Double, Double) -> U.Vector Double -> U.Vector Double
scale_samples (min, max) vs = U.map (\v -> (max - min) * v + min ) vs

-- Sample a given space for n points.
sampled_space :: [( Double, Double)] -> Int -> IO [ U.Vector Double ]
sampled_space [] n = return []
sampled_space (ax:axes) n = do
    vv <- random_floats n 
    let v = scale_samples ax vv 
    vs <- sampled_space axes n
    return $! v:vs

-- Function to evaluate. It should acccept right number of arguments.
func (x:y:[]) 
    | x ** 2 + y ** 2 <= 1 = True
    | otherwise = False

-- This is equivalent to transposing a matrix.
transpose vecs = get_points 0 (U.length $ Prelude.head vecs) where 
    get_points i len | i < len = column i : get_points (i+1) len
                     | otherwise = []
    column i = Prelude.map (\x -> x U.! i) vecs

-- These two function computes the fraction of points satisfying the function. 
monte_carlo_sampling f points = Prelude.filter func points 
--monte_carlo_integration :: (Ord b, Doubleing b) => a -> [[ b ]] -> Double
monte_carlo_integration f points = 
    divide (Prelude.length (monte_carlo_sampling f points)) (Prelude.length points)

-- Compute pi using the monte_carlo_integration on a circle. Generate data to
-- plot the box plots.
monte_carlo_pi sample_size = do 
    vs <- sampled_space [ (-1,1), (-1,1) ] sample_size
    let points = transpose vs
    let pi = 4.0 * monte_carlo_integration func points
    return pi

monte_carlo_pi_n_times n sample_points = do
    let nn = floor sample_points
    pis <- replicateM n $ monte_carlo_pi nn
    putStrLn $ "For sample size " ++ (show nn) ++ ": " ++ show pis
    return pis

csv_data xs = L.transpose $ L.map encodeDoubles xs

main = do
    let space = Prelude.map (10**) [1.0,1.5..2.0] :: [Double]
    mat <- mapM (\x -> monte_carlo_pi_n_times 10 x) space
    let (means, vars) = L.unzip $ Prelude.map meanVariance mat
    let csvdata = csv_data [ space, means, vars ]
    let outfile = "data.csv" :: String
    writeCSVFile (CSVSettings ',' Nothing) outfile WriteMode csvdata
    putStrLn $ "Done writing data to " ++ outfile
