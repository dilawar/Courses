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
import System.Directory
import Data.Double.Conversion.ByteString
import Math.Gamma

divide :: Int -> Int -> Double
divide = (/) `on` fromIntegral

-- number of dimensions
dims ndims  = L.map (\x -> (-1.0, 1.0) ) [1,2..ndims]
gamma_factor ndims = gamma ((divide ndims 2) + 1)

volume2pi :: Double -> Int -> Double
volume2pi vol ndims  = ( vol * gamma_factor ndims ) ** (divide 2 ndims )

-- Before we can write to CSV file.
encodeDoubles x = L.map toShortest x

-- Very first thing we need is a random number generator (uniform distribution).
-- WARN: , it does not pick 0.0 
random_floats n = R.createSystemRandom >>= \gen -> R.uniformVector gen n 

-- To scale the random floats in (0, 1.0) to (min, max)
-- y = (max - min) x + min 
scale_samples :: (Double, Double) -> U.Vector Double -> U.Vector Double
scale_samples (min, max) vs = U.map (\v -> (max - min) * v + min ) vs

-- Sample a given space for n points.
sampled_space :: Int -> Int -> IO [ U.Vector Double ]
sampled_space n ndims = do 
    vecs <- Prelude.mapM (\x -> random_floats n) $ dims ndims 
    let scaledVecs = L.map (\x -> scale_samples (-1.0, 1.0) x) vecs
    return scaledVecs

-- Function to evaluate. It should acccept right number of arguments.
func xs
    | L.sum (L.map (**2.0) xs) <= 1.0 = True
    | otherwise = False

-- This is equivalent to transposing a matrix.
transpose vecs = get_points 0 (U.length $ Prelude.head vecs) where 
    get_points i len | i < len = column i : get_points (i+1) len
                     | otherwise = []
    column i = Prelude.map (\x -> x U.! i) vecs

-- These two function computes the fraction of points satisfying the function. 
monte_carlo_sampling f points = Prelude.filter f points 

monte_carlo_integration f points = 
    divide (Prelude.length (monte_carlo_sampling f points)) (Prelude.length points)

-- Compute pi using the monte_carlo_integration on a circle. Generate data to
-- plot the box plots.
monte_carlo_hypersphere_vol :: Int -> Int -> IO Double
monte_carlo_hypersphere_vol sample_size ndims = do 
    vs <- sampled_space sample_size ndims
    let points = transpose vs
    let vol = (2.0 ^ ndims) * monte_carlo_integration func points
    return vol

monte_carlo_hypersphere_vols :: Int -> a -> Int -> IO (Vector Double)
monte_carlo_hypersphere_vols nn sample_points ndims = do
    vols <- replicateM nn $ monte_carlo_hypersphere_vol nn  ndims
    return vols

compute_pi :: Int -> Int -> Int -> IO (Vector Double)
compute_pi n sample_points ndims = do 
    vols <- monte_carlo_hypersphere_vols n sample_points ndims
    let pis = U.map (\x -> volume2pi x ndims) vols 
    -- putStrLn $ "Computed pis: " ++ show pis
    return $ pis

csv_data xs = L.transpose $ L.map encodeDoubles xs

generate_mean_vars :: [Int] -> Int -> Int -> IO ([Double], [Double])
generate_mean_vars space repeat ndims = do 
    mat <- mapM (\x -> compute_pi repeat x ndims) space
    let (means, vars) = L.unzip $ Prelude.map meanVariance mat
    return $ (means, vars)

exp_variance_vs_ndims ndims = do 
    let space = L.map (\x -> floor $ 10.0**x) [1.0,1.05 .. 5.0]
    (means, vars) <- generate_mean_vars space 30 ndims
    let csvdata = csv_data [ Prelude.map fromIntegral space, means, vars ]
    let outfile = "_data/data_" ++ show ndims ++ ".csv" :: String
    writeCSVFile (CSVSettings ',' Nothing) outfile WriteMode csvdata
    putStrLn $ "Done writing data to " ++ outfile

main = do
    createDirectoryIfMissing True "_data"
    Prelude.mapM ( exp_variance_vs_ndims ) [2,3 .. 30]
    putStrLn "Done"

