{-
 - Monte Calro sampling.
 - It has nothing to do with Monte Carlo (code name given by Ulam and Neumann).
 -}

import qualified System.Random.MWC as R
import Data.Vector.Unboxed as U hiding ((++), mapM)
import Data.Function 
import qualified Data.List as L
import Numeric.LinearAlgebra.Data as LAD
import Graphics.Gnuplot.Simple

divide :: Int -> Int -> Float
divide = (/) `on` fromIntegral

-- Very first thing we need is a random number generator (uniform distribution).
-- NOTE, it does not pick 0.0 
random_floats :: Int -> IO (U.Vector Float)
random_floats n = R.createSystemRandom >>= \gen -> R.uniformVector gen n 

-- To scale the random floats in (0, 1.0) to (min, max)
-- y = (max - min) x + min 
scale_samples :: (Float, Float) -> U.Vector Float -> U.Vector Float
scale_samples (min, max) vs = U.map (\v -> (max - min) * v + min ) vs

-- Sample a given space for n points.
sampled_space :: [( Float, Float)] -> Int -> IO [ U.Vector Float ]
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
--monte_carlo_integration :: (Ord b, Floating b) => a -> [[ b ]] -> Float
monte_carlo_integration f points = 
    divide (Prelude.length (monte_carlo_sampling f points)) (Prelude.length points)

-- Compute pi using the monte_carlo_integration on a circle. Generate data to
-- plot the box plots.
monte_carlo_pi sample_size = do 
    vs <- sampled_space [ (-1,1), (-1,1) ] sample_size
    let points = transpose vs
    let pi = 4.0 * monte_carlo_integration func points
    return (sample_size, pi)

monte_carlo_pi_n_times n sample_points = do
    let nn = floor sample_points
    pis <- replicateM n $ monte_carlo_pi nn
    putStrLn $ "For sample size " ++ (show nn) ++ ": " ++ show pis
    return pis

main = do
    let space = Prelude.map (10**) [1.0,2.0..4.0]
    mat <- mapM (\x -> monte_carlo_pi_n_times 10 x) space
    --print $ L.transpose (Prelude.map U.toList mat)
    let mystyle = defaultStyle { lineSpec = CustomStyle [ LineTitle "" ] }
    plotListsStyle [ 
        Title "Monte Carlo"
        , Custom "logscale x 10" [] 
        ] (Prelude.map (\x -> (mystyle , U.toList x)) mat)
    putStrLn $ "Done"

