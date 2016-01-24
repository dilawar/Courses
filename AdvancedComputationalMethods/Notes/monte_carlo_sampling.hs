{-
 - Monte Calro sampling.
 - It has nothing to do with Monte Carlo (code name given by Ulam and Neumann).
 -}

import qualified System.Random.MWC as R
import Data.Vector.Unboxed as U

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

monte_carlo_sampling f vecs = Prelude.filter func points  where
    points = get_points 0 (U.length $ Prelude.head vecs)  
    -- This is equivalent to transposing a matrix.
    get_points i len | i < len = column i : get_points (i+1) len
                     | otherwise = []
    column i = Prelude.map (\x -> x U.! i) vecs

main = do
    vs <- sampled_space [ (-1,1), (-1,1) ] 10
    print $ vs
    let p = monte_carlo_sampling func vs
    print $ p
    {-print $ monte_carlo_sampling func vs-}
    putStrLn $ "Done"

