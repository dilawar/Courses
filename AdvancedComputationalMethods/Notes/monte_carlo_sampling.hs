{-
 - Monte Calro sampling.
 - It has nothing to do with Monte Carlo (code name given by Ulam and Neumann).
 -}

import qualified System.Random.MWC as R
import Data.Vector.Unboxed as U

-- Very first thing we need is a random number generator (uniform distribution).
random_floats :: Int -> IO (U.Vector Float)
random_floats n = R.create >>= \gen -> R.uniformVector gen n 

-- To scale the random floats in (0, 1.0) to (min, max)
-- y = (max - min) x + min 
scale_samples :: (Float, Float) -> U.Vector Float -> U.Vector Float
scale_samples (min, max) vs = U.map (\v -> (max - min) * v + min ) vs

-- Sample a given space for n points.
sampled_space :: [( Float, Float)] -> Int -> IO [ U.Vector Float ]
sampled_space axes n = 
    random_floats n >>= \vv -> return $ Prelude.map (\x -> scale_samples x vv ) axes 
    

main = do
    vs <- sampled_space [ (1,3), (-1,1) ] 100
    print vs
    putStrLn $ "Done"

