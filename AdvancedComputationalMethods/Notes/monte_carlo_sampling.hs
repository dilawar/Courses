{-
 - Monte Calro sampling.
 - It has nothing to do with Monte Carlo (code name given by Ulam and Neumann).
 -}

import qualified System.Random.MWC as R
import Data.Vector.Unboxed

-- Very first thing we need is a random number generator (uniform distribution).
random_floats :: Int -> IO (Vector Float)
random_floats n = R.create >>= \gen -> R.uniformVector gen n 

-- 

main = do
    vs <- random_floats 100
    print $ vs 
    putStrLn $ "Done"

