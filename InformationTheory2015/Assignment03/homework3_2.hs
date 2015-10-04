-- Second problem
import Control.Monad.State
import System.Random
import qualified Data.Vector as V

data Morse = Dot | Dash | LSpace | WSpace deriving (Show, Ord, Eq)
states = [Dash, Dot, LSpace, WSpace]
all_states = [ (x, y) | x <- states, y <- states ]
transition_mat = [
        [ 1/4, 1/4, 1/4, 1/4 ], [1/4, 1/4, 1/4, 1/4 ]
        , [1/2, 1/2, 0, 0 ], [1/2, 1/2, 0, 0 ]
    ]

markov n = do
    g <- getStdGen
    return $ take n $ randomRs (0, 1000) g

main = do
    putStrLn "Done"
