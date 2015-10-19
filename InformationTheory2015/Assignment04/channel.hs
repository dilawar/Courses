import Data.List
import Data.Maybe
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Random as DR
import Entropy

type Symbol = Int

-- Channel type.
data Channel a = Channel { inSymbols :: [a]
    , outSymbols :: [a]
    -- Given a (inSymbols, outSymbols) pair, what is the transition prob.
    -- Ideally it should be one.
    , transitionProbs :: D.T Float (Symbol, Symbol)
} deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Let's construct our channel with input symbols (inputSymbols), output symbols
-- (outSymbols) and probability of a inputSymbols going to outSymbols.
inputSymbols = [0..10]
inputDist :: D.T Float Symbol
inputDist = D.uniform inputSymbols

outputSymbols = [0..10]
transitions = [ (x, y) | x <- inputSymbols, y <- map (\a -> mod a 11) [x+1..x+3] ]
tranProbs = [ (a, 1/3) | a <- transitions ]

-- Channel
channel1 :: Channel Symbol
channel1 = Channel inputSymbols outputSymbols (D.fromFreqs tranProbs)

-- Given a symbol find the next possible symbols.
possibleOutputs :: Symbol -> [(Symbol, Float)]
possibleOutputs x = map (\(e, p) -> (snd e, p)) $ filter (\(e, p) -> fst e == x) tranProbs

-- Given a symbol, pick a one output symbol depending on the given
-- probabilities.
transition x = DR.run $ DR.pick $ D.fromFreqs $ possibleOutputs x

--------------------------------------------------------------------------------
-- Simulate channel
--------------------------------------------------------------------------------

-- generate a input sequence of given length
inputSeq 0 = return []
inputSeq n = do 
    xv <- inputSeq (n-1)
    x <- DR.run $ DR.pick $ inputDist
    return $! (x : xv)

simulate [] = return []
simulate (x:xs) = do
    xv <- simulate xs
    x <- transition x
    return $! (x:xv) 

main = do
    {-solve1 >>= print .show -}
    input <- inputSeq 100000
    output <- simulate $ input
    print $ entropy input
    print $ entropy output
    putStrLn "Done"
