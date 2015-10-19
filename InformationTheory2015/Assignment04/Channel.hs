import Data.List
import Data.Maybe
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Random as DR
import Entropy

type Symbol = Int

-- Channel type.
data Channel a = Channel { inSymbols :: [a]
    , outSymbols :: [a]
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

--------------------------------------------------------------------------------
-- Simulate channel
--------------------------------------------------------------------------------
transition x channel = DR.run $ DR.pick $ transitionProbs channel

-- generate a input sequence of given length
inputSeq 0 d = return []
inputSeq n dist = do 
    xv <- inputSeq (n-1) dist
    x <- DR.run $ DR.pick $ dist
    return $! (x : xv)

simulate [] channel = return []
simulate (x:xs) channel = do
    xv <- simulate xs channel
    x <- transition x channel
    return $! (x:xv) 

main = do
    {-solve1 >>= print .show -}
    input <- inputSeq 10000 $ inputDist
    output <- simulate input channel1
    let hx = entropy input
    let hy = entropy output
    let hxy = entropy (zip input output)
    print $ hx + hy - hxy
    putStrLn "Done"
