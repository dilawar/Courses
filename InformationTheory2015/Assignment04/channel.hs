import Data.List
import Data.Maybe
import qualified Numeric.Probability.Distribution as D

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
outputSymbols = [0..10]
transitions = [ (x, y) | x <- inputSymbols, y <- map (\a -> mod a 11) [x+1..x+3] ]
tranProbs = [ (a, 1/3) | a <- transitions ]

-- Channel
channel1 :: Channel Symbol
channel1 = Channel inputSymbols outputSymbols (D.fromFreqs tranProbs)

-- probs
--prob :: (Symbol, Symbol) -> Float
prob x = (==fst x) D.?? (transitionProbs channel1) 

--------------------------------------------------------------------------------
-- Simulate channel
--------------------------------------------------------------------------------

{-
-- Apply a single input to the channel and get the output.
--input :: Symbol -> Channel -> Symbol
input x ch = probOutput where 
    probOutput = map (\(a, x) -> (snd a, x)) $ filter (\(a, y) -> fst a == x) $ transitionProbs ch
    
-}

main = do
    {-solve1 >>= print .show -}
    print $ channel1 
    putStrLn "Done"
