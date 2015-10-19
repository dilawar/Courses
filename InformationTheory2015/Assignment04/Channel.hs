import Data.List
import Data.Maybe
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Random as DR
import Entropy

type Symbol = Int

-- Channel type.
data Channel a = Channel { inSymbols :: [a]
    , outSymbols :: [a]
    , transitionProbs :: [(Symbol, D.T Float Symbol)]
} deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Let's construct our channel with input symbols (inputSymbols), output symbols
-- (outSymbols) and probability of a inputSymbols going to outSymbols.
inputSymbols = [0..10]
inputDist :: D.T Float Symbol
inputDist = D.uniform inputSymbols

inputDist2 :: D.T Float Symbol
inputDist2 = D.uniform [0..25]

outputSymbols = [0..10]
transitions = map (\x -> (x, D.uniform $ output x)) inputSymbols
    where output x = map (\y -> mod y 11) [(x+1)..(x+3)]

-- Channel
channel1 :: Channel Symbol
channel1 = Channel inputSymbols outputSymbols transitions

channel_textbook :: Channel Symbol
channel_textbook = Channel [0..25] [0..25] pbs where
    pbs = map ( \x -> (x, D.uniform [mod x 26, mod (x+1) 26]) ) [0..25]

--------------------------------------------------------------------------------
-- Functions on channel.
--------------------------------------------------------------------------------
next x channel = do
    let a = fromJust $ find (\(y, d) -> y == x) (transitionProbs channel)
    n <- DR.run $ DR.pick $ snd a
    return n
        
-- generate a input sequence of given length
inputSeq 0 d = return []
inputSeq n dist = do 
    xv <- inputSeq (n-1) dist
    x <- DR.run $ DR.pick $ dist
    return $! (x : xv)

simulate [] channel = return []
simulate (x:xs) channel = do
    xv <- simulate xs channel
    x <- next x channel
    return $! (x:xv) 

sim_channel1 n = do
    {-solve1 >>= print .show -}
    input <- inputSeq n $ inputDist
    output <- simulate input channel1
    let hx = entropy input
    let hy = entropy output
    let hxy = entropy (zip input output)
    print $ hx + hy - hxy
    putStrLn "Done"

sim_channel2 n = do
    input <- inputSeq n $ inputDist2
    output <- simulate input channel_textbook
    let hx = entropy input
    let hy = entropy output
    let hxy = entropy (zip input output)
    print $ hx + hy - hxy
    putStrLn "Done"
