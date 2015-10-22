import qualified Numeric.Probability.Distribution as P
import qualified Numeric.Probability.Random as R
import Data.List

inputSymbols = [ "x1", "x2" ]
outputSymbols = [ "y1", "y2" ]

transition :: String -> P.T Double String
transition x = case x of 
    "x1" -> P.fromFreqs [ ("y1", 1.0), ("y2", 0.0) ]
    "x2" -> P.fromFreqs [ ("y1", 0.5), ("y2", 0.5) ]

-- Here is channel
channel = map (\y -> (y, transition y)) inputSymbols
dist x chan = case find (\y -> fst y == x) chan of
    Just d -> snd d
    Nothing -> error $ "Channel does not accept " ++ show x 

next x chan = R.run $ R.pick $ dist x chan

main = do
    {-solve4 >>= print -}
    putStrLn "Done"
