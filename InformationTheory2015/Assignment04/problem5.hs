import qualified Numeric.Probability.Distribution as P
import qualified Numeric.Probability.Random as R
import Data.List
import System.Random

inputSymbols = [ '0', '1' ]
outputSymbols = [ '0', '1' ]

type Channel = [(Char, P.T Double Char)]

transition :: Char -> P.T Double Char
transition x = case x of 
    '0' -> P.fromFreqs [ ('0', 1.0), ('1', 0.0) ]
    '1' -> P.fromFreqs [ ('0', 0.5), ('1', 0.5) ]

-- Here is channel
channel = map (\y -> (y, transition y)) inputSymbols
dist x chan = case find (\y -> fst y == x) chan of
    Just d -> snd d
    Nothing -> error $ "Channel does not accept " ++ show x 

output :: Char -> Channel -> IO Char
output x chan = R.run $ R.pick $ dist x chan

inputDist :: P.T Double Char
inputDist =  P.fromFreqs [ ('0', 0.5), ('1', 0.5) ]
input_seq 0 = return []
input_seq n = do 
    v <- R.run $ R.pick inputDist 
    vs <- input_seq (n-1)
    return (v:vs)

-- Check if input i and output string o has error. Return the number of places
-- where error has occured.
check i o =  sum $ map is_error (zip i o) where 
    is_error (x, y) 
        | x == y = 0.0
        | otherwise = 1.0

-- Send a stream over channel. Get the output stream as well as the error rate.
apply :: [Char] -> Channel -> IO [Char]
apply [] chan = return []
apply (i:is) channel = do
    x <- output i channel 
    xv <- apply is channel
    return (x:xv)

simulate n l = do 
    input <- mapM (\x -> input_seq n) [1..l]
    output <- mapM (\x -> apply x channel) input
    let csvText = unlines $ map (\(x, y) -> show x ++ "," ++ show y) $ zip input output
    writeFile ("_data/output_"++show n++"_"++show l++".csv") csvText
    putStrLn "Done writing data to a file"

main = do
    simulate 3 100
    putStrLn "Done"
