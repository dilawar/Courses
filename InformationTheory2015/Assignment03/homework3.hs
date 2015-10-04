-- Homework 3.

import Data.List

import qualified Numeric.Probability.Distribution as P
import Control.Monad

data Neucleotide = A | T | C | G deriving (Show,Eq,Ord)

-- Type DNA is synonym for list of Neucleotides
type DNA = [ Neucleotide ]

neucleotide_dist :: P.T Double Neucleotide
neucleotide_dist = P.fromFreqs [(A, 0.5), (T, 0.25), (G,1/8), (C,1/8)]

-- Create a distribution of length 8 dna sequence when neucleotide distribution
-- is given.

n = 8 :: Int
fI = fromIntegral

fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

dna_dist :: P.T Double DNA
dna_dist = replicateM n neucleotide_dist

entropy :: P.T Double a -> Double
entropy dist = sum $ map (\(a,x) -> - x * logBase 2 x) $ P.decons dist
dna_entropy = entropy dna_dist

prob_p :: Double
prob_p = 1.0/2**(entropy neucleotide_dist * (fromIntegral n))

-- Solve 1.2
problem1c' = length $ filter (\(a, p) -> p == prob_p) $ P.decons dna_dist

possible_nnuc = filter (\x -> sum x == n) [ 
    [a, t, c, g] | a <- [0..n], t <- [0..n-a], c <- [0..n-a-t], g <- [0..n-a-t-c] 
    ] 

-- How many ways to do it? There are total 2716 number of ways when n = 8.
problem1c = sum $ count_ways $ filter (\(a,p) -> p == prob_p) $ prob_of_n_nucs possible_nnuc
count_ways [] = []
count_ways ((x,a):xs) = (div (fact $ sum x) (foldr (*) 1 $ map fact x)) : count_ways xs

prob_of_n_nucs [] = []
prob_of_n_nucs x = map (\ns ->(ns,(1/2^ns!!0)*(1/4^ns!!1)*(1/8^ns!!2)*(1/8^ns!!3))) x
 
main = do
    let a = problem1c 
    putStrLn "Done"
