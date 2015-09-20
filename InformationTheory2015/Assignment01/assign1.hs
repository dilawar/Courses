-- Problem 1 in Haskell
import Data.List

-- These are useless for now.
data Horse int = Horse { id :: Int } deriving Show
horses = map (\x -> Horse x ) $ [ 1.. 8 ]


partitionxy n = map (\x -> (x, n-x)) $ [1..n-1]
-- partitionxyz n = map (\x -> map (\y -> (x, fst y, snd y)) $ (partitionxy (n-x))) [1..n-1]
partitionxyz n = map (\x -> map (\y -> (fst x, fst y, snd y)) (partitionxy $ snd x)) $ (partitionxy n)

-- For a given number n, return a list of partitions with element (x,y,z) such 
-- that x + y + z = n and given probability distribution (px, py, pz) is
-- satisfied i.e. x*px + y*py + z*pz == 1.0.
valid_partitions n prob_dist = filter (\x -> prob_dist_valid x prob_dist) (concat $ partitionxyz n)
prob_dist_valid (x, y, z) (px, py, pz) 
    | x*px + y*py + z*pz == 1.0 = True
    | otherwise = False

-- Generate all subsets of cardinatlity 3 from a given list. (x, y, z) == (x, z,
-- y) == (z, y, x) etc.
subsets3 [] = []
subsets3 (x:xs) = map (\y -> (x, fst y, snd y)) (subsets2 xs) ++ subsets3 xs
subsets2 [] = []
subsets2 (x:xs) = [ (x, y) | y <- xs ] ++ subsets2 xs

-- Use the above subset generating function to generate all possible probability
-- distributions.
all_dists = subsets3 probs where probs = map (\x -> 1/2**x) [1..8]


-- Return all possible sets (x, y, z: x + y + z = n) which also satisfy
-- valid_partitions function.
all_partitions n = map (\x -> (x, valid_partitions n x)) all_dists

draw [] = ""
draw (x:xs) | (length $ snd x) == 0 = draw xs
            | otherwise = print_row x ++ "\n" ++ draw xs
print_row row = show row

parta = putStrLn $ draw $ all_partitions 8
