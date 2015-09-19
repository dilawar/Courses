-- Problem 1 in Haskell

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

probs = map (\x -> 1/2**x) [1..8]
prob_dists = [(1/2, 1/4, 1/8), (1/4, 1/8, 1/16), (1/8, 1/16, 1/32), (1/16, 1/32, 1/64)]

all_partitions n = map (\x -> valid_partitions n x) prob_dists
