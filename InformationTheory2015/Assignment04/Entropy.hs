module Entropy where

import qualified Data.Map as M

buildMap [] m = m
buildMap (x:xs) m = buildMap xs (M.insertWith (+) x 1 m)

compute_entropy_of_dist xs = sum $ map (\x -> - x*(logBase 2 x)) xs

entropy seq = compute_entropy_of_dist $ M.elems probMap where 
    probMap = M.map (\y -> y / seqLength) counter
    counter = buildMap seq M.empty
    seqLength = fromIntegral $ length seq


