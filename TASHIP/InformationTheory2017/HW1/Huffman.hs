module Huffman where 

import qualified Data.List as L
import qualified Data.Graph as G

data Bit = Zero | One deriving (Show,Ord,Eq)

-- Huffman tree. 
data HTree = Branch Double (HTree, HTree) | Leaf  Double deriving (Show, Eq, Ord)

nodeVal (Branch x (l,h)) = x
nodeVal (Leaf x) = x

compareHt x1 x2 = compare (nodeVal x1 ) (nodeVal x2)

-- Convert HTree to String.
toString (Leaf d) = show d
toString (Branch p (l,r)) = "<" ++  toString l ++ "," ++ toString r ++ ">"

-- Step: merge pair of HTree into one HTree
step [ ] = []
step (h:[]) = [h]
step (h1:h2:hs) = (Branch (nodeVal h1 + nodeVal h2) (h1, h2)) : merge hs

merge hs | (length hs) > 1 = merge $ step hs -- merge $ L.sortBy compareHt (step hs)
         | otherwise = hs

-- Test function.
test x = merge initHuffman 
  where 
    initHuffman = map (\a -> Leaf a) $ L.sortBy (compare) x

main = do
    let probs = [ 1/2, 1/4 ] ++ replicate 2 (1/16)  ++ replicate 4 (1/32)
    print $ probs
    let res = test probs 
    print $ toString $ head res
    putStrLn "All done"
