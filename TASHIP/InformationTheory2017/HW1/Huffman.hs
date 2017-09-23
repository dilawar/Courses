module Huffman where 

import qualified Data.List as L
import qualified Data.Tree as T

data Bit = Zero | One deriving (Show,Ord,Eq)

-- Huffman tree. 
data HTree = Branch Double (HTree, HTree) | Leaf Double deriving (Show, Eq, Ord)

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

-- Enumerate all paths of a graph.
paths hs = paths' hs [ ] 0
  where 
    paths' (Branch x (l,r)) vs vid = paths' l (vid:vs) (vid+1) ++  paths' r (vid:vs) (vid+2) 
    paths' (Leaf x) vs vid = [ vid ]

edges hs = L.nub $ edges' hs 0 [ ]
  where 
    edges' (Leaf x) id es = es
    edges' (Branch x (l,r)) id es = edges' l (id+2) ess ++ edges' r (id+3) ess 
      where 
        ess = (id,id+1):(id,id+2):es

merge hs | (length hs) > 1 = merge $ step hs -- merge $ L.sortBy compareHt (step hs)
         | otherwise = hs

-- Test function.
test x = merge initHuffman 
  where 
    initHuffman = map (\a -> Leaf a) $ L.sortBy (compare) x

main = do
    let probs = [ 1/2, 1/4 ] ++ replicate 2 (1/16)  ++ replicate 4 (1/32)
    -- let probs = [1/3, 1/3, 1/3]
    print $ probs
    let g = head $ test probs 
    print $ toString g
    print $ edges g
    putStrLn "All done"
