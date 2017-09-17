module Huffman where 

import qualified Data.List as L

data HTree = Branch HTree HTree | Leaf Int deriving (Show, Eq, Ord)

huffman ps = huffman' (L.length ps)

huffman' 0 = Leaf 0
huffman' n = Branch (huffman' (n-1)) (huffman' (n-1))

-- Test function.
test x = huffman $ L.sort x

main = do
    let res = test $ [ 1/2, 1/4 ] ++  replicate 2 (1/16)  ++ replicate 4 (1/32)
    print $ res
