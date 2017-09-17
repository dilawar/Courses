{-# LANGUAGE BangPatterns #-}

import qualified Data.List as L

import Data.Compression.Huffman  -- cabal install huffman

tf = fromIntegral

probs :: Floating a => Int -> [a]
probs k = map (\i -> 2.0 ^^ (-i) ) [1..k]

probSpace = probs 5

plogp p = p * (logBase 2.0 p)

sortCode (i1, c1) (i2, c2) = compare i1 i2 

space :: Int -> [ [Int] ]
space n = [ [a,b,c,d,e] | a <- [0,1..n]
    , b <- [0,1..n-a]
    , c <- [0,1..n-a-b]
    , d <- [0,1..n-a-b-c] 
    , e <- [n-a-b-c-d] 
    ]


avg l = (tf $ sum l) / (tf $ length l)

codeToStr code = L.intercalate "" $ map show code

sumProb :: [Int] -> [Float] -> Float
sumProb [] [] = 0.0
sumProb (x:xs) (p:ps) = (tf x * p) + sumProb xs ps

{- computeEntropy :: Floating a => [a] -> [a] -> a -}
computeEntropy [ ] [ ] = 0.0
computeEntropy (x:xs) (p:ps) = - (tf x) * (plogp p) + computeEntropy xs ps

problem1 = map (\s -> (s, computeEntropy s probSpace) ) sol
    {- ( sol, probSpace ) -}
  where 
    sol = filter (\s -> (sumProb s probSpace) == 1.0) $ allSpace 
    allSpace = space 8 

toProbs a i =  replicate a (probSpace!!i) 

printLine (xs, p) = L.intercalate " | " (map show xs) ++ " | " ++ show p

printCode [ ] = ""
printCode ((i,c):xs) = show i ++ " : " ++ codeToStr c ++ "\n" ++ printCode xs

main = do
    let sol1 = problem1 
    let p = L.intercalate "\n" (map printLine sol1) 
    putStrLn p

    -- huffman code.
    let pbs = map ( \(x,p) -> zip [0..] (concat $ zipWith toProbs x [0..])) sol1
    let codes = map (codewords . huffman) pbs
    let sortedCodes = map (\l -> L.sortBy sortCode l) codes 
    let codesStr = L.intercalate "\n" $ map printCode $ sortedCodes
    let r = map (snd . unzip) sortedCodes
    let cs = map  (\l -> map length l) r
    -- print $ map avg cs
    -- putStrLn codesStr
    print cs
    putStrLn "All done"

