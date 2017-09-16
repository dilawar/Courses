import qualified Data.List as L


tf = fromIntegral

probs :: Floating a => Int -> [a]
probs k = map (\i -> 2.0 ^^ (-i) ) [1..k]

space :: Int -> [ [Int] ]
space n = [ 
    [a,b,c,d,e] | a <- [0,1..n]
    , b <- [0,1..n-a]
    , c <- [0,1..n-a-b]
    , d <- [0,1..n-a-b-c] 
    , e <- [0,1..n-a-b-c-d] 
    ]

allProbs :: [Int] -> [Float] -> Float
allProbs [] [] = 0.0
allProbs (x:xs) (p:ps) = (tf x * p) + allProbs xs ps

computeEntropy :: Floating a => [a] -> [a] -> a
computeEntropy [ ] [ ] = 0.0
computeEntropy (x:xs) (p:ps) = - x * p * (logBase 2.0 p) + computeEntropy xs ps

problem1 = computeEntropy sol (probs 5) 
  where 
    sol = filter (\b -> sum b == 8) $ filter (\s -> (allProbs s (probs 5)) == 1.0) $ allSpace 
    allSpace = space 8 
