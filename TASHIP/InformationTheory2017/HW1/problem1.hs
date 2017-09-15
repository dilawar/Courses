import qualified Data.List as L



probs = map (\i -> 2 ** (-i) ) [1..5]
space n = [ 
    [a,b,c,d,e] | a <- [0,1..n]
    , b <- [0,1..n-a]
    , c <- [0,1..n-a-b]
    , d <- [0,1..n-a-b-c] 
    , e <- [0,1..n-a-b-c-d] 
    ]

allProbs [] [] = 0.0
allProbs (x:xs) (p:ps) = x * p + allProbs xs ps

computeEntropy [ ] [ ] = 0.0
computeEntropy (x:xs) (p:ps) = - x * p * (logBase 2.0 p) + computeEntropy xs ps

problem1 = computeEntropy sol probs
  where 
    sol = filter (\b -> sum b == 8.0) $ filter (\a -> allProbs a probs == 1.0) $ allSpace 
    allSpace = space 8
