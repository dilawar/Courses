import qualified Data.List as L

tf = fromIntegral

probs :: Floating a => Int -> [a]
probs k = map (\i -> 2.0 ^^ (-i) ) [1..k]

probSpace = probs 5

plogp p = p * (logBase 2.0 p)

space :: Int -> [ [Int] ]
space n = [ [a,b,c,d,e] | a <- [0,1..n]
    , b <- [0,1..n-a]
    , c <- [0,1..n-a-b]
    , d <- [0,1..n-a-b-c] 
    , e <- [n-a-b-c-d] 
    ]


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

printLine (xs, p ) = L.intercalate " | " (map show xs) ++ " | " ++ show p

main = do
    let sol1 = problem1 
    let p = L.intercalate "\n" (map printLine sol1) 
    putStrLn p
