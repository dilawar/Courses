import qualified Data.List as L

probs k = map (\i -> 2 ** (-i) ) [1..k]

problem1 = helper' 8 5

space n = [ (a,b,c) | a <- [0,1..n], b <- [0,1..n-a], c <- [0,1..n-a-b] ]

helper' n k = sequence $ L.replicate k allopts
  where 
    allopts = [0,1..n]
    {- pbs = probs k -}
