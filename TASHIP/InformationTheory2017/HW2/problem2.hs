module HW2 where 

import Data.List

data Coin = H | L | N deriving (Eq,Show)

-- Enumerate states for n coins, given that 1 coin is either heavy or light or
-- normal.
states n = map (\x -> mix x normals n (n-1)) [H,L,N]
  where
    normals = replicate (n-1) N

-- For N coin there is only one state.
mix N coins n _ = [ N : coins ]
-- For other coins, enumerate all states.
mix c coins n (-1) = [ ]
mix c coins n m = ((take m coins) ++ [ c ] ++  (take (n-m-1) coins)) : mix c coins n (m-1)

-- problem2 :: Int -> a
problem2 n = n

main = do 
    print $ states 12
    let res = map problem2 [1,2,3,4,5,6]
    print res
    putStrLn "All done"
