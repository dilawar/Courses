module HW2 where 

import Data.List
import qualified Data.Map.Strict as M

data Coin = H | L | N | W Float deriving (Eq,Show,Ord)

instance Num Coin where 
    (+) H N = W 2.1
    (+) L N = W 1.9
    (+) L L = W 2.0
    (+) N H = H + N
    (+) N L = L + N


-- Enumerate states for n coins, given that 1 coin is either heavy or light or
-- normal.
states n = map (\x -> mix x normals n (n-1)) [H,L,N]
  where
    normals = replicate (n-1) N

allStates :: [[ [ Coin ] ]]
allStates = states 12

-- For N coin there is only one state.
mix N coins n _ = [ N : coins ]
-- For other coins, enumerate all states.
mix c coins n (-1) = [ ]
mix c coins n m = ((take m coins) ++ [ c ] ++  (take (n-m-1) coins)) : mix c coins n (m-1)

-- Reduce the states when left and right coins have certain outcome.
reduce leftCoins rightCoins observation 
    | observation == 'H' = reduceRightHeavy leftCoins rightCoins
    | observation == 'L' = reduceRightLight leftCoins rightCoins
    | otherwise = reduceRightEqual leftCoins rightCoins 

reduceRightHeavy left right = map (filter (\x -> (weigh left x) > (weigh right x))) allStates
reduceRightLight left right = reduceRightHeavy right left

reduceRightEqual left right = map (filter (\x -> (weigh left x) /= (weigh right x))) allStates

weigh :: [ Int ] -> [ Coin ] -> Coin
weigh cis coins = sum ( [ coins !! i | i <- cis ] )

-- Construct map.
setupTable n = helper n allStates M.empty
  where 
    helper n [ ] table = table
    helper n (s:sx) table = helper n sx (M.insert s 0.0 table)

-- problem2 :: Int -> a
problem2 n = setupTable n

main = do 
    --let res = problem2 2
    --print res
    print $ concat $ allStates
    putStrLn "All done"
