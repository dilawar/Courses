import Data.Matrix 
import Test.QuickCheck

-- Solution to problem 2

-- This function accepts equal no of coins both side, and a measurement whether
-- left is heavy, light or equal and update the states of coins.
data Balance = L | B | H deriving (Show,Eq,Ord)

-- no of coins
ncoins = 4

-- Potential outcome states. (2n+1)
states = 2*ncoins + 1

p x | x > 0 = 1/(fromIntegral x)
    | otherwise = error "Need a non-zero positive number"

{-
balance n  = newstates n ++ states where 
    states = map (\x -> p rest_coins)[0..(ncoins-1)]
    rest_coins = ncoins - (2 * n)
-}

entropy :: [Double] -> Double
entropy [] = 0.0
entropy (0:xs) = entropy xs
entropy (x:xs) = (- x * logBase 2 x) + entropy xs

balance :: Int -> Balance -> [Double]
balance n x
    | n < 1 = error $ "Need at least 1 coins on each side"
    | 2*n > ncoins = error $ "Can't weigh more than " ++ show (div ncoins 2) ++ " coins"
    | otherwise = legal_balance n x

legal_balance n B = 
    map (\x -> 0.0) [1..2*n] ++ map (\x -> p (states-2*n)) [1..(states-2*n)] 
legal_balance n _ =
    map (\x -> p (2*n)) [1..2*n] ++ map (\x -> 0.0) [1..(states-2*n)] 

-- CAUTION: This will fail: 0.9999999999 is not 1.0.. Floating arithmatic
-- horror!
prop_balance_sum_to_one = 
    map (\x -> [sum (balance x y) | y <- xs ]) [1..(div ncoins 2)]

-- The outcome of experiment. Light, Balance or Heavy
xs = [ L, B, H]

-- The probabilities of outcome. (worst case?)
probs_x = [ 1/3, 1/3, 1/3 ]

--xys :: Int -> [[Double]] --[Double]
xys n = -- foldr (++) [] 
    [map (\a -> a * (p $ length xs)) $ balance n x | x <- xs]

{-
hx = entropy probs_x
hy n = sum $ map (\(x,y) -> y * (entropy $ balance n x)) (zip [L, B, H] probs_x)
hxy n = entropy $ xys n

mutual_info n = (hx + hy n) - (hxy n)

main = do
    putStrLn "All done"

-}
