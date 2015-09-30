import Data.List

-- Solution to problem 2

-- This function accepts equal no of coins both side, and a measurement whether
-- left is heavy, light or equal and update the states of coins.
data Balance = L | B | H deriving (Show,Eq,Ord)

-- no of coins
ncoins = 12

-- Potential outcome states. (2n+1)
states = 2*ncoins + 1

p x | x > 0 = 1/(fromIntegral x)
    | otherwise = error "Need a non-zero positive number"

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
probs_x = [ 1/2, 1/4, 1/2 ]

-- For given n coins, compute the outcome of balance for all possible xs i.e. L,
-- B, and H. Compute the new probabilities and return them.
xys :: Int -> [[Double]]
xys n = [balance n x | x <- xs]

-- hxy is the entropy of xys with given probabilities of xs in probs_x.
hy_given_x n = 
    sum $ map (\(x,y) -> x*y) $ zip (map entropy $ xys n) (probs_x)

hx = entropy probs_x
hy n = entropy $ map (\x -> x/3) ys where
    ys = foldr sum_two [] $ xys n

sum_two x [] = x
sum_two [] y = y
sum_two (x:xs) (y:ys) = (x+y) : sum_two xs ys

mutual_info n = hy n - hy_given_x n

main = do
    let ans = map mutual_info [1..6]
    print ans
    putStrLn "Done"
