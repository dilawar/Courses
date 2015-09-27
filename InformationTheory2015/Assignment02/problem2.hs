-- Solution to problem 2

-- This function accepts equal no of coins both side, and a measurement whether
-- left is heavy, light or equal and update the states of coins.
data Balance = L | B | H deriving (Show,Eq,Ord)

ncoins = 12
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
balance 0 _ = error "Need more than 0 coins on each side"
balance n B = map (\x -> 0.0) [1..2*n] ++ map (\x -> p (states-2*n)) [1..(states-2*n)] 
balance n _ = map (\x -> p (2*n)) [1..2*n] ++ map (\x -> 0.0) [1..(states-2*n)] 

xs = [ L, B, H]
probs_x = [ 0.25, 0.5, 0.25 ]

xys :: Int -> [Double]
xys n = foldr (++) [] [map (\a -> a * (p $ length xs)) $ balance n x | x <- xs]

hx = entropy probs_x
hy n = sum $ map (\(x,y) -> y * (entropy $ balance n x)) (zip [L, B, H] probs_x)
hxy n = entropy $ xys n

mutual_info n = (hx + hy n) - (hxy n)

main = do
    putStrLn "All done"
