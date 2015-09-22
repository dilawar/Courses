-- Problem 2
import System.Random
import Data.List

data LeftSideOfBalance = Light | Equal | Heavy deriving (Eq, Show, Ord)

-- args: number of coins, counterfiet position and its weight
-- Create a list of coins.
generate_coins :: Int -> Int -> Float -> [Float]
generate_coins ncoins pos w = helper ncoins pos w [] where 
    helper 0 pos w coins = coins
    helper n pos w coins 
        | n == pos = helper (n-1) pos w (w:coins)
        | otherwise = helper (n-1) pos w (1.0:coins)
--
-- This is our balance. We can only use this in our algorithm.
balance :: [Float] -> [Float] -> LeftSideOfBalance
balance left right 
    | sum left == sum right = Equal
    | sum left > sum right = Heavy
    | otherwise = Light

solve coins = first_partition (splitIntoTwo coins)

--first_partition :: (Maybe Float, ([Float], [Float])) -> [(a, b)]
first_partition (Just x, (left, right)) 
    | Equal == balance left right = (Equal, [x])
    | otherwise = first_step left right (balance left  right)

first_partition (Nothing, (left, right)) 
    | Equal == balance left right  =  (Equal, [])
    | otherwise = first_step left right (balance left right)

first_step left right l_state = case (l_state, next_part_equal $ splitIntoTwo left) of 
    (Heavy, False) ->(Heavy, left) 
    (Light, False) ->(Light, left) 
    (Heavy, True) -> (Light, right)
    (Light, True) -> (Heavy, right)

next_part_equal (Just x, (left, right)) = case balance left right of 
    Equal -> Equal == balance [x] [head left]
    otherwise -> Equal == balance left right

-- Split the coins into two part. If odd, return the first coin along with split
-- of rest of coins. If even, just split into two parts.
splitIntoTwo :: [Float ] -> (Maybe Float, ([Float], [Float]))
splitIntoTwo coins 
    | odd (length coins) = (Just $ head coins, splitIntoTwo' $ tail coins)
    | otherwise = (Nothing, splitIntoTwo' coins)

-- Split the given even coins into two. 
splitIntoTwo' :: [Float] -> ([Float], [Float])
splitIntoTwo' coins  = splitAt (div (length coins) 2) coins

{-
solve coins = filter_phony (splitIntoTwo coins) Equal

filter_phony (Just x) left right prev = case (prev, balance' x left right) of
-}
    
            
-- Randomly generate n coins with one counterfiet.
main = do
    let ncoins = 7
    g <- getStdGen
    let pos = (fst $ randomR (1, ncoins) g) :: Int
    let weight = (fst $ randomR (1.5, 0.5) g) :: Float
    let coins  = generate_coins ncoins pos weight
    print $  coins
    print $ solve coins
    {-
    let ans = solve coins 
    print ans
    -}
    putStrLn "Done"
