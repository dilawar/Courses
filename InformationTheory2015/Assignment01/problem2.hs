-- Problem 2
import System.Random
import Data.List

data LeftSideOfBalance = Light | Equal | Heavy deriving (Eq, Show, Ord)

-- Generate the coins, put a light or heavy coin in a randomly generated
-- location. The coins has equal probability of being light of heavy.
generate_coins :: Int -> Int -> Float -> [Float]
generate_coins ncoins pos w = helper ncoins pos w [] where 
    helper 0 pos w coins = coins
    helper n pos w coins 
        | n == pos = helper (n-1) pos w (w:coins)
        | otherwise = helper (n-1) pos w (1.0:coins)

-- This is our balance. If return Equal, Heavy or light if left side of the
-- balance is equal to, heavier or lighter than right side of it.
balance :: [Float] -> [Float] -> LeftSideOfBalance
balance left right 
    | sum left == sum right = Equal
    | sum left > sum right = Heavy
    | otherwise = Light

-- This is the solve function,
solve coins = find_coin $ first_partition (splitIntoTwo coins)

-- find_coin :: (LeftSideOfBalance, [Float]) -> Float
find_coin (t, x) 
    | t == Equal = x
    | Light == t = find_light_coin $ splitIntoTwo x
    | t == Heavy = find_heavy_coin $ splitIntoTwo x 

find_light_coin (x, (left, right)) = case balance left right of 
    Equal -> case x of 
        Just y -> find_coin (Equal, [y])
        Nothing -> find_coin (Equal, [])
    Heavy -> find_light_coin $ splitIntoTwo right
    Light -> find_light_coin $ splitIntoTwo left

find_heavy_coin (x, (left, right)) = case balance left right of 
    Equal -> case x of 
        Just y -> find_coin (Equal, [y])
        Nothing -> find_coin (Equal, [])
    Heavy -> find_heavy_coin $ splitIntoTwo left
    Light -> find_heavy_coin $ splitIntoTwo right

-- This function established if the counterfiet is heavier or lighter for sure.
-- It also return a reduced partition of coins where one can search for
-- counterfiet. It takes 2 balance operation to figure this out.
first_partition :: (Maybe Float, ([Float], [Float])) -> (LeftSideOfBalance, [Float])
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
next_part_equal (Nothing, (left, right)) = case balance left right of 
    _ -> Equal == balance left right

-- Split the coins into two part. If odd, return the first coin along with split
-- of rest of coins. If even, just split into two parts.
splitIntoTwo :: [Float ] -> (Maybe Float, ([Float], [Float]))
splitIntoTwo coins 
    | odd (length coins) = (Just $ head coins, splitIntoTwo' $ tail coins)
    | otherwise = (Nothing, splitIntoTwo' coins)

-- Split the given even coins into two. 
splitIntoTwo' :: [Float] -> ([Float], [Float])
splitIntoTwo' coins  = splitAt (div (length coins) 2) coins
            
-- Randomly generate n coins with one counterfiet.
main = do
    let ncoins = 2^15
    g <- getStdGen
    let pos = (fst $ randomR (1, ncoins) g) :: Int
    let weight = (fst $ randomR (1.5, 0.5) g) :: Float
    let coins  = generate_coins ncoins pos weight
    print $ solve coins
    putStrLn "Done"
