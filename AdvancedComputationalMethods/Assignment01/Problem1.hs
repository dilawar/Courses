module Problem1 where

import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Random as R

-- Generate an infinite list of random numbers. Take as many as you want.
prg_linear_conguential :: Int -> Int -> Int -> Int -> [ Int ]
prg_linear_conguential a b m ri = next_num : prg_linear_conguential a b m next_num
  where 
    next_num = mod (a * ri + b) m 

-- Generate n random numbers.
randoms a b m r0 n = take n $ prg_linear_conguential a b m r0

-- Get the distinct elements from list with their count
elements l = zip distinct $ L.map (\x -> L.elemIndices x l) distinct 
  where 
    distinct =  L.nub l

-- Given an element x and a list ls, return the 
is_1periodic_in :: (Eq a, Ord a) => [a] -> a -> (a, Int, Bool)
is_1periodic_in [] x = (x, 0, True)
is_1periodic_in ls x  = (x, head $ periods ls x, all_the_same $ periods ls x) 

periods ls x 
    | length elements < 2 = [] -- error "There should be at least 2 element"
    | otherwise = diff elements
  where 
    elements = L.elemIndices x ls 

-- Check if a given sequence is periodic, also return the period of sequence.
is_1periodic ls = (period_of_first_element
    , all_the_same $ map (\(x, p, _) -> p) periods_of_elements
    )
  where
    period_of_first_element = (\(a, p, _) -> p) (head periods_of_elements)
    periods_of_elements = map (is_1periodic_in ls) (L.nub ls)

diff [] = []
diff (x:[]) = []
diff (x:xs) = (head xs - x) : diff xs 

all_the_same [] = True
all_the_same xs = and $ map (== head xs) (tail xs)

-- Function to compute correlation between two arrays.
xcorr xs ys = map (\a -> xcorr' a xs) $ L.tails ys 
  where 
    xcorr' as bs = sum . map (\(x, y) -> x * y ) $ zip as bs 

-- Count the number of times each element in list occured.
counter xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs ])

problem1a = do 
    let (r0, n) = (10, 30000)
    r0 <- R.randomRIO (1, 100) 
    a <- R.randomRIO (10, 100)
    b <- R.randomRIO (10, 100)
    --m <- R.randomRIO (10, 3000)
    let m = 997
    putStrLn $ "Generating random numbers: init= " ++ show r0 ++ " N=" ++ show n
    putStrLn $ " -- a: " ++ show a ++ " b: " ++ show b ++ " m: " ++ show m
    let numbers = randoms a b m r0 10000 
    let (period, is_periodic) = is_1periodic numbers
    let seq = take period numbers
    print $ seq
    print $ counter seq


random_number_generator_test = do
    let a = randoms 7 1 13 1 10000
    print $ a
    let b = is_1periodic a
    print $ b

main = do 
    random_number_generator_test
    putStrLn "Done"
