module Problem1 where

import qualified Data.List as L

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
is_1periodic_in :: (Eq a, Ord a) => [a] -> a -> (Int, Bool)
is_1periodic_in [] x = (0, True)
is_1periodic_in ls x  = (head $ periods ls x, all_the_same $ periods ls x)

periods ls x
    | length elements < 3 = error "There should be at least 3 element"
    | otherwise = diff elements
  where 
    elements = L.elemIndices x ls 

is_1periodic ls =  map (is_1periodic_in ls) (L.nub ls)

diff [] = []
diff (x:[]) = []
diff (x:xs) = (head xs - x) : diff xs 

all_the_same [] = True
all_the_same xs = and $ map (== head xs) (tail xs)

random_number_generator_test = do
    let a = randoms 10 5 7 3 1000
    print $ a
    let b = is_1periodic a
    print $ b

main = do 
    random_number_generator_test
    putStrLn "Done"
