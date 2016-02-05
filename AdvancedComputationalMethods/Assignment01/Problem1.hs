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
elements l = L.map (\x -> L.elemIndices x l) distinct 
  where 
    distinct =  L.nub l

-- Given an element x and a list ls, return the 
is_1periodic ls x  = all_the_same . diff $ L.elemIndices x ls 

diff (x:[]) = []
diff (x:xs) = (head xs - x ) : diff xs 

all_the_same xs = and $ map (== head xs) (tail xs)


random_number_generator_test = do
    let a = randoms 10 5 7 3 1000
    let b = elements a
    print $ b

main = do 
    random_number_generator_test
    putStrLn "Done"
