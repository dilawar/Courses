-- Solution to problem 1
import Test.QuickCheck

data Elem = X Int | Y Int deriving (Show, Eq, Ord)

-- Here are my sets of X and Y. and whole universe!
allX = map (\x -> X x)[0, 1]
allY = map (\y -> Y y)[0, 1]

universe :: [ (Elem, Elem) ]
universe = [ (x, y) | x <- allX, y <- allY ]

-- probability that X = a and Y = b.
joint_prob :: Fractional a => (Elem, Elem) -> a
joint_prob (x, y) = case (x, y) of 
     (X 0, Y 0) -> 1/4
     (X 1, Y 0) -> 1/4
     (X 0, Y 1) -> 0
     (X 1, Y 1) -> 1/2
     otherwise -> error $ "How did you reach here!? Not in our universe"

-- Probability distribution when Y = y
conditional_prob :: Fractional a => Elem -> [(Elem, a)]
conditional_prob (Y a) = normalize [ (x, joint_prob (x, Y a)) | x <- allX ]
conditional_prob (X a) = normalize [ (y, joint_prob (X a, y)) | y <- allY ]

normalize ps = ps -- map (\x -> x / sum(ps)) ps

-- which must satisfy the following.
prop1_sum_equal_one = (sum $ map joint_prob universe) == 1.0

-- All tests are here.
tests = do
    quickCheck prop1_sum_equal_one
    putStrLn "Done checking"

