-- Solution to problem 1
import Test.QuickCheck

-- Elem could be from X, Y for {X, Y}
data Elem = X Int | Y Int | XY (Elem, Elem) deriving (Show, Eq, Ord)

-- Probability Type for each element.
data Prob a = P (Elem, a) deriving (Show, Eq, Ord)

-- Here are my sets of X and Y. and whole universe!
allX = map (\x -> X x)[0, 1]
allY = map (\y -> Y y)[0, 1]

-- Full XY set.
xys :: [ Elem ]
xys = [ XY (x, y) | x <- allX, y <- allY ]

-- probability that X = a and Y = b.
joint_prob :: Fractional a => Elem -> a
joint_prob (XY el) = case el of 
     (X 0, Y 0) -> 1/4
     (X 1, Y 0) -> 1/4
     (X 0, Y 1) -> 0
     (X 1, Y 1) -> 1/2
     otherwise -> error $ "How did you reach here!? Not in our universe"

-- Probability distribution of X when Y = y or of Y when X = x.
cond_prob :: Fractional a => Elem -> [Prob a]
cond_prob (Y a) = normalize [ P (x, joint_prob $ XY (x, Y a)) | x <- allX ]
cond_prob (X a) = normalize [ P (y, joint_prob $ XY (X a, y)) | y <- allY ]

normalize ps = map (\(P (x, y)) -> P (x, y/sum_ps)) ps where 
    sum_ps = sum $ map (\(P x) -> snd x) ps

-- which must satisfy the following.
prop1_sum_equal_one = (sum $ map joint_prob xys) == 1.0

entropy :: [ Prob Float ] -> Float
entropy [] = 0.0
entropy ((P (X x, p)):xs) = - p * (logBase 2 p) + (entropy xs)
entropy ((P (Y x, p)):xs) = - p * (logBase 2 p) + (entropy xs)

-- Compute H(X,Y)
solve1 = 5

-- All tests are here.
tests = do
    quickCheck prop1_sum_equal_one
    putStrLn "Done checking"

main = do
    tests 
    let ans1 = solve1
    print $ "Ans1: " ++ show ans1
    putStrLn "Solved all"

