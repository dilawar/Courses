-- Solution to problem 1
import Test.QuickCheck
import Data.List 
import Text.Printf

-- Elem could be from X, Y for {X, Y}
data Elem = X Int | Y Int | XY (Elem, Elem) deriving (Show, Eq, Ord)

-- Probability Type for each element.
data Prob a = P (Elem, a) deriving (Show, Eq, Ord)
getP (P (elem, p)) = p
getE (P (elem, p)) = elem
prob_mat :: Fractional a => [[a]]

prob_mat =  [ [1/4, 1/4], [0, 1/2] ]
-- This is from book for testing.
--prob_mat = [ [1/8, 1/16, 1/32, 1/32 ] , [1/16, 1/8, 1/32, 1/32 ]
--    , [1/16, 1/16, 1/16, 1/16 ] , [1/4, 0, 0, 0 ] ]

-- Here are my sets of X and Y. and whole universe!
allX = map (\x -> X x)[0..(length prob_mat)-1]
allY = map (\y -> Y y)[0..(length $ head prob_mat)-1]

-- Full XY set.
xys :: [ Elem ]
xys = [ XY (x, y) | x <- allX, y <- allY ]


joint_prob :: Fractional a => Elem -> Prob a
joint_prob (XY el) = P (XY el, (prob_mat!!x)!!y) where (X x, Y y) = el

-- Generic function to compute probability of any elem over the joint
-- distribution joint_prob
prob :: Fractional a => Elem -> Prob a
prob (X a) = P (X a, sum [ getP $ joint_prob $ XY (X a, y) | y <- allY ])
prob (Y a) = P (Y a, sum [ getP $ joint_prob $ XY (x, Y a) | x <- allX ])
prob e = joint_prob e

-- Probability distribution of X when Y = y or of Y when X = x.
cond_prob_dist :: Fractional a => Elem -> [Prob a]
cond_prob_dist (Y a) = normalize [ joint_prob $ XY (x, Y a) | x <- allX ]
cond_prob_dist (X a) = normalize [ joint_prob $ XY (X a, y) | y <- allY ]
normalize ps = map (\(P (x, y)) -> P (x, y/sum_ps)) ps where 
    sum_ps = sum $ map (\(P x) -> snd x) ps

-- This function computes the entropy 
-- H = - \sum_{i \in N} p_i * log_2 p_1 
entropy :: [ Prob Float ] -> Float
entropy [] = 0.0
entropy ((P (elem, 0)):xs) = entropy xs
entropy ((P (elem, p)):xs) = - p * (logBase 2 p) + (entropy xs) where 

-- Compute H(X,Y)
-- Use entropy function over p(x, y)
hxy = return $ entropy pxy where pxy = map (\x -> joint_prob x) xys
hx = return $ entropy $ map prob allY
hy = return $ entropy $ map prob allX
hx_y = return $ sum $ map (\x -> ( getP . prob $ x) * ( entropy . cond_prob_dist $ x)) allX
hy_x = return $ sum $ map (\x -> ( getP . prob $ x) * ( entropy . cond_prob_dist $ x)) allY

solveF = do
    x <- hx; y <- hy; xy <- hxy
    return $ x + y - xy
solveG = do
    x <- hx; x_y <- hx_y 
    return $ x - x_y
solveH = do
    y <- hy; y_x <- hy_x
    return $ y - y_x
solvei = solveF
    

-- All tests are here.
tests = do
    quickCheck $ sum (map sum prob_mat) == 1.0
    putStrLn "Done checking"

main = do
    --tests 
    hxy >>= printf "H(X,Y) = %f \n" 
    hx >>= printf "H(X) = %f\n" 
    hy >>= printf "H(Y) = %f\n"
    hx_y >>= printf "H(X|Y) = %f\n"
    hy_x >>= printf "H(Y|X) = %f\n"
    solveF >>= printf "H(X) + H(Y) - H(X,Y) = %f\n"
    solveG >>= printf "H(X) - H(X|Y) = %f\n"
    solveH >>= printf "H(Y) - H(Y|X) = %f\n"
    solvei >>= printf "I(X;Y) = H(X) - H(X,Y) = %f\n"
    putStrLn "Solved all"

