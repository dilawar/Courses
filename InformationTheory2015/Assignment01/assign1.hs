-- Problem 1 in Haskell
import Data.List
import Text.PrettyPrint


partitionxy n = map (\x -> (x, n-x)) $ [1..n-1]
partitionxyz n = map (\x -> map (\y -> (fst x, fst y, snd y)) (partitionxy $ snd x)) $ (partitionxy n)

-- For a given number n, return a list of partitions with element (x,y,z) such 
-- that x + y + z = n and given probability distribution (px, py, pz) is
-- satisfied i.e. x*px + y*py + z*pz == 1.0.
valid_partitions n prob_dist = filter (\x -> prob_dist_valid x prob_dist) (concat $ partitionxyz n)
prob_dist_valid (x, y, z) (px, py, pz) 
    | x*px + y*py + z*pz == 1.0 = True
    | otherwise = False

-- Generate all subsets of cardinatlity 3 from a given list. (x, y, z) == (x, z,
-- y) == (z, y, x) etc.
subsets3 [] = []
subsets3 (x:xs) = map (\y -> (x, fst y, snd y)) (subsets2 xs) ++ subsets3 xs
subsets2 [] = []
subsets2 (x:xs) = [ (x, y) | y <- xs ] ++ subsets2 xs

-- Use the above subset generating function to generate all possible probability
-- distributions.
all_dists = subsets3 probs where probs = map (\x -> 1/2**x) [1..8]


-- Return all possible sets (x, y, z: x + y + z = n) which also satisfy
-- valid_partitions function.
all_partitions n = filter (not . null . snd) $ map (\x -> (x, valid_partitions n x)) all_dists

draw [] = ""
draw (x:xs) = draw_line x ++ "\n" ++ draw xs
draw_line (x, y) = show x ++ "=" ++ foldr (\x y -> show x ++ "," ++ y) "" y

-- Solution to part 2.
part2 = all_partitions 8

answer2 = do
    let msg = "probabilities=partitions\n" ++ draw part2
    print part2
    writeFile "__answer_2__.txt" msg 
    putStrLn "|- Done solving part 2"

-- Solution to part 3.
entropy (x, []) = []
entropy (x, (y:ys)) = helper x y : entropy (x, ys) 

helper (p1,p2,p3) (n1,n2,n3) = 
    -(n1*p1*logBase 2 p1 + n2*p2*logBase 2 p2 + n3*p3*logBase 2 p3)

format [] = ""
format (l:ls) = show l ++ "\n" ++ format ls

answer3 = do
    let sol = map (\x -> (fst x, snd x, entropy x)) part2
    print sol
    writeFile "__answer_3__.txt" $ format sol
    putStrLn "|- Done solving part 3"

bool_string 0 = []
bool_string 1 = [ "0", "1"]
bool_string n = [ x ++ y | x <- bool_string 1 , y <- bool_string (n-1) ]
all_strings = foldl (\x y -> x ++ bool_string y) [] [1..3]

answer4 = do
    putStrLn "|- Done solving part 4"

main = do
    --answer2 >> answer3 >> 
    answer4
    putStrLn "All done"

