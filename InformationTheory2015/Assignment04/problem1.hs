-- Problem 1.

import qualified Data.Map as M
import Data.List

alphabets :: [ Char ]
alphabets = "ABCDEFGH"

code x = case x of
    'A' -> "01"
    'B' -> "11"
    'C' -> "001"
    'D' -> "0000"
    'E' -> "0001"
    'F' -> "1001"
    'G' -> "1010"
    'H' -> "1011"
    otherwise -> "Unknown code"

allcodes = map code alphabets

--------- Solve section a.  
-- Check if a given sequence is prefix of second sequence
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix (x:xs) (y:ys) 
    | x == y = isPrefix xs ys 
    | otherwise = False

-- Given a character, check if its code is prefix of any other character's code.
isPrefixCode :: Char -> Bool
isPrefixCode x = and $ map (\y -> isPrefix (code x) y ) (delete (code x) allcodes)

solvea = not . and $ map isPrefixCode alphabets

-------- Section b

depths = map (length . code ) alphabets
kraft | sum $ map ( \x -> 1 / (2^x)) depths <= 1.0 = True
      | otherwise = False
