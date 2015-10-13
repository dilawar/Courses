-- Problem 1.

import qualified Data.Map as M
import Data.List
import Data.Tree

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

get_alpha :: String -> Char
get_alpha c = case filter (\x -> code x == c) alphabets of
    (x:[]) -> x
    y -> error $ "No valid alphabet for " ++ show c

get_code_string [] = []
get_code_string (x:xs) = (code x) ++ get_code_string xs

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
kraft_sum = sum ( map (\x -> 1 / (2^x) ) depths)
kraft :: Bool
kraft | 1.0 >= kraft_sum  = True
      | otherwise = False

-------- Section c

-- Function parse takes the depth of code and letter, and return the possible
-- character it migtht decode into

search :: Char -> Int -> [ String ] -> [ String ]
search c n codes = filter (\x -> x!!n == c) codes

-- This function returns all possible codes available for given string. It
-- should be used on input which increases 1 by 1.
parse' m 
    | head (parse m 0 allcodes) == m = Just m
    | otherwise = Nothing
         

parse :: String -> Int -> [String] -> [String]
parse [] n (x:[]) = [x]
parse [] n codes = codes
parse (c:cs) n codes = parse cs (n+1) (search c n codes)

-- decode function. Given a string of messages, it decode them to a string of
-- alphabets.
decode msg = helper [head msg] (tail msg) where
    helper :: String -> String -> String
    helper m [] = [get_alpha m]
    helper m (r:rs) = case (parse' m) of 
        (Just c) -> (get_alpha c) : helper [r] rs
        Nothing -> helper (m++[r]) rs
