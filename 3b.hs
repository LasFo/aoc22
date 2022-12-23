import System.IO
import Control.Monad
import Data.List
import Data.Char

split :: [String] -> [[String]]
split [] = []
split l = (take 3 l) : (split (drop 3 l))

intersectp :: String -> String -> String
intersectp x [] = x
intersectp a b = filter (\x -> x `elem` a) b

score :: Char -> Int
score c
   | c >= 'a' && c <= 'z' = ord(c) - ord('a') + 1
   | c >= 'A' && c <= 'Z' = ord(c) - ord('A') + 27
   | otherwise = -123421

main = do
   contents <- readFile "input.txt"
   print . sum . map score . map head . map (foldr intersectp [])  . split . lines  $ contents

readInt :: String -> Int
readInt = read

