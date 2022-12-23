import System.IO
import Control.Monad
import Data.List
import Data.Char

split :: String -> (String, String)
split s = splitAt (length s `div` 2)  s

intersectp :: (String, String) -> String
intersectp ([], _) = []
intersectp (a, b) = filter (\x -> x `elem` a) b

score :: Char -> Int
score c
   | c >= 'a' && c <= 'z' = ord(c) - ord('a') + 1
   | c >= 'A' && c <= 'Z' = ord(c) - ord('A') + 27
   | otherwise = -123421

main = do
   contents <- readFile "input.txt"
   print . sum . map score . map head . map intersectp . map split . lines  $ contents

readInt :: String -> Int
readInt = read

