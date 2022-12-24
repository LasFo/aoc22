import System.IO
import Control.Monad
import Data.List
import Data.Char

parseLine :: String -> (Int,Int,Int,Int)
parseLine ls = let
   (first, rest0) = span isDigit ls
   l0 = read first :: Int
   (second, rest1) = span isDigit (tail rest0)
   r0 = read second :: Int
   (third, rest2) = span isDigit (tail rest1)
   l1 = read third :: Int
   (fourth, rest) = span isDigit (tail rest2)
   r1 = read fourth  :: Int
   in (l0, r0, l1, r1)

score :: (Int, Int, Int, Int) -> Int
score (l0, r0, l1, r1)
   | l0 > l1 = if l0 <= r1 then 1 else 0
   | otherwise = if l1 <= r0 then 1 else 0

main = do
   contents <- readFile "input.txt"
   print . sum . map score . map parseLine . lines  $ contents

readInt :: String -> Int
readInt = read

