import System.IO
import Control.Monad
import Data.List

score :: [String] -> Int
score (l:r:ls)
   | l == "A" && r == "X" = 3
   | l == "A" && r == "Y" = 4
   | l == "A" && r == "Z" = 8
   | l == "B" && r == "X" = 1
   | l == "B" && r == "Y" = 5
   | l == "B" && r == "Z" = 9
   | l == "C" && r == "X" = 2
   | l == "C" && r == "Y" = 6
   | l == "C" && r == "Z" = 7
   | otherwise = -24314

main = do
   contents <- readFile "input.txt"
   print . sum . map score . map words . lines  $ contents

readInt :: String -> Int
readInt = read

