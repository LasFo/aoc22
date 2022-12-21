import System.IO
import Control.Monad
import Data.List

merger :: String -> String -> String
merger a b = 
   if a == "" then "\n" ++ b
   else if b == "" then a ++ "\n" 
   else a ++ " " ++ b

main = do
   contents <- readFile "input.txt"
   print . sum .  take 3 . reverse . sort  . map sum . (map . map) readInt . map words . lines . foldr merger "" . lines $ contents

readInt :: String -> Int
readInt = read

