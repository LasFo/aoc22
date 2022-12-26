import System.IO
import Control.Monad
import Data.List
import Data.Char

lists :: [String]
lists = ["GPNR", "HVSCLBJT", "LNMBDT", "BSPVR", "HVMWSQCG", "JBDCSQW", "LQF", "VFLDTHMW", "FJMVBPL"]

lists2 :: [String]
lists2 = ["NZ", "DCM", "P"]

first :: Int -> Int -> [String] -> ([String], String)
first num from (x:xs)
   | from == 1 = let
      (take, rest) = splitAt num x
      in (rest:xs, {-reverse-}take)
   | otherwise =  let 
      (list,new) = first num (from-1) xs
      in (x:list, new)

second :: Int -> String -> [String] -> [String]
second to new (x:xs)
   | to == 1 = (new ++ x):xs
   | otherwise = x:(second (to-1) new xs)
      
execute :: [Int] -> [String] -> [String]
execute (num:from:to:[]) input = let
   (list,new) = first num from input
   in second to new list

main = do
   contents <- readFile "input.txt"
   print . map head . foldr execute lists . reverse . (map . map) readInt . map words . lines  $ contents

readInt :: String -> Int
readInt = read
