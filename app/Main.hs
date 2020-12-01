module Main where

import Lib

main :: IO ()
main = do 
  contents <- readFile "inputs/day1.txt"
  let numbers = readInt $ lines contents
  print numbers

readInt :: [String] -> [Int]
readInt = map read

