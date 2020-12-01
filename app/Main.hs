module Main where

import SumTo2020

main :: IO ()
main = do 
  contents <- readFile "inputs/day1.txt"
  let numbers = readInt $ lines contents
  putStrLn "Day1.1"
  print $ sumTo2020 numbers
  putStrLn "Day1.2"
  print $ triplet2020 numbers

readInt :: [String] -> [Int]
readInt = map read

