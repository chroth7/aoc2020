module Main where

import SumTo2020
import PasswordsManagement

main :: IO ()
main = do 
  contentsDay1 <- readFile "inputs/day1.txt"
  let numbers = readInt $ lines contentsDay1 
  putStrLn "Day1.1"
  print $ sumTo2020 numbers
  putStrLn "Day1.2"
  print $ triplet2020 numbers
  contentsDay2 <- readFile "inputs/day2.txt"
  let passwords = lines contentsDay2 
  putStrLn "Day2.1"
  print $ countValidPasswords passwords

readInt :: [String] -> [Int]
readInt = map read

