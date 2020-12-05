module Main where

import           Passports
import           PasswordsManagement
import           SumTo2020
import           Toboggan

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
  print $ countValidPasswordsToboggan passwords
  putStrLn "Day2.2"
  print $ countValidPasswordsSanta passwords

  contentsDay3 <- readFile "inputs/day3.txt"
  let landscape = lines contentsDay3
  putStrLn "Day3.1"
  print $ tobogganRunBasic landscape
  putStrLn "Day3.2"
  print $ product $ map (tobogganRun landscape) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

  contentsDay4 <- readFile "inputs/day4.txt"
  putStrLn "Day4.1"
  print $ length $ filter (==True) $ validatePassportsEasy $ readPassportsToCandidates contentsDay4
  putStrLn "Day4.2"
  print $ length $ filter (==True) $ validatePassports $ readPassportsToCandidates contentsDay4


readInt :: [String] -> [Int]
readInt = map read

