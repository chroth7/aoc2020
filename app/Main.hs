module Main where

import           AdapterArray
import           Bags
import           Boarding
import           BootCode
import           BusService
import           Customs
import           Data.List
import           Navigation
import           Passports
import           PasswordsManagement
import           PortEncoding
import           Seating
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

  contentsDay5 <- readFile "inputs/day5.txt"
  putStrLn "Day5.1"
  print $ maximum $ map getSeatId (lines contentsDay5)
  putStrLn "Day5.2"
  print $ findMissing $ map getSeatId (lines contentsDay5)

  contentsDay6 <- readFile "inputs/day6.txt"
  putStrLn "Day6.1"
  print $ sum $ countYesAnyone $ readCustomsGroups contentsDay6
  putStrLn "Day6.1"
  print $ sum $ countYesEveryone $ readCustomsGroups contentsDay6

  contentsDay7 <- readFile "inputs/day7.txt"
  putStrLn "Day7.1"
  print $ countContainsGold $ readInputDay7 contentsDay7
  putStrLn "Day7.2"
  print $ whatsInMyGoldenBag $ readInputDay7 contentsDay7

  contentsDay8 <- readFile "inputs/day8.txt"
  putStrLn "Day8.1"
  print $ runBootSequenceTerminateEverywhere (parseInstructions contentsDay8) initBootState
  putStrLn "Day8.2"
  print $ findTermination (parseInstructions contentsDay8) 0

  contentsDay9 <- readFile "inputs/day9.txt"
  putStrLn "Day9.1"
  print $ checkRollingWindow 25 $ parseDay9 contentsDay9
  putStrLn "Day9.2"
  let seq = sort $ findSubsequence 14360655 2 $ parseDay9 contentsDay9
  print $ head seq + last seq

  contentsDay10 <- readFile "inputs/day10.txt"
  putStrLn "Day10.1"
  print $ countJumps $ parseDay10 contentsDay10
  putStrLn "Day10.2"
  print $ getCombis $ splitIntoOneGroups $ getDiffsInJolts $ parseDay10 contentsDay10

  contentsDay11 <- readFile "inputs/day11.txt"
  putStrLn "Day11.1"
  -- print $ countTotalOccupied $ rinseAndRepeatSeating (Config 3 getNeighbors) $ parseDay11 (Config 3 getNeighbors) contentsDay11
  print 2338
  putStrLn "Day11.2"
  -- print $ countTotalOccupied $ rinseAndRepeatSeating (Config 4 getNeighborsFar) $ parseDay11 (Config 4 getNeighborsFar) contentsDay11
  print 2134

  contentsDay12 <- readFile "inputs/day12.txt"
  putStrLn "Day12.1"
  print $ navigate $ parseDay12 contentsDay12
  putStrLn "Day12.2"
  print $ navigateWP $ parseDay12WP contentsDay12

  contentsDay13 <- readFile "inputs/day13.txt"
  putStrLn "Day13.1"
  print $ nextBusMagicNumber $ parseDay13 contentsDay13

readInt :: [String] -> [Int]
readInt = map read

