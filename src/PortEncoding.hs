module PortEncoding
  ( isValidCombination
  , parseDay9
  , checkRollingWindow
  , findSubsequence
  ) where

import           Data.List

parseDay9 :: String -> [Int]
parseDay9 = map read . lines

isValidCombination :: Int -> [Int] -> Bool
isValidCombination n ns
  | length ns <= 1 = False
  | hd + ls == n = True
  | hd + ls > n  = isValidCombination n $ init sorted
  | hd + ls < n  = isValidCombination n $ tail sorted
    where sorted = sort ns
          hd = head sorted
          ls = last sorted

type RollingWindow = Int

checkRollingWindow :: RollingWindow -> [Int] -> Maybe Int
checkRollingWindow window ns
  | length ns - 1 < window = Nothing
  | not valid              = Just $ ns !! window
  | otherwise              = checkRollingWindow window $ tail ns
  where valid = isValidCombination (ns !! window) (take window ns)

findSubsequence :: Int -> Int -> [Int] -> [Int]
findSubsequence target windowWidth ns
  -- running out of numbers
  | windowWidth > length ns = []
  -- next window
  | runningSum > target = findSubsequence target 2 $ tail ns
  -- bigger window
  | runningSum < target = findSubsequence target (windowWidth + 1) ns
  -- yay found it
  | runningSum == target = window
  where window = take windowWidth ns
        runningSum = sum window
