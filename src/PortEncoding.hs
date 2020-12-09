module PortEncoding
  ( isValidCombination
  , parseDay9
  , checkRollingWindow
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
