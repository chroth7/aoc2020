module AdapterArray
  ( countJumps
  , parseDay10
  ) where

import           Data.List

parseDay10 :: String -> [Int]
parseDay10 = map read . lines

countJumps :: [Int] -> (Int, Int)
countJumps ns = (count 1, count 3 + 1)
  where
        sorted = 0 : sort ns
        jumps = zipWith (-) (tail sorted) sorted
        count n = length $ filter (==n) jumps
