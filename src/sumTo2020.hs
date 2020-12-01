module SumTo2020 where

import Data.List

target :: Int
target = 2020

sumTo2020 :: [Int] -> (Int, Int)
sumTo2020 xs = sortedAlgo sorted $ reverse sorted
  where sorted = sort xs

sortedAlgo :: [Int] -> [Int] -> (Int, Int)
sortedAlgo [] _ = (0, 0)
sortedAlgo _ [] = (0, 0)
sortedAlgo (x:xs) (y:ys)
  | x + y == target = (x,y)
  | x + y < target  = sortedAlgo xs (y: ys)
  | x + y > target  = sortedAlgo (x: xs) ys


-- not a nice implementation, but given the size of the list...
triplet2020 :: [Int] -> (Int, Int, Int)
triplet2020 xs =  head [(x, y, z) | x <- xs, y <- xs, z <- xs, x + y + z == target, x /= y, x /= z, y /= z]
