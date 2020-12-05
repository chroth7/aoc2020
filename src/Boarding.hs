module Boarding
  ( getRow
  , getColumn
  , getSeatId
  , findMissing
  ) where

import           Data.List
import           Data.Maybe

toBinary :: String -> [Int]
toBinary = map (\c -> if c == 'F' || c == 'L' then 0 else 1)

toDec :: [Int] -> Int
toDec = foldl (\a v -> v + a * 2) 0

getRow :: String -> Int
getRow = toDec . toBinary . take 7

getColumn :: String -> Int
getColumn = toDec . toBinary . drop 7

getSeatId :: String -> Int
getSeatId str = 8 * getRow str + getColumn str

findMissing :: [Int] -> Int
findMissing raw = elem + 1
 where
  sorted = sort raw
  zipped = zip sorted $ tail sorted
  dist = [(x, y - x) | (x, y) <- zipped]
  elem = fst $ fromJust $ find (\p -> snd p == 2) dist
