module AdapterArray
  ( countJumps
  , parseDay10
  , getCombis
  , getDiffsInJolts
  , splitIntoOneGroups
  ) where

import           Data.List

parseDay10 :: String -> [Int]
parseDay10 = map read . lines

getDiffsInJolts :: [Int] -> [Int]
getDiffsInJolts ns = jumps
  where
        sorted = 0 : sort ns
        jumps = zipWith (-) (tail sorted) sorted

countJumps :: [Int] -> (Int, Int)
countJumps ns = (count 1, count 3 + 1)
  where
        count n = length $ filter (==n) (getDiffsInJolts ns)

-- note: I know we only have 1 and 3 in the list
splitIntoOneGroups :: [Int] -> [Int]
splitIntoOneGroups [] = []
splitIntoOneGroups list
  | head list == 3 = splitIntoOneGroups $ dropWhile (==3) list
  | head list == 1 = length (takeWhile (==1) list) : splitIntoOneGroups (dropWhile (==1) list)

getCombis :: [Int] -> Int
getCombis = product . map combiPerOne

combiPerOne :: Int -> Int
combiPerOne 1 = 1
combiPerOne 2 = 2
combiPerOne 3 = 4
combiPerOne 4 = 7
combiPerOne _ = 0

