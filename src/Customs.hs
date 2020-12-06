module Customs
  ( readCustomsGroups
  , countYesAnyone
  , countYesEveryone
  ) where

import           Data.List

type AnswerGroup = [[String]]

readCustomsGroups :: String -> AnswerGroup
readCustomsGroups io = linesToGroups rawLines
  where rawLines = lines io

linesToGroups :: [String] -> AnswerGroup
linesToGroups = foldr linesFold []

linesFold :: String -> AnswerGroup -> AnswerGroup
linesFold str [] = [[str]]
linesFold str acc@(x:xs)
  | str == "" = [] : acc
  | otherwise = (str:x):xs

countYesAnyone :: AnswerGroup -> [Int]
countYesAnyone = map (length . group . sort . concat)

countYesEveryone :: AnswerGroup -> [Int]
countYesEveryone = map (length . foldl1 intersect)
