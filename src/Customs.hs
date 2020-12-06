module Customs
  ( readCustomsGroups
  , countYes
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

countYes :: AnswerGroup -> [Int]
countYes = map (length . group . sort . concat)
