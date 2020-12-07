module Bags
  ( Bag(..)
  , parseContents
  , stringToBag
  ) where

import           Data.List

type Color = [String]
type BagContent = (Color, Int)
data Bag = Bag Color [BagContent] deriving (Show, Eq)

stringToBag :: String -> Bag
stringToBag str
  | "no other bags" `isInfixOf` str = Bag color []
  | otherwise = Bag color contents
  where color = take 2 $ words str
        stringContents = drop 4 $ words str
        contents = parseContents stringContents

parseContents :: [String] -> [BagContent]
parseContents [] = []
parseContents strs = (color, count) : parseContents (drop 4 strs)
  where current = take 4 strs
        count = read $ head current
        color = take 2 $ drop 1 current
