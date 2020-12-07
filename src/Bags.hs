module Bags
  ( Bag(..)
  , readInputDay7
  , parseContents
  , stringToBag
  , containsGold
  , countContainsGold
  , whatsInMyGoldenBag
  ) where

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe

type Color = [String]
type BagContent = (Color, Int)
data Bag = Bag Color [BagContent] deriving (Show, Eq)

readInputDay7 :: String -> [Bag]
readInputDay7 input = map stringToBag $ lines input

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

-- SHINY GOLD - BOTTOM UP (Day 7.1)

type GoldenMap = Map.Map Color Bool

containsGold :: [Bag] -> GoldenMap
containsGold allBags = foldr (bagFoldContainsGold allBags) Map.empty allBags

countContainsGold :: [Bag] -> Int
countContainsGold allBags = length $ filter ((== True) . snd) fromMap
  where containsIt = containsGold allBags
        fromMap = Map.toList containsIt

bagFoldContainsGold :: [Bag] -> Bag -> GoldenMap -> GoldenMap
bagFoldContainsGold allBags (Bag col cont) mp
  | null cont = Map.insert col False mp -- no contents
  | col == ["shiny", "gold"] = Map.insert col False mp -- shiny gold itself
  | Map.member col mp = mp -- previously calculated
  | otherwise = Map.insert col (checkIfColorIsGolden allBags cont mp) mp

checkIfColorIsGolden :: [Bag] -> [BagContent] -> GoldenMap -> Bool
checkIfColorIsGolden allBags cont mp = containsGoldItself cont || anyContentsHasKnownGold cont mp || goldenRecMagic allBags cont mp
-- first check if it has gold itself
-- second check if any contents contain gold and we already know it
-- finally recursively go through the remaining contents
-- NOTE: sadly I dont mnemonize the values as I recurse here...

goldenRecMagic :: [Bag] -> [BagContent] -> GoldenMap -> Bool
goldenRecMagic allBags cont mp = any (\cc -> checkIfColorIsGolden allBags cc mp) nestedContents
  where nestedBags :: [Bag]
        nestedBags = map (\(col, _) -> fromJust (find (\(Bag bagColor _) -> col == bagColor) allBags )) cont
        nestedContents :: [[BagContent]]
        nestedContents = map (\(Bag _ cont) -> cont) nestedBags

containsGoldItself :: [BagContent] -> Bool
containsGoldItself conts = not (null filterForGold)
  where filterForGold = filter (\(col, _) -> col == ["shiny", "gold"]) conts

anyContentsHasKnownGold :: [BagContent] -> GoldenMap -> Bool
anyContentsHasKnownGold cont mp = any (\(col, _) -> Map.member col mp && fromJust (Map.lookup col mp)) cont

-- TOP DOWN (What's in my bag? 7.2)
whatsInMyGoldenBag :: [Bag] -> Int
whatsInMyGoldenBag bags = whatsInTheBag bags myShinyGoldenBag - 1
  where myShinyGoldenBag = colToBag bags ["shiny", "gold"]

colToBag :: [Bag] -> Color -> Bag
colToBag allBags col = fromJust $ find (\(Bag bagColor _) -> col == bagColor) allBags

whatsInTheBag :: [Bag] -> Bag -> Int
whatsInTheBag allBags (Bag _ cont)
  | null cont = 1
  | otherwise = 1 + sum (map (\(c1, i1) -> i1 * whatsInTheBag allBags (colToBag allBags c1)) cont)

