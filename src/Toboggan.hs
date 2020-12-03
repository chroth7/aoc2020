module Toboggan
  ( toboggan
  , tobogganRunBasic
  , tobogganRun
  ) where

type Landscape = [String]

type Movement = (Int, Int) -- down right
type Index = Int

tobogganRunBasic :: Landscape -> Int
tobogganRunBasic land = toboggan 0 land (1, 3)

tobogganRun:: Landscape -> Movement-> Int
tobogganRun = toboggan 0

toboggan :: Index -> Landscape -> Movement -> Int
toboggan _ []  _= 0
toboggan index land move@(down, right) = n + toboggan nextIndex (drop down land) move
  where
    row = head land
    nextIndex = (index + right) `rem` length row
    n = if (row !! index) == '#' then 1 else 0
