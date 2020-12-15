module BusService
  ( parseDay13
  , BusState(..)
  , nextBusMagicNumber
  ) where

import           Data.List
import           Data.List.Split

type Entrytime = Int
type Busses = [Int]

data BusState = BusState Entrytime Busses

parseDay13 :: String -> BusState
parseDay13 str = BusState (read $ head ls) bs
  where ls = lines str
        bs = map read $ filter (/= "x") $ splitOn "," $ ls !! 1

nextBusMagicNumber :: BusState -> Int
nextBusMagicNumber (BusState time busses) = bb * (tt - time)
  where divs = map (div time) busses
        divsPlusOne = map (+1) divs
        nextTime = zipWith (*) busses divsPlusOne
        nextBusses = zip busses nextTime
        sortedNextBusses = sortBy (\(_, t1) (_, t2) -> if t1 < t2 then LT else GT) nextBusses
        (bb, tt) = head sortedNextBusses
