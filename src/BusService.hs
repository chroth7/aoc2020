module BusService
  ( parseDay13
  , parseDay13p2
  , BusState(..)
  , BusState2(..)
  , nextBusMagicNumber
  , busPart2
  ) where

import           Data.Bifunctor
import           Data.List
import           Data.List.Split

type Entrytime = Integer
type Busses = [Integer]

data BusState = BusState Entrytime Busses

type BusId = Integer
type Delay = Integer
newtype BusState2 = BusState2 [(BusId, Delay)]

parseDay13 :: String -> BusState
parseDay13 str = BusState (read $ head ls) bs
  where ls = lines str
        bs = map read $ filter (/= "x") $ splitOn "," $ ls !! 1

nextBusMagicNumber :: BusState -> Integer
nextBusMagicNumber (BusState time busses) = bb * (tt - time)
  where divs = map (div time) busses
        divsPlusOne = map (+1) divs
        nextTime = zipWith (*) busses divsPlusOne
        nextBusses = zip busses nextTime
        sortedNextBusses = sortBy (\(_, t1) (_, t2) -> if t1 < t2 then LT else GT) nextBusses
        (bb, tt) = head sortedNextBusses

parseDay13p2 :: String -> BusState2
parseDay13p2 str = BusState2 bs
  where ls = lines str
        splits = splitOn "," $ ls !! 1
        withDelay = zip splits [0..]
        filtered = filter (\(id, _) -> id /= "x") withDelay
        bs = map (first read) filtered

busPart2 :: BusState2 -> Integer -> Integer
busPart2 state@(BusState2 busses) time
  | check    = time
  | otherwise = busPart2 state (time + 1)
  where check = all (checkBus2 time) busses

checkBus2 :: Integer -> (BusId, Delay) -> Bool
checkBus2 t (id, delay) = (t + delay) `rem` id == 0
