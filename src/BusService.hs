module BusService
  ( parseDay13
  ) where

type Entrytime = Int
type Busses = [Int]

data BusState = BusState Entrytime Busses

parseDay13 :: String -> BusState
parseDay13 str = BusState (read $ head ls)
  where ls = lines str
        bs = 
