module Seating
  ( SeatStatus(..)
  , SeatingLayout(..)
  ) where

data SeatStatus = NoSeat | Free | Occupied deriving (Eq, Show)

type Seat = (Int, Int)

type SeatingLayout = [[SeatStatus]]

changeStatus :: SeatStatus -> Int -> SeatStatus
changeStatus NoSeat _ = NoSeat
changeStatus Free occ
  | occ > 0 = Free
  | otherwise = Occupied
changeStatus Occupied occ
  | occ > 3 = Free
  | otherwise = Occupied

countOccAroundSeat :: SeatingLayout -> Seat -> Int
countOccAroundSeat layout (x, y) = sum $ map (checkSeatState layout) seatsAround
  where
    columns = length $ head layout
    rows = length layout
    seatsAround = [(xx, yy) | xx <- [x-1..x+1], yy <- [y-1..y+1], xx /= yy, xx >= 0, yy >= 0, xx < columns, yy < rows]

checkSeatState :: SeatingLayout -> Seat -> Int
checkSeatState layout (x, y) = if seatState == Occupied then 1 else 0
  where seatState = (layout !! y) !! x
