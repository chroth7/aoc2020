module Seating
  ( SeatStatus(..)
  , Layout(..)
  , parseDay11
  , changeStatus
  , countOccAroundSeat
  , applyUpdate
  , rinseAndRepeatSeating
  , countTotalOccupied
  ) where

import           Data.List
import           Data.Maybe

data SeatStatus = NoSeat | Free | Occupied deriving (Eq, Show)

type Column = Int
type Row = Int
type SeatCoordinates = (Row, Column)
type Seat = (SeatCoordinates, SeatStatus)

type Rows = Int
type Columns = Int
type SeatingPlan = [Seat]

data Layout = Layout Rows Columns SeatingPlan deriving (Eq, Show)

-- PARSE
parseDay11 :: String -> Layout
parseDay11 str = Layout numberRows numberCols $ concatMap (uncurry parseRow) numberedRows
  where rows = lines str
        numberRows = length rows
        numberCols = length $ head rows
        numberedRows = zip [0..] rows

parseRow :: Int -> String -> [Seat]
parseRow rowIdx row = map parseSeat numberedRow
  where parseSeat (colIdx, char) = ((rowIdx, colIdx), parseChar char)
        numberedRow = zip [0..] row

parseChar :: Char -> SeatStatus
parseChar 'L' = Free
parseChar '#' = Occupied
parseChar _   = NoSeat

-- UPDATE STATUS
changeStatus :: SeatStatus -> Int -> SeatStatus
changeStatus NoSeat _ = NoSeat
changeStatus Free occ
  | occ > 0 = Free
  | otherwise = Occupied
changeStatus Occupied occ
  | occ > 3 = Free
  | otherwise = Occupied

countOccAroundSeat :: Layout -> SeatCoordinates -> Int
countOccAroundSeat (Layout rows columns layout) (row, col) = sum $ map (isOcc layout) seatsAround
  where
    seatsAround = [(x, y) | x <- [row-1..row+1], y <- [col-1..col+1], (x, y) /= (row, col), x >= 0, y >= 0, x < rows, y < columns]

isOcc :: SeatingPlan -> SeatCoordinates -> Int
isOcc plan coord = if state == Occupied then 1 else 0
  where (_, state) = fromJust $ getSeat coord plan

getSeat :: SeatCoordinates -> SeatingPlan -> Maybe Seat
getSeat coord = find (\(coo, _) -> coo == coord)

applyUpdate :: Layout -> Layout
applyUpdate layout@(Layout r c plan) = Layout r c $ map (updateSeat layout) plan

updateSeat :: Layout -> Seat -> Seat
updateSeat layout ((x, y), status) = ((x, y), changeStatus status (countOccAroundSeat layout (x, y)))

rinseAndRepeatSeating :: Layout -> Layout
rinseAndRepeatSeating layout = if isStable then updated else rinseAndRepeatSeating updated
  where updated = applyUpdate layout
        isStable = layout == updated

countTotalOccupied :: Layout -> Int
countTotalOccupied (Layout _ _ seatingPlan) = sum $ map (\(_, status) -> if status == Occupied then 1 else 0) seatingPlan
