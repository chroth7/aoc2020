module Seating
  ( SeatStatus(..)
  , Layout(..)
  , parseDay11
  , changeStatus
  , countOccAroundSeat
  ) where

import           Data.List
import           Data.Maybe


data SeatStatus = NoSeat | Free | Occupied deriving (Eq, Show)

type Column = Int
type Row = Int
type SeatCoordinates = (Column, Row)
type Seat = (SeatCoordinates, SeatStatus)

type Rows = Int
type Columns = Int
type SeatingPlan = [Seat]
data Layout = Layout Rows Columns SeatingPlan deriving (Eq, Show)

parseDay11 :: String -> Layout
parseDay11 str = Layout numberRows numberCols $ concat $ map (\(colIdx, row) -> parseRow colIdx row) numberedRows
  where rows = lines str
        numberRows = length rows
        numberCols = length $ head rows
        numberedRows = zip [0..] rows

parseRow :: Int -> String -> [Seat]
parseRow colIdx row = map parseSeat numberedRow
  where parseSeat (rowIdx, char) = ((colIdx, rowIdx), parseChar char)
        numberedRow = zip [0..] row

parseChar :: Char -> SeatStatus
parseChar 'L' = Free
parseChar '#' = Occupied
parseChar _   = NoSeat

changeStatus :: SeatStatus -> Int -> SeatStatus
changeStatus NoSeat _ = NoSeat
changeStatus Free occ
  | occ > 0 = Free
  | otherwise = Occupied
changeStatus Occupied occ
  | occ > 3 = Free
  | otherwise = Occupied

countOccAroundSeat :: Layout -> SeatCoordinates -> Int
countOccAroundSeat (Layout rows columns layout) (col, row) = sum $ map (isOcc layout) seatsAround
  where
    seatsAround = [(x, y) | x <- [col-1..col+1], y <- [row-1..row+1], x /= y, x >= 0, y >= 0, x < columns, y < rows]

isOcc :: SeatingPlan -> SeatCoordinates -> Int
isOcc plan coord = if state == Occupied then 1 else 0
  where (_, state) = fromJust $ getSeat coord plan

getSeat :: SeatCoordinates -> SeatingPlan -> Maybe Seat
getSeat coord = find (\(coo, _) -> coo == coord)
