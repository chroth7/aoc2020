module Seating
  ( SeatStatus(..)
  , Layout(..)
  , Config(..)
  , getNeighbors
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
type SeatNeighbors = [SeatCoordinates]
type Seat = (SeatCoordinates, SeatNeighbors, SeatStatus)

type Rows = Int
type Columns = Int
type SeatingPlan = [Seat]

data Layout = Layout Rows Columns SeatingPlan deriving (Eq, Show)

type OccThreshold = Int
type NeighborFunction = Rows -> Columns -> SeatCoordinates -> [SeatCoordinates]
data Config = Config OccThreshold NeighborFunction

-- PARSE
parseDay11 :: String -> Layout
parseDay11 str = Layout numberRows numberCols $ concatMap (uncurry (parseRow numberRows numberCols)) numberedRows
  where rows = lines str
        numberRows = length rows
        numberCols = length $ head rows
        numberedRows = zip [0..] rows

parseRow :: Rows -> Columns -> Int -> String -> [Seat]
parseRow rows columns rowIdx row = map parseSeat numberedRow
  where parseSeat (colIdx, char) = ((rowIdx, colIdx), getNeighbors rows columns (rowIdx, colIdx), parseChar char)
        numberedRow = zip [0..] row

getNeighbors :: NeighborFunction
getNeighbors rows columns (row, col) = [(x, y) | x <- [row-1..row+1], y <- [col-1..col+1], (x, y) /= (row, col), x >= 0, y >= 0, x < rows, y < columns]

parseChar :: Char -> SeatStatus
parseChar 'L' = Free
parseChar '#' = Occupied
parseChar _   = NoSeat

-- UPDATE STATUS
changeStatus :: Config -> SeatStatus -> Int -> SeatStatus
changeStatus _ NoSeat _ = NoSeat
changeStatus _ Free occ
  | occ > 0 = Free
  | otherwise = Occupied
changeStatus (Config n _) Occupied occ
  | occ > n = Free
  | otherwise = Occupied

countOccAroundSeat :: Layout -> SeatNeighbors -> Int
countOccAroundSeat (Layout _ _ seating) neighbors = sum $ map (isOcc seating) neighbors

isOcc :: SeatingPlan -> SeatCoordinates -> Int
isOcc plan coord = if state == Occupied then 1 else 0
  where (_, _, state) = fromJust $ getSeat coord plan

getSeat :: SeatCoordinates -> SeatingPlan -> Maybe Seat
getSeat coord = find (\(coo, _, _) -> coo == coord)

applyUpdate :: Config -> Layout -> Layout
applyUpdate config layout@(Layout r c plan) = Layout r c $ map (updateSeat config layout) plan

updateSeat :: Config -> Layout -> Seat -> Seat
updateSeat config layout ((x, y), neighbors, status) = ((x, y), neighbors, changeStatus config status (countOccAroundSeat layout neighbors))

rinseAndRepeatSeating :: Config -> Layout -> Layout
rinseAndRepeatSeating config layout = if isStable then updated else rinseAndRepeatSeating config updated
  where updated = applyUpdate config layout
        isStable = layout == updated

countTotalOccupied :: Layout -> Int
countTotalOccupied (Layout _ _ seatingPlan) = sum $ map (\(_, _, status) -> if status == Occupied then 1 else 0) seatingPlan
