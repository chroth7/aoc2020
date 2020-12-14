module Navigation
  ( NavigationState(..)
  , parseDay12
  , moveShip
  , navigate
  ) where

-- helpers
--
type Coordinates = (Int, Int)
data Direction = North | South | East | West deriving (Show, Eq)
type Face = Direction
data ShipAction = Forward | TurnLeft | TurnRight deriving (Show, Eq)

-- game
data Move = ShipMove ShipAction Int | DirectionMove Direction Int deriving (Show, Eq)
data NavigationState = NavigationState Coordinates Face deriving (Show, Eq)

parseDay12 :: String -> [Move]
parseDay12 str = map str2move ls
  where ls = lines str

str2move :: String -> Move
str2move s
  | char == "N" = DirectionMove North n
  | char == "E" = DirectionMove East n
  | char == "S" = DirectionMove South n
  | char == "W" = DirectionMove West n
  | char == "F" = ShipMove Forward n
  | char == "L" = ShipMove TurnLeft n
  | char == "R" = ShipMove TurnRight n
  where char = take 1 s
        n    = read $ drop 1 s

moveShip :: NavigationState -> Move -> NavigationState
moveShip (NavigationState (x, y) face) (DirectionMove North n)    = NavigationState (x + n, y) face
moveShip (NavigationState (x, y) face) (DirectionMove South n)    = NavigationState (x - n, y) face
moveShip (NavigationState (x, y) face) (DirectionMove East n)     = NavigationState (x, y + n) face
moveShip (NavigationState (x, y) face) (DirectionMove West n)     = NavigationState (x, y - n) face
moveShip state@(NavigationState _ face) (ShipMove Forward n) = moveShip state (DirectionMove face n)
moveShip state (ShipMove TurnLeft n) = turnShipLeft state (n `div` 90)
moveShip state (ShipMove TurnRight n) = turnShipRight state (n `div` 90)

turnShipLeft :: NavigationState -> Int -> NavigationState
turnShipLeft s 0                   = s
turnShipLeft (NavigationState coord North) n = turnShipLeft (NavigationState coord West) (n -1)
turnShipLeft (NavigationState coord West) n  = turnShipLeft (NavigationState coord South) (n -1)
turnShipLeft (NavigationState coord South) n = turnShipLeft (NavigationState coord East) (n -1)
turnShipLeft (NavigationState coord East) n  = turnShipLeft (NavigationState coord North) (n -1)

turnShipRight :: NavigationState -> Int -> NavigationState
turnShipRight s 0                   = s
turnShipRight (NavigationState coord North) n = turnShipRight (NavigationState coord East) (n -1)
turnShipRight (NavigationState coord East) n  = turnShipRight (NavigationState coord South) (n -1)
turnShipRight (NavigationState coord South) n = turnShipRight (NavigationState coord West) (n -1)
turnShipRight (NavigationState coord West) n  = turnShipRight (NavigationState coord North) (n -1)

navigate :: [Move] -> NavigationState
navigate = foldl moveShip (NavigationState (0, 0) East)

