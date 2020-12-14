module Navigation
  ( NavigationState(..)
  , WaypointState(..)
  , parseDay12
  , moveShip
  , navigate
  , parseDay12WP
  , moveShipWP
  , navigateWP
  ) where

-- helpers
--
type Coordinates = (Int, Int)
type ShipCoordinates = Coordinates
type WaypointCoordinates = Coordinates

data Direction = North | South | East | West deriving (Show, Eq)
type Face = Direction
data ShipAction = Forward | TurnLeft | TurnRight deriving (Show, Eq)

-- without waypoint
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

-- with waypoint
data WaypointState = WaypointState ShipCoordinates WaypointCoordinates deriving (Show, Eq)

data MoveWP = ShipMoveWP ShipAction Int | MoveWP Direction Int deriving (Show, Eq)

parseDay12WP :: String -> [MoveWP]
parseDay12WP str = map str2moveWP ls
  where ls = lines str

str2moveWP :: String -> MoveWP
str2moveWP s
  | char == "N" = MoveWP North n
  | char == "E" = MoveWP East n
  | char == "S" = MoveWP South n
  | char == "W" = MoveWP West n
  | char == "F" = ShipMoveWP Forward n
  | char == "L" = ShipMoveWP TurnLeft n
  | char == "R" = ShipMoveWP TurnRight n
  where char = take 1 s
        n    = read $ drop 1 s

moveShipWP :: WaypointState -> MoveWP -> WaypointState
moveShipWP (WaypointState ship (wx, wy)) (MoveWP North n)    = WaypointState ship (wx, wy + n)
moveShipWP (WaypointState ship (wx, wy)) (MoveWP South n)    = WaypointState ship (wx, wy - n)
moveShipWP (WaypointState ship (wx, wy)) (MoveWP East n)     = WaypointState ship (wx + n, wy)
moveShipWP (WaypointState ship (wx, wy)) (MoveWP West n)     = WaypointState ship (wx - n, wy)
moveShipWP (WaypointState (x, y) (wx, wy)) (ShipMoveWP Forward n) = WaypointState (x + n * wx, y + n * wy) (wx, wy)
moveShipWP state (ShipMoveWP TurnLeft n) = turnShipLeftWP state (n `div` 90)
moveShipWP state (ShipMoveWP TurnRight n) = turnShipRightWP state (n `div` 90)

turnShipLeftWP :: WaypointState -> Int -> WaypointState
turnShipLeftWP s 0                   = s
turnShipLeftWP (WaypointState coord (x, y)) n = turnShipLeftWP (WaypointState coord (-y, x)) (n - 1)

turnShipRightWP :: WaypointState -> Int -> WaypointState
turnShipRightWP s 0                   = s
turnShipRightWP (WaypointState coord (x, y)) n = turnShipRightWP (WaypointState coord (y, -x)) (n - 1)

navigateWP :: [MoveWP] -> WaypointState
navigateWP = foldl moveShipWP (WaypointState (0, 0) (10, 1))
