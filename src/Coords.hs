module Coords where

type Coord = (Int, Int)

data Direction
  = DirUp
  | DirRight
  | DirDown
  | DirLeft
  deriving (Show, Eq)

-- | changes a coordinate by moving in a direction
move :: Direction -> Coord -> Coord
move dir (x,y) = (x+dX, y+dY)
  where
  (dX,dY) = dirToCoord dir
  dirToCoord DirUp    = ( 0, -1)
  dirToCoord DirLeft  = (-1,  0)
  dirToCoord DirDown  = ( 0,  1)
  dirToCoord DirRight = ( 1,  0)


type Dist = Int

manhDist :: Coord -> Coord -> Dist
manhDist (x,y) (x',y') = abs (x'-x) + abs (y'-y)


data Turn
  = TurnLeft
  | TurnRight
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | calculates a new diretion after a turn
turn :: Turn -> Direction -> Direction
turn TurnLeft DirUp = DirLeft
turn TurnLeft DirLeft = DirDown
turn TurnLeft DirDown = DirRight
turn TurnLeft DirRight = DirUp
turn TurnRight DirUp = DirRight
turn TurnRight DirLeft = DirUp
turn TurnRight DirDown = DirLeft
turn TurnRight DirRight = DirDown