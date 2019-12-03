module Coords where

type Coord = (Int, Int)
type Dist = Int


manhDist :: Coord -> Coord -> Dist
manhDist (x,y) (x',y') = abs (x'-x) + abs (y'-y)