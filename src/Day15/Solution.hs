{-# LANGUAGE RecordWildCards #-}
module Day15.Solution where

import           IntCodePure
import qualified Data.Map.Strict as M
import           Coords hiding (move)
import           Data.List (foldl', nub)


dayNr :: Int
dayNr = 15

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  inp <- loadInput

  let (mappedFloor, oxCoord, distToOxygen) = part1 inp
  putStrLn $ "\t Part 1: " ++ show distToOxygen

  let (_, time) = fillOxygen mappedFloor oxCoord
  putStrLn $ "\t Part 2: " ++ show time

  putStrLn "---\n"


----------------------------------------------------------------------
-- Modelling

type Input = Program Int

data FloorTile
  = Empty
  | Wall
  | Oxygen
  deriving (Show, Eq)

-- | this represents a map of the room
--   the robot looks for the oxygen
--   with the distanced needed to walk there and
--   what the robot found at the coord
type Floor = M.Map Coord (Distance, FloorTile)

-- | possible movement-commands
data Movement
  = North
  | South
  | West
  | East
  deriving Show


-- | change coords for that movement
move :: Movement -> Coord -> Coord
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move West  (x,y) = (x-1,y)
move East  (x,y) = (x+1,y)


-- | we need to translate 'Movement' into something
--   our IntCode robot-control-program understands
sendMoveCommand :: (Int -> Continuation Int) -> Movement -> Continuation Int
sendMoveCommand m2c North = m2c 1
sendMoveCommand m2c South = m2c 2
sendMoveCommand m2c West  = m2c 3
sendMoveCommand m2c East  = m2c 4


-- | the status reply the robot-control-program
--   gives us
data Status
  = HitWall
  | Moved
  | FoundOxygen
  deriving (Show, Eq, Ord, Bounded, Enum)

type Distance = Int

-- | we are using a breadth-first-search here
--   to map the floo
--   this is the state/node type tracking
--   the progres
data State = State
  { location :: Coord
  , distanceWalked :: Distance
  , robot :: Continuation Int
  }


-- | explore the floor using a breadth-first-search
--   algorithm
exploreMap :: Input -> Floor
exploreMap prg =
  -- start at (0,0) with an initialized robot-control-program
  -- and a floor we only know that at (0,0) there is nothing
  -- (that's my guess - if the robot sits on a wall/oxygen
  -- the problem is kindof useless)
  let start = State (0,0) 0 (initComputer prg)
      initFloor = M.singleton (0,0) (0, Empty)
  in go initFloor [start]
  where
  -- | doing the search - gets the visited/mapped floor so far
  --   and a list with states it should continue searching at
  go :: Floor -> [State] -> Floor
  go f [] = f
  go curFloor (State{..}:rest) =
        -- continue exploring around the current pos if we did not visit that position yet
    let exploreDirs = [ (pos, mv) | mv <- [North, South, West, East]
                                  , let pos = move mv location
                                  , not (visited curFloor pos) ]
        -- calculate the new states for those positions
        newStates = [ ((pos, st), State pos (distanceWalked+1) robot')
                    | (pos, mv) <- exploreDirs
                    , let (st,robot') = execute robot (Just mv)
                    ]
        -- update the floor plan
        floor' = updateFloor distanceWalked curFloor $ map fst newStates
        -- append the new states to be explored on the back of our list (=> breadth-first)
        exploreMore = [ nextState | ((_,st), nextState) <- newStates, st /= HitWall ]
       -- continue the algorithm
    in go floor' (rest ++ exploreMore)

  visited = flip M.member

  updateFloor dist = foldl' (\m (pos, st) -> M.insert pos (dist+1, status_to_tile st) m)

  status_to_tile HitWall = Wall
  status_to_tile Moved = Empty
  status_to_tile FoundOxygen = Oxygen

  execute cont mMv =
    case runComputer cont of
      Halted -> error "robot control stopped working"
      Error err -> error $ "error in robot control: " ++ err
      RequestInput i2c ->
        case mMv of
          Just mv -> execute (sendMoveCommand i2c mv) Nothing
          Nothing -> error "robot waits for move don't know where"
      NewOutput out cont' -> (toEnum out, cont')


-- | if you ever want to see the resulting floor
drawFloor :: Floor -> String
drawFloor curFloor =
  unlines $ map showLine [minY..maxY]
  where
  showLine y = map (\x -> showCoord (x,y)) [minX..maxX]

  showCoord coord =
    case M.lookup coord curFloor of
      Nothing -> '?'
      Just (_, Empty) -> '.'
      Just (_ , Wall) -> '#'
      Just (_, Oxygen) -> 'O'

  coords = M.keys curFloor

  minX = minimum $ map fst coords
  maxX = maximum $ map fst coords
  minY = minimum $ map snd coords
  maxY = maximum $ map snd coords


----------------------------------------------------------------------
-- Part 1
part1 :: Input -> (Floor, Coord, Int)
part1 prg =
  let resFloor = exploreMap prg
      (oxCoord, dist) = findOxygen resFloor
  in (resFloor, oxCoord, dist)


-- | uses a mapped floor plan to find the oxygen and the
--   distance to it
findOxygen :: Floor -> (Coord, Distance)
findOxygen curFloor = head
  [ (c,d) | (c, (d,t)) <- M.toList curFloor, t == Oxygen ]


----------------------------------------------------------------------
-- Part 2
type Time = Int


-- | uses breadth-first-search again to fill up every
--   empty space with oxygen, tracking the total time/steps
fillOxygen :: Floor -> Coord -> (Floor, Time)
fillOxygen initMap oxCoord = go initMap [oxCoord] 0
  where
                   -- note: we have to decrease by one because the last
                   -- recursive call added one although there was no
                   -- free locations left
  go curMap [] t = (curMap, t-1)
  go curMap locs t =
    let neighbours = nub [ pos | loc <- locs
                               , mv <- [North, South, West, East]
                               , let pos = move mv loc
                               , isFree curMap pos ]
        curMap' = updateMap curMap neighbours
    in go curMap' neighbours (t+1)

  updateMap = foldl' (\m pos -> M.update (\(d,_) -> Just (d, Oxygen)) pos m)

  isFree m c =
    case M.lookup c m of
      Just (_, Empty) -> True
      _               -> False


----------------------------------------------------------------------
-- Loading / Parsing

loadInput :: IO Input
loadInput = parseProgram <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )
