{-# LANGUAGE RecordWildCards #-}
module Day15.Solution where

import           CommonParsers
import           ConsoleTests
import           IntCodePure
import qualified Data.Map.Strict as M
import           Coords hiding (move)
import           Data.List (foldl')
import Debug.Trace


dayNr :: Int
dayNr = 15

type Input = Program Int


data FloorTile
  = Empty
  | Wall
  | Oxygen
  deriving (Show, Eq)

type Floor = M.Map Coord FloorTile

data Movement
  = North
  | South
  | West
  | East
  deriving Show

sendMoveCommand :: (Int -> Continuation Int) -> Movement -> Continuation Int
sendMoveCommand m2c North = m2c 1
sendMoveCommand m2c South = m2c 2
sendMoveCommand m2c West  = m2c 3
sendMoveCommand m2c East  = m2c 4

move :: Movement -> Coord -> Coord
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move West  (x,y) = (x-1,y)
move East  (x,y) = (x+1,y)


data Status
  = HitWall
  | Moved
  | FoundOxygen
  deriving (Show, Eq, Ord, Bounded, Enum)

type Distance = Int

data State = State
  { location :: Coord
  , distanceWalked :: Distance
  , robot :: Continuation Int
  }


findOxygen :: Input -> (Floor, Coord, Distance)
findOxygen prg =
  let start = State (0,0) 0 (initComputer prg)
      initFloor = M.singleton (0,0) Empty
  in go initFloor [start]
  where
  go _ [] = error "nothing more to explore"
  go curFloor (State{..}:_)
    | foundOxygen = (curFloor, location, distanceWalked)
    where foundOxygen = curFloor M.! location == Oxygen
  go curFloor (State{..}:rest) =
    let exploreDirs = [ (pos, mv) | mv <- [North, South, West, East], let pos = move mv location, not (visited curFloor pos) ]
        newStates = [ ((pos, st), State pos (distanceWalked+1) robot') | (pos, mv) <- exploreDirs, let (st,robot') = execute robot (Just mv) ]
        floor' = updateFloor curFloor $ map fst newStates
        exploreMore = [ nextState | ((_,st), nextState) <- newStates, st /= HitWall ]
    in go floor' (rest ++ exploreMore)
  visited = flip M.member
  updateFloor :: Floor -> [(Coord, Status)] -> Floor
  updateFloor = foldl' (\m (pos, st) -> M.insert pos (status_to_tile st) m)
  status_to_tile HitWall = Wall
  status_to_tile Moved = Empty
  status_to_tile FoundOxygen = Oxygen
  execute :: Continuation Int -> Maybe Movement -> (Status, Continuation Int)
  execute cont mMv =
    case runComputer cont of
      Halted -> error "robot control stopped working"
      Error err -> error $ "error in robot control: " ++ err
      RequestInput i2c ->
        case mMv of
          Just mv -> execute (sendMoveCommand i2c mv) Nothing
          Nothing -> error "robot waits for move don't know where"
      NewOutput out cont' -> (toEnum out, cont')


run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Input -> Int
part1 prg =
  let (_, _, dist) = findOxygen prg
  in dist


part2 :: Input -> Int
part2 inp = 0


loadInput :: IO Input
loadInput = parseProgram <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )
