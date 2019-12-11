{-# LANGUAGE RecordWildCards #-}

module Day11.Solution where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (STM, atomically, orElse)
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Concurrent.STM.TMVar as TM
import           Coords
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           IntCodeStm


dayNr :: Int
dayNr = 11

type Input = Program Int

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  prg <- loadInput

  res1 <- part1 prg
  putStrLn $ "\t Part 1: " ++ show res1

  putStrLn "\t Part 2:"
  part2 prg

  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 1

part1 :: Input -> IO Int
part1 prg = do
  (painted, _) <- runJob prg M.empty
  pure $ S.size painted


----------------------------------------------------------------------
-- Part 2

part2 :: Input -> IO ()
part2 prg = do
  (_, hull) <- runJob prg (M.insert (0,0) White M.empty)
  putStrLn $ showHull hull


-- | calculates an image from the Hull-map
showHull :: Hull -> String
showHull hull =
  unlines $ map showLine [minY..maxY]
  where
  showLine y = map (\x -> showCoord (x,y)) [minX..maxX]
  showCoord coord =
    case getColor hull coord of
      Black -> ' '
      White -> '#'
  coords = M.keys hull
  minX = minimum $ map fst coords
  maxX = maximum $ map fst coords
  minY = minimum $ map snd coords
  maxY = maximum $ map snd coords


----------------------------------------------------------------------
-- basic Model

type Hull = Map Coord Color

data Color
  = Black
  | White
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | get the color at the point - returns black if no in the map
getColor :: Hull -> Coord -> Color
getColor hull coord =
  fromMaybe Black $ M.lookup coord hull


-- | sets a new color at the position
setColor :: Coord -> Color -> Hull -> Hull
setColor = M.insert


----------------------------------------------------------------------
-- the robot and it's brain

-- | using this controls the robot is controlled
data Controls = Controls
  { getAction :: STM (Color, Turn)
  , isHalt    :: STM ()
  , setCamera :: Color -> STM ()
  }


-- | sets up the brain of the robot
runBrain :: Program Int -> IO Controls
runBrain prg = do
  camera <- TQ.newTQueueIO
  halt <- TM.newEmptyTMVarIO
  output <- TQ.newTQueueIO

  let setCam col = TQ.writeTQueue camera (fromEnum col)
  let is_halt = TM.takeTMVar halt

  let c = createComputer "Brain" prg (TQ.readTQueue camera) (TQ.writeTQueue output)
  _ <- forkIO $ do
      res <- runComputer c
      case res of
        Left err -> print err
        Right () -> pure ()
      atomically $ TM.putTMVar halt ()

  pure $ Controls (readOutput output) is_halt setCam
  where
    readOutput q = do
      outCol <- toEnum <$> TQ.readTQueue q
      outTurn <- toEnum <$> TQ.readTQueue q
      pure (outCol, outTurn)


-- | the robot as in the problem description
--   notes the positions it visited via the first parameter for part 1
runRobot :: Controls -> Set Coord -> Hull -> Coord -> Direction -> IO (Set Coord, Hull)
runRobot c@Controls{..} painted hull pos dir = do
  let curCol = getColor hull pos
  atomically $ setCamera curCol
  action <- atomically getInput
  case action of
    Nothing -> pure (painted, hull)
    Just (newCol, t) ->
      let painted' = S.insert pos painted
          hull' = setColor pos newCol hull
          dir' = turn t dir
          pos' = move dir' pos
      in runRobot c painted' hull' pos' dir'
  where
    getInput = (Just <$> getAction) `orElse` (Nothing <$ isHalt)


-- | run the robot and let it paint the initial hull
runJob :: Program Int -> Hull -> IO (Set Coord, Hull)
runJob prg hull = do
  contrs <- runBrain prg
  runRobot contrs S.empty hull (0,0) DirUp


----------------------------------------------------------------------
-- Loading and Parsing

loadInput :: IO Input
loadInput =
  parseProgram <$> readFile ("./src/Day" ++ show dayNr ++ "/input.txt")