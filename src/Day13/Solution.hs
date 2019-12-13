{-# LANGUAGE RecordWildCards #-}
module Day13.Solution where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (STM, atomically, orElse)
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Concurrent.STM.TMVar as TM
import           Coords
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           IntCodeStm


dayNr :: Int
dayNr = 13

type Input = Program Int

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  prg <- loadInput

  blocks <- part1 prg
  putStrLn $ "\t Part 1: " ++ show blocks

  score <- runGame prg
  putStrLn $ "\t Part 2: " ++ show score

  putStrLn "---\n"

----------------------------------------------------------------------
-- Game Model

type Score = Int
type Coin = Int

-- | possible outputs of the games engine
data Output
  = Draw Coord TileId
  | Score Score
  | Halted
  deriving Show

type Display = Map Coord TileId

-- | stuff that goes on on display
data TileId
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Show, Eq, Bounded, Enum)

-- | input for the game
data Joystick
  = JoyLeft
  | JoyNeutral
  | JoyRight
  deriving (Show, Eq, Bounded, Enum)


-- | use this instead of 'fromEnum'
--   as Left=-1,Neutral=0,Right=1
fromJoystick :: Joystick -> Int
fromJoystick = (\x -> x - 1) . fromEnum


-- | internal Controls for the game
data Controls = Controls
  { getOutput   :: STM Output
  , setJoystick :: Joystick -> STM ()
  }


----------------------------------------------------------------------
-- part 1

part1 :: Input -> IO Int
part1 prg = do
  controls <- initGame prg
  tileIds <- collectAllTileOutput controls
  pure $ length $ filter (== Block) tileIds


collectAllTileOutput :: Controls -> IO [TileId]
collectAllTileOutput Controls{..} = reverse <$> go []
    where 
    go acc = do
      next <- atomically getOutput 
      case next of
        Halted -> pure acc
        Score _ -> pure acc
        Draw _ tileId -> go (tileId:acc)


----------------------------------------------------------------------
-- Part 2

-- | runs the game
--   uses the obvious strategy of always moving towards the ball
runGame :: Program Int -> IO Score
runGame prg = do
  -- init the game and set the coin-"counter" to 2 ... want to play for free
  controls <- initGame (2 : tail prg)
  -- start the loop
  go controls M.empty 0 Nothing Nothing
  where
    -- | the main loop - keeps track of the display, score, the paddle and ball position
    go :: Controls -> Display -> Score -> Maybe Coord -> Maybe Coord -> IO Int
    -- if both ball and paddle positions are known
    go c d s (Just paddleP) (Just ballP) = do
      -- move towards ball
      moveJoystick c paddleP ballP
      -- and reset the positions - so that the game will only receive input again
      -- after the ball position got updated
      -- need to do this because otherwise the input buffer will be flooded with
      -- wrong input resulting in a loss
      go c d s (Just paddleP) Nothing
    go c@Controls{..} display score paddleP ballP = do
      -- get the next output from the game
      out <- atomically getOutput
      case out of
          Score newScore -> 
            -- update the score and continue
            go c display newScore paddleP ballP
          Draw coord tileId -> do
            -- update paddle or ball pos if redrawn
            -- the game will only clear and redraw
            -- stuff that changed!
            let (paddleP', ballP') =
                  case tileId of
                    Ball -> (paddleP, Just coord)
                    Paddle -> (Just coord, ballP)
                    _ -> (paddleP, ballP)
            -- continue with updated display, paddle and ball positions
            go c (M.insert coord tileId display) score paddleP' ballP'
          Halted -> 
            -- if the game halted return the score
            pure score


-- | move paddle towards ball
moveJoystick :: Controls -> Coord -> Coord -> IO ()
moveJoystick Controls{..} paddlePos ballPos = 
  atomically $ setJoystick (decide paddlePos ballPos)
  where
    decide (pX, _) (bX,_) 
      | pX > bX = JoyLeft
      | pX < bX = JoyRight
      | otherwise = JoyNeutral


----------------------------------------------------------------------
-- Game setup

-- | run the given input as a game of breakout
--   note that the input is a buffer (Queue) and this runs
--   in the background so we have to be careful with the timing and
--   frequency of calling 'setJoystick' - a working solution is to do it
--   only after ball redraws
initGame :: Program Int -> IO Controls
initGame prg = do
  output <- TQ.newTQueueIO
  joystick <- TQ.newTQueueIO
  halt <- TM.newEmptyTMVarIO

  let is_halt = TM.takeTMVar halt
  let getInput = TQ.readTQueue joystick
  let nextOutput = readOutput output `orElse` (Halted <$ is_halt)

  let game = createComputer "Game" prg getInput (TQ.writeTQueue output)
  _ <- forkIO $ do
      res <- runComputer game
      case res of
        Left err -> print err
        Right () -> pure ()
      atomically $ TM.putTMVar halt ()

  pure $ Controls nextOutput (set_joystick joystick)
  where
    set_joystick v = TQ.writeTQueue v . fromJoystick
    readOutput q = do
      x <- TQ.readTQueue q
      y <- TQ.readTQueue q
      if x == -1 && y == 0
        then
          Score <$> TQ.readTQueue q
        else
          Draw (x,y) . toEnum <$> TQ.readTQueue q


loadInput :: IO Input
loadInput = parseProgram <$> readFile ("./src/Day" ++ show dayNr ++ "/input.txt")