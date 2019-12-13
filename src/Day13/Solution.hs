{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Day13.Solution where

import           Coords
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           IntCodePure


dayNr :: Int
dayNr = 13

type Input = Program Int

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  prg <- loadInput

  let blocks = part1 prg
  putStrLn $ "\t Part 1: " ++ show blocks

  let score = runGame prg
  putStrLn $ "\t Part 2: " ++ show score

  putStrLn "---\n"

----------------------------------------------------------------------
-- Game Model

type Score = Int
type Coin = Int

-- | possible outputs of the games engine
data Output
  = Draw Coord TileId (Continuation Int)
  | Score Score (Continuation Int)
  | WaitForInput (Int -> Continuation Int)
  | Stopped


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



----------------------------------------------------------------------
-- part 1

part1 :: Input -> Int
part1 prg =
  let tileIds = getTiles $ initComputer prg
  in length $ filter (== Block) tileIds



-- | runs the computer to Stopped or the first Score
--   and collects all tiles outputed
getTiles :: Continuation Int -> [TileId]
getTiles = go []
  where
  go acc cont =
    case getOutput cont of
      Draw _ tileId cont' -> go (tileId:acc) cont'
      _                   -> acc


-- | translates the IntCode-computers output into the problems output
getOutput :: Continuation Int -> Output
getOutput cont =
  case collect 3 [] cont of
    -- output of -1/0/score indicates a score output
    Right ([-1, 0, score], cont') -> Score score cont'
    -- else it will be coordinates and a tile-id
    Right ([x, y, tileId], cont') -> Draw (x,y) (toEnum tileId) cont'
    Right _ -> error "not enough output"
    -- non-outputs will be passed on
    Left res -> res
  where
  -- | tries to read 3 consecutive outputs from the computer
  collect :: Int -> [Int] -> Continuation Int -> Either Output ([Int], Continuation Int)
  collect 0 !acc !c = Right (reverse acc, c)
  collect n !acc !c =
    case runComputer c of
      NewOutput a c' -> collect (n-1) (a:acc) c'
      RequestInput i2c -> Left (WaitForInput i2c)
      Error s -> error s
      Halted -> Left Stopped


----------------------------------------------------------------------
-- Part 2

-- | runs the game
--   uses the obvious strategy of always moving towards the ball
runGame :: Input -> Score
runGame prg =
  -- init the game and set the coin-"counter" to 2 ... want to play for free
  let cont = initComputer (2 : tail prg)
  -- start the loop
  in go cont M.empty 0 Nothing Nothing
  where
    -- | the main loop - keeps track of the display, score, the paddle and ball position
    go :: Continuation Int -> Display -> Score -> Maybe Coord -> Maybe Coord -> Score
    -- if both ball and paddle positions are known
    go cont display score paddleP ballP =
      case getOutput cont of
        Stopped -> score
        Score newScore cont' -> go cont' display newScore paddleP ballP
        WaitForInput inp2cont -> go (inp2cont $ getJoystick paddleP ballP) display score paddleP ballP
        Draw coord tileId cont' ->
          -- update paddle or ball pos if redrawn
          -- the game will only clear and redraw
          -- stuff that changed!
          let (paddleP', ballP') =
                case tileId of
                  Ball -> (paddleP, Just coord)
                  Paddle -> (Just coord, ballP)
                  _ -> (paddleP, ballP)
          -- continue with updated display, paddle and ball positions
          in go cont' (M.insert coord tileId display) score paddleP' ballP'
    getJoystick (Just (pX, _)) (Just (bX, _))
      | pX > bX = fromJoystick JoyLeft
      | pX < bX = fromJoystick JoyRight
    getJoystick _ _ = fromJoystick JoyNeutral


----------------------------------------------------------------------
-- Game setup

loadInput :: IO Input
loadInput = parseProgram <$> readFile ("./src/Day" ++ show dayNr ++ "/input.txt")
