{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Day17.Solution where

import           Coords
import           Control.Monad (guard)
import           Data.List (inits, intercalate)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import           IntCodePure


dayNr :: Int
dayNr = 17

type Input = Program Int

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  prg <- loadInput
  view <- part1 prg

  res2 <- part2 prg view
  putStrLn $ "\t Part 2: " ++ show res2


  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 1

-- | just runs the computer and
--   collecting outputs in a map
--   and summing up the alignment-parameters for the
--   intersections
--   quite straight forward
part1 :: Input -> IO CameraView
part1 prg = do
  let rbt = initComputer prg
      view = currentView rbt
      bounds = getBounds view
      inters = scaffoldIntersections view
      alignSum = sum $ map (alignmentParameter bounds) inters
  putStrLn $ "\t Part 1: " ++ show alignSum
  pure view


data Tile
  = Empty
  | Scaffold
  | Robot Direction
  | TumblingRobot
  | Walked
  deriving (Show, Eq)

type CameraView = M.Map Coord Tile


scaffoldIntersections :: CameraView -> [Coord]
scaffoldIntersections view = [ c | c <- M.keys view, isIntersection c ]
  where
  isIntersection coord@(x,y) =
    getTile view coord == Scaffold &&
    getTile view (x-1,y) == Scaffold &&
    getTile view (x+1,y) == Scaffold &&
    getTile view (x,y-1) == Scaffold &&
    getTile view (x,y+1) == Scaffold


alignmentParameter :: Bounds -> Coord -> Int
alignmentParameter (Bounds (minX,minY) _) (x,y) =
  (y-minY)*(x-minX)


getTile :: CameraView -> Coord -> Tile
getTile view coord =
  fromMaybe Empty $ M.lookup coord view


currentView :: Continuation Int -> CameraView
currentView rbt = go rbt M.empty (0,0)
  where
  go :: Continuation Int -> CameraView -> Coord -> CameraView
  go cont hull curPos@(x,y) =
    case runComputer cont of
      Error err -> error $ "ASCII errored: " ++ err
      Halted -> hull
      RequestInput _ -> error "don't know what you expect ASCII"
      NewOutput asciiCode next ->
        case toEnum asciiCode of
          '\n' -> go next hull nextLine
          '.' -> go next (M.insert curPos Empty hull) nextInLine
          '#' -> go next (M.insert curPos Scaffold hull) nextInLine
          'X' -> go next (M.insert curPos TumblingRobot hull) nextInLine
          '^' -> go next (M.insert curPos (Robot DirUp) hull) nextInLine
          'v' -> go next (M.insert curPos (Robot DirDown) hull) nextInLine
          '<' -> go next (M.insert curPos (Robot DirLeft) hull) nextInLine
          '>' -> go next (M.insert curPos (Robot DirRight) hull) nextInLine
          c   -> error $ "ASCII produced uninterpretable character " ++ show c

    where
    nextInLine = (x+1,y)
    nextLine = (0,y+1)


----------------------------------------------------------------------
-- Part 2
--

-- | runs the program in "wake-up-mode (2)"
--   till it halts and returns the last output
--   video-feed is disabled for this
part2 :: Input -> CameraView -> IO Int
part2 prg view = do
  let Solution{..} = findSolution $ initSequence view
      wakeUp = 2 : tail prg
      input =
        intercalate "\n" [mainR, funA, funB, funC, "n" ] ++ "\n"
  go (initComputer wakeUp) input 0
  where
    go cont inp dust =
      case runComputer cont of
        Error err -> error $ "ASCII errored: " ++ err
        Halted -> pure dust
        RequestInput i2c ->
          case inp of
            (c:more) -> go (i2c $ fromEnum c) more dust
            []       -> error "ASCII want's input but I don't have any left"
        NewOutput newDust next ->
          go next inp newDust


-- these Values where retrieved by hand
-- I used 'bestPath' to look for the first
-- valid path that visits every scaffold
-- then I looked for the biggest prefix
-- matching the most places in an editor
-- replaced all occurences with A and
-- repeated this with B and C

data Solution = Solution
  { mainR :: String
  , funA :: String
  , funB :: String
  , funC :: String
  }
  deriving Show

manualSolution :: Solution
manualSolution = Solution
  "A,B,A,B,C,A,B,C,A,C"
  "R,6,L,10,R,8"
  "R,8,R,12,L,8,L,8"
  "L,10,R,6,R,6,L,8"

--------------------------------------------------
-- bellow is my attempt to let the computer find
-- the solution as I did manuall
-- it's a bit messy but I spent way to much time on this
-- so it is what it is

type Sequence = [Sequent]

data Sequent
  = Function String
  | Uncollected Movement
  deriving Eq

instance Show Sequent where
  show (Function f) = f
  show (Uncollected m) = show m


initSequence :: CameraView -> Sequence
initSequence view =
  let bestP = bestPath view
  in map Uncollected $ runLengthEncoding bestP


findSolution :: Sequence -> Solution
findSolution =
  head . map toSolution . filter (\(_,_,_,ms) -> isValidMain ms) . findFunctions
  where
  toSolution (fA,fB,fC,mR) = Solution (showS mR) (showS fA) (showS fB) (showS fC)
  showS :: Show a => a -> String
  showS = tail . init . show
  isValidMain ms = length (show ms) - 2 <= 20


findFunctions :: Sequence -> [([Movement], [Movement], [Movement], Sequence)]
findFunctions !initSeq = do
  (!fA, !seqA) <- factorOut "A" initSeq
  (!fB, !seqB) <- factorOut "B" seqA
  (_, !uncol) <- gotoUncollectedPart seqB
  let !fC = uncollectedPrefix uncol
  guard $ not (null fC)
  guard $ validAsciiFunction fC
  let !seqC = refactorFunction "C" fC seqB
  guard $ all isFunction seqC
  pure (fA,fB,fC,seqC)
  where
    isFunction (Function _) = True
    isFunction _ = False


factorOut :: String -> Sequence -> [([Movement], Sequence)]
factorOut name inSeq = do
  (_, uncol) <- gotoUncollectedPart inSeq
  f <- [ body
        | let !pref = uncollectedPrefix uncol
        , body <- reverse $ inits pref
        , not (null body)
        , validAsciiFunction body ]
  let seq' = refactorFunction name f inSeq
  pure (f, seq')


refactorFunction :: String -> [Movement] -> Sequence -> Sequence
refactorFunction _ _ [] = []
refactorFunction name subSeq (f@(Function _) : rest) =
  f : refactorFunction name subSeq rest
refactorFunction name subSeq ss@(Uncollected s:ss') =
  let (ls,rs) = splitAt (length subSeq) ss
  in if map Uncollected subSeq == ls
    then Function name : refactorFunction name subSeq rs
    else Uncollected s : refactorFunction name subSeq ss'

gotoUncollectedPart :: Sequence -> [(Sequence, Sequence)]
gotoUncollectedPart [] = []
gotoUncollectedPart (f@(Function _) : rest) = do
  (ls,rs) <- gotoUncollectedPart rest
  pure (f:ls, rs)
gotoUncollectedPart rest@(Uncollected _ : _) =
  pure ([], rest)

uncollectedPrefix :: Sequence -> [Movement]
uncollectedPrefix [] = []
uncollectedPrefix (Function _ : _) = []
uncollectedPrefix (Uncollected m : rest) = m : uncollectedPrefix rest

runLengthEncoding :: [Movement] -> [Movement]
runLengthEncoding [] = []
runLengthEncoding (Forward n : Forward m : rest) =
  runLengthEncoding $ Forward (n+m) : rest
runLengthEncoding (m : rest) =
  m : runLengthEncoding rest


asciiLength :: [Movement] -> Int
asciiLength mvs = length (show mvs) - 2

validAsciiFunction :: [Movement] -> Bool
validAsciiFunction mvs = asciiLength mvs <= 20


----------------------------------------------------------------------
-- find a path visiting every scaffold

data Movement
  = Forward Int
  | Turn Turn
  deriving Eq

instance Show Movement where
  show (Forward n) = show n
  show (Turn TurnLeft) = "L"
  show (Turn TurnRight) = "R"

-- | assuming the first found path is
--   good enough
--   tried 100 and the first was
--   the shortest so let's go with it
bestPath :: CameraView -> [Movement]
bestPath = head . findPaths


-- | find all paths on the scaffold visiting every place
--   this is brute-forcing it and assuming it will not
--   have to backtrack and not end on a intersection
--   (intersections will not be
--   checked as they might be visitied more than once)
findPaths :: CameraView -> [[Movement]]
findPaths view = go view start startDir
  where
  go :: CameraView -> Coord -> Direction -> [[Movement]]
  go !curView !pos !dir = do
    let marked = if isIntersection pos then curView else M.insert pos Walked curView
    if allMarked marked
      then pure []
      else do
        (mvs, posAfter, dirAfter) <- possibleMoves marked pos dir
        rest <- go marked posAfter dirAfter
        pure $ mvs ++ rest

  (start, startDir) = findStart view

  possibleMoves curView pos dir =
    [ ([Forward 1], pos', dir)
      | let pos' = move dir pos
      , getTile curView pos' == Scaffold
    ] ++
    [ ([Turn TurnLeft, Forward 1], pos', dir')
      | let dir' = turn TurnLeft dir
      , let pos' = move dir' pos
      , getTile curView pos' == Scaffold
    ] ++
    [ ([Turn TurnRight, Forward 1], pos', dir')
      | let dir' = turn TurnRight dir
      , let pos' = move dir' pos
      , getTile curView pos' == Scaffold
    ]

  isIntersection = (`S.member` intersections)
  intersections = S.fromList $ scaffoldIntersections view

  allMarked curView = null
    [ c | c <- M.keys curView
        , let t = getTile curView c
        , t == Scaffold && not (isIntersection c)
    ]


findStart :: CameraView -> (Coord, Direction)
findStart view = head $ mapMaybe isStart $ M.keys view
    where
    isStart coord =
      case getTile view coord of
        Robot d -> Just (coord, d)
        _       -> Nothing

----------------------------------------------------------------------
-- helpers

showCameraView :: CameraView -> String
showCameraView view =
  unlines $ map showLine [minY..maxY]
  where
  showLine y = map (\x -> showCoord (x,y)) [minX..maxX]

  showCoord coord =
    case M.lookup coord view of
      Nothing -> '?'
      Just Empty -> '.'
      Just Scaffold -> '#'
      Just TumblingRobot -> 'X'
      Just (Robot DirUp) -> '^'
      Just (Robot DirDown) -> 'v'
      Just (Robot DirLeft) -> '<'
      Just (Robot DirRight) -> '>'
      Just Walked -> '*'

  Bounds (minX,minY) (maxX,maxY) = getBounds view

data Bounds = Bounds
      { topLeft :: Coord
      , bottomRight :: Coord
      }
      deriving Show

getBounds :: CameraView -> Bounds
getBounds view =
  Bounds (minX,minY) (maxX,maxY)
  where
  coords = M.keys view

  minX = minimum $ map fst coords
  maxX = maximum $ map fst coords
  minY = minimum $ map snd coords
  maxY = maximum $ map snd coords

----------------------------------------------------------------------
-- loading / parsing

loadInput :: IO Input
loadInput =
  parseProgram <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )