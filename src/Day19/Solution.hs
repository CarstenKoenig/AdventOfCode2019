module Day19.Solution where

import           Coords
import qualified Data.Set as S
import           IntCodePure


dayNr :: Int
dayNr = 19

type Input = Continuation Int

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr
  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 1

part1 :: Input -> Int
part1 inp = beamArea inp 49 49


-- | beam area is everything but the parts above the upper
--   and the parts left of the lower Edge of the beam
--   this assumes it's continous
beamArea :: Input -> Int -> Int -> Int
beamArea inp maxX maxY =
  let noBeam = S.union (areaAboveUpperEdge inp 0 maxX) (areaInFrontLowerEdge inp 0 maxY)
  in (maxX+1) * (maxY+1) - S.size noBeam


areaAboveUpperEdge :: Input -> Int -> Int -> S.Set Coord
areaAboveUpperEdge inp sX eX = S.fromList
  [ (x,y') | (x,y) <- upperEdge inp sX eX, y' <- [0..y-1] ]


areaInFrontLowerEdge :: Input -> Int -> Int -> S.Set Coord
areaInFrontLowerEdge inp sY eY = S.fromList
  [ (x',y) | (x,y) <- lowerEdge inp sY eY, x' <- [0..x-1] ]


-- | traces the upper edge
--   note that there are 2 "hole" in the first points
--   of the beam therefore the y coordinate is constricted as well
upperEdge :: Input -> Int -> Int -> [Coord]
upperEdge inp sX maxV = go (sX,0)
  where
  go c@(x,y)
    | x > maxV = []
    | y > maxV = (x,y) : go (x+1,0)
    | otherwise =
      if isPulledAt inp c
        then (x,y) : go (x+1, y)
        else go (x, y+1)


-- | traces the lower edge
--   note that there are 2 "hole" in the first points
--   of the beam therefore the x coordinate is constricted as well
lowerEdge :: Input -> Int -> Int -> [Coord]
lowerEdge inp sY maxV = go (0,sY)
  where
  go c@(x,y)
    | y > maxV = []
    | x > maxV = (x,y) : go (0,y+1)
    | otherwise =
      if isPulledAt inp c
        then (x,y) : go (x, y+1)
        else go (x+1, y)


----------------------------------------------------------------------
-- Part 2

part2 :: Input -> Int
part2 prg =
  let (x,y) = findSpace prg (0,0)
  in x*10000 + y


-- | scans the lower-left corner on the edge of the beam
--   as soon as it and the upper-right are both pulled
--   returns the top-left corner as this should be closest
--   to the origin
findSpace :: Input -> Coord -> Coord
findSpace cont (x,y) =
  if isPulledAt cont (x,y)
    then if isPulledAt cont (x+99, y-99)
      then (x, y-99)
      else findSpace cont (x,y+1)
    else findSpace cont (x+1,y)


----------------------------------------------------------------------
-- helpers

-- | determins if the beam is pulling at the given coordinate
--   this one takes quite some time which is the only "difficulty"
--   for this day
--   so calls to this should be limited, which is done for part 1
--   by only following the the edges and in part 2 by following the
--   lower edge till it fits
isPulledAt :: Input -> Coord -> Bool
isPulledAt cont (x,y) =
  go cont [x,y]
  where
    go cc buffer =
      case runComputer cc of
        Halted           -> error "drone system halted"
        Error err        -> error $ "error in drone system " ++ err
        RequestInput i2c ->
          case buffer of
            []   -> error "drone wants input, have none"
            i:is -> go (i2c i) is
        NewOutput out cc' ->
            out == 1

----------------------------------------------------------------------
-- loading / parsing

loadInput :: IO Input
loadInput =
  initComputer . parseProgram <$>
  readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )