module Day3.Solution where

import           CommonParsers
import           ConsoleTests
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC


run :: IO ()
run = do
  putStrLn "DAY 3"

  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Solutions

part1 :: ([Move], [Move]) -> Int
part1 =
  findCombinedMinimum (manhDist (0,0) . fst)


part2 :: ([Move], [Move]) -> Int
part2 =
  findCombinedMinimum snd


----------------------------------------------------------------------
-- Algorithms

type Coord = (Int, Int)
type PathLenght = Int
type Dist = Int

data Move
  = GoUp Dist
  | GoDown Dist
  | GoLeft Dist
  | GoRight Dist
  deriving (Show)


manhDist :: Coord -> Coord -> Dist
manhDist (x,y) (x',y') = abs (x'-x) + abs (y'-y)


-- | calculates a new coord after a move
move :: Move -> Coord -> Coord
move (GoUp u) (x,y) = (x,y+u)
move (GoDown d) (x,y) = (x,y-d)
move (GoLeft l) (x,y) = (x-l,y)
move (GoRight r) (x,y) = (x+r,y)


-- | returns all the coordinates just after the start-point along the move
--   including the end-coordinate
coordsTo :: Move -> Coord -> [Coord]
coordsTo (GoUp u) (x,y) = [(x,y+i) | i <- [1..u]]
coordsTo (GoDown d) (x,y) = [(x,y-i) | i <- [1..d]]
coordsTo (GoLeft l) (x,y) = [(x-i,y) | i <- [1..l]]
coordsTo (GoRight r) (x,y) = [(x+i,y) | i <- [1..r]]


-- | collects all the coordinates visited along the path given by the moves
--   starting at (0,0) putting them into a Map with coordinate-keys
--   and values being the length of the path to the coord
--   please note that this works because the list given to 'Map.fromList' 
--   will be ordered such that a revisit later along the path will
--   come before an earlier visit in the list - so 'Map.fromList'
--   can safetly choose the shorter value
--   we could make this obvious by using 'Map.fromListWith min' instead
collectCoordsWithLen :: [Move] -> Map Coord PathLenght
collectCoordsWithLen mvs =
  Map.fromList $ go [] 0 mvs (0,0)
  where
    go acc _ [] _ = acc
    go acc l (m:ms) c =
      let
        end = move m c
        dist = manhDist end c
        pts = coordsTo m c
      in go (zip pts [l+1..] ++ acc) (l+dist) ms end


-- | combine two grids of paths adding the length-values
combinedCoords :: ([Move], [Move]) -> [(Coord, PathLenght)]
combinedCoords (ms1, ms2) =
  let
    grd1 = collectCoordsWithLen ms1
    grd2 = collectCoordsWithLen ms2
  in Map.toList $ Map.intersectionWith (+) grd1 grd2


-- | uses 'combinedCoords' and 'mapVal' to find a minimum in the combined grid
findCombinedMinimum 
    :: Ord minVal 
    => ((Coord, PathLenght) -> minVal) -> ([Move], [Move]) -> minVal
findCombinedMinimum mapVal inp =
  let
    common = combinedCoords inp
    values = map mapVal common
  in minimum values
----------------------------------------------------------------------
-- loading / parsing of input

loadInput :: IO ([Move], [Move])
loadInput =
  parseInput <$> readFile "./src/Day3/input.txt"


-- | parses the input - expecting exactly two lines of
--   comma-separated moves
parseInput :: String -> ([Move], [Move])
parseInput input =
  let [mvs1, mvs2] = map parseMoves $ lines input
  in (mvs1,mvs2)


-- | parses a comma-separated list of 'Move's
parseMoves :: String -> [Move]
parseMoves input =
  let res = P.parse movesP "Moves" input
  in case res of
    Left _ -> error "could not parse line"
    Right r -> r


-- | Parser for a comma-separated list of 'Move's
movesP :: Parser [Move]
movesP = moveP `P.sepBy1` PC.char ','


-- | Parser for a single move formated like
--   (U|D|L|R)(number)
moveP :: Parser Move
moveP = do
  c <- PC.alphaNumChar
  n <- numberP
  case c of
    'U' -> pure $ GoUp n
    'D' -> pure $ GoDown n
    'L' -> pure $ GoLeft n
    'R' -> pure $ GoRight n
    _   -> fail "direction-letter expected"


----------------------------------------------------------------------
-- some simple tests from the problem description

tests :: IO ()
tests = do
  assert "simple test" testInput0 (6, 30)
  assert "Test-Input 1" testInput1 (159, 610)
  assert "Test-Input 2" testInput2 (135, 410)


testInput0 :: String
testInput0 =
  "R8,U5,L5,D3\nU7,R6,D4,L4"

testInput1 :: String
testInput1 =
  "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"

testInput2 :: String
testInput2 =
  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

assert :: String -> String -> (Int, Int) -> IO ()
assert label testInput (expected1, expected2) = do
  let input = parseInput testInput
  part1 input `should1` expected1
  part2 input `should2` expected2
  where 
    should1 = shouldEqual (label ++ " - part 1")
    should2 = shouldEqual (label ++ " - part 2")