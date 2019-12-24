{-# LANGUAGE TupleSections #-}
module Day24.Solution where

import           Coords
import qualified Data.Map.Strict as M


dayNr :: Int
dayNr = 24

type Input = M.Map (Level, Coord) Tile

type Level = Int
data Tile = Empty | Bug
  deriving (Show, Eq, Ord)


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
-- part 1

part1 :: Input -> Int
part1 inp =
  let (_,state,_) = findCycle $ steps inp
  in biodiversityRating state


findCycle :: (Eq a, Ord a) => [a] -> (Int, a, Int)
findCycle = go M.empty 0
  where
  go _ _ []        = error "could not detect any cycle"
  go seen n (x:xs) =
    case M.lookup x seen of
      Just n' -> (n', x, n)
      Nothing -> go (M.insert x n seen) (n+1) xs


biodiversityRating :: Input -> Int
biodiversityRating inp =
  sum [ biodiv (y*5+x) (getTile inp (0,(x,y))) | y <- [0..4], x <- [0..4] ]
  where
  biodiv _ Empty = 0
  biodiv n Bug = 2^n


steps :: Input -> [Input]
steps = iterate step


step :: Input -> Input
step inp =
  M.mapWithKey (\coord _ -> nextAtCoord coord) inp
  where
    nextAtCoord coord =
      case (getTile inp coord, nrBugsAdjacentSameLevel inp coord) of
        (Bug, 1) -> Bug
        (Empty, n) | n == 1 || n== 2 -> Bug
        _        -> Empty


----------------------------------------------------------------------
-- part 2

part2 :: Input -> Int
part2 inp =
  let s200 = stepsRec inp !! 200
  in nrBugs s200


nrBugs :: Input -> Int
nrBugs inp =
  length [ b | b <- M.elems inp, b == Bug ]


stepsRec :: Input -> [Input]
stepsRec inp =
  snd <$> iterate stepRec (0, inp)


stepRec :: (Int, Input) -> (Int, Input)
stepRec (d, inp) =
  let d' = d+1
  in (d', step' $ M.union (M.fromList [ ((lvl, (x,y)), Empty) | lvl <- [(negate d'), d'], x <- [0..4], y <- [0..4] ]) inp)
  where
    step' = M.mapWithKey (\coord _ -> nextAtCoordRec coord)

    nextAtCoordRec coord =
      case (getTile inp coord, recNrBugsAdjacent inp coord) of
        (Bug, 1) -> Bug
        (Empty, n) | n == 1 || n== 2 -> Bug
        _        -> Empty


recNrBugsAdjacent :: Input -> (Level, Coord) -> Int
recNrBugsAdjacent inp c@(lvl,_) =
  case snd c of
    (2,2) -> 0 -- ignore center
    -- inner fields going down a level
    (1,2) -> recNrBugsVer (lvl+1) 0 + nrBugsAdjacentSameLevel inp c
    (3,2) -> recNrBugsVer (lvl+1) 4 + nrBugsAdjacentSameLevel inp c
    (2,1) -> recNrBugsHor (lvl+1) 0 + nrBugsAdjacentSameLevel inp c
    (2,3) -> recNrBugsHor (lvl+1) 4 + nrBugsAdjacentSameLevel inp c
    -- top row going up a level
    (0,0) -> countBug (lvl-1) (1,2) + countBug (lvl-1) (2,1) + nrBugsAdjacentSameLevel inp c
    (4,0) -> countBug (lvl-1) (3,2) + countBug (lvl-1) (2,1) + nrBugsAdjacentSameLevel inp c
    (_,0) -> countBug (lvl-1) (2,1) + nrBugsAdjacentSameLevel inp c
    -- bottom row going up a level
    (0,4) -> countBug (lvl-1) (1,2) + countBug (lvl-1) (2,3) + nrBugsAdjacentSameLevel inp c
    (4,4) -> countBug (lvl-1) (3,2) + countBug (lvl-1) (2,3) + nrBugsAdjacentSameLevel inp c
    (_,4) -> countBug (lvl-1) (2,3) + nrBugsAdjacentSameLevel inp c
    -- left side going up
    (0,_) -> countBug (lvl-1) (1,2) + nrBugsAdjacentSameLevel inp c
    -- right side going up
    (4,_) -> countBug (lvl-1) (3,2) + nrBugsAdjacentSameLevel inp c
    -- all other fields
    (_,_) -> nrBugsAdjacentSameLevel inp c
  where
  countBug _ (2,2) = 0
  countBug lvl' c' =
    if getTile inp (lvl', c') == Bug then 1 else 0

  recNrBugsHor lvl' y =
    length [ x | x <- [0..4], getTile inp (lvl', (x,y)) == Bug ]

  recNrBugsVer lvl' x =
    length [ y | y <- [0..4], getTile inp (lvl', (x,y)) == Bug ]


----------------------------------------------------------------------
-- helpers


getTile :: Input -> (Level, Coord) -> Tile
getTile inp coord =
  M.findWithDefault Empty coord inp


nrBugsAdjacentSameLevel :: Input -> (Level, Coord) -> Int
nrBugsAdjacentSameLevel inp (lvl, coordOnLvl) =
  length [ c | c <- neighbours coordOnLvl, getTile inp (lvl,c) == Bug ]


showInput :: Input -> Level -> String
showInput inp lvl =
  unlines [ showLine y | y <- [0..4]]
  where
    showLine y = [ showTile (getTile inp (lvl, (x,y))) | x <- [0..4]]
    showTile Bug = '#'
    showTile Empty = '.'


----------------------------------------------------------------------
-- loading / parsing

loadInput :: IO Input
loadInput =
  parseInput <$> readFile ("./src/Day" ++ show dayNr ++ "/input.txt")


parseInput :: String -> Input
parseInput = M.fromList . concat . zipWith parseLine [0..] . lines
  where
    parseLine y = zipWith (\x -> ((0,(x,y)),) . parseTile) [0..]

    parseTile '.' = Empty
    parseTile '#' = Bug
    parseTile c   = error $ "unknown tile char " ++ show c
