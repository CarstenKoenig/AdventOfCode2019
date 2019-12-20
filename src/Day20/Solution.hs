{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Day20.Solution where

import           Coords
import           Data.Char (isLetter)
import           Data.List (sort)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S

dayNr :: Int
dayNr = 20

type Input = Maze

type Maze = M.Map Coord Tile

data Tile
  = Wall
  | Empty
  | Walk
  | Portal Portal Coord
  deriving (Show, Eq)


run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr
  maze <- loadInput

  let steps1 = findExitSteps maze
  putStrLn $ "\t Part 1: " ++ show steps1

  let steps2 = findExitStepsWithLevel maze
  putStrLn $ "\t Part 2: " ++ show steps2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 1 - BFS

type Distance = Int

findExitSteps :: Maze -> Distance
findExitSteps maze = go S.empty [(0,entry)]
  where
  -- no more nodes to explore?
  go _ [] = error "no path found"
  -- explore the next node
  go visited ((dist, !next):rest)
    -- found the exit?
    | next == exit = dist
    -- already visited?
    | S.member next visited = go visited rest
    | otherwise =
      let visited' = S.insert next visited
          -- look for neighbours - follows portals
          neighs   = findNeighbours visited' next
      in go visited' (rest ++ map (dist+1,) neighs)

  portals = findPortals maze

  entry = entryCoord portals

  exit = exitCoord portals

  findNeighbours visited coord =
    mapMaybe goTo [ c | c <- neighbours coord, not (c `S.member` visited) ]

  -- goes to the coord and follows portals if needed
  goTo coord =
    case getAt maze Empty coord of
      Walk -> Just coord
      Portal pn _ -> 
        let portalCoords = getAt portals [] pn
        in case filter ((/= coord) . fst) portalCoords of
          [] -> Nothing
          [(_,ex)] -> Just ex
          _ -> error "found multiple exits"
      _ -> Nothing

----------------------------------------------------------------------
-- Part 2: same as Part 1 - nodes are tagged with their levels in the
--                          recursive maze

findExitStepsWithLevel :: Maze -> Distance
findExitStepsWithLevel maze = go S.empty [(0,0::Int,entry)]
  where
  -- no more nodes to explore?
  go _ [] = error "no path found"
  go visited ((dist, lvl, !next):rest)
    -- found the exit on level 0?
    | (lvl,next) == exit = dist
    -- already visited?
    | S.member (lvl,next) visited = go visited rest
    | otherwise =
      let visited' = S.insert (lvl,next) visited
          -- look for neighbour - follow portals
          neighs   = findNeighbours lvl visited' next
      in go visited' (rest ++ map (\(lvl',coord) -> (dist+1,lvl',coord)) neighs)

  portals = findPortals maze

  entry = entryCoord portals

  bounds = getBounds maze

  exit = (0, exitCoord portals)

  findNeighbours lvl visited coord =
    mapMaybe (goTo lvl) [ c | c <- neighbours coord, not ((lvl,c) `S.member` visited) ]

  -- goes to the coord and follows portals (updating levels) if possible
  goTo lvl coord =
    case getAt maze Empty coord of
      -- on a walk we stay on the same level
      Walk -> Just (lvl, coord)
      Portal pn _ 
        -- outer portals are only usable at levels > 0
        | isOuter bounds coord && lvl == 0 -> Nothing
        | otherwise ->
          let portalCoords = getAt portals [] pn
          in case filter ((/= coord) . fst) portalCoords of
            []       -> Nothing
                             -- outer go up one level, inner go down 
            [(_,ex)] -> Just (if isOuter bounds coord then lvl-1 else lvl+1, ex)
            _ -> error "found multiple exits"
      _ -> Nothing


----------------------------------------------------------------------
-- Portals, Entrance, Exit

-- | map from a portal-name to the pair
--   (portal-coordinate, portal-exit-coordinate)
--   after following a portal you are at it's exit-coordinate
type Portals = M.Map Portal [(Coord, Coord)]
type Portal = String

entryCoord :: Portals -> Coord
entryCoord ps = snd $ head $ getAt ps (error "entry not found") "AA"

exitCoord :: Portals -> Coord
exitCoord ps = snd $ head $ getAt ps (error "exit not found") "ZZ"

findPortals :: Maze -> Portals
findPortals =
  M.fromListWith (++) . mapMaybe getPortal . M.toList
  where
    getPortal (coord, Portal p exit) = Just (p, [(coord, exit)])
    getPortal _ = Nothing


----------------------------------------------------------------------
-- bounds of the maze

data Bounds = Bounds
  { minX :: Int 
  , maxX :: Int
  , minY :: Int
  , maxY :: Int
  }
  deriving (Show, Eq)


getBounds :: Maze -> Bounds
getBounds mz =
  let 
    xs = map fst $ M.keys mz
    ys = map snd $ M.keys mz
    max_x = maximum xs
    min_x = minimum xs
    max_y = maximum ys
    min_y = minimum ys
  in Bounds min_x max_x min_y max_y


----------------------------------------------------------------------
-- helpers

-- | is the coordinate on the outer bounds of the maze?
isOuter :: Bounds -> Coord -> Bool
isOuter Bounds{..} (x,y) =
  (x <= minX + 2 || x >= maxX - 2) ||
  (y <= minY + 2 || y >= maxY - 2) 


-- | shorthand for 'M.findWithDefault'
getAt :: Ord key => M.Map key a -> a -> key -> a
getAt m def k = M.findWithDefault def k m


----------------------------------------------------------------------
-- loading / parsing

loadInput :: IO Input
loadInput = parseInput <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )


parseInput :: String -> Input
parseInput =
  parseMaze . readInput


parseMaze :: CharMap -> Maze
parseMaze cm =
  M.mapWithKey (\at ch ->
    case ch of
      '#' -> Wall
      ' ' -> Empty
      '.' -> Walk
      c | isLetter c -> findPortal cm at c
        | otherwise -> error $ "unknown tile " ++ show c
    )
    cm

type CharMap = M.Map Coord Char

findPortal :: CharMap -> Coord -> Char -> Tile
findPortal cm at part =
  case nextToWalk of
    [(_, walkAt)] ->
      let other = fst . head $ filter (isLetter . fst) neighs
      in Portal (sort [part,other]) walkAt
    [] -> Empty
    _  -> error $ "multiple exists to portal at " ++ show at
  where
  nextToWalk = filter ((== '.') . fst) neighs
  neighs = [ (getAt cm ' ' c, c) | c <-neighbours at ]


readInput :: String -> CharMap
readInput = 
  M.fromList . concat . zipWith parseLine [0..] . lines
  where
  parseLine y = zipWith (parseChar y) [0..]
  parseChar y x c = ((x,y), c)