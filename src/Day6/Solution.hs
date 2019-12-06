{-# LANGUAGE NamedFieldPuns #-}
module Day6.Solution where

import           Data.Function (on)
import qualified Data.Graph as G
import           Data.List (sortBy, groupBy, break, foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)


type Input = [Orbit]

data OrbitMap = OrbitMap
  { graph          :: G.Graph
  , getEdges       :: G.Vertex -> [Object]
  , getVertex      :: Object -> Maybe G.Vertex
  }

data Orbit = Orbit
  { center   :: Object
  , satelite :: Object
  } deriving Show

type Object = String

run :: IO ()
run = do
  putStrLn "DAY 6"

  oMap <- createOrbitMap <$> loadInput

  putStrLn $ "\t Part 1: " ++ show (countOrbits oMap)
  putStrLn $ "\t Part 2: " ++ show (findMinTransfers "YOU" "SAN" oMap)

  putStrLn "---\n"


-- | the total direct and indirect orbit-count is
--   just the count of all transitive connections in the graph
--   luckily there is a function 'Data.Graph.reachable' that
--   does all the heavy work for us
countOrbits :: OrbitMap -> Int
countOrbits OrbitMap{graph} =
  let vertices = G.vertices graph
  in sum $ map (length . G.reachable graph) vertices


-- | the minimum number of transfer between two objects
--   is just the minimum length of paths from each of
--   the objects to mutable reachable nodes but we have
--   to subtract 2: one for the direct orbiting transfer at
--   the common node and another because we don't have to
--   orbit Santa - just the planet Santa orbits 
findMinTransfers :: Object -> Object -> OrbitMap -> Int
findMinTransfers obj1 obj2 oMap =
  minimum (map snd $ Map.toList commonTransferCounts) - 2
  where
    commonTransferCounts = 
      transferCounts oMap obj1 `combine` transferCounts oMap obj2
    combine = Map.intersectionWith (+)


-- | builds a Map with all reachable vertices from an
--   object together with the path-length to there
transferCounts :: OrbitMap -> Object -> Map G.Vertex Int
transferCounts OrbitMap{getVertex, getEdges} obj =
  case getVertex obj of
    Nothing -> Map.empty
    Just v -> collectPath Map.empty 0 v
  where
    collectPath m tCnt v = foldl' 
      (\m' -> collectPath m' (tCnt+1)) 
      (Map.insertWith min v tCnt m) 
      (mapMaybe getVertex $ getEdges v)


-- | turns the input into a Graph describing the Orbit-Map
--   two objects s and p in the graph are directly connected IFF
--   `p)s` is in the input
--   that is: there is an edge from s to p if s orbits p
createOrbitMap :: Input -> OrbitMap
createOrbitMap inp =
  let (gr, v2o, o2v) = toGraph inp
  in OrbitMap gr (\v -> let (_,_,os) = v2o v in os) o2v
  where
  toGraph orbits =  G.graphFromEdges [ (satelite k, satelite k, map center gr) | gr@(k:_) <- groupOrbits ]
    where
    groupOrbits = groupBy ((==) `on` satelite) sortedOrbits
    sortedOrbits = sortBy (comparing satelite) orbits

----------------------------------------------------------------------
-- input: loading and parsing

loadInput :: IO Input
loadInput = parseInput <$> readFile "./src/Day6/input.txt"


parseInput :: String -> [Orbit]
parseInput = map parseLine . lines

-- | a line is expected to be [center])[satelite]
parseLine :: String -> Orbit
parseLine inp = let (a,b) = parts inp in Orbit a b
    where
      parts s =
        let (a,b) = break (== ')') s
        in (a, tail b)