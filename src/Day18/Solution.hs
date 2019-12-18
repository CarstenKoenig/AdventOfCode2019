{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

{- | remarks:
     sadly today is really slow (~20s both) so I should look into it when
     I have some time
-}
module Day18.Solution where

import           Coords
import           Data.Char (toUpper, isUpper, isLower)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

dayNr :: Int
dayNr = 18

type Input = Vault

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  vault1 <- loadInput "input.txt"
  let res1 = solve vault1
  putStrLn $ "\t Part 1: " ++ show res1

  let vault2 = into4Vaults vault1
  let res2 = solve vault2
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 2

-- | split the vault into 4
into4Vaults :: Vault -> Vault
into4Vaults vault =
  let objs = findObjects vault
      [(eX,eY)] = entrances objs
      changed =
        [ ((eX,  eY  ), Wall)
        , ((eX-1,eY  ), Wall)
        , ((eX+1,eY  ), Wall)
        , ((eX,  eY-1), Wall)
        , ((eX,  eY+1), Wall)
        , ((eX-1,eY-1), Entrance)
        , ((eX+1,eY-1), Entrance)
        , ((eX-1,eY+1), Entrance)
        , ((eX+1,eY+1), Entrance)
        ]
  in M.union (M.fromList changed) vault


----------------------------------------------------------------------
-- Part 1

-- | solve by searching a minimum path
solve :: Input -> Int
solve vault =
  let objs = findObjects vault
  in searchPath vault objs

type Vault = M.Map Coord Tile

data Tile
  = Open
  | Wall
  | Entrance
  | Key KeyChar
  | Door KeyChar
  deriving (Show, Eq, Ord)

type KeyChar = Char

type Cache = M.Map ([Coord], String) Distance
type CacheR = M.Map (Coord, String) [Reachable]

-- | searches the minimum path using dynamic programming
--   with some manual caching
searchPath :: Vault -> Objects -> Distance
searchPath vault objs =
  (\(d,_,_) -> d) $ go M.empty M.empty (entrances objs) S.empty
  where
  go :: Cache -> CacheR -> [Coord] -> FoundKeys -> ( Distance, Cache, CacheR)
  -- if all keys are collected we are done - nothing more to walk
  go !cache !cacheR !curCoords !colKeys =
    maybe (go' cache cacheR curCoords colKeys) (\d -> (d,cache,cacheR)) $
    M.lookup (curCoords, S.toList colKeys) cache
  go' :: Cache -> CacheR -> [Coord] -> FoundKeys -> ( Distance, Cache, CacheR)
  go' !cache !cacheR !curCoords !colKeys =
    let
      (rs, cacheR') =
        foldl' 
          (\(acc, cR) (ind, c) ->
            let (res, cR') = cReachable cR c colKeys
            in (map (ind,) res ++ acc, cR')
          )
          ([], cacheR)
          (zip [(0::Int)..] curCoords) 
      cacheKey = S.toList colKeys
    in if null rs
      then (0, M.insert (curCoords, cacheKey) 0 cache, cacheR')
      else
        let (minD, cache', cacheR'') = foldl' findMin (maxBound, cache, cacheR') rs
            cache'' = M.insert (curCoords, cacheKey) minD cache'
        in (minD, cache'',cacheR'')
    where
    findMin (!curMin, !curCache, !curCacheR) (ind, RKey !kc !dist !coord) =
      let
        coords' = [ if i == ind then coord else c | (i,c) <- zip [(0::Int)..] curCoords ]
        (!minD, !cache', !cacheR') = go curCache curCacheR coords' (S.insert kc colKeys)
      in (min curMin (minD + dist), cache', cacheR')
  cReachable :: CacheR -> Coord -> FoundKeys -> ([Reachable], CacheR)
  cReachable cacheR coord foundKeys =
    case M.lookup (coord, S.toList foundKeys) cacheR of
      Just res -> (res, cacheR)
      Nothing ->
        let res = reachable vault foundKeys coord
            cacheR' = M.insert (coord, S.toList foundKeys) res cacheR
        in (res, cacheR')

----------------------------------------------------------------------
-- what keys are reachable in what distance?

type Distance = Int

data Reachable
  = RKey KeyChar Distance Coord
  deriving (Show, Eq)

type FoundKeys = S.Set KeyChar

-- | finds all reachable keys in vault from position
--   where a set of keys have already been found
--   using BFS
reachable :: Vault -> FoundKeys -> Coord -> [Reachable]
reachable vault !keys !fromCoord =
  go S.empty [] [(fromCoord, 0)]
  where
  go _ !found [] = found
  go !visited !found ((!coord, _) : next)
    | S.member coord visited = go visited found next
  go visited !found ((!coord, !dist) : nexts) =
    let
      tile = getTile vault coord
      visited' = S.insert coord visited
      found' =
        case tile of
          Key kc | not (S.member kc keys)
            -> RKey kc dist coord : found
          _ -> found
      nexts' =
        if isOpen keys tile
          then nexts ++ [ (nc, dist+1) | nc <- neighbours coord
                                       , not (nc `S.member` visited')
                                       , not (isWall $ getTile vault nc) ]
          else nexts
    in go visited' found' (seq nexts' nexts')


----------------------------------------------------------------------
-- locating entrances and keys as we need those frequently

data Objects = Objects
  { entrances :: [Coord]
  , keys      :: M.Map KeyChar Coord
  }
  deriving Show


findObjects :: Vault -> Objects
findObjects =
  M.foldlWithKey' addObjAt (Objects [] M.empty)
  where
    addObjAt objs _ Open          = objs
    addObjAt objs _ Wall          = objs
    addObjAt objs coord Entrance  = objs { entrances = coord : entrances objs }
    addObjAt objs coord (Key kc)  = objs { keys = M.insert kc coord (keys objs) }
    addObjAt objs _ (Door _)      = objs


----------------------------------------------------------------------
-- helpers

isWall :: Tile -> Bool
isWall Wall = True
isWall _    = False


isOpen :: FoundKeys -> Tile -> Bool
isOpen _ Entrance     = True
isOpen _ Open         = True
isOpen keys (Key  kc) = S.member kc keys
isOpen keys (Door kc) = S.member kc keys
isOpen _ _            = False


getTile :: Vault -> Coord -> Tile
getTile vault coord =
  case M.lookup coord vault of
    Nothing -> Wall
    Just Wall -> Wall
    Just Entrance -> Open
    Just Open -> Open
    Just other -> other


----------------------------------------------------------------------
-- loading / parsing

loadInput :: FilePath -> IO Input
loadInput file =
  parseInput <$> readFile ( "./src/Day" ++ show dayNr ++ "/" ++ file )


parseInput :: String -> Vault
parseInput =
  M.fromList . concat .
  zipWith parseLine [0..] . lines


parseLine :: Int -> String -> [(Coord, Tile)]
parseLine y =
  zipWith (\x c -> ((x,y), parseTile c)) [0..]


parseTile :: Char -> Tile
parseTile '.' = Open
parseTile '#' = Wall
parseTile '@' = Entrance
parseTile c | isLower c = Key (toUpper c)
            | isUpper c = Door c
parseTile x = error $ "invalid Tile: " ++ show x