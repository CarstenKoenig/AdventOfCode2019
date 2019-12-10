{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Day10.Solution where

import Prelude hiding (getLine)

import           Coords
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Maybe (mapMaybe)
import           Data.List (foldl', maximumBy, minimumBy)
import           Data.Ord (comparing)


data Input = Input
  { astroids :: Set Coord
  , width    :: Int
  , height   :: Int
  } deriving Show


run :: IO ()
run = do
  putStrLn "DAY 10"

  inp <- loadInput

  let (base, dist) = part1 inp
  putStrLn $ "\t Part 1: " ++ show dist

  let (x,y) = part2 inp base
  putStrLn $ "\t Part 2: " ++ show (x*100 + y)

  putStrLn "---\n"


----------------------------------------------------------------------
-- part 1

part1 :: Input -> (Coord, Int)
part1 = bestLocation


-- | finds the best location for a base:
--   it's the position from 'getSightMap' with
--   the fewest (=maximum as numbers are all negative)
--   blocked astroids
--   the total number of visible astroids is then the total
--   number of astroids minus so that cannot be seen (map value)
--   minus 1 because the base is not interested in the astroid it
--   sits on
bestLocation :: Input -> (Coord, Int)
bestLocation inp@Input{astroids} =
  let (c, doesNotSee) = seesMost $ getSightMap inp
  in (c, S.size astroids + doesNotSee - 1)
  where
  seesMost = maximumBy (comparing snd) . M.toList


-- | calculates a map with values = negative numbers indicating
--   how many astroids are sight-blocked from the position of the maps
--   key
getSightMap :: Input -> Map Coord Int
getSightMap inp@Input{astroids} =
  foldl' combine M.empty $ map (getSightMapFor inp) (S.toList astroids)
  where combine = M.unionWith (+)


-- | returns a map with values -1 at every location
--   that is sight-blocked from a astroid at given coord
getSightMapFor :: Input -> Coord -> Map Coord Int
getSightMapFor inp@Input{astroids} c = M.filterWithKey isAstroid $ 
  foldl' combine M.empty $ map mapFor (S.toList astroids)
  where
  isAstroid p _ = p `S.member` astroids
  mapFor a = M.fromList [ (c',-1) | c' <- sightBlockedAt inp c a ]
  combine = M.union


-- | returns all coords from which a base at the first coord
--   won't have sight because it's blocked by an astroid on the
--   second coord
sightBlockedAt :: Input -> Coord -> Coord -> [Coord]
sightBlockedAt inp@Input{..} (fX,fY) (tX,tY) 
  | (fX,fY) == (tX,tY) = []
  | otherwise = pos
  where 
  dX' = tX - fX
  dY' = tY - fY
  gc = gcd dX' dY'
  dX = dX' `div` gc
  dY = dY' `div` gc
  pos = takeWhile (inBounds inp) [ (tX + i*dX, tY + i*dY) | i <- [1..]]


-- | checks if a coordinate is still inside the bounds of the problem
inBounds :: Input -> Coord -> Bool
inBounds Input{width,height} (x,y) =
  0 <= x && x < width && 0 <= y && y < height

----------------------------------------------------------------------
-- part 2

-- | finds the 200th astroid destroyed by the sweeping laser
part2 :: Input -> Coord -> Coord
part2 inp base = sweep inp base !! 199

-- | enumerates all the astroids as in the problem describtion
--   to part 2 - just looks at the next astroid in sweep distance
--   that is in sight and queues astroids hidden behind it
--   at the end for the next sweep
sweep :: Input -> Coord -> [Coord]
sweep inp@Input{astroids} base = 
  go (S.filter (/= base) astroids) S.empty
  where
  go lookAt cont
    | S.null lookAt && S.null cont = []
    | S.null lookAt = go cont S.empty
    | otherwise =
      let next = findNextSweep base (S.toList lookAt)
          blocked = lookAt `S.intersection` S.fromList (sightBlockedAt inp base next)
          lookAt' = S.delete next lookAt `S.difference` blocked
          cont' = cont `S.union` blocked
      in next : go lookAt' cont'


-- | locates the next coordinate in sweep-polar-coordinates
findNextSweep :: Coord -> [Coord] -> Coord
findNextSweep base =
  minimumBy (comparing $ sweepDist base)


-- | computes a distance in polar-coordinates
--   with angle starting from the Y axis pointing upwards
--   the length is squared as it does not really matter
--   (x -> x^2 is a monoton function) and we can save the
--   time to calcluate the square-root of it
sweepDist :: Coord -> Coord -> (Double, Double)
sweepDist (x0,y0) (x,y) = (angle, dist)
  where
  dist = dX**2 + dY**2
  angle = getAngle dX dY
  dX = fromIntegral (x-x0)
  dY = fromIntegral (y0-y) -- Y-axis UPwards!


-- | get's the angle to the Y-axis (pointing up!)
--   so we can sort by this and get the right sweep
getAngle :: Double -> Double -> Double
getAngle x y =
  let a = atan2 x y
  in if a < 0 then 2*pi + a else a


----------------------------------------------------------------------
-- loading / parsing

loadInput :: IO Input
loadInput = parseInput <$> readFile "./src/Day10/input.txt"


parseInput :: String -> Input
parseInput inp =
  Input (S.fromList $ concatMap getLine ls) width height
  where
    ls = zip [0..] $ lines inp
    height = length ls
    width = length . snd $ head ls
    getLine (y, cs) = mapMaybe (getPos y) $ zip [0..] cs
    getPos y (x, '#') = Just (x,y)
    getPos _ _ = Nothing