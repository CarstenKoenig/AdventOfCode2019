{-# LANGUAGE DeriveFunctor #-}
module Day12.Simulation 
  ( Moon (..)
  , Moons
  , findCycle
  , steps
  )
  where

import           Combinatorics
import           Data.List (foldl1')
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)


-- | make this generic so we can use 'fmap' 
--   this way it's really easy to get the
--   components later
data Moon a = Moon
  { position :: a
  , velocity :: a
  } deriving (Show, Eq, Ord, Functor)


type Moons a = Map String (Moon a)


-- | find a cycle by looking for the iteration
--   at which the first element is repeated
--   this is ok, as the transformation from
--   on system-state to the next is a bijective
--   map, so every state determins and is determined
--   by it's successor and predecessor
--   so IF you have a x_i and x_j with x_i == x_j
--   you know that x_0 == x_(j-i) also and you would
--   have found this here
findCycle :: (Ord a, Num a) => Moons a -> Int
findCycle moons = 
  findRepeatIn $ map getCoords $ iterate step moons
  where 
  getCoords = M.elems . M.map (\(Moon c v) -> (c,v))


-- | the system state after 'n' steps
--   this could be made more efficient by
--   looking for the cycle first, but
--   in my problem the 1000 steps from part1
--   are much smaller than the cycle-Length
--   so there is almost no gain in doing that
steps :: Num a => Int -> Moons a -> Moons a
steps n = head . drop n . iterate step


-- | a single step - applies velocities after gravity
step :: Num a => Moons a -> Moons a
step = applyVelocities . applyGravity


applyVelocities :: Num a => Moons a -> Moons a
applyVelocities = M.map applyVelocity
  where applyVelocity m = m { position = position m + velocity m }


applyGravity :: Num a => Moons a -> Moons a
applyGravity moons = M.unionWith addVelocity moons pulls
  where
  moonList = M.toList moons
  pulls = 
    M.fromListWith addVelocity 
    [ (n, applyGravityToFirst m m') | ((n,m),(_,m')) <- pairs moonList ]
  addVelocity m m' = m { velocity = velocity m + velocity m' }


applyGravityToFirst :: Num a => Moon a -> Moon a -> Moon a
applyGravityToFirst moon1 moon2 =
  moon1 { velocity = pull (position moon1) (position moon2) }


pull :: Num a => a -> a -> a
pull c c' = signum (c' - c)