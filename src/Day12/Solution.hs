module Day12.Solution where

import           Combinatorics
import           Data.List (foldl1')
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Day12.Simulation as Sim
import qualified Vector as V
import           Vector (Vector3(V))


dayNr :: Int
dayNr = 12

type Moon = Sim.Moon Vector3
type Moons = Sim.Moons Vector3


run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  let res1 = part1 moons
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 moons
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Moons -> Int
part1 = systemEnergy . steps 1000


part2 :: Moons -> Int
part2 = findCycle


----------------------------------------------------------------------
-- Part 1

-- | steps are done component-wise using the simulation module
steps :: Int -> Moons -> Moons
steps n moons =
  M.fromList $ zipWith3 combine xMoons yMoons zMoons
  where
  combine (n, Sim.Moon pX vX) (_, Sim.Moon pY vY) (_, Sim.Moon pZ vZ) = (n,Sim.Moon (V (pX,pY,pZ)) (V (vX,vY,vZ)))
  xMoons = M.toList $ Sim.steps n $ M.map (fmap V.x) moons
  yMoons = M.toList $ Sim.steps n $ M.map (fmap V.y) moons
  zMoons = M.toList $ Sim.steps n $ M.map (fmap V.z) moons


systemEnergy :: Moons -> Int
systemEnergy = sum . map energy . M.elems

energy :: Moon -> Int
energy m = potentialEnergy m * kineticEnergy m

potentialEnergy :: Moon -> Int
potentialEnergy m = let V (x,y,z) = Sim.position m in abs x + abs y + abs z

kineticEnergy :: Moon -> Int
kineticEnergy m = let V (x,y,z) = Sim.velocity m in abs x + abs y + abs z

----------------------------------------------------------------------
-- Part 2

-- | finds the cylce of the system by finding the cycles for each
--   coordinate and then calculating the least-common-multiple for
--   those cycle-lenghts
--   this is valid as everything works combonentwise so we
--   can understand the system by understanding the projections
findCycle :: Moons -> Int
findCycle moons = 
  foldl1' lcm $ map findAxisCycle [V.x, V.y, V.z]
  where
  findAxisCycle ax = Sim.findCycle $ M.map (fmap ax) moons


----------------------------------------------------------------------
-- Input 

-- | the input is so tiny that I decided to not parse it
--   I manually entered it - so you have to override it with your own
--   if you want to use this
moons :: Moons
moons = M.fromList
  [ ("Io", Sim.Moon (V (-17, 9, -5)) 0)
  , ("Europa", Sim.Moon (V (-1, 7, 13)) 0)
  , ("Ganymede", Sim.Moon (V (-19, 12, 5)) 0)
  , ("Callisto", Sim.Moon (V (-6, -6, -4)) 0)
  ]