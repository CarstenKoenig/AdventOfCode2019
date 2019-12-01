module Day1 where

import Data.List (unfoldr)

type Input = [Mass]

type Mass = Int
type Fuel = Int


run :: IO ()
run = do
  putStrLn "DAY 1"

  input <- loadInput

  let part1 = totalRequiredFuel requiredFuelForMass input
  putStrLn $ "\t Part 1: " ++ show part1

  let part2 = totalRequiredFuel requiredFuelForMassAndFuel input
  putStrLn $ "\t Part 2: " ++ show part2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 2

-- | calculated the total fuel for a modules mass by including fuel-for-fuel
requiredFuelForMassAndFuel :: Mass -> Fuel
requiredFuelForMassAndFuel mass =
  let fuelForMass = requiredFuelForMass mass
  in fuelForMass + requiredFuelForFuel fuelForMass


-- | fuel ~ mass and mass needs fuel so we have to calculate more fuel
--   thankfully the additional fuel will become non-positive and we can stop there
requiredFuelForFuel :: Fuel -> Fuel
requiredFuelForFuel fuel = sum $ additionalFuelSeq fuel
  where
    additionalFuelSeq = unfoldr requiredForFuelMass
    requiredForFuelMass fuel =
      let req = requiredFuelForMass (fuelMass fuel)
      in if req <= 0 then Nothing else Just (req, req)


----------------------------------------------------------------------
-- basic calculations / Part 1 and 2

-- | calculated the fuel requriement by summing up the requirements
--   for each modules mass
totalRequiredFuel :: (Mass -> Fuel) -> [Mass] -> Fuel
totalRequiredFuel mass2fuel = sum . map mass2fuel


-- | the elf-physicist derived this formula - don't doubt it
requiredFuelForMass :: Mass -> Fuel
requiredFuelForMass mass = mass `div` 3 - 2


-- | seems each amount of fuel corresponds to the same amount of mass
--   lucky!
fuelMass :: Fuel -> Mass
fuelMass = id


----------------------------------------------------------------------
-- load and parse input

-- | loads the input - each line contains a single number (the mass of a module)
loadInput :: IO Input
loadInput = fmap read . lines <$> readFile "./src/day1.txt"
