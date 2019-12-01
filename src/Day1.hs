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


totalRequiredFuel :: (Mass -> Fuel) -> [Mass] -> Fuel
totalRequiredFuel mass2fuel = sum . map mass2fuel


requiredFuelForMassAndFuel :: Mass -> Fuel
requiredFuelForMassAndFuel mass =
  let fuelForMass = requiredFuelForMass mass
      fuelForFuel = requiredFuelForFuel fuelForMass
  in fuelForMass + fuelForFuel


requiredFuelForFuel :: Fuel -> Fuel
requiredFuelForFuel fuel =
  sum $ additionalFuelSeq fuel
  where
    additionalFuelSeq =
      unfoldr (fmap asPair . requiredForFuelMass)
      where
        requiredForFuelMass f =
          let req = requiredFuelForMass f
          in if req <= 0 then Nothing else Just req
        asPair a = (a,a)


requiredFuelForMass :: Mass -> Fuel
requiredFuelForMass m =
  m `div` 3 - 2


loadInput :: IO Input
loadInput = fmap read . lines <$> readFile "./src/day1.txt"
