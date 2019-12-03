module Day4.Solution where

import           CommonParsers
import           ConsoleTests


type Input = String

run :: IO ()
run = do
  putStrLn "DAY 4"

  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Input -> Int
part1 inp = 0


part2 :: Input -> Int
part2 inp = 0


loadInput :: IO Input
loadInput = 
  pure "not ready"
  -- readFile "./src/Day4/input.txt"