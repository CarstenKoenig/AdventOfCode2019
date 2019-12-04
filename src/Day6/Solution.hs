module Day6.Solution where

import           CommonParsers
import           ConsoleTests


type Input = String

run :: IO ()
run = do
  putStrLn "DAY 6"

  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Input -> Int
part1 inp = undefined


part2 :: Input -> Int
part2 inp = undefined


loadInput :: IO Input
loadInput = readFile "./src/Day6/input.txt"