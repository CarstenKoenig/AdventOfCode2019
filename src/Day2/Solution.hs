module Day2.Solution
  ( run
  ) where

import Data.List (unfoldr)

type Input = String

run :: IO ()
run = do
  putStrLn "DAY 2"

  input <- loadInput

  let part1 = 0
  putStrLn $ "\t Part 1: " ++ show part1

  let part2 = 0
  putStrLn $ "\t Part 2: " ++ show part2

  putStrLn "---\n"


loadInput :: IO Input
loadInput = readFile "./src/Day2/input.txt"
