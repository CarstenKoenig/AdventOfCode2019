module Day5.Solution where

import           CommonParsers
import           IntCode
import           ConsoleTests


type Input = String

run :: IO ()
run = do
  putStrLn "DAY 5"

  prg <- loadProgram

  let res1 = part1 prg
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 prg
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Program -> Either String [Int]
part1 mem =
  eval mem [1] $ do
    runComputer
    getOutputs


part2 :: Program -> Int
part2 inp = 0


loadProgram :: IO Program
loadProgram = parseProgram <$> readFile "./src/Day5/input.txt"