module Day5.Solution (run) where

import           IntCode

run :: IO ()
run = do
  putStrLn "DAY 5"

  prg <- loadProgram

  let res1 = part1 prg
  putStrLn $ "\t Part 1: " ++ either ("ERROR: " ++) show res1

  let res2 = part2 prg
  putStrLn $ "\t Part 2: " ++ either ("ERROR: " ++ ) show res2

  putStrLn "---\n"


part1 :: Program -> Either String Int
part1 prg = last <$> runPrg 1 prg


part2 :: Program -> Either String Int
part2 prg = last <$> runPrg 5 prg


runPrg :: Int -> Program -> Either String [Int]
runPrg inp mem =
  eval mem [inp] $ do
    runComputer
    getOutputs


loadProgram :: IO Program
loadProgram = parseProgram <$> readFile "./src/Day5/input.txt"