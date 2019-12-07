module Day7.Solution where

import           CommonParsers
import           ConsoleTests
import           IntCode
import Data.List (permutations)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM)


type Input = Program

run :: IO ()
run = do
  putStrLn "DAY 7"

  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Input -> Int
part1 inp =
  maximum $ mapMaybe (either (const Nothing) Just . runStages inp) (permutations [0..4])


part2 :: Input -> Int
part2 inp = 0


runStages :: Program -> [Int] -> Either String Int
runStages prg sets =
  foldM (amplifierStage prg) 0 sets


amplifierStage :: Program -> Int -> Int -> Either String Int
amplifierStage prg inp set = head <$> runPrg prg set inp


runPrg :: Program -> Int -> Int -> Either String [Int]
runPrg prg set inp =
  eval prg [set, inp] $ do
    runComputer
    getOutputs

loadInput :: IO Input
loadInput = parseProgram <$> readFile "./src/Day7/input.txt"


testProgram1 :: Program
testProgram1 = parseProgram "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"