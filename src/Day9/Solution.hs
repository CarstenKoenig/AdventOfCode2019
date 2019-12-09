module Day9.Solution where

import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TQueue as TQ
import           Data.Int (Int64)
import           IntCodeStm


type Input = Program Int64

run :: IO ()
run = do
  putStrLn "DAY 9"

  prg <- loadInput

  res1 <- runBoost prg 1
  putStrLn $ "\t Part 1: " ++ show res1

  res2 <- runBoost prg 2
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


runBoost :: Input -> Int64 -> IO (Either String [Int64])
runBoost prg inp = do
  output <- TQ.newTQueueIO
  let c = createComputer "BOOST" prg (pure inp) (TQ.writeTQueue output)
  res <- runComputer c
  case res of
    Left err -> pure $ Left err
    Right () -> Right <$> atomically (TQ.flushTQueue output)


loadInput :: IO Input
loadInput = parseProgram <$> readFile "./src/Day9/input.txt"