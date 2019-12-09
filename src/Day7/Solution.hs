module Day7.Solution where

import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Concurrent.STM.TVar as TV
import           Control.Monad (void, forM_)
import qualified Control.Monad.STM as STM
import           Data.List (permutations)
import           Data.Maybe (mapMaybe)
import           IntCodeStm


type Input = Program Int

run :: IO ()
run = do
  putStrLn "DAY 7"

  inp <- loadInput

  res1 <- part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  res2 <- part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Input -> IO Int
part1 prg = do
  results <- mapM (runStages prg False) (permutations [0..4])
  pure $ maximum $ mapMaybe (either (const Nothing) Just) results


part2 :: Input -> IO Int
part2 prg = do
  results <- mapM (runStages prg True) (permutations [5..9])
  pure $ maximum $ mapMaybe (either (const Nothing) Just) results


-- | uses the STM version of the IntCode interpreter
--   and runs the 5 instances in parallel/foked threads
--   communication between the instances is handeled by
--   STM channels and the majority of this function just
--   sets up the right channels and initial input for them
--   the output is handeled by a single TVar the just get's
--   updated on every output of E
--   if the second parameter is True E's output is send
--   to A again (for part 2)
runStages :: Input -> Bool -> [Int] -> IO (Either String Int)
runStages prg connectE2A [a,b,c,d,e] = do
  output <- TV.newTVarIO 0 :: IO (TV.TVar Int)
  input <- setupChannel [a,0]
  a2b <- setupChannel [b]
  b2c <- setupChannel [c]
  c2d <- setupChannel [d]
  d2e <- setupChannel [e]
  let eWriter v = (if connectE2A then TC.writeTChan input v else pure ()) >> TV.modifyTVar' output (const v)
  let ampA = createComputer "A" prg (TC.readTChan input) (TC.writeTChan a2b)
  let ampB = createComputer "B" prg (TC.readTChan a2b) (TC.writeTChan b2c)
  let ampC = createComputer "C" prg (TC.readTChan b2c) (TC.writeTChan c2d)
  let ampD = createComputer "D" prg (TC.readTChan c2d) (TC.writeTChan d2e)
  let ampE = createComputer "E" prg (TC.readTChan d2e) eWriter
  _ <- forkIO $ void $ runComputer ampA
  _ <- forkIO $ void $ runComputer ampB
  _ <- forkIO $ void $ runComputer ampC
  _ <- forkIO $ void $ runComputer ampD
  res <- runComputer ampE
  case res of
    Left err -> pure $ Left err
    Right () -> Right <$> TV.readTVarIO output
  where
  setupChannel initialInputs = do
    chan <- TC.newTChanIO
    forM_ initialInputs $ STM.atomically . TC.writeTChan chan
    pure chan
runStages _ _ _ = error "wrong parameter count"


loadInput :: IO Input
loadInput = parseProgram <$> readFile "./src/Day7/input.txt"