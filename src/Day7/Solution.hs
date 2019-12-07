module Day7.Solution where

import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Concurrent.STM.TVar as TV
import           Control.Monad (foldM, void)
import qualified Control.Monad.STM as STM
import           Control.Monad.STM (STM)
import           Data.List (permutations)
import           Data.Maybe (mapMaybe)
import           IntCodeStm


type Input = Program

run :: IO ()
run = do
  putStrLn "DAY 7"

  inp <- loadInput

  res1 <- part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  res2 <- part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Program -> IO Int
part1 prg = do
  results <- mapM (runStages prg) (permutations [0..4])
  pure $ maximum $ mapMaybe (either (const Nothing) Just) results


part2 :: Program -> IO Int
part2 prg = do
  results <- mapM (runStages2 prg) (permutations [5..9])
  pure $ maximum $ mapMaybe (either (const Nothing) Just) results


runStages2 :: Program -> [Int] -> IO (Either String Int)
runStages2 prg [a,b,c,d,e] = do
  output <- TV.newTVarIO 0 :: IO (TV.TVar Int)
  input <- TC.newTChanIO
  STM.atomically $ do 
    TC.writeTChan input a
    TC.writeTChan input 0
  a2b <- TC.newTChanIO
  STM.atomically $ do 
    TC.writeTChan a2b b
  b2c <- TC.newTChanIO
  STM.atomically $ TC.writeTChan b2c c
  c2d <- TC.newTChanIO
  STM.atomically $ TC.writeTChan c2d d
  d2e <- TC.newTChanIO
  STM.atomically $ TC.writeTChan d2e e
  let ampA = createComputer "A" prg (TC.readTChan input) (TC.writeTChan a2b)
  let ampB = createComputer "B" prg (TC.readTChan a2b) (TC.writeTChan b2c)
  let ampC = createComputer "C" prg (TC.readTChan b2c) (TC.writeTChan c2d)
  let ampD = createComputer "D" prg (TC.readTChan c2d) (TC.writeTChan d2e)
  let ampE = createComputer "E" prg (TC.readTChan d2e) (\x -> TC.writeTChan input x >> TV.modifyTVar' output (const x))
  _ <- forkIO $ void $ runComputer ampA
  _ <- forkIO $ void $ runComputer ampB
  _ <- forkIO $ void $ runComputer ampC
  _ <- forkIO $ void $ runComputer ampD
  res <- runComputer ampE
  case res of
    Left err -> pure $ Left err
    Right () -> Right <$> TV.readTVarIO output


runStages :: Program -> [Int] -> IO (Either String Int)
runStages prg [a,b,c,d,e] = do
  output <- TV.newTVarIO 0
  input <- TC.newTChanIO
  STM.atomically $ do 
    TC.writeTChan input a
    TC.writeTChan input 0
  a2b <- TC.newTChanIO
  STM.atomically $ TC.writeTChan a2b b
  b2c <- TC.newTChanIO
  STM.atomically $ TC.writeTChan b2c c
  c2d <- TC.newTChanIO
  STM.atomically $ TC.writeTChan c2d d
  d2e <- TC.newTChanIO
  STM.atomically $ TC.writeTChan d2e e
  let ampA = createComputer "A" prg (TC.readTChan input) (TC.writeTChan a2b)
  let ampB = createComputer "B" prg (TC.readTChan a2b) (TC.writeTChan b2c)
  let ampC = createComputer "C" prg (TC.readTChan b2c) (TC.writeTChan c2d)
  let ampD = createComputer "D" prg (TC.readTChan c2d) (TC.writeTChan d2e)
  let ampE = createComputer "E" prg (TC.readTChan d2e) (\x -> TV.modifyTVar' output (const x))
  _ <- forkIO $ void $ runComputer ampA
  _ <- forkIO $ void $ runComputer ampB
  _ <- forkIO $ void $ runComputer ampC
  _ <- forkIO $ void $ runComputer ampD
  res <- runComputer ampE
  case res of
    Left err -> pure $ Left err
    Right () -> Right <$> (TV.readTVarIO output)


loadInput :: IO Input
loadInput = parseProgram <$> readFile "./src/Day7/input.txt"


testProgram1 :: Program
testProgram1 = parseProgram "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"

testProgram2 :: Program
testProgram2 = parseProgram "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

testProgram3 :: Program
testProgram3 = parseProgram "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"