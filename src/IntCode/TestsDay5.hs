
module IntCode.TestsDay5 (run) where

import           ConsoleTests
import           IntCode


run :: IO ()
run = do
  putStrLn "DAY 5 - Tests"
  tests


tests :: IO ()
tests = do
  assert "inp 8 == 8?" "3,9,8,9,10,9,4,9,99,-1,8" [8] 1
  assert "inp 9 == 8?" "3,9,8,9,10,9,4,9,99,-1,8" [9] 0

  assert "inp 8 < 8?" "3,9,7,9,10,9,4,9,99,-1,8" [8] 0
  assert "inp 3 < 8?" "3,9,7,9,10,9,4,9,99,-1,8" [3] 1

  assert "IM-MODE: inp 8 == 8?" "3,3,1108,-1,8,3,4,3,99" [8] 1
  assert "IM-MODE: inp 9 == 8?" "3,3,1108,-1,8,3,4,3,99" [9] 0

  assert "IM-MODE: inp 8 < 8?" "3,3,1107,-1,8,3,4,3,99" [8] 0
  assert "IM-MODE: inp 3 < 8?" "3,3,1107,-1,8,3,4,3,99" [3] 1

  assert "inp 0 - JUMP is zero?" "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [0] 0
  assert "inp 9 - JUMP is zero?" "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [9] 1

  assert "IM_MODE: inp 0 - JUMP is zero?" "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [0] 0
  assert "IM_MODE: inp 9 - JUMP is zero?" "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [9] 1

  assert "TestPrg - inp -3 == 999?" testPrg [-3] 999
  assert "TestPrg - inp 3 == 999?" testPrg [3] 999
  assert "TestPrg - inp 8 == 1000?" testPrg [8] 1000
  assert "TestPrg - inp 42 == 1001?" testPrg [42] 1001

  where
    testPrg = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"


assert :: String -> String -> [Int] -> Int -> IO ()
assert label testPrg inps expected = do
  let prg = parseProgram testPrg
      value = last <$> run prg
  value `shouldEq` Right expected
  where 
    shouldEq = shouldEqual label
    run prg =
      eval prg inps $ do
        runComputer
        getOutputs