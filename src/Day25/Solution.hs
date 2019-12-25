module Day25.Solution where

import           IntCodePure


dayNr :: Int
dayNr = 25

type Input = Program Int


-- | I solved today by actually playing this
--   old school text adventure
--   sure we could write an algorith that visits every place
--   on the ship, takes all items (not killing the robot)
--   and then play the heavy/light game on the end
--   but I'm sure this would take way longer to write
--   than visit the 18 rooms I got for my puzzle
run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  prg <- loadInput
  _ <- runDialog prg

  let res1 = part1 prg
  putStrLn $ "\t ok you tried - here is the code: " ++ show res1

  putStrLn "---\n"


part1 :: Input -> Int
part1 _ = 84410376


-- | Variant to directly plug stdin/stdout to
--   the IntCode computer - used to find the solutions
--   fun to play with!
runDialog :: Input -> IO Int
runDialog inp = go 0 "" $ initComputer inp
  where
  go dmg buffer cont =
    case runComputer cont of
      Halted -> pure dmg
      Error err -> error $ "computer errror: " ++ err
      RequestInput i2c ->
        case buffer of
          [] -> do
            buffer' <- getLine
            go dmg (buffer' ++ "\n") cont
          c:cs -> go dmg cs (i2c $ fromEnum c)
      NewOutput out cont' ->
        if out > 255
          then go out buffer cont'
        else do
          putStr (pure $ toEnum out)
          go dmg buffer cont'


-- | loading / parsing
loadInput :: IO Input
loadInput = parseProgram <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )
