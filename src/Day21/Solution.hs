module Day21.Solution where

import           IntCodePure


dayNr :: Int
dayNr = 21

type Input = Program Int

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  prg <- loadInput

  let res1 = part1 prg
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 prg
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Input -> Int
part1 = runScript script1


part2 :: Input -> Int
part2 = runScript script2


type Script = [String]


-- | should jump if it's save to land (D)
--   and if either there is a hole directly in front (~A)
--          or there is a hole in front where to jump (~C)
--   so the formula would be (~A || ~C) && D
script1 :: Script
script1 =
  [ "NOT A J"  -- ~A
  , "NOT C T"  -- ~C ~> T
  , "OR T J"   -- (~C || ~A)
  , "AND D J"  -- D && (~C || ~A)
  , "WALK"
  ]

-- | should jump if it's save to land (D)
--   AND if there is a hole one or two steps away (~A || ~B)
--       or there is a hole 3 steps away but not 8 steps away (~C && H)
--       to see this I just ran the program and it fell in this situation ;)
script2 :: Script
script2 =
  [ "NOT C J"  -- ~C -> J
  , "AND H J"  -- H && ~C
  , "NOT B T"  -- ~B -> T
  , "OR T J"   -- ~B || (H && ~C)
  , "NOT A T"  -- ~A -> T
  , "OR T J"   -- ~A || (~B || (H && ~C))
  , "AND D J"  -- D && (~A || (~B || (H && ~C)))
  , "RUN"
  ]


-- | runs a script to the IntCode computer
--   returning the found hull damage
--   will NOT check for successful runs (return 0 otherwise)
--   so play with 'runDialog' first if you want to see problems
runScript :: Script -> Input -> Int
runScript instrs = go 0 ascii . initComputer
  where
  ascii = map fromEnum $ unlines instrs
  go dmg buffer cont =
    case runComputer cont of
      Halted -> dmg
      Error err -> error $ "computer errror: " ++ err
      RequestInput i2c ->
        case buffer of
          [] -> error "got no more input"
          c:cs -> go dmg cs (i2c c)
      NewOutput out cont' ->
        if out > 255
          then go out buffer cont'
        else
          -- ignore ASCII output
          go dmg buffer cont'


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
