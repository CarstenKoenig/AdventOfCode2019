module Day2.Solution where

import           IntCodeV1

run :: IO ()
run = do
  putStrLn "DAY 2"

  prg <- loadProgram

  let res1 = part1 prg
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


-- | just run the Program with _noun_ 12 and _verb_ 2
part1 :: Program -> Int
part1 prg = 
  case runPair prg 12 2 of
    Left err -> error err
    Right res -> res


-- | used 'runPair' with a few inputs an noticed
--   that incrementing the _noun_ part increases the result by 360000
--   incrementing the _verb_ will just increase the result by 1
--   initial Value for _noun_=0, _verb_=0 was 250635
part2 :: Int
part2 =
  let partNoun = part2TargetNumber `div` 1000  
      solNoun = (partNoun - 250) `div` 360
      partVerb = part2TargetNumber `mod` 1000
      solVerb = partVerb - 635
  in solNoun * 100 + solVerb


-- | brute forcing part2 by enumerating and checking all the combinations
--   of noun/verb each in the range 0 to 99
part2BruteForce :: Program -> Int
part2BruteForce prg = head
  [ 100 * noun + verb | noun <- [0..99]
                      , verb <- [0..99]
                      , runPair prg noun verb == Right part2TargetNumber 
  ]


part2TargetNumber :: Int
part2TargetNumber = 19690720


-- | used via REPL to get some clues for 'part2'
--   the first parameter is called _noun_ in the problem and is set to mem-adr 1
--   the second parameter is called _verb in the problem and is set to mem-adr 2
runPair :: Program -> Int -> Int -> Either String Int
runPair prg noun verb = eval prg $ do
  setMemory 1 noun
  setMemory 2 verb
  runComputer
  getMemory 0


loadProgram :: IO Program
loadProgram = parseProgram <$> readFile "./src/Day2/input.txt"


testProgram :: Program
testProgram = parseProgram "1,9,10,3,2,3,11,0,99,30,40,50"