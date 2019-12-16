{-# LANGUAGE BangPatterns #-}
module Day16.Solution where

import           CommonParsers
import           ConsoleTests
import qualified Data.Set as S
import Data.List (foldl', scanl')


dayNr :: Int
dayNr = 16

type Input = [Digit]
type Digit = Int

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ res1

  let res2 = part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 1

part1 :: Input -> String
part1 inp =
  concatMap show $ take 8 $ phases 100 inp

phases :: Int -> Input -> Input
phases = nTimes phase

phase :: Input -> Input
phase inp = 
  [ computeDigit n inp | n <- [0..length inp - 1] ]

computeDigit :: Int -> Input -> Digit
computeDigit n input = 
  lastDigit $ foldl' (+) 0 $ zipWith (*) input (digitPattern n)


digitPattern :: Int -> [Int]
digitPattern n = tail $ cycle phaseMask
  where
  phaseMask =
    replicate (n+1) 0 ++
    replicate (n+1) 1 ++
    replicate (n+1) 0 ++
    replicate (n+1) (-1)


----------------------------------------------------------------------
-- Helpers

-- | computes the last digit of a number
lastDigit :: Integral a => a -> a
lastDigit = abs . (`rem` 10)


-- | iterates 'n' times the function 'f' starting on input 'x'
nTimes :: (a -> a) -> Int -> a -> a
nTimes _ 0 x = x
nTimes f n x = nTimes f (n-1) (f x)


----------------------------------------------------------------------
-- Part 2
--
-- I'm not 100% happy with this, as the day takes around 4s
-- but right now the efford to optimize seems not worth a few seconds
-- as I'm not really interested in this problem


-- | seems the offset is so big, that it'll land
--   where the phase-Mask is only 1's - so it's
--   only about summing up partial sums
part2 :: Input -> Int
part2 inp = read . concatMap show $ take 8 $ reverse $
  nTimes calcNextSuffix 100 suffix
  where
    calcNextSuffix !sL =
      map fst $ tail $ scanl' calcDigit (0, 0) sL
      where calcDigit (_, !acc) !d = let acc' = acc + d in (lastDigit acc', acc')
    suffix = reverse $ drop offs bigList
    offs = offset inp
    bigList = concat $ replicate 10000 inp


offset :: Input -> Int
offset = read . concatMap show . take 7


----------------------------------------------------------------------
-- Loading / Parsing

loadInput :: IO Input
loadInput = parseString <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )


parseString :: String -> Input
parseString = map (read . pure)