module Day4.Solution (run) where

import Control.Monad (guard)
import Data.List (group, elem)

type Number = Int

myInput :: (Number, Number)
myInput = (158126, 624574)


run :: IO ()
run = do
  putStrLn "DAY 4"
  putStrLn $ "\t Part 1: " ++ show part1
  putStrLn $ "\t Part 2: " ++ show part2
  putStrLn "---\n"


part1 :: Int
part1 = length $ enumSolutions hasConsecutiveDouble


part2 :: Int
part2 = length $ enumSolutions hasGroupOfLength2


type Digit = Int

-- | enumerate non-decreasing-digit numbers in range 
--   and filters by the given predicate
--   note: the digits of the generated numbers are non-decreasing 
--   by construction here
--   uses the first digit of the lower and upper bound to constraint the
--   search space a bit more (~half in my case)
enumSolutions :: ([Digit] -> Bool) -> [Number]
enumSolutions additionalGuard = do
  d1 <- [l1..u1]
  d2 <- [d1..9]
  d3 <- [d2..9]
  d4 <- [d3..9]
  d5 <- [d4..9]
  d6 <- [d5..9]
  let digits = [d1,d2,d3,d4,d5,d6]
  let number = digitsToNumber digits
  guard (lowerBound <= number && number <= upperBound && additionalGuard digits)
  pure number
  where
    (lowerBound, upperBound) = myInput
    (l1:_) = digits lowerBound
    (u1:_) = digits upperBound


-- | part 1 asks to check if the number contains two
--   consecutive digits that are the same
hasConsecutiveDouble :: Eq a => [a] -> Bool
hasConsecutiveDouble (a:rest@(b:_)) = 
  a==b || hasConsecutiveDouble rest
hasConsecutiveDouble _ = 
  False


-- | part 2 asks to count the length of groups
--   of digits instead of just checking with 'hasConsecutiveDouble'
hasGroupOfLength2 :: Eq a => [a] -> Bool
hasGroupOfLength2 xs = 
  2 `elem` map length (group xs)


-- | neat little trick to get the digits of a number
--   use show to get the number into a list of characters
--   pack each of those into it's own list and read this string into the digit
digits :: Number -> [Digit]
digits = map (read . pure) . show

digitsToNumber :: [Digit] -> Number
digitsToNumber = read . concatMap show