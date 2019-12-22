{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day22.Solution where

import           CommonParsers
import           ConsoleTests
import Text.Megaparsec as P
import Text.Megaparsec.Char as PC
import Data.Char (isSpace)
import Data.List (intercalate, foldl')


dayNr :: Int
dayNr = 22

type Input = ShuffleInput

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Input -> Int
part1 inp =
  let shuffle = toShuffle 10007 inp
  in apply shuffle 2019


part2 :: Input -> Int
part2 inp = undefined


data Shuffle = Shuffle
  { s_multiply :: Int
  , s_add      :: Int
  , s_base     :: Int
  }
  deriving (Show)


showShuffle :: Shuffle -> String
showShuffle s@Shuffle{s_base} =
  intercalate " " $ map (show . apply s) [0..s_base-1]


toShuffle :: Int -> ShuffleInput -> Shuffle
toShuffle base =
  foldl' (flip technique_to_shuffle) (idShuffle base) . reverse


technique_to_shuffle :: Technique -> Shuffle -> Shuffle
technique_to_shuffle DealIntoNewStack s@Shuffle{s_base} = add (s_base-1) $ multiply (-1) s
technique_to_shuffle (CutNCards cut) s = add cut s
technique_to_shuffle (DealWithIncrement incr) s@Shuffle{s_base} = multiply (findInverse s_base incr) s


posOf :: Shuffle -> Int -> Int
posOf s@Shuffle{s_base} card = head $ [ i | i <- [0..s_base-1], apply s i == card ]

apply :: Shuffle -> Int -> Int
apply Shuffle{..} x = (x * s_multiply + s_add) `mod` s_base


idShuffle :: Int -> Shuffle
idShuffle = Shuffle 1 0


multiply :: Int -> Shuffle -> Shuffle
multiply n (Shuffle m a b) = Shuffle ((m*n) `mod` b) ((a*n) `mod` b) b


add :: Int -> Shuffle -> Shuffle
add n (Shuffle m a b) = Shuffle m ((a+n) `mod` b) b


findInverse :: Int -> Int -> Int
findInverse base n =
  head [ x | x <- [1..base-1], (n*x) `mod` base == 1 ]


loadInput :: IO Input
loadInput =
  parseInput <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )


parseInput :: String -> ShuffleInput
parseInput = either (error . P.errorBundlePretty) id . P.parse shuffleInputP "Input" . rstrip
  where rstrip = reverse . dropWhile isSpace . reverse


type ShuffleInput = [Technique]

shuffleInputP :: Parser ShuffleInput
shuffleInputP = techniqueP `P.sepBy` (PC.char '\n')


data Technique
  = DealIntoNewStack
  | CutNCards Int
  | DealWithIncrement Int
  deriving (Show)

techniqueP :: Parser Technique
techniqueP = P.choice
  [ dealNewStackP
  , cutP
  , dealWithIncP
  ]
  where
  dealNewStackP = DealIntoNewStack <$ PC.string "deal into new stack"
  cutP = CutNCards <$> (PC.string "cut " *> numberP)
  dealWithIncP = DealWithIncrement <$> (PC.string "deal with increment " *> numberP)
