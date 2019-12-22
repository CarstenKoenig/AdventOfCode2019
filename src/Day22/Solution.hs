{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module Day22.Solution where

import CommonParsers
import Data.Char (isSpace)
import Data.List (foldl')
import Text.Megaparsec as P
import Text.Megaparsec.Char as PC


dayNr :: Int
dayNr = 22

type Input = ShuffleInput

run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  inp <- loadInput

  let res1 = part1 inp
  putStrLn $ "\t Part 1: " ++ show res1

  res2 <- part2 inp
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


littleDeck :: Integer
littleDeck = 10007

part1 :: Input -> Integer
part1 inp =
  let shuffle = toShuffle littleDeck inp
  in apply shuffle 2019


hugeDeck :: Integer
hugeDeck = 119315717514047


times :: Integer
times = 101741582076661


part2 :: Input -> IO Integer
part2 inp = do
  let !shuffle = toShuffle hugeDeck inp
      !shuffle' = nTimes times shuffle
  pure $ applyInv shuffle' 2020


nTimes :: Integer -> Shuffle -> Shuffle
nTimes n Shuffle{..} =
  let a_n = binPow (fromIntegral s_base) (fromIntegral s_multiply) (fromIntegral n)
      mult = a_n `mod` fromIntegral s_base
      add = (s_add * (fromIntegral a_n - 1) * modInv s_base (s_multiply - 1)) `mod` s_base
  in Shuffle (fromIntegral mult) add s_base


data Shuffle = Shuffle
  { s_multiply :: Integer
  , s_add      :: Integer
  , s_base     :: Integer
  }
  deriving (Show)


toShuffle :: Integer -> ShuffleInput -> Shuffle
toShuffle base =
  foldl' (flip technique_to_shuffle) (idShuffle base)


technique_to_shuffle :: Technique -> Shuffle -> Shuffle
technique_to_shuffle DealIntoNewStack Shuffle{..} = Shuffle ((negate s_multiply) `mod` s_base) ((negate s_add - 1) `mod` s_base) s_base
technique_to_shuffle (CutNCards cut) Shuffle{..} = Shuffle s_multiply ((s_add - cut) `mod` s_base) s_base
technique_to_shuffle (DealWithIncrement incr) Shuffle{..} = Shuffle ((s_multiply * incr) `mod` s_base) ((s_add * incr) `mod` s_base) s_base


apply :: Shuffle -> Integer -> Integer
apply Shuffle{..} x = (x * s_multiply + s_add) `mod` s_base

applyInv :: Shuffle -> Integer -> Integer
applyInv Shuffle{..} x =
  ((x - s_add) * modInv s_base s_multiply) `mod` s_base


idShuffle :: Integer -> Shuffle
idShuffle = Shuffle 1 0


modInv :: Integral n => n -> n -> n
modInv base n =
  let (g,x,_) = egcd(n, base)
  in if g /= 1
     then error "no inverse"
     else x `mod` base
  where
    egcd (a, b) =
      if a == 0
      then (b, 0, 1)
      else
        let (g,y,x) = egcd(b `mod` a, a)
        in (g, x - (b `div` a) * y, y)


binPow :: Integer -> Integer -> Integer -> Integer
binPow base = go
  where
  go a 1 = a
  go a b =
    let (b',r) = b `divMod` 2
    in if r == 0
       then ((go a b')^(2::Int)) `mod` base
       else (a * go a (b-1)) `mod` base


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
  | CutNCards Integer
  | DealWithIncrement Integer
  deriving (Show)

techniqueP :: Parser Technique
techniqueP = P.choice
  [ dealNewStackP
  , cutP
  , dealWithIncP
  ]
  where
  dealNewStackP = DealIntoNewStack <$ PC.string "deal into new stack"
  cutP = CutNCards <$> (PC.string "cut " *> fmap fromIntegral numberP)
  dealWithIncP = DealWithIncrement <$> (PC.string "deal with increment " *>  fmap fromIntegral numberP)
