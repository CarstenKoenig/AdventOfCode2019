{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Day22.Solution where

import CommonParsers
import Data.Char (isSpace)
import Data.List (foldl')
import Text.Megaparsec as P
import Text.Megaparsec.Char as PC


dayNr :: Int
dayNr = 22


run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  res1 <- part1 <$> loadInput @Int
  putStrLn $ "\t Part 1: " ++ show res1

  res2 <- part2 <$> loadInput @Integer
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


part1 :: Integral n => ShuffleInput n -> n
part1 inp =
  let shuffle = toShuffle littleDeck inp
  in apply shuffle 2019
  where
    littleDeck = 10007


part2 :: Integral n => ShuffleInput n -> n
part2 inp =
  let !shuffle = toShuffle hugeDeck inp
      !shuffle' = nTimes times shuffle
  in applyInv shuffle' 2020
  where
    hugeDeck = 119315717514047
    times = 101741582076661


-- | calculates a shuffle doing n-times the original shuffle
--   this is based on a bit of algebra:
--   if shuffle x = mult * x + add
--   then shuffle^n x = mult^n * x + (mult^n-1)/(mult-1) * b
--   the last bit can be seen as (a^(n-1) + a^(n-2) + ... + 1) * (a-1) = a^n - 1
--   of course everything mod base here
nTimes :: Integral n => n -> Shuffle n -> Shuffle n
nTimes n Shuffle{..} =
  let a_n = modPow (fromIntegral s_base) (fromIntegral s_multiply) (fromIntegral n)
      mult = a_n `mod` fromIntegral s_base
      add = (s_add * (fromIntegral a_n - 1) * modInv s_base (s_multiply - 1)) `mod` s_base
  in Shuffle mult add s_base


data Shuffle n = Shuffle
  { s_multiply :: n
  , s_add      :: n
  , s_base     :: n
  }
  deriving (Show)


-- | applies a shuffle to a number
apply :: Integral n => Shuffle n -> n -> n
apply Shuffle{..} x = (x * s_multiply + s_add) `mod` s_base


-- | applies a inverse shuffle to a number
applyInv :: Integral n => Shuffle n -> n -> n
applyInv Shuffle{..} x =
  ((x - s_add) * modInv s_base s_multiply) `mod` s_base


-- | identity shuffle
idShuffle :: Integral n => n -> Shuffle n
idShuffle = Shuffle 1 0


toShuffle :: Integral n => n -> ShuffleInput n -> Shuffle n
toShuffle base =
  foldl' (flip technique_to_shuffle) (idShuffle base)


technique_to_shuffle :: Integral n => Technique n -> Shuffle n -> Shuffle n
technique_to_shuffle DealIntoNewStack Shuffle{..} = Shuffle ((negate s_multiply) `mod` s_base) ((negate s_add - 1) `mod` s_base) s_base
technique_to_shuffle (CutNCards cut) Shuffle{..} = Shuffle s_multiply ((s_add - cut) `mod` s_base) s_base
technique_to_shuffle (DealWithIncrement incr) Shuffle{..} = Shuffle ((s_multiply * incr) `mod` s_base) ((s_add * incr) `mod` s_base) s_base


----------------------------------------------------------------------
-- helpers

-- | @modInv base n@ calculates a @n'@ with @(n * n') `mod` base == 1@
modInv :: Integral n => n -> n -> n
modInv base n =
  let (g,x,_) = ext_eucl(n, base)
  in if g /= 1
     then error "no inverse"
     else x `mod` base
  where
    -- | extended euclidean algorithm
    ext_eucl (a, b) =
      if a == 0
      then (b, 0, 1)
      else
        let (g,y,x) = ext_eucl(b `mod` a, a)
        in (g, x - (b `div` a) * y, y)



-- | @modPow base a b@  calculates @a^b `mod` base
modPow :: Integral n => n -> n -> n -> n
modPow base = go
  where
  go a 1 = a
  go a b =
    let (b',r) = b `divMod` 2
    in if r == 0
       then ((go a b')^(2::Int)) `mod` base
       else (a * go a (b-1)) `mod` base


----------------------------------------------------------------------
-- loading / parsing


loadInput :: (Read n, Integral n) => IO (ShuffleInput n)
loadInput =
  parseInput <$> readFile ( "./src/Day" ++ show dayNr ++ "/input.txt" )


parseInput :: (Read n, Integral n) => String -> ShuffleInput n
parseInput = either (error . P.errorBundlePretty) id . P.parse shuffleInputP "Input" . rstrip
  where rstrip = reverse . dropWhile isSpace . reverse


type ShuffleInput n = [Technique n]


shuffleInputP :: (Read n, Integral n) => Parser (ShuffleInput n)
shuffleInputP = techniqueP `P.sepBy` (PC.char '\n')


data Technique n
  = DealIntoNewStack
  | CutNCards n
  | DealWithIncrement n
  deriving (Show)


techniqueP :: (Read n, Integral n) => Parser (Technique n)
techniqueP = P.choice
  [ dealNewStackP
  , cutP
  , dealWithIncP
  ]
  where
  dealNewStackP = DealIntoNewStack <$ PC.string "deal into new stack"
  cutP = CutNCards <$> (PC.string "cut " *> numberP)
  dealWithIncP = DealWithIncrement <$> (PC.string "deal with increment " *>  numberP)
