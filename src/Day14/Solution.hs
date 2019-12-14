{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Day14.Solution where

import           CommonParsers
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Data.Map.Strict as MS


dayNr :: Int
dayNr = 14


run :: IO ()
run = do
  putStrLn $ "DAY " ++ show dayNr

  inp <- loadInput
  let lu = mkLookup inp

  let res1 = part1 lu
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 lu
  putStrLn $ "\t Part 2: " ++ show res2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Modeling

type Input = [Formula]

-- | wraps the main cost-center (finding formulas) into
--   a map - took around 1s away from the runtime
type Lookup = MS.Map Chemical Formula

data Formula = Formula
  { f_ingredients    :: [Ingredient]
  , f_product        :: Chemical
  , f_product_amount :: Amount
  }
  deriving Show

type Ingredient = (Chemical, Amount)
type Chemical = String
type Amount = Int



-- | calculates the amount of ORE needed for 1 FUEL
part1 :: Lookup -> Amount
part1 lu = minimumNeededOre lu 1


-- | folds the tree using 'neededFor' till ORE
--   is found (the rest in the map will be left-over ingredients)
minimumNeededOre :: Lookup -> Amount -> Amount
minimumNeededOre lu = go . MS.singleton "FUEL"
  where
  go :: MS.Map Chemical Amount -> Amount
  go ns =
    case MS.lookup "ORE" ns of
      Just nr -> nr
      Nothing -> go $ neededFor lu ns


-- | collect the pre-products in the given map that share the
--   same maximum distance to ORE - this way we can stepwise
--   collect ingredients on the same level in the tree
neededFor :: Lookup -> MS.Map Chemical Amount -> MS.Map Chemical Amount
neededFor lu ns =
  MS.unionsWith (+) $ map step ls
  where
  ls = MS.toList ns
  maxDist = maximum $ map (distToOre lu . fst) ls
  step (c,a)
    | distToOre lu c == maxDist = neededForSingle lu a c
    | otherwise                 = MS.singleton c a


-- | collects pre-products and their amounts needed
--   to produce the given chemical with given amount
--   ORE is never produced so we can shortcut this
neededForSingle :: Lookup -> Amount -> Chemical -> MS.Map Chemical Amount
neededForSingle _ needed "ORE" = MS.singleton "ORE" needed
neededForSingle lu needed chem =
  let Formula{..} = producing lu chem
      factor = calcFactor needed f_product_amount
  in MS.fromList [(ing, factor*amt) | (ing, amt) <- f_ingredients ]
  where
  calcFactor n p =
    let (d,r) = n `divMod` p
    in  if r > 0 then d+1 else d


-- | how many formulas must be used to get from ore to the chemical?
--   this should likely be memoized but the problem is small enough
--   to not make this extremely costy
distToOre :: Lookup -> Chemical -> Int
distToOre _ "ORE" = 0
distToOre lu chem =
  1 + maximum [ distToOre lu c | (c,_) <- f_ingredients $ producing lu chem ]


-- | looks in the input for *the* single formula producing the given chemical
--   this assumes, that there is ever only one formula - if not this algorithm
--   here would not work anyways, as an optimizing search would be require
producing :: Lookup -> Chemical -> Formula
producing lu chem = lu MS.! chem

----------------------------------------------------------------------
-- Part 2


maxOreInCargo :: Amount
maxOreInCargo = 1000000000000


-- | conducts a binary search between 0 and maxOreInCarge
--   (assuming you need at least one ORE per FUEL)
--   to get the maximum amount of producable FUEL
part2 :: Lookup -> Amount
part2 lu = go 0 maxOreInCargo
  where
  go lo hi
    -- the will find the first value where there is
    -- NOT enough ORE to produce the fuel - therefore
    -- we need to subtract one from it
    | lo >= hi = lo-1
    | otherwise =
      let
        mid = (hi + lo) `div` 2
        midVal = minimumNeededOre lu mid
      in case () of
           () | midVal < maxOreInCargo -> go (mid+1) hi
              | otherwise -> go lo mid


----------------------------------------------------------------------
-- Loading / Parsing

-- | creates a lookup map to quickly get a formula producing a chemical
--   this *assumes* that only one formula produces a chemical
--   (which is true for the problems presented)
mkLookup :: Input -> Lookup
mkLookup inp = MS.fromList $ [ (f_product,f) | f@Formula{..} <- inp ]


loadInput :: IO Input
loadInput = map parseFormula . lines <$> readFile ("./src/Day" ++ show dayNr ++ "/input.txt")

parseFormula :: String -> Formula
parseFormula = either (error . P.errorBundlePretty) id . P.parse formulaP "Formula"

formulaP :: Parser Formula
formulaP = do
  ings <- ingredientsP
  _ <- PC.string "=>" <* PC.space
  (pr,am) <- ingredientP
  pure $ Formula ings pr am

ingredientsP :: Parser [Ingredient]
ingredientsP = ingredientP `P.sepBy1` PC.string ", "

ingredientP :: Parser Ingredient
ingredientP = do
  !n <- numberP <* PC.space
  !ch <- chemicalP
  pure (ch, n)

chemicalP :: Parser Chemical
chemicalP = nameP <* PC.space
