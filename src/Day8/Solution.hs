module Day8.Solution where

import Data.List (minimumBy, foldl1')
import Data.Ord (comparing)


type Input = [Int]
type Pixel = Int

type Image = [Layer]
type Layer = [[Pixel]]


width :: Int
width = 25


height :: Int
height = 6


run :: IO ()
run = do
  putStrLn "DAY 8"

  img <- toImage <$> loadInput

  let res1 = part1 img
  putStrLn $ "\t Part 1: " ++ show res1

  let res2 = part2 img
  putStrLn $ "\t Part 2:\n" ++ res2

  putStrLn "---\n"


----------------------------------------------------------------------
-- Part 1

part1 :: Image -> Int
part1 img =
  let zeroLayer = minimumBy (comparing $ countPixels 0) img
      ones = countPixels 1 zeroLayer
      twos = countPixels 2 zeroLayer
  in ones * twos


countPixels :: Int -> Layer -> Int
countPixels val rows = length [ p | row <- rows, p <- row, p == val ]

----------------------------------------------------------------------
-- Part 2

part2 :: Image -> String
part2 = showImage


showImage :: Image -> String
showImage = unlines . map showLine . render
  where 
  showLine = map showPixel
  showPixel 1 = '#'
  showPixel _ = ' '


render :: Image -> [[Pixel]]
render = foldl1' combineLayers


combineLayers :: Layer -> Layer -> Layer
combineLayers l1 l2 =
  zipWith combRow l1 l2
  where
  combRow = zipWith comp
  comp 2 x = x
  comp y _ = y


----------------------------------------------------------------------
-- Loading / Parsing

loadInput :: IO Input
loadInput = concatMap (map (read . pure)) . lines <$> readFile "./src/Day8/input.txt"


toImage :: Input -> Image
toImage inp = layers
  where
    layers = [ rows l | l <- takeEvery (width*height) inp ]
    rows = takeEvery width


takeEvery :: Int -> [a] -> [[a]]
takeEvery _ [] = []
takeEvery n xs = let (x,xs') = splitAt n xs in x : takeEvery n xs'