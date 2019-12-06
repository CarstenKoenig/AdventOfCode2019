module Main where

import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import qualified Day3.Solution as D3
import qualified Day4.Solution as D4
import qualified Day5.Solution as D5
import qualified Day6.Solution as D6


main :: IO ()
main = do
  D6.run
  D5.run
  D4.run
  D3.run
  D2.run
  D1.run
  putStrLn "done"
