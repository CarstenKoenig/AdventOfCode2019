module Main where

import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import qualified Day3.Solution as D3
import qualified Day4.Solution as D4
import qualified Day5.Solution as D5
import qualified Day6.Solution as D6
import qualified Day7.Solution as D7
import qualified Day8.Solution as D8
import qualified Day9.Solution as D9
import qualified Day10.Solution as D10
import qualified Day11.Solution as D11
import qualified Day12.Solution as D12
import qualified Day13.Solution as D13
import qualified Day14.Solution as D14
import qualified Day15.Solution as D15


main :: IO ()
main = D15.run

main' :: IO ()
main' = do
  D14.run
  D13.run
  D12.run
  D11.run
  D10.run
  D9.run
  D8.run
  D7.run
  D6.run
  D5.run
  D4.run
  D3.run
  D2.run
  D1.run
  putStrLn "done"
