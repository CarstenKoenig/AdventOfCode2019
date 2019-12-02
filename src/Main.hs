module Main where

import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import qualified Day3.Solution as D3

main :: IO ()
main = do
  D1.run
  D2.run
  D3.run
  putStrLn "done"
