module Main where

import qualified Day1.Solution as D1
import qualified Day2.Solution as D2

main :: IO ()
main = do
  D1.run
  D2.run
  putStrLn "done"
