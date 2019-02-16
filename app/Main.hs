module Main where

import Lib (doParse)

main :: IO ()
main = do
  doParse
  putStrLn "done"


