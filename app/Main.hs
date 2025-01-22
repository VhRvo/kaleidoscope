module Main where

import Repl qualified

main :: IO ()
main = do
  Repl.repl
  pure ()
