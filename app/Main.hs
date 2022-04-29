module Main where

import Cardano.Faucet
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  runApp args
