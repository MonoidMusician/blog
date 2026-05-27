module Main where

import qualified ParseMachine
import qualified WASMFP

main :: IO ()
main = do
  -- ParseMachine.main
  WASMFP.main
