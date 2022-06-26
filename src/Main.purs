module Main where

import Prelude

import Deku.Toplevel (runInBody)
import Effect (Effect)
import Parser.Main as Parser

main :: Effect Unit
main = runInBody Parser.main
