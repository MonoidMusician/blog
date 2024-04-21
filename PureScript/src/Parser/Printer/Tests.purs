module Parser.Printer.Tests where

import Prelude

import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Console (log)
import Idiolect (type (/\/))
import Parser.Printer.Juxt (awajuxtSepN, awajuxtSepsN, conjuxtN, ing)
import Parser.Printer.Types (PP)

main :: Effect Unit
main = log "Hi"

conjuxtN_2 :: PP String -> PP Int -> PP ((Unit /\ String) /\ Int)
conjuxtN_2 (x :: PP String) (y :: PP Int) = conjuxtN x y ing

awajuxtSepN_2 :: PP String -> PP Unit -> PP Int -> PP ((Void /\/ String) /\/ Int)
awajuxtSepN_2 (x :: PP String) (h :: PP Unit) (y :: PP Int) = awajuxtSepN h x y ing

awajuxtSepsN_2 :: PP String -> PP Unit -> PP Int -> PP ((Void /\/ String) /\/ Int)
awajuxtSepsN_2 (x :: PP String) (h :: PP Unit) (y :: PP Int) = awajuxtSepsN x h y ing
