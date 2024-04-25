module Parser.Printer.Tests where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Iso')
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Console (log)
import Idiolect (type (/\/))
import Parser.Printer.Juxt (class Awajuxt, class AwajuxtSepsN, Sentinel, _GenericTensor', awajuxtSepN, awajuxtSepsN, conjuxt2, conjuxtN, disjuxtN, ing, norm, (!!!), (!/), (/!), (/!\), (\!/))
import Parser.Printer.Types (PP)
import Parser.Printer.Types as IO

data TestAST
  = TestUnit
  | TestInt Int
  | TestProduct TestAST TestAST

derive instance Generic TestAST _

main :: Effect Unit
main = log "Hi"

conjuxtN_2 :: PP String -> PP Int -> PP ((Unit /\ String) /\ Int)
conjuxtN_2 (x :: PP String) (y :: PP Int) = conjuxtN x y ing

awajuxtSepN_2 :: PP String -> PP Unit -> PP Int -> PP ((Void /\/ String) /\/ Int)
awajuxtSepN_2 (x :: PP String) (h :: PP Unit) (y :: PP Int) = awajuxtSepN h x y ing

awajuxtSepsN_2 :: PP String -> PP Unit -> PP Int -> PP ((Void /\/ String) /\/ Int)
awajuxtSepsN_2 (x :: PP String) (h :: PP Unit) (y :: PP Int) = awajuxtSepsN x h y ing

awajuxtSepsN_2' ::
  forall p.
    Awajuxt p =>
    AwajuxtSepsN p
      (Void /\/ String)
      (Void /\/ String)
      (p Unit Unit)
      (p Int Int -> Sentinel -> p ((Void /\/ String) /\/ Int) ((Void /\/ String) /\/ Int)) =>
  p String String ->
  p Unit Unit ->
  p Int Int ->
  p ((Void /\/ String) /\/ Int) ((Void /\/ String) /\/ Int)
awajuxtSepsN_2' x h y = awajuxtSepsN x h y ing

awajuxtSepsf ::
  forall p.
    Awajuxt p =>
  p String String ->
  p Unit Unit ->
  p Int Int ->
  p (String /\/ Int) (String /\/ Int)
awajuxtSepsf x h y = x /! h !/ y

_testAST = _GenericTensor' :: Iso' TestAST _

testAST_N :: PP Unit -> PP Int -> PP TestAST
testAST_N u i = IO.namedRec "testAST" \rec ->
  _testAST $ norm $ disjuxtN
    u
    i
    (conjuxt2 rec rec)
  ing

testAST_2 :: PP Unit -> PP Int -> PP TestAST
testAST_2 u i = IO.namedRec "testAST" \rec ->
  _testAST
  !!! u
  \!/ i
  \!/ rec /!\ rec
