module Parser.Printer.Tests where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Iso')
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Console (log)
import Idiolect (type (/\/))
import Parser.Printer.Juxt (class Subjuxt, class SubjuxtSepsN, Sentinel, _GenericTensor', subjuxtSepN, subjuxtSepsN, conjuxt2, conjuxtN, disjuxtN, ing, norm, (!!!), (!/), (/!), (/!\), (\!/))
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

subjuxtSepN_2 :: PP String -> PP Unit -> PP Int -> PP ((Void /\/ String) /\/ Int)
subjuxtSepN_2 (x :: PP String) (h :: PP Unit) (y :: PP Int) = subjuxtSepN h x y ing

subjuxtSepsN_2 :: PP String -> PP Unit -> PP Int -> PP ((Void /\/ String) /\/ Int)
subjuxtSepsN_2 (x :: PP String) (h :: PP Unit) (y :: PP Int) = subjuxtSepsN x h y ing

subjuxtSepsN_2' ::
  forall p.
    Subjuxt p =>
    SubjuxtSepsN p
      (Void /\/ String)
      (Void /\/ String)
      (p Unit Unit)
      (p Int Int -> Sentinel -> p ((Void /\/ String) /\/ Int) ((Void /\/ String) /\/ Int)) =>
  p String String ->
  p Unit Unit ->
  p Int Int ->
  p ((Void /\/ String) /\/ Int) ((Void /\/ String) /\/ Int)
subjuxtSepsN_2' x h y = subjuxtSepsN x h y ing

subjuxtSepsf ::
  forall p.
    Subjuxt p =>
  p String String ->
  p Unit Unit ->
  p Int Int ->
  p (String /\/ Int) (String /\/ Int)
subjuxtSepsf x h y = x /! h !/ y

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
