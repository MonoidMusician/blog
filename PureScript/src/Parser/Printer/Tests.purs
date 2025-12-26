module Parser.Printer.Tests where

import Prelude

import Control.Plus (empty)
import Data.Argonaut as J
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', re)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Console (log, logShow)
import Idiolect (type (/\/), JSON)
import Parser.Codecs (parseStepsCodec)
import Parser.Printer.JSON as C
import Parser.Printer.Juxt (class Subjuxt, class SubjuxtSepsN, Sentinel, _GenericTensor', conjuxt2, conjuxtN, disjuxtN, ing, norm', subjuxtSepN, subjuxtSepsN, (!!!), (!/), (/!), (/!\), (<!), (\!/))
import Parser.Printer.Types (PP)
import Parser.Printer.Types as IO
import Parser.Proto (ParseSteps(..), Stack(..))
import Parser.Types (CST(..))
import Type.Proxy (Proxy(..))

data TestAST
  = TestUnit
  | TestInt Int
  | TestProduct TestAST TestAST

derive instance Generic TestAST _

main :: Effect Unit
main = do
  log "Hi"
  logEncode (C.eitherO C.int C.int) $ Right 5
  logEncAuto @(Either Int Int) $ Right 5
  logEncode (C.eitherO C.int C.int) $ Left 5
  logEncAuto @(Either Int Int) $ Left 5
  logDecode (C.eitherO C.int C.int) $ """{ "data": 5, "type": "Left" }"""
  logDecode (C.eitherO C.int C.int) $ """{ "data": 5, "type": "Right" }"""
  logDecAuto @(Either Int Int) $ """[ "L", 5 ]"""
  logDecAuto @(Either Int Int) $ """[ "L" ]"""
  logDecAuto @(Either Int Int) $ """[ "R", 5 ]"""
  logDecAuto @(Either Int Int) $ """[ "R" ]"""

  logEncAuto @(Maybe Int) $ Just 3
  logEncAuto @(Maybe Int) Nothing
  logDecAuto @(Maybe Int) "[ 3 ]"
  logDecAuto @(Maybe Int) "null"

  logEncAuto { x: 5, y: unit, sadf: [2345,34] }
  let stack = Snoc (Zero 234) (Leaf bottom) 23434
  let step = { stack, inputs: empty }
  logEncode (C.registration C.autoJ (C.maybeA parseStepsCodec)) $ Just $
    Step step (Left bottom) $ Step step (Right (Tuple (NonEmptyString "x") "test")) $ Complete step stack

logEncode :: forall @t. C.JRCodec Void t -> t -> Effect Unit
logEncode codec value = log $ J.stringifyWithIndent 2 $ (C.encode codec value :: JSON)

logEncAuto :: forall @t. C.AutoJSON t Void => t -> Effect Unit
logEncAuto = logEncode C.autoJ

logDecode :: forall @t. Show t => C.JRCodec Void t -> String -> Effect Unit
logDecode codec = J.parseJson >>> traverse_ \json ->
  logShow $ C.decode codec json

logDecAuto :: forall @t. C.AutoJSON t Void => Show t => String -> Effect Unit
logDecAuto = logDecode (C.autoJ @t)

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
  _testAST $ re norm' $ norm' $ disjuxtN
    (pure Proxy <! u)
    i
    (conjuxt2 rec rec)
  ing

testAST_2 :: PP Unit -> PP Int -> PP TestAST
testAST_2 u i = IO.namedRec "testAST" \rec ->
  _testAST
  !!! pure (Proxy @"TestUnit") /!\ u
  \!/ pure (Proxy @"TestInt") /!\ i
  \!/ pure (Proxy @"TestProduct") /!\ rec /!\ rec
