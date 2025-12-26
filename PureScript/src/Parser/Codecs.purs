module Parser.Codecs where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Argonaut as Json
import Data.Array ((!!))
import Data.Either (Either(..), either, note)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (dimap)
import Data.String (CodePoint)
import Data.String.NonEmpty (NonEmptyString)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Data.Variant as Variant
import Parser.Algorithms (parseDefinitions, unParseDefinition)
import Parser.Printer.JSON (Conv, JCodec, OneJCodec', noRegister, noneCodec, (:=), (<~<))
import Parser.Printer.JSON as C
import Parser.Printer.Juxt (rec0, (!!!), (/!\))
import Parser.Proto (ParseSteps(..), Stack(..))
import Parser.Types (CST(..), Fragment, Grammar(..), Part(..), SAugmented, SCParseSteps, SCST, SCStack, SProducible, SStateInfo, SStates, Zipper(..))
import Record as Record
import Type.Proxy (Proxy(..))


rulesCodec :: forall r. Conv
  (Array { pName :: NonEmptyString, rule :: String | r })
  (Array { pName :: NonEmptyString, rule :: Fragment Unit NonEmptyString CodePoint | r })
rulesCodec = dimap
  (map \r -> r { rule = unParseDefinition r.rule })
  (parseDefinitions)
  noneCodec

lastMile :: Conv
  (Array { pName :: NonEmptyString, rName :: String, rule :: Fragment Unit NonEmptyString CodePoint })
  SAugmented
lastMile = noRegister
  (unwrap <<< _.augmented)
  (either C.jsonError pure <<< fromRules)
  where
  fromRules rules = do
    top <- note (AtIndex 0 MissingValue) $ rules !! 0
    case top.rule of
      [ NonTerminal entry, Terminal eof ] -> pure
        { augmented: MkGrammar rules
        , entry
        , eof
        , start: { lookbehind: Additive one, lookahead: [], pName: top.pName, rName: top.rName, rule: Zipper [] top.rule }
        }
      _ -> Left $ AtIndex 0 $ UnexpectedValue $ Json.fromString $ unParseDefinition top.rule

grammarCodec :: JCodec SAugmented
grammarCodec = lastMile <~< rulesCodec <~< C.array do
  C.tupled
    !!! Proxy @"pName" := C.nestring
    !!! Proxy @"rule"  := C.string
    !!! Proxy @"rName" := C.string
    !!! rec0

_N :: forall c d a b. Newtype a b => OneJCodec' c d b b -> OneJCodec' c d a a
_N = _Newtype

statesCodec :: JCodec SStates
statesCodec = _N $ C.array $ stateInfoCodec

stateInfoCodec :: JCodec SStateInfo
stateInfoCodec = C.autoJ

producibleCodec :: JCodec SProducible
producibleCodec = C.record
  { grammar: grammarCodec
  , produced: dimap smuggle smuggled $ rulesCodec <~< do
      dimap smuggled smuggle $ C.autoJ
  }

smuggle :: forall f a b c d. Functor f =>
  f { produced :: a
    , production ::
      { pName :: b, rName :: c, rule :: d }
    } ->
  f { pName :: b, produced :: a, rName :: c, rule :: d }
smuggle = map \{ produced, production } ->
  { pName: production.pName
  , rName: production.rName
  , rule: production.rule
  , produced: produced
  }

smuggled :: forall f a b c d. Functor f =>
  f { pName :: a, produced :: b, rName :: c, rule :: d } ->
  f { produced :: b
    , production :: { pName :: a, rName :: c, rule :: d }
    }
smuggled = map \r ->
  { produced: r.produced
  , production: { pName: r.pName, rName: r.rName, rule: r.rule }
  }

parseStepsCodec :: JCodec SCParseSteps
parseStepsCodec = dimap conv1 conv2 $ C.record
  { steps: C.list $ C.record
    { stack: stackCodec
    , inputs: C.list input
    , step: C.eitherA input rule
    }
  , ending: C.variant
    { "Error": C.fields
      { stack: stackCodec
      , inputs: C.list input
      }
    , "Complete": C.fields
      { stack: stackCodec
      , inputs: C.list input
      , final: stackCodec
      }
    }
  }
  where
  input = C.codePoint
  rule = C.tuple C.nestring C.string
  conv1 = case _ of
    Error final ->
      { steps: Nil
      , ending: Variant.inj (Proxy @"Error") final
      }
    Complete final stack ->
      { steps: Nil
      , ending: Variant.inj (Proxy @"Complete") $
          Record.insert (Proxy @"final") stack final
      }
    Step { stack, inputs } step more ->
      conv1 more # Record.modify (Proxy @"steps")
        (Cons { stack, inputs, step })
  conv2 = case _ of
    { steps: Nil, ending } ->
      ending # Variant.match
        { "Error": Error
        , "Complete": \{ stack, inputs, final } ->
            Complete { stack, inputs } final
        }
    { steps: Cons { stack, inputs, step } steps, ending } ->
      Step { stack, inputs } step (conv2 { steps, ending })

cstCodec :: JCodec SCST
cstCodec = C.fix \rec ->
  dimap conv1 conv2 $ C.adt
    { "Leaf": C.item C.codePoint
    , "Branch": (C.item C.nestring /!\ C.item C.string) /!\ C.item (C.array rec)
    }
  where
  conv1 = case _ of
    Leaf v -> Variant.inj (Proxy @"Leaf") v
    Branch r cs -> Variant.inj (Proxy @"Branch") (r /\ cs)
  conv2 = Variant.match
    { "Leaf": Leaf
    , "Branch": uncurry Branch
    }

stackCodec :: JCodec SCStack
stackCodec = dimap conv1 conv2 $
  C.record
    { head: C.int
    , tail: C.list $ C.record
        { tok: cstCodec
        , state: C.int
        }
    }
  where
  conv1 = case _ of
    Zero head -> { head, tail: Nil }
    Snoc more tok state ->
      conv1 more # Record.modify (Proxy @"tail")
        (Cons { tok, state })
  conv2 = case _ of
    { head, tail: Nil } -> Zero head
    { head, tail: Cons { tok, state } tail } ->
      Snoc (conv2 { head, tail }) tok state
