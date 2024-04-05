module Parser.Codecs where

import Prelude

import Data.Argonaut as Json
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor as BF
import Data.Codec (Codec(..), Codec', codec, decode, encode, (~))
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..), (<~<))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant as V
import Data.Variant as Variant
import Foreign.Object as Object
import Parser.Algorithms (parseDefinitions, unParseDefinition)
import Parser.Proto (ParseSteps(..), Stack(..))
import Parser.Types (CST(..), Grammar(..), Part(..), SAugmented, SCParseSteps, SCST, SCStack, SFragment, SPart, SProducible, SState, SStateInfo, SStates, ShiftReduce(..), Zipper(..))
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))


nonEmptyStringCodec :: JsonCodec NonEmptyString
nonEmptyStringCodec =
  CA.prismaticCodec "NonEmptyString" NES.fromString NES.toString CA.string
nonEmptyArrayCodec :: forall a. JsonCodec a -> JsonCodec (NonEmptyArray a)
nonEmptyArrayCodec =
  CA.prismaticCodec "NonEmptyArray" NEA.fromArray NEA.toArray <<< CA.array
intStringCodec :: JsonCodec Int
intStringCodec =
  CA.prismaticCodec "Int" Int.fromString show CA.string

rulesCodec
  :: forall m r
   . Applicative m
  => Codec' m
       (Array { pName :: NonEmptyString, rule :: String | r })
       (Array { pName :: NonEmptyString, rule :: SFragment | r })
rulesCodec = codec
  (pure <<< parseDefinitions)
  (map \r -> r { rule = unParseDefinition r.rule })

lastMile
  :: Codec' (Either JsonDecodeError)
       (Array { pName :: NonEmptyString, rName :: String, rule :: SFragment })
       SAugmented
lastMile = codec
  (fromRules)
  (unwrap <<< _.augmented)
  where
  fromRules rules = do
    top <- note (AtIndex 0 MissingValue) $ rules !! 0
    case top.rule of
      [ NonTerminal entry, Terminal eof ] -> pure
        { augmented: MkGrammar rules
        , entry
        , eof
        , start: { lookahead: [], pName: top.pName, rName: top.rName, rule: Zipper [] top.rule }
        }
      _ -> Left $ AtIndex 0 $ UnexpectedValue $ Json.fromString $ unParseDefinition top.rule

grammarCodec :: JsonCodec SAugmented
grammarCodec = lastMile <~< rulesCodec <~< CA.array do
  CA.indexedArray "GrammarItem" $ { pName: _, rule: _, rName: _ }
    <$> _.pName ~ CA.index 0 nonEmptyStringCodec
    <*> _.rule ~ CA.index 1 CA.string
    <*> _.rName ~ CA.index 2 CA.string

_n :: forall m c d a b. Monad m => Newtype a b => Codec m c d b b -> Codec m c d a a
_n (Codec a b) = Codec (map wrap <<< a) (coerce b)

tuple :: forall a b. JsonCodec a -> JsonCodec b -> JsonCodec (a /\ b)
tuple a b = CA.indexedArray "Tuple" $ (/\)
  <$> fst ~ CA.index 0 a
  <*> snd ~ CA.index 1 b

mappy :: forall k a. Ord k => JsonCodec k -> JsonCodec a -> JsonCodec (Map k a)
mappy key codec = Codec dec enc
  where
  asArray = identity :: Array ~> Array
  dec = \j -> map Map.fromFoldable $
    traverse (\(k /\ j') -> BF.lmap (AtKey k) ((/\) <$> decode key (Json.fromString k) <*> decode codec j'))
      <<< asArray <<< Object.toUnfoldable
      =<< decode CA.jobject j
  enc = \xs -> flip (/\) xs $ Json.fromObject $ Object.fromFoldable $ asArray $
    Array.mapMaybe (\(k /\ v) -> (/\) <$> Json.toString (encode key k) <@> encode codec v) $ Map.toUnfoldable xs

maybeCodec :: forall a. JsonCodec a -> JsonCodec (Maybe a)
maybeCodec codecA =
  dimap toVariant fromVariant
    (CAV.variant
      # CAV.variantCase (Proxy :: Proxy "Just") (Right codecA)
      # CAV.variantCase (Proxy :: Proxy "Nothing") (Left unit))
  where
  toVariant = case _ of
    Just a -> Variant.inj (Proxy :: Proxy "Just") a
    Nothing -> Variant.inj (Proxy :: Proxy "Nothing") unit
  fromVariant = V.case_
    # Variant.on (Proxy :: Proxy "Just") Just
    # Variant.on (Proxy :: Proxy "Nothing") (const Nothing)

eitherCodec :: forall a b. JsonCodec a -> JsonCodec b -> JsonCodec (Either a b)
eitherCodec codecA codecB =
  dimap toVariant fromVariant
    (CAV.variant
      # CAV.variantCase (Proxy :: Proxy "Left") (Right codecA)
      # CAV.variantCase (Proxy :: Proxy "Right") (Right codecB)
    )
  where
  toVariant = case _ of
    Left a -> Variant.inj (Proxy :: Proxy "Left") a
    Right b -> Variant.inj (Proxy :: Proxy "Right") b
  fromVariant = V.case_
    # Variant.on (Proxy :: Proxy "Left") Left
    # Variant.on (Proxy :: Proxy "Right") Right

setCodec :: forall a. Ord a => JsonCodec a -> JsonCodec (Set a)
setCodec codecA = dimap Set.toUnfoldable Set.fromFoldable $
  CA.array codecA

listCodec :: forall a. JsonCodec a -> JsonCodec (List a)
listCodec codecA = dimap Array.fromFoldable Array.toUnfoldable $
  CA.array codecA

statesCodec :: JsonCodec SStates
statesCodec = _n $ CA.array $ stateInfoCodec

stateInfoCodec :: JsonCodec SStateInfo
stateInfoCodec =
  CAR.object "State"
    { sName: CA.int
    , items: stateCodec
    , advance: _n $ mappy CA.codePoint $ shiftReduceCodec CA.int (tuple fragmentCodec $ tuple nonEmptyStringCodec CA.string)
    , receive: mappy nonEmptyStringCodec CA.int
    }

shiftReduceCodec :: forall s r. JsonCodec s -> JsonCodec r -> JsonCodec (ShiftReduce s r)
shiftReduceCodec sC rC = CAV.variantMatch
  { "Shift": Right sC
  , "Reduces": Right $ nonEmptyArrayCodec rC
  , "ShiftReduces": Right $ tuple sC (nonEmptyArrayCodec rC)
  } # dimap
    do
      case _ of
        Shift s -> Variant.inj (Proxy :: Proxy "Shift") s
        Reduces r -> Variant.inj (Proxy :: Proxy "Reduces") r
        ShiftReduces s r -> Variant.inj (Proxy :: Proxy "ShiftReduces") (s /\ r)
    do
      Variant.match
        { "Shift": Shift
        , "Reduces": Reduces
        , "ShiftReduces": \(s /\ r) -> ShiftReduces s r
        }

stateCodec :: JsonCodec SState
stateCodec = _n $ CA.array $ CAR.object "StateItem"
  { lookahead: CA.array CA.codePoint
  , pName: nonEmptyStringCodec
  , rName: CA.string
  , rule: CA.indexedArray "Zipper" $ Zipper
    <$> (\(Zipper before _) -> before) ~ CA.index 0 fragmentCodec
    <*> (\(Zipper _ after) -> after) ~ CA.index 1 fragmentCodec
  }

fragmentCodec :: JsonCodec SFragment
fragmentCodec = CA.array partCodec

partCodec :: JsonCodec SPart
partCodec = CAV.variantMatch
  { "Terminal": Right CA.codePoint
  , "NonTerminal": Right nonEmptyStringCodec
  } # dimap
    do
      case _ of
        Terminal s -> Variant.inj (Proxy :: Proxy "Terminal") s
        NonTerminal r -> Variant.inj (Proxy :: Proxy "NonTerminal") r
    do
      Variant.match
        { "Terminal": Terminal
        , "NonTerminal": NonTerminal
        }

producibleCodec :: JsonCodec SProducible
producibleCodec = CAR.object "Producible"
  { grammar: grammarCodec
  , produced: prulesCodec <~< CA.array do
      CAR.object "Produced"
        { produced: codec (pure <<< String.toCodePointArray) String.fromCodePointArray <~< CA.string
        , production:
            CAR.object "GrammarItem"
              { pName: nonEmptyStringCodec
              , rName: CA.string
              , rule: CA.string
              }
        }
  }
  where
  prulesCodec = codec (pure <<< smuggled) smuggle <~< rulesCodec <~< codec (pure <<< smuggle) smuggled

smuggle
  :: forall f a b c d
   . Functor f
  => f
       { produced :: a
       , production ::
           { pName :: b
           , rName :: c
           , rule :: d
           }
       }
  -> f
       { pName :: b
       , produced :: a
       , rName :: c
       , rule :: d
       }
smuggle = map \{ produced, production } ->
  { pName: production.pName
  , rName: production.rName
  , rule: production.rule
  , produced: produced
  }

smuggled
  :: forall f a b c d
   . Functor f
  => f
       { pName :: a
       , produced :: b
       , rName :: c
       , rule :: d
       }
  -> f
       { produced :: b
       , production ::
           { pName :: a
           , rName :: c
           , rule :: d
           }
       }
smuggled = map \r ->
  { produced: r.produced
  , production:
      { pName: r.pName
      , rName: r.rName
      , rule: r.rule
      }
  }

parseStepsCodec :: JsonCodec SCParseSteps
parseStepsCodec = dimap conv1 conv2 $
  CAR.object "ParseSteps"
    { steps: listCodec $
        CAR.object "ParseStep"
          { stack: stackCodec
          , inputs: listCodec input
          , step: eitherCodec input rule
          }
    , ending:
      CAV.variantMatch
        { "Error": Right $
            CAR.object "ParseError"
              { stack: stackCodec
              , inputs: listCodec input
              }
        , "Complete": Right $
            CAR.object "ParseComplete"
              { stack: stackCodec
              , inputs: listCodec input
              , final: stackCodec
              }
        }
    }
  where
  input = CA.codePoint
  rule = tuple nonEmptyStringCodec CA.string
  conv1 = case _ of
    Error final ->
      { steps: Nil
      , ending: Variant.inj (Proxy :: Proxy "Error") final
      }
    Complete final stack ->
      { steps: Nil
      , ending: Variant.inj (Proxy :: Proxy "Complete") $
          Record.insert (Proxy :: Proxy "final") stack final
      }
    Step { stack, inputs } step more ->
      conv1 more # Record.modify (Proxy :: Proxy "steps")
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

cstCodec :: JsonCodec SCST
cstCodec = CA.fix \rec ->
  dimap conv1 conv2 $ CAV.variantMatch
    { "Leaf": Right CA.codePoint
    , "Branch": Right $ tuple (tuple nonEmptyStringCodec CA.string) (CA.array rec)
    }
  where
  conv1 = case _ of
    Leaf v -> Variant.inj (Proxy :: Proxy "Leaf") v
    Branch r cs -> Variant.inj (Proxy :: Proxy "Branch") (r /\ cs)
  conv2 = Variant.match
    { "Leaf": Leaf
    , "Branch": uncurry Branch
    }

stackCodec :: JsonCodec SCStack
stackCodec = dimap conv1 conv2 $
  CAR.object "Stack"
    { head: CA.int
    , tail: listCodec $ CAR.object "StackItem"
        { tok: cstCodec
        , state: CA.int
        }
    }
  where
  conv1 = case _ of
    Zero head -> { head, tail: Nil }
    Snoc more tok state ->
      conv1 more # Record.modify (Proxy :: Proxy "tail")
        (Cons { tok, state })
  conv2 = case _ of
    { head, tail: Nil } -> Zero head
    { head, tail: Cons { tok, state } tail } ->
      Snoc (conv2 { head, tail }) tok state
