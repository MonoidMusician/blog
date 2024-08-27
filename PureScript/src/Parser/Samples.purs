module Parser.Samples where

import Prelude

import Data.Array as Array
import Data.Either (Either, fromRight')
import Data.List (List)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (CodePoint, codePointFromChar)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested (type (/\))
import Parser.Algorithms (fromSeed, fromSeed', generate, generate', indexStates, numberStatesBy, parseIntoGrammar, toTable, toTable', verifyTokens)
import Parser.Proto as Proto
import Parser.ProtoG8 as G8
import Parser.Types (AST, Augmented, CST, Grammar(..), OrEOF, Part(..), SATable, SAugmented, SCTable, SGrammar, State, States)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

g8Grammar :: Grammar Unit G8.Sorts G8.Rule G8.Tok
g8Grammar = MkGrammar
  [ { pName: G8.RE, rName: G8.RE1, rule: [ Terminal G8.LParen, NonTerminal G8.RL, Terminal G8.RParen ] }
  , { pName: G8.RE, rName: G8.RE2, rule: [ Terminal G8.X ] }
  , { pName: G8.RL, rName: G8.RL1, rule: [ NonTerminal G8.RE ] }
  , { pName: G8.RL, rName: G8.RL2, rule: [ NonTerminal G8.RL, Terminal G8.Comma, NonTerminal G8.RE ] }
  ]

exGrammar :: SGrammar
exGrammar = parseIntoGrammar
  [ { pName: "E", rName: "E1", rule: "(L)" }
  , { pName: "E", rName: "E2", rule: "x" }
  , { pName: "L", rName: "L1", rule: "E" }
  , { pName: "L", rName: "L2", rule: "L,E" }
  ]

g8Seed :: Augmented Unit (Maybe G8.Sorts) (Maybe G8.Rule) (OrEOF G8.Tok)
g8Seed = fromSeed g8Grammar G8.RE

defaultTopName :: NonEmptyString
defaultTopName = (unsafePartial (fromJust (NES.fromString "TOP")))

defaultTopRName :: String
defaultTopRName = "TOP"

defaultEOF :: CodePoint
defaultEOF = (codePointFromChar '␄')

exSeed :: SAugmented
exSeed = fromSeed' defaultTopName defaultTopRName defaultEOF exGrammar (unsafePartial (fromJust (NES.fromString "E")))

g8Generated :: forall a. a -> Array (State Unit (Maybe G8.Sorts) (Maybe G8.Rule) (OrEOF G8.Tok))
g8Generated _ = generate g8Grammar G8.RE

exGenerated :: forall t. t -> Array (State Unit NonEmptyString String CodePoint)
exGenerated _ = generate' defaultTopName defaultTopRName defaultEOF exGrammar (unsafePartial (fromJust (NES.fromString "E")))

g8States :: forall a. a -> States Int Unit (Maybe G8.Sorts) (Maybe G8.Rule) (OrEOF G8.Tok)
g8States a = fromRight' (\_ -> unsafeCrashWith "state generation did not work")
  (numberStatesBy (add 1) g8Seed.augmented (g8Generated a))

exStates :: forall t. t -> States Int Unit NonEmptyString String CodePoint
exStates a = fromRight' (\_ -> unsafeCrashWith "state generation did not work")
  (numberStatesBy (add 1) exSeed.augmented (exGenerated a))

g8Table :: forall a. a -> Proto.Table Int (Maybe G8.Sorts /\ Maybe G8.Rule) (OrEOF G8.Tok) (CST (Maybe G8.Sorts /\ Maybe G8.Rule) (OrEOF G8.Tok))
g8Table = apply toTable indexStates <<< g8States

exTable :: forall t. t -> SCTable
exTable = apply toTable indexStates <<< exStates

g8Table' :: forall a. a -> Proto.Table Int (Maybe G8.Sorts /\ Maybe G8.Rule) (OrEOF G8.Tok) (Either (OrEOF G8.Tok) (AST (Maybe G8.Sorts /\ Maybe G8.Rule)))
g8Table' = apply toTable' indexStates <<< g8States

exTable' :: forall t. t -> SATable
exTable' = apply toTable' indexStates <<< exStates

g8FromString :: String -> Maybe (List (Maybe G8.Tok))
g8FromString = G8.g8FromString >>> map map map case _ of
  G8.EOF -> Nothing
  t -> Just t

exFromString :: String -> Maybe (List CodePoint)
exFromString = String.toCodePointArray >>> flip append [ defaultEOF ] >>> Array.toUnfoldable >>> verifyTokens exSeed.augmented
