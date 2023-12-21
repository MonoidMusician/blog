module Parser.ProtoG8 where

import Prelude

import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (CodePoint, codePointFromChar)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Idiolect ((>==))
import Parser.Proto (Stack(..), Table(..), parseMany, parseOne, topOf)

data State
  = S0 | SA
  | S1 | S2 | S3 | S4
  | S5 | S6 | S7 | S8

derive instance eqState :: Eq State
derive instance ordState :: Ord State
derive instance genericState :: Generic State _
instance showState :: Show State where
  show = genericShow
instance enumState :: Enum State where
  succ = genericSucc
  pred = genericPred
instance boundedState :: Bounded State where
  top = genericTop
  bottom = genericBottom
instance boundedEnumState :: BoundedEnum State where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data Rule
  = RE1 | RE2 | RL1 | RL2

derive instance eqRule :: Eq Rule
derive instance ordRule :: Ord Rule
derive instance genericRule :: Generic Rule _
instance showRule :: Show Rule where
  show = genericShow
instance enumRule :: Enum Rule where
  succ = genericSucc
  pred = genericPred
instance boundedRule :: Bounded Rule where
  top = genericTop
  bottom = genericBottom
instance boundedEnumRule :: BoundedEnum Rule where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data Sorts
  = RE | RL

derive instance eqSorts :: Eq Sorts
derive instance ordSorts :: Ord Sorts
derive instance genericSorts :: Generic Sorts _
instance showSorts :: Show Sorts where
  show = genericShow
instance enumSorts :: Enum Sorts where
  succ = genericSucc
  pred = genericPred
instance boundedSorts :: Bounded Sorts where
  top = genericTop
  bottom = genericBottom
instance boundedEnumSorts :: BoundedEnum Sorts where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data Grammar = T Tok | NT Sorts


derive instance eqGrammar :: Eq Grammar
derive instance ordGrammar :: Ord Grammar
derive instance genericGrammar :: Generic Grammar _
instance showGrammar :: Show Grammar where
  show = genericShow
instance enumGrammar :: Enum Grammar where
  succ = genericSucc
  pred = genericPred
instance boundedGrammar :: Bounded Grammar where
  top = genericTop
  bottom = genericBottom
instance boundedEnumGrammar :: BoundedEnum Grammar where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data Tok = LParen | RParen | Comma | X | EOF

derive instance eqTok :: Eq Tok
derive instance ordTok :: Ord Tok
derive instance genericTok :: Generic Tok _
instance showTok :: Show Tok where
  show = genericShow
instance enumTok :: Enum Tok where
  succ = genericSucc
  pred = genericPred
instance boundedTok :: Bounded Tok where
  top = genericTop
  bottom = genericBottom
instance boundedEnumTok :: BoundedEnum Tok where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data Parsed = E E | L L | Tok Tok

derive instance eqParsed :: Eq Parsed
derive instance ordParsed :: Ord Parsed
derive instance genericParsed :: Generic Parsed _
instance showParsed :: Show Parsed where
  show = genericShow

data E = E1 L | E2

derive instance eqE :: Eq E
derive instance ordE :: Ord E
derive instance genericE :: Generic E _
instance showE :: Show E where
  show = genericShow

data L = L1 E | L2 L E

derive instance eqL :: Eq L
derive instance ordL :: Ord L
derive instance genericL :: Generic L _
instance showL :: Show L where
  show x = genericShow x

g8Table :: Partial => Table State Rule Tok Parsed
g8Table = Table { step: g8Step, promote: g8Promote, goto: g8Reduce } where
  g8Promote = Tok

  g8Rules RE1 = [T LParen, NT RL, T RParen]
  g8Rules RE2 = [T X]
  g8Rules RL1 = [NT RE]
  g8Rules RL2 = [NT RL, T Comma, NT RE]

  g8Step S0 EOF = Just (Left SA)

  g8Step S1 LParen = Just (Left S3)
  g8Step S1 X = Just (Left S2)

  g8Step S2 _ = Just (Right RE2)

  g8Step S3 LParen = Just (Left S3)
  g8Step S3 X = Just (Left S2)

  g8Step S4 RParen = Just (Left S6)
  g8Step S4 Comma = Just (Left S7)

  g8Step S5 _ = Just (Right RL1)

  g8Step S6 _ = Just (Right RE1)

  g8Step S7 LParen = Just (Left S3)
  g8Step S7 X = Just (Left S2)

  g8Step S8 _ = Just (Right RL2)

  g8Step _ _ = Nothing

  g8RuleType RE1 = RE
  g8RuleType RE2 = RE
  g8RuleType RL1 = RL
  g8RuleType RL2 = RL

  g8Goto RE S1 = Just S0
  g8Goto RE S3 = Just S5
  g8Goto RE S7 = Just S8
  g8Goto RL S3 = Just S4

  g8Reduce RE1 (Snoc (Snoc (Snoc stack (Tok LParen) _) (L l1) _) (Tok RParen) _)
    | Just jmp <- g8Goto RE (topOf stack)
    = Just (Snoc stack (E (E1 l1)) jmp)
  g8Reduce RE2 (Snoc stack (Tok X) _)
    | Just jmp <- g8Goto RE (topOf stack)
    = Just (Snoc stack (E E2) jmp)
  g8Reduce RL1 (Snoc stack (E e1) _)
    | Just jmp <- g8Goto RL (topOf stack)
    = Just (Snoc stack (L (L1 e1)) jmp)
  g8Reduce RL2 (Snoc (Snoc (Snoc stack (L l1) _) (Tok Comma) _) (E e1) _)
    | Just jmp <- g8Goto RL (topOf stack)
    = Just (Snoc stack (L (L2 l1 e1)) jmp)

g8ParseOne :: Partial => Tok -> Stack State Parsed -> Maybe (Stack State Parsed)
g8ParseOne = parseOne g8Table
g8ParseAll :: Partial => List Tok -> Maybe E
g8ParseAll toks = parseMany g8Table (toks <> pure EOF) S1 >>= g8ParseResult
g8ParseResult :: Stack State Parsed -> Maybe E
g8ParseResult = case _ of
  Snoc (Snoc (Zero S1) (E r) S0) (Tok EOF) SA -> Just r
  _ -> Nothing
g8ParseString :: Partial => String -> Maybe E
g8ParseString = g8FromString >=> g8ParseAll
g8FromString :: String -> Maybe (List Tok)
g8FromString =
  String.toCodePointArray
  >>> traverse (flip Map.lookup g8Toks)
  >== Array.catMaybes
  >>> ensureEOF
  >>> Array.toUnfoldable
ensureEOF :: Array Tok -> Array Tok
ensureEOF toks = case Array.last toks of
  Just EOF -> toks
  _ -> toks <> [EOF]
g8Toks :: Map CodePoint (Maybe Tok)
g8Toks = Map.fromFoldable
  [ Tuple (codePointFromChar '(') $ Just LParen
  , Tuple (codePointFromChar ')') $ Just RParen
  , Tuple (codePointFromChar ',') $ Just Comma
  , Tuple (codePointFromChar 'x') $ Just X
  , Tuple (codePointFromChar '$') $ Just EOF
  , Tuple (codePointFromChar ' ') Nothing
  , Tuple (codePointFromChar '\t') Nothing
  , Tuple (codePointFromChar '\n') Nothing
  ]
