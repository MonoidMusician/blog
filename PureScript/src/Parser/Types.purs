module Parser.Types where

import Prelude

import Control.Alternative (guard)
import Data.Array (foldl, mapWithIndex)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..), hush)
import Data.Filterable (partitionMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map, SemigroupMap)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.String as String
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Traversable (class Foldable, foldMap, mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Parser.Proto as Proto

data OrEOF a = EOF | Continue a
derive instance eqOrEOF :: Eq a => Eq (OrEOF a)
derive instance ordOrEOF :: Ord a => Ord (OrEOF a)

instance showOrEOF :: Show a => Show (OrEOF a) where
  show EOF = "EOF"
  show (Continue a) = "(Continue " <> show a <> ")"

notEOF :: OrEOF ~> Maybe
notEOF EOF = Nothing
notEOF (Continue a) = Just a

newtype Grammar meta nt r tok = MkGrammar
  (Array (GrammarRule meta nt r tok))
derive newtype instance showGrammar :: (Show meta, Show nt, Show r, Show tok) => Show (Grammar meta nt r tok)
derive instance newtypeGrammar :: Newtype (Grammar meta nt r tok) _
derive newtype instance semigroupGrammar :: Semigroup (Grammar meta nt r tok)
derive newtype instance monoidGrammar :: Monoid (Grammar meta nt r tok)

type GrammarRule meta nt r tok =
  { pName :: nt -- nonterminal / production rule name
  , rName :: r -- each rule has a unique name
  , rule :: Fragment meta nt tok -- sequence of nonterminals and terminals that make up the rule
  }

getRulesFor :: forall meta nt r tok. Eq nt => Array (Produced meta nt r tok) -> nt -> Array { rule :: Fragment meta nt tok, produced :: Array tok }
getRulesFor rules nt = rules # Array.mapMaybe \rule ->
  if rule.production.pName /= nt then Nothing
  else
    Just { rule: rule.production.rule, produced: rule.produced }

type Produced meta nt r tok =
  { production :: GrammarRule meta nt r tok
  , produced :: Array tok
  }

type Producible meta nt r tok =
  { grammar :: Augmented meta nt r tok
  , produced :: Array (Produced meta nt r tok)
  }

type SProducible = Producible Unit NonEmptyString String CodePoint

type Augmented meta nt r tok =
  { augmented :: Grammar meta nt r tok
  , start :: StateItem meta nt r tok
  , eof :: tok
  , entry :: nt
  }
type Augmenteds meta nt r tok =
  { augmented :: Grammar meta nt r tok
  , eof :: tok
  , starts :: Array (StateItem meta nt r tok)
  }

type SAugmented = Augmented Unit NonEmptyString String CodePoint



type SGrammar = Grammar Unit NonEmptyString String CodePoint
data Part meta nt tok = NonTerminal nt | Terminal tok | InterTerminal meta

derive instance eqPart :: (Eq meta, Eq nt, Eq tok) => Eq (Part meta nt tok)
derive instance ordPart :: (Ord meta, Ord nt, Ord tok) => Ord (Part meta nt tok)
derive instance genericPart :: Generic (Part meta nt tok) _
instance showPart :: (Show meta, Show nt, Show tok) => Show (Part meta nt tok) where
  show x = genericShow x

derive instance functorPart :: Functor (Part meta nt)
instance bifunctorPart :: Bifunctor (Part meta) where
  bimap f _ (NonTerminal nt) = NonTerminal (f nt)
  bimap _ g (Terminal tok) = Terminal (g tok)
  bimap _ _ (InterTerminal meta) = InterTerminal meta

type SPart = Part Unit NonEmptyString CodePoint

isNonTerminal :: forall meta nt tok. Part meta nt tok -> Boolean
isNonTerminal (NonTerminal _) = true
isNonTerminal _ = false

isTerminal :: forall meta nt tok. Part meta nt tok -> Boolean
isTerminal (Terminal _) = true
isTerminal _ = false

isInterTerminal :: forall meta nt tok. Part meta nt tok -> Boolean
isInterTerminal (InterTerminal _) = true
isInterTerminal _ = false

unNonTerminal :: forall meta nt tok. Part meta nt tok -> Maybe nt
unNonTerminal (NonTerminal nt) = Just nt
unNonTerminal _ = Nothing

unTerminal :: forall meta nt tok. Part meta nt tok -> Maybe tok
unTerminal (Terminal t) = Just t
unTerminal _ = Nothing

unInterTerminal :: forall meta nt tok. Part meta nt tok -> Maybe meta
unInterTerminal (InterTerminal it) = Just it
unInterTerminal _ = Nothing

noInterTerminal :: forall meta nt tok. Part meta nt tok -> Maybe (Part Void nt tok)
noInterTerminal (InterTerminal _) = Nothing
noInterTerminal (Terminal t) = Just (Terminal t)
noInterTerminal (NonTerminal nt) = Just (NonTerminal nt)

noInterTerminals :: forall meta nt tok. Fragment meta nt tok -> Fragment Void nt tok
noInterTerminals = Array.mapMaybe noInterTerminal

noInterTerminalz :: forall meta nt tok. Zipper meta nt tok -> Zipper Void nt tok
noInterTerminalz (Zipper before after) = Zipper (noInterTerminals before) (noInterTerminals after)

unSPart :: SPart -> String
unSPart (Terminal t) = String.singleton t
unSPart (NonTerminal nt) = NES.toString nt
unSPart (InterTerminal _) = mempty

type Fragment meta nt tok = Array (Part meta nt tok)
type SFragment = Fragment Unit NonEmptyString CodePoint

data Zipper meta nt tok = Zipper (Fragment meta nt tok) (Fragment meta nt tok)

derive instance bifunctorZipper :: Bifunctor (Zipper meta)

derive instance eqZipper :: (Eq meta, Eq nt, Eq tok) => Eq (Zipper meta nt tok)
derive instance ordZipper :: (Ord meta, Ord nt, Ord tok) => Ord (Zipper meta nt tok)
derive instance genericZipper :: Generic (Zipper meta nt tok) _
instance showZipper :: (Show meta, Show nt, Show tok) => Show (Zipper meta nt tok) where
  show x = genericShow x

type SZipper = Zipper Unit NonEmptyString CodePoint

unZipper :: forall meta nt tok. Zipper meta nt tok -> Fragment meta nt tok
unZipper (Zipper before after) = before <> after

newtype State meta nt r tok = State (Array (StateItem meta nt r tok))
derive instance newtypeState :: Newtype (State meta nt r tok) _

instance eqState :: (Eq meta, Eq nt, Eq r, Eq tok) => Eq (State meta nt r tok) where
  eq (State s1) (State s2) = s1 == s2 ||
    Array.length s1 == Array.length s2 &&
      noNew s1 s2 && noNew s2 s1

instance ordState :: (Ord meta, Ord nt, Ord r, Ord tok) => Ord (State meta nt r tok) where
  compare (State s1) (State s2) = compare (deepSort s1) (deepSort s2)
    where
    deepSort = Array.sort <<< map \item ->
      item { lookahead = Array.sort item.lookahead }

derive instance genericState :: Generic (State meta nt r tok) _
instance showState :: (Show meta, Show nt, Show r, Show tok) => Show (State meta nt r tok) where
  show = genericShow

instance semigroupState :: (Semiring meta, Eq meta, Eq nt, Eq r, Eq tok) => Semigroup (State meta nt r tok) where
  append s1 (State s2) = minimizeStateCat s1 s2

nubEqCat :: forall a. Eq a => Array a -> Array a -> Array a
nubEqCat as bs = as <> Array.filter (not Array.elem <@> as) bs

-- TODO meta
sameRule :: forall meta nt r tok. Eq meta => Eq nt => Eq r => Eq tok => StateItem meta nt r tok -> StateItem meta nt r tok -> Boolean
sameRule item newItem = item.pName == newItem.pName && item.rName == newItem.rName && item.rule == newItem.rule

minimizeState :: forall meta nt r tok. Semiring meta => Eq meta => Eq nt => Eq r => Eq tok => Array (StateItem meta nt r tok) -> State meta nt r tok
minimizeState = compose State $ [] # Array.foldl \items newItem ->
  let
    accumulate :: Boolean -> StateItem meta nt r tok -> { accum :: Boolean, value :: StateItem meta nt r tok }
    accumulate alreadyFound item =
      if sameRule item newItem
        then { accum: true, value: item { lookbehind = item.lookbehind <> newItem.lookbehind, lookahead = nubEqCat item.lookahead newItem.lookahead } }
        else { accum: alreadyFound, value: item }
    { accum: found, value: items' } =
      mapAccumL accumulate false items
  in
    if found then items' else items' <> [ newItem ]

minimizeStateCat :: forall meta nt r tok. Semiring meta => Eq meta => Eq nt => Eq r => Eq tok => State meta nt r tok -> Array (StateItem meta nt r tok) -> State meta nt r tok
minimizeStateCat prev [] = prev
minimizeStateCat (State prev) newItems = State result
  where
  indexed = mapWithIndex (/\) newItems
  discarded /\ prevAugmented = prev # traverse \item ->
    case indexed # Array.mapMaybe \x@(_ /\ newItem) -> if sameRule item newItem then Just x else Nothing of
      [] -> [] /\ item
      found -> map fst found /\ item { lookahead = foldl (\x (_ /\ { lookahead }) -> nubEqCat x lookahead) item.lookahead found }
  State newFiltered = minimizeState $
    indexed # Array.mapMaybe \(i /\ y) -> if discarded # Array.any (eq i) then Nothing else Just y
  result = case newFiltered of
    [] -> prevAugmented
    _ -> prevAugmented <> newFiltered

noNew :: forall meta nt r tok. Eq meta => Eq nt => Eq r => Eq tok => Array (StateItem meta nt r tok) -> Array (StateItem meta nt r tok) -> Boolean
noNew prev newItems = newItems # Array.all \newItem ->
  prev # Array.any \item ->
    sameRule item newItem && item.lookbehind == newItem.lookbehind && do
      newItem.lookahead # Array.all \look -> item.lookahead # Array.any (eq look)

type SState = State Unit NonEmptyString String CodePoint
type Lookahead meta tok = Array (Tuple meta tok)
type StateItem meta nt r tok =
  { rName :: r
  , pName :: nt
  , rule :: Zipper meta nt tok
  , lookbehind :: Additive meta
  , lookahead :: Lookahead meta tok
  }

type SStateItem = StateItem Unit NonEmptyString String CodePoint

data ShiftReduce s r
  = Shift s
  | Reduces (NonEmptyArray r)
  | ShiftReduces s (NonEmptyArray r)

derive instance eqShiftReduce :: (Eq s, Eq r) => Eq (ShiftReduce s r)
derive instance ordShiftReduce :: (Ord s, Ord r) => Ord (ShiftReduce s r)

derive instance genericShiftReduce :: Generic (ShiftReduce s r) _
instance showShiftReduce :: (Show s, Show r) => Show (ShiftReduce s r) where
  show = genericShow

unShift :: forall s r. ShiftReduce s r -> Maybe s
unShift (Shift s) = Just s
unShift (ShiftReduces s _) = Just s
unShift (Reduces _) = Nothing

reductions :: forall s r. ShiftReduce s r -> Array r
reductions (Shift _) = []
reductions (ShiftReduces _ rs) = NEA.toArray rs
reductions (Reduces rs) = NEA.toArray rs

-- Prefer shifts because they are unique
decide :: forall s r. ShiftReduce s r -> Maybe (Either s r)
decide (Shift s) = Just (Left s)
decide (ShiftReduces s _) = Just (Left s)
decide (Reduces r) = if NEA.length r == 1 then Just (Right (NEA.head r)) else Nothing

decisionUnique :: forall s r. ShiftReduce s r -> Boolean
decisionUnique (Shift _) = true
decisionUnique (ShiftReduces _ _) = false
decisionUnique (Reduces r) = NEA.length r == 1

filterMapSR :: forall s r s' r'. (s -> Maybe s') -> (r -> Maybe r') -> ShiftReduce s r -> Maybe (ShiftReduce s' r')
filterMapSR f _ (Shift s) = Shift <$> f s
filterMapSR _ g (Reduces rs) = Reduces <$> NEA.fromArray (NEA.mapMaybe g rs)
filterMapSR f g (ShiftReduces s rs) = case f s, NEA.fromArray (NEA.mapMaybe g rs) of
  Nothing, Nothing -> Nothing
  Just s', Nothing -> Just (Shift s')
  Nothing, Just rs' -> Just (Reduces rs')
  Just s', Just rs' -> Just (ShiftReduces s' rs')

filterSR :: forall s r. (s -> Boolean) -> (r -> Boolean) -> ShiftReduce s r -> Maybe (ShiftReduce s r)
filterSR f _ (Shift s) = Shift s <$ guard (f s)
filterSR _ g (Reduces rs) = Reduces <$> NEA.fromArray (NEA.filter g rs)
filterSR f g (ShiftReduces s rs) = case f s, NEA.fromArray (NEA.filter g rs) of
  false, Nothing -> Nothing
  true, Nothing -> Just (Shift s)
  false, Just rs' -> Just (Reduces rs')
  true, Just rs' -> Just (ShiftReduces s rs')

someSuccess :: forall e a b. Monoid e => (a -> Either e b) -> NonEmptyArray a -> Either e (NonEmptyArray b)
someSuccess f = NEA.toArray >>> partitionMap f >>> case _ of
  { right } | Just r <- NEA.fromArray right -> Right r
  { left } -> Left (Array.fold left)

filterSR' :: forall e s s' r r'. Monoid e => (s -> Either e s') -> (r -> Either e r') -> ShiftReduce s r -> Either e (ShiftReduce s' r')
filterSR' f _ (Shift s) = Shift <$> f s
filterSR' _ g (Reduces rs) = Reduces <$> someSuccess g rs
filterSR' f g (ShiftReduces s rs) = case f s, someSuccess g rs of
  Left e1, Left e2 -> Left (e1 <> e2)
  Right s', Left _ -> Right (Shift s')
  Left _, Right rs' -> Right (Reduces rs')
  Right s', Right rs' -> Right (ShiftReduces s' rs')

derive instance functorShiftReduce :: Functor (ShiftReduce s)
derive instance bifunctorShiftReduce :: Bifunctor ShiftReduce
derive instance foldableShiftReduce :: Foldable (ShiftReduce s)
derive instance bifoldableShiftReduce :: Bifoldable ShiftReduce
instance semigroupShiftReduce :: Semigroup (ShiftReduce s r) where
  -- We do not expect to see two shifts, so arbitrarily prefer the first one
  append (Shift s) (Shift _) = Shift s
  append (Shift s) (ShiftReduces _ rs) = ShiftReduces s rs
  append (ShiftReduces s rs) (Shift _) = ShiftReduces s rs
  append (ShiftReduces s rs) (ShiftReduces _ rs') = ShiftReduces s (rs <> rs')
  append (Shift s) (Reduces rs) = ShiftReduces s rs
  append (Reduces rs) (Shift s) = ShiftReduces s rs
  append (Reduces rs) (Reduces rs') = Reduces (rs <> rs')
  append (ShiftReduces s rs) (Reduces rs') = ShiftReduces s (rs <> rs')
  append (Reduces rs) (ShiftReduces s rs') = ShiftReduces s (rs <> rs')

type StateInfo s meta nt r tok =
  { sName :: s
  , items :: State meta nt r tok
  , advance :: SemigroupMap tok (Additive meta /\ ShiftReduce s (Fragment meta nt tok /\ nt /\ r))
  , receive :: Map nt s
  }

type SStateInfo = StateInfo Int Unit NonEmptyString String CodePoint

newtype States s meta nt r tok = States
  (Array (StateInfo s meta nt r tok))

derive instance newtypeStates :: Newtype (States s meta nt r tok) _

type SStates = States Int Unit NonEmptyString String CodePoint

conflicts ::
  forall s meta nt r tok.
  States s meta nt r tok ->
  Array
    { sName :: s
    , items :: State meta nt r tok
    , advance :: meta /\ tok
    , conflict :: ShiftReduce s (Fragment meta nt tok /\ nt /\ r)
    }
conflicts (States states) =
  states # foldMap \{ sName, items, advance } ->
  advance # foldMapWithIndex \tok (Additive meta /\ sr) ->
    { sName, items, advance: meta /\ tok, conflict: sr } <$
      guard (not decisionUnique sr)


data CST r tok
  = Leaf tok
  | Branch r (Array (CST r tok))
derive instance eqCST :: (Eq r, Eq tok) => Eq (CST r tok)
derive instance functorCST :: Functor (CST r)
derive instance bifunctorCST :: Bifunctor CST

sourceCST :: forall r tok. CST r tok -> Array tok
sourceCST (Leaf tok) = pure tok
sourceCST (Branch _ children) = children >>= sourceCST

data ICST air r tok
  = ILeaf tok
  | IAir air
  | IBranch r (Array (ICST air r tok))
derive instance eqICST :: (Eq air, Eq r, Eq tok) => Eq (ICST air r tok)
derive instance functorICST :: Functor (ICST air r)
derive instance bifunctorICST :: Bifunctor (ICST air)

sourceICST :: forall air r tok. ICST air r tok -> Array (Either air tok)
sourceICST (ILeaf tok) = pure (Right tok)
sourceICST (IAir tok) = pure (Left tok)
sourceICST (IBranch _ children) = children >>= sourceICST

type SCST = CST (NonEmptyString /\ String) CodePoint

derive instance genericCST :: Generic (CST r tok) _
instance showCST :: (Show r, Show tok) => Show (CST r tok) where
  show x = genericShow x

data AST r = Layer r (Array (AST r))
type SAST = AST (NonEmptyString /\ String)

derive instance functorAST :: Functor AST
derive instance genericAST :: Generic (AST r) _
instance showAST :: (Show r) => Show (AST r) where
  show x = genericShow x

prune :: forall r tok. CST r tok -> Either tok (AST r)
prune (Leaf tok) = Left tok
prune (Branch r rec) = Right (Layer r (Array.mapMaybe (hush <<< prune) rec))

type StateIndex s = s -> Maybe Int
type SStateIndex = StateIndex Int

type SCTable = Proto.Table Int (NonEmptyString /\ String) CodePoint SCST
type SATable = Proto.Table Int (NonEmptyString /\ String) CodePoint (Either CodePoint SAST)

type SAStack = Proto.Stack Int (Either CodePoint SAST)
type SCStack = Proto.Stack Int SCST
type SAParseSteps = Proto.ParseSteps (NonEmptyString /\ String) CodePoint SAStack
type SCParseSteps = Proto.ParseSteps (NonEmptyString /\ String) CodePoint SCStack
