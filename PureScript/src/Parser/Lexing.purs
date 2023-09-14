module Parser.Lexing where

import Prelude

import Control.Alternative (guard)
import Control.Plus (empty)
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either, isLeft, note)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, for_, null, oneOfMap, traverse_)
import Data.List (List)
import Data.List as List
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.Foldable (maximum)
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.String.Regex (Regex)
import Data.String.Regex as Re
import Data.String.Regex as Regex
import Data.String.Regex.Flags (dotAll, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Parser.Algorithms (indexStates)
import Parser.Proto (Stack(..), statesOn, topOf)
import Parser.Types (CST(..), OrEOF(..), Part(..), ShiftReduce(..), State(..), StateInfo, States(..), Zipper(..), decide, decisionUnique)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

data Scanning i = Scanning Int i
derive instance functorScanning :: Functor Scanning
derive instance foldableScanning :: Foldable Scanning
derive instance traversableScanning :: Traversable Scanning

newtype Similar a b = Similar (Either a b)
infixr 9 type Similar as ~
derive newtype instance eqSimilar :: (Eq a, Eq b) => Eq (Similar a b)
derive newtype instance ordSimilar :: (Ord a, Ord b) => Ord (Similar a b)
derive newtype instance showSimilar :: (Show a, Show b) => Show (Similar a b)
derive instance newtypeSimilar :: Newtype (Similar a b) _

-- | Typeclass for generic length of things
-- |
-- | `len mempty == 0`
class Len i where
  len :: i -> Int

instance lenString :: Len String where
  len = String.length

instance lenArray :: Len (Array i) where
  len = Array.length

instance lenList :: Len (List i) where
  len = List.length

instance lenScanning :: Len i => Len (Scanning i) where
  len (Scanning idx i) = len i - idx

instance lenTuple :: (Len i1, Len i2) => Len (Tuple i1 i2) where
  len (Tuple i1 i2) = len i1 + len i2

instance lenOrEOF :: Len i => Len (OrEOF i) where
  len EOF = 0
  len (Continue i) = len i + 1

-- | Typeclass for recognizing tokens based on categories.
-- |
-- | Law: if you recognized it, you must unrecognize it, and vice versa.
class (Token cat o, Len i) <= Tokenize cat i o | cat -> o where
  recognize :: cat -> i -> Maybe (o /\ i)
  unrecognize :: cat -> o -> Maybe (i -> i)

class Token cat o | cat -> o where
  rerecognize :: cat -> o -> Boolean

instance tokenizeString :: Tokenize String String String where
  recognize prefix current =
    String.stripPrefix (String.Pattern prefix) current
      <#> Tuple prefix
  unrecognize prefix recognized =
    guard (prefix == recognized) $> (recognized <> _)

instance tokenString :: Token String String where
  rerecognize = eq

instance tokenizeChar :: Tokenize Char String Char where
  recognize c current = case CU.uncons current of
    Just { head, tail } | head == c -> pure (Tuple head tail)
    _ -> empty
  unrecognize prefix recognized =
    guard (prefix == recognized) $> (CU.singleton recognized <> _)

instance tokenChar :: Token Char Char where
  rerecognize = eq

instance tokenizeCodePoint :: Tokenize CodePoint String CodePoint where
  recognize c current = case String.uncons current of
    Just { head, tail } | head == c -> pure (Tuple head tail)
    _ -> empty
  unrecognize prefix recognized =
    guard (prefix == recognized) $> (String.singleton recognized <> _)

instance tokenCodePoint :: Token CodePoint CodePoint where
  rerecognize = eq

class ToString s where
  toString :: s -> String

instance toStringChar :: ToString Char where
  toString = CU.singleton

instance toStringCodePoint :: ToString CodePoint where
  toString = String.singleton

instance toStringString :: ToString String where
  toString = identity

class FromString s where
  fromString :: String -> s

instance fromStringString :: FromString String where
  fromString = identity


newtype Rawr = Rawr Regex
derive instance newtypeRawr :: Newtype Rawr _
derive newtype instance showRawr :: Show Rawr
instance eqRaw :: Eq Rawr where
  eq (Rawr r1) (Rawr r2) = show r1 == show r2
instance ordRaw :: Ord Rawr where
  compare (Rawr r1) (Rawr r2) = show r1 `compare` show r2
rawr :: String -> Rawr
rawr source = Rawr $ unsafeRegex ("^(?:" <> source <> ")") $ unicode <> dotAll
unRawr :: Rawr -> String
unRawr (Rawr re) = fromMaybe (Re.source re) do
  x <- String.stripPrefix (String.Pattern "^(?:") (Re.source re)
  y <- String.stripSuffix (String.Pattern ")") x
  pure y

-- TODO: this isn't right:
-- - doesn't handle lookaround since we dropped the start of the string
-- - I think the behavior of flags on the regex will do weird things
-- - fucking terrible API
-- - why does every high-level regex API suck ;.;
instance tokenizeRawr :: Tokenize Rawr String String where
  recognize (Rawr re) current = do
    groups <- Regex.match re current
    matched <- NEA.head groups
    pure (Tuple matched (CU.drop (CU.length matched) current))
  unrecognize re recognized =
    guard (recognize re recognized == Just (Tuple recognized mempty)) $>
      (recognized <> _)

instance tokenRawr :: Token Rawr String where
  rerecognize re recognized =
    recognize re recognized == Just (Tuple recognized mempty)

instance tokenizeOrEOF :: (Monoid i, Tokenize cat i o) => Tokenize (OrEOF cat) (OrEOF i) (OrEOF o) where
  recognize EOF (Continue i) | len i == 0 = Just (EOF /\ EOF)
  recognize (Continue cat) (Continue i) = bimap Continue Continue <$> recognize cat i
  recognize _ _ = Nothing
  unrecognize EOF EOF = Just (const (Continue mempty))
  unrecognize (Continue l) (Continue r) =
    unrecognize l r <#> case _, _ of
      p, Continue i -> Continue (p i)
      _, i -> i
  unrecognize _ _ = Nothing

instance tokenOrEOF :: (Token cat o) => Token (OrEOF cat) (OrEOF o) where
  rerecognize EOF EOF = true
  rerecognize (Continue l) (Continue r) = rerecognize l r
  rerecognize _ _ = false

-- else instance tokenizeOrEOFPeek :: (Monoid i, Tokenize cat i o) => Tokenize (OrEOF cat) i (OrEOF o) where
--   recognize EOF i | len i == 0 = Just (EOF /\ i)
--   recognize (Continue cat) i = lmap Continue <$> recognize cat i
--   recognize _ _ = Nothing
--   unrecognize EOF EOF = Just (const mempty)
--   unrecognize (Continue l) (Continue r) = unrecognize l r
--   unrecognize _ _ = Nothing

instance tokenizeEither ::
  ( Tokenize cat1 i o1
  , Tokenize cat2 i o2
  ) => Tokenize (cat1 \/ cat2) i (o1 \/ o2) where
    recognize (Left prefix) current =
      lmap Left <$> recognize prefix current
    recognize (Right prefix) current =
      lmap Right <$> recognize prefix current
    unrecognize (Left l) (Left r) = unrecognize l r
    unrecognize (Right l) (Right r) = unrecognize l r
    unrecognize _ _ = Nothing

instance tokenEither ::
  ( Token cat1 o1
  , Token cat2 o2
   ) => Token (cat1 \/ cat2) (o1 \/ o2) where
    rerecognize (Left l) (Left r) = rerecognize l r
    rerecognize (Right l) (Right r) = rerecognize l r
    rerecognize _ _ = false

instance tokenizeSimilar ::
  ( Tokenize cat1 i o
  , Tokenize cat2 i o
  ) => Tokenize (Similar cat1 cat2) i o where
    recognize (Similar e) = either recognize recognize e
    unrecognize (Similar e) = either unrecognize unrecognize e

instance tokenSimilar ::
  ( Token cat1 o
  , Token cat2 o
  ) => Token (Similar cat1 cat2) o where
    rerecognize (Similar e) = either rerecognize rerecognize e

instance tokenizeTuple ::
  ( Tokenize cat1 i1 o1
  , Tokenize cat2 i2 o2
  ) => Tokenize (cat1 /\ cat2) (i1 /\ i2) (o1 /\ o2) where
    recognize (cat1 /\ cat2) (i1 /\ i2) = ado
      o1 /\ j1 <- recognize cat1 i1
      o2 /\ j2 <- recognize cat2 i2
      in (o1 /\ o2) /\ (j1 /\ j2)
    unrecognize (l1 /\ l2) (r1 /\ r2) = ado
      p1 <- unrecognize l1 r1
      p2 <- unrecognize l2 r2
      in \(i1 /\ i2) -> p1 i1 /\ p2 i2

instance tokenTuple ::
  ( Token cat1 o1
  , Token cat2 o2
  ) => Token (cat1 /\ cat2) (o1 /\ o2) where
    rerecognize (l1 /\ l2) (r1 /\ r2) = rerecognize l1 r1 && rerecognize l2 r2


infixr 9 note as ?

thenNote :: forall f a b. Functor f => a -> f (Maybe b) -> f (Either a b)
thenNote = map <<< note

infixr 9 thenNote as ?!

whenFailed :: forall f a. Foldable f => f a -> Effect Unit -> f a
whenFailed a _ = a
whenFailed a b | null a = let _ = unsafePerformEffect b in a

infixr 9 whenFailed as ?>

asdf :: forall t670. t670 -> Effect Unit
asdf = log <<< unsafeCoerce

-- | Prioritize matches.
type Best s r cat i o =
  NonEmptyArray ((cat /\ ShiftReduce s r) /\ o /\ i) ->
  Maybe (Either s (cat /\ r) /\ o /\ i)

prioritize :: forall a score. Ord score => (a -> score) -> NonEmptyArray a -> NonEmptyArray a
prioritize f as =
  let
    scored = (Tuple <*> f) <$> as
    scores = snd <$> scored
    winner = maximum scores
    winners = fst <$> NEA.filter (snd >>> eq winner) scored
  in fromMaybe as $ NEA.fromArray winners

guessBest :: forall s r cat i o. Best s r cat i o
guessBest options = case NEA.toArray options of
  [ (cat /\ sr) /\ d ] -> do
    let
      _ = (guard (decisionUnique sr) :: Maybe Unit) ?> do
        asdf "Decision not unique"
        asdf sr
    guard (decisionUnique sr) *> decide sr <#> map (Tuple cat) >>> (_ /\ d)
  _ -> Nothing

longest :: forall s r cat i o. Len i => Best s r cat i o
longest = guessBest <<< prioritize \(_ /\ _ /\ i) -> negate (len i)

bestRegexOrString :: forall s r. Best s r (OrEOF (Similar String Rawr)) (OrEOF String) (OrEOF String)
bestRegexOrString options = options # guessBest <<< prioritize case _ of
  ((Continue (Similar cat) /\ _) /\ _ /\ Continue i) -> Just $ Tuple (isLeft cat) (negate (len i))
  _ -> Nothing

lexingParse ::
  forall s nt r cat i o.
    Ord s =>
    Ord nt =>
    Eq r =>
    Ord cat =>
    Tokenize cat i o =>
  { best :: Best s (nt /\ r) cat i o
  } ->
  s /\ States s nt r cat -> i ->
  ((Stack s (CST (nt /\ r) o)) /\ String) \/ (Stack s (CST (nt /\ r) o))
lexingParse { best } (initialState /\ States states) initialInput =
  go (Zero initialState) (Left initialInput)
  where
  tabulated = indexStates (States states)
  lookupState s = tabulated s >>= Array.index states
  lookupAction :: Stack s (CST (nt /\ r) o) -> StateInfo s nt r cat -> Either i (cat /\ o /\ i) -> Either String _
  lookupAction stack info@{ advance: SemigroupMap m } = case _ of
    Left input -> do
      -- traceM $ [ unsafeCoerce input ] <> Array.fromFoldable (Map.keys m)
      let actions = NEA.fromArray $ Array.mapMaybe (matchCat input) $ Map.toUnfoldable m
      possibilities <- "No action"? actions ?> do
        asdf input
        asdf "items"
        traverse_ asdf (unwrap info.items)
        let rules = Array.fromFoldable (statesOn stack) # Array.mapMaybe lookupState
        asdf "rules"
        for_ rules $ asdf <<< do
          _.items >>> unwrap >>> map do
            _.rule >>> \(Zipper l r) -> fold
              [ map (unsafeCoerce _.value0 >>> _.value0) l
              , [ "•" ]
              , map (unsafeCoerce _.value0 >>> _.value0) r
              ]
        asdf "advance"
        asdf $ Array.fromFoldable $ Map.keys m
      "No best action"? best possibilities ?> do
        asdf "possibilities"
        asdf possibilities
        let (cat /\ sr) /\ o /\ i = NEA.head possibilities
        case sr of
          Shift s -> do
            asdf "rules to just shift to?"
            for_ (lookupState s) $ asdf <<< do
              _.items >>> unwrap >>> map do
                _.rule >>> \(Zipper l r) -> fold
                  [ map (unsafeCoerce _.value0 >>> _.value0) l
                  , [ "•" ]
                  , map (unsafeCoerce _.value0 >>> _.value0) r
                  ]
          Reduces rs -> do
            asdf "rules to reduce to?"
            asdf rs
          ShiftReduces s rs -> do
            asdf "rules to shift to?"
            for_ (lookupState s) $ asdf <<< do
              _.items >>> unwrap >>> map do
                _.rule >>> \(Zipper l r) -> fold
                  [ map (unsafeCoerce _.value0 >>> _.value0) l
                  , [ "•" ]
                  , map (unsafeCoerce _.value0 >>> _.value0) r
                  ]
            asdf "rules to reduce to?"
            asdf rs
    Right (cat /\ o /\ i) -> do
      sr <- "No repeat action"? Map.lookup cat m ?> do
        asdf cat
        asdf $ Array.fromFoldable $ Map.keys m
      "No best repeat action"? best (NEA.singleton $ (cat /\ sr) /\ o /\ i) ?> do
        case sr of
          Shift s -> do
            asdf "rules to just shift to?"
            for_ (lookupState s) $ asdf <<< do
              _.items >>> unwrap >>> map do
                _.rule >>> \(Zipper l r) -> fold
                  [ map (unsafeCoerce _.value0 >>> _.value0) l
                  , [ "•" ]
                  , map (unsafeCoerce _.value0 >>> _.value0) r
                  ]
          Reduces rs -> do
            asdf "rules to reduce to?"
            asdf rs
          ShiftReduces s rs -> do
            asdf "rules to shift to?"
            for_ (lookupState s) $ asdf <<< do
              _.items >>> unwrap >>> map do
                _.rule >>> \(Zipper l r) -> fold
                  [ map (unsafeCoerce _.value0 >>> _.value0) l
                  , [ "•" ]
                  , map (unsafeCoerce _.value0 >>> _.value0) r
                  ]
            asdf "rules to reduce to?"
            asdf rs
        let rules = Array.fromFoldable (statesOn stack) # Array.mapMaybe lookupState
        asdf "rules"
        for_ rules $ asdf <<< do
          _.items >>> unwrap >>> map do
            _.rule >>> \(Zipper l r) -> fold
              [ map (unsafeCoerce _.value0 >>> _.value0) l
              , [ "•" ]
              , map (unsafeCoerce _.value0 >>> _.value0) r
              ]
  matchCat input (cat /\ act) =
    recognize cat input <#> ((cat /\ act) /\ _)
  go :: Stack s (CST (nt /\ r) o) -> Either i (cat /\ o /\ i) -> ((Stack s (CST (nt /\ r) o)) /\ String) \/ (Stack s (CST (nt /\ r) o))
  go stack (Left input) | len input == 0 = pure stack
  go stack input = do
    state <- Tuple stack "Missing state"? lookupState (topOf stack)
    act <- lmap (Tuple stack) $ lookupAction stack state input
    case act of
      Left s /\ o /\ i -> do
        go (Snoc stack (Leaf o) s) (Left i)
      Right (cat /\ r) /\ o /\ i -> do
        reduction <- Tuple stack "Unknown reduction"? lookupReduction r state
        stacked <- Tuple stack "Failed"? takeStack r stack reduction
        go stacked (Right (cat /\ o /\ i))
  lookupReduction (p /\ r) { items: State items } = items # oneOfMap case _ of
    { rule: Zipper parsed [], pName, rName } | pName == p && rName == r ->
      Just parsed
    _ -> Nothing
  takeStack r stack0 parsed =
    let
      take1 (taken /\ stack) = case _ of
        Terminal _ -> case stack of
          Snoc stack' v@(Leaf _) _ ->
            Just $ ([ v ] <> taken) /\ stack'
          _ -> unsafeCrashWith "expected token on stack"
        NonTerminal nt -> case stack of
          Snoc stack' v@(Branch (p /\ _) _) _ | p == nt ->
            Just $ ([ v ] <> taken) /\ stack'
          _ -> unsafeCrashWith "expected terminal on stack"
    in
      Array.foldM take1 ([] /\ stack0) (Array.reverse parsed) >>= \(taken /\ stack) ->
        Snoc stack (Branch r taken) <$> goto r (topOf stack)
  goto (p /\ _) s' = lookupState s' >>= _.receive >>> Map.lookup p
