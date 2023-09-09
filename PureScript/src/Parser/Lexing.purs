module Parser.Lexing where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, oneOfMap)
import Data.List (List)
import Data.List as List
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Debug (traceM)
import Parser.Algorithms (indexStates)
import Parser.Proto (Stack(..), topOf)
import Parser.Types (CST(..), OrEOF(..), Part(..), ShiftReduce, State(..), StateInfo, States(..), Zipper(..), decide)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

data Scanning i = Scanning Int i
derive instance functorScanning :: Functor Scanning
derive instance foldableScanning :: Foldable Scanning
derive instance traversableScanning :: Traversable Scanning

-- | Typeclass for generic length of things
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
class Len i <= Token cat i o | cat -> o where
  recognize :: cat -> i -> Maybe (o /\ i)

instance tokenString :: Token String String String where
  recognize prefix current =
    String.stripPrefix (String.Pattern prefix) current
      <#> Tuple prefix

instance tokenChar :: Token Char String Char where
  recognize c current = case CU.uncons current of
    Just { head, tail } | head == c -> pure (Tuple head tail)
    _ -> empty

instance tokenCodePoint :: Token CodePoint String CodePoint where
  recognize c current = case String.uncons current of
    Just { head, tail } | head == c -> pure (Tuple head tail)
    _ -> empty

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


-- TODO: this isn't right:
-- - doesn't handle lookaround since we dropped the start of the string
-- - I think the behavior of flags on the regex will do weird things
-- - fucking terrible API
-- - why does every high-level regex API suck ;.;
instance tokenRegex :: Token Regex String String where
  recognize re current = do
    groups <- Regex.match re current
    matched <- NEA.head groups
    pure (Tuple matched (CU.drop (CU.length matched) current))

instance tokenOrEOF :: Token cat i o => Token (OrEOF cat) (OrEOF i) (OrEOF o) where
  recognize EOF (Continue i) | len i == 0 = Just (EOF /\ EOF)
  recognize (Continue cat) (Continue i) = bimap Continue Continue <$> recognize cat i
  recognize _ _ = Nothing

instance tokenEither ::
  ( Token cat1 i o1
  , Token cat2 i o2
  ) => Token (cat1 \/ cat2) i (o1 \/ o2) where
    recognize (Left prefix) current =
      lmap Left <$> recognize prefix current
    recognize (Right prefix) current =
      lmap Right <$> recognize prefix current

instance tokenTuple ::
  ( Token cat1 i1 o1
  , Token cat2 i2 o2
  ) => Token (cat1 /\ cat2) (i1 /\ i2) (o1 /\ o2) where
    recognize (cat1 /\ cat2) (i1 /\ i2) = ado
      o1 /\ j1 <- recognize cat1 i1
      o2 /\ j2 <- recognize cat2 i2
      in (o1 /\ o2) /\ (j1 /\ j2)



infixr 9 note as ?

thenNote :: forall f a b. Functor f => a -> f (Maybe b) -> f (Either a b)
thenNote = map <<< note

infixr 9 thenNote as ?!


-- | Prioritize matches.
type Best s r cat i o =
  NonEmptyArray ((cat /\ ShiftReduce s r) /\ o /\ i) ->
  Maybe (Either s (cat /\ r) /\ o /\ i)

guessBest :: forall s r cat i o. Best s r cat i o
guessBest opts = case NEA.toArray opts of
  [ (cat /\ sr) /\ d ] -> decide sr <#> map (Tuple cat) >>> (_ /\ d)
  _ -> Nothing

lexingParse ::
  forall s nt r cat i o.
    Ord s =>
    Ord nt =>
    Eq r =>
    Ord cat =>
    Token cat i o =>
  { best :: Best s (nt /\ r) cat i o
  } ->
  s /\ States s nt r cat -> i ->
  ((Stack s (CST (nt /\ r) o)) /\ String) \/ (Stack s (CST (nt /\ r) o))
lexingParse { best } (initialState /\ States states) initialInput =
  go (Zero initialState) (Left initialInput)
  where
  tabulated = indexStates (States states)
  lookupState s = tabulated s >>= Array.index states
  lookupAction :: StateInfo s nt r cat -> Either i (cat /\ o /\ i) -> Either String _
  lookupAction { advance: SemigroupMap m } = case _ of
    Left input -> do
      -- traceM $ [ unsafeCoerce input ] <> Array.fromFoldable (Map.keys m)
      possibilities <- "No action"?! NEA.fromArray $ Array.mapMaybe (matchCat input) $ Map.toUnfoldable m
      "No best action"?! best $ possibilities
    Right (cat /\ o /\ i) -> do
      act1 <- "No repeat action"? Map.lookup cat m
      "No best repeat action"?! best $ NEA.singleton $ (cat /\ act1) /\ o /\ i
  matchCat input (cat /\ act) =
    recognize cat input <#> ((cat /\ act) /\ _)
  go :: Stack s (CST (nt /\ r) o) -> Either i (cat /\ o /\ i) -> ((Stack s (CST (nt /\ r) o)) /\ String) \/ (Stack s (CST (nt /\ r) o))
  go stack (Left input) | len input == 0 = pure stack
  go stack input = do
    state <- Tuple stack "Missing state"? lookupState (topOf stack)
    act <- lmap (Tuple stack) $ lookupAction state input
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
