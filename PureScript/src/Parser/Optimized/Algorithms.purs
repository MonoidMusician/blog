module Parser.Optimized.Algorithms where

import Prelude

import Control.Comonad (extract)
import Control.Monad.State (gets)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity)
import Data.Lens (Lens', use, (%=), (+=), (.=))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
-- import Debug (spy, spyWith, traceM)
import Idiolect ((>==))
import Parser.Optimized.Types (Grammar(..), Lookahead, State, StateInfo, StateItem, States(..), fromItem, fromItem', fromOld, toItems, toOld, toOldState)
import Parser.Types (Fragment, OrEOF(..), Part(..), ShiftReduce(..), Zipper(..), isNonTerminal, unNonTerminal, unTerminal)
import Parser.Types as Old
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Uncurried.StateT (StateT, runStateT)

type Augmenteds i nt tok =
  { augmented :: Grammar nt tok
  , eof :: tok
  , starts :: Array (i /\ StateItem nt tok)
  }

fromSeeds ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  Array nt ->
  Augmenteds nt (Either nt nt) (OrEOF tok)
fromSeeds (MkGrammar rules) entries =
  let
    entryRules = Map.fromFoldable $ entries <#> \pName ->
      Left pName /\ [ Just [ NonTerminal (Right pName), Terminal EOF ] ]
    rules' = Map.fromFoldable $ rules # mapWithIndex \pName mrules ->
      Right pName /\ (map (map (bimap Right Continue)) <$> mrules)
  in
    { augmented: MkGrammar (Map.union entryRules rules')
    , eof: EOF
    , starts: entries <#> \pName -> pName /\
      { pName: Left pName
      , rName: Nothing
      , rule: Zipper [] [ NonTerminal (Right pName), Terminal EOF ]
      , lookahead: Set.empty
      }
    }

statesNumberedMany ::
  forall f nt tok.
    Foldable f =>
    Ord nt =>
    Ord tok =>
  Old.Grammar nt Int tok ->
  f nt ->
  Map nt Int /\
    Old.States Int (Either nt nt) (Maybe Int) (OrEOF tok)
statesNumberedMany initial entrief =
  let
    entries = Array.nub $ Array.fromFoldable entrief
    { augmented: grammar, starts } = fromSeeds (fromOld initial) entries
  in map (toOld grammar) $ closeStates grammar $ map fromItem <$> starts

getReduction :: forall nt tok. Ord tok => StateItem nt tok -> SemigroupMap tok (Fragment nt tok /\ nt /\ Maybe Int)
getReduction { pName, rName, rule: Zipper rule [], lookahead } =
  SemigroupMap $ Set.toMap lookahead $> (rule /\ pName /\ rName)
getReduction _ = SemigroupMap $ Map.empty

getReductions :: forall nt tok. Ord nt => Ord tok => Grammar nt tok -> State nt tok -> SemigroupMap tok (NonEmptyArray (Fragment nt tok /\ nt /\ Maybe Int))
getReductions grammar = toItems grammar \item ->
  NEA.singleton <$> getReduction item



findNT ::
  forall nt tok.
    Zipper nt tok ->
    Maybe
      { nonterminal :: nt
      , following :: Array nt
      , continue :: Maybe tok
      }
findNT (Zipper _ after) = Array.uncons after >>= case _ of
  { head: NonTerminal nt, tail } ->
    let
      { following, continue } = preview tail
    in
      Just { nonterminal: nt, following, continue }
  _ -> Nothing

preview ::
  forall nt tok.
    Array (Part nt tok) ->
    { following :: Array nt, continue :: Maybe tok }
preview tail = { following, continue }
  where
  { init, rest } = Array.span isNonTerminal tail
  following = Array.mapMaybe unNonTerminal init
  continue = Array.head rest >>= unTerminal

continueOn :: forall tok. Maybe tok -> Lookahead tok -> Lookahead tok
continueOn continue lookahead = case continue of
  Just tok -> Set.singleton tok
  Nothing -> lookahead

startRules :: forall nt tok. Ord nt => Ord tok => Grammar nt tok -> nt -> (Lookahead tok -> State nt tok)
startRules (MkGrammar grammar) p
  | Just mrules <- Map.lookup p grammar =
    \lookahead -> foldMap fromItem' $ Array.catMaybes $ mrules # mapWithIndex \i mrule ->
        mrule $> { pName: p, rName: Just i, rule: 0, lookahead }
startRules _ _ = unsafeCrashWith "startRules"

closeItem :: forall nt tok. Ord nt => Ord tok => Grammar nt tok -> Zipper nt tok -> Lookahead tok -> State nt tok
closeItem grammar rule lookahead = case findNT rule of
  Nothing -> mempty
  Just { nonterminal: p, following, continue } ->
    startRules grammar p $ firsts grammar following (continueOn continue lookahead)

close1 :: forall nt tok. Ord nt => Ord tok => Grammar nt tok -> State nt tok -> State nt tok
close1 grammar = toItems grammar
  \{ rule, lookahead } ->
    closeItem grammar rule lookahead

close ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  State nt tok ->
  State nt tok
close = closeSemilattice close1

firsts :: forall nt tok. Ord nt => Ord tok => Grammar nt tok -> Array nt -> Lookahead tok -> Lookahead tok
firsts (MkGrammar rules0) ps0 lookahead0 = readyset rules0 ps0 lookahead0
  where
  readyset rules ps lookahead = case Array.uncons ps of
    Just { head, tail } -> go rules head tail lookahead
    _ -> lookahead
  go rules p ps lookahead = case Map.lookup p rules of
    Nothing -> mempty
    Just matches -> matches #
      let rules' = Map.delete p rules in
      foldMap $ foldMap $ preview >>> \{ following, continue } ->
        continueOn continue lookahead #
          case continue of
            Just _ -> readyset rules' following
            Nothing -> readyset rules' (following <> ps)

nextSteps ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  State nt tok ->
  { terminal :: SemigroupMap tok (State nt tok)
  , nonTerminal :: SemigroupMap nt (State nt tok)
  }
nextSteps grammar = toItems grammar
  \{ pName, rName, rule: Zipper before after, lookahead } ->
    case Array.head after of
      Nothing -> mempty
      Just (Terminal tok) ->
        { nonTerminal: mempty, terminal: SemigroupMap $ Map.singleton tok $ fromItem'
          { pName, rName, rule: Array.length before + 1, lookahead }
        }
      Just (NonTerminal nt) ->
        { terminal: mempty, nonTerminal: SemigroupMap $ Map.singleton nt $ fromItem'
          { pName, rName, rule: Array.length before + 1, lookahead }
        }

closeStates ::
  forall k nt tok.
    Ord k =>
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  Array (k /\ State nt tok) ->
  Map k Int /\ States nt tok
closeStates grammar initial = Tuple entrypoints $
  runStateT initialState act
    # extract # extract # un KnownStates
    -- # spyWith "metrics" _.metrics
    # _.finalized
    # tally # States
  where
  initialNumbered = mapWithIndex (flip Tuple) $ map (close grammar) <$> initial
  entrypoints = Map.fromFoldable $
    initialNumbered <#> lmap fst
  act = closeStatesLoop $ Array.toUnfoldable $
    initialNumbered <#> lmap snd
  initialState = KnownStates
    { singles: Map.empty
    , seeded: Map.empty
    , known: Map.fromFoldable $ initialNumbered <#> lmap snd
    , finalized: Map.empty
    , grammar
    , metrics: zero
    , missed: List.Nil
    }
  tally = Map.toUnfoldable >>> mapWithIndex \i (Tuple i' s) ->
    if i == i' then s else
    unsafeCrashWith "internal error: closeStates: index mismatch"

closeStatesLoop ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  List (KnownState nt tok) ->
  Knowing nt tok Unit
closeStatesLoop newly = do
  next <- closeStates1 newly
  case next of
    List.Nil -> pure unit
    _ -> closeStatesLoop next

closeStates1 ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  List (KnownState nt tok) ->
  Knowing nt tok (List (KnownState nt tok))
closeStates1 newly = do
  _m_rounds += 1
  rounds <- use _m_rounds
  -- when (mod rounds 5 == 0) do
  --   metrics <- gets (unwrap >>> _.metrics)
  --   spyWith "metrics" (const metrics) (pure unit)
  --   spyWith "front" (const (List.length newly)) (pure unit)
  grammar <- use _grammar
  -- These states will be finalized, as the new states will already be numbered
  for_ newly \(Tuple st si) -> do
    let
      next = nextSteps grammar st
      reductions = Reduces <$> getReductions grammar st
    shifts <- for next.terminal $ seedState >== snd >>> Shift
    receive <- for (unwrap next.nonTerminal) $ seedState >== snd
    _finalized %= Map.insert si
      { items: st
      , advance: shifts <> reductions
      , receive: receive
      }
  missed <- use _missed
  _missed .= List.Nil
  pure missed


closeSemilattice :: forall i m. Eq m => Monoid m => (i -> m -> m) -> i -> m -> m
closeSemilattice f = join <<< closeSemilattice2 <<< f

closeSemilattice2 :: forall m. Eq m => Monoid m => (m -> m) -> m -> m -> m
closeSemilattice2 f acc step =
  let new = f step in if new == mempty then acc else
  let next = acc <> new in if acc == next then acc else
  closeSemilattice2 f next new



type KnownState nt tok = Tuple (State nt tok) Int
data MaybeKnownState nt tok = MKS (State nt tok) (Maybe Int)
instance semigroupMaybeKnownState :: (Ord nt, Ord tok) => Semigroup (MaybeKnownState nt tok) where
  append l@(MKS s1 si1) r@(MKS s2 si2)
    | isJust si1, si1 == si2 = l
    | otherwise =
      let
        s3 = s1 <> s2
      in
        if s3 == s1 then l else
        if s3 == s2 then r else
        MKS s3 Nothing
newtype KnownStates nt tok = KnownStates
  { singles :: Map (State nt tok) (MaybeKnownState nt tok)
  , seeded :: Map (State nt tok) (KnownState nt tok)
  , known :: Map (State nt tok) Int
  , finalized :: Map Int (StateInfo nt tok)
  , grammar :: Grammar nt tok
  , metrics :: Record Metrics
  , missed :: List (KnownState nt tok)
  }
type Metrics =
  ( known_hit :: Int
  , known_miss :: Int
  , seeded_hit :: Int
  , seeded_miss :: Int
  , rounds :: Int
  )
derive instance newtypeKnownStates :: Newtype (KnownStates nt tok) _
_finalized :: forall nt tok. Lens' (KnownStates nt tok) (Map Int (StateInfo nt tok))
_finalized = _Newtype <<< prop (Proxy :: Proxy "finalized")
_known :: forall nt tok. Lens' (KnownStates nt tok) (Map (State nt tok) Int)
_known = _Newtype <<< prop (Proxy :: Proxy "known")
_seeded :: forall nt tok. Lens' (KnownStates nt tok) (Map (State nt tok) (KnownState nt tok))
_seeded = _Newtype <<< prop (Proxy :: Proxy "seeded")
_singles :: forall nt tok. Lens' (KnownStates nt tok) (Map (State nt tok) (MaybeKnownState nt tok))
_singles = _Newtype <<< prop (Proxy :: Proxy "singles")
_grammar :: forall nt tok. Lens' (KnownStates nt tok) (Grammar nt tok)
_grammar = _Newtype <<< prop (Proxy :: Proxy "grammar")
_missed :: forall nt tok. Lens' (KnownStates nt tok) (List (KnownState nt tok))
_missed = _Newtype <<< prop (Proxy :: Proxy "missed")

bump metric = metric += 1
touch metric sz = metric += Tuple 1 sz

_m :: forall nt tok s t r'. IsSymbol s => Row.Cons s t r' Metrics => Proxy s -> Lens' (KnownStates nt tok) t
_m p = _Newtype <<< prop (Proxy :: Proxy "metrics") <<< prop p

_m_hit_known :: forall nt tok. Lens' (KnownStates nt tok) Int
_m_hit_known = _m (Proxy :: Proxy "known_hit")

_m_miss_known :: forall nt tok. Lens' (KnownStates nt tok) Int
_m_miss_known = _m (Proxy :: Proxy "known_miss")

_m_hit_seeded :: forall nt tok. Lens' (KnownStates nt tok) Int
_m_hit_seeded = _m (Proxy :: Proxy "seeded_hit")

_m_miss_seeded :: forall nt tok. Lens' (KnownStates nt tok) Int
_m_miss_seeded = _m (Proxy :: Proxy "seeded_miss")

_m_rounds :: forall nt tok. Lens' (KnownStates nt tok) Int
_m_rounds = _m (Proxy :: Proxy "rounds")

type Knowing nt tok = StateT (KnownStates nt tok) Identity

addCompletedState :: forall nt tok. Ord nt => Ord tok => State nt tok -> Knowing nt tok Int
addCompletedState st = do
  known <- use _known
  case Map.lookup st known of
    Just si -> do
      bump _m_hit_known
      pure si
    Nothing -> do
      bump _m_miss_known
      let si = Map.size known
      _known %= Map.insert st si
      _missed %= List.Cons (Tuple st si)
      pure si

seedState :: forall nt tok. Ord nt => Ord tok => State nt tok -> Knowing nt tok (KnownState nt tok)
seedState seed = do
  seeded <- use _seeded
  case Map.lookup seed seeded of
    Just ks -> do
      bump _m_hit_seeded
      pure ks
    Nothing -> do
      bump _m_miss_seeded
      grammar <- use _grammar
      let st = close grammar seed
      si <- addCompletedState st
      let ks = Tuple st si
      _seeded %= Map.insert seed ks
      pure ks
