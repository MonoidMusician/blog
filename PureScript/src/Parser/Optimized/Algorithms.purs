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
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Semigroup.First (First(..))
import Data.Symbol (class IsSymbol)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Idiolect ((>==))
import Parser.Optimized.Types (Grammar(..), Lookahead, State, StateInfo, StateItem, States(..), Reduction, fromItem, fromItem', fromOld, toItems, toOld, toOldState)
import Parser.Types (Fragment, OrEOF(..), Part(..), ShiftReduce(..), Zipper(..), isNonTerminal, noInterTerminal, unNonTerminal, unTerminal)
import Parser.Types as Old
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Uncurried.StateT (StateT, runStateT)

type Augmenteds i meta nt tok =
  { augmented :: Grammar meta nt tok
  , eof :: tok
  , starts :: Array (i /\ StateItem meta nt tok)
  }

fromSeeds ::
  forall meta nt tok.
    Semiring meta =>
    Ord meta =>
    Ord nt =>
    Ord tok =>
  Grammar meta nt tok ->
  Array nt ->
  Augmenteds nt meta (Either nt nt) (OrEOF tok)
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
      , lookbehind: Additive (one :: meta)
      , lookahead: mempty
      }
    }

statesNumberedMany ::
  forall f meta nt tok.
    Foldable f =>
    Semiring meta =>
    Ord meta =>
    Ord nt =>
    Ord tok =>
  Old.Grammar meta nt Int tok ->
  f nt ->
  Map nt Int /\
    Old.States Int meta (Either nt nt) (Maybe Int) (OrEOF tok)
statesNumberedMany initial entrief =
  let
    entries = Array.nub $ Array.fromFoldable entrief
    { augmented: grammar, starts } = fromSeeds (fromOld initial) entries
  in map (toOld grammar) $ closeStates grammar $ map fromItem <$> starts

getReduction :: forall meta nt tok. Semiring meta => Ord tok => StateItem meta nt tok -> SemigroupMap tok (Additive meta /\ Reduction meta nt tok)
getReduction { pName, rName, rule: Zipper rule [], lookbehind: Additive lookbehind, lookahead } =
  lookahead <#> unwrap >>> (Additive <<< if Array.null rule then (lookbehind * _) else identity) >>> (_ /\ (rule /\ pName /\ rName))
getReduction _ = SemigroupMap $ Map.empty

getReductions :: forall meta nt tok. Semiring meta => Ord nt => Ord tok => Grammar meta nt tok -> State meta nt tok -> SemigroupMap tok (Additive meta /\ NonEmptyArray (Reduction meta nt tok))
getReductions grammar = toItems grammar \item ->
  map NEA.singleton <$> getReduction item



findNT ::
  forall meta nt tok.
    Semiring meta =>
  Zipper meta nt tok ->
  Maybe
    { interterminal :: meta
    , nonterminal :: nt
    , following :: List (Tuple meta nt)
    , continue :: Tuple meta (Maybe tok)
    }
findNT (Zipper _ after0) = go (one :: meta) (Array.toUnfoldable after0)
  where
  go interterminal (NonTerminal nt : tail) =
    let
      { following, continue } = preview tail
    in
      Just { interterminal, nonterminal: nt, following, continue }
  go interterm (InterTerminal meta : tail) =
    go (interterm * meta) tail
  go _ _ = Nothing

preview ::
  forall meta nt tok.
    Semiring meta =>
  List (Part meta nt tok) ->
  { following :: List (Tuple meta nt), continue :: Tuple meta (Maybe tok) }
preview = scanFollowing Nil (one :: meta)
  where
  scanFollowing acc interterm (InterTerminal meta : tail) =
    scanFollowing acc (interterm * meta) tail
  scanFollowing acc interterm (NonTerminal nt : tail) =
    scanFollowing (Tuple interterm nt : acc) (one :: meta) tail
  scanFollowing acc interterm (Terminal tok : _done) =
    { following: List.reverse acc, continue: Tuple interterm (Just tok) }
  scanFollowing acc interterm Nil =
    { following: List.reverse acc, continue: Tuple interterm Nothing }

continueOn :: forall meta tok. Semiring meta => Ord meta => Tuple meta (Maybe tok) -> Lookahead meta tok -> Lookahead meta tok
continueOn (Tuple meta continue) lookahead = case continue of
  Just tok -> SemigroupMap $ Map.singleton tok $ Additive meta
  Nothing -> coerce (meta * _) <$> lookahead

startRules :: forall meta nt tok. Semiring meta => Ord meta => Ord nt => Ord tok => Grammar meta nt tok -> nt -> (Additive meta -> Lookahead meta tok -> State meta nt tok)
startRules (MkGrammar grammar) p
  | Just mrules <- Map.lookup p grammar =
    \lookbehind lookahead -> foldMap fromItem' $ Array.catMaybes $ mrules # mapWithIndex \i mrule ->
        mrule $> { pName: p, rName: Just i, rule: 0, lookbehind, lookahead }
startRules _ _ = unsafeCrashWith "startRules"

closeItem :: forall meta nt tok. Semiring meta => Ord meta => Ord nt => Ord tok => Grammar meta nt tok -> Zipper meta nt tok -> Lookahead meta tok -> State meta nt tok
closeItem grammar rule lookahead = case findNT rule of
  Nothing -> mempty
  Just { interterminal, nonterminal: p, following, continue } ->
    startRules grammar p (Additive interterminal) $ firsts grammar following (continueOn continue lookahead)

close1 :: forall meta nt tok. Semiring meta => Ord meta => Ord nt => Ord tok => Grammar meta nt tok -> State meta nt tok -> State meta nt tok
close1 grammar = toItems grammar
  \{ rule, lookahead } ->
    closeItem grammar rule lookahead

close ::
  forall meta nt tok.
    Semiring meta =>
    Ord meta =>
    Ord nt =>
    Ord tok =>
  Grammar meta nt tok ->
  State meta nt tok ->
  State meta nt tok
close grammar = closeSemilattice (close1 grammar)

firsts :: forall meta nt tok. Semiring meta => Ord meta => Ord nt => Ord tok => Grammar meta nt tok -> List (Tuple meta nt) -> Lookahead meta tok -> Lookahead meta tok
firsts (MkGrammar rules0) ps0 lookahead0 = readyset rules0 (one :: meta) ps0 lookahead0
  where
  readyset rules meta ps lookahead = case ps of
    Tuple meta' head : tail -> go rules (meta * meta') head tail lookahead
    Nil -> coerce (meta * _) <$> lookahead
  go rules meta p ps lookahead = case Map.lookup p rules of
    Nothing -> mempty
    Just matches -> matches #
      let rules' = Map.delete p rules in
      foldMap $ foldMap $ Array.toUnfoldable >>> preview >>> \{ following, continue } ->
        let
          continue' = continueOn continue lookahead
          following'
            | isJust (snd continue) = following
            | otherwise = following <> ps
        in readyset rules' meta following' continue'

nextSteps ::
  forall meta nt tok.
    Semiring meta =>
    Ord meta =>
    Ord nt =>
    Ord tok =>
  Grammar meta nt tok ->
  State meta nt tok ->
  { terminal :: SemigroupMap tok (Additive meta /\ State meta nt tok)
  , nonTerminal :: SemigroupMap nt (Additive meta /\ State meta nt tok)
  }
nextSteps grammar = toItems grammar
  \{ pName, rName, rule: Zipper before after, lookbehind, lookahead } ->
    let
      go acc scanning = case Array.uncons scanning of
        Nothing ->
          { nonTerminal: SemigroupMap Map.empty
          , terminal: SemigroupMap Map.empty
          }
        Just { head, tail } ->
          let
            nextStateSeed = fromItem'
              { pName, rName, rule: Array.length before + 1, lookbehind, lookahead }
          in
            case head of
              NonTerminal nt ->
                { nonTerminal: SemigroupMap (Map.singleton nt (Additive acc /\ nextStateSeed))
                , terminal: SemigroupMap Map.empty
                }
              Terminal tok ->
                { nonTerminal: SemigroupMap Map.empty
                , terminal: SemigroupMap (Map.singleton tok (Additive acc /\ nextStateSeed))
                }
              InterTerminal meta ->
                go (acc * meta) tail
    in go (one :: meta) after

closeStates ::
  forall k meta nt tok.
    Semiring meta =>
    Ord k =>
    Ord meta =>
    Ord nt =>
    Ord tok =>
  Grammar meta nt tok ->
  Array (k /\ State meta nt tok) ->
  Map k Int /\ States meta nt tok
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
    { seeded: Map.empty
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
  forall meta nt tok.
    Semiring meta =>
    Ord meta =>
    Ord nt =>
    Ord tok =>
  List (KnownState meta nt tok) ->
  Knowing meta nt tok Unit
closeStatesLoop newly = do
  next <- closeStates1 newly
  case next of
    List.Nil -> pure unit
    _ -> closeStatesLoop next

closeStates1 ::
  forall meta nt tok.
    Semiring meta =>
    Ord meta =>
    Ord nt =>
    Ord tok =>
  List (KnownState meta nt tok) ->
  Knowing meta nt tok (List (KnownState meta nt tok))
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
      reductions = map Reduces <$> getReductions grammar st
    shifts <- for next.terminal $ traverse $ seedState >== snd >>> Shift
    receive <- for (unwrap next.nonTerminal) $ snd >>> seedState >== snd
    _finalized %= Map.insert si
      { items: st
      , advance: shifts <> reductions
      , receive: receive
      }
  missed <- use _missed
  _missed .= List.Nil
  pure missed


closeSemilattice :: forall i m. Eq m => Monoid m => (m -> m) -> m -> m
closeSemilattice f = join $ closeSemilattice2 f

closeSemilattice2 :: forall m. Eq m => Monoid m => (m -> m) -> m -> m -> m
closeSemilattice2 f acc step =
  let new = f step in if new == mempty then acc else
  let next = acc <> new in if acc == next then acc else
  closeSemilattice2 f next new



type KnownState meta nt tok = Tuple (State meta nt tok) Int
newtype KnownStates meta nt tok = KnownStates
  { seeded :: Map (State meta nt tok) (KnownState meta nt tok)
  , known :: Map (State meta nt tok) Int
  , finalized :: Map Int (StateInfo meta nt tok)
  , grammar :: Grammar meta nt tok
  , metrics :: Record Metrics
  , missed :: List (KnownState meta nt tok)
  }
type Metrics =
  ( known_hit :: Int
  , known_miss :: Int
  , seeded_hit :: Int
  , seeded_miss :: Int
  , rounds :: Int
  )
derive instance newtypeKnownStates :: Newtype (KnownStates meta nt tok) _
_finalized :: forall meta nt tok. Lens' (KnownStates meta nt tok) (Map Int (StateInfo meta nt tok))
_finalized = _Newtype <<< prop (Proxy :: Proxy "finalized")
_known :: forall meta nt tok. Lens' (KnownStates meta nt tok) (Map (State meta nt tok) Int)
_known = _Newtype <<< prop (Proxy :: Proxy "known")
_seeded :: forall meta nt tok. Lens' (KnownStates meta nt tok) (Map (State meta nt tok) (KnownState meta nt tok))
_seeded = _Newtype <<< prop (Proxy :: Proxy "seeded")
_grammar :: forall meta nt tok. Lens' (KnownStates meta nt tok) (Grammar meta nt tok)
_grammar = _Newtype <<< prop (Proxy :: Proxy "grammar")
_missed :: forall meta nt tok. Lens' (KnownStates meta nt tok) (List (KnownState meta nt tok))
_missed = _Newtype <<< prop (Proxy :: Proxy "missed")

bump metric = metric += 1
touch metric sz = metric += Tuple 1 sz

_m :: forall meta nt tok s t r'. IsSymbol s => Row.Cons s t r' Metrics => Proxy s -> Lens' (KnownStates meta nt tok) t
_m p = _Newtype <<< prop (Proxy :: Proxy "metrics") <<< prop p

_m_hit_known :: forall meta nt tok. Lens' (KnownStates meta nt tok) Int
_m_hit_known = _m (Proxy :: Proxy "known_hit")

_m_miss_known :: forall meta nt tok. Lens' (KnownStates meta nt tok) Int
_m_miss_known = _m (Proxy :: Proxy "known_miss")

_m_hit_seeded :: forall meta nt tok. Lens' (KnownStates meta nt tok) Int
_m_hit_seeded = _m (Proxy :: Proxy "seeded_hit")

_m_miss_seeded :: forall meta nt tok. Lens' (KnownStates meta nt tok) Int
_m_miss_seeded = _m (Proxy :: Proxy "seeded_miss")

_m_rounds :: forall meta nt tok. Lens' (KnownStates meta nt tok) Int
_m_rounds = _m (Proxy :: Proxy "rounds")

type Knowing meta nt tok = StateT (KnownStates meta nt tok) Identity

addCompletedState :: forall meta nt tok. Ord meta => Ord nt => Ord tok => State meta nt tok -> Knowing meta nt tok Int
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

seedState :: forall meta nt tok. Semiring meta => Ord meta => Ord nt => Ord tok => State meta nt tok -> Knowing meta nt tok (KnownState meta nt tok)
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
