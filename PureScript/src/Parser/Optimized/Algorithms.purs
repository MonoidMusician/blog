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

type Augmenteds i space nt tok =
  { augmented :: Grammar space nt tok
  , eof :: tok
  , starts :: Array (i /\ StateItem space nt tok)
  }

fromSeeds ::
  forall space nt tok.
    Semiring space =>
    Ord space =>
    Ord nt =>
    Ord tok =>
  Grammar space nt tok ->
  Array nt ->
  Augmenteds nt space (Either nt nt) (OrEOF tok)
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
      , lookbehind: Additive (one :: space)
      , lookahead: mempty
      }
    }

statesNumberedMany ::
  forall f space nt tok.
    Foldable f =>
    Semiring space =>
    Ord space =>
    Ord nt =>
    Ord tok =>
  Old.Grammar space nt Int tok ->
  f nt ->
  Map nt Int /\
    Old.States Int space (Either nt nt) (Maybe Int) (OrEOF tok)
statesNumberedMany initial entrief =
  let
    entries = Array.nub $ Array.fromFoldable entrief
    { augmented: grammar, starts } = fromSeeds (fromOld initial) entries
  in map (toOld grammar) $ closeStates grammar $ map fromItem <$> starts

getReduction :: forall space nt tok. Semiring space => Ord tok => StateItem space nt tok -> SemigroupMap tok (Additive space /\ Reduction space nt tok)
getReduction { pName, rName, rule: Zipper rule [], lookbehind: Additive lookbehind, lookahead } =
  lookahead <#> unwrap >>> (Additive <<< if Array.null rule then (lookbehind * _) else identity) >>> (_ /\ (rule /\ pName /\ rName))
getReduction _ = SemigroupMap $ Map.empty

getReductions :: forall space nt tok. Semiring space => Ord nt => Ord tok => Grammar space nt tok -> State space nt tok -> SemigroupMap tok (Additive space /\ NonEmptyArray (Reduction space nt tok))
getReductions grammar = toItems grammar \item ->
  map NEA.singleton <$> getReduction item



findNT ::
  forall space nt tok.
    Semiring space =>
  Zipper space nt tok ->
  Maybe
    { interterminal :: space
    , nonterminal :: nt
    , following :: List (Tuple space nt)
    , continue :: Tuple space (Maybe tok)
    }
findNT (Zipper _ after0) = go (one :: space) (Array.toUnfoldable after0)
  where
  go interterminal (NonTerminal nt : tail) =
    let
      { following, continue } = preview tail
    in
      Just { interterminal, nonterminal: nt, following, continue }
  go interterm (InterTerminal space : tail) =
    go (interterm * space) tail
  go _ _ = Nothing

preview ::
  forall space nt tok.
    Semiring space =>
  List (Part space nt tok) ->
  { following :: List (Tuple space nt), continue :: Tuple space (Maybe tok) }
preview = scanFollowing Nil (one :: space)
  where
  scanFollowing acc interterm (InterTerminal space : tail) =
    scanFollowing acc (interterm * space) tail
  scanFollowing acc interterm (NonTerminal nt : tail) =
    scanFollowing (Tuple interterm nt : acc) (one :: space) tail
  scanFollowing acc interterm (Terminal tok : _done) =
    { following: List.reverse acc, continue: Tuple interterm (Just tok) }
  scanFollowing acc interterm Nil =
    { following: List.reverse acc, continue: Tuple interterm Nothing }

continueOn :: forall space tok. Semiring space => Ord space => Tuple space (Maybe tok) -> Lookahead space tok -> Lookahead space tok
continueOn (Tuple space continue) lookahead = case continue of
  Just tok -> SemigroupMap $ Map.singleton tok $ Additive space
  Nothing -> coerce (space * _) <$> lookahead

startRules :: forall space nt tok. Semiring space => Ord space => Ord nt => Ord tok => Grammar space nt tok -> nt -> (Additive space -> Lookahead space tok -> State space nt tok)
startRules (MkGrammar grammar) p
  | Just mrules <- Map.lookup p grammar =
    \lookbehind lookahead -> foldMap fromItem' $ Array.catMaybes $ mrules # mapWithIndex \i mrule ->
        mrule $> { pName: p, rName: Just i, rule: 0, lookbehind, lookahead }
startRules _ _ = unsafeCrashWith "startRules"

closeItem :: forall space nt tok. Semiring space => Ord space => Ord nt => Ord tok => Grammar space nt tok -> Zipper space nt tok -> Lookahead space tok -> State space nt tok
closeItem grammar rule lookahead = case findNT rule of
  Nothing -> mempty
  Just { interterminal, nonterminal: p, following, continue } ->
    startRules grammar p (Additive interterminal) $ firsts grammar following (continueOn continue lookahead)

close1 :: forall space nt tok. Semiring space => Ord space => Ord nt => Ord tok => Grammar space nt tok -> State space nt tok -> State space nt tok
close1 grammar = toItems grammar
  \{ rule, lookahead } ->
    closeItem grammar rule lookahead

close ::
  forall space nt tok.
    Semiring space =>
    Ord space =>
    Ord nt =>
    Ord tok =>
  Grammar space nt tok ->
  State space nt tok ->
  State space nt tok
close grammar = closeSemilattice (close1 grammar)

firsts :: forall space nt tok. Semiring space => Ord space => Ord nt => Ord tok => Grammar space nt tok -> List (Tuple space nt) -> Lookahead space tok -> Lookahead space tok
firsts (MkGrammar rules0) ps0 lookahead0 = readyset rules0 (one :: space) ps0 lookahead0
  where
  readyset rules space ps lookahead = case ps of
    Tuple space' head : tail -> go rules (space * space') head tail lookahead
    Nil -> coerce (space * _) <$> lookahead
  go rules space p ps lookahead = case Map.lookup p rules of
    Nothing -> mempty
    Just matches -> matches #
      let rules' = Map.delete p rules in
      foldMap $ foldMap $ Array.toUnfoldable >>> preview >>> \{ following, continue } ->
        let
          continue' = continueOn continue lookahead
          following'
            | isJust (snd continue) = following
            | otherwise = following <> ps
        in readyset rules' space following' continue'

nextSteps ::
  forall space nt tok.
    Semiring space =>
    Ord space =>
    Ord nt =>
    Ord tok =>
  Grammar space nt tok ->
  State space nt tok ->
  { terminal :: SemigroupMap tok (Additive space /\ State space nt tok)
  , nonTerminal :: SemigroupMap nt (Additive space /\ State space nt tok)
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
              InterTerminal space ->
                go (acc * space) tail
    in go (one :: space) after

closeStates ::
  forall k space nt tok.
    Semiring space =>
    Ord k =>
    Ord space =>
    Ord nt =>
    Ord tok =>
  Grammar space nt tok ->
  Array (k /\ State space nt tok) ->
  Map k Int /\ States space nt tok
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
  forall space nt tok.
    Semiring space =>
    Ord space =>
    Ord nt =>
    Ord tok =>
  List (KnownState space nt tok) ->
  Knowing space nt tok Unit
closeStatesLoop newly = do
  next <- closeStates1 newly
  case next of
    List.Nil -> pure unit
    _ -> closeStatesLoop next

closeStates1 ::
  forall space nt tok.
    Semiring space =>
    Ord space =>
    Ord nt =>
    Ord tok =>
  List (KnownState space nt tok) ->
  Knowing space nt tok (List (KnownState space nt tok))
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



type KnownState space nt tok = Tuple (State space nt tok) Int
newtype KnownStates space nt tok = KnownStates
  { seeded :: Map (State space nt tok) (KnownState space nt tok)
  , known :: Map (State space nt tok) Int
  , finalized :: Map Int (StateInfo space nt tok)
  , grammar :: Grammar space nt tok
  , metrics :: Record Metrics
  , missed :: List (KnownState space nt tok)
  }
type Metrics =
  ( known_hit :: Int
  , known_miss :: Int
  , seeded_hit :: Int
  , seeded_miss :: Int
  , rounds :: Int
  )
derive instance newtypeKnownStates :: Newtype (KnownStates space nt tok) _
_finalized :: forall space nt tok. Lens' (KnownStates space nt tok) (Map Int (StateInfo space nt tok))
_finalized = _Newtype <<< prop (Proxy :: Proxy "finalized")
_known :: forall space nt tok. Lens' (KnownStates space nt tok) (Map (State space nt tok) Int)
_known = _Newtype <<< prop (Proxy :: Proxy "known")
_seeded :: forall space nt tok. Lens' (KnownStates space nt tok) (Map (State space nt tok) (KnownState space nt tok))
_seeded = _Newtype <<< prop (Proxy :: Proxy "seeded")
_grammar :: forall space nt tok. Lens' (KnownStates space nt tok) (Grammar space nt tok)
_grammar = _Newtype <<< prop (Proxy :: Proxy "grammar")
_missed :: forall space nt tok. Lens' (KnownStates space nt tok) (List (KnownState space nt tok))
_missed = _Newtype <<< prop (Proxy :: Proxy "missed")

bump metric = metric += 1
touch metric sz = metric += Tuple 1 sz

_m :: forall space nt tok s t r'. IsSymbol s => Row.Cons s t r' Metrics => Proxy s -> Lens' (KnownStates space nt tok) t
_m p = _Newtype <<< prop (Proxy :: Proxy "metrics") <<< prop p

_m_hit_known :: forall space nt tok. Lens' (KnownStates space nt tok) Int
_m_hit_known = _m (Proxy :: Proxy "known_hit")

_m_miss_known :: forall space nt tok. Lens' (KnownStates space nt tok) Int
_m_miss_known = _m (Proxy :: Proxy "known_miss")

_m_hit_seeded :: forall space nt tok. Lens' (KnownStates space nt tok) Int
_m_hit_seeded = _m (Proxy :: Proxy "seeded_hit")

_m_miss_seeded :: forall space nt tok. Lens' (KnownStates space nt tok) Int
_m_miss_seeded = _m (Proxy :: Proxy "seeded_miss")

_m_rounds :: forall space nt tok. Lens' (KnownStates space nt tok) Int
_m_rounds = _m (Proxy :: Proxy "rounds")

type Knowing space nt tok = StateT (KnownStates space nt tok) Identity

addCompletedState :: forall space nt tok. Ord space => Ord nt => Ord tok => State space nt tok -> Knowing space nt tok Int
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

seedState :: forall space nt tok. Semiring space => Ord space => Ord nt => Ord tok => State space nt tok -> Knowing space nt tok (KnownState space nt tok)
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
