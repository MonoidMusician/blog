module Parser.Optimized.Algorithms where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Parser.Debug (perf)
import Parser.Optimized.Types (Grammar(..), Lookahead, State(..), StateItem, States(..), fromItem, fromOld, toItems, toOld, unsafeFromJust)
import Parser.Types (Fragment, OrEOF(..), Part(..), ShiftReduce(..), Zipper(..), isNonTerminal, unNonTerminal, unTerminal)
import Parser.Types as Old
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)

type Augmenteds i nt tok =
  { augmented :: Grammar nt tok
  , eof :: tok
  , starts :: Array (StateItem nt tok /\ i)
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
    , starts: entries <#> \pName ->
      { pName: Left pName
      , rName: Nothing
      , rule: Zipper [] [ NonTerminal (Right pName), Terminal EOF ]
      , lookahead: Set.empty
      } /\ pName
    }

calculateStatesMany ::
  forall i nt tok.
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  Map (State nt tok) i ->
  Map (State nt tok) (Maybe i)
calculateStatesMany grammar starts =
  let
    seed = Map.fromFoldable $ starts # mapWithIndex
      \state i -> close grammar state /\ i
    allStates = closeStates grammar $ Set.fromMap $ void seed
  in
    Set.toMap allStates # mapWithIndex \state _ ->
      Map.lookup state seed

numberStates ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  Array (State nt tok) ->
  States nt tok
numberStates grammar states =
  States $ states <#> \items ->
    let
      findState seed = unsafeFromJust "findState" $
        Array.findIndex (eq seed) states
      next = nextSteps grammar items
      reductions = Reduces <$> getReductions grammar items
      shifts = map (Shift <<< findState) $ next.terminal
      receive = map findState $ unwrap $ next.nonTerminal
    in
      { items
      , advance: shifts <> reductions
      , receive
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
    seed = Map.fromFoldable $ lmap fromItem <$> starts
    ordered = Map.toUnfoldable $
      perf "calculateStatesMany" (calculateStatesMany grammar) seed
    entryStates = Map.fromFoldable $ Array.catMaybes $ ordered
      # mapWithIndex \i (_ /\ midx) -> do
        midx <#> (_ /\ i)
    numberedStates =
      toOld grammar $ perf "numberStates" (numberStates grammar) $ map fst ordered
  in
    entryStates /\ numberedStates

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
    \lookahead -> State $ SemigroupMap $ Map.singleton p $
      SemigroupMap $ Map.fromFoldable $ Array.catMaybes $ mrules # mapWithIndex \i mrule ->
        mrule $> (Just i /\ SemigroupMap (Map.singleton 0 lookahead))
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
nextSteps grammar = closeBoth <<< toItems grammar
  \{ pName, rName, rule: Zipper before after, lookahead } ->
    case Array.head after of
      Nothing -> mempty
      Just (Terminal tok) ->
        { nonTerminal: mempty, terminal: SemigroupMap $ Map.singleton tok $ coerce $
          Map.singleton pName $ Map.singleton rName $ Map.singleton (Array.length before + 1) lookahead
        }
      Just (NonTerminal nt) ->
        { terminal: mempty, nonTerminal: SemigroupMap $ Map.singleton nt $ coerce $
          Map.singleton pName $ Map.singleton rName $ Map.singleton (Array.length before + 1) lookahead
        }
  where
  closeBoth r =
    { terminal: close grammar <$> r.terminal
    , nonTerminal: close grammar <$> r.nonTerminal
    }

newStates ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  State nt tok ->
  Set (State nt tok)
newStates grammar = nextSteps grammar >>> \r ->
  Set.fromFoldable r.terminal <> Set.fromFoldable r.nonTerminal


closeStates1 ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  Set (State nt tok) ->
  Set (State nt tok)
closeStates1 grammar states = states # foldMap (newStates grammar)

closeStates ::
  forall nt tok.
    Ord nt =>
    Ord tok =>
  Grammar nt tok ->
  Set (State nt tok) ->
  Set (State nt tok)
closeStates = closeSemilattice closeStates1



closeSemilattice :: forall i m. Eq m => Monoid m => (i -> m -> m) -> i -> m -> m
closeSemilattice f = join <<< closeSemilattice2 <<< f

closeSemilattice2 :: forall m. Eq m => Monoid m => (m -> m) -> m -> m -> m
closeSemilattice2 f acc step =
  let new = f step in if new == mempty then acc else
  let next = acc <> new in if acc == next then acc else
  closeSemilattice2 f next new
