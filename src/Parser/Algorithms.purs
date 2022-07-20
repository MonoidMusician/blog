module Parser.Algorithms where

import Parser.Types
import Prelude

import Control.Alt ((<|>))
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray(..))
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), hush, note)
import Data.Filterable (filterMap)
import Data.Foldable (elem, foldMap, oneOfMap, sum)
import Data.Function (on)
import Data.List (List)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString(..))
import Data.String.NonEmpty as NES
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Parser.Proto (Stack(..), topOf)
import Parser.Proto as Proto
import Partial.Unsafe (unsafeCrashWith)


countNTs :: forall nt tok. Fragment nt tok -> Int
countNTs = sum <<< map (\t -> if isNonTerminal t then 1 else 0)

countTs :: forall nt tok. Fragment nt tok -> Int
countTs = sum <<< map (\t -> if isTerminal t then 1 else 0)

-- NonEmptyArray (Int /\ rule)
-- every minimum that occurs

chooseBySize :: forall r nt tok. Int -> NonEmptyArray { rule :: Fragment nt tok | r } -> NonEmptyArray { rule :: Fragment nt tok | r }
chooseBySize i as =
  let
    sized = as <#> \rule -> countNTs rule.rule /\ rule
  in
    case NEA.fromArray (NEA.filter (\(size /\ _) -> size <= i) sized) of
      Nothing ->
        -- If none are small enough, take the smallest ones with the least tokens
        -- (this prevents e.g. infinite extra parentheses)
        map snd $ NEA.head $
          NEA.groupAllBy (compare `on` fst <> compare `on` (snd >>> _.rule >>> Array.length)) sized
      Just as' -> map snd as'

withProduceable
  :: forall nt r tok
   . Eq nt
  => Eq r
  => Eq tok
  => Augmented nt r tok
  -> Produceable nt r tok
withProduceable = { grammar: _, produced: _ } <*> (produceable <<< _.augmented)

produceable
  :: forall nt r tok
   . Eq nt
  => Eq r
  => Eq tok
  => Grammar nt r tok
  -> Array (Produced nt r tok)
produceable (MkGrammar initialRules) = produceAll []
  where
  produceOne produced (NonTerminal nt) =
    Array.find (_.production >>> _.pName >>> eq nt) produced <#> _.produced
  produceOne _ (Terminal tok) = pure [ tok ]
  produceAll rules =
    let
      rules' = produceMore rules
    in
      if rules' == rules then rules else produceAll rules'
  produceMore produced =
    let
      rejected = initialRules `Array.difference` map _.production produced
      more = rejected # filterMap \rule ->
        rule.rule # traverse (produceOne produced) # map \prod ->
          { production: rule
          , produced: join prod
          }
    in
      produced <> more

fromSeed
  :: forall nt r tok
   . Grammar nt r tok
  -> nt
  -> Augmented (Maybe nt) (Maybe r) (Maybe tok)
fromSeed (MkGrammar rules) entry =
  let
    rule0 = { pName: Nothing, rName: Nothing, rule: [ NonTerminal (Just entry), Terminal Nothing ] }
    rules' = rules <#> \{ pName, rName, rule } ->
      { pName: Just pName
      , rName: Just rName
      , rule: bimap Just Just <$> rule
      }
  in
    { augmented: MkGrammar ([ rule0 ] <> rules')
    , start: { pName: Nothing, rName: Nothing, rule: Zipper [] rule0.rule, lookahead: [] }
    , eof: Nothing
    , entry: Just entry
    }

fromSeed'
  :: forall nt r tok
   . nt
  -> r
  -> tok
  -> Grammar nt r tok
  -> nt
  -> Augmented nt r tok
fromSeed' nt0 r0 tok0 (MkGrammar rules) entry =
  let
    rule0 = { pName: nt0, rName: r0, rule: [ NonTerminal entry, Terminal tok0 ] }
  in
    { augmented: MkGrammar ([ rule0 ] <> rules)
    , start: { pName: nt0, rName: r0, rule: Zipper [] rule0.rule, lookahead: [] }
    , eof: tok0
    , entry
    }

calculateStates
  :: forall nt r tok
   . Ord nt
  => Eq r
  => Ord tok
  => Grammar nt r tok
  -> StateItem nt r tok
  -> Array (State nt r tok)
calculateStates grammar start = closeStates grammar [ close grammar (minimizeState [ start ]) ]

generate
  :: forall nt r tok
   . Ord nt
  => Eq r
  => Ord tok
  => Grammar nt r tok
  -> nt
  -> Array (State (Maybe nt) (Maybe r) (Maybe tok))
generate initial entry =
  let
    { augmented: grammar, start } = fromSeed initial entry
  in
    calculateStates grammar start

generate'
  :: forall nt r tok
   . Ord nt
  => Eq r
  => Ord tok
  => nt
  -> r
  -> tok
  -> Grammar nt r tok
  -> nt
  -> Array (State nt r tok)
generate' nt0 r0 tok0 initial entry =
  let
    { augmented: grammar, start } = fromSeed' nt0 r0 tok0 initial entry
  in
    calculateStates grammar start

getResultA :: forall x y z. Stack x (Either y z) -> Maybe z
getResultA (Snoc (Snoc (Zero _) (Right result) _) (Left _) _) = Just result
getResultA _ = Nothing

getResultC :: forall x y z. Stack x (CST y z) -> Maybe (CST y z)
getResultC (Snoc (Snoc (Zero _) result@(Branch _ _) _) (Leaf _) _) = Just result
getResultC _ = Nothing

parseIntoGrammar
  :: forall t
   . Array
       { pName :: String
       , rName :: t
       , rule :: String
       }
  -> Grammar NonEmptyString t CodePoint
parseIntoGrammar = compose MkGrammar $
  Array.mapMaybe (\r -> NES.fromString r.pName <#> \p -> r { pName = p })
    >>> parseDefinitions

parseDefinitions grammar =
  let
    nts = longestFirst (grammar <#> _.pName)
    p = parseDefinition nts
  in
    grammar <#> \r -> r { rule = p r.rule }





verifyTokens :: forall nt r tok. Ord tok => Grammar nt r tok -> List tok -> Maybe (List tok)
verifyTokens = traverse <<< verifyToken

verifyToken :: forall nt r tok. Ord tok => Grammar nt r tok -> tok -> Maybe tok
verifyToken = gatherTokens >>> \toks tok ->
  if Set.member tok toks then Just tok else Nothing

gatherTokens :: forall nt r tok. Ord tok => Grammar nt r tok -> Set tok
gatherTokens (MkGrammar rules) = rules # Array.foldMap \{ rule } ->
  Array.mapMaybe unTerminal rule # Set.fromFoldable

gatherTokens_ :: forall nt r tok. Ord tok => Grammar nt r tok -> Map tok (NonEmptyArray r)
gatherTokens_ (MkGrammar rules) = unwrap $ rules # Array.foldMap \{ rule, rName } ->
  Array.mapMaybe unTerminal rule # foldMap \tok ->
    SemigroupMap $ Map.singleton tok (NEA.singleton rName)

gatherTokens' :: forall nt r tok. Ord tok => Grammar nt r tok -> Array tok
gatherTokens' (MkGrammar rules) = Array.nub $ rules # Array.foldMap \{ rule } ->
  Array.mapMaybe unTerminal rule

gatherNonTerminals :: forall nt r tok. Ord nt => Grammar nt r tok -> Set nt
gatherNonTerminals (MkGrammar rules) = rules # Array.foldMap \{ rule, pName } ->
  Array.mapMaybe unNonTerminal rule # NonEmpty pName # Set.fromFoldable

gatherNonTerminals_ :: forall nt r tok. Ord nt => Grammar nt r tok -> Map nt (NonEmptyArray r)
gatherNonTerminals_ (MkGrammar rules) = unwrap $ rules # Array.foldMap \{ rule, pName, rName } ->
  Array.mapMaybe unNonTerminal rule # NonEmpty pName # foldMap \nt ->
    SemigroupMap $ Map.singleton nt (NEA.singleton rName)

gatherNonTerminals' :: forall nt r tok. Ord nt => Grammar nt r tok -> Array nt
gatherNonTerminals' (MkGrammar rules) = Array.nub $ rules # Array.foldMap \{ rule, pName } ->
  [ pName ] <> Array.mapMaybe unNonTerminal rule


fromString :: SAugmented -> String -> Maybe (List CodePoint)
fromString grammar = addEOF grammar >>> fromString' grammar

addEOF :: SAugmented -> String -> String
addEOF grammar e =
  if String.contains (String.Pattern (String.singleton grammar.eof)) e then e
  else e <> String.singleton grammar.eof

addEOF' :: SAugmented -> List CodePoint -> List CodePoint
addEOF' grammar e =
  if grammar.eof `elem` e then e
  else e <> pure grammar.eof

fromString' :: SAugmented -> String -> Maybe (List CodePoint)
fromString' grammar = String.toCodePointArray
  >>> Array.toUnfoldable
  >>> verifyTokens grammar.augmented




numberStates
  :: forall s nt r tok
   . Ord nt
  => Eq r
  => Ord tok
  => (Int -> s)
  -> Grammar nt r tok
  -> Array (State nt r tok)
  -> Either (Array (StateItem nt r tok)) (States s nt r tok)
numberStates ix grammar states = map States $ states #
  traverseWithIndex \i items ->
    let
      findState seed = note seed $ map ix $
        Array.findIndex (eq (close grammar (minimizeState seed))) states
      next = nextSteps items
      reductions = Reduces <$> getReductions items
    in
      ado
        shifts <- traverse (map Shift <<< findState) $ next.terminal
        receive <- traverse findState $ unwrap $ next.nonTerminal
        in
          { sName: ix i
          , items
          , advance: shifts <> reductions
          , receive
          }

toAdvance :: forall nt tok. Zipper nt tok -> Maybe (Part nt tok)
toAdvance (Zipper _ after) = Array.head after

toAdvanceTo
  :: forall s nt r tok
   . Ord nt
  => Ord tok
  => StateInfo s nt r tok
  -> Zipper nt tok
  -> Maybe s
toAdvanceTo { advance, receive } = toAdvance >=> case _ of
  NonTerminal nt -> Map.lookup nt receive
  Terminal tok -> Map.lookup tok (unwrap advance) >>= unShift

getReduction :: forall nt r tok. Ord tok => StateItem nt r tok -> SemigroupMap tok (nt /\ r)
getReduction { pName, rName, rule: Zipper _ [], lookahead } =
  SemigroupMap $ Map.fromFoldable $ (/\) <$> lookahead <@> (pName /\ rName)
getReduction _ = SemigroupMap $ Map.empty

getReductions :: forall nt r tok. Ord tok => State nt r tok -> SemigroupMap tok (NonEmptyArray (nt /\ r))
getReductions (State items) = items # foldMap \item ->
  NEA.singleton <$> getReduction item



findNT
  :: forall nt tok
   . Zipper nt tok
  -> Maybe
       { nonterminal :: nt, following :: Array nt, continue :: Maybe tok }
findNT (Zipper _ after) = Array.uncons after >>= case _ of
  { head: NonTerminal nt, tail } ->
    let
      { following, continue } = preview tail
    in
      Just { nonterminal: nt, following, continue }
  _ -> Nothing

preview
  :: forall nt tok
   . Array (Part nt tok)
  -> { following :: Array nt, continue :: Maybe tok }
preview tail = { following, continue }
  where
  { init, rest } = Array.span isNonTerminal tail
  following = Array.mapMaybe unNonTerminal init
  continue = Array.head rest >>= unTerminal

continueOn :: forall tok. Maybe tok -> Lookahead tok -> Lookahead tok
continueOn continue lookahead = case continue of
  Just tok -> [ tok ]
  Nothing -> lookahead

startRules :: forall nt r tok. Eq nt => Grammar nt r tok -> nt -> (Lookahead tok -> Array (StateItem nt r tok))
startRules (MkGrammar rules) p =
  let
    filtered = Array.filter (\{ pName } -> pName == p) rules
  in
    \lookahead -> filtered <#> \{ pName, rName, rule } -> { pName, rName, rule: Zipper [] rule, lookahead }

closeItem :: forall nt r tok. Eq nt => Grammar nt r tok -> StateItem nt r tok -> Array (StateItem nt r tok)
closeItem grammar item = case findNT item.rule of
  Nothing -> []
  Just { nonterminal: p, following, continue } ->
    startRules grammar p $
      firsts grammar following (continueOn continue item.lookahead)

close1 :: forall nt r tok. Eq nt => Grammar nt r tok -> State nt r tok -> Array (StateItem nt r tok)
close1 grammar (State items) = closeItem grammar =<< items

close
  :: forall nt r tok
   . Eq r
  => Eq nt
  => Eq tok
  => Grammar nt r tok
  -> State nt r tok
  -> State nt r tok
close grammar state0 =
  let
    state' = close1 grammar state0
  in
    if Array.null state' then state0
    else
      let
        state = state0 <> State state'
      in
        if state == state0 then state0 else close grammar state

firsts :: forall nt r tok. Eq nt => Grammar nt r tok -> Array nt -> Lookahead tok -> Lookahead tok
firsts (MkGrammar rules0) ps0 lookahead0 = readyset rules0 ps0 lookahead0
  where
  readyset rules ps lookahead = case Array.uncons ps of
    Just { head, tail } -> go rules head tail lookahead
    _ -> lookahead
  go rules p ps lookahead =
    let
      { yes: matches, no: rules' } = Array.partition (\{ pName } -> pName == p) rules
    in
      matches >>= _.rule >>> preview >>> \{ following, continue } ->
        -- (p : following continue) (ps lookahead)
        case continue of
          Just tok -> readyset rules' following [ tok ]
          Nothing -> readyset rules' (following <> ps) lookahead

nextStep
  :: forall nt r tok
   . StateItem nt r tok
  -> { nonTerminal :: SemigroupMap nt (Array (StateItem nt r tok))
     , terminal :: SemigroupMap tok (Array (StateItem nt r tok))
     }
nextStep item@{ rule: Zipper before after } = case Array.uncons after of
  Nothing ->
    { nonTerminal: SemigroupMap Map.empty
    , terminal: SemigroupMap Map.empty
    }
  Just { head, tail } ->
    let
      nextStateSeed = pure
        { pName: item.pName
        , rName: item.rName
        , rule: Zipper (before <> [ head ]) tail
        , lookahead: item.lookahead
        }
    in
      case head of
        NonTerminal nt ->
          { nonTerminal: SemigroupMap (Map.singleton nt nextStateSeed)
          , terminal: SemigroupMap Map.empty
          }
        Terminal tok ->
          { nonTerminal: SemigroupMap Map.empty
          , terminal: SemigroupMap (Map.singleton tok nextStateSeed)
          }

nextSteps
  :: forall nt r tok
   . Ord nt
  => Ord tok
  => State nt r tok
  -> { nonTerminal :: SemigroupMap nt (Array (StateItem nt r tok))
     , terminal :: SemigroupMap tok (Array (StateItem nt r tok))
     }
nextSteps (State items) = Array.foldMap nextStep items

-- Meant to handle nondeterminacy of shifts, but not reduces
nextSteps'
  :: forall nt r tok
   . Ord nt
  => Ord tok
  => State nt r tok
  -> Array (Part nt tok /\ Array (StateItem nt r tok))
nextSteps' state =
  let
    { nonTerminal: SemigroupMap nts, terminal: SemigroupMap toks } = nextSteps state
  in
    lmap Terminal <$> Map.toUnfoldable toks <|> lmap NonTerminal <$> Map.toUnfoldable nts

newStates
  :: forall nt r tok
   . Ord nt
  => Eq r
  => Ord tok
  => Grammar nt r tok
  -> State nt r tok
  -> Array (State nt r tok)
newStates grammar state =
  Array.nubEq (close grammar <<< minimizeState <<< snd <$> nextSteps' state)

closeStates1
  :: forall nt r tok
   . Ord nt
  => Eq r
  => Ord tok
  => Grammar nt r tok
  -> Array (State nt r tok)
  -> Array (State nt r tok)
closeStates1 grammar states = Array.nubEq (states <> (states >>= newStates grammar))

closeStates
  :: forall nt r tok
   . Ord nt
  => Eq r
  => Ord tok
  => Grammar nt r tok
  -> Array (State nt r tok)
  -> Array (State nt r tok)
closeStates grammar states =
  let
    states' = closeStates1 grammar states
  in
    if states' == states then states else closeStates grammar states'




indexStates :: forall s nt r tok. Ord s => States s nt r tok -> StateIndex s
indexStates (States states) =
  Map.fromFoldable $ mapWithIndex (\i { sName } -> sName /\ i) states

toTable
  :: forall s nt r tok
   . Ord s
  => Ord nt
  => Eq r
  => Ord tok
  => States s nt r tok
  -> StateIndex s
  -> Proto.Table s (nt /\ r) tok (CST (nt /\ r) tok)
toTable (States states) tabulated =
  let
    lookupState s = Map.lookup s tabulated >>= Array.index states
    lookupAction tok { advance: SemigroupMap m } = Map.lookup tok m
    lookupReduction (p /\ r) { items: State items } = items # oneOfMap case _ of
      { rule: Zipper parsed [], pName, rName } | pName == p && rName == r ->
        Just parsed
      _ -> Nothing
    takeStack r stack0 parsed =
      let
        take1 (taken /\ stack) = case _ of
          Terminal tok -> case stack of
            Snoc stack' v@(Leaf tok') _ | tok == tok' ->
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
  in
    Proto.Table
      { promote: Leaf
      , step: \s tok -> lookupState s >>= lookupAction tok >>= decide
      , goto: \r stack ->
          lookupState (topOf stack) >>= \state -> do
            lookupReduction r state >>= takeStack r stack
      }

toTable'
  :: forall s nt r tok
   . Ord s
  => Ord nt
  => Eq r
  => Ord tok
  => States s nt r tok
  -> StateIndex s
  -> Proto.Table s (nt /\ r) tok (Either tok (AST (nt /\ r)))
toTable' (States states) tabulated =
  let
    lookupState s = Map.lookup s tabulated >>= Array.index states
    lookupAction tok { advance: SemigroupMap m } = Map.lookup tok m
    lookupReduction (p /\ r) { items: State items } = items # oneOfMap case _ of
      { rule: Zipper parsed [], pName, rName } | pName == p && rName == r ->
        Just parsed
      _ -> Nothing
    takeStack r stack0 parsed =
      let
        take1 (taken /\ stack) = case _ of
          Terminal tok -> case stack of
            Snoc stack' v@(Left tok') _ | tok == tok' ->
              Just $ ([ v ] <> taken) /\ stack'
            _ -> unsafeCrashWith "expected token on stack"
          NonTerminal nt -> case stack of
            Snoc stack' v@(Right (Layer (p /\ _) _)) _ | p == nt ->
              Just $ ([ v ] <> taken) /\ stack'
            _ -> unsafeCrashWith "expected terminal on stack"
      in
        Array.foldM take1 ([] /\ stack0) (Array.reverse parsed) >>= \(taken /\ stack) ->
          Snoc stack (Right (Layer r (Array.mapMaybe hush taken))) <$> goto r (topOf stack)
    goto (p /\ _) s' = lookupState s' >>= _.receive >>> Map.lookup p
  in
    Proto.Table
      { promote: Left
      , step: \s tok -> lookupState s >>= lookupAction tok >>= decide
      , goto: \r stack ->
          lookupState (topOf stack) >>= \state -> do
            lookupReduction r state >>= takeStack r stack
      }


longestFirst :: Array NonEmptyString -> Array NonEmptyString
longestFirst = Array.nub >>> Array.sortBy (flip compare `on` NES.length <> compare)

parseDefinition :: Array NonEmptyString -> String -> SFragment
parseDefinition nts s = case String.uncons s of
  Just { head: c, tail: s' } ->
    case recognize nts s of
      Just nt -> [ NonTerminal nt ] <> parseDefinition nts (String.drop (NES.length nt) s)
      Nothing -> [ Terminal c ] <> parseDefinition nts s'
  Nothing -> []

unParseDefinition :: SFragment -> String
unParseDefinition = foldMap case _ of
  NonTerminal nt -> NES.toString nt
  Terminal tok -> String.singleton tok

recognize :: Array NonEmptyString -> String -> Maybe NonEmptyString
recognize nts s = nts # Array.find \nt ->
  String.take (NES.length nt) s == NES.toString nt
