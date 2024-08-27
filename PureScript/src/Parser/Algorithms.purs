module Parser.Algorithms where

import Prelude

import Control.Alt ((<|>))
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bisequence, bitraverse)
import Data.Either (Either(..), either, hush, note)
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, elem, foldMap, oneOfMap, product, sum)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Idiolect ((>==))
import Parser.Proto (Stack(..), topOf)
import Parser.Proto as Proto
import Parser.Types (AST(..), Augmented, Augmenteds, CST(..), Fragment, Grammar(..), ICST(..), Lookahead, OrEOF(..), Part(..), Produced, Producible, SAugmented, SFragment, ShiftReduce(..), State(..), StateIndex, StateInfo, StateItem, States(..), Zipper(..), decide, isInterTerminal, isNonTerminal, isTerminal, minimizeState, minimizeStateCat, noInterTerminals, noInterTerminalz, notEOF, nubEqCat, unInterTerminal, unNonTerminal, unShift, unTerminal)
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)


countNTs :: forall meta nt tok. Fragment meta nt tok -> Int
countNTs = sum <<< map (\t -> if isNonTerminal t then 1 else 0)

countTs :: forall meta nt tok. Fragment meta nt tok -> Int
countTs = sum <<< map (\t -> if isTerminal t then 1 else 0)

-- NonEmptyArray (Int /\ rule)
-- every minimum that occurs

chooseBySize :: forall r meta nt tok. Int -> NonEmptyArray { rule :: Fragment meta nt tok | r } -> NonEmptyArray { rule :: Fragment meta nt tok | r }
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

withProducible
  :: forall meta nt r tok
   . Eq meta
  => Eq nt
  => Eq r
  => Eq tok
  => Augmented meta nt r tok
  -> Producible meta nt r tok
withProducible = { grammar: _, produced: _ } <*> (producible <<< _.augmented)

producible
  :: forall meta nt r tok
   . Eq meta
  => Eq nt
  => Eq r
  => Eq tok
  => Grammar meta nt r tok
  -> Array (Produced meta nt r tok)
producible (MkGrammar initialRules) = produceAll []
  where
  produceOne produced (NonTerminal nt) =
    Array.find (_.production >>> _.pName >>> eq nt) produced <#> _.produced
  produceOne _ (Terminal tok) = pure [ tok ]
  produceOne _ (InterTerminal _) = pure []
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

fromSeed ::
  forall meta nt r tok.
    Semiring meta =>
  Grammar meta nt r tok ->
  nt ->
  Augmented meta (Maybe nt) (Maybe r) (OrEOF tok)
fromSeed (MkGrammar rules) entry =
  let
    rule0 = { pName: Nothing, rName: Nothing, rule: [ NonTerminal (Just entry), Terminal EOF ] }
    rules' = rules <#> \{ pName, rName, rule } ->
      { pName: Just pName
      , rName: Just rName
      , rule: bimap Just Continue <$> rule
      }
  in
    { augmented: MkGrammar ([ rule0 ] <> rules')
    , start: { pName: Nothing, rName: Nothing, rule: Zipper [] rule0.rule, lookbehind: Additive one, lookahead: [] }
    , eof: EOF
    , entry: Just entry
    }

fromSeeds ::
  forall meta nt r tok.
    Semiring meta =>
  Grammar meta nt r tok ->
  Array nt ->
  Augmenteds meta (Either nt nt) (Maybe r) (OrEOF tok)
fromSeeds (MkGrammar rules) entries =
  let
    entryRules = entries <#> \entry ->
      { pName: Left entry
      , rName: Nothing
      , rule: [ NonTerminal (Right entry), Terminal EOF ]
      }
    rules' = rules <#> \{ pName, rName, rule } ->
      { pName: Right pName
      , rName: Just rName
      , rule: bimap Right Continue <$> rule
      }
  in
    { augmented: MkGrammar (entryRules <> rules')
    , eof: EOF
    , starts: entryRules <#> \{ pName, rName, rule } ->
      { pName
      , rName
      , rule: Zipper [] rule
      , lookbehind: Additive (one :: meta)
      , lookahead: []
      }
    }

fromSeed'
  :: forall meta nt r tok
   . Semiring meta
  => nt
  -> r
  -> tok
  -> Grammar meta nt r tok
  -> nt
  -> Augmented meta nt r tok
fromSeed' nt0 r0 tok0 (MkGrammar rules) entry =
  let
    rule0 = { pName: nt0, rName: r0, rule: [ NonTerminal entry, Terminal tok0 ] }
  in
    { augmented: MkGrammar ([ rule0 ] <> rules)
    , start: { pName: nt0, rName: r0, rule: Zipper [] rule0.rule, lookbehind: Additive one, lookahead: [] }
    , eof: tok0
    , entry
    }

calculateStates
  :: forall meta nt r tok
   . Semiring meta => Eq meta => Ord nt
  => Eq r
  => Ord tok
  => Grammar meta nt r tok
  -> StateItem meta nt r tok
  -> Array (State meta nt r tok)
calculateStates grammar start = closeStates grammar [ close grammar (minimizeState [ start ]) ]

calculateStatesMany
  :: forall meta nt r tok
   . Semiring meta => Eq meta => Ord nt
  => Eq r
  => Ord tok
  => Grammar meta nt r tok
  -> Array (StateItem meta nt r tok)
  -> Array (State meta nt r tok)
calculateStatesMany grammar starts = closeStates grammar $ starts <#>
  \start -> close grammar (minimizeState [ start ])

generate
  :: forall meta nt r tok
   . Semiring meta => Eq meta => Ord nt
  => Eq r
  => Ord tok
  => Grammar meta nt r tok
  -> nt
  -> Array (State meta (Maybe nt) (Maybe r) (OrEOF tok))
generate initial entry =
  let
    { augmented: grammar, start } = fromSeed initial entry
  in
    calculateStates grammar start

generate'
  :: forall meta nt r tok
   . Semiring meta => Eq meta => Ord nt
  => Eq r
  => Ord tok
  => nt
  -> r
  -> tok
  -> Grammar meta nt r tok
  -> nt
  -> Array (State meta nt r tok)
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

getResultIC :: forall air x y z. Stack x (ICST air y z) -> Maybe (ICST air y z)
getResultIC (Snoc (Snoc (Zero _) result@(IBranch _ _) _) (ILeaf _) _) = Just result
getResultIC _ = Nothing

getResultCM :: forall w x y z. Stack w (CST (Maybe x /\ Maybe y) (OrEOF z)) -> Maybe (CST (x /\ y) z)
getResultCM = getResultC >=> revertCST

getResultCM' :: forall w x y z. Stack w (CST (Either x x /\ Maybe y) (OrEOF z)) -> Maybe (CST (x /\ y) z)
getResultCM' = getResultC >=> revertCST'

getResultICM' :: forall air w x y z. Stack w (ICST air (Either x x /\ Maybe y) (OrEOF z)) -> Maybe (ICST air (x /\ y) z)
getResultICM' = getResultIC >=> revertICST'

revertCST :: forall x y z. CST (Maybe x /\ Maybe y) (OrEOF z) -> Maybe (CST (x /\ y) z)
revertCST (Leaf t) = Leaf <$> notEOF t
revertCST (Branch r cs) = Branch <$> bisequence r <*> traverse revertCST cs

revertCST' :: forall x y z. CST (Either x x /\ Maybe y) (OrEOF z) -> Maybe (CST (x /\ y) z)
revertCST' (Leaf t) = Leaf <$> notEOF t
revertCST' (Branch r cs) = Branch <$> bitraverse hush identity r <*> traverse revertCST' cs

revertICST' :: forall air x y z. ICST air (Either x x /\ Maybe y) (OrEOF z) -> Maybe (ICST air (x /\ y) z)
revertICST' (ILeaf t) = ILeaf <$> notEOF t
revertICST' (IAir air) = Just (IAir air)
revertICST' (IBranch r cs) = IBranch <$> bitraverse hush identity r <*> traverse revertICST' cs


parseIntoGrammar
  :: forall meta t
   . Array
       { pName :: String
       , rName :: t
       , rule :: String
       }
  -> Grammar meta NonEmptyString t CodePoint
parseIntoGrammar = compose MkGrammar $
  Array.mapMaybe (\r -> NES.fromString r.pName <#> \p -> r { pName = p })
    >>> parseDefinitions

parseDefinitions :: forall meta r.
  Array
    { pName :: NonEmptyString
    , rule :: String
    | r
    }
  -> Array
       { pName :: NonEmptyString
       , rule :: Array (Part meta NonEmptyString CodePoint)
       | r
       }
parseDefinitions grammar =
  let
    nts = longestFirst (grammar <#> _.pName)
    p = parseDefinition nts
  in
    grammar <#> \r -> r { rule = p r.rule }





verifyTokens :: forall meta nt r tok. Ord tok => Grammar meta nt r tok -> List tok -> Maybe (List tok)
verifyTokens = traverse <<< verifyToken

verifyToken :: forall meta nt r tok. Ord tok => Grammar meta nt r tok -> tok -> Maybe tok
verifyToken = gatherTokens >>> \toks tok ->
  if Set.member tok toks then Just tok else Nothing

gatherTokens :: forall meta nt r tok. Ord tok => Grammar meta nt r tok -> Set tok
gatherTokens (MkGrammar rules) = rules # Array.foldMap \{ rule } ->
  Array.mapMaybe unTerminal rule # Set.fromFoldable

gatherTokens_ :: forall meta nt r tok. Ord tok => Grammar meta nt r tok -> Map tok (NonEmptyArray r)
gatherTokens_ (MkGrammar rules) = unwrap $ rules # Array.foldMap \{ rule, rName } ->
  Array.mapMaybe unTerminal rule # foldMap \tok ->
    SemigroupMap $ Map.singleton tok (NEA.singleton rName)

gatherTokens' :: forall meta nt r tok. Ord tok => Grammar meta nt r tok -> Array tok
gatherTokens' (MkGrammar rules) = Array.nub $ rules # Array.foldMap \{ rule } ->
  Array.mapMaybe unTerminal rule

gatherNonTerminals :: forall meta nt r tok. Ord nt => Grammar meta nt r tok -> Set nt
gatherNonTerminals (MkGrammar rules) = rules # Array.foldMap \{ rule, pName } ->
  Array.mapMaybe unNonTerminal rule # NonEmpty pName # Set.fromFoldable

gatherNonTerminals_ :: forall meta nt r tok. Ord nt => Grammar meta nt r tok -> Map nt (NonEmptyArray r)
gatherNonTerminals_ (MkGrammar rules) = unwrap $ rules # Array.foldMap \{ rule, pName, rName } ->
  Array.mapMaybe unNonTerminal rule # NonEmpty pName # foldMap \nt ->
    SemigroupMap $ Map.singleton nt (NEA.singleton rName)

gatherNonTerminals' :: forall meta nt r tok. Ord nt => Grammar meta nt r tok -> Array nt
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

addEOF'' :: forall tok. Array tok -> Array (Maybe tok)
addEOF'' i = map Just i <> [ Nothing ]

fromString' :: SAugmented -> String -> Maybe (List CodePoint)
fromString' grammar = String.toCodePointArray
  >>> Array.toUnfoldable
  >>> verifyTokens grammar.augmented




numberStatesBy
  :: forall s meta nt r tok
   . Semiring meta
  => Eq meta
  => Ord nt
  => Eq r
  => Ord tok
  => (Int -> s)
  -> Grammar meta nt r tok
  -> Array (State meta nt r tok)
  -> Either (Array (StateItem meta nt r tok)) (States s meta nt r tok)
numberStatesBy ix grammar states = map States $ states #
  traverseWithIndex \i items ->
    let
      findState seed = note seed $ map ix $
        Array.findIndex (eq (close grammar (minimizeState seed))) states
      next = nextSteps items
      reductions = map Reduces <$> getReductions items
    in
      ado
        shifts <- traverse (traverse (map Shift <<< findState)) $ next.terminal
        receive <- traverse (findState <<< snd) $ unwrap $ next.nonTerminal
        in
          { sName: ix i
          , items
          , advance: shifts <> reductions
          , receive
          }

statesNumberedBy
  :: forall s meta nt r tok
   . Semiring meta => Eq meta => Ord nt
  => Eq r
  => Ord tok
  => (Int -> s)
  -> Grammar meta nt r tok
  -> nt
  -> s /\ States s meta (Maybe nt) (Maybe r) (OrEOF tok)
statesNumberedBy ix initial entry =
  let { augmented: grammar, start } = fromSeed initial entry in
  ix 0 /\ do
    either (const (States [])) identity $ numberStatesBy ix grammar $
      calculateStates grammar start

statesNumberedByMany
  :: forall f s meta nt r tok
   . Semiring meta => Eq meta => Foldable f
  => Ord nt
  => Eq r
  => Ord tok
  => (Int -> s)
  -> Grammar meta nt r tok
  -> f nt
  -> Array (nt /\ s) /\ States s meta (Either nt nt) (Maybe r) (OrEOF tok)
statesNumberedByMany ix initial entrief =
  let entries = Array.fromFoldable entrief in
  let { augmented: grammar, starts } = fromSeeds initial entries in
  case numberStatesBy ix grammar $ calculateStatesMany grammar starts of
    Left _ -> [] /\ States []
    Right states -> mapWithIndex (\i nt -> nt /\ ix i) entries /\ states

toAdvance :: forall meta nt tok. Zipper meta nt tok -> Maybe (Part meta nt tok)
toAdvance (Zipper _ after) = Array.head after

toAdvanceTo
  :: forall s meta nt r tok
   . Ord nt
  => Ord tok
  => StateInfo s meta nt r tok
  -> Zipper meta nt tok
  -> Maybe s
toAdvanceTo { advance, receive } = noInterTerminalz >>> toAdvance >=> case _ of
  NonTerminal nt -> Map.lookup nt receive
  Terminal tok -> Map.lookup tok (unwrap advance) >>= snd >>> unShift
  InterTerminal vd -> absurd vd

getReduction :: forall meta nt r tok. Semiring meta =>Ord tok => StateItem meta nt r tok -> SemigroupMap tok (Additive meta /\ Fragment meta nt tok /\ nt /\ r)
getReduction { pName, rName, rule: Zipper rule [], lookbehind: Additive lookbehind, lookahead } =
  SemigroupMap $ Map.fromFoldable $ lookahead <#> \(Tuple meta tok) ->
    tok /\ (Additive if Array.null rule then (lookbehind * meta) else meta) /\ rule /\ pName /\ rName
getReduction _ = SemigroupMap $ Map.empty

getReductions :: forall meta nt r tok. Ord tok => Semiring meta => State meta nt r tok -> SemigroupMap tok (Additive meta /\ NonEmptyArray (Fragment meta nt tok /\ nt /\ r))
getReductions (State items) = items # foldMap \item ->
  map NEA.singleton <$> getReduction item



findNT ::
  forall meta nt tok.
    Semiring meta =>
  Zipper meta nt tok ->
  Maybe
    { interterminal :: meta
    , nonterminal :: nt
    , following :: Array (Tuple meta nt)
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
  { following :: Array (Tuple meta nt), continue :: Tuple meta (Maybe tok) }
preview = scanFollowing Nil (one :: meta)
  where
  scanFollowing acc interterm (InterTerminal meta : tail) =
    scanFollowing acc (interterm * meta) tail
  scanFollowing acc interterm (NonTerminal nt : tail) =
    scanFollowing (Tuple interterm nt : acc) (one :: meta) tail
  scanFollowing acc interterm (Terminal tok : _done) =
    { following: Array.reverse (List.toUnfoldable acc), continue: Tuple interterm (Just tok) }
  scanFollowing acc interterm Nil =
    { following: Array.reverse (List.toUnfoldable acc), continue: Tuple interterm Nothing }

continueOn :: forall meta tok. Semiring meta => Tuple meta (Maybe tok) -> Lookahead meta tok -> Lookahead meta tok
continueOn continue lookahead = case continue of
  Tuple meta (Just tok) -> [ Tuple meta tok ]
  Tuple meta Nothing -> lmap (coerce (meta * _)) <$> lookahead

startRules :: forall meta nt r tok. Eq nt => Grammar meta nt r tok -> nt -> (Additive meta -> Lookahead meta tok -> Array (StateItem meta nt r tok))
startRules (MkGrammar rules) p =
  let
    filtered = Array.filter (\{ pName } -> pName == p) rules
  in
    \lookbehind lookahead -> filtered <#> \{ pName, rName, rule } -> { pName, rName, rule: Zipper [] rule, lookbehind, lookahead }

closeItem :: forall meta nt r tok. Semiring meta => Eq meta => Eq nt => Eq tok => Grammar meta nt r tok -> StateItem meta nt r tok -> Array (StateItem meta nt r tok)
closeItem grammar item = case findNT item.rule of
  Nothing -> []
  Just { interterminal, nonterminal: p, following, continue } ->
    startRules grammar p (Additive interterminal) $
      Array.nubEq $ firsts grammar following (continueOn continue item.lookahead)

close1 :: forall meta nt r tok. Semiring meta => Eq meta => Eq nt => Eq tok => Grammar meta nt r tok -> State meta nt r tok -> Array (StateItem meta nt r tok)
close1 grammar (State items) = closeItem grammar =<< items

close
  :: forall meta nt r tok
   . Semiring meta => Eq meta => Eq r
  => Eq nt
  => Eq tok
  => Grammar meta nt r tok
  -> State meta nt r tok
  -> State meta nt r tok
close grammar state0 =
  let
    state' = close1 grammar state0
  in
    if Array.null state' then state0
    else
      let
        state = minimizeStateCat state0 state'
      in
        if unwrap state == unwrap state0 then state0 else close grammar state

firsts :: forall meta nt r tok. Semiring meta => Eq nt => Grammar meta nt r tok -> Array (Tuple meta nt) -> Lookahead meta tok -> Lookahead meta tok
firsts (MkGrammar rules0) ps0 lookahead0 = readyset rules0 (one :: meta) ps0 lookahead0
  where
  readyset rules meta ps lookahead = case Array.uncons ps of
    Just { head: Tuple meta' head, tail } -> go rules (meta * meta') head tail lookahead
    _ ->  lmap (coerce (meta * _)) <$> lookahead
  go rules meta p ps lookahead =
    let
      { yes: matches, no: rules' } = Array.partition (\{ pName } -> pName == p) rules
    in
      matches >>= _.rule >>> Array.toUnfoldable >>> preview >>> \{ following, continue } ->
        -- (p : following continue) (ps lookahead)
        case continue of
          Tuple meta' (Just tok) -> readyset rules' (meta * meta') following [ Tuple (one :: meta) tok ]
          Tuple meta' Nothing -> readyset rules' (meta * meta') (following <> ps) lookahead

nextStep
  :: forall meta nt r tok
   . Semiring meta => StateItem meta nt r tok
  -> { nonTerminal :: SemigroupMap nt (Additive meta /\ Array (StateItem meta nt r tok))
     , terminal :: SemigroupMap tok (Additive meta /\ Array (StateItem meta nt r tok))
     }
nextStep item@{ rule: Zipper before after } = go (one :: meta) after
  where
  go acc scanning = case Array.uncons scanning of
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
          , lookbehind: Additive acc
          , lookahead: item.lookahead
          }
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

nextSteps
  :: forall meta nt r tok
   . Semiring meta => Ord nt
  => Ord tok
  => State meta nt r tok
  -> { nonTerminal :: SemigroupMap nt (Additive meta /\ Array (StateItem meta nt r tok))
     , terminal :: SemigroupMap tok (Additive meta /\ Array (StateItem meta nt r tok))
     }
nextSteps (State items) = Array.foldMap nextStep items

-- Meant to handle nondeterminacy of shifts, but not reduces
nextSteps'
  :: forall meta nt r tok
   . Semiring meta => Ord nt
  => Ord tok
  => State meta nt r tok
  -> Array (Part meta nt tok /\ Additive meta /\ Array (StateItem meta nt r tok))
nextSteps' state =
  let
    { nonTerminal: SemigroupMap nts, terminal: SemigroupMap toks } = nextSteps state
  in
    lmap Terminal <$> Map.toUnfoldable toks <|> lmap NonTerminal <$> Map.toUnfoldable nts

newStates'
  :: forall meta nt r tok
   . Semiring meta => Eq meta => Ord nt
  => Eq r
  => Ord tok
  => Grammar meta nt r tok
  -> State meta nt r tok
  -> Array (State meta nt r tok)
newStates' grammar state =
  close grammar <<< minimizeState <<< snd <<< snd <$> nextSteps' state

closeStates1
  :: forall meta nt r tok
   . Semiring meta => Eq meta => Ord nt
  => Eq r
  => Ord tok
  => Grammar meta nt r tok
  -> Array (State meta nt r tok)
  -> Array (State meta nt r tok)
closeStates1 grammar states = nubEqCat states (Array.nubEq (states >>= newStates' grammar))

closeStates
  :: forall meta nt r tok
   . Semiring meta => Eq meta => Ord nt
  => Eq r
  => Ord tok
  => Grammar meta nt r tok
  -> Array (State meta nt r tok)
  -> Array (State meta nt r tok)
closeStates grammar states =
  let
    states' = closeStates1 grammar states
  in
    if states' == states then states else closeStates grammar states'




indexStates :: forall s meta nt r tok. Ord s => States s meta nt r tok -> StateIndex s
indexStates states = Map.lookup <@> indexStates' states

indexStates' :: forall s meta nt r tok. Ord s => States s meta nt r tok -> Map s Int
indexStates' (States states) =
  Map.fromFoldable $ mapWithIndex (\i { sName } -> sName /\ i) states

toTable
  :: forall s meta nt r tok
   . Semiring meta => Ord s
  => Ord nt
  => Eq r
  => Ord tok
  => States s meta nt r tok
  -> StateIndex s
  -> Proto.Table s (nt /\ r) tok (CST (nt /\ r) tok)
toTable (States states) tabulated =
  let
    lookupState s = tabulated s >>= Array.index states
    lookupAction tok { advance: SemigroupMap m } = snd <$> Map.lookup tok m
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
          InterTerminal vd -> absurd vd
      in
        Array.foldM take1 ([] /\ stack0) (Array.reverse parsed) >>= \(taken /\ stack) ->
          Snoc stack (Branch r taken) <$> goto r (topOf stack)
    goto (p /\ _) s' = lookupState s' >>= _.receive >>> Map.lookup p
  in
    Proto.Table
      { promote: Leaf
      , step: \s tok -> lookupState s >>= lookupAction tok >>= decide >== map snd
      , goto: \r stack ->
          lookupState (topOf stack) >>= \state -> do
            lookupReduction r state >>= noInterTerminals >>> takeStack r stack
      }

toTable'
  :: forall s meta nt r tok
   . Semiring meta => Ord s
  => Ord nt
  => Eq r
  => Ord tok
  => States s meta nt r tok
  -> StateIndex s
  -> Proto.Table s (nt /\ r) tok (Either tok (AST (nt /\ r)))
toTable' (States states) tabulated =
  let
    lookupState s = tabulated s >>= Array.index states
    lookupAction tok { advance: SemigroupMap m } = snd <$> Map.lookup tok m
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
          InterTerminal vd -> absurd vd
      in
        Array.foldM take1 ([] /\ stack0) (Array.reverse parsed) >>= \(taken /\ stack) ->
          Snoc stack (Right (Layer r (Array.mapMaybe hush taken))) <$> goto r (topOf stack)
    goto (p /\ _) s' = lookupState s' >>= _.receive >>> Map.lookup p
  in
    Proto.Table
      { promote: Left
      , step: \s tok -> lookupState s >>= lookupAction tok >>= decide >== map snd
      , goto: \r stack ->
          lookupState (topOf stack) >>= \state -> do
            lookupReduction r state >>= noInterTerminals >>> takeStack r stack
      }


longestFirst :: Array NonEmptyString -> Array NonEmptyString
longestFirst = Array.nub >>> Array.sortBy (flip compare `on` NES.length <> compare)

parseDefinition :: forall meta. Array NonEmptyString -> String -> Fragment meta NonEmptyString CodePoint
parseDefinition nts s = case String.uncons s of
  Just { head: c, tail: s' } ->
    case recognize nts s of
      Just nt -> [ NonTerminal nt ] <> parseDefinition nts (String.drop (NES.length nt) s)
      Nothing -> [ Terminal c ] <> parseDefinition nts s'
  Nothing -> []

unParseDefinition :: forall any. Fragment any NonEmptyString CodePoint -> String
unParseDefinition = foldMap case _ of
  NonTerminal nt -> NES.toString nt
  Terminal tok -> String.singleton tok
  InterTerminal _ -> ""


recognize :: Array NonEmptyString -> String -> Maybe NonEmptyString
recognize nts s = nts # Array.find \nt ->
  String.take (NES.length nt) s == NES.toString nt
