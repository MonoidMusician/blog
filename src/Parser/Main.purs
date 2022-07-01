module Parser.Main where

import Prelude

import Bolson.Core (Child(..), dyn, envy, fixed)
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Monad.ST.Class (class MonadST)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trampoline (Trampoline, runTrampoline)
import Control.Plus (empty)
import Data.Array ((..), (!!))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Compactable (compact)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either, fromRight', hush, note)
import Data.Filterable (filter)
import Data.Foldable (foldMap, for_, oneOf, oneOfMap)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (floor)
import Data.List (List)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number (e, pi)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (CodePoint, codePointFromChar)
import Data.String as String
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (mapAccumL, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant as Variant
import Deku.Attribute (class Attr, Attribute, Cb, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (class Korok, Domable, Nut, bus, bussed, insert, remove, vbussed)
import Deku.Core as DC
import Deku.DOM as D
import Deku.Listeners (click, slider)
import Effect (Effect)
import Effect.Class.Console as Log
import Effect.Now (now)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (step)
import FRP.Event (class IsEvent, AnEvent, bang, filterMap, fold, keepLatest, mapAccum, memoize, sampleOn, subscribe, sweep, toEvent, withLast)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class (biSampleOn)
import FRP.Event.Time (withTime)
import FRP.Event.VBus (V)
import FRP.Rate (Beats(..), RateInfo, timeFromRate)
import FRP.SampleJIT (readersT, sampleJITE)
import FRP.SelfDestruct (selfDestruct)
import Parser.Proto (ParseSteps(..), Stack(..), parseSteps, topOf)
import Parser.Proto as Proto
import Parser.ProtoG8 as G8
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

data StepAction = Initial | Toggle | Slider | Play

type Nuts =
  forall s m lock payload
   . DC.Korok s m
  => Array (DC.Domable m lock payload)

type Nutss =
  forall s m lock payload
   . DC.Korok s m
  => Array (Array (DC.Domable m lock payload))

newtype Grammar nt r tok = MkGrammar
  ( Array
      { pName :: nt
      , rName :: r
      , rule :: Fragment nt tok
      }
  )

derive instance newtypeGrammar :: Newtype (Grammar nt r tok) _

type Augmented nt r tok =
  { augmented :: Grammar nt r tok
  , start :: StateItem nt r tok
  , eof :: tok
  }

type SAugmented = Augmented NonEmptyString String CodePoint

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

getResult :: forall x y z. Stack x (Either y z) -> Maybe z
getResult (Snoc (Snoc (Zero _) (Right result) _) (Left _) _) = Just result
getResult _ = Nothing

g8Grammar :: Grammar G8.Sorts G8.Rule G8.Tok
g8Grammar = MkGrammar
  [ { pName: G8.RE, rName: G8.RE1, rule: [ Terminal G8.LParen, NonTerminal G8.RL, Terminal G8.RParen ] }
  , { pName: G8.RE, rName: G8.RE2, rule: [ Terminal G8.X ] }
  , { pName: G8.RL, rName: G8.RL1, rule: [ NonTerminal G8.RE ] }
  , { pName: G8.RL, rName: G8.RL2, rule: [ NonTerminal G8.RL, Terminal G8.Comma, NonTerminal G8.RE ] }
  ]

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
    >>> \grammar ->
      let
        nts = longestFirst (grammar <#> _.pName)
        p = parseDefinition nts
      in
        grammar <#> \r -> r { rule = p r.rule }

exGrammar :: SGrammar
exGrammar = parseIntoGrammar
  [ { pName: "E", rName: "E1", rule: "(L)" }
  , { pName: "E", rName: "E2", rule: "x" }
  , { pName: "L", rName: "L1", rule: "E" }
  , { pName: "L", rName: "L2", rule: "L,E" }
  ]

g8Seed :: Augmented (Maybe G8.Sorts) (Maybe G8.Rule) (Maybe G8.Tok)
g8Seed = fromSeed g8Grammar G8.RE

defaultTopName :: NonEmptyString
defaultTopName = (unsafePartial (fromJust (NES.fromString "TOP")))

defaultTopRName :: String
defaultTopRName = "TOP"

defaultEOF :: CodePoint
defaultEOF = (codePointFromChar '␄')

exSeed :: SAugmented
exSeed = fromSeed' defaultTopName defaultTopRName defaultEOF exGrammar (unsafePartial (fromJust (NES.fromString "E")))

g8Generated :: forall a. a -> Array (State (Maybe G8.Sorts) (Maybe G8.Rule) (Maybe G8.Tok))
g8Generated _ = generate g8Grammar G8.RE

exGenerated :: forall t. t -> Array (State NonEmptyString String CodePoint)
exGenerated _ = generate' defaultTopName defaultTopRName defaultEOF exGrammar (unsafePartial (fromJust (NES.fromString "E")))

g8States :: forall a. a -> States Int (Maybe G8.Sorts) (Maybe G8.Rule) (Maybe G8.Tok)
g8States a = fromRight' (\_ -> unsafeCrashWith "state generation did not work")
  (numberStates (add 1) g8Seed.augmented (g8Generated a))

exStates :: forall t. t -> States Int NonEmptyString String CodePoint
exStates a = fromRight' (\_ -> unsafeCrashWith "state generation did not work")
  (numberStates (add 1) exSeed.augmented (exGenerated a))

g8Table :: forall a. a -> Proto.Table Int (Maybe G8.Sorts /\ Maybe G8.Rule) (Maybe G8.Tok) (CST (Maybe G8.Sorts /\ Maybe G8.Rule) (Maybe G8.Tok))
g8Table = toTable <<< g8States

exTable :: forall t. t -> Proto.Table Int (NonEmptyString /\ String) CodePoint (CST (NonEmptyString /\ String) CodePoint)
exTable = toTable <<< exStates

g8Table' :: forall a. a -> Proto.Table Int (Maybe G8.Sorts /\ Maybe G8.Rule) (Maybe G8.Tok) (Either (Maybe G8.Tok) (AST (Maybe G8.Sorts /\ Maybe G8.Rule)))
g8Table' = toTable' <<< g8States

exTable' :: forall t. t -> Proto.Table Int (NonEmptyString /\ String) CodePoint (Either CodePoint (AST (NonEmptyString /\ String)))
exTable' = toTable' <<< exStates

g8FromString :: String -> Maybe (List (Maybe G8.Tok))
g8FromString = G8.g8FromString >>> map map map case _ of
  G8.EOF -> Nothing
  t -> Just t

verifyTokens :: forall nt r tok. Ord tok => Grammar nt r tok -> List tok -> Maybe (List tok)
verifyTokens = traverse <<< verifyToken

verifyToken :: forall nt r tok. Ord tok => Grammar nt r tok -> tok -> Maybe tok
verifyToken = gatherTokens >>> \toks tok ->
  if Set.member tok toks then Just tok else Nothing

gatherTokens :: forall nt r tok. Ord tok => Grammar nt r tok -> Set tok
gatherTokens (MkGrammar rules) = rules # Array.foldMap \{ rule } ->
  Array.mapMaybe unTerminal rule # Set.fromFoldable

exFromString :: String -> Maybe (List CodePoint)
exFromString = String.toCodePointArray >>> flip append [ defaultEOF ] >>> Array.toUnfoldable >>> verifyTokens exSeed.augmented

fromString :: SAugmented -> String -> Maybe (List CodePoint)
fromString grammar = String.toCodePointArray >>> flip append [ grammar.eof ]
  >>> Array.toUnfoldable
  >>> verifyTokens grammar.augmented

type SGrammar = Grammar NonEmptyString String CodePoint
data Part nt tok = NonTerminal nt | Terminal tok

derive instance eqPart :: (Eq nt, Eq tok) => Eq (Part nt tok)
derive instance ordPart :: (Ord nt, Ord tok) => Ord (Part nt tok)
derive instance genericPart :: Generic (Part state tok) _
instance showPart :: (Show nt, Show tok) => Show (Part nt tok) where
  show x = genericShow x

derive instance functorPart :: Functor (Part nt)
instance bifunctorPart :: Bifunctor Part where
  bimap f _ (NonTerminal nt) = NonTerminal (f nt)
  bimap _ g (Terminal tok) = Terminal (g tok)

type SPart = Part NonEmptyString CodePoint
type Fragment nt tok = Array (Part nt tok)
type SFragment = Fragment String CodePoint

data Zipper nt tok = Zipper (Fragment nt tok) (Fragment nt tok)

derive instance eqZipper :: (Eq nt, Eq tok) => Eq (Zipper nt tok)
derive instance ordZipper :: (Ord nt, Ord tok) => Ord (Zipper nt tok)
derive instance genericZipper :: Generic (Zipper state tok) _
instance showZipper :: (Show nt, Show tok) => Show (Zipper nt tok) where
  show x = genericShow x

type SZipper = Zipper NonEmptyString CodePoint
newtype State nt r tok = State (Array (StateItem nt r tok))

instance eqState :: (Eq nt, Eq r, Eq tok) => Eq (State nt r tok) where
  eq (State s1) (State s2) = s1 == s2 ||
    let
      State s1' = minimizeState s1
      State s2' = minimizeState s2
      State s12 = minimizeState (s1' <> s2')
      State s21 = minimizeState (s2' <> s1')
    in
      s1' == s12 && s2' == s21

instance ordState :: (Ord nt, Ord r, Ord tok) => Ord (State nt r tok) where
  compare (State s1) (State s2) = compare (deepSort s1) (deepSort s2)
    where
    deepSort = Array.sort <<< map \item ->
      item { lookahead = Array.sort item.lookahead }

derive instance genericState :: Generic (State nt r tok) _
instance showState :: (Show nt, Show r, Show tok) => Show (State nt r tok) where
  show = genericShow

instance semigroupState :: (Eq nt, Eq r, Eq tok) => Semigroup (State nt r tok) where
  append (State s1) (State s2) = minimizeState (s1 <> s2)

minimizeState :: forall nt r tok. Eq nt => Eq r => Eq tok => Array (StateItem nt r tok) -> State nt r tok
minimizeState = compose State $ [] # Array.foldl \items newItem ->
  let
    accumulate :: Boolean -> StateItem nt r tok -> { accum :: Boolean, value :: StateItem nt r tok }
    accumulate alreadyFound item =
      if item.rName == newItem.rName && item.rule == newItem.rule then { accum: true, value: item { lookahead = Array.nubEq (item.lookahead <> newItem.lookahead) } }
      else { accum: alreadyFound, value: item }
    { accum: found, value: items' } =
      mapAccumL accumulate false items
  in
    if found then items' else items' <> [ newItem ]

type SState = State NonEmptyString String CodePoint
type Lookahead tok = Array tok
type StateItem nt r tok =
  { rName :: r
  , pName :: nt
  , rule :: Zipper nt tok
  , lookahead :: Lookahead tok
  }

type SStateItem = StateItem NonEmptyString String CodePoint

data ShiftReduce s r
  = Shift s
  | Reduces (NonEmptyArray r)
  | ShiftReduces s (NonEmptyArray r)

derive instance functorShiftReduce :: Functor (ShiftReduce s)
instance semigroupShiftReduce :: Semigroup (ShiftReduce s r) where
  -- We don't expect to see two shifts
  append (Shift s) (Shift _) = Shift s
  append (Shift s) (ShiftReduces _ rs) = ShiftReduces s rs
  append (ShiftReduces s rs) (Shift _) = ShiftReduces s rs
  append (ShiftReduces s rs) (ShiftReduces _ rs') = ShiftReduces s (rs <> rs')
  append (Shift s) (Reduces rs) = ShiftReduces s rs
  append (Reduces rs) (Shift s) = ShiftReduces s rs
  append (Reduces rs) (Reduces rs') = Reduces (rs <> rs')
  append (ShiftReduces s rs) (Reduces rs') = ShiftReduces s (rs <> rs')
  append (Reduces rs) (ShiftReduces s rs') = ShiftReduces s (rs <> rs')

type StateInfo s nt r tok =
  { sName :: s
  , items :: State nt r tok
  , advance :: SemigroupMap tok (ShiftReduce s (nt /\ r))
  , receive :: Map nt s
  }

newtype States s nt r tok = States
  (Array (StateInfo s nt r tok))

derive instance newtypeStates :: Newtype (States s nt r tok) _

type SStates = States Int NonEmptyString String CodePoint

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

getReduction :: forall nt r tok. Ord tok => StateItem nt r tok -> SemigroupMap tok (nt /\ r)
getReduction { pName, rName, rule: Zipper _ [], lookahead } =
  SemigroupMap $ Map.fromFoldable $ (/\) <$> lookahead <@> (pName /\ rName)
getReduction _ = SemigroupMap $ Map.empty

getReductions :: forall nt r tok. Ord tok => State nt r tok -> SemigroupMap tok (NonEmptyArray (nt /\ r))
getReductions (State items) = items # foldMap \item ->
  NEA.singleton <$> getReduction item

isNonTerminal :: forall nt tok. Part nt tok -> Boolean
isNonTerminal (NonTerminal _) = true
isNonTerminal _ = false

isTerminal :: forall nt tok. Part nt tok -> Boolean
isTerminal (Terminal _) = true
isTerminal _ = false

unNonTerminal :: forall nt tok. Part nt tok -> Maybe nt
unNonTerminal (NonTerminal nt) = Just nt
unNonTerminal _ = Nothing

unTerminal :: forall nt tok. Part nt tok -> Maybe tok
unTerminal (Terminal t) = Just t
unTerminal _ = Nothing

unSPart :: SPart -> String
unSPart = NES.toString <<< unSPart'

unSPart' :: SPart -> NonEmptyString
unSPart' (Terminal t) = NES.singleton t
unSPart' (NonTerminal nt) = nt

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

data CST r tok
  = Leaf tok
  | Branch r (Array (CST r tok))

derive instance genericCST :: Generic (CST r tok) _
instance showCST :: (Show r, Show tok) => Show (CST r tok) where
  show x = genericShow x

data AST r = Layer r (Array (AST r))

derive instance functorAST :: Functor AST
derive instance genericAST :: Generic (AST r) _
instance showAST :: (Show r) => Show (AST r) where
  show x = genericShow x

prune :: forall r tok. CST r tok -> Either tok (AST r)
prune (Leaf tok) = Left tok
prune (Branch r rec) = Right (Layer r (Array.mapMaybe (hush <<< prune) rec))

toTable
  :: forall s nt r tok
   . Ord s
  => Ord nt
  => Eq r
  => Ord tok
  => States s nt r tok
  -> Proto.Table s (nt /\ r) tok (CST (nt /\ r) tok)
toTable (States states) =
  let
    tabulated = Map.fromFoldable $ mapWithIndex (\i { sName } -> sName /\ i) states
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
              Just $ (taken <> [ v ]) /\ stack'
            _ -> unsafeCrashWith "expected token on stack"
          NonTerminal nt -> case stack of
            Snoc stack' v@(Branch (p /\ _) _) _ | p == nt ->
              Just $ (taken <> [ v ]) /\ stack'
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
  -> Proto.Table s (nt /\ r) tok (Either tok (AST (nt /\ r)))
toTable' (States states) =
  let
    tabulated = Map.fromFoldable $ mapWithIndex (\i { sName } -> sName /\ i) states
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
              Just $ (taken <> [ v ]) /\ stack'
            _ -> unsafeCrashWith "expected token on stack"
          NonTerminal nt -> case stack of
            Snoc stack' v@(Right (Layer (p /\ _) _)) _ | p == nt ->
              Just $ (taken <> [ v ]) /\ stack'
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

-- Prefer shifts because they are unique
decide :: forall s r. ShiftReduce s r -> Maybe (Either s r)
decide (Shift s) = Just (Left s)
decide (ShiftReduces s _) = Just (Left s)
decide (Reduces r) = if NEA.length r == 1 then Just (Right (NEA.head r)) else Nothing

longestFirst :: Array NonEmptyString -> Array NonEmptyString
longestFirst = Array.nub >>> Array.sortBy (flip compare `on` NES.length <> compare)

parseDefinition :: Array NonEmptyString -> String -> Fragment NonEmptyString CodePoint
parseDefinition nts s = case String.uncons s of
  Just { head: c, tail: s' } ->
    case recognize nts s of
      Just nt -> [ NonTerminal nt ] <> parseDefinition nts (String.drop (NES.length nt) s)
      Nothing -> [ Terminal c ] <> parseDefinition nts s'
  Nothing -> []

recognize :: Array NonEmptyString -> String -> Maybe NonEmptyString
recognize nts s = nts # Array.find \nt ->
  String.take (NES.length nt) s == NES.toString nt

bangAttr :: forall m a b e. Applicative m => Attr e a b => a -> b -> AnEvent m (Attribute e)
bangAttr a b = bang (a := b)

infix 5 bangAttr as !:=

maybeAttr :: forall m a b e. Applicative m => Attr e a b => a -> Maybe b -> AnEvent m (Attribute e)
maybeAttr a (Just b) = bang (a := b)
maybeAttr _ Nothing = empty

infix 5 maybeAttr as ?:=

mapAttr :: forall m a b e. Functor m => Attr e a b => a -> m b -> m (Attribute e)
mapAttr a b = (a := _) <$> b

infix 5 mapAttr as <:=>

withValue :: (String -> Effect Unit) -> Cb
withValue fn = cb \e -> for_
  ( target e
      >>= fromEventTarget
  )
  (value >=> fn)

input :: String -> String -> (String -> Effect Unit) -> Nut
input placeholder initialValue onInput =
  D.input
    ( oneOf
        [ inputClass
        , D.Placeholder <:=> if placeholder == "" then empty else bang placeholder
        , D.Value <:=> if initialValue == "" then empty else bang initialValue
        , D.OnInput !:= withValue onInput
        ]
    )
    []

buttonClass :: forall m e. Applicative m => Attr e D.Class String => AnEvent m (Attribute e)
buttonClass = D.Class !:= "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"

arrowClass :: forall m e. Applicative m => Attr e D.Class String => AnEvent m (Attribute e)
arrowClass = D.Class !:= "bg-green-500 hover:bg-green-700 text-white font-bold"

inputClass :: forall m e. Applicative m => Attr e D.Class String => AnEvent m (Attribute e)
inputClass = D.Class !:=
  "shadow appearance-none border rounded py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"

type Header nt tok = Array tok /\ Array nt

getHeader :: forall s nt r tok. Ord nt => Ord tok => States s nt r tok -> Header nt tok
getHeader (States states) = bimap Array.nub Array.nub $
  states # foldMap \{ items: State items } -> items # foldMap \item ->
    ([] /\ [ item.pName ]) <> foldZipper fromPart item.rule
  where
  foldZipper f (Zipper l r) = foldMap f l <> foldMap f r
  fromPart (NonTerminal nt) = [] /\ [ nt ]
  fromPart (Terminal tok) = [ tok ] /\ []

renderParseTable
  :: SStates
  -> Nut
renderParseTable (States states) =
  let
    terminals /\ nonTerminals = getHeader (States states)
    renderTerminals x = renderTok mempty x
    renderNonTerminals x = renderNT mempty x
    renderShiftReduce Nothing = text_ ""
    renderShiftReduce (Just (Shift s)) = fixed [ renderCmd mempty "s", renderSt mempty s ]
    renderShiftReduce (Just (Reduces rs)) = fixed $ rs # foldMap \r -> [ renderCmd mempty "r", renderRule mempty r ]
    renderShiftReduce (Just (ShiftReduces s rs)) = fixed $
      [ renderCmd mempty "s", renderSt mempty s ] <> (rs # foldMap \r -> [ renderCmd mempty "r", renderRule mempty r ])
    cols state =
      let
        forTerminal tok = map snd <$> Map.lookup tok (unwrap state.advance)
        forNonTerminal nt = Shift <$> Map.lookup nt state.receive
      in
        map forTerminal terminals <> map forNonTerminal nonTerminals
    header = D.tr_ $ map (D.th_ <<< pure) $
      [ text_ "" ] <> map renderTerminals terminals <> map renderNonTerminals nonTerminals
    rows = states <#> \state -> D.tr_
      $ Array.cons (D.th_ [ renderSt mempty state.sName ])
      $
        map (D.td_ <<< pure <<< renderShiftReduce) (cols state)
  in
    D.table (D.Class !:= "parse-table")
      [ fixed
          [ D.colgroup_ [ D.col_ [] ]
          , D.colgroup_ $ terminals $> D.col_ []
          , D.colgroup_ $ nonTerminals $> D.col_ []
          ]
      , D.thead_ [ header ]
      , D.tbody_ rows
      ]

type StartingTick = Boolean

type ParsedUIAction = V
  ( toggleLeft :: Unit
  , toggleRight :: Unit
  , slider :: Number
  , rate :: Number
  , startState :: Maybe (Effect Unit)
  , animationTick :: StartingTick /\ RateInfo
  )

data TodoAction = Prioritize | Delete

showStack :: SStack -> Nut
showStack i = D.span (D.Class !:= "stack") (go i)
  where
  go (Zero state) = [ D.sub_ [ renderSt mempty state ] ]
  go (Snoc stack tok state) = go stack
    <> [ renderStackItem tok ]
    <> [ D.sub_ [ renderSt mempty state ] ]

renderStackItem :: Either CodePoint (AST String) -> Nut
renderStackItem (Left x) = renderTok mempty x
renderStackItem (Right x) = renderAST x

renderAST :: AST String -> Nut
renderAST (Layer r []) = D.span (D.Class !:= "layer") [ renderRule mempty r ]
renderAST (Layer r cs) =
  D.span (D.Class !:= "layer")
    [ renderMeta mempty "("
    , renderRule mempty r
    , fixed $ cs # foldMap \c -> [ text_ " ", renderAST c ]
    , renderMeta mempty ")"
    ]

showMaybeStack :: Maybe (SStack) -> Nut
showMaybeStack Nothing = text_ "Parse error"
showMaybeStack (Just stack) = showStack stack

type SStack = Stack Int (Either CodePoint (AST String))
type SParseSteps = ParseSteps CodePoint (Stack Int (Either CodePoint (AST String)))

showMaybeParseSteps :: forall s m lock payload. Korok s m => Maybe SParseSteps -> SuperStack m (Domable m lock payload)
showMaybeParseSteps Nothing = pure (pure (text_ "Parse error"))
showMaybeParseSteps (Just stack) = showParseSteps stack

getVisibilityAndIncrement
  :: forall m s element
   . MonadST s m
  => Attr element D.Class String
  => SuperStack m (Int /\ AnEvent m (Attribute element))
getVisibilityAndIncrement = getVisibilityAndIncrement' ""

getVisibilityAndIncrement'
  :: forall m s element
   . MonadST s m
  => Attr element D.Class String
  => String
  -> SuperStack m (Int /\ AnEvent m (Attribute element))
getVisibilityAndIncrement' s = do
  n <- get
  put (n + 1)
  pure
    ( \f -> n /\
        ( f n <#> \v ->
            D.Class := (s <> if v then "" else " hidden")
        )
    )

showParseStep
  :: forall r s m lock payload
   . Korok s m
  => Either (Maybe SStack)
       { inputs :: List CodePoint
       , stack :: SStack
       | r
       }
  -> SuperStack m (Domable m lock payload)
showParseStep (Left Nothing) = do
  getVisibilityAndIncrement <#> map \(n /\ vi) ->
    D.div vi [ (text_ $ ("Step " <> show n <> ": ") <> "Parse error") ]
showParseStep (Left (Just v)) = do
  getVisibilityAndIncrement <#> map \(n /\ vi) ->
    case getResult v of
      Just r ->
        D.div vi [ text_ $ ("Step the last: "), renderAST r ]
      _ ->
        D.div vi [ text_ $ ("Step the last: ") <> "Something went wrong" ]
showParseStep (Right { stack, inputs }) = do
  getVisibilityAndIncrement' "flex justify-between" <#> map \(n /\ vi) ->
    D.div vi [ D.div_ [ text_ ("Step " <> show n <> ": "), showStack stack ], D.div_ (foldMap (\x -> [ renderTok mempty x ]) inputs) ]

type SuperStack m a = StateT Int Trampoline ((Int -> AnEvent m Boolean) -> a)

showParseSteps
  :: forall s m lock payload
   . Korok s m
  => SParseSteps
  -> SuperStack m (Domable m lock payload)
showParseSteps i = map fixed <$> (go i)
  where
  go =
    let
      s v = showParseStep v
    in
      case _ of
        Error -> do
          s (Left Nothing) <#> map \o -> [ o ]
        (Complete v) -> do
          s (Left (Just v)) <#> map \o -> [ o ]
        (Step step more) -> do
          lift2 (\o r -> [ o ] <> r) <$> s (Right step) <*> go more

renderState :: Int -> SState -> Nutss
renderState i (State items) = (\j v -> renderItem i j v) `mapWithIndex` items

renderStateTable :: Array SState -> Nut
renderStateTable states = do
  let
    mkTH n 0 0 = D.th (D.Rowspan !:= show n)
    mkTH _ _ 0 = const (fixed [])
    mkTH _ _ _ = D.td_
    renderStateHere items =
      let n = Array.length items
      in items # mapWithIndex \j -> D.tr_ <<< mapWithIndex (\i -> mkTH n j i <<< pure)
  D.table (D.Class !:= "state-table")
    $ map (D.tbody_ <<< renderStateHere)
    $
      (\i v -> renderState i v) `mapWithIndex` states

renderItem :: Int -> Int -> SStateItem -> Nuts
renderItem i j { pName, rName, rule: rule@(Zipper _ after), lookahead } =
  [ if j == 0 then renderSt mempty (i + 1) else text_ ""
  , renderNT mempty pName
  , renderMeta mempty ": "
  , renderZipper rule
  , renderLookahead (if Array.null after then " reducible" else "") lookahead
  , fixed [ renderMeta mempty " #", renderRule mempty rName ]
  ]

renderZipper :: SZipper -> Nut
renderZipper (Zipper before after) =
  D.span (D.Class !:= ("zipper" <> if Array.null after then " reducible" else ""))
    [ D.span (D.Class !:= "parsed") $ before <#> \x -> renderPart mempty x
    , if Array.null after then fixed empty else
      D.span empty $ after <#> \x -> renderPart mempty x
    ]

renderLookahead :: String -> Array CodePoint -> Nut
renderLookahead moreClass items = D.span (D.Class !:= append "lookahead" moreClass) $
  [ renderMeta mempty "{ " ]
    <> Array.intercalate [ renderMeta mempty ", " ] (items <#> \x -> [ renderTok mempty x ])
    <> [ renderMeta mempty " }" ]

--------------------------------------------------------------------------------
counter :: forall s m a. MonadST s m => AnEvent m a → AnEvent m (a /\ Int)
counter event = mapAccum f event 0
  where
  f a b = (b + 1) /\ (a /\ b)

bangFold :: forall m a b t. Applicative m => MonadST t m => (a -> b -> b) -> AnEvent m a -> b -> AnEvent m b
bangFold folder event start = bang start <|> fold folder event start

memoBangFold :: forall m a b t r. Applicative m => MonadST t m => (a -> b -> b) -> AnEvent m a -> b -> (AnEvent m b -> r) -> AnEvent m r
memoBangFold folder event start doWithIt = memoize (fold folder event start)
  \folded -> doWithIt (bang start <|> folded)

toggle :: forall s m a b. HeytingAlgebra b => MonadST s m => b -> AnEvent m a → AnEvent m b
toggle start event = bangFold (\_ x -> not x) event start

withLast' :: forall event a. IsEvent event => event a -> event { last :: a, now :: a }
withLast' = filterMap (\{ last, now } -> last <#> { last: _, now }) <<< withLast

dedup :: forall s m a. Eq a => Applicative m => MonadST s m => AnEvent m a -> AnEvent m a
dedup = dedupOn eq

dedupOn :: forall s m a. (a -> a -> Boolean) -> Applicative m => MonadST s m => AnEvent m a -> AnEvent m a
dedupOn feq e = compact $
  mapAccum (\a b -> let ja = Just a in ja /\ (if (feq <$> b <*> ja) == Just true then Nothing else Just a)) e Nothing

interpolate :: Int -> Int -> Array Int
interpolate i j | i > j = j .. (i - 1)
interpolate i j | i < j = i .. (j - 1)
interpolate _ _ = []

stepByStep :: forall s m r. MonadST s m => Boolean -> AnEvent m Int -> ((Int -> AnEvent m Boolean) -> r) -> AnEvent m r
stepByStep start index cb =
  let
    state = withLast' index
    swept = keepLatest $ map (oneOfMap bang) $
      state <#> \{ last, now } -> interpolate last now
  in
    sweep swept \sweeper ->
      let
        sweeper' = toggle start <<< sweeper
      in
        cb sweeper'

debug :: forall m a. Show a => String -> AnEvent m a -> AnEvent m a
debug tag = map \a -> unsafePerformEffect (a <$ (Log.info (tag <> show a)))

unsafeDebug :: forall m a. String -> AnEvent m a -> AnEvent m a
unsafeDebug tag = map \a -> unsafePerformEffect (a <$ (Log.info tag <* Log.info (unsafeCoerce a)))

type ListicleEvent a = Variant (add :: a, remove :: Int)

-- | Render a list of items, with begin, end, separator elements and finalize button
-- | and remove buttons on each item. (All of those are optional, except for the items.)
-- |
-- | [ begin, ...[ item, remove, separator ]..., end, finalize ]
-- |
-- | Start from an initial value, listen for external add events, internal remove events,
-- | raise messages on change, and return the current value on finalize.
listicle
  :: forall s m lock payload a
   . Korok s m
  => Show a
  => { initial :: Array a -- initial value
     , addEvent :: AnEvent m a -- external add events

     , remove :: Maybe (Effect Unit -> Domable m lock payload) -- remove button
     , finalize :: Maybe (AnEvent m (Array a) -> Domable m lock payload) -- finalize button

     , renderItem :: a -> Domable m lock payload
     , begin :: Maybe (Domable m lock payload)
     , end :: Maybe (Domable m lock payload)
     , separator :: Maybe (Domable m lock payload)
     }
  -> ComponentSpec m lock payload (Array a)
listicle desc = keepLatest $ bus \pushRemove removesEvent ->
  let
    addEvent = counter desc.addEvent <#> \(v /\ i) -> (i + Array.length desc.initial) /\ v
    initialEvent = oneOfMap bang initialValue

    initialValue :: Array (Int /\ a)
    initialValue = mapWithIndex (/\) desc.initial

    performChange :: ListicleEvent (Int /\ a) -> Array (Int /\ a) -> Array (Int /\ a)
    performChange = V.match
      { add: \(j /\ v) vs -> Array.snoc vs (j /\ v)
      , remove: \i -> Array.filter \(i' /\ _) -> i' /= i
      }
    changesEvent =
      Variant.inj (Proxy :: Proxy "add") <$> addEvent
        <|> Variant.inj (Proxy :: Proxy "remove") <$> removesEvent
  in
    memoBangFold performChange changesEvent initialValue \currentValue ->
      let
        intro = case desc.begin of
          Nothing -> []
          Just x -> [ x ]
        extro = case desc.end of
          Nothing -> []
          Just x -> [ x ]
        fin = case desc.finalize of
          Nothing -> []
          Just thingy ->
            [ thingy (currentValue <#> map snd) ]
        sep = case desc.separator of
          Nothing -> []
          Just v -> [ v ]

        withRemover :: Domable m lock payload -> Int -> Array (Domable m lock payload)
        withRemover item idx = case desc.remove of
          Nothing -> [ item ]
          Just remover ->
            [ item, remover (pushRemove idx) ]

        renderOne :: Int /\ a -> Array (Domable m lock payload)
        renderOne (idx /\ item) = withRemover (desc.renderItem item) idx

        dropComma :: Int -> AnEvent m Boolean
        dropComma idx = filter identity
          $
            -- `currentValue` may or may not have updated before this `sampleOn` fires,
            -- depending on the order of subscriptions to `removesEvent`, so we just
            -- detect both here.
            -- (in particular, for elements rendered in the initial view, it seems that
            -- their subscription beats that of `currentValue` somehow)
            sampleOn currentValue
          $ removesEvent <#> \rem vs ->
              -- let _ = unsafePerformEffect (logShow { idx, rem, vs }) in
              rem == idx
                || ((fst <$> (vs !! 0)) == Just rem && (fst <$> (vs !! 1)) == Just idx)
                ||
                  ((fst <$> (vs !! 0)) == Just idx)

        element = fixed $
          let
            renderItems = sampleOn (Array.length <$> currentValue) $
              (initialEvent <|> addEvent) <#> \(idx /\ item) len ->
                ( bang $ insert $ fixed $ append
                    (if len > 0 && idx /= 0 then [ switcher (fixed <<< if _ then [] else sep) (bang false <|> dropComma idx) ] else [])
                    (renderOne (idx /\ item))
                ) <|> filter (eq idx) removesEvent $> remove
          in
            intro <> [ D.span_ [ dyn renderItems ] ] <> extro <> fin
      in
        { element, value: map snd <$> currentValue }

-- | Abstract component
type ComponentSpec m lock payload d =
  AnEvent m (Component m lock payload d)

-- | Instantiated component
type Component m lock payload d =
  { element :: Domable m lock payload
  , value :: AnEvent m d
  }

-- | Instantiate a component spec to get an actual component, with its element
-- | and value-event.
-- |
-- | Component specs are eventful so they can maintain state, so they need to
-- | be memoized in order that the element and value refer to the same instance,
-- | otherwise the value is attached to a phantom instance that has no DOM
-- | presence, due to the way busses and subscriptions work.
withInstance
  :: forall s d m lock payload
   . Korok s m
  => ComponentSpec m lock payload d
  -> (Component m lock payload d -> Domable m lock payload)
  -> Domable m lock payload
withInstance componentSpec renderer =
  envy $ memoize componentSpec \component ->
    renderer
      { element: envy (component <#> _.element)
      , value: keepLatest (component <#> _.value)
      }

renderAs :: String -> String -> Nut
renderAs c t = D.span (D.Class !:= c) [ text_ t ]

renderTok :: Maybe (Effect Unit) -> CodePoint -> Nut
renderTok c t = D.span (D.OnClick ?:= c <|> D.Class !:= "terminal") [ text_ (String.singleton t) ]

renderNT :: Maybe (Effect Unit) -> NonEmptyString -> Nut
renderNT c nt = D.span (D.OnClick ?:= c <|> D.Class !:= "non-terminal") [ text_ (NES.toString nt) ]

renderRule :: Maybe (Effect Unit) -> String -> Nut
renderRule c r = D.span (D.OnClick ?:= c <|> D.Class !:= "rule") [ text_ r ]

renderMeta :: Maybe (Effect Unit) -> String -> Nut
renderMeta c x = D.span (D.OnClick ?:= c <|> D.Class !:= "meta") [ text_ x ]

renderSt :: Maybe (Effect Unit) -> Int -> Nut
renderSt c x = D.span (D.OnClick ?:= c <|> D.Class !:= "state") [ text_ (show x) ]

renderPart :: Maybe (Effect Unit) -> Part NonEmptyString CodePoint -> Nut
renderPart c (NonTerminal nt) = renderNT c nt
renderPart c (Terminal t) = renderTok c t

renderCmd :: Maybe (Effect Unit) -> String -> Nut
renderCmd c x = D.span (D.OnClick ?:= c <|> D.Class !:= "cmd") [ text_ x ]

stateComponent
  :: forall s m lock payload
   . Korok s m
  => Domable m lock payload
stateComponent = bussed \addNew addEvent ->
  let
    component0 = listicle
      { begin: Just $ renderMeta mempty "{ "
      , end: Just $ renderMeta mempty " }"
      , separator: Just $ renderMeta mempty ", "
      , renderItem: \x -> renderTok mempty x
      , remove: Nothing
      , finalize: Nothing
      , addEvent: addEvent
      , initial: codePointFromChar <$> [ 'x', ',', ')' ]
      }
  in
    withInstance component0 \{ element, value } ->
      let
        length = map Array.length value
      in
        D.div_
          -- Without this div, it comes after the button upon update
          [ D.div_ [ element ]
          -- , D.button ((length <#> \v -> (D.OnClick := addNew v)) <|> buttonClass) [ text_ "Add" ]
          ]

type GrammarInputs =
  ( pName :: String
  , rule :: String
  , rName :: String
  , top :: String
  , entry :: String
  , eof :: String
  , topName :: String
  )

type GrammarAction =
  ( errorMessage :: Maybe String
  , addRule :: Int /\ { pName :: NonEmptyString, rule :: String, rName :: String }
  , removeRule :: Int
  )

only :: forall a. Array a -> Maybe a
only [ a ] = Just a
only _ = Nothing

parseGrammar
  :: { top :: NonEmptyString
     , entry :: Maybe NonEmptyString
     , eof :: CodePoint
     , topName :: String
     }
  -> Array { pName :: NonEmptyString, rule :: String, rName :: String }
  -> Either String SAugmented
parseGrammar top rules = do
  firstRule <- note "Need at least 1 rule in the grammar" $ Array.head rules
  let
    entry = fromMaybe firstRule.pName top.entry
    nonTerminals = longestFirst $ rules <#> _.pName
    parse = parseDefinition nonTerminals
    rules' = rules <#> \r -> r { rule = parse r.rule }
    topRule = { pName: top.top, rName: top.topName, rule: [ NonTerminal entry, Terminal top.eof ] }
    start =
      { pName: topRule.pName
      , rName: topRule.rName
      , rule: Zipper [] topRule.rule
      , lookahead: []
      }
  if Array.length (Array.nub ((rules <#> _.rName) <> [top.topName])) /= 1 + Array.length rules
    then Left "Rule names need to be unique"
    else pure unit
  if not isJust (Array.find (eq entry <<< _.pName) rules)
    then Left "Top-level does not refer to nonterminal"
    else pure unit
  if Set.member top.eof (gatherTokens (MkGrammar rules'))
    then Left "EOF symbol not unique"
    else pure unit
  pure $ { augmented: MkGrammar ([ topRule ] <> rules'), start, eof: top.eof }

grammarComponent
  :: forall s m lock payload
   . Korok s m
  => String
  -> SAugmented
  -> (SAugmented -> Effect Unit)
  -> Domable m lock payload
grammarComponent buttonText initialGrammar sendGrammar =
  vbussed (Proxy :: Proxy (V GrammarInputs)) \putInput inputs ->
    vbussed (Proxy :: Proxy (V GrammarAction)) \pushState changeState ->
      let
        changeRule =
          ( \change rules ->
              case change of
                Left new -> Array.snoc rules new
                Right remove -> Array.filter (fst >>> not eq remove) rules
          )
        ruleChanges = (Left <$> changeState.addRule <|> Right <$> changeState.removeRule)
        initialRules = unwrap initialGrammar.augmented # Array.drop 1 # mapWithIndex \i rule ->
          i /\ rule { rule = rule.rule # foldMap unSPart }
        initialTop =
          case Array.head (unwrap initialGrammar.augmented) of
            Just { pName, rName, rule: [ NonTerminal entry, Terminal eof ] } ->
              { top: NES.toString pName
              , entry: NES.toString entry
              , eof: String.singleton eof
              , topName: rName
              }
            _ ->
              { top: NES.toString defaultTopName
              , entry: ""
              , eof: String.singleton defaultEOF
              , topName: defaultTopRName
              }
        currentText =
          biSampleOn (bang "" <|> inputs.pName)
            $ biSampleOn (bang "" <|> inputs.rule)
            $
              (bang "" <|> inputs.rName) <#> \rName rule pName ->
                { rName, rule, pName }
        currentTop =
          biSampleOn (bang initialTop.top <|> inputs.top)
            $ biSampleOn (bang initialTop.entry <|> inputs.entry)
            $ biSampleOn (bang initialTop.eof <|> inputs.eof)
            $ biSampleOn (bang initialTop.topName <|> inputs.topName)
            $ bang { topName: _, eof: _, entry: _, top: _ }
        counted = add (Array.length initialRules) <$>
          (bang 0 <|> (add 1 <$> fst <$> changeState.addRule))
      in
        envy $ memoBangFold changeRule ruleChanges initialRules \currentRules -> do
          let
            currentNTs = dedup $ longestFirst <<< map (_.pName <<< snd) <$> currentRules
            currentTopParsed = biSampleOn currentRules $ currentTop <#> \r rules ->
              { top: fromMaybe defaultTopName $ NES.fromString r.top
              , entry: NES.fromString r.entry <|> (Array.head rules <#> snd >>> _.pName)
              , eof: fromMaybe defaultEOF $ only $ String.toCodePointArray r.eof
              , topName: r.topName
              }
            currentGrammar = biSampleOn (map snd <$> currentRules) (currentTopParsed <#> parseGrammar)
          D.div_
            [ D.div_
                [ changeState.errorMessage # switcher \et -> case et of
                    Nothing -> envy empty
                    Just e -> D.div_ [ D.span (D.Class !:= "text-red-300") [ text_ e ] ]
                ]
            , D.div_
                [ D.span (D.Class !:= "non-terminal") [ join input initialTop.top putInput.top ]
                , D.span (D.Class !:= "non-terminal") [ input "" initialTop.entry putInput.entry ]
                , D.span (D.Class !:= "terminal") [ join input initialTop.eof putInput.eof ]
                , D.span (D.Class !:= "rule") [ join input initialTop.topName putInput.topName ]
                ]
            , D.table_
                [ D.tr_
                    [ D.td_ [ switcher (\x -> renderNT mempty x) (currentTopParsed <#> _.top) ]
                    , D.td_ [ renderMeta mempty " : " ]
                    , D.td_
                        [ D.span_ [ switcher (maybe (text_ "—") (\x -> renderNT mempty x)) (currentTopParsed <#> _.entry) ]
                        , D.span_ [ switcher (\x -> renderTok mempty x) (currentTopParsed <#> _.eof) ]
                        ]
                    , D.td_
                        [ renderMeta mempty " #"
                        , D.span_ [ switcher (\x -> renderRule mempty x) (currentTopParsed <#> _.topName) ]
                        ]
                    ]
                , D.tbody_ $ pure $ dyn $ map
                    ( \(i /\ txt) -> keepLatest $ bus \p' e' ->
                        ( bang $ Insert $ D.tr_ $ map (D.td_ <<< pure) $
                            [ renderNT mempty txt.pName
                            , renderMeta mempty " : "
                            , D.span_
                                [ switcher
                                    ( \nts ->
                                        fixed $ map (\x -> renderPart mempty x) (parseDefinition nts txt.rule)
                                    )
                                    currentNTs
                                ]
                            , D.span_
                                [ renderMeta mempty " #"
                                , renderRule mempty txt.rName
                                ]
                            , D.span_
                                [ text_ " "
                                , D.button
                                    ( oneOf
                                        [ buttonClass
                                        , D.OnClick !:= (p' Remove *> pushState.removeRule i)
                                        ]
                                    )
                                    [ text_ "Delete" ]
                                ]
                            ]
                        ) <|> e'
                    )
                    (oneOfMap bang initialRules <|> changeState.addRule)
                ]
            , input "" "" putInput.pName
            , input "" "" putInput.rule
            , input "" "" putInput.rName
            , D.button
                ( oneOf
                    [ buttonClass
                    , D.OnClick <:=> do
                        sampleJITE currentText $ sampleJITE counted
                          $ map readersT
                          $ bang \i text -> do
                              pushState.errorMessage Nothing
                              case NES.fromString text.pName of
                                Nothing -> pushState.errorMessage (Just "Need name for the non-terminal.")
                                Just pName -> pushState.addRule (i /\ text { pName = pName })
                    ]
                )
                [ text_ "Add" ]
            , if buttonText == "" then fixed []
              else
                D.div_ $ pure $ D.button
                  ( oneOf
                      [ buttonClass
                      , currentGrammar <#> \g -> D.OnClick := do
                          pushState.errorMessage Nothing
                          case g of
                            Left err -> pushState.errorMessage (Just err)
                            Right g' -> sendGrammar g'
                      ]
                  )
                  [ text_ buttonText ]
            ]

type TopLevelUIAction = V
  ( changeText :: String
  , errorMessage :: Maybe String
  , grammar :: SAugmented
  )

sampleGrammar :: SAugmented
sampleGrammar = exSeed

main :: Nut
main =
  vbussed (Proxy :: _ TopLevelUIAction) \push event -> do
    envy $ memoBangFold const event.grammar sampleGrammar \currentGrammar -> do
      let
        currentValue = bang "" <|> event.changeText
        currentStates = filterMap (either (const Nothing) Just) $ currentGrammar <#> \{ augmented, start } ->
          numberStates (add 1) augmented (calculateStates augmented start)
        currentTable = toTable' <$> currentStates
        currentFromString = fromString <$> currentGrammar
        currentTokens = biSampleOn currentValue currentFromString
        currentParseSteps =
          biSampleOn currentTable
            $ biSampleOn currentTokens
            $
              bang \toks table -> parseSteps table <$> toks <@> 1
      D.div_
        [ D.div_
            [ event.errorMessage # switcher \et -> case et of
                Nothing -> envy empty
                Just e -> D.div_ [ D.span (D.Class !:= "text-red-300") [ text_ e ] ]
            ]
        , grammarComponent "Generate grammar" sampleGrammar push.grammar
        {-
        , D.table_ $ pure $ D.tbody_ $
            let
              { m, nt, t, k, r } = { m: renderAs "meta", nt: renderAs "non-terminal", t: renderAs "terminal", k: renderAs "keyword", r: renderAs "non-terminal rule" }
            in
              D.tr_ <<< map D.td_ <$>
                [ [ [ nt "E" ], [ m " : " ], [ t "(", nt "L", t ")" ], [ k "data ", nt "E" ], [ m " = " ], [ r "E1", text_ " ", nt "L" ] ]
                , [ [], [ m " | " ], [ t "x" ], [], [ m "|" ], [ r "E2" ] ]
                , [ [ nt "L" ], [ m " : " ], [ nt "E" ], [ k "data ", nt "L" ], [ m " = " ], [ r "L1", text_ " ", nt "E" ] ]
                , [ [], [ m " | " ], [ nt "L", t ",", nt "E" ], [], [ m " | " ], [ r "L2", text_ " ", nt "L", text_ " ", nt "E" ] ]
                ]
        -}
        , D.div_ $ pure $ switcher (\x -> renderStateTable (map _.items (unwrap x))) currentStates
        , D.div_ $ pure $ switcher (\x -> renderParseTable x) currentStates
        , D.div_ $ pure $
            switcher (\x -> fixed $ gatherTokens x # foldMap \tok -> [ renderTok mempty tok ]) (_.augmented <$> unsafeDebug "g " currentGrammar)
        , D.div_
            [ D.span (D.Class !:= "terminal") [ input "" "" push.changeText ] ]
        , D.div_ $ pure $ currentParseSteps `flip switcher` \todaysSteps ->
            let
              contentAsMonad = showMaybeParseSteps $ map (map (map (map (map snd)))) $ todaysSteps
              -- Run the first layer of the monad, to get the number of items being rendered up-front
              contentAsMonad2 /\ nEntities = runTrampoline (runStateT contentAsMonad 0)
            in
              vbussed (Proxy :: _ ParsedUIAction) \pPush pEvent ->
                let
                  -- Maintain the current index, clamped between 0 and nEntitities
                  -- (Note: it is automatically reset, since `switcher` resubscribes,
                  -- creating new state for it)
                  startState = pEvent.startState <|> bang Nothing
                  rate = pEvent.rate <|> bang 1.0
                  animationTick = compact $ mapAccum
                    ( \i@(tf /\ { beats: Beats beats }) { target: Beats target' } ->
                        let
                          target = if tf then 0.0 else target'
                        in
                          if target > beats then ({ target: Beats target } /\ Nothing)
                          else ({ target: Beats (target + 1.0) } /\ Just i)
                    )
                    pEvent.animationTick
                    { target: Beats 0.0 }
                  currentIndex = dedupOn (eq `on` fst) $ bang (nEntities /\ Initial) <|>
                    mapAccum
                      (\(f /\ a) x -> let fx = clamp 0 nEntities (f x) in fx /\ fx /\ a)
                      ( oneOf
                          [ ((_ - 1) /\ Toggle) <$ pEvent.toggleLeft
                          , ((_ + 1) /\ Toggle) <$ pEvent.toggleRight
                          -- if we're starting and at the end of a play, loop back to the beginning
                          , (\(tf /\ _) -> if tf then ((\n -> if n == nEntities then 0 else n + 1) /\ Play) else ((_ + 1) /\ Play)) <$> animationTick
                          , (floor >>> const >>> (_ /\ Slider)) <$> pEvent.slider
                          ]
                      )
                      nEntities
                in
                  -- Memoize it and run it through `stepByStep` to toggle each
                  -- item individually
                  envy $ keepLatest $ memoize currentIndex \stackIndex ->
                    stepByStep true (map fst stackIndex) \sweeper ->
                      let
                        content = contentAsMonad2 sweeper
                      in
                        D.div_
                          [ D.div_
                              [ D.button
                                  ( oneOf
                                      [ buttonClass
                                      , (biSampleOn rate ((/\) <$> startState)) <#> \(s /\ rt) -> D.OnClick := do
                                          case s of
                                            Just unsub -> do
                                              unsub
                                              pPush.startState Nothing
                                            Nothing -> do
                                              let toSeconds = unInstant >>> unwrap >>> (_ / 1000.0)
                                              t <- toSeconds <$> now
                                              sub <- subscribe
                                                ( selfDestruct (\((isStart /\ _) /\ ci) -> (fst ci == nEntities && not isStart)) (pPush.startState Nothing)
                                                    ( sampleOn (toEvent currentIndex)
                                                        ( (/\) <$> mapAccum (\i tf -> false /\ tf /\ i)
                                                            ( timeFromRate (step rt $ toEvent pEvent.rate)
                                                                ( _.time
                                                                    >>> toSeconds
                                                                    >>> (_ - t)
                                                                    >>> Seconds <$> withTime (animationFrame)
                                                                )
                                                            )
                                                            true
                                                        )
                                                    )
                                                )
                                                \(info /\ _) -> pPush.animationTick info
                                              pPush.startState (Just sub)
                                      ]
                                  )
                                  [ text
                                      ( startState <#> case _ of
                                          Just _ -> "Pause"
                                          Nothing -> "Play"
                                      )
                                  ]
                              ]
                          , D.div_
                              [ text_ "Speed"
                              , D.span_ $ join $ map
                                  ( \(n /\ l) ->
                                      [ D.input
                                          ( oneOfMap bang
                                              [ D.Xtype := "radio"
                                              , D.Checked := show (l == "1x")
                                              , D.Name := "speed"
                                              , D.Value := show n
                                              , D.OnClick := cb \_ -> pPush.rate n
                                              ]
                                          )
                                          []
                                      , D.label_ [ text_ l ]
                                      ]
                                  )
                                  [ 1.0 /\ "1x", (1.0 / e) /\ "ex", (1.0 / pi) /\ "pix" ]
                              ]
                          , D.div_
                              [ D.input
                                  ( oneOf
                                      [ D.Xtype !:= "range"
                                      , D.Min !:= "0"
                                      , D.Max !:= show nEntities
                                      , stackIndex
                                          # filterMap case _ of
                                              _ /\ Slider -> Nothing
                                              x /\ _ -> Just x
                                          <#> (\si -> D.Value := show si)
                                      , slider $ startState <#> case _ of
                                          Nothing -> pPush.slider
                                          Just unsub -> \n -> pPush.slider n
                                            *> unsub
                                            *> pPush.startState Nothing
                                      ]
                                  )
                                  []
                              ]
                          , let
                              clickF f = click $
                                startState <#>
                                  ( case _ of
                                      Nothing -> f unit
                                      Just unsub -> f unit
                                        *> unsub
                                        *> pPush.startState Nothing
                                  )
                            in
                              D.div_
                                [ D.button
                                    (oneOf [ arrowClass, clickF $ pPush.toggleLeft ])
                                    [ text_ "<" ]
                                , D.button (oneOf [ arrowClass, clickF $ pPush.toggleRight ])
                                    [ text_ ">" ]
                                ]
                          , D.div_ [ content ]
                          ]
        ]
