module Parser.Main where

import Prelude

import Bolson.Core (Child(..), dyn, envy, fixed)
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as STRef
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
import Data.Foldable (any, foldMap, for_, oneOf, oneOfMap, sequence_, sum, traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (floor)
import Data.Int as Int
import Data.List (List)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number (e, pi)
import Data.Semigroup.Foldable (minimum, minimumBy)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (CodePoint, codePointFromChar)
import Data.String as String
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (for, mapAccumL, sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
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
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (step)
import FRP.Event (class IsEvent, AnEvent, bang, create, filterMap, fold, keepLatest, makeEvent, mapAccum, memoize, sampleOn, subscribe, sweep, toEvent, withLast)
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
import Random.LCG as LCG
import Test.QuickCheck.Gen as QC
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
  (Array (GrammarRule nt r tok))

type GrammarRule nt r tok =
  { pName :: nt -- nonterminal / production rule name
  , rName :: r -- each rule has a unique name
  , rule :: Fragment nt tok -- sequence of nonterminals and terminals that make up the rule
  }

getRulesFor :: forall nt r tok. Eq nt => Array (Produced nt r tok) -> nt -> Array { rule :: Fragment nt tok, produced :: Array tok }
getRulesFor rules nt = rules # filterMap \rule ->
  if rule.production.pName /= nt then Nothing
  else
    Just { rule: rule.production.rule, produced: rule.produced }

countNTs :: forall nt tok. Fragment nt tok -> Int
countNTs = sum <<< map (\t -> if isNonTerminal t then 1 else 0)

-- NonEmptyArray (Int /\ rule)
-- every minimum that occurs

chooseBySize :: forall r nt tok. Int -> NonEmptyArray { rule :: Fragment nt tok | r } -> NonEmptyArray { rule :: Fragment nt tok | r }
chooseBySize i as =
  let
    sized = as <#> \rule -> countNTs rule.rule /\ rule
  in
    case NEA.fromArray (NEA.filter (\(size /\ _) -> size <= i) sized) of
      Nothing ->
        map snd $ NEA.head $
          NEA.groupAllBy (compare `on` fst) sized
      Just as' -> map snd as'

type Produced nt r tok =
  { production :: GrammarRule nt r tok
  , produced :: Array tok
  }

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

shrink :: forall m. MonadGen m => m ~> m
shrink = Gen.resize (_ `div` 2)

genNT
  :: forall m nt r tok
   . Eq nt
  => MonadGen m
  => Array (Produced nt r tok)
  -> (nt -> Maybe (m (Array tok)))
genNT grammar nt = genNT1 grammar nt <#> \mr ->
  mr >>= genMore grammar

genMore
  :: forall m nt r tok
   . Eq nt
  => MonadGen m
  => Array (Produced nt r tok)
  -> { rule :: Fragment nt tok, produced :: Array tok }
  -> m (Array tok)
genMore grammar { rule, produced } =
  -- `genMoreMaybe` should never fail, if the `Produced` data is any good
  -- but just in case! we still need to produce a value, so `produced` is a
  -- default value we have access to from the `Produced` data.
  fromMaybe (pure produced) (genMoreMaybe grammar rule)

genMoreMaybe
  :: forall m nt r tok
   . Eq nt
  => MonadGen m
  => Array (Produced nt r tok)
  -> Fragment nt tok
  -> Maybe (m (Array tok))
genMoreMaybe grammar rule =
  map (map join <<< sequence) $
    for rule case _ of
      Terminal tok -> Just (pure [ tok ] :: m (Array tok))
      NonTerminal nt ->
        shrink <$> genNT grammar nt

genNT1
  :: forall m nt r tok
   . Eq nt
  => MonadGen m
  => Array (Produced nt r tok)
  -> (nt -> Maybe (m { rule :: Fragment nt tok, produced :: Array tok }))
genNT1 grammar nt =
  getRulesFor grammar nt # NEA.fromArray #
    map (\rules -> Gen.sized \sz -> chooseBySize sz rules # Gen.elements)

derive instance newtypeGrammar :: Newtype (Grammar nt r tok) _

type Augmented nt r tok =
  { augmented :: Grammar nt r tok
  , start :: StateItem nt r tok
  , eof :: tok
  , entry :: nt
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

gatherTokens' :: forall nt r tok. Ord tok => Grammar nt r tok -> Array tok
gatherTokens' (MkGrammar rules) = Array.nub $ rules # Array.foldMap \{ rule } ->
  Array.mapMaybe unTerminal rule

gatherNonTerminals :: forall nt r tok. Ord nt => Grammar nt r tok -> Set nt
gatherNonTerminals (MkGrammar rules) = rules # Array.foldMap \{ rule } ->
  Array.mapMaybe unNonTerminal rule # Set.fromFoldable

gatherNonTerminals' :: forall nt r tok. Ord nt => Grammar nt r tok -> Array nt
gatherNonTerminals' (MkGrammar rules) = Array.nub $ rules # Array.foldMap \{ rule } ->
  Array.mapMaybe unNonTerminal rule

exFromString :: String -> Maybe (List CodePoint)
exFromString = String.toCodePointArray >>> flip append [ defaultEOF ] >>> Array.toUnfoldable >>> verifyTokens exSeed.augmented

fromString :: SAugmented -> String -> Maybe (List CodePoint)
fromString grammar =
  ( join \e ->
      if String.contains (String.Pattern (String.singleton grammar.eof)) e then identity
      else flip append (String.singleton grammar.eof)
  )
    >>> fromString' grammar

fromString' :: SAugmented -> String -> Maybe (List CodePoint)
fromString' grammar = String.toCodePointArray
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
type SFragment = Fragment NonEmptyString CodePoint

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

type SCST = CST (NonEmptyString /\ String) CodePoint

derive instance genericCST :: Generic (CST r tok) _
instance showCST :: (Show r, Show tok) => Show (CST r tok) where
  show x = genericShow x

data AST r = Layer r (Array (AST r))
type SAST = AST String

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

infixr 5 bangAttr as !:=

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

input :: String -> String -> String -> (String -> Effect Unit) -> Nut
input label placeholder initialValue onInput =
  D.label_
    [ D.span_ [ text_ label ]
    , D.input
        ( oneOf
            [ D.Placeholder <:=> if placeholder == "" then empty else bang placeholder
            , D.Value <:=> if initialValue == "" then empty else bang initialValue
            , D.OnInput !:= withValue onInput
            ]
        )
        []
    ]

input' :: forall s m lock payload. Korok s m => String -> String -> AnEvent m String -> (String -> Effect Unit) -> Domable m lock payload
input' label placeholder initialValue onInput =
  D.label_
    [ D.span_ [ text_ label ]
    , D.input
        ( oneOf
            [ D.Placeholder <:=> if placeholder == "" then empty else bang placeholder
            , D.Value <:=> initialValue
            , D.OnInput !:= withValue onInput
            ]
        )
        []
    ]

type Header nt tok = Array tok /\ Array nt

getHeader :: forall s nt r tok. Ord nt => Ord tok => States s nt r tok -> Header nt tok
getHeader (States states) = bimap Array.nub Array.nub $
  states # foldMap \{ items: State items } -> items # foldMap \item ->
    ([] /\ [ item.pName ]) <> foldZipper fromPart item.rule
  where
  foldZipper f (Zipper l r) = foldMap f l <> foldMap f r
  fromPart (NonTerminal nt) = [] /\ [ nt ]
  fromPart (Terminal tok) = [ tok ] /\ []

col :: forall a m e. Eq a => Applicative m => Attr e D.Class String => a -> a -> AnEvent m (Attribute e)
col j i =
  if i == j then D.Class !:= "first" else empty

renderParseTable
  :: forall s m lock payload r
   . Korok s m
  => { getCurrentState :: Int -> AnEvent m Boolean | r }
  -> SStates
  -> Domable m lock payload
renderParseTable info (States states) =
  bussed \push event ->
    let
      stateHighlighted = bang Nothing <|> event
      terminals /\ nonTerminals = getHeader (States states)
      renderTerminals x = renderTok mempty x
      renderNonTerminals x = renderNT mempty x
      renderStHere s =
        D.span
          ( oneOf
              [ D.Class !:= "state hoverable"
              , D.OnMouseenter !:= push (Just s)
              , D.OnMouseleave !:= push Nothing
              ]
          )
          [ text_ (show s) ]
      renderShiftReduce Nothing = fixed []
      renderShiftReduce (Just (Shift s)) = D.span_ [ renderCmd mempty "s", renderStHere s ]
      renderShiftReduce (Just (Reduces rs)) =
        D.span (if NEA.length rs > 1 then D.Class !:= "conflict" else empty) $
          rs # foldMap \r -> [ renderCmd mempty "r", renderRule mempty r ]
      renderShiftReduce (Just (ShiftReduces s rs)) =
        D.span (D.Class !:= "conflict") $
          [ renderCmd mempty "s", renderStHere s ] <> (rs # foldMap \r -> [ renderCmd mempty "r", renderRule mempty r ])
      renderGoto Nothing = []
      renderGoto (Just s) = [ renderCmd mempty "g", renderStHere s ]
      cols state =
        let
          forTerminal tok = map snd <$> Map.lookup tok (unwrap state.advance)
          forNonTerminal nt = Map.lookup nt state.receive
        in
          map (pure <<< renderShiftReduce <<< forTerminal) terminals <> map (renderGoto <<< forNonTerminal) nonTerminals

      header = D.tr_ $ mapWithIndex (\i -> D.th (col (Array.length terminals + 1) i) <<< pure) $
        [ text_ "" ] <> map renderTerminals terminals <> map renderNonTerminals nonTerminals
      clsFor s =
        biSampleOn
          ((if _ then " active " else "") <$> info.getCurrentState s)
          $ stateHighlighted <#> \s' -> append $
              if s' == Just s then " hover " else ""
      rows = states <#> \state -> D.tr (D.Class <:=> clsFor state.sName)
        $ Array.cons (D.th_ [ renderStHere state.sName ])
        $
          mapWithIndex (\i -> D.td (col (Array.length terminals) i)) (cols state)
    in
      D.table (D.Class !:= "parse-table")
        [ D.thead_ [ header ]
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

showStack :: SCStack -> Nut
showStack i = D.span (D.Class !:= "stack") (go i)
  where
  go (Zero state) = [ D.sub_ [ renderSt mempty state ] ]
  go (Snoc stack tok state) = go stack
    <> [ renderStackItem (map snd <$> prune tok) ]
    <> [ D.sub_ [ renderSt mempty state ] ]

renderStackItem :: Either CodePoint SAST -> Nut
renderStackItem (Left x) = renderTok mempty x
renderStackItem (Right x) = renderAST x

renderAST :: SAST -> Nut
renderAST (Layer r []) = D.span (D.Class !:= "layer") [ renderRule mempty r ]
renderAST (Layer r cs) =
  D.span (D.Class !:= "layer")
    [ renderMeta mempty "("
    , renderRule mempty r
    , fixed $ cs # foldMap \c -> [ text_ " ", renderAST c ]
    , renderMeta mempty ")"
    ]

renderASTTree :: SAST -> Nut
renderASTTree ast =
  D.ol (D.Class !:= "AST")
    [ D.li_ (renderASTChild ast) ]

renderASTChild :: SAST -> Nuts
renderASTChild (Layer r []) =
  [ D.span (D.Class !:= "leaf node")
      [ renderRule mempty r ]
  ]
renderASTChild (Layer r cs) =
  [ D.span (D.Class !:= "node")
      [ renderRule mempty r ]
  , D.ol (D.Class !:= "layer") $
      cs <#> \c -> D.li_ (renderASTChild c)
  ]

renderCSTTree :: SCST -> Nut
renderCSTTree ast =
  D.ol (D.Class !:= "AST CST")
    [ D.li_ (renderCSTChild ast) ]

renderCSTChild :: SCST -> Nuts
renderCSTChild (Leaf tok) =
  [ D.span (D.Class !:= "leaf node")
      [ renderTok mempty tok ]
  ]
renderCSTChild (Branch (_ /\ r) cs) =
  [ D.span (D.Class !:= "node")
      [ renderRule mempty r ]
  , D.ol (D.Class !:= "layer") $
      cs <#> \c -> D.li_ (renderCSTChild c)
  ]

showMaybeStack :: Maybe SCStack -> Nut
showMaybeStack Nothing = text_ "Parse error"
showMaybeStack (Just stack) = showStack stack

type SAStack = Stack Int (Either CodePoint SAST)
type SCStack = Stack Int SCST
type SAParseSteps = ParseSteps CodePoint SAStack
type SCParseSteps = ParseSteps CodePoint SCStack

showMaybeParseSteps :: forall s m lock payload. Korok s m => Maybe SCParseSteps -> SuperStack m (Domable m lock payload)
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
  => Either (Maybe SCStack)
       { inputs :: List CodePoint
       , stack :: SCStack
       | r
       }
  -> SuperStack m (Domable m lock payload)
showParseStep (Left Nothing) = do
  getVisibilityAndIncrement <#> map \(n /\ vi) ->
    D.div vi [ (text_ $ ("Step " <> show n <> ": ") <> "Parse error") ]
showParseStep (Left (Just v)) = do
  getVisibilityAndIncrement <#> map \(n /\ vi) ->
    case getResultC v of
      Just r | Right p <- map snd <$> prune r ->
        D.div vi [ text_ $ ("Step the last: "), renderAST p, renderASTTree p, renderCSTTree r ]
      _ ->
        D.div vi [ text_ $ ("Step the last: ") <> "Something went wrong" ]
showParseStep (Right { stack, inputs }) = do
  getVisibilityAndIncrement' "flex justify-between" <#> map \(n /\ vi) ->
    D.div vi [ D.div_ [ text_ ("Step " <> show n <> ": "), showStack stack ], D.div_ (foldMap (\x -> [ renderTok mempty x ]) inputs) ]

type SuperStack m a = StateT Int Trampoline ((Int -> AnEvent m Boolean) -> a)

showParseSteps
  :: forall s m lock payload
   . Korok s m
  => SCParseSteps
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
renderState sName (State items) = (\j v -> renderItem sName j v) `mapWithIndex` items

renderStateTable :: forall s m lock payload r. Korok s m => { getCurrentState :: Int -> AnEvent m Boolean | r } -> SStates -> Domable m lock payload
renderStateTable info (States states) = do
  let
    mkTH n 0 0 = D.th (D.Rowspan !:= show n)
    mkTH _ _ 0 = const (fixed [])
    mkTH _ _ _ = D.td_
    stateClass sName = (if _ then "active" else "") <$> info.getCurrentState sName
    renderStateHere items =
      let
        n = Array.length items
      in
        items # mapWithIndex \j -> D.tr_ <<< mapWithIndex (\i -> mkTH n j i <<< pure)
  D.table (D.Class !:= "state-table")
    $ states <#>
        \{ sName, items } ->
          D.tbody (D.Class <:=> stateClass sName)
            $ renderStateHere
            $ renderState sName items

renderItem :: Int -> Int -> SStateItem -> Nuts
renderItem sName j { pName, rName, rule: rule@(Zipper _ after), lookahead } =
  [ if j == 0 then renderSt mempty sName else text_ ""
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
    , if Array.null after then fixed empty
      else
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

memoBang :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> a -> (AnEvent m a -> r) -> AnEvent m r
memoBang event start doWithIt = memoize event
  \memoized -> doWithIt (bang start <|> memoized)

memoBeh :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> a -> (AnEvent m a -> r) -> AnEvent m r
memoBeh e a f = makeEvent \k -> do
  { push, event } <- create
  current <- liftST (STRef.new a)
  let
    event' = makeEvent \k' -> do
      liftST (STRef.read current) >>= k'
      subscribe event k'
  k (f event')
  subscribe e push

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

spotlight :: forall a s m r. Ord a => MonadST s m => Boolean -> AnEvent m a -> ((a -> AnEvent m Boolean) -> r) -> AnEvent m r
spotlight start shineAt cb =
  let
    state = withLast shineAt
    swept = keepLatest $ state <#> \{ now, last } ->
      if last == Just now then empty
      else
        oneOfMap bang last <|> bang now
  in
    sweep swept \sweeper ->
      let
        sweeper' a = toggle start <<< sweeper $ a
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
renderTok c t = D.span (D.OnClick ?:= c <|> D.Class !:= "terminal" <> if isJust c then " clickable" else "") [ text_ (String.singleton t) ]

renderTok' :: forall s m lock payload. Korok s m => AnEvent m String -> AnEvent m (Maybe (Effect Unit)) -> CodePoint -> Domable m lock payload
renderTok' cls c t = D.span (D.OnClick <:=> filterMap identity c <|> D.Class <:=> (bang "terminal" <|> (append "terminal " <$> cls))) [ text_ (String.singleton t) ]

renderNT :: Maybe (Effect Unit) -> NonEmptyString -> Nut
renderNT c nt = D.span (D.OnClick ?:= c <|> D.Class !:= "non-terminal" <> if isJust c then " clickable" else "") [ text_ (NES.toString nt) ]

renderRule :: Maybe (Effect Unit) -> String -> Nut
renderRule c r = D.span (D.OnClick ?:= c <|> D.Class !:= "rule" <> if isJust c then " clickable" else "") [ text_ r ]

renderMeta :: Maybe (Effect Unit) -> String -> Nut
renderMeta c x = D.span (D.OnClick ?:= c <|> D.Class !:= "meta" <> if isJust c then " clickable" else "") [ text_ x ]

renderSt :: Maybe (Effect Unit) -> Int -> Nut
renderSt c x = D.span (D.OnClick ?:= c <|> D.Class !:= "state" <> if isJust c then " clickable" else "") [ text_ (show x) ]

renderSt' :: forall s m lock payload. Korok s m => AnEvent m String -> Maybe (Effect Unit) -> Int -> Domable m lock payload
renderSt' cls c x = D.span (D.OnClick ?:= c <|> D.Class <:=> (bang "state" <|> (append "state " <$> cls))) [ text_ (show x) ]

renderPart :: Maybe (Effect Unit) -> Part NonEmptyString CodePoint -> Nut
renderPart c (NonTerminal nt) = renderNT c nt
renderPart c (Terminal t) = renderTok c t

renderCmd :: Maybe (Effect Unit) -> String -> Nut
renderCmd c x = D.span (D.OnClick ?:= c <|> D.Class !:= "cmd" <> if isJust c then " clickable" else "") [ text_ x ]

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
    nonTerminals = longestFirst $ [ top.top ] <> (rules <#> _.pName)
    parse = parseDefinition nonTerminals
    rules' = rules <#> \r -> r { rule = parse r.rule }
    topRule = { pName: top.top, rName: top.topName, rule: [ NonTerminal entry, Terminal top.eof ] }
    start =
      { pName: topRule.pName
      , rName: topRule.rName
      , rule: Zipper [] topRule.rule
      , lookahead: []
      }
  if Array.length (Array.nub ((rules <#> _.rName) <> [ top.topName ])) /= 1 + Array.length rules then Left "Rule names need to be unique"
  else pure unit
  if not isJust (Array.find (eq entry <<< _.pName) rules) then Left "Top-level does not refer to nonterminal"
  else pure unit
  if Set.member top.eof (gatherTokens (MkGrammar rules')) then Left "Grammar references EOF symbol"
  else pure unit
  if Set.member top.top (gatherNonTerminals (MkGrammar rules')) then Left "Grammar references top rule"
  else pure unit
  pure $ { augmented: MkGrammar ([ topRule ] <> rules'), start, eof: top.eof, entry }

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
            currentNTs = dedup $ map longestFirst $
              biSampleOn
                (map (_.pName <<< snd) <$> currentRules)
                (append <<< pure <<< fromMaybe defaultTopName <<< NES.fromString <$> (bang initialTop.top <|> inputs.top))
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
                    Just e -> D.div (D.Class !:= "Error") [ text_ e ]
                ]
            , D.div_
                [ D.span (D.Class !:= "non-terminal") [ join (input "Top name") initialTop.top putInput.top ]
                , D.span (D.Class !:= "non-terminal") [ input "Entrypoint" "" initialTop.entry putInput.entry ]
                , D.span (D.Class !:= "terminal") [ join (input "Final token") initialTop.eof putInput.eof ]
                , D.span (D.Class !:= "rule") [ join (input "Top rule name") initialTop.topName putInput.topName ]
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
                                        [ D.Class !:= "delete"
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
            , D.div_
                [ D.span (D.Class !:= "non-terminal")
                    [ input "Nonterminal name" "" "" putInput.pName ]
                , renderMeta mempty " : "
                , D.span (D.Class !:= "terminal")
                    [ input "Value" "" "" putInput.rule ]
                , renderMeta mempty " #"
                , D.span (D.Class !:= "rule")
                    [ input "Rule name" "" "" putInput.rName ]
                , D.button
                    ( oneOf
                        [ D.Class !:= "big"
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
                    [ text_ "Add rule" ]
                ]
            , if buttonText == "" then fixed []
              else
                D.div_ $ pure $ D.button
                  ( oneOf
                      [ D.Class !:= "big"
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
  ( changeText :: Boolean /\ String
  , errorMessage :: Maybe String
  , grammar :: SAugmented
  )

sampleGrammar :: SAugmented
sampleGrammar = fromSeed' defaultTopName defaultTopRName defaultEOF
  ( parseIntoGrammar
      [ { pName: "Additive", rName: "A:Add", rule: "Additive+Multiplicative" }
      , { pName: "Additive", rName: "A<-M", rule: "Multiplicative" }
      , { pName: "Multiplicative", rName: "M:Mul", rule: "Multiplicative*Unit" }
      , { pName: "Multiplicative", rName: "M<-U", rule: "Unit" }
      , { pName: "Unit", rName: "U:Val", rule: "Number" }
      , { pName: "Unit", rName: "U<-A", rule: "(Additive)" }
      , { pName: "Number", rName: "N<-D", rule: "Digit" }
      , { pName: "Digit", rName: "0", rule: "0" }
      , { pName: "Digit", rName: "1", rule: "1" }
      , { pName: "Digit", rName: "2", rule: "2" }
      {-
      , { pName: "Digit", rName: "3", rule: "3" }
      , { pName: "Digit", rName: "4", rule: "4" }
      , { pName: "Digit", rName: "5", rule: "5" }
      , { pName: "Digit", rName: "6", rule: "6" }
      , { pName: "Digit", rName: "7", rule: "7" }
      , { pName: "Digit", rName: "8", rule: "8" }
      , { pName: "Digit", rName: "9", rule: "9" }
      -}
      ]
  )
  (unsafePartial (fromJust (NES.fromString "Additive")))

lastState :: SCParseSteps -> Int
lastState Error = 0
lastState (Step x Error) = topOf x.stack
lastState (Complete x) = topOf x
lastState (Step _ s) = lastState s

type ExplorerAction =
  ( focus :: Maybe (Int /\ NonEmptyString)
  , select :: SFragment
  )

explorerComponent
  :: forall s m lock payload
   . Korok s m
  => SAugmented
  -> (Array CodePoint -> Effect Unit)
  -> Domable m lock payload
explorerComponent { augmented: MkGrammar rules, start: { pName: entry } } sendUp =
  vbussed (Proxy :: Proxy (V ExplorerAction)) \push event -> do
    envy $ memoBeh event.select [ NonTerminal entry ] \currentParts -> do
      let
        firstNonTerminal = Array.head <<< foldMapWithIndex
          \i v -> maybe [] (\r -> [ i /\ r ]) (unNonTerminal v)
      envy $ memoBeh (event.focus <|> map firstNonTerminal currentParts) (Just (0 /\ entry)) \currentFocused -> do
        let
          producedRules = produceable (MkGrammar rules)
          activity here = here <#> if _ then "active" else "inactive"
          renderPartHere i (NonTerminal nt) =
            D.span
              (D.Class <:=> (currentFocused <#> any (fst >>> eq i) >>> if _ then "selected" else ""))
              [ renderNT (Just (push.focus (Just (i /\ nt)))) nt ]
          renderPartHere _ (Terminal tok) = renderTok mempty tok
          send = currentParts <#> \parts ->
            case traverse unTerminal parts of
              Nothing -> Nothing
              Just toks -> Just (sendUp toks)
        D.div_
          [ D.span_ [ switcher (fixed <<< mapWithIndex renderPartHere) currentParts ]
          , D.button
              ( D.Class <:=> maybe "disabled" mempty <$> send
                  <|> D.OnClick <:=> sequence_ <$> send
              )
              [ text_ "Send" ]
          , D.button
              ( D.Class !:= "delete"
                  <|> D.OnClick !:= push.select [ NonTerminal entry ]
              )
              [ text_ "Reset" ]
          , D.table (D.Class !:= "explorer-table")
              [ D.tbody_ $ rules <#> \rule -> do
                  let
                    focusHere = currentFocused # map (any (snd >>> eq rule.pName))
                    replacement = sampleOn currentParts $ currentFocused <#> \mfoc parts -> do
                      focused /\ nt <- mfoc
                      guard $ nt == rule.pName
                      guard $ focused <= Array.length parts
                      pure $ Array.take focused parts <> rule.rule <> Array.drop (focused + 1) parts
                  D.tr (D.Class <:=> activity focusHere) $ map (D.td_ <<< pure) $
                    [ renderNT mempty rule.pName
                    , renderMeta mempty " : "
                    , fixed $ map (\x -> renderPart mempty x) rule.rule
                    , D.span_
                        [ renderMeta mempty " #"
                        , renderRule mempty rule.rName
                        ]
                    , D.span_
                        [ text_ " "
                        , D.button
                            ( oneOf
                                [ D.Class <:=> (append "select")
                                    <$> (if _ then mempty else " disabled")
                                    <$> focusHere
                                , D.OnClick <:=> traverse_ push.select <$> replacement
                                ]
                            )
                            [ text_ "Select" ]
                        ]
                    , case Array.find (_.production >>> eq rule) producedRules of
                        Nothing -> text_ "Unproduceable"
                        Just { produced } ->
                          D.span_
                            [ D.em_ [ text_ "e.g. " ]
                            , fixed $ map (\x -> renderTok mempty x) produced
                            ]
                    ]
              ]
          ]

type RandomAction =
  ( size :: Int
  , amt :: Int
  , randomMany :: Array (Array CodePoint)
  )

randomComponent
  :: forall s m lock payload
   . Korok s m
  => SAugmented
  -> (Array CodePoint -> Effect Unit)
  -> Domable m lock payload
randomComponent { augmented: MkGrammar rules, start: { pName: entry } } sendUp =
  vbussed (Proxy :: Proxy (V RandomAction)) \push event -> do
    let
      producedRules = produceable (MkGrammar rules)
      initialSize = 50
      initialAmt = 15
      randomProductions nt sz amt =
        genNT producedRules nt # traverse (Gen.resize (const sz) >>> QC.randomSample' amt)
      initial = genNT producedRules entry # maybe [] (QC.sample (LCG.mkSeed 1234) initialAmt)
    D.div_
      [ D.div_
          [ D.label (D.Class !:= "range")
              [ D.span_ [ text_ "Simple" ]
              , D.input
                  ( oneOf
                      [ D.Xtype !:= "range"
                      , D.Min !:= "0"
                      , D.Max !:= "100"
                      , slider $ bang $ push.size <<< Int.round
                      ]
                  )
                  []
              , D.span_ [ text_ "Complex" ]
              ]
          , D.label (D.Class !:= "range")
              [ D.span_ [ text_ "Few" ]
              , D.input
                  ( oneOf
                      [ D.Xtype !:= "range"
                      , D.Min !:= "1"
                      , D.Max !:= "30"
                      , D.Value !:= "15"
                      , slider $ bang $ push.amt <<< Int.round
                      ]
                  )
                  []
              , D.span_ [ text_ "Many" ]
              ]
          , D.button
              ( D.Class !:= ""
                  <|> D.OnClick <:=>
                    ( biSampleOn (bang initialAmt <|> event.amt) $
                        ( (bang initialSize <|> event.size) <#> \sz amt ->
                            (traverse_ (push.randomMany) =<< randomProductions entry sz amt)
                        )
                    )
              )
              [ text_ "Random" ]
          ]
      , D.ul_ $ pure $ switcher (fixed <<< map (\xs -> D.li (D.Class !:= "clickable" <|> D.OnClick !:= sendUp xs) <<< map (\x -> renderTok mempty x) $ xs))
          (bang initial <|> event.randomMany)
      ]

peas :: Array String -> Nut
peas x = fixed (map (D.p_ <<< pure <<< text_) x)

main :: Nut
main =
  vbussed (Proxy :: _ TopLevelUIAction) \push event -> do
    envy $ memoBangFold const event.grammar sampleGrammar \currentGrammar -> do
      let
        currentValue = bang "0+1*2" <|> map snd event.changeText
        currentValue' = bang "0+1*2" <|> filterMap (\(keep /\ v) -> if keep then Just v else Nothing) event.changeText
        currentStates = map (either (const (States [])) identity) $ currentGrammar <#> \{ augmented, start } ->
          numberStates (add 1) augmented (calculateStates augmented start)
        currentTable = toTable <$> currentStates
        currentFromString = fromString <$> currentGrammar
        currentFromString' = fromString' <$> currentGrammar
        currentTokens = biSampleOn currentValue currentFromString
        currentTokens' = biSampleOn currentValue currentFromString'
        currentParseSteps =
          sampleOn currentTable
            $ currentTokens <#> \toks table -> parseSteps table <$> toks <@> 1
        currentParseSteps' =
          sampleOn currentTable
            $ currentTokens' <#> \toks table -> parseSteps table <$> toks <@> 1
        currentState = maybe 0 lastState <$> currentParseSteps'
        currentGrammarTokens = gatherTokens' <<< _.augmented <$> currentGrammar
        receiveToks toks = push.changeText (true /\ String.fromCodePointArray toks)
      envy $ memoBang currentState 0 \currentState -> do
        envy $ memoize currentStates \currentStates -> do
          -- Note quite sure what's happening here, but we basically need a
          -- fresh memoized instance of `spotlight …`` each time it refreshes,
          -- which we happen to know is on updates of `currentStates`
          let currentGetCurrentState = keepLatest $ currentStates $> spotlight false currentState identity
          envy $ memoize (sampleOn currentGetCurrentState (map (/\) currentStates)) \currentStatesAndGetState -> do
            let
              currentStateItem =
                sampleOn currentStates
                  $ currentState <#> \st (States states) -> Array.find (_.sName >>> eq st) states
              currentValidTokens =
                currentStateItem <#> case _ of
                  Nothing -> Map.empty
                  Just { advance: SemigroupMap adv } -> adv

              renderTokenHere mtok = do
                let
                  onClick = sampleJITE currentValue $ bang $ ReaderT \v ->
                    push.changeText $ true /\ case mtok of
                      Just tok -> v <> String.singleton tok
                      Nothing -> String.take (String.length v - 1) v
                  valid = case mtok of
                    Just tok -> currentValidTokens <#> Map.member tok
                    Nothing -> currentValue <#> not String.null
                  toktext = case mtok of
                    Nothing -> codePointFromChar '⌫'
                    Just tok -> tok
                renderTok' (map (append "clickable") $ valid <#> if _ then "" else " unusable") (Just <$> onClick) toktext
            D.div_
              [ D.div_
                  [ event.errorMessage # switcher \et -> case et of
                      Nothing -> envy empty
                      Just e -> D.div (D.Class !:= "Error") [ text_ e ]
                  ]
              , D.h2_ [ text_ "Input a grammar" ]
              , peas
                  [ "Craft a grammar out of a set of rules. Each rule consists of a nonterminal name, then a colon followed by a sequence of nonterminals and terminals; each rule must also have a unique name."
                  , "The top rule is controlled by the upper input boxes (LR(1) grammars often require a top rule that contains a unique terminal to terminate each input), while the lower input boxes are for adding a new rule. The nonterminals are automatically parsed out of the entered rule, which is otherwise assumed to consist of terminals."
                  , "Click “Use grammar” to see the current set of rules in action! It may take a few seconds, depending on the size of the grammar and how many states it produces."
                  ]
              , grammarComponent "Use grammar" sampleGrammar push.grammar
              , D.h2_ [ text_ "Generate random matching inputs" ]
              , peas
                  [ "This will randomly generate some inputs that conform to the grammar. Click on one to send it to be tested down below!"
                  ]
              , D.div_ [ switcher (flip randomComponent receiveToks) currentGrammar ]
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
              , D.h2_ [ text_ "List of parsing states" ]
              , peas
                  [ "To construct the LR(1) parse table, the possible states are enumerated. Each state represents partial progress of some rules in the grammar. The center dot “•” represents the dividing line between already-parsed and about-to-be-parsed."
                  , "Each state starts from a few seed rules, which are then closed by adding all nonterminals that could be parsed next. Then new states are explored by advancing on terminals or nonterminals, each of which generates some new seed states."
                  , "When a full rule is parsed, it is eligible to be reduced, but this is only done when one of its lookaheads come next (highlighted in red)."
                  ]
              , D.div_ $ pure $ switcher (\(x /\ getCurrentState) -> renderStateTable { getCurrentState } x) currentStatesAndGetState
              , D.h2_ [ text_ "Table of states and parse actions" ]
              , peas
                  [ "Once the states are enumerated, the table of parse actions can be read off:"
                  , "Terminals can be “shifted” onto the stack, transitioning to a new state seeded by pushing through that terminal in all applicable rules in the current state."
                  , "Completely parsed rules will be “reduced” when their lookahead appears, popping the values matching the rule off of the stack and replacing it with the corresponding nonterminal, which then is received by the last state not involved in the rule."
                  , "Nonterminals received from another state trigger “gotos” to indicate the next state."
                  , "Two types of conflicts may occur: if a terminal indicates both a shift and reduce actions (shift–reduce conflict) or multiple reduce actions (reduce–reduce conflict). Note that there cannot be multiple shift actions at once, so most implementations (including this one) choose to do the shift action in the case of shift–reduce conflict."
                  ]
              , D.div_ $ pure $ switcher (\(x /\ getCurrentState) -> renderParseTable { getCurrentState } x) currentStatesAndGetState
              , D.h2_ [ text_ "Explore building trees in the grammar" ]
              , peas
                  [ "Each rule can be read as a transition: “this nonterminal may be replaced with this sequence of terminals and nonterminals”. Build a tree by following these state transitions, and when it consists of only terminals, send it off to be parsed below!"
                  ]
              , D.div_ [ switcher (flip explorerComponent receiveToks) currentGrammar ]
              , D.h2_ [ text_ "Input custom text to see parsing step-by-step" ]
              , peas
                  [ "Text entered here (which may also be generated by the above widgets) will be parsed step-by-step, and the final parse tree displayed if the parse succeeded. (Note that the closing terminal is automatically appended, if necessary.) Check the state tables above to see what state the current input ends up in, and the valid next terminals will be highlighted for entry."
                  ]
              , D.div_ $ pure $
                  switcher (\x -> fixed $ ([ Nothing ] <> map Just x) <#> renderTokenHere) currentGrammarTokens
              , D.div_
                  [ D.span (D.Class !:= "terminal") [ input' "Source text" "" currentValue' (\v -> push.changeText (false /\ v)) ] ]
              , D.div_ $ pure $ currentParseSteps `flip switcher` \todaysSteps ->
                  let
                    contentAsMonad = showMaybeParseSteps $ todaysSteps
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
                                            [ (biSampleOn rate ((/\) <$> startState)) <#> \(s /\ rt) -> D.OnClick := do
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
                                          (oneOf [ clickF $ pPush.toggleLeft ])
                                          [ text_ "<" ]
                                      , D.button (oneOf [ clickF $ pPush.toggleRight ])
                                          [ text_ ">" ]
                                      ]
                                , D.div_ [ content ]
                                ]
              ]
