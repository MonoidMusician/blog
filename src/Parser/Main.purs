module Parser.Main where

import Prelude

import Bolson.Core (envy, fixed, vbussed)
import Control.Alt ((<|>))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.ST.Class (class MonadST)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trampoline (Trampoline, runTrampoline)
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Compactable (compact)
import Data.Either (Either(..))
import Data.Foldable (for_, oneOfMap)
import Data.Generic.Rep (class Generic)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (class Attr, Attribute, cb, (:=))
import Deku.Control (switcher, text_)
import Deku.Core (class Korok, Domable, Nut)
import Deku.Core as DC
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (AnEvent, bang, fold, keepLatest, mapAccum, memoize, sweep)
import FRP.Event.VBus (V)
import Parser.Proto (ParseSteps(..), Stack(..), parseSteps)
import Parser.ProtoG8 (Parsed, g8FromString, g8ParseResult, g8Table)
import Parser.ProtoG8 as G8
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

type Nuts =
  forall s m lock payload
   . DC.Korok s m
  => Array (DC.Domable m lock payload)

newtype Grammar nt r tok = MkGrammar
  ( Array
      { pName :: nt
      , rName :: r
      , rule :: Fragment nt tok
      }
  )

seed
  :: forall nt r tok
   . Grammar nt r tok
  -> nt
  -> { augmented :: Grammar (Maybe nt) (Maybe r) (Maybe tok)
     , start :: StateItem (Maybe nt) (Maybe r) (Maybe tok)
     }
seed (MkGrammar rules) entry =
  let
    rule0 = { pName: Nothing, rName: Nothing, rule: [ NonTerminal (Just entry), Terminal Nothing ] }
    rules' = rules <#> \{ pName, rName, rule } ->
      { pName: Just pName
      , rName: Just rName
      , rule: bimap Just Just <$> rule
      }
  in
    { augmented: MkGrammar ([ rule0 ] <> rules')
    , start: { rName: Nothing, rule: Zipper [] rule0.rule, lookahead: [] }
    }

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
    { augmented: grammar, start } = seed initial entry
  in
    closeStates grammar [ close grammar (minimizeState [ start ]) ]

g8Grammar :: Grammar G8.Sorts G8.Rule G8.Tok
g8Grammar = MkGrammar
  [ { pName: G8.RE, rName: G8.RE1, rule: [ Terminal G8.LParen, NonTerminal G8.RL, Terminal G8.RParen ] }
  , { pName: G8.RE, rName: G8.RE2, rule: [ Terminal G8.X ] }
  , { pName: G8.RL, rName: G8.RL1, rule: [ NonTerminal G8.RE ] }
  , { pName: G8.RL, rName: G8.RL2, rule: [ NonTerminal G8.RL, Terminal G8.Comma, NonTerminal G8.RE ] }
  ]

g8Seed
  :: { augmented :: Grammar (Maybe G8.Sorts) (Maybe G8.Rule) (Maybe G8.Tok)
     , start :: StateItem (Maybe G8.Sorts) (Maybe G8.Rule) (Maybe G8.Tok)
     }
g8Seed = seed g8Grammar G8.RE

g8Generated :: forall a. a -> Array (State (Maybe G8.Sorts) (Maybe G8.Rule) (Maybe G8.Tok))
g8Generated _ = generate g8Grammar G8.RE

type SGrammar = Grammar String String CodePoint
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

type SPart = Part String CodePoint
type Fragment nt tok = Array (Part nt tok)
type SFragment = Fragment String CodePoint

data Zipper nt tok = Zipper (Fragment nt tok) (Fragment nt tok)

derive instance eqZipper :: (Eq nt, Eq tok) => Eq (Zipper nt tok)
derive instance ordZipper :: (Ord nt, Ord tok) => Ord (Zipper nt tok)
derive instance genericZipper :: Generic (Zipper state tok) _
instance showZipper :: (Show nt, Show tok) => Show (Zipper nt tok) where
  show x = genericShow x

type SZipper = Zipper String CodePoint
newtype State nt r tok = State (Array (StateItem nt r tok))

instance eqState :: (Eq nt, Eq r, Eq tok) => Eq (State nt r tok) where
  eq (State s1) (State s2) =
    let
      State s1' = minimizeState s1
    in
      let
        State s2' = minimizeState s2
      in
        let
          State s12 = minimizeState (s1' <> s2')
        in
          let
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

type SState = State String String CodePoint
type Lookahead tok = Array tok
type StateItem nt r tok =
  { rName :: r
  , rule :: Zipper nt tok
  , lookahead :: Lookahead tok
  }

newtype States s nt r tok = States
  (Array { sName :: s, state :: State nt r tok })

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
    \lookahead -> filtered <#> \{ rName, rule } -> { rName, rule: Zipper [] rule, lookahead }

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
  -> SemigroupMap (Part nt tok) (Array (StateItem nt r tok))
nextStep item@{ rule: Zipper before after } = case Array.uncons after of
  Just { head, tail } -> SemigroupMap $ Map.singleton head $ pure
    { rName: item.rName
    , rule: Zipper (before <> [ head ]) tail
    , lookahead: item.lookahead
    }
  Nothing -> SemigroupMap $ Map.empty

nextSteps
  :: forall nt r tok
   . Ord nt
  => Ord tok
  => State nt r tok
  -> SemigroupMap (Part nt tok) (Array (StateItem nt r tok))
nextSteps (State items) = Array.foldMap nextStep items

newStates
  :: forall nt r tok
   . Ord nt
  => Eq r
  => Ord tok
  => Grammar nt r tok
  -> State nt r tok
  -> Array (State nt r tok)
newStates grammar state =
  Array.nubEq (Array.fromFoldable (close grammar <<< minimizeState <$> nextSteps state))

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

-- what run are we are and how many steps are in the run
type RunAndSteps = Int /\ Int

type TopLevelUIAction = V (changeText :: String)
type ParsedUIAction = V (toggleLeft :: RunAndSteps, toggleRight :: RunAndSteps)

data TodoAction = Prioritize | Delete

showStack :: forall tok state. Show state => Show tok => Stack state tok -> Nut
showStack i = fixed (go i)
  where
  go (Zero state) = [ D.sub_ [ text_ (show state) ] ]
  go (Snoc stack tok state) = go stack
    <> [ text_ (show tok) ]
    <> [ D.sub_ [ text_ (show state) ] ]

showMaybeStack :: forall tok state. Show tok => Show state => Maybe (Stack state tok) -> Nut
showMaybeStack Nothing = text_ "Parse error"
showMaybeStack (Just stack) = showStack stack

showMaybeParseSteps :: forall input s m lock payload. Korok s m => Show input => Maybe (ParseSteps input (Stack G8.State Parsed)) -> SuperStack m (Domable m lock payload)
showMaybeParseSteps Nothing = pure (text_ "Parse error")
showMaybeParseSteps (Just stack) = showParseSteps stack

getVisibilityAndIncrement
  :: forall m s element
   . MonadST s m
  => Attr element D.Style String
  => SuperStack m (AnEvent m (Attribute element))
getVisibilityAndIncrement = getVisibilityAndIncrement' ""

getVisibilityAndIncrement'
  :: forall m s element
   . MonadST s m
  => Attr element D.Style String
  => String
  -> SuperStack m (AnEvent m (Attribute element))
getVisibilityAndIncrement' s = do
  f <- ask
  n <- get
  put (n + 1)
  pure
    ( toggle true (f n) <#> \v ->
        D.Style := (s <> "display:" <> if v then "block" else "none" <> ";")
    )

showParseStep
  :: forall r tok state inputs s m lock payload
   . Show tok
  => Show state
  => Show inputs
  => Korok s m
  => Either (Maybe (Stack G8.State Parsed))
       { inputs :: inputs
       , stack :: Stack state tok
       | r
       }
  -> SuperStack m (Domable m lock payload)
showParseStep (Left Nothing) = do
  vi <- getVisibilityAndIncrement
  pure $ D.div vi [ (text_ "Parse error") ]
showParseStep (Left (Just v)) = do
  vi <- getVisibilityAndIncrement
  pure $ D.div vi [ text_ (show (g8ParseResult v)) ]
showParseStep (Right { stack, inputs }) = do
  vi <- getVisibilityAndIncrement' "display: flex; justify-content: space-between;"
  pure $ D.div vi [ D.div_ [ showStack stack ], D.div_ [ text_ (show inputs) ] ]

type SuperStack m a = ReaderT
  (Int -> AnEvent m Unit)
  (StateT Int Trampoline)
  a

showParseSteps
  :: forall input s m lock payload
   . Show input
  => Korok s m
  => ParseSteps input (Stack G8.State Parsed)
  -> SuperStack m (Domable m lock payload)
showParseSteps i = fixed <$> (go i)
  where
  go =
    let
      s v = showParseStep v
    in
      case _ of
        Error -> do
          o <- s (Left Nothing)
          pure [ o ]
        (Complete v) -> do
          o <- s (Left (Just v))
          pure [ o ]
        (Step step more) -> do
          o <- s (Right step)
          r <- go more
          pure ([ o ] <> r)

renderState :: forall nt r tok. Show nt => Show r => Show tok => State nt r tok -> Nut
renderState (State items) = D.ul_ $ D.li_ <<< (\v -> renderItem v) <$> items

renderItem :: forall nt r tok. Show nt => Show r => Show tok => StateItem nt r tok -> Nuts
renderItem { rName, rule, lookahead } =
  [ D.span (bang (D.Class := "rule name")) [ text_ (show rName) ]
  , text_ ": "
  , renderZipper rule
  , text_ " "
  , D.span (bang (D.Class := "lookahead")) [ text_ (show lookahead) ]
  ]

renderZipper :: forall nt tok. Show nt => Show tok => Zipper nt tok -> Nut
renderZipper (Zipper before after) =
  D.span (bang (D.Class := "zipper"))
    [ D.span (bang (D.Class := "before")) $ text_ <<< show <$> before
    , D.span (bang (D.Class := "after")) $ text_ <<< show <$> after
    ]

-- the current run and the current index
type RunAndIndex = Int /\ Int

counter :: forall s m a. MonadST s m => AnEvent m a → AnEvent m (a /\ Int)
counter event = mapAccum f event 0
  where
  f a b = (b + 1) /\ (a /\ b)

dedup :: forall s m a. Eq a => Applicative m => MonadST s m => AnEvent m a -> AnEvent m a
dedup e = compact $
  mapAccum (\a b -> let ja = Just a in ja /\ (if b == ja then Nothing else Just a)) e Nothing

toggle :: forall s m a b. HeytingAlgebra b => MonadST s m => b -> AnEvent m a → AnEvent m b
toggle start event = fold (\_ x -> not x) event start

main :: Effect Unit
main = runInBody
  ( vbussed (Proxy :: _ TopLevelUIAction) \push event -> do
      let
        currentValue = counter (bang "" <|> event.changeText)
        top =
          [ D.input
              ( oneOfMap bang
                  [ D.OnInput := cb \e -> for_
                      ( target e
                          >>= fromEventTarget
                      )
                      (value >=> push.changeText)
                  ]
              )
              []
          ]
      D.div_
        [ D.style_ $ pure $ text_
            """
            .before { color: lightgray; }
          """
        , D.table_ $ pure $ D.tbody_ $
            D.tr_ <<< map D.td_ <$>
              [ [ [ text_ "E" ], [ text_ "::=" ], [ text_ "(", text_ "L", text_ ")" ], [ text_ "data E" ], [ text_ "=" ], [ text_ "E1", text_ " ", text_ "L" ] ]
              , [ [], [ text_ "|" ], [ text_ "x" ], [], [ text_ "|" ], [ text_ "E2" ] ]
              , [ [ text_ "L" ], [ text_ "::=" ], [ text_ "E" ], [ text_ "data L" ], [ text_ "=" ], [ text_ "L1", text_ " ", text_ "E" ] ]
              , [ [], [ text_ "|" ], [ text_ "L", text_ ",", text_ "E" ], [], [ text_ "|" ], [ text_ "L2", text_ " ", text_ "L", text_ " ", text_ "E" ] ]
              ]
        , D.div_ $ pure $ D.ol_ $ D.li_ <<< pure <<< (\v -> renderState v) <$> g8Generated unit
        , D.div_ top
        , D.div_ $ pure $ currentValue `flip switcher` \(v /\ count) ->
            vbussed (Proxy :: _ ParsedUIAction) \pPush pEvent ->
              let
                currentIndex = dedup $ mapAccum
                  ( \(lr /\ myRun /\ myMax) (run /\ ix) ->
                      let
                        curIx = min (myMax - 1) $ max 0
                          ( if myRun /= run
                            -- new run, so we show all steps
                            then if lr then myMax - 1 else myMax - 2
                            -- not a new run, so we can use the previous index
                            else (if lr then add else sub) ix 1
                          )
                      in
                        (myRun /\ curIx) /\ curIx
                  )
                  (((false /\ _) <$> pEvent.toggleLeft) <|> ((true /\ _) <$> pEvent.toggleRight))
                  (-1 /\ 0)
              in
                envy $ keepLatest $ memoize currentIndex \stackIndex -> sweep stackIndex \sweeper ->
                  let
                    contentAsMonad = showMaybeParseSteps $ parseSteps (unsafePartial g8Table) <$> g8FromString v <@> G8.S1
                    content /\ nEntities = runTrampoline (runStateT (runReaderT contentAsMonad sweeper) 0)
                  in
                    D.div_
                      [ D.div_
                          [ D.button (bang $ D.OnClick := pPush.toggleLeft (count /\ nEntities)) [ text_ "<" ]
                          , D.button (bang $ D.OnClick := pPush.toggleRight (count /\ nEntities)) [ text_ ">" ]
                          ]
                      , D.div_ [ content ]
                      ]
        ]
  )
