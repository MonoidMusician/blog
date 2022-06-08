module Parser.Main where

import Prelude

import Bolson.Core (Child(..), fixed)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (for_, oneOfMap)
import Data.Generic.Rep (class Generic)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (switcher, text_)
import Deku.Core (Nut, dyn, sendToTop)
import Deku.Core as DC
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event (bang, bus, keepLatest, mapAccum)
import Parser.Proto (ParseSteps(..), Stack(..), parseSteps)
import Parser.ProtoG8 (Parsed, g8FromString, g8ParseResult, g8Table)
import Parser.ProtoG8 as G8
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.UIEvent.KeyboardEvent (code, fromEvent)

type Nuts =
  forall s m lock payload
   . DC.Korok s m
  => Array (DC.Domable m lock payload)


newtype Grammar nt r tok = MkGrammar
  (Array
    { pName :: nt
    , rName :: r
    , rule :: Fragment nt tok
    }
  )

seed :: forall nt r tok. Grammar nt r tok -> nt ->
  { augmented :: Grammar (Maybe nt) (Maybe r) (Maybe tok)
  , start :: StateItem (Maybe nt) (Maybe r) (Maybe tok)
  }
seed (MkGrammar rules) entry =
  let
    rule0 = { pName: Nothing, rName: Nothing, rule: [NonTerminal (Just entry), Terminal Nothing] }
    rules' = rules <#> \{ pName, rName, rule } ->
      { pName: Just pName
      , rName: Just rName
      , rule: bimap Just Just <$> rule
      }
  in
    { augmented: MkGrammar ([rule0] <> rules')
    , start: { rName: Nothing, rule: Zipper [] rule0.rule, lookahead: [] }
    }

generate :: forall nt r tok. Ord nt => Eq r => Ord tok =>
  Grammar nt r tok -> nt ->
  Array (State (Maybe nt) (Maybe r) (Maybe tok))
generate initial entry =
  let { augmented: grammar, start } = seed initial entry in
  closeStates grammar [close grammar (minimizeState [start])]

g8Grammar :: Grammar G8.Sorts G8.Rule G8.Tok
g8Grammar = MkGrammar
  [ { pName: G8.RE, rName: G8.RE1, rule: [Terminal G8.LParen, NonTerminal G8.RL, Terminal G8.RParen] }
  , { pName: G8.RE, rName: G8.RE2, rule: [Terminal G8.X] }
  , { pName: G8.RL, rName: G8.RL1, rule: [NonTerminal G8.RE] }
  , { pName: G8.RL, rName: G8.RL2, rule: [NonTerminal G8.RL, Terminal G8.Comma, NonTerminal G8.RE] }
  ]

g8Seed ::
  { augmented :: Grammar (Maybe G8.Sorts) (Maybe G8.Rule) (Maybe G8.Tok)
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
    let State s1' = minimizeState s1 in
    let State s2' = minimizeState s2 in
    let State s12 = minimizeState (s1' <> s2') in
    let State s21 = minimizeState (s2' <> s1') in
    s1' == s12 && s2' == s21
instance ordState :: (Ord nt, Ord r, Ord tok) => Ord (State nt r tok) where
  compare (State s1) (State s2) = compare (deepSort s1) (deepSort s2) where
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
      if item.rName == newItem.rName && item.rule == newItem.rule
        then { accum: true, value: item { lookahead = Array.nubEq (item.lookahead <> newItem.lookahead) } }
        else { accum: alreadyFound, value: item }
    { accum: found, value: items' } =
      mapAccumL accumulate false items
  in if found then items' else items' <> [newItem]

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

findNT :: forall nt tok. Zipper nt tok -> Maybe
  { nonterminal :: nt, following :: Array nt, continue :: Maybe tok }
findNT (Zipper _ after) = Array.uncons after >>= case _ of
  { head: NonTerminal nt, tail } ->
    let { following, continue } = preview tail
    in Just { nonterminal: nt, following, continue }
  _ -> Nothing

preview :: forall nt tok.
  Array (Part nt tok) -> { following :: Array nt, continue :: Maybe tok }
preview tail = { following, continue }
  where
    { init, rest } = Array.span isNonTerminal tail
    following = Array.mapMaybe unNonTerminal init
    continue = Array.head rest >>= unTerminal

continueOn :: forall tok. Maybe tok -> Lookahead tok -> Lookahead tok
continueOn continue lookahead = case continue of
  Just tok -> [tok]
  Nothing -> lookahead

startRules :: forall nt r tok. Eq nt => Grammar nt r tok -> nt -> (Lookahead tok -> Array (StateItem nt r tok))
startRules (MkGrammar rules) p =
  let filtered = Array.filter (\{ pName } -> pName == p) rules in
  \lookahead -> filtered <#> \{ rName, rule } -> { rName, rule: Zipper [] rule, lookahead }

closeItem :: forall nt r tok. Eq nt => Grammar nt r tok -> StateItem nt r tok -> Array (StateItem nt r tok)
closeItem grammar item = case findNT item.rule of
  Nothing -> []
  Just { nonterminal: p, following, continue } ->
    startRules grammar p $
      firsts grammar following (continueOn continue item.lookahead)

close1 :: forall nt r tok. Eq nt => Grammar nt r tok -> State nt r tok -> Array (StateItem nt r tok)
close1 grammar (State items) = closeItem grammar =<< items

close :: forall nt r tok. Eq r => Eq nt => Eq tok =>
  Grammar nt r tok ->
  State nt r tok -> State nt r tok
close grammar state0 =
  let state' = close1 grammar state0 in
  if Array.null state' then state0 else
  let state = state0 <> State state' in
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
      in matches >>= _.rule >>> preview >>> \{ following, continue } ->
        -- (p : following continue) (ps lookahead)
        case continue of
          Just tok -> readyset rules' following [tok]
          Nothing -> readyset rules' (following <> ps) lookahead

nextStep :: forall nt r tok.
  StateItem nt r tok -> SemigroupMap (Part nt tok) (Array (StateItem nt r tok))
nextStep item@{ rule: Zipper before after } = case Array.uncons after of
  Just { head, tail } -> SemigroupMap $ Map.singleton head $ pure
    { rName: item.rName
    , rule: Zipper (before <> [head]) tail
    , lookahead: item.lookahead
    }
  Nothing -> SemigroupMap $ Map.empty

nextSteps :: forall nt r tok. Ord nt => Ord tok =>
  State nt r tok -> SemigroupMap (Part nt tok) (Array (StateItem nt r tok))
nextSteps (State items) = Array.foldMap nextStep items

newStates :: forall nt r tok. Ord nt => Eq r => Ord tok =>
  Grammar nt r tok -> State nt r tok -> Array (State nt r tok)
newStates grammar state =
  Array.nubEq (Array.fromFoldable (close grammar <<< minimizeState <$> nextSteps state))

closeStates1 :: forall nt r tok. Ord nt => Eq r => Ord tok =>
  Grammar nt r tok -> Array (State nt r tok) -> Array (State nt r tok)
closeStates1 grammar states = Array.nubEq (states <> (states >>= newStates grammar))

closeStates :: forall nt r tok. Ord nt => Eq r => Ord tok =>
  Grammar nt r tok -> Array (State nt r tok) -> Array (State nt r tok)
closeStates grammar states =
  let states' = closeStates1 grammar states in
  if states' == states then states else closeStates grammar states'

data MainUIAction
  = UIShown
  | AddTodo
  | ChangeText String

data TodoAction = Prioritize | Delete

showStack :: forall tok9 a17. Show a17 => Show tok9 => Stack a17 tok9 -> Nut
showStack i = fixed (go i)
  where
  go (Zero state) = [ D.sub_ [ text_ (show state) ] ]
  go (Snoc stack tok state) = go stack
    <> [ text_ (show tok) ]
    <> [ D.sub_ [ text_ (show state) ] ]

showMaybeStack :: forall tok952 a1757. Show tok952 => Show a1757 => Maybe (Stack a1757 tok952) -> Nut
showMaybeStack Nothing = text_ "Parse error"
showMaybeStack (Just stack) = showStack stack

showMaybeParseSteps :: forall input109158. Show input109158 => Maybe (ParseSteps input109158 (Stack G8.State Parsed)) -> Nut
showMaybeParseSteps Nothing = text_ "Parse error"
showMaybeParseSteps (Just stack) = showParseSteps stack

showParseStep
  :: forall t66 tok982 a1787 a95
   . Show tok982
  => Show a1787
  => Show a95
  => Either (Maybe (Stack G8.State Parsed))
       { inputs :: a95
       , stack :: Stack a1787 tok982
       | t66
       }
  -> Nut
showParseStep (Left Nothing) = text_ "Parse error"
showParseStep (Left (Just v)) = D.div_ [ text_ (show (g8ParseResult v)) ]
showParseStep (Right { stack, inputs }) = D.div
  (bang (D.Style := "display: flex; justify-content: space-between"))
  [ D.div_ [ showStack stack ], D.div_ [ text_ (show inputs) ] ]

showParseSteps :: forall input109. Show input109 => ParseSteps input109 (Stack G8.State Parsed) -> Nut
showParseSteps i = fixed (go i)
  where
  go =
    let
      s v = showParseStep v
    in
      case _ of
        Error -> [ s (Left Nothing) ]
        (Complete v) -> [ s (Left (Just v)) ]
        (Step step more) -> [ s (Right step) ] <> go more

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

main :: Effect Unit
main = runInBody1
  ( bus \push -> lcmap (bang UIShown <|> _) \event -> do
      let
        currentValue =
          bang "" <|>
            flip filterMap event case _ of
              ChangeText s -> Just s
              _ -> Nothing
      let
        top =
          [ D.input
              ( oneOfMap bang
                  [ D.OnInput := cb \e -> for_
                      ( target e
                          >>= fromEventTarget
                      )
                      ( value
                          >=> push <<< ChangeText
                      )
                  , D.OnKeyup := cb
                      \e -> for_ (fromEvent e) \evt -> do
                        when (code evt == "Enter") $ do
                          push AddTodo
                  ]
              )
              []
          , D.button
              (bang $ D.OnClick := push AddTodo)
              [ text_ "Add" ]
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
        , D.div_ $ pure $ currentValue `flip switcher` \v ->
            D.div_ [ showMaybeParseSteps $ parseSteps (unsafePartial g8Table) <$> g8FromString v <@> G8.S1 ]
        , D.div_
            [ dyn $
                map
                  ( \txt -> keepLatest $ bus \p' e' ->
                      ( bang $ Insert $ D.div_
                          [ text_ txt
                          , D.button
                              ( bang
                                  $ D.OnClick := p' sendToTop
                              )
                              [ text_ "Prioritize" ]
                          , D.button
                              ( bang
                                  $ D.OnClick := p' Remove
                              )
                              [ text_ "Delete" ]
                          ]
                      ) <|> e'
                  )
                  ( filterMap
                      ( \(tf /\ s) ->
                          if tf then Just s else Nothing
                      )
                      ( mapAccum
                          ( \a b -> case a of
                              ChangeText s -> s /\ (false /\ s)
                              AddTodo -> b /\ (true /\ b)
                              _ -> "" /\ (false /\ "")
                          )
                          event
                          ""
                      )
                  )
            ]
        ]
  )
