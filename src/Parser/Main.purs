module Parser.Main where

import Prelude

import Bolson.Core (fixed, vbussed)
import Control.Alt ((<|>))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trampoline (Trampoline)
import Control.Monad.Writer (WriterT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, oneOfMap)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive)
import Data.String (CodePoint)
import Deku.Attribute (cb, (:=))
import Deku.Control (switcher, text_)
import Deku.Core (class Korok, Nut, Domable)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (AnEvent, bang)
import FRP.Event.VBus (V)
import Parser.Proto (ParseSteps(..), Stack(..), parseSteps)
import Parser.ProtoG8 (Parsed, g8FromString, g8ParseResult, g8Table)
import Parser.ProtoG8 as G8
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

newtype Grammar nt r tok = MkGrammar
  ( Array
      { pName :: nt
      , rName :: r
      , rule :: Fragment nt tok
      }
  )

type SGrammar = Grammar String String CodePoint
data Part nt tok = NonTerminal nt | Terminal tok

derive instance eqPart :: (Eq nt, Eq tok) => Eq (Part nt tok)
derive instance ordPart :: (Ord nt, Ord tok) => Ord (Part nt tok)
type SPart = Part String CodePoint
type Fragment nt tok = Array (Part nt tok)
type SFragment = Fragment String CodePoint

data Zipper nt tok = Zipper (Fragment nt tok) (Fragment nt tok)
type SZipper = Zipper String CodePoint
newtype State nt r tok = State (Array (StateItem nt r tok))
type SState = State String String CodePoint
type Lookahead tok = Array (Maybe tok)
type StateItem nt r tok =
  { rName :: r
  , rule :: Zipper nt tok
  , lookahead :: Lookahead tok
  }

newtype States s nt r tok = States
  (Array { sName :: s, state :: StateItem nt r tok })

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
  Just tok -> [ Just tok ]
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
          Just tok -> readyset rules' following [ Just tok ]
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
  => Array (StateItem nt r tok)
  -> SemigroupMap (Part nt tok) (Array (StateItem nt r tok))
nextSteps = Array.foldMap nextStep

type UIAction = V (changeText :: String)

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

showMaybeParseSteps :: forall input158. Show input158 => Maybe (ParseSteps input158 (Stack G8.State Parsed)) -> Nut
showMaybeParseSteps Nothing = text_ "Parse error"
showMaybeParseSteps (Just stack) = showParseSteps stack

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
showParseStep (Left Nothing) = pure (text_ "Parse error")
showParseStep (Left (Just v)) = pure (D.div_ [ text_ (show (g8ParseResult v)) ])
showParseStep (Right { stack, inputs }) = pure
  ( D.div
      (bang (D.Style := "display: flex; justify-content: space-between"))
      [ D.div_ [ showStack stack ], D.div_ [ text_ (show inputs) ] ]
  )

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

main :: Effect Unit
main = runInBody
  ( vbussed (Proxy :: _ UIAction) \push event -> do
      let
        currentValue = bang "" <|> event.changeText
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
        [ D.table_ $ pure $ D.tbody_ $
            D.tr_ <<< map D.td_ <$>
              [ [ [ text_ "E" ], [ text_ "::=" ], [ text_ "(", text_ "L", text_ ")" ], [ text_ "data E" ], [ text_ "=" ], [ text_ "E1", text_ " ", text_ "L" ] ]
              , [ [], [ text_ "|" ], [ text_ "x" ], [], [ text_ "|" ], [ text_ "E2" ] ]
              , [ [ text_ "L" ], [ text_ "::=" ], [ text_ "E" ], [ text_ "data L" ], [ text_ "=" ], [ text_ "L1", text_ " ", text_ "E" ] ]
              , [ [], [ text_ "|" ], [ text_ "L", text_ ",", text_ "E" ], [], [ text_ "|" ], [ text_ "L2", text_ " ", text_ "L", text_ " ", text_ "E" ] ]
              ]
        , D.div_ top
        , D.div_ $ pure $ currentValue `flip switcher` \v ->
            D.div_ [ showMaybeParseSteps $ parseSteps (unsafePartial g8Table) <$> g8FromString v <@> G8.S1 ]
        ]
  )
