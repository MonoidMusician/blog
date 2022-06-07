module Parser.Main where

import Prelude

import Bolson.Core (Child(..), fixed)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (for_, oneOfMap)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (switcher, text_)
import Deku.Core (Nut, dyn, sendToTop)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event (bang, bus, keepLatest, mapAccum)
import Parser.Proto (ParseSteps(..), Stack(..), parseSteps)
import Parser.ProtoG8 (Parsed, State, g8FromString, g8ParseResult, g8Table)
import Parser.ProtoG8 as G8
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.UIEvent.KeyboardEvent (code, fromEvent)

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

showMaybeParseSteps :: forall input109158. Show input109158 => Maybe (ParseSteps input109158 (Stack State Parsed)) -> Nut
showMaybeParseSteps Nothing = text_ "Parse error"
showMaybeParseSteps (Just stack) = showParseSteps stack

showParseStep
  :: forall t66 tok982 a1787 a95
   . Show tok982
  => Show a1787
  => Show a95
  => Either (Maybe (Stack State Parsed))
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

showParseSteps :: forall input109. Show input109 => ParseSteps input109 (Stack State Parsed) -> Nut
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
