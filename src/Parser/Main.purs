module Parser.Main where

import Prelude

import Bolson.Core (Child(..), Entity)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (for_, oneOfMap)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (switcher, text_)
import Deku.Core (class Korok, Node, dyn, sendToTop)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event (bang, bus, keepLatest, mapAccum)
import Parser.Proto (ParseSteps(..), Stack(..), parseSteps)
import Partial.Unsafe (unsafePartial)
import Parser.ProtoG8 (Parsed, State, g8FromString, g8ParseResult, g8Table)
import Parser.ProtoG8 as G8
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.UIEvent.KeyboardEvent (code, fromEvent)

data MainUIAction
  = UIShown
  | AddTodo
  | ChangeText String

data TodoAction = Prioritize | Delete

showStack :: forall tok9 s10 m11 lock12 payload13 a17. Korok s10 m11 => Show tok9 => Show a17 => Stack a17 tok9 -> Array (Entity Unit (Node m11 lock12 payload13) m11 lock12)
showStack (Zero state) = [ D.sub_ [ text_ (show state) ] ]
showStack (Snoc stack tok state) = showStack stack <>
  [ text_ (show tok) ] <> [ D.sub_ [ text_ (show state) ] ]

showMaybeStack :: forall m47 lock48 payload49 tok952 s1053 a1757. Monad m47 => Korok s1053 m47 => Show tok952 => Show a1757 => Maybe (Stack a1757 tok952) -> Array (Entity Unit (Node m47 lock48 payload49) m47 lock48)
showMaybeStack Nothing = [ text_ "Parse error" ]
showMaybeStack (Just stack) = showStack stack

showMaybeParseSteps :: forall m149 lock150 payload151 s55102157 input109158. Monad m149 => Korok s55102157 m149 => Show input109158 => Maybe (ParseSteps input109158 (Stack State Parsed)) -> Array (Entity Unit (Node m149 lock150 payload151) m149 lock150)
showMaybeParseSteps Nothing = [ text_ "Parse error" ]
showMaybeParseSteps (Just stack) = showParseSteps stack

showParseStep :: forall m49 lock50 payload51 s55 t66 tok982 a1787 a95.
  Monad m49 => Korok s55 m49 => Show tok982 => Show a1787 => Show a95 => Either (Maybe (Stack State Parsed))
                                                                           { inputs :: a95
                                                                           , stack :: Stack a1787 tok982
                                                                           | t66
                                                                           }
                                                                         -> Entity Unit (Node m49 lock50 payload51) m49 lock50
showParseStep (Left Nothing) = text_ "Parse error"
showParseStep (Left (Just v)) = D.div_ [ text_ (show (g8ParseResult v)) ]
showParseStep (Right { stack, inputs }) = D.div
  (bang (D.Style := "display: flex; justify-content: space-between"))
  [ D.div_ (showStack stack), D.div_ [ text_ (show inputs) ] ]


showParseSteps :: forall m4999 lock50100 payload51101 s55102 input109. Monad m4999 => Korok s55102 m4999 => Show input109 => ParseSteps input109 (Stack State Parsed) -> Array (Entity Unit (Node m4999 lock50100 payload51101) m4999 lock50100)
showParseSteps =
  let s v = showParseStep v in
  case _ of
    Error -> [ s (Left Nothing) ]
    (Complete v) -> [ s (Left (Just v)) ]
    (Step step more) -> [ s (Right step) ] <> showParseSteps more

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
            , [ [           ], [ text_ "|"   ], [ text_ "x" ], [], [ text_ "|" ], [ text_ "E2" ] ]
            , [ [ text_ "L" ], [ text_ "::=" ], [ text_ "E" ], [ text_ "data L" ], [ text_ "=" ], [ text_ "L1", text_ " ", text_ "E" ] ]
            , [ [           ], [ text_ "|"   ], [ text_ "L", text_ ",", text_ "E" ], [], [ text_ "|" ], [ text_ "L2", text_ " ", text_ "L", text_ " ", text_ "E" ] ]
            ]
        , D.div_ top
        , D.div_ $ pure $ currentValue `flip switcher` \v ->
            D.div_ $ showMaybeParseSteps $ parseSteps (unsafePartial g8Table) <$> g8FromString v <@> G8.S1
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
