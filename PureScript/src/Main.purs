module Main where

import Prelude

import Data.Foldable (foldl)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Ref as Ref
import Foreign.Object as Object
import Parser.Main as Parser
import Parser.Main.CSS as CSS
import Parser.Main.Comb as Parser.Main.Comb
import Parser.Main.TMTTMT as TMTTMT
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (load)
import Web.HTML.HTMLDocument (readyState)
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.Window (document, requestAnimationFrame, toEventTarget)
import Widget (Widgets, instantiateAll)
import Widget.Datatypes as Widget.Datatypes
import Widget.Query as Widget.Query
import Widget.Unicode as Widget.Unicode
import Widget.Widgets as Widget.Widgets

widgets :: Widgets
widgets = foldl Object.union Object.empty
  [ Parser.widgets
  , CSS.widgets
  , TMTTMT.widgets
  , Object.fromFoldable
    [ "Widget.Query" /\ Widget.Query.widget
    , "Widget.Control" /\ Widget.Widgets.controlWidget
    , "Widget.Unicode" /\ Widget.Unicode.widget
    , "Widget.Show" /\ Widget.Unicode.widgetShow
    , "Parser.Main.Comb" /\ Parser.Main.Comb.widget
    , "" /\ Widget.Datatypes.widget
    ]
  ]

-- Returns a cleanup effect
main :: Effect (Effect Unit)
main = do
  unsub <- Ref.new mempty
  ready <- window >>= document >>= readyState
  if ready == Complete
    then flip Ref.write unsub =<< instantiateAll widgets
    else do
      l <- eventListener \_ ->
        window >>= requestAnimationFrame do
          flip Ref.write unsub =<< instantiateAll widgets
      w <- window <#> toEventTarget
      addEventListener load l true w
      Ref.write (removeEventListener load l true w) unsub
  pure $ join $ Ref.read unsub
