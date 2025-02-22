module Main where

import Prelude

import Data.Foldable (foldl)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Ref as Ref
import Foreign.Object as Object
import Parser.Main as Parser
import Parser.Main.CSS as CSS
import Parser.Main.HFS as Parser.Main.HFS
import Parser.Main.Live as Parser.Main.Live
import Parser.Main.TMTTMT as TMTTMT
import Riverdragon.Main.Live as Riverdragon.Main.Live
import Riverdragon.Roar.Live as Riverdragon.Roar.Live
import Riverdragon.Test as Riverdragon.Test
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (load)
import Web.HTML.HTMLDocument (readyState)
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.Window (document, requestAnimationFrame, toEventTarget)
import Widget (Widgets, instantiateAll)
import Widget.Datatypes as Widget.Datatypes
import Widget.Query as Widget.Query
import Widget.Roar as Widget.Roar
import Widget.Unicode as Widget.Unicode
import Widget.Widgets as Widget.Widgets

widgets :: Widgets
widgets = foldl Object.union Object.empty
  [ Parser.widgets
  , CSS.widgets
  , TMTTMT.widgets
  , Parser.Main.HFS.widgets
  , Object.fromFoldable
    [ "Widget.Query" /\ Widget.Query.widget
    , "Widget.Control" /\ Widget.Widgets.controlWidget
    , "Widget.Unicode" /\ Widget.Unicode.widget
    , "Widget.Show" /\ Widget.Unicode.widgetShow
    , "Widget.Roar.Harpsynthorg" /\ Widget.Roar.widgetHarpsynthorg
    , "Parser.Main.HFS" /\ Parser.Main.HFS.widget
    , "Parser.Main.Live" /\ Parser.Main.Live.widget
    , "Riverdragon.Main.Live" /\ Riverdragon.Main.Live.widget
    , "Riverdragon.Roar.Live" /\ Riverdragon.Roar.Live.widget
    , "Riverdragon.Test" /\ Riverdragon.Test.widget
    , "" /\ Widget.Datatypes.widget
    ]
  ]

-- Returns a cleanup effect
main :: Effect (Effect Unit)
main = installWidgets widgets

installWidgets :: Widgets -> Effect (Effect Unit)
installWidgets widgetsToInstall = do
  unsub <- Ref.new mempty
  ready <- window >>= document >>= readyState
  if ready == Complete
    then flip Ref.write unsub =<< instantiateAll widgetsToInstall
    else do
      l <- eventListener \_ ->
        window >>= requestAnimationFrame do
          flip Ref.write unsub =<< instantiateAll widgetsToInstall
      w <- window <#> toEventTarget
      addEventListener load l true w
      Ref.write (removeEventListener load l true w) unsub
  pure $ join $ Ref.read unsub
