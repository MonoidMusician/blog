module Main where

import Prelude

import Airplane.Main as Airplane.Main
import Data.Foldable (foldl)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Object as Object
import Lite as Lite
import Parser.Main as Parser
import Parser.Main.CSS as CSS
import Parser.Main.HFS as Parser.Main.HFS
import Parser.Main.Live as Parser.Main.Live
import Parser.Main.TMTTMT as TMTTMT
import Riverdragon.Main.Live as Riverdragon.Main.Live
import Riverdragon.Roar.LaunchkeyMK4 as LaunchkeyMK4
import Riverdragon.Roar.Live as Riverdragon.Roar.Live
import Riverdragon.Test as Riverdragon.Test
import Train.Main as Train.Main
import Widget (Widgets)
import Widget.Datatypes as Widget.Datatypes
import Widget.Playground as Widget.Playground
import Widget.Query as Widget.Query
import Widget.Roar as Widget.Roar
import Widget.Unicode as Widget.Unicode
import Widget.Widgets as Widget.Widgets

widgets :: Widgets
widgets = foldl Object.union Object.empty
  [ Lite.widgets
  , Parser.widgets
  , CSS.widgets
  , TMTTMT.widgets
  , Parser.Main.HFS.widgets
  , Object.fromFoldable
    [ "Widget.Query" /\ Widget.Query.widget
    , "Widget.Control" /\ Widget.Widgets.controlWidget
    , "Widget.Unicode" /\ Widget.Unicode.widget
    , "Widget.Show" /\ Widget.Unicode.widgetShow
    , "Widget.Roar.Harpsynthorg" /\ Widget.Roar.widgetHarpsynthorg
    , "Widget.Roar.Launchkey" /\ LaunchkeyMK4.widget
    , "Widget.Playground" /\ Widget.Playground.widget
    , "Parser.Main.HFS" /\ Parser.Main.HFS.widget
    , "Parser.Main.Live" /\ Parser.Main.Live.widget
    , "Riverdragon.Main.Live" /\ Riverdragon.Main.Live.widget
    , "Riverdragon.Roar.Live" /\ Riverdragon.Roar.Live.widget
    , "Riverdragon.Test" /\ Riverdragon.Test.widget
    , "Airplane" /\ Airplane.Main.widget
    , "Traintle" /\ Train.Main.widget
    , "" /\ Widget.Datatypes.widget
    ]
  ]

-- Returns a cleanup effect
main :: Effect (Effect Unit)
main = Lite.installWidgets widgets
