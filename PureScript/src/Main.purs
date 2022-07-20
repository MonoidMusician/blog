module Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Object as Object
import Parser.Main as Parser
import Widget (Widgets, instantiateAll)
import Widget.Query as Widget.Query
import Widget.Types (SafeNut(..))

widgets :: Widgets
widgets = Parser.widgets `Object.union` Object.fromFoldable
  [ "Widget.Query" /\ Widget.Query.widget
  ]

-- Returns a cleanup effect
main :: Effect (Effect Unit)
main = instantiateAll widgets
