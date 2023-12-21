module Widget.Widgets where

import Prelude

import Control.Alt ((<|>))
import Data.Codec.Argonaut as CA
import Data.Maybe (fromMaybe)
import Data.Traversable (for_, oneOf)
import Deku.Attribute ((!:=))
import Deku.Control (text)
import Deku.DOM as D
import Web.DOM.DOMTokenList as TL
import Web.DOM.Element (classList)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)
import Widget (Widget, adaptInterface)
import Widget.Types (SafeNut(..))

controlWidget :: Widget
controlWidget { interface } = do
  let
    focus_mode = adaptInterface CA.boolean (interface "focus_mode")
    setFocusMode fm = do
      mb <- window >>= document >>= body
      for_ mb \b -> do
        cl <- classList (toElement b)
        (if fm then TL.add else TL.remove) cl "focus-mode"
  pure $ SafeNut do
    D.div (D.Class !:= "widgets-ui")
      [ D.button
          (oneOf
            [ D.OnClick !:= ((focus_mode.send <> setFocusMode) <<< not <<< fromMaybe false =<< focus_mode.current)
            , D.Class !:= "big bonus"
            ]
          )
          [ text $ (pure false <|> focus_mode.loopback) <#> if _ then "Leave dashboard mode" else "Enter dashboard mode" ]
      ]
