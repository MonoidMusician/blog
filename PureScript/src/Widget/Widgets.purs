module Widget.Widgets where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Data.Traversable (for_)
import Riverdragon.Dragon.Bones ((=:=))
import Riverdragon.Dragon.Bones as D
import Web.DOM.DOMTokenList as TL
import Web.DOM.Element (classList)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)
import Widget (Widget, autoAdaptInterface)

controlWidget :: Widget
controlWidget { interface } = do
  let
    focus_mode = autoAdaptInterface (interface "focus_mode")
    setFocusMode fm = do
      mb <- window >>= document >>= body
      for_ mb \b -> do
        cl <- classList (toElement b)
        (if fm then TL.add else TL.remove) cl "focus-mode"
  pure do
    D.div [ D.className =:= "widgets-ui" ] $
      D.buttonW' (pure "big bonus")
        do (pure false <|> focus_mode.loopback) <#> if _ then "Leave dashboard mode" else "Enter dashboard mode"
        do ((focus_mode.send <> setFocusMode) <<< not <<< fromMaybe false =<< focus_mode.current)
