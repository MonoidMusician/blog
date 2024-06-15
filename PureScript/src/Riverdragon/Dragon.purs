module Riverdragon.Dragon where

import Prelude
import Riverdragon.River (Event)

import Data.Maybe (Maybe)
import Effect (Effect)
import Web.Event.Event (EventType)
import Web.Event.Event as Web

data Dragon
  = Fragment (Array Dragon)
  | Replaceable (Event Dragon)
  | Collection (Event Dragon)
  | Element (Maybe String) String (Event AttrProp) Dragon

data AttrProp
  = Attr (Maybe String) String String
  | Prop String PropVal
  | Listener EventType (Maybe (Web.Event -> Effect Unit))

data PropVal
  = PropString String
  | PropBoolean Boolean
  | PropInt Int
  | PropNumber Number
