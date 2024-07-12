module Riverdragon.Dragon where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Prim.Boolean (False)
import Riverdragon.River (Stream)
import Web.Event.Event (EventType)
import Web.Event.Event as Web

data Dragon
  = Fragment (Array Dragon)
  | Replaceable (Stream False Dragon)
  | Collection (Stream False Dragon)
  | Element (Maybe String) String (Stream False AttrProp) Dragon

data AttrProp
  = Attr (Maybe String) String String
  | Prop String PropVal
  | Listener EventType (Maybe (Web.Event -> Effect Unit))

data PropVal
  = PropString String
  | PropBoolean Boolean
  | PropInt Int
  | PropNumber Number
