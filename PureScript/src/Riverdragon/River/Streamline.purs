module Riverdragon.River.Streamline where

import Prelude

import Effect (Effect)
import Math.Matrix (BBox2, Vec2(..), mkBounds)
import Web.DOM (Element)
import Web.DOM.Element (getBoundingClientRect)

clientRect :: Element -> Effect (BBox2 Number)
clientRect = getBoundingClientRect >>> map \bb ->
  V2 (mkBounds bb.left bb.right) (mkBounds bb.top bb.bottom)
