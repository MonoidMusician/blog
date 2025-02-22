module Riverdragon.River.Streamline where

import Prelude

import Effect (Effect)
import Web.DOM (Element)
import Web.DOM.Element (getBoundingClientRect)

type Pt d = { x :: d, y :: d }
data Interval = Interval Number Number

linmap :: Interval -> Interval -> Number -> Number
linmap (Interval a0 a1) (Interval b0 b1) a = b0 + (a - a0) * (b1 - b0) / (a1 - a0)

linmapClamp :: Interval -> Interval -> Number -> Number
linmapClamp from to pt = clamp to (linmap from to pt)

clamp :: Interval -> Number -> Number
clamp (Interval lower upper) pt = min (max lower upper) (max (min upper lower) pt)

clamp2D :: Pt Interval -> Pt Number -> Pt Number
clamp2D bounds pt = { x: clamp bounds.x pt.x, y: clamp bounds.y pt.y }

linmap2D :: Pt Interval -> Pt Interval -> Pt Number -> Pt Number
linmap2D from to pt =
  { x: linmap from.x to.x pt.x
  , y: linmap from.y to.y pt.y
  }



bbInterval ::
  Element ->
  Effect
    { x :: Interval
    , y :: Interval
    }
bbInterval el = do
  bb <- getBoundingClientRect el
  pure
    { x: Interval bb.left bb.right
    , y: Interval bb.top bb.bottom
    }

