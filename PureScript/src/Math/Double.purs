module Math.Double where

import Prelude

import Data.Number as Math

-- c.f. Java/C "nextAfter"
-- https://stackoverflow.com/questions/72185266/how-to-get-closest-floating-point-number-that-is-less-greater-than-a-given-numbe
-- https://gist.github.com/Yaffle/4654250
-- https://marc-b-reynolds.github.io/math/2019/05/14/FloatCmp.html
nudgeTowards :: Number -> Number -> Number
nudgeTowards start destination
  | destination < start = nudgeBy start (-1)
  | destination > start = nudgeBy start 1
  | destination == start = destination
  | otherwise = start + destination -- NaN(s)

foreign import nudgeBy :: Number -> Int -> Number

ulp :: Number -> Number
ulp v | { above, below } <- adjacent v =
  Math.max (above - v) (v - below)

adjacent :: Number -> { above :: Number, below :: Number }
adjacent v = { above: nudgeBy v 1, below: nudgeBy v (-1) }
