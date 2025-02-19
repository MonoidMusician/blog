module Riverdragon.Roar.Dimensions where

import Prelude

import Data.Number as Number
import Web.Audio.Types (Cents, Frequency, Float)

-- TIME -> TEMPO
-- TUNING

-- http://www.instrument-tuner.com/temperaments.html
-- https://www.rollingball.com/images/HT.pdf
temperaments =
  { equal: [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
  , kirnbergerIII: [10.264, 0.489, 3.421, 4.399, -3.422, 8.309, 0.488, 6.842, 2.444, 0.0, 6.354, -1.467]
  , pythagorean: [-5.865, 7.82, -1.955, -11.73, 1.955, -7.82, 5.865, -3.91, 9.775, 0.0, -9.775, 3.91]
  , pythagoreanPerfect: [15.64, 7.82, 19.55, 11.73, 1.955, 13.685, 5.865, 17.595, 9.775, 0.0, 11.73, 3.91]
  , comma1_7: [3.351, -4.468, 1.117, 6.702, -1.117, 4.468, -3.351, 2.234, -5.585, 0.0, 5.585, -2.234]
  } ::
  { equal :: Array Cents
  , kirnbergerIII :: Array Cents
  , pythagorean :: Array Cents
  , pythagoreanPerfect :: Array Cents
  , comma1_7 :: Array Cents
  }

aWeighting :: Frequency -> Number
aWeighting freq =
  let
    f2 = freq*freq
    g r = f2 + r*r
    c1 = 12194.0*12194.0
  in (c1 * f2*f2) / do g 20.6 * g 12194.0 * Number.sqrt(g 107.7 * g 737.9)

loudnessCorrection :: forall r.
  { freq :: Frequency, gain :: Float | r } ->
  { freq :: Frequency, gain :: Float | r }
loudnessCorrection r@{ freq, gain } = r { gain = gain / aWeighting freq }
