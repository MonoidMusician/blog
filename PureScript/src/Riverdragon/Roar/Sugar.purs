module Riverdragon.Roar.Sugar where

import Prelude

import Data.RecordOverloads (class Labels)
import Riverdragon.Roar.Knob (class ToKnob, toKnob)
import Riverdragon.Roar.Roarlette as YY
import Riverdragon.Roar.Types (class ToRoars, Roar, toRoars)
import Riverdragon.Roar.Score (ScoreM)
import Riverdragon.Roar.Score as Y
import Type.Equality (proof)

class ToNode i o | i -> o where toNode :: i -> ScoreM o

instance ToNode Roar Roar where toNode = pure
instance (Labels tgt overload, RecordToNode overload tgt o) => ToNode (Record tgt) o where
  toNode = recordToNode @overload

class RecordToNode (overload :: Symbol) tgt o | overload -> tgt o where
  recordToNode :: Record tgt -> ScoreM o
instance
  ( ToKnob knob
  , ToRoars signal
  ) => RecordToNode "gain,signal" ( gain :: knob, signal :: signal ) Roar where
  recordToNode = proof >>> \tgt -> Y.gain (toKnob tgt.gain) (toRoars tgt.signal)

data Noise = PinkNoise

instance ToNode Noise Roar where
  toNode PinkNoise = YY.pinkNoise
