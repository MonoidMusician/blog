module Riverdragon.Roar.Sugar where

import Prelude

import Data.RecordOverloads (class Labels)
import Effect.Class (liftEffect)
import Riverdragon.Roar.Knob (class ToKnob, toKnob)
import Riverdragon.Roar.Types (class ToRoars, Roar, pinkNoise, toRoars)
import Riverdragon.Roar.Yawn (YawnM, yaaawn)
import Riverdragon.Roar.Yawn as Yawn
import Type.Equality (proof)

class ToNode i o | i -> o where toNode :: i -> YawnM o

instance (Labels tgt overload, RecordToNode overload tgt o) => ToNode (Record tgt) o where
  toNode = recordToNode @overload

class RecordToNode (overload :: Symbol) tgt o | overload -> tgt o where
  recordToNode :: Record tgt -> YawnM o
instance
  ( ToKnob knob
  , ToRoars signal
  ) => RecordToNode "gain,signal" ( gain :: knob, signal :: signal ) Roar where
  recordToNode = proof >>> \tgt -> Yawn.gain (toRoars tgt.signal) (toKnob tgt.gain)

data Noise = PinkNoise

instance ToNode Noise Roar where
  toNode PinkNoise = yaaawn \{ ctx } -> do
    { destroy, output } <- liftEffect =<< pinkNoise ctx
    pure { destroy, result: output, ready: mempty }
