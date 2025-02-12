module Web.Audio.Param where

import Prelude (Unit)
import Web.Audio.Types

import Effect (Effect)

applyCmd :: forall rate. AudioParam rate -> AudioParamCmd -> Effect Unit
applyCmd param = case _ of
  CmdCurve r -> setValueCurveAtTime param r.curve r.time r.duration
  CmdCancel r@{ hold: Hold } -> cancelAndHoldAtTime param r.time
  CmdCancel r@{ hold: Revert } -> cancelScheduledValues param r.time
  CmdTarget r@{ ramp: NoRamp } -> setValueAtTime param r.target r.time
  CmdTarget r@{ ramp: LinRamp } -> linearRampToValueAtTime param r.target r.time
  CmdTarget r@{ ramp: ExpRamp } -> exponentialRampToValueAtTime param r.target r.time
  CmdTarget r@{ ramp: DecayRamp scale } -> setTargetAtTime param r.target r.time scale

foreign import cancelAndHoldAtTime :: forall rate. AudioParam rate -> Time -> Effect Unit
foreign import cancelScheduledValues :: forall rate. AudioParam rate -> Time -> Effect Unit
foreign import exponentialRampToValueAtTime :: forall rate. AudioParam rate -> Float -> Time -> Effect Unit
foreign import linearRampToValueAtTime :: forall rate. AudioParam rate -> Float -> Time -> Effect Unit
foreign import setTargetAtTime :: forall rate. AudioParam rate -> Float -> Time -> Duration -> Effect Unit
foreign import setValueAtTime :: forall rate. AudioParam rate -> Float -> Time -> Effect Unit
foreign import setValueCurveAtTime :: forall rate. AudioParam rate -> SequenceFloat -> Time -> Duration -> Effect Unit

foreign import defaultValue :: forall rate. AudioParam rate -> Float
foreign import maxValue :: forall rate. AudioParam rate -> Float
foreign import minValue :: forall rate. AudioParam rate -> Float
foreign import currentValue :: forall rate. AudioParam rate -> Effect Float
