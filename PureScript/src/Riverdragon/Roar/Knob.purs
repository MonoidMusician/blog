module Riverdragon.Roar.Knob where

import Prelude

import Data.Foldable (fold, sum, traverse_)
import Riverdragon.River (Allocar, Lake, Stream, dam)
import Riverdragon.River as River
import Riverdragon.Roar.Types (RoarO, subscribing)
import Web.Audio.Node (intoParam)
import Web.Audio.Param as AudioParam
import Web.Audio.Types (AudioContext, AudioParam, AudioParamCmd(..), Float, Ramp(..), Time, currentTime)
import Web.Audio.Types as Audio

-- sum params: Array (AudioParam into ConstantSourceNode) into AudioParam
-- so maybe Roars are lists of either audio params or audio sources? and can lazily sum and convert between them …

-- multiply signals with GainNode I guess?

-- can approximate arbitrary mapping functions with WaveShaperNode

-- instance ToAudioParam (Stream flow ) where


-- const :: Float -> Knob

-- | `Knob`s are always `NotFlowing` (despite their name?)
data Knob
  = KConst Float
  | KAdd (Array Knob)
  -- | KMul (Array Knob)
  | KCmd (Lake (Time -> Allocar (Array AudioParamCmd)))
  | KAudio (Lake (Array RoarO))
  | KAware Float (Allocar Float -> Allocar Knob)
  | KStartFrom Float Knob

renderKnob :: forall rate.
  Knob ->
  AudioContext ->
  { default :: Float
  , apply :: AudioParam rate -> Allocar (Allocar Unit)
  }
renderKnob knob ctx = case knob of
  KConst v -> { default: v, apply: mempty }
  KCmd cmds -> { default: 0.0, apply: _ } \param -> do
    River.subscribe cmds \fire -> do
      cmdArray <- fire =<< currentTime ctx
      traverse_ (AudioParam.applyCmd param) cmdArray
  KAudio srcs -> { default: 0.0, apply: _ } \param -> do
    subscribing srcs (intoParam param)
  KAdd items ->
    let knobs = renderKnob <$> items <@> ctx in
    { default: sum $ knobs <#> _.default, apply: _ } \param -> do
      fold (knobs <#> _.apply <@> param)
  KAware default mkKnob -> { default, apply: _ } \param -> do
    nested <- mkKnob (AudioParam.currentValue param)
    (renderKnob nested ctx).apply param
  KStartFrom default next -> (renderKnob next ctx) { default = default }

impulse :: forall flow. Number -> Stream flow Number -> Knob
impulse k stream = KCmd $ dam stream <#> \target now -> pure
  [ CmdTarget { ramp: NoRamp, target, time: now }
  , CmdTarget { ramp: DecayRamp k, target: 0.0, time: now }
  ]
