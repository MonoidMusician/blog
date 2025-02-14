module Riverdragon.Roar.Types where

import Prelude

import Data.Compactable (compact)
import Data.Foldable (foldMap, traverse_)
import Data.Maybe (Maybe)
import Data.RecordOverloads (class RecordOverloads, overloads)
import Effect (Effect)
import Effect.Aff (Aff)
import Idiolect ((>==))
import Prim.Boolean (False)
import Riverdragon.River (Allocar, Lake, Stream, cumulate, dam, mapAl, oneStream, (>>~))
import Riverdragon.River as River
import Riverdragon.River.Bed (diffingArraySet)
import Riverdragon.River.Beyond (affToLake)
import Type.Equality (class TypeEquals, proof, to)
import Type.Proxy (Proxy(..))
import Web.Audio.Node (AudioDest, AudioSrc, createPeriodicWave, intoNode, intoParam, outOfNode)
import Web.Audio.Node as AudioNode
import Web.Audio.Param as AudioParam
import Web.Audio.Types (ARate, AudioContext, AudioNode, AudioParamCmd, Float, OscillatorType(..), SequenceFloat, Time, currentTime)
import Web.Audio.Types as Audio
import Web.Audio.Worklet (AudioWorkletNode, mkAudioWorkletteNode)

type Roar = AudioSrc

type RoarI = AudioDest
type RoarO = AudioSrc

type RoarP =
  { default :: Maybe Float
  , ctl :: RoarCtl
  }
type RoarCtl =
  -- Current value
  Allocar Float ->
  Allocar
    -- Produced value is a sum of audio inputs and the current value on
    -- the timeline generated from the commands
    { srcs :: Lake (Array RoarO)
    , cmds :: Lake AudioParamCmd
    }

withCurrentTime :: forall flow a. AudioContext -> Stream flow (Time -> a) -> Stream flow a
withCurrentTime ctx = mapAl \f -> f <$> (currentTime ctx)

connecting :: Lake (Array RoarO) -> RoarI -> Allocar (Allocar Unit)
connecting srcsStream dest = do
  srcs <- diffingArraySet
  unsub <- River.subscribe srcsStream \newSrcs -> do
    { added, removed } <- srcs.swap newSrcs
    traverse_ (AudioNode.disConnect true <@> dest) removed
    traverse_ (AudioNode.disConnect false <@> dest) added
  pure do
    unsub
    traverse_ (AudioNode.disConnect true <@> dest) =<< srcs.destroy


class ToRoars i where toRoars :: i -> Lake (Array Roar)
instance ToRoars Roar where toRoars = pure <<< pure
instance ToRoars i => ToRoars (Maybe i) where toRoars = foldMap toRoars
instance ToRoars i => ToRoars (Array i) where toRoars = cumulate <<< map toRoars
instance ToRoars i => ToRoars (Stream flow i)  where toRoars stream = stream >>~ toRoars
instance ToRoars i => ToRoars (Aff i)  where toRoars act = compact (affToLake act) >>~ toRoars

class ToLake i t where toLake :: i -> Lake t
instance ToLake i t => ToLake (Array i) t where toLake = oneStream <<< map toLake
else instance TypeEquals i t => ToLake (Stream flow i) t where toLake = dam <<< proof
else instance TypeEquals i t => ToLake (Aff i) t where toLake = compact <<< affToLake <<< proof
else instance TypeEquals i t => ToLake i t where toLake = pure <<< to

class ToOther i o where thingy :: i -> o

type OscillatorTypeOverloads =
  ( sines :: { sines :: SequenceFloat } -> OscillatorType
  , cosines :: { cosines :: SequenceFloat } -> OscillatorType
  , both :: { sines :: SequenceFloat, cosines :: SequenceFloat } -> OscillatorType
  )

instance
  ( TypeEquals ctx AudioContext
  , RecordOverloads OscillatorTypeOverloads r OscillatorType
  ) => ToOther (Record r) (ctx -> OscillatorType) where
    thingy = flip \ctx -> overloads @OscillatorTypeOverloads
      { sines: \r -> Custom $ createPeriodicWave (to ctx) false (0.0 <$ r.sines) r.sines
      , cosines: \r -> Custom $ createPeriodicWave (to ctx) false r.cosines (0.0 <$ r.cosines)
      , both: \r ->  Custom $ createPeriodicWave (to ctx) false r.cosines r.sines
      }
instance ToOther OscillatorType (ctx -> OscillatorType) where thingy = const


pinkNoise ::
  AudioContext -> Aff
    (Allocar
      { output :: RoarO
      , node :: AudioWorkletNode ()
      , destroy :: Effect Unit
      }
    )
pinkNoise = mkAudioWorkletteNode
  { name: "PinkNoiseGenerator"
  , function:
    """
      function PinkNoiseProcessor(_options, port) {
        // https://noisehack.com/generate-noise-web-audio-api/
        // Copyright (c) 2013 Zach Denton: The MIT License (MIT)
        var b0, b1, b2, b3, b4, b5, b6;
        b0 = b1 = b2 = b3 = b4 = b5 = b6 = 0.0;
        var stop = false; port.onmessage = (e) => { stop = true; };
        return function process(_inputs, outputs, _parameters) {
          for (var output of outputs) for (var channel of output) {
            for (var i = 0; i < channel.length; i++) {
              var white = Math.random() * 2 - 1;
              b0 = 0.99886 * b0 + white * 0.0555179;
              b1 = 0.99332 * b1 + white * 0.0750759;
              b2 = 0.96900 * b2 + white * 0.1538520;
              b3 = 0.86650 * b3 + white * 0.3104856;
              b4 = 0.55000 * b4 + white * 0.5329522;
              b5 = -0.7616 * b5 - white * 0.0168980;
              channel[i] = b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362;
              channel[i] *= 0.11; // (roughly) compensate for gain
              b6 = white * 0.115926;
            }
          }
          return !stop;
        };
      }
    """
  , parameters: {}
  } >== \mkNode -> do
    { node, destroy, send } <- mkNode
      { numberOfInputs: 0
      , numberOfOutputs: 1
      , outputChannelCount: [1]
      , parameterData: {}
      }
    pure
      { node
      , output: outOfNode node 0
      , destroy: send unit <> destroy
      }

rescalePower ::
  AudioContext -> Aff
    ( { input :: Lake (Array RoarO)
      , pow :: RoarP
      } -> Allocar
      { output :: RoarO
      , node :: AudioWorkletNode ( pow :: ARate )
      , destroy :: Effect Unit
      }
    )
rescalePower = mkAudioWorkletteNode
  { name: "RescalePowerNode"
  , function:
    """
      function RescalePowerNode(_options, _port) {
        var stop = false; port.onmessage = (e) => { stop = true; };
        return (inputs, outputs, parameters) => {
          const pow = parameters.pow;
          const pow0 = pow[0];
          for (var n = 0; n < outputs.length; n++) {
            const output = outputs[n];
            const input = inputs[n];
            for (var channel = 0; channel < output.length; channel++) {
              var ic = input[channel];
              var oc = output[channel];
              for (var sample = 0; sample < ic.length; sample++) {
                oc[sample] = ic[sample] ** (pow[sample] ?? pow0);
              }
            }
          }
          return !stop;
        };
      }
    """
  , parameters: { pow: { defaultValue: 1.0 } }
  } >== \mkNode { input, pow } -> do
    { send, node, destroy: destroy0 } <- mkNode
      { numberOfInputs: 1
      , numberOfOutputs: 1
      , parameterData: { pow: pow.default }
      }
    destroy1 <- connecting input (intoNode node 0)
    let powP = Audio.getParam node (Proxy :: Proxy "pow")
    powCtl <- pow.ctl (AudioParam.currentValue powP)
    destroy2 <- connecting powCtl.srcs (intoParam powP)
    destroy3 <- River.subscribe powCtl.cmds (AudioParam.applyCmd powP)
    let destroy4 = send unit
    pure
      { node
      , output: outOfNode node 0
      , destroy: destroy0 <> destroy1 <> destroy2 <> destroy3 <> destroy4
      }



