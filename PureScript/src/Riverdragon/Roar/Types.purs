module Riverdragon.Roar.Types where

import Prelude

import Control.Plus (empty)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Idiolect ((>==))
import Prim.Boolean (False, True)
import Riverdragon.River (Allocar, Lake, Stream, cumulate, dam, mapAl, oneStream, (<?*>), (>>~))
import Riverdragon.River as River
import Riverdragon.River.Bed (diffingArraySet)
import Type.Equality (class TypeEquals, proof)
import Type.Proxy (Proxy(..))
import Web.Audio.Context (createAudioContext)
import Web.Audio.Node (AudioDest, AudioSrc, destination, intoNode, intoParam, outOfNode)
import Web.Audio.Node as AudioNode
import Web.Audio.Param as AudioParam
import Web.Audio.Types (ARate, AudioContext, AudioNode, AudioParamCmd(..), Float, OscillatorType(..), Ramp(..), Time, currentTime)
import Web.Audio.Types as Audio
import Web.Audio.Worklet (mkAudioWorkletteNode)

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
instance ToRoars i => ToRoars (Array i) where toRoars = cumulate <<< map toRoars
instance ToRoars i => ToRoars (Stream flow i)  where toRoars stream = stream >>~ toRoars

class ToLake i t where toLake :: i -> Lake t
instance ToLake t t where toLake = pure
else instance ToLake i t => ToLake (Array i) t where toLake = oneStream <<< map toLake
else instance TypeEquals i t => ToLake (Stream flow i) t  where toLake = dam <<< proof


pinkNoise ::
  AudioContext -> Aff
    (Allocar
      { output :: RoarO
      , node :: AudioNode "AudioWorkletNode" False () () ()
      , destroy :: Effect Unit
      }
    )
pinkNoise = mkAudioWorkletteNode
  { name: "PinkNoiseGenerator"
  , function:
    """
      function PinkNoiseProcessor(_options, _port) {
        // https://noisehack.com/generate-noise-web-audio-api/
        // Copyright (c) 2013 Zach Denton: The MIT License (MIT)
        var b0, b1, b2, b3, b4, b5, b6;
        b0 = b1 = b2 = b3 = b4 = b5 = b6 = 0.0;
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
          return true;
        };
      }
    """
  , parameters: {}
  } >== \mkNode -> do
    { node, destroy } <- mkNode
      { numberOfInputs: 0
      , numberOfOutputs: 1
      , outputChannelCount: [1]
      , parameterData: {}
      }
    pure
      { node
      , output: outOfNode node 0
      , destroy
      }

rescalePower ::
  AudioContext -> Aff
    ( { input :: Lake (Array RoarO)
      , pow :: RoarP
      } -> Allocar
      { output :: RoarO
      , node :: AudioNode "AudioWorkletNode" False () () ( pow :: ARate )
      , destroy :: Effect Unit
      }
    )
rescalePower = mkAudioWorkletteNode
  { name: "RescalePowerNode"
  , function:
    """
      function RescalePowerNode(_options, _port) {
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
          return true;
        };
      }
    """
  , parameters: { pow: { defaultValue: 1.0 } }
  } >== \mkNode { input, pow } -> do
    { node, destroy: destroy0 } <- mkNode
      { numberOfInputs: 1
      , numberOfOutputs: 1
      , parameterData: { pow: pow.default }
      }
    destroy1 <- connecting input (intoNode node 0)
    let powP = Audio.getParam node (Proxy :: Proxy "pow")
    powCtl <- pow.ctl (AudioParam.currentValue powP)
    destroy2 <- connecting powCtl.srcs (intoParam powP)
    destroy3 <- River.subscribe powCtl.cmds (AudioParam.applyCmd powP)
    pure
      { node
      , output: outOfNode node 0
      , destroy: destroy0 <> destroy1 <> destroy2 <> destroy3
      }



