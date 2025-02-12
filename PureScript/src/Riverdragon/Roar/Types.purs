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
import Riverdragon.River (Allocar, Lake, Stream, mapAl, (<?*>))
import Riverdragon.River as River
import Riverdragon.River.Bed (diffingArraySet)
import Type.Proxy (Proxy(..))
import Web.Audio.Context (createAudioContext)
import Web.Audio.Node (AudioDest, AudioSrc, destination, intoNode, intoParam, outOfNode)
import Web.Audio.Node as AudioNode
import Web.Audio.Param as AudioParam
import Web.Audio.Types (ARate, AudioContext, AudioNode, AudioParamCmd(..), Float, OscillatorType(..), Ramp(..), Time, currentTime)
import Web.Audio.Types as Audio
import Web.Audio.Worklet (mkAudioWorkletteNode)

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

fixed :: Float -> RoarP
fixed default = { default: Just default, ctl: const $ pure { srcs: empty, cmds: empty } }

roaring :: forall o.
  {} ->
  (AudioContext -> Aff
    { outputs :: Lake (Array RoarO)
    , output :: o
    , start :: Effect (Effect Unit)
    }
  ) ->
  Aff
    { ctx :: AudioContext
    , output :: o
    , destroy :: Allocar Unit
    }
roaring options f = do
  ctx <- liftEffect $ createAudioContext options
  { outputs, output, start } <- f ctx
  destroy2 <- liftEffect $ subscribing outputs (destination ctx)
  destroy3 <- liftEffect $ start
  pure { ctx, output, destroy: destroy2 <> destroy3 }



withCurrentTime :: forall flow a. AudioContext -> Stream flow (Time -> a) -> Stream flow a
withCurrentTime ctx = mapAl \f -> f <$> (currentTime ctx)

targeting :: AudioContext -> Lake { ramp :: Ramp, duration :: Number } -> Lake Number -> Lake AudioParamCmd
targeting ctx ramping targets = withCurrentTime ctx (f <$> ramping <?*> targets)
  where
  f { duration, ramp } target currentTime = CmdTarget
    { ramp, target, time }
    where
    time = case ramp of
      NoRamp -> currentTime
      LinRamp -> currentTime + duration
      ExpRamp -> currentTime + duration
      DecayRamp _ -> currentTime

subscribing :: Lake (Array RoarO) -> RoarI -> Allocar (Allocar Unit)
subscribing srcsStream dest = do
  srcs <- diffingArraySet
  unsub <- River.subscribe srcsStream \newSrcs -> do
    { added, removed } <- srcs.swap newSrcs
    traverse_ (AudioNode.disConnect true <@> dest) removed
    traverse_ (AudioNode.disConnect false <@> dest) added
  pure do
    unsub
    traverse_ (AudioNode.disConnect true <@> dest) =<< srcs.destroy

createSines ::
  { ctx :: AudioContext
  , frequency :: RoarP
  , detune :: RoarP
  , sines :: Array Float
  } -> Allocar
  { output :: RoarO
  , node :: AudioNode "OscillatorNode" True () () ( detune :: ARate, frequency :: ARate )
  , destroy :: Effect Unit
  }
createSines { ctx, frequency, detune, sines } = do
  node <- AudioNode.createOscillatorNode ctx
    { detune: detune.default
    , frequency: frequency.default
    , type: Custom $ AudioNode.createPeriodicWave ctx true (0.0 <$ sines) sines
    }
  pure { node, output: outOfNode node 0, destroy: mempty }

constant :: Float -> AudioContext -> Allocar RoarO
constant val ctx = do
  node <- AudioNode.createConstantSourceNode ctx { offset: val }
  AudioNode.startNow node
  pure $ outOfNode node 0

createGainNode ::
  { ctx :: AudioContext
  , input :: Lake (Array RoarO)
  , gain :: RoarP
  } -> Allocar
  { output :: RoarO
  , node :: AudioNode "GainNode" False () () ( gain :: ARate )
  , destroy :: Effect Unit
  }
createGainNode { ctx, input, gain } = do
  node <- AudioNode.createGainNode ctx { gain: gain.default }
  destroy1 <- subscribing input (intoNode node 0)
  let gainP = Audio.getParam node (Proxy :: Proxy "gain")
  gainCtl <- gain.ctl (AudioParam.currentValue gainP)
  destroy2 <- subscribing gainCtl.srcs (intoParam gainP)
  destroy3 <- River.subscribe gainCtl.cmds (AudioParam.applyCmd gainP)
  pure { node, output: outOfNode node 0, destroy: destroy1 <> destroy2 <> destroy3 }




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
    destroy1 <- subscribing input (intoNode node 0)
    let powP = Audio.getParam node (Proxy :: Proxy "pow")
    powCtl <- pow.ctl (AudioParam.currentValue powP)
    destroy2 <- subscribing powCtl.srcs (intoParam powP)
    destroy3 <- River.subscribe powCtl.cmds (AudioParam.applyCmd powP)
    pure
      { node
      , output: outOfNode node 0
      , destroy: destroy0 <> destroy1 <> destroy2 <> destroy3
      }



