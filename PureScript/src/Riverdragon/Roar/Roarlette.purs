module Riverdragon.Roar.Roarlette where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Riverdragon.River (createRiverStore)
import Riverdragon.River.Bed (cleanup, postHocDestructors, runningAff)
import Riverdragon.River.Beyond (affToLake)
import Riverdragon.Roar.Knob (class ToKnob, renderKnobs)
import Riverdragon.Roar.Types (class ToRoars, Roar, connecting, toRoars)
import Riverdragon.Roar.Yawn (YawnM)
import Riverdragon.Roar.Yawn as Y
import Web.Audio.Node (intoNode, outOfNode)
import Web.Audio.Types (ARate)
import Web.Audio.Worklet (AudioWorkletNode, mkAudioWorkletteNode)

pinkNoise :: YawnM Roar
pinkNoise = do
  { ctx } <- ask
  destr <- liftEffect postHocDestructors
  creating <- liftEffect $ runningAff do
    { node, destroy, send } <- mkAudioWorkletteNode
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
      } ctx >>= \mkNode -> liftEffect $ mkNode
        { numberOfInputs: 0
        , numberOfOutputs: 1
        , outputChannelCount: [1]
        , parameterData: {}
        }
    liftEffect $ destr.destructor $ send unit <> destroy
    pure $ outOfNode node 0
  Y.putYawn { destroy: destr.finalize, ready: void creating }
  output <- Y.gain creating 1
  pure output

-- runningAffUntil

pow :: forall roar knob. ToRoars roar => ToKnob knob => roar -> knob -> YawnM Roar
pow input knob = do
  { ctx } <- ask
  destr <- liftEffect postHocDestructors
  { stream: whenDestroyed, send: destroyOutput } <- liftEffect $ createRiverStore Nothing
  liftEffect $ destr.destructor $ destroyOutput Nothing
  let { defaults, apply: applyKnobs } = renderKnobs { pow: knob } ctx
  creating <- liftEffect $ runningAff do
    { node, destroy: destroy0, send } <- mkAudioWorkletteNode
      { name: "RescalePowerNode"
      , function:
        """
          function RescalePowerNode(_options, port) {
            var stop = false; port.onmessage = (e) => { stop = true; };
            return (inputs, outputs, parameters) => {
              const pow = parameters.pow;
              const pow0 = pow[0];
              // if (Math.random() > 0.99) console.log(Array.from(inputs, input => input.length), Array.from(outputs, output => output.length));
              for (var n = 0; n < outputs.length; n++) {
                const output = outputs[n];
                const input = inputs[n] ?? inputs[0];
                if (input && output)
                for (var channel = 0; channel < output.length; channel++) {
                  var ic = input[channel] ?? input[0];
                  var oc = output[channel];
                  if (ic && oc)
                  for (var sample = 0; sample < ic.length; sample++) {
                    oc[sample] = ic[sample] ** (pow[sample] ?? pow0);
                  }
                }
              }
              return !stop;
            };
          }
        """
      , parameters:
        { pow:
          { defaultValue: 1.0
          }
        }
      } ctx >>= \mkNode -> liftEffect $ mkNode
        { numberOfInputs: 1
        , numberOfOutputs: 1
        -- , outputChannelCount: [1]
        , parameterData: defaults
        }
    destroy1 <- liftEffect $ cleanup =<< do applyKnobs $ (node :: AudioWorkletNode ( pow :: ARate ))
    destroy2 <- liftEffect $ cleanup =<< do connecting (toRoars input) (intoNode node 0)
    let destroy = destroy2 <> destroy1 <> destroy0
    liftEffect $ destr.destructor $ destroy2 <> destroy1 <> send unit <> destroy
    pure $ outOfNode node 0
  Y.putYawn { destroy: destr.finalize, ready: void creating }
  output <- Y.gain (affToLake creating <|> whenDestroyed) 1
  pure output

