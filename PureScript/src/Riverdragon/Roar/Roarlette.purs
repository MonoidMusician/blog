module Riverdragon.Roar.Roarlette where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.ResourceM (destr, trackA)
import Effect.Class (liftEffect)
import Riverdragon.Roar.Knob (class ToKnob)
import Riverdragon.Roar.Score (ScoreM, roarAsync, roaring, scoreKnobs, scoreStream, waveshape)
import Riverdragon.Roar.Types (class ToLake, class ToRoars, Roar, connecting, toLake, toRoars)
import Web.Audio.Node (intoNode, outOfNode)
import Web.Audio.Types (ARate, KRate, Float, getSampleRate)
import Web.Audio.Worklet (AudioWorkletNode, mkAudioWorkletteNode)

pinkNoise :: ScoreM Roar
pinkNoise = roarAsync do
  { ctx } <- ask
  { node, send } <- trackA "pinkNoise" $ mkAudioWorkletteNode
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
  destr do send unit
  pure $ outOfNode node 0

-- runningAffUntil

pow :: forall knob roar. ToKnob knob => ToRoars roar => knob -> roar -> ScoreM Roar
pow knob input = roarAsync do
  { ctx } <- ask
  { defaults, apply: applyKnobs } <- scoreKnobs { pow: knob }
  { node, send } <- trackA "pow" $ mkAudioWorkletteNode
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
  applyKnobs (node :: AudioWorkletNode ( pow :: ARate ))
  connecting (toRoars input) (intoNode node 0)
  destr $ send unit
  pure $ outOfNode node 0

wavetime :: forall freq. ToKnob freq => freq -> ScoreM Roar
wavetime freqKnob = roarAsync do
  { ctx } <- ask
  { defaults, apply: applyKnobs } <- scoreKnobs { freq: freqKnob }
  { node, send } <- trackA "wavetime" $ mkAudioWorkletteNode
    { name: "WaveTimeGenerator"
    , function:
      """
        function WaveTimeGenerator(options, port) {
          var sampleRate = options.processorOptions.sampleRate;
          var phase = 0.0;
          var stop = false; port.onmessage = (e) => { stop = true; };
          return (_inputs, outputs, parameters) => {
            const freq = parameters.freq[0];
            const output = outputs[0][0];
            for (var sample = 0; sample < output.length; sample++) {
              output[sample] = -1 + 2 * ((phase + (sample * freq) / sampleRate) % 1);
            }
            phase = (phase + (output.length * freq) / sampleRate) % 1;
            return !stop;
          };
        }
      """
    , parameters:
      { freq:
        { defaultValue: 220.0
        }
      }
    } ctx >>= \mkNode -> liftEffect $ mkNode
      { numberOfInputs: 0
      , numberOfOutputs: 1
      , outputChannelCount: [1]
      , parameterData: defaults
      , processorOptions: { sampleRate: getSampleRate ctx }
      }
  applyKnobs (node :: AudioWorkletNode ( freq :: KRate ))
  destr $ send unit
  pure $ outOfNode node 0

wavetable :: forall freq wave. ToKnob freq => ToLake wave (Array Float) => freq -> wave -> ScoreM Roar
wavetable freqKnob currentWaveShape = do
  driver <- wavetime freqKnob
  waving <- scoreStream $ toLake currentWaveShape <#> \waveShape ->
    waveshape waveShape driver
  roaring waving
