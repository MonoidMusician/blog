module Widget.Roar where

import Prelude

import Control.Monad.Reader (ask, asks)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Traversable (for, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Random (randomRange)
import Riverdragon.Dragon.Bones ((=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Components.Envelope (envelopeComponent)
import Riverdragon.Dragon.Wings (hatching)
import Riverdragon.River (Lake, River, createRiverStore, mapAl)
import Riverdragon.River as River
import Riverdragon.River.Beyond (delay)
import Riverdragon.River.Streamline (Interval(..), linmap)
import Riverdragon.Roar.Dimensions (aWeighting, loudnessCorrection, temperaments)
import Riverdragon.Roar.Knob (Envelope, Knob(..), knobToAudio, planned, toKnob)
import Riverdragon.Roar.Roarlette as YY
import Riverdragon.Roar.Synth (installSynth, notesToNoises)
import Riverdragon.Roar.Types (Roar, toRoars)
import Riverdragon.Roar.Viz (oscilloscope, spectrogram)
import Riverdragon.Roar.Score (ScoreM, mtf, mtf_)
import Riverdragon.Roar.Score as Y
import Web.Audio.Types (BiquadFilterType(..), OscillatorType(..))
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Widget (Widget)

type Env =
  { pinkEnv :: Envelope
  , sineEnv :: Envelope
  }

pitchGain :: Int -> Number
pitchGain semitones = linmap (Interval 48.0 84.0) (Interval 1.0 0.1) $ Int.toNumber semitones

oneVoice :: Env -> Int -> River Unit -> ScoreM
  { value :: Array
    { audio :: Array Roar
    , gain :: Knob
    }
  , leave :: River Unit
  }
oneVoice { pinkEnv, sineEnv } semitones release = do
  -- Console.logShow semitones
  let volume = pitchGain semitones
  -- pink <- toNode PinkNoise
  -- pinkGain <- Y.gain pink { adsr: pinkEnv, volume: 0.2, release }
  frequency <- mtf_ semitones -- this live updates if tuning or temperament changes
  sine <- Y.osc
    { detune: 0
    , frequency: frequency
    , type: { sines: [0.4, 0.5, 0.1] }
    }
  let gainKnob = toKnob { decay: 0.7, impulse: pure volume, damp: 0.05, dampen: release }
  sineGain <- Y.gain gainKnob sine
  let leave = delay (100.0 # Milliseconds) release
  { ctx: _ctx } <- ask
  pure { value: [{ audio: [ sineGain ], gain: gainKnob }], leave }

harpsynthorgVoiceV1 :: forall env. env -> Int -> River Unit -> ScoreM
  { value :: Array
    { audio :: Array Roar
    , gain :: Knob
    }
  , leave :: River Unit
  }
harpsynthorgVoiceV1 _env semitones release = do
  let
    rangeScaled lo hi = linmap (Interval 20.0 83.0) (Interval lo hi) (Int.toNumber semitones)
    rangeScaled' lo hi = linmap (Interval 36.0 83.0) (Interval lo hi) (Int.toNumber semitones)
    duration = rangeScaled' 6.0 3.5
  { ctx: _ctx } <- ask
  baseRamp /\ destroyRamp <- liftEffect $ knobToAudio _ctx $ toKnob
    { adsr:
      { attack: 0.001
      , decay: duration
      , sustain: 0.0
      , release: 0.1
      }
    , release
    }
  Y.putScore { destroy: destroyRamp, ready: mempty }
  ramp0 <- pure baseRamp -- TODO: `scale~ 8. 8.`
  ramp1 <- YY.pow (8.0) ramp0
  ramp2 <- YY.pow (8.0 * 2.0) ramp0
  ramp3 <- YY.pow (8.0 * 4.5) ramp0
  ramp4 <- YY.pow (8.0 * 4.5 * 4.0) ramp0
  freq <- mtf semitones
  let
    waves =
      [ { freq: freq * 1.0, gain: rangeScaled 0.45 1.0, ramp: ramp1 }
      , { freq: freq * 2.005, gain: rangeScaled 0.5 0.2, ramp: ramp1 }
      , { freq: freq * 3.004, gain: 0.28, ramp: ramp2 }
      , { freq: freq * 2.005 * 2.005, gain: 0.05, ramp: ramp3 }
      , { freq: freq * 5.0, gain: 0.04, ramp: ramp3 }
      ] <#> loudnessCorrection
  overtones <- for waves \wave -> do
    sine <- Y.osc { type: Sine, frequency: wave.freq, detune: 0 }
    scaled <- sine # Y.gain wave.gain
    decaying <- scaled # Y.gain (KStartFrom 0.0 $ toKnob wave.ramp)
    pure decaying
  -- Console.logShow $ waves <#> \{ freq, gain } -> { freq, gain, duration }
  twangLow <- liftEffect $ randomRange 0.991 0.999
  twangHigh <- liftEffect $ randomRange 1.005 1.010
  let
    twang = planned
      (twangLow * freq)
      [ { target: twangHigh * freq, after: 0.1 }
      , { target: 1.0 * freq,       after: 0.08 }
      ]
  square <- do
    square <- Y.pwm { width: 0.379, frequency: twang, detune: 0 }
    scaled <- square # Y.gain (Number.pow (1.0 / aWeighting freq) 0.25)
    decaying <- scaled # Y.gain ramp3
    defanged <- decaying # Y.filter
      { type: Lowpass
      , frequency: freq * 34.0
      , detune: 0.0
      , "Q": 0.1 -- ??
      , gain: unit
      }
    pure defanged
  hammer <- do
    hammer <- YY.pinkNoise
    scaled <- hammer # Y.gain (90.0 * rangeScaled' 0.15 0.04)
    decaying <- scaled # Y.gain ramp4
    defanged <- decaying # Y.filter
      { type: Lowpass
      , frequency: freq
      , detune: 0.0
      , "Q": 0.3 -- ??
      , gain: unit
      }
    pure defanged
  joined <- Y.gain 1.0 [ overtones, [ square, hammer ] ]
  let leave = delay (150.0 # Milliseconds) release
  pure { value: [{ audio: [ joined ], gain: toKnob ramp1 }], leave }

widgetHarpsynthorg :: Widget
widgetHarpsynthorg _ = pure $ hatching \shell -> do
  { ui: pinkEnvUi, stream: pinkEnvStream } <- envelopeComponent shell
    { attack: 0.10, decay: 0.95, sustain: 0.0, release: 0.1 }
  { ui: sineEnvUi, stream: sineEnvStream } <- envelopeComponent shell
    { attack: 0.05, decay: 0.95, sustain: 0.8, release: 0.3 }
  { send: sendScopeParent, stream: scopeParent } <- shell.track $ createRiverStore Nothing

  synth <- shell.track $ installSynth \noteStream -> do
    { ctx: _ctx } <- ask
    -- This triggers synthesizers and shuts them down when they are released
    activeSynths <- notesToNoises
      { pinkEnv: pinkEnvStream
      , sineEnv: sineEnvStream
      } noteStream harpsynthorgVoiceV1
    let
      activeAudio :: Lake (Array Roar)
      activeAudio = join <<< map _.audio <$> activeSynths

    -- Set the tuning of the instrument
    iface <- asks _.iface
    liftEffect $ iface.temperament.send temperaments.kirnbergerIII
    liftEffect $ iface.pitch.send 441.0

    -- Mix them and reduce their volume
    melody <- Y.gain 0.1 activeAudio
    -- Take the edge off slightly ... not a substitute for proper synth design
    antialiased <- melody # Y.filter
      { type: Lowpass
      , frequency: 8000.0
      , detune: 0.0
      , "Q": 0.3
      , gain: unit
      }
    let
      debug :: Lake (Array Roar)
      debug = activeSynths # mapAl do
        traverse \{ gain: k } -> do
          -- Console.log "Knob incoming!"
          liftEffect $ fst <$> knobToAudio _ctx k
    scopeEl1 <- oscilloscope { width: 1024, height: 512 } melody -- debug
    scopeEl2 <- spectrogram { height: 512, width: 400 } antialiased
    void $ liftEffect $ River.subscribe scopeParent \el -> do
      Node.appendChild (HTMLCanvasElement.toNode scopeEl1) (Element.toNode el)
      Node.appendChild (HTMLCanvasElement.toNode scopeEl2) (Element.toNode el)
    pure $ toRoars antialiased

  pure $ D.Fragment
    [ synth.playPause
    , synth.midi
    , D.div [ D.Self =:= \el -> mempty <$ sendScopeParent el ] mempty
    -- , pinkEnvUi
    -- , sineEnvUi
    ]
