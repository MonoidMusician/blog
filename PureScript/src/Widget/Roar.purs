module Widget.Roar where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Array as Array
import Data.Filterable (compact, partition)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Prim.Boolean (False)
import Riverdragon.Dragon.Bones ((=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy)
import Riverdragon.River (Lake, River, allStreams, bursting, createRiver, dam, mailboxRiver, mapAl, withInstantiated)
import Riverdragon.River.Beyond (affToLake, delay, documentEvent, fallingLeaves, fallingLeavesAff, joinLeave, risingFalling)
import Riverdragon.Roar.Sugar (Noise(..), toNode)
import Riverdragon.Roar.Types (Roar, RoarO, toRoars)
import Riverdragon.Roar.Yawn (biiigYawn, osc, yawnytime)
import Riverdragon.Roar.Yawn as Y
import Web.Audio.Node (createPeriodicWave)
import Web.Audio.Node as AudioNode
import Web.Audio.Types (ARate, AudioContext, AudioNode, AudioParamCmd(..), BiquadFilterType(..), Duration, OscillatorType(..), Ramp(..), Time, currentTime)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Widget (Widget)

-- | This picks keys out of a standard QWERTY keyboard layout in a way that
-- | roughly mirrors the alternating black and white keys on a keyboard,
-- | starting an octave below middle C at the bottom left of the keyboard
-- | and ending an octave above middle C at the top right of the keyboard.
keymap :: Array String
keymap =
  -- "zsxdcvgbhnjm,l.;/q2w3e4rt6y7ui9o0p-[]" :: String
  [ "KeyZ"
  , "KeyS"
  , "KeyX"
  , "KeyD"
  , "KeyC"

  , "KeyV"
  , "KeyG"
  , "KeyB"
  , "KeyH"
  , "KeyN"
  , "KeyJ"
  , "KeyM"

  , "Comma"
  , "KeyL"
  , "Period"
  , "Semicolon"
  , "Slash"

  , "KeyQ"
  , "Digit2"
  , "KeyW"
  , "Digit3"
  , "KeyE"
  , "Digit4"
  , "KeyR"

  , "KeyT"
  , "Digit6"
  , "KeyY"
  , "Digit7"
  , "KeyU"

  , "KeyI"
  , "Digit9"
  , "KeyO"
  , "Digit0"
  , "KeyP"
  , "Minus"
  , "BracketLeft"

  , "BracketRight"
  ]

data Interval = Interval Number Number

linmap :: Interval -> Interval -> Number -> Number
linmap (Interval a0 a1) (Interval b0 b1) a = b0 + (a - a0) * (b1 - b0) / (a1 - a0)

pitchGain :: Int -> Number
pitchGain semitones = linmap (Interval 48.0 84.0) (Interval 1.0 0.1) $ Int.toNumber semitones

widgetHarpsynthorg :: Widget
widgetHarpsynthorg _ = pure $ eggy \shell -> do
  { send: setValue, stream: valueSet } <- shell.track createRiver
  { send: sendNote, stream: noteStream } <- shell.track createRiver
  let
    startSynth = void $ biiigYawn {} \ctx -> do
      makeItHappen <- yawnytime
      let
        oneVoice semitones release = do
          let volume = pitchGain semitones
          let pinkEnv = { attack: 0.10, decay: 0.95, sustain: 0.0, release: 0.1 }
          let sineEnv = { attack: 0.05, decay: 0.95, sustain: 0.8, release: 0.3 }
          pink <- toNode PinkNoise
          pinkGain <- Y.gain pink { adsr: pinkEnv, volume: 0.03, release }
          sine <- Y.osc
            { detune: 100 * (semitones - 69)
            , frequency: 440.0
            , type: Custom $ createPeriodicWave ctx false [0.4, 0.5, 0.1] [0.0, 0.0, 0.0]
            }
          sineGain <- Y.gain sine { adsr: sineEnv, volume, release }
          let leave = delay (500.0 # Milliseconds) release
          pure { value: [ pinkGain, sineGain ], leave }
        activeSynths = join <$> fallingLeavesAff noteStream \key release ->
          map _.result $ makeItHappen $ oneVoice key release
      melody <- Y.gain activeSynths 0.3
      antialiased <- Y.filter melody
        { type: Lowpass
        , frequency: 8000.0
        , detune: 0.0
        , "Q": 0.3
        , gain: unit
        }
      pure $ pure [antialiased]
  startSynth

  let
    getNote kb
      | not KeyboardEvent.altKey kb
      , not KeyboardEvent.ctrlKey kb
      , not KeyboardEvent.metaKey kb
      , Just idx <- Array.elemIndex (KeyboardEvent.code kb) keymap = do
        Just $ idx + 48
    getNote _ = Nothing
  shell.destructor =<<
    documentEvent (EventType "keydown") KeyboardEvent.fromEvent case _ of
      kb | Just note <- getNote kb -> do
        Event.preventDefault (KeyboardEvent.toEvent kb)
        sendNote { key: note, value: true }
      _ -> pure unit
  shell.destructor =<<
    documentEvent (EventType "keyup") KeyboardEvent.fromEvent case _ of
      kb | Just note <- getNote kb -> do
        Event.preventDefault (KeyboardEvent.toEvent kb)
        sendNote { key: note, value: false }
      _ -> pure unit

  pure $ D.Fragment
    [ D.button
      [ D.onClick =:= \_ -> setValue unit
      ] $ D.text "Play"
    ]
