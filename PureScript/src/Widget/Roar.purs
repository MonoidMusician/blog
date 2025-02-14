module Widget.Roar where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Prim.Boolean (False)
import Riverdragon.Dragon.Bones ((=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy)
import Riverdragon.River (River, bursting, createRiver, dam, mapAl)
import Riverdragon.River.Beyond (delay, documentEvent, fallingLeaves)
import Riverdragon.Roar.Types (RoarO, createGainNode, createSines, fixed, pinkNoise, roaring)
import Web.Audio.Node as AudioNode
import Web.Audio.Types (ARate, AudioContext, AudioNode, AudioParamCmd(..), Duration, Ramp(..), Time, currentTime)
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

type Envelope
  = { attack :: Duration
    , decay :: Duration
    , sustain :: Number
    , release :: Duration
    }

type GainNode
  = { output :: RoarO
    , node :: AudioNode "GainNode" False () () ( gain :: ARate )
    , destroy :: Effect Unit
    }

applyGain :: AudioContext -> Number -> Envelope -> River Time -> RoarO -> Effect GainNode
applyGain ctx gain e releaseTime output = do
  t0 <- currentTime ctx
  createGainNode
    { ctx
    , input: pure [ output ]
    , gain:
      { default: Just 0.0
      , ctl: const $ pure $ { srcs: empty, cmds: _ } $
        bursting
        [ CmdTarget { ramp: LinRamp, target: gain, time: t0 + e.attack }
        , CmdTarget { ramp: LinRamp, target: gain * e.sustain, time: t0 + e.attack + e.decay }
        ]
        <|> (\t1 -> CmdTarget { ramp: LinRamp, target: 0.0, time: t1 + e.release })
        <$> dam releaseTime
      }
    }

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
    startSynth = void $ roaring {} \ctx -> do
      createPinkNoise <- pinkNoise ctx
      let
        activeSynths = fallingLeaves noteStream \semitones release -> do
          let releaseTime = mapAl (const (currentTime ctx)) release
          let volume = pitchGain semitones
          let pinkEnv = { attack: 0.05, decay: 0.95, sustain: 0.0, release: 0.1 }
          let sineEnv = { attack: 0.05, decay: 0.95, sustain: 0.0, release: 0.3 }
          t0 <- liftEffect $ currentTime ctx
          pink <- createPinkNoise
          gain2 <- liftEffect $ applyGain ctx 0.1 pinkEnv releaseTime pink.output
          sine <- liftEffect $ createSines
            { ctx
            , detune: fixed $ Int.toNumber $ 100 * (semitones - 69)
            , frequency: fixed 440.0
            , sines: [0.4, 0.5, 0.1]
            }
          gain <- liftEffect $ applyGain ctx volume sineEnv releaseTime sine.output
          AudioNode.start sine.node (t0 + 0.0)
          pure
            { value: [ gain.output, gain2.output ]
            , leave: delay (1200.0 # Milliseconds) release
            }
      t0 <- liftEffect $ currentTime ctx
      pure
        { output: unit
        , outputs: join <$> activeSynths
        , start: mempty <* Console.logShow t0
        }
  launchAff_ $ void startSynth

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
