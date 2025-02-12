module Widget.Roar where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Riverdragon.Dragon.Bones ((=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy)
import Riverdragon.River (bursting, createRiver, dam, mapAl)
import Riverdragon.River.Beyond (delay, documentEvent, fallingLeaves)
import Riverdragon.Roar.Types (createGainNode, createSines, fixed, pinkNoise, roaring)
import Web.Audio.Node as AudioNode
import Web.Audio.Types (AudioParamCmd(..), Ramp(..), currentTime)
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
          let range = (Int.toNumber semitones - 48.0) / 36.0
          let acrossRange low high = low + range * (high - low)
          let adjustedVolume = acrossRange 1.0 0.1
          t0 <- liftEffect $ currentTime ctx
          pink <- createPinkNoise
          gain2 <- liftEffect $ createGainNode
            { ctx
            , input: pure [ pink.output ]
            , gain:
              { default: Just 0.0
              , ctl: const $ pure $ { srcs: empty, cmds: _ } $
                  bursting
                    [ CmdTarget { ramp: LinRamp, target: 0.1, time: t0 + 0.05 }
                    , CmdTarget { ramp: LinRamp, target: 0.0, time: t0 + 1.0 }
                    ]
                  <|> (\t1 -> CmdTarget { ramp: LinRamp, target: 0.0, time: t1 + 0.1 })
                    <$> dam releaseTime
              }
            }
          sine <- liftEffect $ createSines
            { ctx
            , detune: fixed $ Int.toNumber $ 100 * (semitones - 69)
            , frequency: fixed 440.0
            , sines: [0.4, 0.5, 0.1]
            }
          gain <- liftEffect $ createGainNode
            { ctx
            , input: pure [ sine.output ]
            , gain:
              { default: Just 0.0
              , ctl: const $ pure $ { srcs: empty, cmds: _ } $
                  bursting
                    [ CmdTarget { ramp: LinRamp, target: 1.0 * adjustedVolume, time: t0 + 0.05 }
                    , CmdTarget { ramp: LinRamp, target: 0.0, time: t0 + 1.0 }
                    ]
                  <|> (\t1 -> CmdTarget { ramp: LinRamp, target: 0.0, time: t1 + 0.3 })
                    <$> dam releaseTime
              }
            }
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
