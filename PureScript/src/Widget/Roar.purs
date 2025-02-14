module Widget.Roar where

import Prelude

import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Filterable (filter)
import Data.HeytingAlgebra (ff)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Riverdragon.Dragon.Bones (smarts, ($~~), (=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy)
import Riverdragon.River (River, createRiver, createRiverStore)
import Riverdragon.River as River
import Riverdragon.River.Bed as Bed
import Riverdragon.River.Beyond (KeyPhase(..), delay, keyEvents)
import Riverdragon.Roar.Dimensions (temperaments)
import Riverdragon.Roar.Knob (Envelope)
import Riverdragon.Roar.Sugar (Noise(..), toNode)
import Riverdragon.Roar.Synth (notesToNoises)
import Riverdragon.Roar.Types (Roar, toRoars)
import Riverdragon.Roar.Viz (oscilloscope, spectrogram)
import Riverdragon.Roar.Yawn (YawnM, biiigYawn, mtf_)
import Riverdragon.Roar.Yawn as Y
import Web.Audio.Types (BiquadFilterType(..))
import Web.DOM.Element (getBoundingClientRect)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.UIEvent.MouseEvent as MouseEvent
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

type Env =
  { pinkEnv :: Envelope
  , sineEnv :: Envelope
  }

oneVoice :: Env -> Int -> River Unit -> YawnM { value :: Array Roar, leave :: River Unit }
oneVoice { pinkEnv, sineEnv } semitones release = do
  -- Console.logShow semitones
  let volume = pitchGain semitones
  pink <- toNode PinkNoise
  pinkGain <- Y.gain pink { adsr: pinkEnv, volume: 0.03, release }
  frequency <- mtf_ semitones -- this live updates if tuning or temperament changes
  sine <- Y.osc
    { detune: 0
    , frequency: frequency
    , type: { sines: [0.4, 0.5, 0.1] }
    }
  sineGain <- Y.gain sine { adsr: sineEnv, volume, release }
  let leave = delay (500.0 # Milliseconds) release
  pure { value: [ sineGain, pinkGain ], leave }

widgetHarpsynthorg :: Widget
widgetHarpsynthorg _ = pure $ eggy \shell -> do
  { send: setValue, stream: valueSet } <- shell.track createRiver
  { send: sendNote, stream: noteStream } <- shell.track createRiver
  { send: sendPinkEnv, stream: pinkEnvStream } <- shell.track $
    createRiverStore $ Just { attack: 0.10, decay: 0.95, sustain: 0.0, release: 0.1 }
  { send: sendSineEnv, stream: sineEnvStream } <- shell.track $
    createRiverStore $ Just { attack: 0.05, decay: 0.95, sustain: 0.8, release: 0.3 }
  { send: sendScopeParent, stream: scopeParent } <- shell.track $ createRiverStore Nothing

  destroyLastSynth <- Bed.rolling
  let
    startSynth = do
      { destroy: stopSynth } <- biiigYawn {} \_ctx -> do
        -- This triggers synthesizers and shuts them down when they are released
        activeSynths <- notesToNoises
          { pinkEnv: pinkEnvStream
          , sineEnv: sineEnvStream
          } noteStream oneVoice

        -- Set the tuning of the instrument
        iface <- asks _.iface
        liftEffect $ iface.temperament.send temperaments.kirnbergerIII
        liftEffect $ iface.pitch.send 441.0

        -- Mix them and reduce their volume
        melody <- Y.gain activeSynths 0.3
        -- Take the edge off slightly ... not a substitute for proper synth design
        antialiased <- Y.filter melody
          { type: Lowpass
          , frequency: 8000.0
          , detune: 0.0
          , "Q": 0.3
          , gain: unit
          }
        scopeEl1 <- oscilloscope { width: 1024, height: 512 } antialiased
        scopeEl2 <- spectrogram { height: 512, width: 400 } antialiased
        void $ liftEffect $ River.subscribe scopeParent \el -> do
          Node.appendChild (HTMLCanvasElement.toNode scopeEl1) (Element.toNode el)
          Node.appendChild (HTMLCanvasElement.toNode scopeEl2) (Element.toNode el)
        pure $ toRoars antialiased
      destroyLastSynth stopSynth

  startSynth
  void $ River.subscribe valueSet if _
    then void startSynth
    else destroyLastSynth mempty

  let
    -- Transform a `KeyEvent` into a note value
    getNote kb
      | kb.mod == ff
      , Just idx <- Array.elemIndex kb.code keymap = do
        Just $ idx + 48
    getNote _ = Nothing

  -- Listen for keydown and keyup events on `document`
  shell.destructor =<< keyEvents case _ of
    event | Just note <- getNote event -> do
      event.preventDefault
      case event.phase of
        KeyDown -> sendNote { key: note, pressed: true }
        KeyRepeat -> pure unit
        KeyUp -> sendNote { key: note, pressed: false }
    _ -> pure unit

  { stream: circlePos, send: sendCirclePos } <- shell.track $ createRiverStore $
    Just { r: 5.0, cx: 25.0, cy: 80.0 }
  svgRef <- Ref.new Nothing

  pure $ D.Fragment
    [ D.button
      [ D.onClick =:= \_ -> setValue true
      ] $ D.text "Play"
    , D.button
      [ D.onClick =:= \_ -> setValue false
      ] $ D.text "Pause"
    , D.div [ D.Self =:= \el -> mempty <$ sendScopeParent el ] mempty
    , D.svg
      [ D.stylish =:= smarts
        { "width": "200px"
        , "height": "100px"
        , "stroke": "currentColor"
        , "stroke-width": "2px"
        , "border": "1px solid currentColor"
        , "fill": "rebeccapurple"
        }
      , D.Self =:= \el -> Ref.write Nothing svgRef <$ Ref.write (Just el) svgRef
      {-
        There are a lot of things wrong with this:
        - It should be a mousemove event on document, installed during mousedown
        - mousedown should also record the initial position as a reference for dragging
        - this does not take into account the viewBox (there is an API for this?)
        - maybe you want escape to cancel a movement, before mouseup
        But the basic idea is sound, and I think client coordinates are the right play here
      -}
      , D.on_"mousemove" =:= \e -> do
          Ref.read svgRef >>= case MouseEvent.fromEvent e, _ of
            Just ev, Just svg -> do
              bb <- getBoundingClientRect svg
              let mx = Int.toNumber (MouseEvent.clientX ev)
              let my = Int.toNumber (MouseEvent.clientY ev)
              let cx = 200.0 * (mx - bb.left) / bb.width
              let cy = 100.0 * (my - bb.top) / bb.height
              -- Console.logShow { bb, cx, cy, mx, my }
              sendCirclePos { r: 5.0, cx, cy }
            _, _ -> pure unit
      ] $~~
      [ D.path [ D.d =:= "M 0 15 L 100 50 L 200 100" ]
      , D.circleW' circlePos [] mempty
      ]
    ]
