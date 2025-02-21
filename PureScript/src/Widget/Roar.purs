module Widget.Roar where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ask, asks)
import Data.Array as Array
import Data.Foldable (fold, for_, traverse_)
import Data.HeytingAlgebra (ff)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Random (randomRange)
import Effect.Ref as Ref
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones (smarts, ($~~), (<:>), (=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (Shell, eggy)
import Riverdragon.River (Allocar, Lake, River, Stream, createRiver, createRiverStore, mapAl, oneStream, (/?*\))
import Riverdragon.River as River
import Riverdragon.River.Bed as Bed
import Riverdragon.River.Beyond (KeyPhase(..), delay, documentEvent, keyEvents)
import Riverdragon.Roar.Dimensions (aWeighting, loudnessCorrection, temperaments)
import Riverdragon.Roar.Knob (Envelope, Knob(..), adsr, audioToKnob, knobToAudio, planned, toKnob)
import Riverdragon.Roar.Roarlette as YY
import Riverdragon.Roar.Synth (notesToNoises)
import Riverdragon.Roar.Types (Roar, toRoars)
import Riverdragon.Roar.Viz (oscilloscope, spectrogram)
import Riverdragon.Roar.Yawn (YawnM, biiigYawn, mtf, mtf_)
import Riverdragon.Roar.Yawn as Y
import Unsafe.Coerce (unsafeCoerce)
import Web.Audio.Context (LatencyHint(..))
import Web.Audio.MIDI (PermissionStatus(..))
import Web.Audio.MIDI as MIDI
import Web.Audio.Types (BiquadFilterType(..), OscillatorType(..))
import Web.DOM.Element (Element, getBoundingClientRect)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (EventType(..))
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

type Pt d = { x :: d, y :: d }
data Interval = Interval Number Number

linmap :: Interval -> Interval -> Number -> Number
linmap (Interval a0 a1) (Interval b0 b1) a = b0 + (a - a0) * (b1 - b0) / (a1 - a0)

linmapClamp :: Interval -> Interval -> Number -> Number
linmapClamp from to pt = clamp to (linmap from to pt)

clamp :: Interval -> Number -> Number
clamp (Interval lower upper) pt = min (max lower upper) (max (min upper lower) pt)

clamp2D :: Pt Interval -> Pt Number -> Pt Number
clamp2D bounds pt = { x: clamp bounds.x pt.x, y: clamp bounds.y pt.y }

linmap2D :: Pt Interval -> Pt Interval -> Pt Number -> Pt Number
linmap2D from to pt =
  { x: linmap from.x to.x pt.x
  , y: linmap from.y to.y pt.y
  }

pitchGain :: Int -> Number
pitchGain semitones = linmap (Interval 48.0 84.0) (Interval 1.0 0.1) $ Int.toNumber semitones

random :: Interval -> Allocar Number
random (Interval lo hi) = randomRange lo hi

type Env =
  { pinkEnv :: Envelope
  , sineEnv :: Envelope
  -- , wibbleWobble :: Lake Number
  }

bbInterval ::
  Element ->
  Effect
    { x :: Interval
    , y :: Interval
    }
bbInterval svg = do
  bb <- getBoundingClientRect svg
  pure
    { x: Interval bb.left bb.right
    , y: Interval bb.top bb.bottom
    }

envelopeComponent ::
  Shell -> Envelope ->
  Allocar
    { ui :: Dragon
    , stream :: River Envelope
    }
envelopeComponent shell init = do
  { send, stream } <- shell.track $ createRiverStore $ Just init
  svgRef <- Ref.new Nothing

  let
    width = 200.0
    height = 100.0
    padding = 40.0
    pudding = 2.0 * padding
    external = { x: Interval 0.0 (width + pudding), y: Interval 0.0 (height + pudding) }
    graphExternal = { x: Interval padding (width + padding), y: Interval (height + padding) padding }

  { send: mouseDown, stream: dragging } <- shell.track $ createRiverStore
    (Nothing :: Maybe (Pt Number -> Effect Unit))
  shell.destructor =<< River.subscribe (stream /?*\ dragging) \(Tuple _current selected) -> do
    unsubs <- Bed.accumulator
    unsub1 <- documentEvent (EventType "mousemove") MouseEvent.fromEvent \event -> do
      Ref.read svgRef >>= traverse_ \svg -> do
        bb <- bbInterval svg
        let
          ptExternal = linmap2D bb external $ clamp2D bb
            { x: Int.toNumber (MouseEvent.clientX event)
            , y: Int.toNumber (MouseEvent.clientY event)
            }
        selected ptExternal
    unsubs.put unsub1
    unsub2 <- documentEvent (EventType "mouseup") Just \_ -> do
      -- TODO: confirm or cancel? stuff like that
      join unsubs.get
    -- TODO: listen for escape key to cancel?
    unsubs.put unsub2
    pure unit

  let
    ui = D.svg
      [ D.stylish =:= smarts
        { "width": show (width + pudding) <> "px"
        , "height": show (height + pudding) <> "px"
        , "stroke": "currentColor"
        , "stroke-width": "2px"
        , "border": "1px solid currentColor"
        , "fill": "none"
        }
      , D.Self =:= \el -> Ref.write Nothing svgRef <$ Ref.write (Just el) svgRef
      ] $~~
      [ D.pathW' $ stream <#> \current ->
          let
            adr = current.attack + current.decay + current.release
            sustain = adr
            totalTime = adr + sustain
            graphInternal = { x: Interval 0.0 totalTime, y: Interval 0.0 1.0 }
            display = linmap2D graphInternal graphExternal
            pt { time, volume } =
              let
                displayPt = display { x: time, y: volume }
              in show displayPt.x <> " " <> show displayPt.y
          in fold
            -- M is for Move: set the cursor
            [ "M" <> pt { time: 0.0, volume: 0.0 }
            -- L is for Line: line to the next point
            , "L" <> pt { time: current.attack, volume: 1.0 }
            , "L" <> pt { time: current.attack + current.decay, volume: current.sustain }
            , "L" <> pt { time: totalTime - current.release, volume: current.sustain }
            , "L" <> pt { time: totalTime, volume: 0.0 }
            ]
      , D.g
        [ D.stylish =:= smarts
            { "font-family": "inherit"
            , "font-size": "30px"
            , "fill": "currentColor"
            , "stroke": "none"
            }
        ] $~~
        [ D.svg_"text"
            [ D.attr "x" =:= 100.0
            , D.attr "y" =:= 170.0
            ] $ D.text "Time ->"
        , D.svg_"text"
            [ D.attr "transform" =:= "rotate(-90)"
            , D.attr "transform-origin" =:= "center"
            , D.attr "x" =:= 80.0
            , D.attr "y" =:= -20.0
            ] $ D.text "Volume ->"
        ]
      , D.g [] $~~
        [ D.circleW'
          ( stream <#> \current -> do
              let
                adr = current.attack + current.decay + current.release
                sustain = adr
                totalTime = adr + sustain
                graphInternal = { x: Interval 0.0 totalTime, y: Interval 0.0 1.0 }
                display = linmap2D graphInternal graphExternal
                pt { time, volume } =
                  let
                    displayPt = display { x: time, y: volume }
                  in { cx: displayPt.x, cy: displayPt.y, r: 5.0 }
              pt { time: current.attack + current.decay, volume: current.sustain }
          ) []
        , D.circleW'
          ( stream <#> \current -> do
              let
                adr = current.attack + current.decay + current.release
                sustain = adr
                totalTime = adr + sustain
                graphInternal = { x: Interval 0.0 totalTime, y: Interval 0.0 1.0 }
                display = linmap2D graphInternal graphExternal
                pt { time, volume } =
                  let
                    displayPt = display { x: time, y: volume }
                  in { cx: displayPt.x, cy: displayPt.y, r: 10.0 }
              pt { time: current.attack + current.decay, volume: current.sustain }
          )
          [ D.stylish =:= smarts
            { "fill": "transparent"
            , "stroke": "transparent"
            }
          , D.on_"mousedown" <:> stream <#> \current _mousedown -> do
              -- TODO: graph initial coordinates? maybe?
              mouseDown \ptExternal -> do
                let
                  adr = current.attack + current.decay + current.release
                  totalTime = adr + adr
                  graphInternal = { x: Interval 0.0 totalTime, y: Interval 0.0 1.0 }
                  -- decay = linmap graphExternal.x graphInternal.x ptExternal.x
                  sustain = linmapClamp graphExternal.y graphInternal.y ptExternal.y
                  new = current { sustain = sustain }
                Console.logShow new
                send new
          ]
        ]
      ]
  pure { ui, stream }

oneVoice :: Env -> Int -> River Unit -> YawnM
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
    , frequency: frequency -- + map (52.0 * _) wibbleWobble
    , type: { sines: [0.4, 0.5, 0.1] }
    }
  let gainKnob = toKnob { decay: 0.7, impulse: pure volume, damp: 0.05, dampen: release }
  sineGain <- Y.gain sine gainKnob
  let leave = delay (100.0 # Milliseconds) release
  { ctx: _ctx } <- ask
  pure { value: [{ audio: [ sineGain ], gain: gainKnob }], leave }

harpsynthorgVoiceV1 :: forall env. env -> Int -> River Unit -> YawnM
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
      { attack: 0.0002
      , decay: duration
      , sustain: 0.0
      , release: 0.1
      }
    , release
    }
  ramp0 <- pure baseRamp -- TODO: `scale~ 8. 8.`
  ramp1 <- YY.pow ramp0 $ 8.0
  ramp2 <- YY.pow ramp0 $ 8.0 * 2.0
  ramp3 <- YY.pow ramp0 $ 8.0 * 4.5
  ramp4 <- YY.pow ramp0 $ 8.0 * 4.5 * 4.0
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
    scaled <- Y.gain sine 0.5 -- wave.gain
    decaying <- Y.gain scaled (KStartFrom 0.0 $ toKnob wave.ramp)
    pure decaying
  -- Console.logShow $ waves <#> \{ freq, gain } -> { freq, gain, duration }
  twangLow <- liftEffect $ randomRange 0.991 0.999
  twangHigh <- liftEffect $ randomRange 1.005 1.015
  let
    twang = planned
      (twangLow * freq)
      [ { target: twangHigh * freq, after: 0.1 }
      , { target: 1.0 * freq,       after: 0.08 }
      ]
  square <- do
    square <- Y.pwm { width: 0.379, frequency: twang, detune: 0 }
    scaled <- Y.gain square $ Number.pow (1.0 / aWeighting freq) 0.25
    decaying <- Y.gain scaled ramp3
    defanged <- Y.filter decaying
      { type: Lowpass
      , frequency: freq * 34.0
      , detune: 0.0
      , "Q": 0.1 -- ??
      , gain: unit
      }
    pure defanged
  hammer <- do
    hammer <- YY.pinkNoise
    scaled <- Y.gain hammer $ 90.0 * rangeScaled' 0.15 0.04
    decaying <- Y.gain scaled ramp4
    defanged <- Y.filter decaying
      { type: Lowpass
      , frequency: freq
      , detune: 0.0
      , "Q": 0.3 -- ??
      , gain: unit
      }
    pure defanged
  joined <- Y.gain [ overtones, [ square, hammer ] ] 1.0
  let leave = delay (150.0 # Milliseconds) release
  pure { value: [{ audio: [ joined ], gain: toKnob ramp1 }], leave }

widgetHarpsynthorg :: Widget
widgetHarpsynthorg _ = pure $ eggy \shell -> do
  { send: setValue, stream: valueSet } <- shell.track createRiver
  { send: sendNote, stream: noteStream } <- shell.track createRiver
  { ui: pinkEnvUi, stream: pinkEnvStream } <- envelopeComponent shell
    { attack: 0.10, decay: 0.95, sustain: 0.0, release: 0.1 }
  { ui: sineEnvUi, stream: sineEnvStream } <- envelopeComponent shell
    { attack: 0.05, decay: 0.95, sustain: 0.8, release: 0.3 }
  { send: sendScopeParent, stream: scopeParent } <- shell.track $ createRiverStore Nothing

  destroyLastSynth <- Bed.rolling
  let
    startSynth = do
      { destroy: stopSynth } <- biiigYawn { latencyHint: Interactive, sampleRate: 48000 } \_ctx -> do
        -- This triggers synthesizers and shuts them down when they are released
        activeSynths <- notesToNoises
          { pinkEnv: pinkEnvStream
          , sineEnv: sineEnvStream
          -- , wibbleWobble: pure $ pinkEnvStream <#> _.sustain
          } noteStream harpsynthorgVoiceV1
        let
          activeAudio :: Lake (Array Roar)
          activeAudio = join <<< map _.audio <$> activeSynths

        -- Set the tuning of the instrument
        iface <- asks _.iface
        liftEffect $ iface.temperament.send temperaments.kirnbergerIII
        liftEffect $ iface.pitch.send 441.0

        -- Mix them and reduce their volume
        melody <- Y.gain activeAudio 0.1
        -- Take the edge off slightly ... not a substitute for proper synth design
        antialiased <- Y.filter melody
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

  Console.log "TEST"

  midiAccess <- shell.track $ createRiverStore Nothing
  let
    grabMIDI = launchAff_ do
      Console.log "Requesting"
      { access } <- MIDI.requestMIDI {}
      Console.log $ unsafeCoerce access
      liftEffect $ midiAccess.send access
  launchAff_ $ MIDI.getPermissionStatusMIDI {} >>= case _ of
    Granted -> liftEffect grabMIDI
    _ -> pure unit
  midiMessages <- shell.track createRiver
  shell.destructor =<< River.subscribe midiAccess.stream \access -> do
    { inputs } <- MIDI.access access
    for_ inputs \input -> do
      Console.log $ "Input " <> (MIDI.getInfo input).name
      shell.destructor =<< MIDI.onmidimessage input midiMessages.send
  shell.destructor =<< River.subscribe midiMessages.stream case _ of
    [0x90, note, velocity] -> sendNote { key: note, pressed: velocity > 0 }
    [0x80, note, _velocity] -> sendNote { key: note, pressed: false }
    _ -> pure unit

  pure $ D.Fragment
    [ D.button
      [ D.onClick =:= \_ -> setValue true
      ] $ D.text "Play"
    , D.button
      [ D.onClick =:= \_ -> setValue false
      ] $ D.text "Pause"
    , D.button
      [ D.onClick =:= \_ -> grabMIDI
      ] $ D.text "MIDI"
    , D.div [ D.Self =:= \el -> mempty <$ sendScopeParent el ] mempty
    -- , pinkEnvUi
    -- , sineEnvUi
    ]
