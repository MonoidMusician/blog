module Riverdragon.Roar.Synth where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.HeytingAlgebra (ff)
import Data.Maybe (Maybe(..))
import Data.SequenceRecord (class SequenceRecord, sequenceRecord)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones ((=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (Lake, River, Stream, createRiver, createRiverStore, stillRiver, (/?*\))
import Riverdragon.River as River
import Riverdragon.River.Bed (postHocDestructors)
import Riverdragon.River.Bed as Bed
import Riverdragon.River.Beyond (KeyPhase(..), fallingLeaves, keyEvents)
import Riverdragon.Roar.Types (Roar)
import Riverdragon.Roar.Yawn (YawnM, biiigYawn, yawnytime)
import Unsafe.Coerce (unsafeCoerce)
import Web.Audio.Context (LatencyHint(..))
import Web.Audio.Context as Context
import Web.Audio.MIDI as MIDI

notesToNoises ::
  forall envStreams envValues key r flow1 flow2 flow3 roar.
    SequenceRecord envStreams envValues (Stream flow1) =>
    Ord key =>
  Record envStreams ->
  Stream flow1 { key :: key, pressed :: Boolean | r } ->
  ( Record envValues ->
    key ->
    Stream flow2 Unit ->
    YawnM { value :: Array roar, leave :: Stream flow3 Unit }
  ) ->
  YawnM (Lake (Array roar))
notesToNoises envStreams noteStream oneVoice = do
  { run: startAudio } <- yawnytime
  let
    -- Passively listen the the environment streams and take their most recent
    -- values
    notesWithEnv =
      sequenceRecord envStreams /?*\ noteStream
        <#> \(env /\ { key, pressed }) -> { key, pressed, env }
    attackOrRelease { pressed, env } = if pressed then Left env else Right unit

    activeSynths = join <$> fallingLeaves attackOrRelease notesWithEnv \{ key } env release -> do
      -- This manual wiring is a bit ugly ...
      { result, destroy } <- startAudio do
        oneVoice env key (stillRiver release)
      -- Need to `destroy` the node when it leaves
      _ <- liftEffect $ River.subscribe result.leave \_ -> destroy
      -- Return the result
      pure result
  pure activeSynths


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

installSynth ::
  (River { key :: Int, pressed :: Boolean } -> YawnM (Lake (Array Roar))) ->
  Effect
    { destroy :: Effect Unit
    , playPause :: Dragon
    , midi :: Dragon
    , noteStream :: River { key :: Int, pressed :: Boolean }
    }
installSynth synthVoices = do
  shell <- postHocDestructors

  { send: setPlaying, stream: isPlaying } <- shell.track $ createRiverStore Nothing
  { send: sendNote, stream: noteStream } <- shell.track createRiver

  destroyLastSynth <- Bed.rolling
  shell.destructor $ destroyLastSynth mempty
  let
    startSynth = do
      destroyLastSynth mempty
      { destroy: stopSynth } <- biiigYawn { latencyHint: Interactive, sampleRate: 48000 } \_ctx -> do
        synthVoices noteStream
      destroyLastSynth stopSynth

  -- FIXME
  startSynth
  void $ River.subscribe isPlaying if _
    then void startSynth
    else destroyLastSynth mempty

  -- Install inputs (keyboard and MIDI)
  inputs <- shell.track $ synthInputs sendNote

  pure
    { destroy: shell.finalize
    , playPause: D.Fragment
      [ D.button
        [ D.onClick =:= \_ -> setPlaying true
        ] $ D.text "Play"
      , D.button
        [ D.onClick =:= \_ -> setPlaying false
        ] $ D.text "Pause"
      ]
    , midi: D.button
      [ D.onClick =:= \_ -> inputs.grabMIDI
      ] $ D.text "MIDI"
    , noteStream
    }

synthInputs :: forall flow.
  ( { key :: Int
    , pressed :: Boolean
    } -> Effect Unit
  ) -> Effect
  { destroy :: Effect Unit
  , grabMIDI :: Effect Unit
  , midiMessages :: Stream flow (Array Int)
  }
synthInputs sendNote = do
  shell <- postHocDestructors
  let
    -- Transform a `KeyEvent` into a note value
    getNote kb
      | kb.mod == ff
      , Just idx <- Array.elemIndex kb.code keymap = do
        Just $ idx + 48
    getNote _ = Nothing

  -- Listen for keydown and keyup events on `document`
  shell.destructor =<< keyEvents case _ of
    event | Array.elem event.targetType [ "body", "a", "button" ], Just note <- getNote event -> do
      event.preventDefault
      case event.phase of
        KeyDown -> sendNote { key: note, pressed: true }
        KeyRepeat -> pure unit
        KeyUp -> sendNote { key: note, pressed: false }
    _ -> pure unit

  midiAccess <- shell.track $ createRiverStore Nothing
  let
    grabMIDI = launchAff_ do
      Console.log "Requesting"
      { access } <- MIDI.requestMIDI {}
      Console.log $ unsafeCoerce access
      liftEffect $ midiAccess.send access
  launchAff_ $ MIDI.getPermissionStatusMIDI {} >>= case _ of
    MIDI.Granted -> liftEffect grabMIDI
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

  pure
    { destroy: shell.finalize
    , midiMessages: midiMessages.stream
    , grabMIDI
    }

