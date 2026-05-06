module Riverdragon.Roar.Synth where

import Prelude

import Control.Monad.ResourceM (class MonadResource, inSubScope, selfDestruct, track_)
import Control.Plus (empty)
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
import Riverdragon.River (Lake, River, Stream, createRiver, createRiverStore, mailboxRiver, stillRiver, store, whileJust, (/?*\))
import Riverdragon.River as River
import Riverdragon.River.Beyond (KeyPhase(..), fallingLeaves, keyEvents)
import Riverdragon.Roar.Score (ScoreM, performM, scoreScope)
import Riverdragon.Roar.Types (Roar)
import Unsafe.Coerce (unsafeCoerce)
import Web.Audio.Context (LatencyHint(..))
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
    ScoreM { value :: Array roar, leave :: Stream flow3 Unit }
  ) ->
  ScoreM (Lake (Array roar))
notesToNoises envStreams noteStream oneVoice = do
  { run: startAudio } <- scoreScope
  let
    -- Passively listen the the environment streams and take their most recent
    -- values
    notesWithEnv =
      sequenceRecord envStreams /?*\ noteStream
        <#> \(env /\ { key, pressed }) -> { key, pressed, env }
    attackOrRelease { pressed, env } = if pressed then Left env else Right unit

    activeSynths = join <$> fallingLeaves attackOrRelease notesWithEnv
      \{ key } env release -> startAudio do
        inSubScope "noteToNoise" do
          result <- oneVoice env key (stillRiver release)
          -- Need to `destroy` the node when it leaves
          River.subscribeM result.leave \_ -> selfDestruct
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

installSynth :: forall m. MonadResource m =>
  (River { key :: Int, pressed :: Boolean, velocity :: Maybe Int, aftertouch :: River Int } -> ScoreM (Lake (Array Roar))) ->
  m
    { playPause :: Dragon
    , midi :: Dragon
    , noteStream :: River { key :: Int, pressed :: Boolean, velocity :: Maybe Int, aftertouch :: River Int }
    }
installSynth synthVoices = do
  { send: setPlaying, stream: isPlaying } <- createRiverStore (Just true)
  { send: sendNote, stream: noteStream } <- createRiver

  -- (Re-)run `startSynth` when `true`, otherwise destroy the last one
  River.reenableM1 isPlaying do
    void $ performM { latencyHint: Interactive, sampleRate: 48000 } do
      synthVoices noteStream

  -- Install inputs (keyboard and MIDI)
  inputs <- synthInputs sendNote

  pure
    { playPause: D.Fragment
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

synthInputs :: forall flow m. MonadResource m =>
  ( { key :: Int
    , pressed :: Boolean
    , velocity :: Maybe Int
    , aftertouch :: River Int
    } -> Effect Unit
  ) -> m
  { grabMIDI :: Effect Unit
  , midiMessages :: Stream flow (Array Int)
  }
synthInputs sendNote = do
  let
    -- Transform a `KeyEvent` into a note value
    getNote kb
      | kb.mod == ff
      , Just idx <- Array.elemIndex kb.code keymap = do
        Just $ idx + 48
    getNote _ = Nothing

  -- Listen for keydown and keyup events on `document`
  keyEvents case _ of
    event | Array.elem event.targetType [ "body", "a", "button" ], Just note <- getNote event -> do
      event.preventDefault
      case event.phase of
        KeyDown -> sendNote { key: note, pressed: true, velocity: Nothing, aftertouch: empty }
        KeyRepeat -> pure unit
        KeyUp -> sendNote { key: note, pressed: false, velocity: Nothing, aftertouch: empty }
    _ -> pure unit

  -- Handle requesting MIDI access and receiving events from a connected device
  midiAccess <- createRiverStore Nothing
  let
    grabMIDI = launchAff_ do
      Console.log "Requesting"
      { access } <- MIDI.requestMIDI {}
      Console.log $ unsafeCoerce access
      liftEffect $ midiAccess.send access
  -- TODO: track
  liftEffect $ launchAff_ $ MIDI.getPermissionStatusMIDI {} >>= case _ of
    MIDI.Granted -> liftEffect grabMIDI
    _ -> pure unit
  midiMessages <- createRiver
  River.subscribeM midiAccess.stream \access -> do
    { inputs } <- liftEffect do MIDI.access access
    for_ inputs \input -> do
      Console.log $ "Input " <> (MIDI.getInfo input).name
      track_ do MIDI.onmidimessage input midiMessages.send

  -- Track aftertouch with a mailbox, so each message is addressed to
  -- the right stream
  { send: sendAftertouch, stream: aftertouchBundle } <- createRiver
  let aftertouchFor = mailboxRiver aftertouchBundle
  -- Parse each message
  River.subscribeM midiMessages.stream case _ of
    [0x90, note, velocity] -> do
      -- Track aftertouch values until the note is released
      { stream: aftertouch } <- store $ whileJust $ aftertouchFor note
      let event = { key: note, pressed: velocity > 0, velocity: if velocity > 0 then Just velocity else Nothing, aftertouch }
      liftEffect do sendNote event
    [0x80, note, velocity] -> do
      -- Release aftertouch and then the note
      liftEffect do sendAftertouch { key: note, value: Nothing }
      liftEffect do sendNote { key: note, pressed: false, velocity: if velocity > 0 then Just velocity else Nothing, aftertouch: empty }
    [0xA0, note, pressure] -> do
      liftEffect do sendAftertouch { key: note, value: Just pressure }
    _ -> pure unit

  pure
    { midiMessages: midiMessages.stream
    , grabMIDI
    }

