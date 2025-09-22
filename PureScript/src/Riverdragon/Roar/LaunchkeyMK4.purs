module Riverdragon.Roar.LaunchkeyMK4 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Monad.ResourceM (destr, selfDestructor, track_)
import Control.Monad.ResourceT (run)
import Data.Array as A
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (fold, traverse_)
import Data.HeytingAlgebra (ff)
import Data.Int as Int
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.String as String
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Class (liftEffect)
import Idiolect (intercalateMap)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones ((.$), (.$~~), (=:=), (>@), (~~<))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings as Wings
import Riverdragon.River (Flowing, NotFlowing, Stream, createRiverStore, createStore, dam, mailboxRiver, makeLake, mapArray, mayMemoize, singleShot, stillRiver, unsafeRiver)
import Riverdragon.River as River
import Riverdragon.River.Beyond (KeyPhase(..), affToLake, delay, mkKeyEvent)
import Riverdragon.River.Streamline as S
import Riverdragon.Roar.Dimensions (aWeighting, temperaments)
import Riverdragon.Roar.Knob (toKnob)
import Riverdragon.Roar.Score (ScoreLive, ScoreM, mtf, mtf_)
import Riverdragon.Roar.Score as Y
import Riverdragon.Roar.Synth (installSynth, notesToNoises)
import Riverdragon.Roar.Types (Roar, toRoars)
import Riverdragon.Roar.Viz (oscilloscope, spectrogram)
import Web.Audio.MIDI (MIDIAccess, MIDIInput, MIDIOutput)
import Web.Audio.MIDI as MIDI
import Web.Audio.Types (BiquadFilterType(..), OscillatorType(..))
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Widget (Widget, valueInterface)

_prefix :: forall v. Eq v => Array v -> Prism' (Array v) (Array v)
_prefix vs = prism' (vs <> _) \vs' ->
  if Array.take (Array.length vs) vs' == vs
    then Just (Array.drop (Array.length vs) vs')
    else Nothing

_only :: forall v. Prism' (Array v) v
_only = prism' Array.singleton case _ of
  [v] -> Just v
  _ -> Nothing

matchName :: String -> Boolean
matchName = String.contains (String.Pattern "Launchkey MK4 ")

type Launchkey =
  { midiIn :: MIDIInput
  , dawIn :: MIDIInput
  , midiOut :: MIDIOutput
  , dawOut :: MIDIOutput
  }
data WhichInterface
  = MidiIn
  | DawIn
  | MidiOut
  | DawOut

drumColor :: Int -> Int -> Array Int
drumColor idx clr_idx = [0x90, 0x60+idx + 8*(idx / 8), mod clr_idx 0x7F]

detect :: MIDIAccess -> Aff (Maybe Launchkey)
detect = MIDI.access >>> liftEffect >=> case _ of
  { inputs, outputs }
    | [i1, i2] <- Array.filter (MIDI.getInfo >>> _.name >>> matchName) inputs
    , [o1, o2] <- Array.filter (MIDI.getInfo >>> _.name >>> matchName) outputs
    ->
      pure $ Just { midiIn: i1, dawIn: i2, midiOut: o1, dawOut: o2 }
      -- makeAff \cb -> do
        -- finish <- Bed.postHocDestructors
        -- liftEffect $ finish.destructor =<< do
        --   clearTimeout <$> setTimeout 100 do
        --     finish.finalize
        --     cb (Right Nothing)
        -- pure $ Canceler \_ -> liftEffect finish.finalize
  _ -> pure Nothing

init :: forall flowing.
  Launchkey -> Aff
  { destroy :: Effect Unit
  , input ::
    { dawFocus :: Array Int -> Stream flowing (Array Int)
    , dawStream :: Stream Flowing (Array Int)
    , dawWait :: Array Int -> Stream NotFlowing (Array Int)
    , midiStream :: Stream flowing (Array Int)
    }
  , output ::
    { dawCmd :: Array Int -> Effect Unit
    , midiCmd :: Array Int -> Effect Unit
    }
  , ui :: Dragon
  }
init lk = _.result <$> run do
  traceM lk
  logger <- River.createRiver
  let
    logging which event = logger.send (Tuple which event)
    midiCmd = MIDI.send lk.midiOut <> logging MidiOut
    midiStream = mayMemoize $ unsafeRiver $ makeLake \cb -> track_ do MIDI.onmidimessage lk.midiIn cb
    dawCmd = MIDI.send lk.dawOut <> logging DawOut
    dawStream = mayMemoize $ unsafeRiver $ makeLake \cb -> track_ do MIDI.onmidimessage lk.dawIn cb
    dawFocus = mailboxRiver $ dawStream # mapArray
      \midi -> Array.range 0 (Array.length midi) <#> \l ->
        let r = Array.splitAt l midi in
        { key: r.before, value: r.after }
    dawWait = dawFocus >>> singleShot
  River.subscribe midiStream do logging MidiIn
  River.subscribe dawStream do logging DawIn

  do
    -- Enter DAW Mode
    liftEffect $ dawCmd [0x9F, 0x0C, 0x7F]
    -- Exit DAW Mode
    destr $ dawCmd [0x9F, 0x0C, 0x00]
    -- Relative encoders, pivot around 0x40 = 0
    liftEffect $ dawCmd [0xB6, 0x45, 0x01]
    -- Drum colors
    -- finish.destructor =<< River.subscribe (interval (Milliseconds 1200.0)) \loop ->
    --   for_ (Array.range 0 15) \idx -> do
    --     dawCmd $ drumColor idx (16 * mod loop 8 + idx)

  let
    dir = case _ of
      MidiIn -> "›"
      DawIn -> "»"
      MidiOut -> "‹"
      DawOut -> "«"
    important [0xF0] = false
    important [0xF8] = false
    important _ = true
    importantEvents = filter (important <<< snd) logger.stream
    lastLog = mayMemoize $ stillRiver $ importantEvents <#> \(Tuple whence event) ->
      D.code.$ D.text $ dir whence <> " " <>
        let pad s = if String.length s == 1 then "0" <> s else s in
        intercalateMap " " (pad <<< Int.toStringAs Int.hexadecimal) event
    ui = Egg do
      { stream: cleared, send: clear } <- River.createRiver
      input <- createStore ""
      let
        sendDaw = do
          val <- input.current
          case sequence $ String.split (String.Pattern " ") val <#> Int.fromStringAs Int.hexadecimal of
            Just cmd -> do
              dawCmd cmd
            Nothing -> pure unit
      pure $ fold
        [ D.details.$~~
          [ D.summary.$~~
            [ D.text "Logs"
            , D.text " "
            , D.Replacing lastLog
            , D.text " "
            , D.buttonW "delete" "Clear" (clear unit)
            ]
          , (pure unit <|> cleared) >@ \_ -> D.div[] ~~< lastLog
          ]
        , D.input
            [ D.onInputValue =:= input.send
            , D.placeholder =:= "Hex message"
            , D.on_"keydown" =:= mkKeyEvent >>> traverse_ case _ of
                e@{ phase: KeyDown, key: "Enter" } | e.mod == ff -> sendDaw
                _ -> pure unit
            ]
        , D.buttonW "" "Send" sendDaw
        ]
  destroy <- selfDestructor
  pure
    { destroy
    , input:
      { midiStream, dawStream, dawFocus, dawWait
      }
    , output:
      { midiCmd, dawCmd
      }
    , ui
    }

widget :: Widget
widget _ = pure $ D.Replacing $ map fold $ affToLake $ do
  lkInterface <- MIDI.requestMIDI {} >>= _.access >>> detect
  lk <- maybe mempty init lkInterface
  pure $ Egg do
    lazyDragon <- createRiverStore Nothing
    { send: sendScopeParent, stream: scopeParent } <- createRiverStore Nothing
    synth <- installSynth \noteStream -> do
      { iface } <- ask
      { dragon, voice } <- mkDragonVoice iface

      liftEffect $ iface.temperament.send temperaments.kirnbergerIII
      liftEffect $ iface.pitch.send 441.0

      liftEffect $ lazyDragon.send dragon

      -- This triggers synthesizers and shuts them down when they are released
      activeSynths <- notesToNoises {} noteStream $ const voice

      antialiased <- activeSynths # Y.filter
        { type: Lowpass
        , frequency: 15000.0
        , detune: 0.0
        , "Q": 0.3
        , gain: unit
        }

      scopeEl1 <- oscilloscope { width: 1024, height: 512 } antialiased
      scopeEl2 <- spectrogram { height: 512, width: 400 } antialiased
      River.subscribe scopeParent \el -> do
        Node.appendChild (HTMLCanvasElement.toNode scopeEl1) (Element.toNode el)
        Node.appendChild (HTMLCanvasElement.toNode scopeEl2) (Element.toNode el)
      pure $ toRoars antialiased

    pure $ D.Fragment
      [ synth.playPause
      , synth.midi
      , lk.ui
      , D.div [] $ D.Replacing lazyDragon.stream
      , D.div [ D.Self =:= \el -> mempty <$ sendScopeParent el ] mempty
      ]

mkDragonVoice :: ScoreLive -> ScoreM { dragon :: Dragon, voice :: Int -> Stream Flowing Unit -> ScoreM { value :: Array Roar, leave :: Stream _ Unit } }
mkDragonVoice = \{ pitch, temperament } -> do
  perfectOvertones <- liftEffect do valueInterface false
  let
    -- A gentle organ voice to start off with
    voice semitones release = do
      -- Calculate the frequency from the MIDI note number
      initialFreq <- mtf semitones
      dynamicFreq <- mtf_ semitones
      -- A Knob (control) that represents a simple ADSR envelope
      ramp <- pure $ toKnob
        { adsr:
          { attack: 0.08 -- seconds
          , decay: 0.2 -- seconds
          , sustain: 0.8 -- gain
          , release: 0.08 -- seconds
          }
        -- It starts immediately and releases on the release event
        , release
        }
      let
        -- Scale a number over the range of the instrument
        ranging from to = S.linmap
          (S.Interval 20.0 83.0)
          (S.Interval from to)
          (Int.toNumber semitones)
        -- Build up a sound as a collection of pure sine tones
        waves =
          [ { overtone: 1.0, gain: ranging 0.5 1.0, ramp: ramp }
          , { overtone: 2.005, gain: ranging 1.0 0.5, ramp: ramp }
          , { overtone: 3.004, gain: 0.28, ramp: ramp }
          , { overtone: 2.005 * 2.005, gain: ranging 0.10 0.05, ramp: ramp }
          , { overtone: 5.0, gain: ranging 0.18 0.02, ramp: ramp }
          , { overtone: 3.004 * 2.0, gain: ranging 0.18 0.02, ramp: ramp }
          , { overtone: 7.0, gain: ranging 0.12 0.005, ramp: ramp }
          , { overtone: 8.0, gain: ranging 0.10 0.005, ramp: ramp }
          , { overtone: 9.0, gain: ranging 0.08 0.00, ramp: ramp }
          ]
          -- Correct each frequency for its apparent loudness
          -- (using A-Weighting ... it is not the best)
          <#> \r -> r { gain = r.gain / aWeighting (initialFreq * r.overtone) }
      -- Turn the information above into actual waves
      overtones <- for (A.take 10 waves) \wave -> do
        let
          -- Use the perfectOvertones stream to react to changes
          -- determining whether to use numerically perfect
          -- overtones or slightly distorted ones
          -- (try perfect intervals!)
          waveFreq = ado
            perfect <- perfectOvertones.loopback
            freq <- dynamicFreq
            in freq * if perfect
              then Number.round wave.overtone
              else wave.overtone
        -- Create a sine wave oscillator
        sine <- Y.osc { type: Sine, frequency: waveFreq, detune: 0 }
        -- Set the base volume of the overtone
        scaled <- Y.gain wave.gain sine
        -- Stack the ADSR envelope on top
        shaped <- Y.gain wave.ramp scaled
        -- Return the Roar
        pure (shaped :: Roar)
      -- Gather all of the overtones and quiet them down
      output <- Y.gain (0.2 * ranging 0.35 0.10) overtones
      -- Return the sound, and a leave stream which
      -- lets the synth know that the voice is finished
      pure { value: [ output ], leave: dam (delay (1000.0 # Milliseconds) release) }

    dragon = D.Fragment
      -- Buttons to change parameters
      [ Wings.pushButtonRadio perfectOvertones
        [ true /\ D.text "Perfect overtones"
        , false /\ D.text "Imperfect overtones"
        ]
      , Wings.pushButtonRadio pitch
        [ 415.0 /\ D.text "A415"
        , 440.0 /\ D.text "A440"
        , 441.0 /\ D.text "A441"
        ]
      , Wings.pushButtonRadio temperament
        [ temperaments.equal /\ D.text "Equal Temperament"
        , temperaments.kirnbergerIII /\ D.text "Kirnberger III"
        , temperaments.pythagorean /\ D.text "Pythagorean"
        ]
      ]
  pure { voice, dragon }


