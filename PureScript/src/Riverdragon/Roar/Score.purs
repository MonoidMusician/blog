module Riverdragon.Roar.Score where

import Prelude

import Control.Monad.MutStateT (MutStateT, mutStateT, runMutStateT, unMutStateT)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.ResourceM (destr, liftResourceM, selfScope, waitr, whenReady)
import Control.Monad.ResourceT (ResourceT, Scope, oneSubScopeAtATime, scopedRun, scopedStart_, start)
import Control.Monad.State (get)
import Data.Array ((!!))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Number as Number
import Data.SequenceRecord (sequenceRecord)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Prim.Row as Row
import Riverdragon.River (Lake, Stream, alwaysBurst, dam, unsafeRiver, (>>~))
import Riverdragon.River as River
import Riverdragon.River.Beyond (affToLake)
import Riverdragon.Roar.Dimensions (temperaments)
import Riverdragon.Roar.Knob (class ToKnob, renderKnobs)
import Riverdragon.Roar.Types (class ToLake, class ToOther, class ToRoars, Roar, RoarO, connecting, thingy, toLake, toRoars)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Web.Audio.Context (LatencyHint, createAudioContext)
import Web.Audio.Context as Context
import Web.Audio.FFI (class FFI)
import Web.Audio.Node (destination, intoNode, outOfNode)
import Web.Audio.Node as AudioNode
import Web.Audio.Node as Node
import Web.Audio.Types (AudioContext, BiquadFilterType, Float, Frequency, OscillatorType(..))
import Web.Audio.Types as Audio
import Widget (Interface, storeInterface)

type ScoreM = MutStateT ScoreSt (ReaderT ScoreRd (ResourceT Effect))
type ScoreSt = Record (ScoreS + ())
type ScoreRd = Record (ScoreR + ())

-- Reader state
type ScoreR r =
  ( ctx :: AudioContext
  , iface :: ScoreLive
  | r
  )
-- Mutable state
-- TODO: caching of knobs?
type ScoreS r =
  ( id :: Int
  , worklets :: Map String (Aff Unit) -- unused at the moment
  | r
  )
-- Live variables that notify when they change
type ScoreLive =
  { temperament :: Interface (Array Number)
  , pitch :: Interface Frequency
  }

perform ::
  forall options unused ffi flow.
    Row.Union options unused
      ( latencyHint :: LatencyHint
      , sampleRate :: Int
      ) =>
    FFI (Record options) ffi =>
  Record options ->
  (AudioContext -> ScoreM (Stream flow (Array RoarO))) ->
  Effect
    { ctx :: AudioContext
    , destroy :: Effect Unit
    , wait :: Fiber Unit
    , scope :: Scope
    }
perform options creator = perform' options (creator >>> map { result: unit, audio: _ })
  <#> \{ ctx, destroy, wait, scope } -> { ctx, destroy, wait, scope }

perform' ::
  forall options unused ffi flow result.
    Row.Union options unused
      ( latencyHint :: LatencyHint
      , sampleRate :: Int
      ) =>
    FFI (Record options) ffi =>
  Record options ->
  (AudioContext -> ScoreM { audio :: Stream flow (Array RoarO), result :: result }) ->
  Effect
    { result :: result
    , ctx :: AudioContext
    , destroy :: Effect Unit
    , wait :: Fiber Unit
    , scope :: Scope
    }
perform' options creator = do
  ctx <- createAudioContext options
  temperament <- storeInterface
  temperament.send temperaments.equal
  pitch <- storeInterface
  pitch.send 440.0
  let
    iface = { temperament, pitch }
    state0 =
      { id: 0
      , worklets: Map.empty
      }
  r <- start do
    Tuple _state { audio: outputs, result } <- runReaderT (runMutStateT state0 (creator ctx)) { ctx, iface }
    connecting (dam outputs) (destination ctx)
    destr do Context.close ctx
    pure result
  pure
    { result: r.result
    , ctx
    , destroy: r.destroy
    , wait: r.wait
    , scope: r.scope
    }

scoreScope :: ScoreM
  { run :: ScoreM ~> Effect
  , scope :: Scope
  }
scoreScope = do
  st <- mutStateT pure
  ctx <- ask
  scope <- selfScope
  let
    run :: ScoreM ~> Effect
    run act = map _.result $ scopedStart_ scope $ flip runReaderT ctx $ flip unMutStateT st $ act
  pure { run: run :: ScoreM ~> Effect, scope }

scoreStream :: forall flow1 flow2 x. Stream flow1 (ScoreM x) -> ScoreM (Stream flow2 x)
scoreStream upstream = do
  st <- mutStateT pure
  ctx <- ask
  revolving <- selfScope >>= oneSubScopeAtATime
  let
    runHere act = _.result <$> do
      newScope <- liftEffect revolving
      scopedRun newScope $ liftResourceM $
        flip runReaderT ctx $
          flip unMutStateT st $ act
  { stream } <- River.instantiate $
    upstream >>~ \act -> compact $ affToLake do
      runHere act
  pure stream



roars :: Array Roar -> ScoreM Roar
roars = gain 1.0

roaring :: forall flow. Stream flow Roar -> ScoreM Roar
roaring = gain 1.0

roarings :: forall flow. Stream flow (Array Roar) -> ScoreM Roar
roarings = gain 1.0



gain :: forall knob roar. ToKnob knob => ToRoars roar => knob -> roar -> ScoreM Roar
gain knob input = do
  { ctx } <- ask
  let { defaults, apply: applyKnobs } = renderKnobs { gain: knob } ctx
  node <- liftEffect do AudioNode.createGainNode ctx defaults
  liftResourceM do applyKnobs node
  connecting (toRoars input) (intoNode node 0)
  pure (outOfNode node 0)

osc ::
  forall flowOscillatorType oscType frequencyKnob detuneKnob.
    ToLake flowOscillatorType oscType =>
    ToOther oscType (AudioContext -> OscillatorType) =>
    ToKnob frequencyKnob =>
    ToKnob detuneKnob =>
  { type :: flowOscillatorType
  , frequency :: frequencyKnob
  , detune :: detuneKnob
  } -> ScoreM Roar
osc config@{ frequency, detune } = do
  { ctx } <- ask
  let { defaults, apply: applyKnobs } = renderKnobs { frequency, detune } ctx
  node <- liftEffect do AudioNode.createOscillatorNode ctx defaults
  liftResourceM $ applyKnobs node
  River.subscribe
    (thingy <$> (toLake config.type :: Lake oscType) <@> ctx)
    (Node.setOscillatorType node)
  whenReady do Node.startNow node
  destr do Node.stopNow node
  pure (outOfNode node 0)

offset ::
  forall offsetKnob.
    ToKnob offsetKnob =>
  { offset :: offsetKnob
  } -> ScoreM Roar
offset config = do
  { ctx } <- ask
  let { defaults, apply: applyKnobs } = renderKnobs config ctx
  node <- liftEffect do AudioNode.createConstantSourceNode ctx defaults
  liftResourceM $ applyKnobs node
  whenReady do Node.startNow node
  destr do Node.stopNow node
  pure (outOfNode node 0)

filter ::
  forall flowBiquadFilterType knobQ knobDetune knobFrequency knobGain roar.
    ToLake flowBiquadFilterType BiquadFilterType =>
    ToKnob knobQ =>
    ToKnob knobDetune =>
    ToKnob knobFrequency =>
    ToKnob knobGain =>
    ToRoars roar =>
  { type :: flowBiquadFilterType
  , "Q" :: knobQ
  , detune :: knobDetune
  , frequency :: knobFrequency
  , gain :: knobGain
  } ->
  roar -> ScoreM Roar
filter config input = do
  { ctx } <- ask
  let
    { defaults, apply: applyKnobs } = renderKnobs
      { "Q": config."Q"
      , detune: config.detune
      , frequency: config.frequency
      , gain: config.gain
      } ctx
  node <- liftEffect do AudioNode.createBiquadFilterNode ctx defaults
  liftResourceM do applyKnobs node
  River.subscribe (toLake config.type) $ Audio.setProp node (Proxy :: Proxy "type")
  connecting (toRoars input) (intoNode node 0)
  pure (outOfNode node 0)

-- | Convert a MIDI note number to a pitch frequency, based upon the currently
-- | set temperament (e.g. equal temperament or Kirnberger III) and pitch
-- | reference (e.g. 440.0 Hz)
mtf :: Int -> ScoreM Frequency
mtf note = do
  { temperament, pitch: reference } <- ask >>= \{ iface } -> liftEffect do
    sequenceRecord { temperament: iface.temperament.current, pitch: iface.pitch.current }
  let
    semitones = (note - 69)
    temper = fromMaybe 0.0 $ fromMaybe temperaments.equal temperament !! (note `mod` 12)
    cents = Int.toNumber (100 * semitones) + temper
    octaves = cents / 1200.0
    freq = fromMaybe 440.0 reference * Number.pow 2.0 octaves
  pure freq

mtf_ :: forall flow flowInt. ToLake flowInt Int => flowInt -> ScoreM (Stream flow Frequency)
mtf_ notes = do
  { temperament, pitch } <- asks _.iface
  scoreStream ado
    alwaysBurst (unsafeRiver temperament.loopback)
    alwaysBurst (unsafeRiver pitch.loopback)
    note <- toLake notes
    in mtf note

freqs :: forall flow knob. ToKnob knob => Stream flow (Array knob) -> ScoreM Roar
freqs = roarings <=< scoreStream <<< map (traverse \frequency -> osc { type: Sine, frequency, detune: 0 })

waveshape :: forall roar. ToRoars roar => Array Float -> roar -> ScoreM Roar
waveshape curve input = do
  { ctx } <- ask
  node <- liftEffect do AudioNode.createWaveShaperNode ctx { curve }
  connecting (toRoars input) (intoNode node 0)
  pure (outOfNode node 0)

wavesample :: Int -> (Float -> Float) -> Array Float
wavesample nsamples shaping =
  let maxsample = Int.toNumber nsamples - 1.0 in
  Array.replicate nsamples unit
    # Array.mapWithIndex \i _ -> shaping $
        (Int.toNumber i / maxsample) * 2.0 - 1.0

binarize :: forall roar. ToRoars roar => roar -> ScoreM Roar
binarize = waveshape $ wavesample 1024 \v -> case compare v 0.0 of
  LT -> -1.0
  EQ -> 0.0
  GT -> 1.0

pwm ::
  forall width frequency detune.
    ToKnob width =>
    ToKnob frequency =>
    ToKnob detune =>
  { width :: width
  , frequency :: frequency
  , detune :: detune
  } -> ScoreM Roar
pwm { width, frequency, detune } = do
  timing <- osc { type: Sawtooth, frequency, detune }
  undefault <- offset { offset: -0.5 }
  widthOffset <- offset { offset: width }
  binarize [ timing, undefault, widthOffset ]
