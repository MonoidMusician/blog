module Riverdragon.Roar.Score where

import Prelude

import Control.Monad.MutStateT (MutStateT(..), runMutStateT)
import Control.Monad.Reader (ReaderT(..), asks, runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State (get)
import Data.Array ((!!))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid.Dual (Dual(..))
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.SequenceRecord (sequenceRecord)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, ParAff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prim.Row as Row
import Riverdragon.River (Lake, Stream, alwaysBurst, dam, unsafeRiver, (>>~))
import Riverdragon.River as River
import Riverdragon.River.Bed (cleanup, runningAff)
import Riverdragon.River.Bed as Bed
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

type ScoreM = MutStateT ScoreSt (ReaderT ScoreRW Effect)
type ScoreSt = Record (ScoreS + ())
type ScoreRW = Record (ScoreR + ( written :: Ref (Record (ScoreW + ())) ))

-- Reader state
type ScoreR r =
  ( ctx :: AudioContext
  , iface :: ScoreLive
  | r
  )
-- Writer state
type ScoreW r =
  ( destroy :: Dual (Effect Unit)
  , ready :: ParAff Unit
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
    , waitUntilReady :: Aff Unit
    }
perform options creator = do
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
  written <- Ref.new mempty
  Tuple _state outputs <- runReaderT (runMutStateT state0 (creator ctx)) { ctx, written, iface }
  { destroy: Dual destroy1, ready } <- Ref.read written <* Ref.write mempty written
  destroy2 <- liftEffect $ connecting (dam outputs) (destination ctx)
  destroy <- liftEffect $ cleanup $ destroy1 <> destroy2 <> Context.close ctx
  waitUntilReady <- runningAff $ unit <$ sequential ready
  pure
    { ctx
    , destroy
    , waitUntilReady
    }


type ScoreFn result =
  { ctx :: AudioContext
  , state :: ScoreSt
  , ref :: Ref ScoreSt
  } -> Effect
  { destroy :: Effect Unit
  , ready :: Aff Unit
  , result :: result
  }

scoreElement :: forall result. ScoreFn result -> ScoreM result
scoreElement f = do
  { ctx, written } <- ask
  state <- get
  ref <- MutStateT (ReaderT pure)
  r <- liftEffect $ f { ctx, state, ref }
  isReady <- liftEffect $ Bed.prealloc false
  let
    ready = do
      alreadyRun <- liftEffect $ isReady.get
      when (not alreadyRun) r.ready
      liftEffect $ isReady.set true
  liftEffect $ written # Ref.modify_ \s -> s
    { destroy = s.destroy <> Dual r.destroy
    , ready = s.ready <> parallel ready
    }
  pure r.result

scoreStream :: forall flow1 flow2 x. Stream flow1 (ScoreM x) -> ScoreM (Stream flow2 x)
scoreStream upstream = do
  rollingDestroy <- liftEffect Bed.rolling
  { run } <- scoreScope
  { destroy: destroyStream, stream } <- liftEffect $ River.instantiate $
    upstream >>~ \act -> compact $ affToLake do
      { destroy: newDestroy, waitForReady, result } <- liftEffect $ run act
      liftEffect $ rollingDestroy newDestroy
      result <$ waitForReady
  { written } <- ask
  liftEffect $ written # Ref.modify_ \s -> s <>
    { destroy: Dual $ destroyStream <> rollingDestroy mempty
    , ready: mempty
    }
  pure stream

type RunScore =
  forall x. ScoreM x -> Effect
    { result :: x
    , destroy :: Effect Unit
    , waitForReady :: Aff Unit
    }

-- | `scoreScope` creates a localized runner for `Score`. This is basically
-- | bracketed so you can destroy all of them at once, or individually.
-- | If you need to wait for asynchronous availability (e.g. of worklettes),
-- | you may use `waitForReady`, but this is not necessary.
scoreScope :: ScoreM { run :: RunScore, destroyScoped :: Effect Unit }
scoreScope = MutStateT $ ReaderT \state -> ReaderT \upper@{ ctx } -> do
  destroyScoped <- Bed.accumulator
  upper.written # Ref.modify_ \s -> s <>
    { destroy: Dual $ join destroyScoped.get
    , ready: mempty
    }
  let
    run :: RunScore
    run (MutStateT (ReaderT creator)) = do
      writing <- Ref.new mempty
      r <- runReaderT (creator state) { ctx, written: writing, iface: upper.iface }
      written <- Ref.read writing <* Ref.write mempty writing
      destroy <- liftEffect $ cleanup $ unwrap written.destroy
      liftEffect $ destroyScoped.put destroy
      waitForReady <- runningAff $ unit <$ sequential written.ready
      pure { result: r, destroy, waitForReady }
  pure ({ run, destroyScoped: join destroyScoped.get } :: { run :: RunScore | _ })

putScore ::
  { destroy :: Effect Unit
  , ready :: Aff Unit
  } -> ScoreM Unit
putScore eep = do
  { written } <- ask
  liftEffect $ written # Ref.modify_ \s -> s <>
    { destroy: Dual eep.destroy
    , ready: parallel eep.ready
    }


roars :: Array Roar -> ScoreM Roar
roars = gain 1.0

roaring :: forall flow. Stream flow Roar -> ScoreM Roar
roaring = gain 1.0

roarings :: forall flow. Stream flow (Array Roar) -> ScoreM Roar
roarings = gain 1.0



gain :: forall knob roar. ToKnob knob => ToRoars roar => knob -> roar -> ScoreM Roar
gain knob input = scoreElement \{ ctx } -> liftEffect do
  let { defaults, apply: applyKnobs } = renderKnobs { gain: knob } ctx
  node <- AudioNode.createGainNode ctx defaults
  destroy1 <- applyKnobs node
  destroy2 <- connecting (toRoars input) (intoNode node 0)
  pure { result: outOfNode node 0, destroy: destroy1 <> destroy2, ready: mempty }

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
osc config@{ frequency, detune } = scoreElement \{ ctx } -> liftEffect do
  let { defaults, apply: applyKnobs } = renderKnobs { frequency, detune } ctx
  node <- AudioNode.createOscillatorNode ctx defaults
  destroy1 <- applyKnobs node
  destroy2 <- River.subscribe
    (thingy <$> (toLake config.type :: Lake oscType) <@> ctx)
    (Node.setOscillatorType node)
  pure
    { result: outOfNode node 0
    , destroy: destroy1 <> destroy2 <> Node.stopNow node
    , ready: liftEffect $ Node.startNow node
    }

offset ::
  forall offsetKnob.
    ToKnob offsetKnob =>
  { offset :: offsetKnob
  } -> ScoreM Roar
offset config = scoreElement \{ ctx } -> liftEffect do
  let { defaults, apply: applyKnobs } = renderKnobs config ctx
  node <- AudioNode.createConstantSourceNode ctx defaults
  destroy1 <- applyKnobs node
  pure
    { result: outOfNode node 0
    , destroy: destroy1 <> Node.stopNow node
    , ready: liftEffect $ Node.startNow node
    }

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
filter config input = scoreElement \{ ctx } -> liftEffect do
  let
    { defaults, apply: applyKnobs } = renderKnobs
      { "Q": config."Q"
      , detune: config.detune
      , frequency: config.frequency
      , gain: config.gain
      } ctx
  node <- AudioNode.createBiquadFilterNode ctx defaults
  destroy1 <- applyKnobs node
  destroy2 <- River.subscribe (toLake config.type) $ Audio.setProp node (Proxy :: Proxy "type")
  destroy3 <- connecting (toRoars input) (intoNode node 0)
  pure
    { result: outOfNode node 0
    , destroy: destroy1 <> destroy2 <> destroy3
    , ready: mempty
    }

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
waveshape curve input = scoreElement \{ ctx } -> liftEffect do
  node <- AudioNode.createWaveShaperNode ctx { curve }
  destroy <- connecting (toRoars input) (intoNode node 0)
  pure { result: outOfNode node 0, destroy: destroy, ready: mempty }

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
