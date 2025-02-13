module Riverdragon.Roar.Yawn where

import Prelude

import Control.Monad.MutStateT (MutStateT(..), runMutStateT)
import Control.Monad.Reader (ReaderT(..), asks, runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State (get)
import Data.Array ((!!))
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
import Effect.Aff (Aff, ParAff, launchAff, parallel, sequential)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Riverdragon.River (Lake, Stream, alwaysBurst, dam, unsafeRiver, (>>~))
import Riverdragon.River as River
import Riverdragon.River.Bed (cleanup)
import Riverdragon.River.Bed as Bed
import Riverdragon.River.Beyond (affToLake)
import Riverdragon.Roar.Dimensions (temperaments)
import Riverdragon.Roar.Knob (class ToKnob, Knob(..), renderKnobs)
import Riverdragon.Roar.Types (class ToLake, class ToOther, class ToRoars, Roar, RoarO, connecting, thingy, toLake, toRoars)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Web.Audio.Context (createAudioContext)
import Web.Audio.Node (destination, intoNode, outOfNode)
import Web.Audio.Node as AudioNode
import Web.Audio.Node as Node
import Web.Audio.Types (AudioContext, BiquadFilterType, Frequency, OscillatorType(..))
import Web.Audio.Types as Audio
import Widget (Interface, storeInterface)

type YawnM = MutStateT YawnSt (ReaderT YawnRW Effect)
type YawnSt = Record (YawnS + ())
type YawnRW = Record (YawnR + ( written :: Ref (Record (YawnW + ())) ))

-- Reader state
type YawnR r =
  ( ctx :: AudioContext
  , iface :: YawnLive
  | r
  )
-- Writer state
type YawnW r =
  ( destroy :: Dual (Effect Unit)
  , ready :: ParAff Unit
  | r
  )
-- Mutable state
-- TODO: caching of knobs?
type YawnS r =
  ( id :: Int
  , worklets :: Map String (Aff Unit) -- unused at the moment
  | r
  )
-- Live variables that notify when they change
type YawnLive =
  { temperament :: Interface (Array Number)
  , pitch :: Interface Frequency
  }

biiigYawn :: forall flow.
  {} ->
  (AudioContext -> YawnM (Stream flow (Array RoarO))) ->
  Effect
    { ctx :: AudioContext
    , destroy :: Effect Unit
    , waitUntilReady :: Aff Unit
    }
biiigYawn options creator = do
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
  { destroy: Dual destroy1, ready } <- Ref.read written
  destroy2 <- liftEffect $ connecting (dam outputs) (destination ctx)
  destroy <- liftEffect $ cleanup $ destroy1 <> destroy2
  fiber <- launchAff $ unit <$ sequential ready
  pure
    { ctx
    , destroy
    , waitUntilReady: Aff.joinFiber fiber
    }


type YawnFn result =
  { ctx :: AudioContext
  , state :: YawnSt
  , ref :: Ref YawnSt
  } -> Effect
  { destroy :: Effect Unit
  , ready :: Aff Unit
  , result :: result
  }

yaaawn :: forall result. YawnFn result -> YawnM result
yaaawn f = do
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

yawnOnDemand :: forall flow1 flow2 x. Stream flow1 (YawnM x) -> YawnM (Stream flow2 x)
yawnOnDemand upstream = do
  rollingDestroy <- liftEffect Bed.rolling
  { run }  <- yawnytime
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

type RunYawn =
  forall x. YawnM x -> Effect
    { result :: x
    , destroy :: Effect Unit
    , waitForReady :: Aff Unit
    }

-- | `yawnytime` creates a localized runner for `Yawn`. This is basically
-- | bracketed so you can destroy all of them at once, or individually.
-- | If you need to wait for asynchronous availability (e.g. of worklettes),
-- | you may use `waitForReady`, but this is not necessary.
yawnytime :: YawnM { run :: RunYawn, destroyAll :: Effect Unit }
yawnytime = MutStateT $ ReaderT \state -> ReaderT \upper@{ ctx } -> do
  destroyAll <- Bed.accumulator
  upper.written # Ref.modify_ \s -> s <>
    { destroy: Dual $ join destroyAll.get
    , ready: mempty
    }
  let
    run :: RunYawn
    run (MutStateT (ReaderT creator)) = do
      writing <- Ref.new mempty
      r <- runReaderT (creator state) { ctx, written: writing, iface: upper.iface }
      written <- Ref.read writing
      destroy <- liftEffect $ cleanup $ unwrap written.destroy
      liftEffect $ destroyAll.put destroy
      fiber <- launchAff $ sequential written.ready
      pure { result: r, destroy, waitForReady: Aff.joinFiber fiber }
  pure ({ run, destroyAll: join destroyAll.get } :: { run :: RunYawn | _ })


roars :: Array Roar -> YawnM Roar
roars = gain <@> KConst 1.0

roaring :: forall flow. Stream flow Roar -> YawnM Roar
roaring = gain <@> KConst 1.0

roarings :: forall flow. Stream flow (Array Roar) -> YawnM Roar
roarings = gain <@> KConst 1.0



gain :: forall roar knob. ToRoars roar => ToKnob knob => roar -> knob -> YawnM Roar
gain input knob = yaaawn \{ ctx } -> liftEffect do
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
  } -> YawnM Roar
osc config@{ frequency, detune } = yaaawn \{ ctx } -> liftEffect do
  let { defaults, apply: applyKnobs } = renderKnobs { frequency, detune } ctx
  node <- AudioNode.createOscillatorNode ctx defaults
  destroy1 <- applyKnobs node
  destroy2 <- River.subscribe
    (thingy <$> (toLake config.type :: Lake oscType) <@> ctx)
    (Node.setOscillatorType node)
  pure
    { result: outOfNode node 0
    , destroy: destroy1 <> destroy2
    , ready: liftEffect $ Node.startNow node
    }

filter ::
  forall roar flowBiquadFilterType knobQ knobDetune knobFrequency knobGain.
    ToRoars roar =>
    ToLake flowBiquadFilterType BiquadFilterType =>
    ToKnob knobQ =>
    ToKnob knobDetune =>
    ToKnob knobFrequency =>
    ToKnob knobGain =>
  roar ->
  { type :: flowBiquadFilterType
  , "Q" :: knobQ
  , detune :: knobDetune
  , frequency :: knobFrequency
  , gain :: knobGain
  } -> YawnM Roar
filter input config = yaaawn \{ ctx } -> liftEffect do
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

mtf :: Int -> YawnM Frequency
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

mtf_ :: forall flow flowInt. ToLake flowInt Int => flowInt -> YawnM (Stream flow Number)
mtf_ notes = do
  { temperament, pitch } <- asks _.iface
  yawnOnDemand ado
    alwaysBurst (unsafeRiver temperament.loopback)
    alwaysBurst (unsafeRiver pitch.loopback)
    note <- toLake notes
    in mtf note

freqs :: forall flow knob. ToKnob knob => Stream flow (Array knob) -> YawnM Roar
freqs = roarings <=< yawnOnDemand <<< map (traverse \frequency -> osc { type: Sine, frequency, detune: 0 })
