module Riverdragon.Roar.Yawn where

import Prelude

import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell)
import Data.Either (either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Dual (Dual(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, ParAff, launchAff, launchAff_, parallel, sequential)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Riverdragon.River (Stream, Allocar, createRiver, dam)
import Riverdragon.River as River
import Riverdragon.River.Bed (cleanup)
import Riverdragon.River.Bed as Bed
import Riverdragon.Roar.Knob (class ToKnob, Knob(..), renderKnobs)
import Riverdragon.Roar.Types (class ToLake, class ToRoars, Roar, RoarO, connecting, toLake, toRoars)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Uncurried.RWSET (RWSET, runRWSET)
import Web.Audio.Context (createAudioContext)
import Web.Audio.Node (destination, intoNode, outOfNode)
import Web.Audio.Node as AudioNode
import Web.Audio.Node as Node
import Web.Audio.Types (AudioContext, BiquadFilterType, OscillatorType(..))
import Web.Audio.Types as Audio

type YawnM =
  RWSET
    { ctx :: AudioContext
    , state :: Ref
      { id :: Int
      , worklets :: Map String (Aff Unit) -- unused at the moment
      }
    }
    { destroy :: Dual (Effect Unit)
    , ready :: ParAff Unit
    }
    Unit
    Void
    Aff

type YawnFn result =
  { ctx :: AudioContext
  -- TODO: caching of knobs?
  , state :: Ref
    { id :: Int
    , worklets :: Map String (Aff Unit)
    }
  } -> Aff
  { destroy :: Effect Unit
  , ready :: Aff Unit
  , result :: result
  }

biiigYawn :: forall flow.
  {} ->
  (AudioContext -> YawnM (Stream flow (Array RoarO))) ->
  Effect
    { ctx :: AudioContext
    , destroy :: Effect Unit
    , waitUntilReady :: Aff (Stream flow (Array RoarO))
    }
biiigYawn options creator = do
  ctx <- createAudioContext options
  state <- Ref.new { id: 0, worklets: Map.empty }
  currentDestroy <- Bed.prealloc mempty
  -- TODO: bracket
  fiber <- launchAff do
    _ /\ orStream /\ written <- runRWSET { ctx, state } unit (creator ctx)
    let outputs = either absurd identity orStream
    let { destroy: Dual destroy1, ready } = written
    destroy2 <- liftEffect $ connecting (dam outputs) (destination ctx)
    destroy <- liftEffect $ cleanup $ destroy1 <> destroy2
    sequential ready
    liftEffect $ currentDestroy.set destroy
    pure outputs
  currentDestroy.set $ launchAff_ $ Aff.killFiber (Aff.error "Canceled Yawn") fiber
  pure
    { ctx
    , destroy: join currentDestroy.get
    , waitUntilReady: Aff.joinFiber fiber
    }


yaaawn :: forall result. YawnFn result -> YawnM result
yaaawn f = do
  ctx <- ask
  { result, destroy, ready } <- lift $ f ctx
  tell { destroy: Dual destroy, ready: parallel ready }
  pure result

-- yawnOnDemand :: forall flow1 flow2 x. Stream flow1 (YawnM x) -> YawnM (Stream flow2 x)
-- yawnOnDemand upstream = yaaawn \{ ctx, state } -> liftEffect do
--   { send, stream, destroy: destroy1 } <- createRiver
--   pendingReady <- Bed.prealloc (Just (mempty :: ParAff Unit))
--   let
--     whenReady :: Aff Unit -> Aff Unit
--     whenReady act =
--       liftEffect pendingReady.get >>= case _ of
--         Just pending -> liftEffect do
--           pendingReady.set (Just (pending <> parallel act))
--         Nothing -> act
--   rollingDestroy <- Bed.rolling
--   destroy2 <- River.subscribe upstream \creator -> launchAff_ do
--     _ /\ orStream /\ written <- runRWSET { ctx, state } unit creator
--     liftEffect $ rollingDestroy $ coerce written.destroy
--     whenReady do
--       sequential written.ready
--       liftEffect $ send $ either absurd identity orStream
--   let destroy = destroy1 <> destroy2 <> rollingDestroy mempty
--   pure { destroy, ready: mempty, result: stream }

yawnytime :: YawnM (forall x. YawnM x -> Aff { result :: x, destroy :: Allocar Unit })
yawnytime = yaaawn \{ ctx, state } -> liftEffect do
  destroyAll <- Bed.accumulator
  let
    result :: forall x. YawnM x -> Aff { result :: x, destroy :: Allocar Unit }
    result creator = do
      _ /\ orStream /\ written <- runRWSET { ctx, state } unit creator
      destroy <- liftEffect $ cleanup (unwrap written.destroy)
      liftEffect $ destroyAll.put destroy
      sequential written.ready
      pure { result: either absurd identity orStream, destroy }
  pure
    { destroy: join destroyAll.get
    , ready: mempty
    , result: result :: forall x. YawnM x -> Aff { result :: x, destroy :: Allocar Unit }
    }


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
  forall flowOscillatorType frequencyKnob detuneKnob.
    ToLake flowOscillatorType OscillatorType =>
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
  destroy2 <- River.subscribe (toLake config.type) $ Node.setOscillatorType node
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
filter input config@{ detune, frequency, gain } = yaaawn \{ ctx } -> liftEffect do
  let
    { defaults, apply: applyKnobs } = renderKnobs
      { "Q": config."Q"
      , detune
      , frequency
      , gain
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

-- freqs :: forall flow. Stream flow (Array Knob) -> YawnM Roar
-- freqs = roarings <=< yawnOnDemand <<< map (traverse \frequency -> osc { type: Sine, frequency, detune: 0 })
