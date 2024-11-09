module Riverdragon.River.Beyond where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (hush)
import Data.Filterable (compact)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect, foreachE, whileE)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Now (now)
import Effect.Timer (clearInterval, clearTimeout, setInterval, setTimeout)
import Riverdragon.Dragon.Breath (microtask)
import Riverdragon.River (Allocar, IsFlowing(..), Lake, River, Stream(..), fixPrjBurst, makeLake, makeLake', memoize, oneStream, statefulStream, subscribeIsh, unsafeCopyFlowing, (<?*>), (>>~))
import Riverdragon.River.Bed (breaker, freshId, ordMap, prealloc, pushArray, requestAnimationFrame)

-- | Requires its delay function to be predictable, especially if the result
-- | is used as a river.
delayWith :: forall flow.
  (Effect Unit -> Allocar (Allocar Unit)) ->
  Stream flow ~> Stream flow
delayWith delaying stream =
  unsafeCopyFlowing stream $ makeLake' \selfDestruct cb -> do
    isDestroyed <- prealloc false
    ids <- freshId
    inflight <- ordMap
    let
      upstreamDestroyed = do
        isDestroyed.set true
        -- if it is not destroyed, the last in flight callback will trigger it
        whenM (Map.isEmpty <$> inflight.read) selfDestruct
      receive value = whenM (not <$> isDestroyed.get) do
        id <- ids
        cancelIt <- delaying do
          void $ inflight.remove id
          cb value
          whenM isDestroyed.get do
            whenM (Map.isEmpty <$> inflight.read) do
              selfDestruct
        inflight.set id cancelIt
    unsub <- subscribeIsh upstreamDestroyed stream receive
    pure $ unsub <> inflight.traverse (const identity)

delay :: forall flow. Milliseconds -> Stream flow ~> Stream flow
delay (Milliseconds ms) = delayWith \cb -> do
  clearTimeout <$> setTimeout (Int.round ms) cb

delayMicro :: forall flow. Stream flow ~> Stream flow
delayMicro = delayWith \cb -> do
  -- Microtasks have no canceler, so we have to drop events ourselves
  brk <- breaker $ const cb
  brk.trip <$ microtask (brk.run unit)

delayAnim :: forall flow. Stream flow ~> Stream flow
delayAnim = delayWith requestAnimationFrame

data AFBState
  = Idle
  | Requested (Effect Unit)
  | Draining (Maybe { start :: Instant, timeout :: Milliseconds })
  | Destroyed

mkAnimFrameBuffer :: Allocar
  { buffering :: forall flow. Stream flow ~> Stream flow
  , destroy :: Effect Unit
  , drain :: Allocar Unit
  , setDrainTimeout :: Maybe Milliseconds -> Allocar Unit
  }
mkAnimFrameBuffer = mkBufferedDelayer requestAnimationFrame

mkBufferedDelayer ::
  (Allocar Unit -> Allocar (Allocar Unit)) ->
  Allocar
    { buffering :: forall flow. Stream flow ~> Stream flow
    , drain :: Effect Unit
    , destroy :: Allocar Unit
    , setDrainTimeout :: Maybe Milliseconds -> Allocar Unit
    }
mkBufferedDelayer delayFn = do
  pending <- pushArray
  state <- prealloc Idle
  drained <- prealloc true
  currentDrainTimeout <- prealloc Nothing
  let
    notDestroyed = case _ of
      Destroyed -> false
      _ -> true
    drain :: Effect Unit
    drain = whenM (notDestroyed <$> state.get) do
      timeout <- currentDrainTimeout.get
      case timeout of
        Nothing -> do
          state.set (Draining Nothing)
          whileE (not <$> drained.get) do
            drained.set true
            pending.reset >>= \items -> foreachE items identity
        Just drainTimeout -> do
          drainStart <- now
          state.set (Draining (Just { start: drainStart, timeout: drainTimeout }))
          whileE (not <$> drained.get) do
            drained.set true
            pending.reset >>= \items -> foreachE items \item -> do
              drainTime <- now
              when (Instant.diff drainTime drainStart < drainTimeout) do
                item
      state.set Idle
    animFrameBuffer :: forall flow. Stream flow ~> Stream flow
    animFrameBuffer = delayWith \cb0 -> do
      { trip, run } <- breaker \_ -> cb0
      let cb = run unit
      state.get >>= case _ of
        Idle -> do
          canceler <- delayFn drain
          state.set (Requested canceler)
          drained.set false
          pending.push cb
        Requested _ -> do
          drained.set false
          pending.push cb
        Draining _ -> do
          drained.set false
          pending.push cb
        Destroyed -> pure unit
      pure trip
    r ::
      { buffering :: forall flow. Stream flow ~> Stream flow
      , drain :: Effect Unit
      , destroy :: Allocar Unit
      , setDrainTimeout :: Maybe Milliseconds -> Effect Unit
      }
    r =
      { buffering: animFrameBuffer
      , drain
      , destroy: do
          state.get >>= case _ of
            Requested canceler -> canceler
            _ -> pure unit
          state.set Destroyed
      , setDrainTimeout: currentDrainTimeout.set
      }
  pure r

counter :: forall flow a. Stream flow a -> Lake (a /\ Int)
counter = statefulStream 0 <@> \b a ->
  { state: (b + 1), emit: Just (a /\ b) }

debounce :: forall flow. Milliseconds -> Stream flow ~> Lake
debounce timing input =
  let
    tagged = counter input
    tags = map snd tagged
    delayLine = delay timing tagged
    gateLatest requested (datum /\ arriving) =
      if requested > arriving then Nothing else Just datum
  in compact $ gateLatest <$> tags <?*> delayLine

dedupBy :: forall flow a. (a -> a -> Boolean) -> Stream flow a -> Lake a
dedupBy method = statefulStream Nothing <@> case _, _ of
  Just last, next | method last next ->
    { state: Just next, emit: Nothing }
  _, next ->
    { state: Just next, emit: Just next }

dedupOn :: forall flow a b. Eq b => (a -> b) -> Stream flow a -> Lake a
dedupOn f = statefulStream Nothing <@> case _, _ of
  Just last, next | f next == last ->
    { state: Just (f next), emit: Nothing }
  _, next ->
    { state: Just (f next), emit: Just next }

dedup :: forall flow a. Eq a => Stream flow a -> Lake a
dedup = dedupBy eq

-- dedupStore :: forall flow a. Eq a => River a -> River a
-- dedupStore (Stream _ f) = memoize $ Stream Flowing \cbs -> do


affToLake :: forall a. Aff a -> Lake (Maybe a)
affToLake aff = makeLake \cb -> do
  fiber <- Aff.runAff (cb <<< hush) aff
  pure $ Aff.launchAff_ $ Aff.killFiber (Aff.error "event unsubscribed") fiber

interval :: Milliseconds -> Lake Int
interval (Milliseconds ms) = makeLake \cb -> do
  counted <- freshId
  clearInterval <$> setInterval (Int.floor ms) (cb =<< counted)

animationLoop :: forall state out.
  state -> out ->
  Array (Lake (state -> Lake { state :: state, emit :: Maybe out })) ->
  Lake out
animationLoop s0 out actions =
  fixPrjBurst
    -- initial state, state projection
    (Just s0) (Just <<< _.state)
    -- initial output, output projection
    (Just out) _.emit
    -- process the state stream
    \prevState ->
      -- Keep track of the last state every time an action comes in
      (/\) <$> prevState <?*> oneStream actions
        -- And follow that action, until a new one arrives
        >>~ \(state /\ action) -> action state

