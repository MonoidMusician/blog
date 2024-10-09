module Riverdragon.River.Beyond where

import Prelude

import Data.Either (hush)
import Data.Filterable (compact)
import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Timer (clearInterval, clearTimeout, setInterval, setTimeout)
import Riverdragon.River (Lake, Stream, fixPrjBurst, makeLake, mapCb, oneStream, statefulStream, (<?*>), (>>~))
import Riverdragon.River.Bed (freshId, ordSet, storeLast)

-- delay :: Milliseconds -> Stream False ~> Stream False
-- delay (Milliseconds ms) stream = makeLake \cb -> do
--   subscribe stream do void <<< setTimeout (Int.round ms) <<< cb


delay :: forall flow. Milliseconds -> Stream flow ~> Stream flow
delay (Milliseconds ms) = mapCb \upstream -> do
  -- TODO: arena
  inflight <- ordSet
  let
    receive value = do
      idStore <- storeLast
      id <- setTimeout (Int.round ms) do
        idStore.get >>= traverse_ inflight.remove
        upstream.receive value
      inflight.set id
      idStore.set id
  for_ upstream.burst receive
  pure
    { receive: receive
    , unsubscribe: mempty $ inflight.reset >>= traverse_ clearTimeout
    , burst: []
    , destroyed: void do
        -- We do not care about canceling this
        setTimeout (Int.round ms) upstream.destroyed
    }

-- delayMicro :: forall flow. Stream flow ~> Stream flow
-- delayMicro = overCb \cb -> do
--   -- Microtasks have no canceler, so we have to drop events ourselves
--   brk <- breaker cb
--   pure
--     { receive: microtask <<< brk.run
--     , unsubscribe: brk.trip
--     }

-- delayAnim :: forall flow. Stream flow ~> Stream flow
-- delayAnim = overCb \cb -> do
--   -- TODO: arena
--   inflight <- ordSet
--   w <- window
--   pure
--     { receive: \value -> do
--         idStore <- storeLast
--         id <- flip requestAnimationFrame w do
--           idStore.get >>= traverse_ inflight.remove
--           cb value
--         inflight.set id
--         idStore.set id
--     , unsubscribe: inflight.reset >>= traverse_ (cancelAnimationFrame <@> w)
--     }

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

