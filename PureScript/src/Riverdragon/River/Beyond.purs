module Riverdragon.River.Beyond where

import Prelude

import Control.Alt ((<|>))
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
import Effect.Timer (clearTimeout, setTimeout)
import Prim.Boolean (False)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.River (Lake, Stream, limitTo, makeStream, mapCb, statefulStream, (<?*>))
import Riverdragon.River.Bed (ordSet, storeLast)

-- delay :: Milliseconds -> Stream False ~> Stream False
-- delay (Milliseconds ms) stream = makeStream \cb -> do
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
      _ <- inflight.set id
      _ <- idStore.set id
      pure unit
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
--         _ <- inflight.set id
--         _ <- idStore.set id
--         pure unit
--     , unsubscribe: inflight.reset >>= traverse_ (cancelAnimationFrame <@> w)
--     }

vanishing :: Milliseconds -> Stream False Dragon -> Stream False Dragon
vanishing ms stream = stream <#> \element -> Replacing do
  pure element <|> limitTo 1 (delay ms (pure mempty))

counter :: forall flow a. Stream flow a -> Stream flow (a /\ Int)
counter = statefulStream 0 <@> \b a ->
  { state: (b + 1), emit: (a /\ b) }

debounce :: forall flow. Milliseconds -> Stream flow ~> Stream flow
debounce timing input =
  let
    tagged = counter input
    tags = map snd tagged
    delayLine = delay timing tagged
    gateLatest requested (datum /\ arriving) =
      if requested > arriving then Nothing else Just datum
  in compact $ gateLatest <$> tags <?*> delayLine

dedupBy :: forall flow a. (a -> a -> Boolean) -> Stream flow a -> Stream flow a
dedupBy method = compose compact $ statefulStream Nothing <@> case _, _ of
  Just last, next | not method last next -> { state: Just next, emit: Nothing }
  _, next -> { state: Just next, emit: Just next }

dedup :: forall flow a. Eq a => Stream flow a -> Stream flow a
dedup = dedupBy eq

affToLake :: forall a. Aff a -> Lake (Maybe a)
affToLake aff = makeStream \cb -> do
  fiber <- Aff.runAff (cb <<< hush) aff
  pure $ Aff.launchAff_ $ Aff.killFiber (Aff.error "event unsubscribed") fiber
