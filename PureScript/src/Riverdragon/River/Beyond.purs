module Riverdragon.River.Beyond where

import Prelude

import Control.Alt ((<|>))
import Data.Either (hush)
import Data.Filterable (compact)
import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Timer (clearInterval, clearTimeout, setInterval, setTimeout)
import Prim.Boolean (False)
import Prim.Row as Row
import Prim.RowList as RL
import Record as R
import Record.Builder as Builder
import Riverdragon.River (Lake, Stream, limitTo, makeStream, mapCb, statefulStream, (<?*>))
import Riverdragon.River.Bed (freshId, ordSet, storeLast)
import Type.Equality (class TypeEquals, proof)
import Type.Proxy (Proxy(..))

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
  Just last, next | method last next ->
    { state: Just next, emit: Nothing }
  _, next ->
    { state: Just next, emit: Just next }

dedupOn :: forall flow a b. Eq b => (a -> b) -> Stream flow a -> Stream flow a
dedupOn f = compose compact $ statefulStream Nothing <@> case _, _ of
  Just last, next | f next == last ->
    { state: Just (f next), emit: Nothing }
  _, next ->
    { state: Just (f next), emit: Just next }

dedup :: forall flow a. Eq a => Stream flow a -> Stream flow a
dedup = dedupBy eq

affToLake :: forall a. Aff a -> Lake (Maybe a)
affToLake aff = makeStream \cb -> do
  fiber <- Aff.runAff (cb <<< hush) aff
  pure $ Aff.launchAff_ $ Aff.killFiber (Aff.error "event unsubscribed") fiber

interval :: Milliseconds -> Lake Int
interval (Milliseconds ms) = makeStream \cb -> do
  counted <- freshId
  clearInterval <$> setInterval (Int.floor ms) (cb =<< counted)


class Functor m <= SequenceRecord (rl :: RL.RowList Type) row from to m | rl row from -> to
  where
    sequenceRecordImpl :: Proxy rl -> Record row -> m (Builder.Builder { | from } { | to })

instance sequenceRecordSingle ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Functor m
  , Row.Lacks name from
  , Row.Cons name ty from to
  ) => SequenceRecord (RL.Cons name (m ty) RL.Nil) row from to m where
  sequenceRecordImpl _ a  =
      Builder.insert namep <$> valA
    where
      namep = Proxy :: _ name
      valA = R.get namep a

else instance sequenceRecordCons ::
  ( IsSymbol name
  , Row.Cons name (m1 ty) trash row
  , Apply m2
  , TypeEquals m1 m2
  , SequenceRecord tail row from from' m2
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) => SequenceRecord (RL.Cons name (m1 ty) tail) row from to m2 where
  sequenceRecordImpl _ a  =
       fn <$> valA <*> rest
    where
      namep = Proxy :: _ name
      valA = to1 (R.get namep a)
      tailp = Proxy :: _ tail
      rest = sequenceRecordImpl tailp a
      fn valA' rest' = Builder.insert namep valA' <<< rest'

instance sequenceRecordNil :: Applicative m => SequenceRecord RL.Nil row from from m where
  sequenceRecordImpl _ _ = pure identity

sequenceRecord :: forall row row' rl m
   . RL.RowToList row rl
  => SequenceRecord rl row () row' m
  => Record row
  -> m (Record row')
sequenceRecord a = Builder.build <@> {} <$> builder
  where
    builder = sequenceRecordImpl (Proxy :: _ rl) a

newtype To1 :: forall k. k -> (k -> Type) -> (k -> Type) -> Type
newtype To1 c a b = To1 (a c -> b c)

to1 :: forall a b c. TypeEquals a b => a c -> b c
to1 = case proof (To1 (\a -> a)) of To1 f -> f
