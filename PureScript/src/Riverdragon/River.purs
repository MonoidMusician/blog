module Riverdragon.River where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as STRef
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Bifoldable (bifoldMap)
import Data.Filterable (class Compactable, class Filterable, filterDefault, filterMap, partitionDefaultFilterMap, partitionMapDefault)
import Data.Foldable (fold, for_, traverse_)
import Data.HeytingAlgebra (ff, tt)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Set as Set
import Data.These (These(..))
import Data.Traversable (mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Foreign.Object.ST as STO
import Prim.Boolean (False, True)
import Riverdragon.River.Bed (type (-!>), type (-&>), Allocar, accumulator, allocLazy, breaker, cleanup, globalId, loadingBurst, ordMap, prealloc, prealloc2, rolling, storeLast, subscriptions, threshold)

type Id = Int

-- | Transactions, especially for event deduplication (DAG).
-- | Actor model? “I changed” vs “you changed”?
-- | Errors/stream cancelation like RxJS?
data Stream (flow :: Boolean) a = Stream (Conj Boolean)
  -- FIXME: buffer destroyed after burst?
  ( { receive :: a -!> Unit, commit :: Id -!> Unit, destroyed :: Allocar Unit } -&>
    { burst :: Array a, sources :: Array Id, unsubscribe :: Allocar Unit }
  )
-- | A lake is stagnant until you tap into it.
type Lake = Stream False
-- | A river keeps flowing regardless of whether anyone is listening.
type River = Stream True

instance functorStream :: Functor (Stream flow) where
  -- TODO memoize
  map f (Stream t g) = Stream t \cbs -> do
    r <- g cbs { receive = \a -> cbs.receive (f a) }
    pure r { burst = map f r.burst }
-- | The `alt` of two streams forwards all data and commits, and waits until
-- | both are destroyed to destroy downstream.
instance altStream :: Alt (Stream flow) where
  alt (Stream t1 e1) (Stream t2 e2) = Stream (t1 <> t2) \cbs -> do
    -- only run it once both have been destroyed
    destroyed <- threshold 2 cbs.destroyed
    -- and only count each once (just in case)
    cbs1 <- cbs { destroyed = _ } <$> cleanup destroyed
    cbs2 <- cbs { destroyed = _ } <$> cleanup destroyed
    e1 cbs1 <> e2 cbs2
-- | The `empty` stream destroys itself immediately upon subscription.
instance plusStream :: Plus (Stream flow) where
  empty = Stream mempty \cbs -> do
    cbs.destroyed
    pure { burst: empty, sources: mempty, unsubscribe: mempty }
instance applyStream :: Apply (Stream flow) where
  apply = combineStreams (Both tt tt) ($)

combineStreams ::
  forall flow a b c.
  These (a -> Boolean) (b -> Boolean) ->
  (a -> b -> c) ->
  Stream flow a -> Stream flow b -> Stream flow c
combineStreams logic comb (Stream t1 e1) (Stream t2 e2) = Stream (t1 <> t2) \cbs -> do
  -- only run the upstream destroyer once both have been destroyed
  destroyed <- threshold 2 cbs.destroyed
  lastValues <- prealloc2 Nothing Nothing
  needsPush <- prealloc false
  sourcesR <- prealloc Set.empty
  let
    cbL a = do
      _ <- lastValues.setL (Just a)
      _ <- needsPush.set true
      pure unit
    commitL id = do
      l <- lift2 (*>) (guard <$> needsPush.get) (fst <$> lastValues.get)
      Tuple shouldCommit shouldPush <- case logic of
        -- It is always our responsibility to commit
        This p -> pure $ Tuple true $ map p l == Just true
        -- Never commit
        That _ -> pure ff
        -- Only commit if this source is unique to us
        Both p _ -> do
          shouldCommit <- not Set.member id <$> sourcesR.get
          pure $ Tuple shouldCommit $ map p l == Just true
      when shouldCommit do
        commit id shouldPush
    cbR b = do
      _ <- lastValues.setR (Just b)
      _ <- needsPush.set true
      pure unit
    commitR id = do
      r <- lift2 (*>) (guard <$> needsPush.get) (snd <$> lastValues.get)
      Tuple shouldCommit shouldPush <- case logic of
        -- Never commit
        This _ -> pure ff
        -- Always commit
        That p -> pure $ Tuple true $ map p r == Just true
        Both _ p -> pure $ Tuple true $ map p r == Just true
      when shouldCommit do
        commit id shouldPush

    commit id shouldPush = do
      when shouldPush do
        Tuple a b <- lastValues.get
        case lift2 comb a b of
          -- Have not received a value on both sides
          Nothing -> pure unit
          Just c -> cbs.receive c
      cbs.commit id
  -- and only count each destructor once (just in case)
  cbs1 <- cbs { receive = cbL, commit = commitL, destroyed = _ } <$> cleanup destroyed
  cbs2 <- cbs { receive = cbR, commit = commitR, destroyed = _ } <$> cleanup destroyed
  -- subscribe to upstream
  r1 <- e1 cbs1
  r2 <- e2 cbs2
  _ <- sourcesR.set (Set.fromFoldable r2.sources)
  -- initialize from the burst
  _ <- lastValues.setL (Array.last r1.burst)
  _ <- lastValues.setR (Array.last r2.burst)
  pure
    -- TODO: burst logic?
    { burst: lift2 comb r1.burst r2.burst
    , sources: bifoldMap (const r1.sources) (const r2.sources) logic
    , unsubscribe: r1.unsubscribe <> r2.unsubscribe
    }

sampleOnRight :: forall flow a b. Stream flow a -> Stream flow (a -> b) -> Stream flow b
sampleOnRight = combineStreams (That tt) (#)

sampleOnLeft :: forall flow a b. Stream flow a -> Stream flow (a -> b) -> Stream flow b
sampleOnLeft = combineStreams (This tt) (#)

infixl 4 sampleOnRight as <?**>
infixl 4 sampleOnLeft as <**?>

sampleOnRightOp :: forall flow a b. Stream flow (a -> b) -> Stream flow a -> Stream flow b
sampleOnRightOp = combineStreams (That tt) ($)

infixl 4 sampleOnRightOp as <?*>

sampleOnLeftOp :: forall flow a b. Stream flow (a -> b) -> Stream flow a -> Stream flow b
sampleOnLeftOp = combineStreams (This tt) ($)

infixl 4 sampleOnLeftOp as <*?>

applyOp :: forall f a b. Applicative f => f a -> f (a -> b) -> f b
applyOp ea ef = apply ((#) <$> ea) ef

infixl 4 applyOp as <**>

instance applicativeStream :: Applicative (Stream flow) where
  pure a = Stream mempty \{ destroyed } -> do
    destroyed
    pure
      { sources: []
      , burst: [a]
      , unsubscribe: mempty
      }


-- | Create a (hot) stream as a message channel, that sends any message to all of
-- | its subscribers (and then commits).
createStream :: forall flow a. Allocar { send :: a -!> Unit, stream :: Stream flow a, destroy :: Allocar Unit }
createStream = createStreamBurst mempty

createStreamBurst :: forall flow a. Allocar (Array a) -> Allocar { send :: a -!> Unit, stream :: Stream flow a, destroy :: Allocar Unit }
createStreamBurst burst = do
  id <- globalId
  { push, notify, destroy } <- subscriptions
  pure
    { send: \a -> do
        notify do
          \{ receive } -> receive a
        notify do
          \{ commit } -> commit id
    , stream: Stream mempty \cbs -> do
        bursted <- burst
        push cbs <#> { burst: bursted, sources: [id], unsubscribe: _ }
    , destroy: destroy _.destroyed
    }

createStreamStore :: forall flow a. Maybe a -> Allocar { send :: a -!> Unit, stream :: Stream flow a, destroy :: Allocar Unit }
createStreamStore initialValue = do
  lastValue <- storeLast
  case initialValue of
    Just a -> void $ lastValue.set a
    Nothing -> pure unit
  r <- createStreamBurst $ lastValue.get <#> case _ of
    Just a -> [a]
    Nothing -> []
  pure r { send = \a -> lastValue.set a *> r.send a }


createStream' :: forall flow a. Allocar (Array a) -> Allocar { send :: a -!> Unit, commit :: Id -!> Unit, stream :: Array Id -> Stream flow a, destroy :: Allocar Unit }
createStream' burst = do
  { push, notify, destroy } <- subscriptions
  pure
    { send: \a -> do
        notify \{ receive } -> receive a
    , commit: \id -> do
        notify \{ commit } -> commit id
    , stream: \sources -> Stream mempty \cbs -> do
        bursted <- burst
        push cbs <#> { burst: bursted, sources, unsubscribe: _ }
    , destroy: destroy $ _.destroyed
    }

-- | Make a (cold) stream that gets allocated per subscriber, e.g. for listeners
-- | or timeouts or the like.
makeStream :: forall a. ((a -!> Unit) -> Allocar (Allocar Unit)) -> Stream False a
makeStream streamTemplate = Stream (Conj false) \cbs -> do
  id <- globalId
  loadingBurst \whenLoaded -> do
    unsubscribe <- streamTemplate do
      \a -> whenLoaded a do
        cbs.receive a
        cbs.commit id
    pure { burst: _, sources: [id], unsubscribe }


-- | Unsafe.
unsafeMarkHot :: forall flowIn flowOut a. Stream flowIn a -> Stream flowOut a
unsafeMarkHot (Stream _ f) = Stream (Conj true) f

chill :: forall flowIn a. Stream flowIn a -> Stream False a
chill (Stream _ f) = Stream (Conj false) f

-- | Subscribe to a stream with a specific callback. Returns an unsubscribe
-- | procedure (`Allocar Unit`).
subscribe :: forall flow a. Stream flow a -> (a -!> Unit) -> Allocar (Allocar Unit)
subscribe (Stream _ stream) receive = do
  r <- stream { receive, commit: mempty, destroyed: mempty }
  for_ r.burst receive
  pure r.unsubscribe

subscribeIsh :: forall flow a. Allocar Unit -> Stream flow a -> (a -!> Unit) -> Allocar (Allocar Unit)
subscribeIsh destroyed (Stream _ stream) receive = do
  r <- stream { receive, commit: mempty, destroyed }
  for_ r.burst receive
  pure r.unsubscribe

-- | Hmm, ugly but kind of necessary
burstOf :: forall flow a. Stream flow a -> Allocar (Array a)
burstOf (Stream _ stream) = do
  { burst, unsubscribe } <- stream mempty
  burst <$ unsubscribe

-- | Create a single subscriber to an upstream stream that broadcasts it to
-- | multiple downstream subscribers at once. This creation is effectful because
-- | it creates a subscriber, thus starting some effects.
instantiate :: forall flowIn flowOut a. Stream flowIn a -> Allocar { stream :: Stream flowOut a, destroy :: Allocar Unit }
instantiate strm@(Stream _ stream) = do
  { send, commit, stream: streamDependingOn, destroy } <- createStream' (burstOf strm)
  { sources, unsubscribe } <-
    stream { receive: send, commit, destroyed: destroy }
  pure { stream: streamDependingOn sources, destroy: unsubscribe <> destroy }

instantiateStore :: forall flowIn flowOut a. Stream flowIn a -> Allocar { stream :: Stream flowOut a, destroy :: Allocar Unit }
instantiateStore (Stream _ stream) = do
  lastValue <- storeLast
  { send, commit, stream: streamDependingOn, destroy } <- createStream' (Array.fromFoldable <$> lastValue.get)
  { burst, sources, unsubscribe } <-
    stream { receive: send, commit, destroyed: destroy }
  for_ (Array.last burst) lastValue.set
  pure { stream: streamDependingOn sources, destroy: unsubscribe <> destroy }

foldStream :: forall flow a b. b -> Stream flow a -> (b -> a -> b) -> Stream flow b
foldStream b upstream folder = pure b <|> statefulStream b upstream
  \b' a -> join { state: _, emit: _ } $ folder b' a

statefulStream :: forall flow a b c. b -> Stream flow a -> (b -> a -> { state :: b, emit :: c }) -> Stream flow c
statefulStream b0 (Stream t stream) folder = Stream t \cbs -> do
  current <- prealloc b0
  upstream <- stream $ cbs { receive = _ } \a -> do
    { state: b, emit: c } <- folder <$> current.get <@> a
    _ <- current.set b
    cbs.receive c
  let
    folder' b a =
      let r = folder b a in
      { accum: r.state, value: r.emit }
  let { value: burst, accum: b1 } = mapAccumL folder' b0 upstream.burst
  _ <- current.set b1
  pure upstream { burst = burst }

instance compactableStream :: Compactable (Stream flow) where
  compact s = filterMap identity s
  separate s = partitionMapDefault identity s
instance filterableStream :: Filterable (Stream flow) where
  filter f s = filterDefault f s
  partition f s = partitionDefaultFilterMap f s
  partitionMap f s = partitionMapDefault f s
  -- TODO memoize
  filterMap f (Stream flow stream) = Stream flow \cbs -> do
    r <- stream
      { receive: \a -> case f a of
          Nothing -> mempty
          Just b -> do
            cbs.receive b
      -- always pass commit through
      , commit: cbs.commit
      , destroyed: cbs.destroyed
      }
    pure r { burst = filterMap f r.burst }


limitTo :: forall flow a. Int -> Stream flow a -> Stream flow a
limitTo 0 _ = empty
limitTo n (Stream flow setup) = Stream flow \cbs -> do
  setUnsub <- rolling
  receive <- breaker cbs.receive
  commit <- breaker cbs.commit
  let destroyed = receive.trip <> commit.trip <> cbs.destroyed
  incr <- threshold n (setUnsub mempty <> destroyed)
  sub <- setup
    { receive: receive.run
    , commit: const incr <> commit.run
    , destroyed: destroyed
    }
  setUnsub sub.unsubscribe
  pure sub

selfGating :: forall flow a. (a -> Boolean) -> Stream flow a -> Stream flow a
selfGating pred = selfGatingEf \stop -> pure \a -> when (pred a) stop

selfGatingEf :: forall flow a.
  (Effect Unit -> Effect (a -> Effect Unit)) ->
  Stream flow a -> Stream flow a
selfGatingEf logic (Stream flow setup) = Stream flow \cbs -> do
  setUnsub <- rolling
  receive <- breaker cbs.receive
  commit <- breaker cbs.commit
  let destroyed = receive.trip <> commit.trip <> cbs.destroyed
  lastValue <- storeLast
  process <- logic (setUnsub mempty <> destroyed)
  sub <- setup $
      { receive: lastValue.set >>> void
      , commit: const $ lastValue.get >>= traverse_ process
      , destroyed: mempty
      }
    <>
      { receive: receive.run
      , commit: commit.run
      , destroyed: destroyed
      }
  setUnsub sub.unsubscribe
  pure sub



mapEf :: forall a b. (a -> Effect b) -> Stream True a -> Stream True b
mapEf f (Stream t g) = Stream t \cbs -> do
  r <- g cbs { receive = \a -> cbs.receive =<< f a }
  burst <- traverse f r.burst
  pure r { burst = burst }

mapAl :: forall flow a b. (a -> Allocar b) -> Stream flow a -> Stream flow b
mapAl f (Stream t g) = Stream t \cbs -> do
  r <- g cbs { receive = \a -> cbs.receive =<< f a }
  burst <- traverse f r.burst
  pure r { burst = burst }

-- overCb ::
--   forall flow a.
--   ( (Maybe b -> Effect Unit) -&>
--     { receive :: Maybe a -> Effect Unit
--     , unsubscribe :: Allocar Unit
--     }
--   ) ->
--   Stream flow a -> Stream flow b
-- overCb f = mapCb \upstream ->
--   f (maybe upstream.destroyed upstream.receive) >>= \r -> do
--     for_ upstream.burst $ r.receive <<< Just
--     pure
--       { burst: []
--       , receive: r.receive <<< Just
--       , unsubscribe: r.unsubscribe
--       , destroyed:
--       }

mapCb ::
  forall flow a b.
  ( { burst :: Array a
    , receive :: b -> Effect Unit
    , destroyed :: Allocar Unit
    } -&>
    { burst :: Array b
    , receive :: a -> Effect Unit
    , unsubscribe :: Allocar Unit
    , destroyed :: Allocar Unit
    }
  ) ->
  Stream flow a -> Stream flow b
mapCb f (Stream t g) = Stream t \cbs -> do
  lastValue <- storeLast
  receiver <- accumulator
  destroyer <- accumulator
  r <- g cbs
    { receive = void <<< lastValue.set
    , commit = \_ -> lastValue.run \a ->
        join do receiver.read <@> a
    , destroyed = join destroyer.read
    }
  { burst, receive, unsubscribe, destroyed } <- f
    { burst: r.burst, receive: cbs.receive, destroyed: cbs.destroyed }
  receiver.put receive
  destroyer.put destroyed
  pure
    { burst
    , sources: r.sources
    , unsubscribe: unsubscribe <> r.unsubscribe
    }


mapLatest :: forall a b. (a -> Lake b) -> Lake a -> Lake b
mapLatest mkStream source = makeStream \cb -> do
  replace <- rolling
  unsubscribe <- subscribe source \a -> do
    replace mempty
    let stream = mkStream a
    replace =<< subscribe stream cb
  pure $ replace mempty <> unsubscribe

latestStream :: forall a b. Lake a -> (a -> Lake b) -> Lake b
latestStream = flip mapLatest

infixl 1 latestStream as >>~

fix' :: forall flow1 o i. (Stream flow1 i -> Allocar { loop :: Lake i, output :: Lake o, destroy :: Allocar Unit }) -> Lake o
fix' mkLoop = makeStream \cb -> do
  { stream, send, destroy: destroy1 } <- createStream
  { loop, output, destroy: destroy2 } <- mkLoop stream
  unsub1 <- subscribe loop send
  unsub2 <- subscribe output cb
  pure $ unsub2 <> unsub1 <> destroy2 <> destroy1

-- the reason i created allocar tbh
mailbox :: forall flowIn flowOut k v. Ord k =>
  Stream flowIn { key :: k, value :: v } ->
  Allocar { byKey :: k -> Stream flowOut v, destroy :: Allocar Unit }
mailbox (Stream _ upstream) = do
  mailboxes <- ordMap
  needsCommit <- ordMap
  destroyers <- accumulator
  let
    destroyDownstream =
      join destroyers.reset <* mailboxes.reset
  { sources, unsubscribe } <- upstream
    { receive: \{ key, value } -> do
        mailboxes.onKey key \downstream -> do
          _ <- needsCommit.set key downstream.commit
          downstream.send value
    , commit: \id -> do
        prev <- needsCommit.reset
        for_ prev \commit -> do
          commit id
    , destroyed: destroyDownstream
    }
  byKey <- allocLazy $ pure do
    \selected -> do
      downstream <- createStream' mempty -- ??
      destroyers.put downstream.destroy
      _ <- mailboxes.set selected downstream
      pure $ downstream.stream sources
  pure { byKey, destroy: unsubscribe <> destroyDownstream }


-- | An interface to a mutable value, stored in one location.
type Interface a =
  { send :: a -!> Unit
  , receive :: Stream True a
  , loopback :: Stream True a
  , mailbox :: a -> Stream True Unit
  , current :: Effect (Maybe a)
  , destroy :: Allocar Unit
  }
disconnected :: forall a. Interface a
disconnected =
  { send: mempty
  , receive: empty
  , loopback: empty
  , mailbox: const empty
  , current: pure Nothing
  , destroy: mempty
  }

-- Each new subscription gets the latest value immediately.
createBehavioral :: forall a. Ord a => Allocar (Interface a)
createBehavioral = do
  ref <- liftST do STRef.new Nothing
  upstream <- createStream
  -- destroyed by upstream.destroy
  m <- mailbox (map { key: _, value: unit } upstream.stream)
  let
    write = \a -> void $ liftST (STRef.write (Just a) ref)
    downstream = unsafeMarkHot do
      makeStream \k -> do
        liftST (STRef.read ref) >>= traverse_ k
        subscribe upstream.stream k
  pure
    { send: write <> upstream.send
    , receive: downstream
    , loopback: downstream
    , mailbox: m.byKey
    , current: liftST (STRef.read ref)
    , destroy: upstream.destroy
    }

makeKeyedInterface :: forall a. Ord a => Allocar (String -> Interface a)
makeKeyedInterface = allocLazy do
  interfaces <- liftST STO.new
  pure \k -> do
    existing <- liftST (STO.peek k interfaces)
    case existing of
      Just r -> pure r
      Nothing -> do
        io <- createBehavioral
        io <$ liftST (STO.poke k io interfaces)
