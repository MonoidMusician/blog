-- | A fresh new library for FRP (Functional Reactive Programming).
-- |
-- | Iʼm trying to hit the sweet spot in complexity: just enough to enable nice
-- | features, but not a whole runtime or kitchen sink. Here are my design
-- | notes:
-- |
-- | - Transactions? Values can be pushed out through the network and then
-- |   committed when this is done. This allows intermediate states from
-- |   partially-updated nodes to be dropped, reducing the burden on coding
-- |   defensive logic to work around complex DAGs of event streams, especially
-- |   when those events are coming from disparate or abstract places, and
-- |   potentially also increasing performance by reducing downstream updates.
-- | - Stream destruction, the converse of unsubscribe, so that upstream can
-- |   inform downstream that it will no longer send events. (This is kind of
-- |   like errors in RxJS maybe, but without taking another type parameter.)
-- | - Burst: special handling for sending events during subscription. This is
-- |   formalized both because it ended up being necessary (you cannot really
-- |   get subscription logic right while sending events during subscription:
-- |   what if one of them triggers an unsubscription, and what about `<*>`) and
-- |   because making it explicit means we can make more interesting choices
-- |   around it (see below).
-- |
-- | Finally, there are a bit more distinctions to be made about the behavior
-- | of event streams:
-- |
-- | - `Lake`s are the default type of stream, where resources may be allocated
-- |   per subscriber and may function independently of each other. This is
-- |   necessary for subscriptions like timers and intervals, in order to keep
-- |   them functional: since they represent **descriptions of how to set up a
-- |   stream for a subscriber**, as opposed to a running stream. Otherwise it
-- |   would violate referential transparency/the substitution principle.
-- | - `River`s represent running streams: their contract is that they must
-- |   broadcast the same value to each subscriber. (Possibly ordered.) This
-- |   is necessary for allowing transformations of the stream (things like
-- |   `map`, `filterMap`, and so on) to be memoized and shared between
-- |   subscribers.
-- |
-- | `River`s may be turned into `Lake`s, but not vice-versa.
-- |
-- | Mostly orthogonal to this distinction is the fact that streams may have
-- | various burst behaviors. For `River`s it often makes sense to send a
-- | “latest” value (e.g. from a `Ref`) to each new subscriber that joins, or
-- | even the complete history. Thus the bursts are a mechanism that works
-- | alongside the subscription streams to achieve a cohesive working system,
-- | but is largely independent and simply adjoined to it as applicatives can
-- | be combined with the product of functors.
-- |
-- | Do note that a stream should take no observable effects when a subscriber
-- | joins or leaves. It is merely setting up further effects to happen, by
-- | calling the subscribed callback. (Of course it may need to allocate things
-- | and manage state internally). (This is what the distinction between
-- | `Effect` and `Allocar` was supposed to accomplish, but it was not worth
-- | encoding in a `newtype`, particularly as it remains nebulous.)
-- |
-- | Note that `flow` is a phantom type and may not exactly reflect the value of
-- | the `Conj Boolean` inside. It is used according to variance: an output
-- | is typed as either `Lake` or `Stream flow` (for `River`), and conversely
-- | an input is typed as `Stream flow` (for `Lake`) or `River`, in the case
-- | that it is required to be `River`. This is designed to make fitting types
-- | together the easiest, to avoid inserting `dam` manually.
module Riverdragon.River
  ( Stream(..)
  , Lake
  , River
  , IsFlowing(..)
  , Flowing
  , NotFlowing
  , Id
  , oneStream
  , combineStreams
  , sampleOnLeft
  , sampleOnRight
  , (<*?>)
  , (<?*>)
  , sampleOnLeftOp
  , sampleOnRightOp
  , (<**?>)
  , (<?**>)
  , applyOp
  , (<**>)
  , createRiver
  , createRiverBurst
  , createRiverStore
  , createProxy'
  , makeLake
  , makeLake'
  , subscribe
  , subscribeIsh
  , onDestroyed
  , dam
  , stillRiver
  , unsafeRiver
  , unsafeCopyFlowing
  , burstOf
  , noBurst
  , bursting
  , alwaysBurst
  , instantiate
  , store
  , mayMemoize
  , memoize
  , cumulate
  , foldStream
  , emitState
  , statefulStream
  , limitTo
  , selfGating
  , singleShot
  , selfGatingEf
  , withInstantiated
  , alLake
  , alLake'
  , mapAl
  , mapLatest
  , latestStream
  , (>>~)
  , latestStreamEf
  , allStreams
  , allStreamsEf
  , fix
  , fixPrj
  , fixPrjBurst
  , fix'
  , mailbox
  , mailboxRiver
  , module ReExports
  )
  where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Bifoldable (bifoldMap)
import Data.Filterable (class Compactable, class Filterable, filterDefault, filterMap, partitionDefaultFilterMap, partitionMapDefault)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.These (These(..))
import Data.Traversable (class Foldable, foldMap, mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect, foreachE)
import Idiolect ((#..))
import Riverdragon.River.Bed (type (-!>), type (-&>), Allocar) as ReExports
import Riverdragon.River.Bed (type (-!>), type (-&>), Allocar, accumulator, allocLazy, breaker, cleanup, globalId, iteM, loadingBurst, ordMap, prealloc, prealloc2, pushArray, rolling, storeLast, storeUnsubscriber, subscriptions, threshold, unsafeAllocate)

-- | A notion of event streams that comes with a few more features in addition
-- | to the usual subscribe and unsubscribe.
-- |
-- | For the most part you can treat it as a normal FRP library and not care
-- | about the details of implementing these features (`destroy` and `commit`
-- | and so on).
-- |
-- | You can create streams with `createRiver` or `makeLake` (depending on how
-- | you want to generate events) and then subscribe to these streams with
-- | `subscribe` or `subscribeUntil`.
-- |
-- | `River`s (= `Stream True`) and `Lake`s (= `Stream False`) are the two
-- | flavours of streams, see elsewhere for details.
-- |
-- | There are various combinators, in addition to the standard `Applicative`
-- | and `Plus` interfaces.
data Stream (flow :: IsFlowing) a = Stream IsFlowing
  -- FIXME: buffer released after burst?
  ( { receive :: a -!> Unit, commit :: Id -!> Unit, destroyed :: Allocar Unit } -&>
    { burst :: Array a, sources :: Array Id, unsubscribe :: Allocar Unit }
  )
-- | A lake is (mostly) stagnant until you tap into it. Resources may be
-- | allocated per-subscriber. A lake may also be obtained from a river.
-- |
-- | `Lake = Stream NotFlowing`
type Lake = Stream NotFlowing
-- | A river keeps flowing regardless of whether anyone is listening. Events are
-- | broadcast uniformly to all current listeners. (With the exception of the
-- | initial burst.)
-- |
-- | `River = Stream Flowing`
type River = Stream Flowing

data IsFlowing = Flowing | NotFlowing
instance semigroupIsFlowing :: Semigroup IsFlowing where
  append Flowing Flowing = Flowing
  append Flowing r = r
  append r _ = r
instance monoidIsFlowing :: Monoid IsFlowing where
  mempty = Flowing
foreign import data Flowing :: IsFlowing
foreign import data NotFlowing :: IsFlowing

-- | Stream IDs are just integers. Used for tracking transactions via the
-- | `commit` event.
type Id = Int

-- | Using `map` on a `River` will memoize it to share the mapped values
-- | among all subscribers.
instance functorStream :: Functor (Stream flow) where
  map f (Stream t g) = mayMemoize $ Stream t \cbs -> do
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

-- | Takes two streams and combines the latest value from each.
-- |
-- | Since there is no ordering during initial subscriptions, the burst behavior
-- | takes the Cartesian product, according to `Apply Array`. This is so that
-- | it remains as a lawful instance, to preserve associativity and such.
-- |
-- | If you are looking to only take events from one stream with the latest
-- | value from the other, see below for `<*?>` and friends.
instance applyStream :: Apply (Stream flow) where
  apply = combineStreams (Both tt tt) ($)
-- | A `pure` stream emits a single value in its burst and immediately
-- | destroys itself.
instance applicativeStream :: Applicative (Stream flow) where
  pure a = Stream mempty \{ destroyed } -> do
    destroyed
    pure
      { sources: []
      , burst: [a]
      , unsubscribe: mempty
      }

instance semigroupStream :: Semigroup a => Semigroup (Stream flow a) where
  append = lift2 append
instance monoidStream :: Monoid a => Monoid (Stream flow a) where
  mempty = pure mempty
instance semiringStream :: Semiring a => Semiring (Stream flow a) where
  one = pure one
  zero = pure zero
  add = lift2 add
  mul = lift2 mul
instance ringStream :: Ring a => Ring (Stream flow a) where
  sub = lift2 sub
instance heytingAlgebraStream :: HeytingAlgebra a => HeytingAlgebra (Stream flow a) where
  tt = pure tt
  ff = pure ff
  not = map not
  conj = lift2 conj
  disj = lift2 disj
  implies = lift2 implies
instance booleanAlgebraStream :: BooleanAlgebra a => BooleanAlgebra (Stream flow a)

-- | Take events from every stream as they come in. This is an optimized version
-- | of `oneOf`, which combines events with `<|>`, only two at a time.
oneStream :: forall flow a. Array (Stream flow a) -> Stream flow a
oneStream [] = empty
oneStream [s] = s
oneStream streams = Stream (streams #.. \(Stream t _) -> t) \cbs -> do
  destroyed <- threshold (Array.length streams) cbs.destroyed
  streams #.. \(Stream _ s) ->
    s =<< do cbs { destroyed = _ } <$> cleanup destroyed

-- | Combine streams according to selection logic, where each side can reject
-- | having its update result in a downstream event (note that it still updates
-- | the latest value internally!).
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
      lastValues.setL (Just a)
      needsPush.set true
      pure unit
    commitL id = do
      n <- needsPush.get
      l <- if n then fst <$> lastValues.get else pure Nothing
      Tuple shouldCommit shouldPush <- case logic of
        -- It is always our responsibility to commit
        This p -> pure $ Tuple true $ map p l == Just true
        -- Never commit
        That _ -> pure $ Tuple false false
        -- Only commit if this source is unique to us
        Both p _ -> do
          shouldCommit <- not Set.member id <$> sourcesR.get
          pure $ Tuple shouldCommit $ map p l == Just true
      when shouldCommit do
        commit id shouldPush
    cbR b = do
      lastValues.setR (Just b)
      needsPush.set true
      pure unit
    commitR id = do
      n <- needsPush.get
      r <- if n then snd <$> lastValues.get else pure Nothing
      Tuple shouldCommit shouldPush <- case logic of
        -- Never commit
        This _ -> pure $ Tuple false false
        -- Always commit
        That p -> pure $ Tuple true $ map p r == Just true
        Both _ p -> pure $ Tuple true $ map p r == Just true
      when shouldCommit do
        commit id shouldPush

    commit id shouldPush = do
      when shouldPush do
        Tuple a b <- lastValues.get
        case lift2 comb a b of
          Just c -> cbs.receive c
          -- Have not received a value on both sides
          _ -> pure unit
      cbs.commit id
  -- and only count each destructor once (just in case)
  cbs1 <- cbs { receive = cbL, commit = commitL, destroyed = _ } <$> cleanup destroyed
  cbs2 <- cbs { receive = cbR, commit = commitR, destroyed = _ } <$> cleanup destroyed
  -- subscribe to upstream
  r1 <- e1 cbs1
  r2 <- e2 cbs2
  sourcesR.set (Set.fromFoldable r2.sources)
  -- initialize from the burst
  lastValues.setL (Array.last r1.burst)
  lastValues.setR (Array.last r2.burst)
  pure
    -- TODO: burst logic?
    { burst: lift2 comb r1.burst r2.burst
    , sources: bifoldMap (const r1.sources) (const r2.sources) logic
    , unsubscribe: r1.unsubscribe <> r2.unsubscribe
    }

sampleOnLeft :: forall flow a b. Stream flow (a -> b) -> Stream flow a -> Stream flow b
sampleOnLeft = combineStreams (This tt) ($)

sampleOnRight :: forall flow a b. Stream flow (a -> b) -> Stream flow a -> Stream flow b
sampleOnRight = combineStreams (That tt) ($)

infixl 4 sampleOnLeft as <*?>
infixl 4 sampleOnRight as <?*>

sampleOnLeftOp :: forall flow a b. Stream flow a -> Stream flow (a -> b) -> Stream flow b
sampleOnLeftOp = combineStreams (This tt) (#)

sampleOnRightOp :: forall flow a b. Stream flow a -> Stream flow (a -> b) -> Stream flow b
sampleOnRightOp = combineStreams (That tt) (#)

infixl 4 sampleOnLeftOp as <**?>
infixl 4 sampleOnRightOp as <?**>

applyOp :: forall f a b. Applicative f => f a -> f (a -> b) -> f b
applyOp ea ef = apply ((#) <$> ea) ef

infixl 4 applyOp as <**>


-- | Create a river as a message channel, that broadcasts any message to all of
-- | its subscribers (and then commits). This is effectful since each allocation
-- | creates a separate stream that does not affect other instantiations.
createRiver :: forall flow a. Allocar { send :: a -!> Unit, stream :: Stream flow a, destroy :: Allocar Unit }
createRiver = createRiverBurst mempty

-- | Create a river that has specific burst behavior.
createRiverBurst :: forall flow a. Allocar (Array a) -> Allocar { send :: a -!> Unit, stream :: Stream flow a, destroy :: Allocar Unit }
createRiverBurst burst = do
  id <- globalId
  { push, notify, destroy, running } <- subscriptions
  pure
    { send: \a -> do
        notify do
          \{ receive } -> receive a
        notify do
          \{ commit } -> commit id
    , stream: Stream mempty \cbs -> do
        bursted <- burst
        { burst: bursted, sources: [id], unsubscribe: _ } <$>
          iteM running (push cbs) (mempty <$ cbs.destroyed)
    , destroy: destroy _.destroyed
    }

-- | Create a river that stores its last value (you may give it an initial value
-- | too). Each time a new subscriber joins, it sends the last value it has
-- | seen, so that the subscriber is not missing anything.
createRiverStore ::
  forall flow a.
  Maybe a ->
  Allocar
    { send :: a -!> Unit
    , stream :: Stream flow a
    , destroy :: Allocar Unit
    , current :: Effect (Maybe a)
    }
createRiverStore initialValue = do
  lastValue <- storeLast
  case initialValue of
    Just a -> lastValue.set a
    _ -> pure unit
  r <- createRiverBurst $ lastValue.get <#> case _ of
    Just a -> [a]
    _ -> []
  pure
    { send: \a -> lastValue.set a *> r.send a
    , stream: r.stream
    , destroy: r.destroy
    , current: lastValue.get
    }

-- | Create a river that proxies another stream, including all receive and
-- | commit events individually, as opposed to creating a new `id` for itself.
createProxy' ::
  forall flow a.
  Allocar (Array a) ->
  Allocar
    { send :: a -!> Unit
    , commit :: Id -!> Unit
    , stream :: Array Id -> Stream flow a
    , destroy :: Allocar Unit
    }
createProxy' burst = do
  { push, notify, destroy, running } <- subscriptions
  pure
    { send: \a -> do
        notify \{ receive } -> receive a
    , commit: \id -> do
        notify \{ commit } -> commit id
    , stream: \sources -> Stream Flowing \cbs -> do
        bursted <- burst
        { burst: bursted, sources, unsubscribe: _ } <$>
          iteM running (push cbs) (mempty <$ cbs.destroyed)
    , destroy: destroy _.destroyed
    }

-- | Make a lake whose event stream gets allocated per subscriber, e.g. for
-- | timeouts or the like.
makeLake :: forall a. ((a -!> Unit) -> Allocar (Allocar Unit)) -> Stream NotFlowing a
makeLake = makeLake' <<< const

makeLake' :: forall a. (Allocar Unit -> (a -!> Unit) -> Allocar (Allocar Unit)) -> Stream NotFlowing a
makeLake' streamTemplate = Stream NotFlowing \cbs -> do
  id <- globalId
  loadingBurst \whenLoaded -> do
    unsubscribe <- streamTemplate cbs.destroyed do
      \a -> whenLoaded a do
        cbs.receive a
        cbs.commit id
    pure { burst: _, sources: [id], unsubscribe }


-- | Subscribe to a stream with a specific callback (`a -!> Unit`). Returns an
-- | unsubscribe procedure (`Allocar Unit`).
-- |
-- | Burst events are applied before `subscribe` returns: you do not need to
-- | handle them manually.
subscribe :: forall flow a. Stream flow a -> (a -!> Unit) -> Allocar (Allocar Unit)
subscribe = subscribeIsh mempty

-- | Subscribe with an additional callback for when the stream is destroyed.
subscribeIsh :: forall flow a. Allocar Unit -> Stream flow a -> (a -!> Unit) -> Allocar (Allocar Unit)
subscribeIsh destroyed (Stream _ stream) receive = do
  -- We delete the unsubscriber when the stream is destroyed, to avoid leaking
  -- resources from the closure of it.
  unsubscriber <- storeUnsubscriber
  hasLoaded <- prealloc false
  isDestroyed <- prealloc false
  let
    upstreamDestroyed = unsubscriber.wrapDestroy do
      isDestroyed.set true
      whenM hasLoaded.get destroyed
  r <- stream { receive, commit: mempty, destroyed: upstreamDestroyed }
  unsubscriber.set r.unsubscribe
  foreachE r.burst receive
  hasLoaded.set true
  whenM isDestroyed.get destroyed
  -- this stages `destroyed`
  pure unsubscriber.unsub

onDestroyed :: forall a. Effect Unit -> River a -> Effect Unit
onDestroyed cb stream = void $ subscribeIsh cb stream mempty

-- | Dam.
dam :: forall flowIn a. Stream flowIn a -> Lake a
dam (Stream _ f) = Stream NotFlowing f

stillRiver :: forall flowOut a. River a -> Stream flowOut a
stillRiver (Stream t s) = Stream t s

-- | Unsafe.
unsafeRiver :: forall flowIn flowOut a. Stream flowIn a -> Stream flowOut a
unsafeRiver (Stream _ f) = Stream Flowing f

unsafeCopyFlowing :: forall flow1 flow2 a b.
  Stream flow1 a -> Stream flow2 b -> Stream flow1 b
unsafeCopyFlowing (Stream f _) (Stream _ stream) = Stream f stream

-- | Hmm, ugly but kind of necessary
burstOf :: forall flow a. Stream flow a -> Allocar (Array a)
burstOf (Stream _ stream) = do
  { burst, unsubscribe } <- stream mempty
  burst <$ unsubscribe

-- | Cancel out the burst behavior of any stream
noBurst :: forall flow a. Stream flow a -> Stream flow a
noBurst (Stream t stream) = Stream t \cbs -> stream cbs <#> _ { burst = [] }

-- | Create a stream that is only a burst of elements. Like `oneOfMap pure`.
bursting :: forall flow a. Array a -> Stream flow a
bursting burst = Stream mempty \{ destroyed } -> do
  destroyed
  pure
    { sources: []
    , burst
    , unsubscribe: mempty
    }

-- | If there is no burst, insert a `Nothing`.
-- | Kind of like `pure Nothing <|> Just <$> stream`
alwaysBurst :: forall flow a. Stream flow a -> Stream flow (Maybe a)
alwaysBurst = map Just >>> \(Stream t stream) ->
  Stream t \cbs -> stream cbs <#>
    \r -> r { burst = if Array.null r.burst then [Nothing] else r.burst }

alwaysBurstM :: forall flow m. Monoid m => Stream flow m -> Stream flow m
alwaysBurstM (Stream t stream) =
  Stream t \cbs -> stream cbs <#>
    \r -> r { burst = if Array.null r.burst then [mempty] else r.burst }


data Course a
  = StoreDedup (a -> a -> Boolean)
  | Bursting (Effect (Array a) -> Effect (Array a))
  | History

coursing :: forall flowIn flowOut a.
  Course a ->
  Stream flowIn a ->
  Allocar
    { burst :: Array a
    , stream :: Stream flowOut a
    , destroy :: Allocar Unit
    }
coursing chosenCourse strm@(Stream _ stream) = case chosenCourse of
  StoreDedup equalitor -> do
    lastValue <- storeLast
    { send, commit, stream: streamDependingOn, destroy } <- createProxy' (Array.fromFoldable <$> lastValue.get)
    { burst, sources, unsubscribe } <-
      stream { receive: lastValue.set <> send, commit, destroyed: destroy }
    case Array.last burst of
      Just v -> lastValue.set v
      _ -> pure unit
    pure { burst, stream: streamDependingOn sources, destroy: unsubscribe <> destroy }
  Bursting getBurstBasedOn -> do
    { send, commit, stream: streamDependingOn, destroy } <- createProxy' (getBurstBasedOn (burstOf strm))
    { burst, sources, unsubscribe } <-
      stream { receive: send, commit, destroyed: destroy }
    pure { burst, stream: streamDependingOn sources, destroy: unsubscribe <> destroy }
  History -> do
    history <- pushArray
    { send, commit, stream: streamDependingOn, destroy } <- createProxy' history.read
    { burst, sources, unsubscribe } <-
      stream { receive: history.push <> send, commit, destroyed: destroy }
    pure { burst, stream: streamDependingOn sources, destroy: unsubscribe <> destroy }

-- | Create a single subscriber to an upstream stream that broadcasts it to
-- | multiple downstream subscribers at once. It returns any flow type simply
-- | for the sake of polymorphism: its return stream is definitely a `River`.
-- |
-- | It inherits burst behavior from upstream, by subscribing to it and
-- | immediately unsubscribing.
instantiate ::
  forall flowIn flowOut a.
  Stream flowIn a ->
  Allocar
    { burst :: Array a
    , stream :: Stream flowOut a
    , destroy :: Allocar Unit
    }
instantiate (Stream Flowing stream) = pure
  { burst: [], stream: Stream Flowing stream, destroy: pure unit }
instantiate strm@(Stream _ stream) = do
  { send, commit, stream: streamDependingOn, destroy } <- createProxy' (burstOf strm)
  { burst, sources, unsubscribe } <-
    stream { receive: send, commit, destroyed: destroy }
  pure { burst, stream: streamDependingOn sources, destroy: unsubscribe <> destroy }

-- | Instantiate a stream and inform each new subscriber of the last value that
-- | it saw.
store ::
  forall flowIn flowOut a.
  Stream flowIn a ->
  Allocar
    { burst :: Array a
    , stream :: Stream flowOut a
    , destroy :: Allocar Unit
    }
store (Stream _ stream) = do
  lastValue <- storeLast
  { send, commit, stream: streamDependingOn, destroy } <- createProxy' (Array.fromFoldable <$> lastValue.get)
  { burst, sources, unsubscribe } <-
    stream { receive: lastValue.set <> send, commit, destroyed: destroy }
  case Array.last burst of
    Just v -> lastValue.set v
    _ -> pure unit
  pure { burst, stream: streamDependingOn sources, destroy: unsubscribe <> destroy }

-- | This function memoizes rivers, because it is safe to do so. It does not
-- | memoize lakes.
mayMemoize :: forall flow a. Stream flow a -> Stream flow a
mayMemoize (Stream Flowing upstream) = unsafeAllocate do
  -- the implementation is funny just to capture the initial burst, really.
  -- otherwise it would just be a lazy instantiate.
  { push, notify, destroy, size, running } <- subscriptions
  cachedStream <- prealloc Nothing
  let
    receive = \a -> notify \sub -> sub.receive a
    commit = \id -> notify \sub -> sub.commit id
    destroyed = destroy _.destroyed *> down
    upstreamCbs = { receive, commit, destroyed }
    up = cachedStream.get >>= case _ of
      Just { sources } -> do
        { sources, burst: _ } <$> burstOf (Stream Flowing upstream)
      _ -> do
        { burst, sources, unsubscribe } <- upstream upstreamCbs
        { burst, sources } <$ cachedStream.set (Just { sources, unsubscribe })
    down = do
      sz <- size
      cs <- cachedStream.get
      case sz, cs of
        0, Just { unsubscribe } -> unsubscribe
        _, _ -> pure unit
  pure $ Stream Flowing \cbs ->
    iteM (not <$> running) (mempty <$ cbs.destroyed) do
      unsub <- push cbs
      { sources, burst } <- up
      pure { burst, sources, unsubscribe: unsub <> down }
mayMemoize stream = stream

-- | This function is only pure for rivers. It returns any flow type simply for
-- | the sake of polymorphism: morally it returns a `River`.
memoize :: forall flow a. River a -> Stream flow a
memoize (Stream _ stream) = mayMemoize (Stream Flowing stream)


cumulate :: forall f flow m. Foldable f => Monoid m => f (Stream flow m) -> Stream flow m
cumulate = foldMap alwaysBurstM


-- | Fold a function over a stream, giving it internal state.
foldStream :: forall flow a b. b -> Stream flow a -> (b -> a -> b) -> Lake b
foldStream b0 upstream folder = pure b0 <|> statefulStream b0 upstream
  \b a -> folder b a # emitState

-- | Helper: always emit the state.
emitState :: forall state. state -> { state :: state, emit :: Maybe state }
emitState state = { state, emit: Just state }

-- | A stateful stream that can update its own state and emit values.
statefulStream :: forall flow a b c. b -> Stream flow a -> (b -> a -> { state :: b, emit :: Maybe c }) -> Lake c
statefulStream b0 (Stream t stream) folder = Stream t \cbs -> do
  current <- prealloc b0
  upstream <- stream $ cbs { receive = _ } \a -> do
    { state: b, emit: c } <- folder <$> current.get <@> a
    current.set b
    case c of
      Just v -> cbs.receive v
      _ -> pure unit
  let
    folder' b a =
      let r = folder b a in
      { accum: r.state, value: r.emit }
  let { value: burst, accum: b1 } = mapAccumL folder' b0 upstream.burst
  current.set b1
  pure upstream { burst = Array.catMaybes burst }

instance compactableStream :: Compactable (Stream flow) where
  compact s = filterMap identity s
  separate s = partitionMapDefault identity s
instance filterableStream :: Filterable (Stream flow) where
  filter f s = filterDefault f s
  partition f s = partitionDefaultFilterMap f s
  partitionMap f s = partitionMapDefault f s
  filterMap f (Stream flow stream) = mayMemoize $ Stream flow \cbs -> do
    r <- stream
      { receive: \a -> case f a of
          Just b -> do
            cbs.receive b
          _ -> pure unit
      -- always pass commit through
      , commit: cbs.commit
      , destroyed: cbs.destroyed
      }
    pure r { burst = filterMap f r.burst }


limitTo :: forall flow a. Int -> Stream flow a -> Lake a
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

selfGating :: forall flow a. (a -> Boolean) -> Stream flow a -> Lake a
selfGating pred = selfGatingEf \stop -> pure \a -> when (pred a) stop

singleShot :: forall flow. Stream flow ~> Lake
singleShot = selfGating tt

selfGatingEf :: forall flow a.
  (Effect Unit -> Effect (a -> Effect Unit)) ->
  Stream flow a -> Lake a
selfGatingEf logic (Stream flow setup) = Stream flow \cbs -> do
  setUnsub <- rolling
  receive <- breaker cbs.receive
  commit <- breaker cbs.commit
  let destroyed = receive.trip <> commit.trip <> cbs.destroyed
  lastValue <- storeLast
  process <- logic (setUnsub mempty <> destroyed)
  sub <- setup $
      { receive: lastValue.set
      , commit: const $ lastValue.run process
      , destroyed: mempty
      }
    <>
      { receive: receive.run
      , commit: commit.run
      , destroyed: destroyed
      }
  setUnsub sub.unsubscribe
  pure sub


withInstantiated :: forall flow a b.
  Stream flow a ->
  (Array a -> River a -> Lake b) ->
  Lake b
withInstantiated toFlow f = alLake do
  { burst, stream, destroy } <- instantiate toFlow
  pure { lake: f burst stream, destroy }


alLake :: forall a.
  Allocar { lake :: Lake a, destroy :: Allocar Unit } ->
  Lake a
alLake mkLake = Stream NotFlowing \cbs -> do
  { lake: Stream _ lake, destroy } <- mkLake
  r <- lake cbs
  pure r { unsubscribe = r.unsubscribe <> destroy }

alLake' :: forall a.
  Allocar (Lake a) ->
  Lake a
alLake' mkLake = Stream NotFlowing \cbs -> do
  Stream _ lake <- mkLake
  lake cbs


-- is this allowed to be polymorphic?
-- mapEf :: forall flow a b. (a -> Effect b) -> Stream flow a -> Stream flow b
-- mapEf f (Stream t g) = Stream t \cbs -> do
--   r <- g cbs { receive = \a -> cbs.receive =<< f a }
--   burst <- traverse f r.burst
--   pure r { burst = burst }

mapAl :: forall flow a b. (a -> Allocar b) -> Stream flow a -> Stream flow b
mapAl f (Stream t g) = mayMemoize $ Stream t \cbs -> do
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

-- mapCb ::
--   forall flow a b.
--   ( { burst :: Array a
--     , receive :: b -> Effect Unit
--     , destroyed :: Allocar Unit
--     } -&>
--     { burst :: Array b
--     , receive :: a -> Effect Unit
--     , unsubscribe :: Allocar Unit
--     , destroyed :: Allocar Unit
--     }
--   ) ->
--   Stream flow a -> Stream flow b
-- mapCb f (Stream t g) = Stream t \cbs -> do
--   lastValue <- storeLast
--   receiver <- accumulator
--   destroyer <- accumulator
--   r <- g cbs
--     { receive = lastValue.set
--     , commit = \_ -> lastValue.run \a ->
--         join do receiver.get <@> a
--     , destroyed = join destroyer.get
--     }
--   { burst, receive, unsubscribe, destroyed } <- f
--     { burst: r.burst, receive: cbs.receive, destroyed: cbs.destroyed }
--   receiver.put receive
--   destroyer.put destroyed
--   pure
--     { burst
--     , sources: r.sources
--     , unsubscribe: unsubscribe <> r.unsubscribe
--     }

mapLatest :: forall flowInner flowIn a b. (a -> Stream flowInner b) -> Stream flowIn a -> Lake b
mapLatest = flip latestStream

latestStream :: forall flowIn flowInner a b. Stream flowIn a -> (a -> Stream flowInner b) -> Lake b
latestStream source mkStream = latestStreamEf source (mkStream >>> pure)

infixl 1 latestStream as >>~

latestStreamEf ::
  forall flowIn flowInner a b.
  Stream flowIn a ->
  (a -> Allocar (Stream flowInner b)) ->
  Lake b
latestStreamEf source mkStream = makeLake \cb -> do
  replace <- rolling
  unsubscribe <- subscribe source \a -> do
    -- Unsubscribe first in case one of the next actions would have triggered
    -- the old stream to emit any more events before the subscription got
    -- properly replaced
    replace mempty
    stream <- mkStream a
    replace =<< subscribe stream cb
  pure $ replace mempty <> unsubscribe

-- | Will subscribe to all streams produced. Use with care!
allStreamsEf ::
  forall flowIn flowInner a b.
  Stream flowIn a ->
  (a -> Allocar (Stream flowInner b)) ->
  Lake b
allStreamsEf source mkStream = makeLake \cb -> do
  unsubscribes <- accumulator
  unsubscribe <- subscribe source \a -> do
    stream <- mkStream a
    unsubscribes.put =<< subscribe stream cb
  pure $ join unsubscribes.get <> unsubscribe

allStreams ::
  forall flowIn flowInner a b.
  Stream flowIn a ->
  (a -> Stream flowInner b) ->
  Lake b
allStreams source mkStream = allStreamsEf source $ pure <<< mkStream


-- | Compute a “fixpoint” or “fixed point” of a stream function.
-- |
-- | The creation function returns one stream to be used as a loopback (which
-- | can be used to maintain state and other kinds of feedback) and another
-- | stream to be used as the output.
fix :: forall flow1 flow2 flow3 o i.
  ( Stream flow1 i ->
    { loopback :: Stream flow2 i
    , output :: Stream flow3 o
    }
  ) ->
  Lake o
fix f = fix' \feedback ->
  let { loopback, output } = f feedback in
  pure { loopback, output, destroy: mempty }

-- | Fix with two projections, so that there is a single subscription that
-- | generates both the output and loopback.
fixPrj :: forall flow1 flow2 i r o.
  (r -> Maybe i) ->
  (r -> Maybe o) ->
  (Stream flow1 i -> Stream flow2 r) ->
  Lake o
fixPrj p1 p2 f = fix' \feedback -> do
  { stream: common, destroy } <- instantiate (f feedback)
  let loopback = filterMap p1 common
  let output = filterMap p2 common
  pure { loopback, output, destroy }

fixPrjBurst :: forall flow1 flow2 i r o.
  Maybe i -> (r -> Maybe i) ->
  Maybe o -> (r -> Maybe o) ->
  (Stream flow1 i -> Stream flow2 r) ->
  Lake o
fixPrjBurst bi p1 bo p2 f =
  maybe empty pure bo <|>
    fixPrj p1 p2 \i -> f (maybe empty pure bi <|> i)

-- | - The feedback always comes in as a `River`: it is specifically allocated
-- |   just for this subscription and merely proxies events from the loopback.
-- | - The loopback stream that it returns can be either a `River` or a `Lake`:
-- |   it does not matter since there will only ever be one subscriber.
-- | - The output stream also only has one subscriber and can be either.
-- | - Finally, the whole function always produces a `Lake`: it is not a
-- |   running stream on its own, just a description of how to produce one
-- |   on demand, when a subscriber arrives.
fix' :: forall flow1 flow2 flow3 o i.
  ( Stream flow1 i ->
    Allocar
      { loopback :: Stream flow2 i
      , output :: Stream flow3 o
      , destroy :: Allocar Unit
      }
  ) ->
  Lake o
fix' mkLoop = makeLake \cb -> do
  { stream: feedback, send, destroy: destroy1 } <- createRiver
  { loopback, output, destroy: destroy2 } <- mkLoop feedback
  unsub1 <- subscribe loopback send
  unsub2 <- subscribe output cb
  pure $ unsub2 <> unsub1 <> destroy2 <> destroy1

-- | This efficiently sorts “mail” values based on their key, so that upstream
-- | only receives on listener and the downstream rivers only get pinged for
-- | value that match their key.
-- |
-- | Because we want each downstream mailbox to only receive updates it really
-- | cares about (for efficiency), each stream gets its own ID and it will
-- | behave differently than `filterMap`, which preserves stream ID and thus has
-- | to pass commits through even for filtered events. For `mailbox`, each
-- | stream counts as its own stream and does its own commit immediately.
mailbox :: forall flowIn flowOut k v. Ord k =>
  Stream flowIn { key :: k, value :: v } ->
  Allocar { byKey :: k -> Stream flowOut v, destroy :: Allocar Unit }
mailbox upstream = do
  -- the reason i created allocar tbh
  mailboxes <- ordMap
  destroyers <- accumulator
  destroyed <- prealloc false
  let
    -- When upstream goes away, downstreams need to also inform that they are
    -- going away
    destroyDownstreams = do
      destroyed.set true
      join destroyers.reset
      void mailboxes.reset
  unsubscribe <- subscribeIsh
    destroyDownstreams
    upstream
    \{ key, value } -> do
      -- this runs only if there has been a subscriber for the event
      mailboxes.onKey key \downstreamSend -> do
        downstreamSend value
  -- if we're been keep good track of Alloc vs Effect, allocLazy is slightly
  -- nicer than raw unsafePerformEffect
  byKey <- allocLazy $ pure \selected -> do
    -- if it has been destroyed, return an empty stream
    iteM destroyed.get (pure empty) do
      downstream <- createRiver
      destroyers.put downstream.destroy
      mailboxes.set selected downstream.send
      pure downstream.stream
  pure { byKey, destroy: unsubscribe <> destroyDownstreams }

mailboxRiver :: forall flowOut k v. Ord k =>
  River { key :: k, value :: v } ->
  (k -> Stream flowOut v)
mailboxRiver = _.byKey <<< unsafeAllocate <<< mailbox

