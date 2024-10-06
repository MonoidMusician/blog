module Riverdragon.River where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as STRef
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object.ST as STO
import Prim.Boolean (False, True)
import Riverdragon.River.Bed (type (-!>), type (-&>), Allocar, accumulator, allocLazy, cleanup, globalId, iteM, loadingBurst, ordMap, prealloc, prealloc2, subscriptions, threshold)

type Id = Int

-- | Transactions, especially for event deduplication (DAG).
-- | Actor model? “I changed” vs “you changed”?
-- | Errors/stream cancelation like RxJS?
data Stream (flow :: Boolean) a = Stream (Conj Boolean)
  -- FIXME: buffer destroyed after burst?
  ( { receive :: a -!> Unit, commit :: Id -!> Unit, destroyed :: Allocar Unit } -&>
    { burst :: Array a, sources :: Array Id, unsubscribe :: Allocar Unit }
  )

instance functorStream :: Functor (Stream flow) where
  map f (Stream t g) = Stream t \cbs -> do
    r <- g do
      cbs { receive = \a -> cbs.receive (f a) }
    pure r { burst = map f r.burst }
-- | The `alt` of two events forwards all data and commits, and waits until
-- | both are destroyed to destroy downstream.
instance altStream :: Alt (Stream flow) where
  alt (Stream t1 e1) (Stream t2 e2) = Stream (t1 <> t2) \cbs -> do
    -- only run it once both have been destroyed
    destroyed <- threshold 2 cbs.destroyed
    -- and only count each once (just in case)
    cbs1 <- cbs { destroyed = _ } <$> cleanup destroyed
    cbs2 <- cbs { destroyed = _ } <$> cleanup destroyed
    e1 cbs1 <> e2 cbs2
-- | The `empty` event destroys itself immediately upon subscription.
instance plusStream :: Plus (Stream flow) where
  empty = Stream mempty \cbs -> do
    cbs.destroyed
    pure { burst: empty, sources: mempty, unsubscribe: mempty }
instance applyStream :: Apply (Stream flow) where
  apply (Stream t1 e1) (Stream t2 e2) = Stream (t1 <> t2) \cbs -> do
      -- only run it once both have been destroyed
      destroyed <- threshold 2 cbs.destroyed
      lastValues <- prealloc2 Nothing Nothing
      sourcesR <- prealloc Set.empty
      let
        cbL = \f -> do
          _ <- lastValues.setL (Just f)
          pure unit
        commitL = \id -> do
          srcsR <- sourcesR.get
          when (not Set.member id srcsR) do
            commitR id
        cbR = \a -> do
          _ <- lastValues.setR (Just a)
          pure unit
        commitR = \id -> do
          Tuple f a <- lastValues.get
          case f <*> a of
            Nothing -> pure unit
            Just b -> cbs.receive b
          cbs.commit id
      -- and only count each once (just in case)
      cbs1 <- cbs { receive = cbL, commit = commitL, destroyed = _ } <$> cleanup destroyed
      cbs2 <- cbs { receive = cbR, commit = commitR, destroyed = _ } <$> cleanup destroyed
      r1 <- e1 cbs1
      r2 <- e2 cbs2
      _ <- lastValues.setL (Array.last r1.burst)
      _ <- lastValues.setR (Array.last r2.burst)
      _ <- sourcesR.set (Set.fromFoldable r2.sources)
      pure
        { burst: r1.burst <*> r2.burst
        , sources: r1.sources <> r2.sources
        , unsubscribe: r1.unsubscribe <> r2.unsubscribe
        }
instance applicativeStream :: Applicative (Stream flow) where
  pure a = Stream mempty \{ destroyed } -> do
    destroyed
    pure
      { sources: []
      , burst: [a]
      , unsubscribe: mempty
      }


-- | Create a (hot) event as a message channel, that sends any message to all of
-- | its subscribers (and then commits).
createStream :: forall flow a. Allocar { send :: a -!> Unit, event :: Stream flow a, destroy :: Allocar Unit }
createStream = do
  id <- globalId
  { push, notify, destroy } <- subscriptions
  pure
    { send: \a -> do
        notify do
          \{ receive } -> receive a
        notify do
          \{ commit } -> commit id
    , event: Stream mempty \cbs -> do
        push cbs <#> { burst: [], sources: [id], unsubscribe: _ }
    , destroy: destroy do
        _.destroyed
    }

createStream' :: forall flow a. Allocar { send :: a -!> Unit, commit :: Id -!> Unit, event :: Array Id -> Stream flow a, destroy :: Allocar Unit }
createStream' = do
  { push, notify, destroy } <- subscriptions
  pure
    { send: \a -> do
        notify \{ receive } -> receive a
    , commit: \id -> do
        notify \{ commit } -> commit id
    , event: \sources -> Stream mempty \cbs -> do
        push cbs <#> { burst: [], sources, unsubscribe: _ }
    , destroy: destroy $ _.destroyed
    }

-- | Make a (cold) event that gets allocated per subscriber, e.g. for listeners
-- | or timeouts or the like.
makeStream :: forall a. ((a -!> Unit) -> Allocar (Allocar Unit)) -> Stream False a
makeStream eventTemplate = Stream (Conj false) \cbs -> do
  id <- globalId
  loadingBurst \pushInitial isLoading -> do
    unsubscribe <- eventTemplate do
      \a -> do
        iteM isLoading
          (pushInitial a)
          do
            cbs.receive a
            cbs.commit id
    pure { burst: _, sources: [id], unsubscribe }

-- | Unsafe.
unsafeMarkHot :: forall flowIn flowOut a. Stream flowIn a -> Stream flowOut a
unsafeMarkHot (Stream _ f) = Stream (Conj true) f

chill :: forall flowIn a. Stream flowIn a -> Stream False a
chill (Stream _ f) = Stream (Conj false) f

-- | Subscribe to an event with a specific callback. Returns an unsubscribe
-- | procedure (`Allocar Unit`).
subscribe :: forall flow a. Stream flow a -> (a -!> Unit) -> Allocar (Allocar Unit)
subscribe (Stream _ event) receive = do
  r <- event { receive, commit: mempty, destroyed: mempty }
  for_ r.burst \a -> receive a
  pure r.unsubscribe

-- | Create a single subscriber to an upstream event that broadcasts it to
-- | multiple downstream subscribers at once. This creation is effectful because
-- | it creates a subscriber, thus starting some effects.
instantiate :: forall flowIn flowOut a. Stream flowIn a -> Allocar { event :: Stream flowOut a, destroy :: Allocar Unit }
instantiate (Stream _ event) = do
  { send, commit, event: eventDependingOn, destroy } <- createStream'
  { sources, unsubscribe } <-
    event { receive: send, commit, destroyed: destroy }
  pure { event: eventDependingOn sources, destroy: unsubscribe <> destroy }

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
      downstream <- createStream'
      destroyers.put downstream.destroy
      _ <- mailboxes.set selected downstream
      pure $ downstream.event sources
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
  m <- mailbox (map { key: _, value: unit } upstream.event)
  let
    write = \a -> void $ liftST (STRef.write (Just a) ref)
    downstream = unsafeMarkHot do
      makeStream \k -> do
        liftST (STRef.read ref) >>= traverse_ (k)
        subscribe upstream.event k
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
  pure do
    \k -> do
      existing <- liftST (STO.peek k interfaces)
      case existing of
        Just r -> pure r
        Nothing -> do
          io <- createBehavioral
          io <$ liftST (STO.poke k io interfaces)
