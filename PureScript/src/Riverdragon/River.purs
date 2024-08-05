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
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, runEffectFn1)
import Foreign.Object.ST as STO
import Idiolect (type (-!>))
import Prim.Boolean (False, True)
import Riverdragon.River.Bed (type (-&>), Allocar(..), AllocateFrom(..), accumulator, allocLazy, cleanup, globalId, iteM, loading, loadingBurst, ordMap, prealloc, prealloc2, pushArray, subscriptions, threshold)
import Safe.Coerce (coerce)

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
  map f (Stream t (AllocateFrom g)) = Stream t do
    AllocateFrom do
      mkEffectFn1 \cbs -> do
        r <- runEffectFn1 g do
          cbs { receive = mkEffectFn1 \a -> runEffectFn1 cbs.receive (f a) }
        pure r { burst = map f r.burst }
-- | The `alt` of two events forwards all data and commits, and waits until
-- | both are destroyed to destroy downstream.
instance altStream :: Alt (Stream flow) where
  alt (Stream t1 (AllocateFrom e1)) (Stream t2 (AllocateFrom e2)) = Stream (t1 <> t2) do
    AllocateFrom do
      mkEffectFn1 \cbs -> unwrap do
        -- only run it once both have been destroyed
        destroyed <- threshold 2 cbs.destroyed
        -- and only count each once (just in case)
        cbs1 <- cbs { destroyed = _ } <$> cleanup destroyed
        cbs2 <- cbs { destroyed = _ } <$> cleanup destroyed
        Allocar do runEffectFn1 e1 cbs1 <> runEffectFn1 e2 cbs2
-- | The `empty` event destroys itself immediately upon subscription.
instance plusStream :: Plus (Stream flow) where
  empty = Stream mempty do
    AllocateFrom do
      mkEffectFn1 \cbs -> do
        unwrap cbs.destroyed
        pure { burst: empty, sources: mempty, unsubscribe: mempty }
instance applyStream :: Apply (Stream flow) where
  apply (Stream t1 (AllocateFrom e1)) (Stream t2 (AllocateFrom e2)) = Stream (t1 <> t2) do
    AllocateFrom do
      mkEffectFn1 \cbs -> unwrap do
        -- only run it once both have been destroyed
        destroyed <- threshold 2 cbs.destroyed
        lastValues <- prealloc2 Nothing Nothing
        sourcesR <- prealloc Set.empty
        let
          cbL = mkEffectFn1 \f -> do
            _ <- runEffectFn1 (unwrap lastValues.setL) (Just f)
            pure unit
          commitL = mkEffectFn1 \id -> do
            srcsR <- unwrap sourcesR.get
            when (not Set.member id srcsR) do
              runEffectFn1 commitR id
          cbR = mkEffectFn1 \a -> do
            _ <- runEffectFn1 (unwrap lastValues.setR) (Just a)
            pure unit
          commitR = mkEffectFn1 \id -> do
            Tuple f a <- unwrap lastValues.get
            case f <*> a of
              Nothing -> pure unit
              Just b -> runEffectFn1 cbs.receive b
            runEffectFn1 cbs.commit id
        -- and only count each once (just in case)
        cbs1 <- cbs { receive = cbL, commit = commitL, destroyed = _ } <$> cleanup destroyed
        cbs2 <- cbs { receive = cbR, commit = commitR, destroyed = _ } <$> cleanup destroyed
        r1 <- Allocar do runEffectFn1 e1 cbs1
        r2 <- Allocar do runEffectFn1 e2 cbs2
        _ <- Allocar do runEffectFn1 (unwrap lastValues.setL) (Array.last r1.burst)
        _ <- Allocar do runEffectFn1 (unwrap lastValues.setR) (Array.last r2.burst)
        _ <- Allocar do runEffectFn1 (unwrap sourcesR.set) (Set.fromFoldable r2.sources)
        pure
          { burst: r1.burst <*> r2.burst
          , sources: r1.sources <> r2.sources
          , unsubscribe: r1.unsubscribe <> r2.unsubscribe
          }
instance applicativeStream :: Applicative (Stream flow) where
  pure a = Stream mempty do
    AllocateFrom do
      mkEffectFn1 \{ destroyed } -> do
        unwrap destroyed
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
  { push: AllocateFrom push, notify, destroy: AllocateFrom destroy } <- subscriptions
  pure
    { send: mkEffectFn1 \a -> do
        runEffectFn1 notify do
          mkEffectFn1 \{ receive } -> runEffectFn1 receive a
        runEffectFn1 notify do
          mkEffectFn1 \{ commit } -> runEffectFn1 commit id
    , event: Stream mempty $ AllocateFrom $ mkEffectFn1 \cbs -> do
        runEffectFn1 push cbs <#> { burst: [], sources: [id], unsubscribe: _ }
    , destroy: Allocar do
        runEffectFn1 destroy do
          mkEffectFn1 \{ destroyed: Allocar destroyed } -> destroyed
    }

createStream' :: forall flow a. Allocar { send :: a -!> Unit, commit :: Id -!> Unit, event :: Array Id -> Stream flow a, destroy :: Allocar Unit }
createStream' = do
  { push: AllocateFrom push, notify, destroy: AllocateFrom destroy } <- subscriptions
  pure
    { send: mkEffectFn1 \a -> do
        runEffectFn1 notify do
          mkEffectFn1 \{ receive } -> runEffectFn1 receive a
    , commit: mkEffectFn1 \id -> do
        runEffectFn1 notify do
          mkEffectFn1 \{ commit } -> runEffectFn1 commit id
    , event: \sources -> Stream mempty $ AllocateFrom $ mkEffectFn1 \cbs -> do
        runEffectFn1 push cbs <#> { burst: [], sources, unsubscribe: _ }
    , destroy: Allocar do
        runEffectFn1 destroy do
          mkEffectFn1 \{ destroyed: Allocar destroyed } -> destroyed
    }

-- | Make a (cold) event that gets allocated per subscriber, e.g. for listeners
-- | or timeouts or the like.
makeStream :: forall a. ((a -!> Unit) -> Allocar (Allocar Unit)) -> Stream False a
makeStream eventTemplate = Stream (Conj false) do
  AllocateFrom do
    mkEffectFn1 \cbs -> do
      id <- unwrap globalId
      unwrap $ loadingBurst \pushInitial isLoading -> do
        unsubscribe <- coerce eventTemplate do
          mkEffectFn1 \a -> do
            iteM (unwrap isLoading)
              do
                unwrap (pushInitial a)
              do
                runEffectFn1 cbs.receive a
                runEffectFn1 cbs.commit id
        pure { burst: _, sources: [id], unsubscribe }

-- | Unsafe.
unsafeMarkHot :: forall flowIn flowOut a. Stream flowIn a -> Stream flowOut a
unsafeMarkHot (Stream _ f) = Stream (Conj true) f

chill :: forall flowIn a. Stream flowIn a -> Stream False a
chill (Stream _ f) = Stream (Conj false) f

-- | Subscribe to an event with a specific callback. Returns an unsubscribe
-- | procedure (`Allocar Unit`).
subscribe :: forall flow a. Stream flow a -> (a -!> Unit) -> Allocar (Allocar Unit)
subscribe (Stream _ (AllocateFrom event)) receive = coerce do
  r <- runEffectFn1 event { receive, commit: mempty, destroyed: mempty }
  for_ r.burst \a -> runEffectFn1 receive a
  pure r.unsubscribe

-- | Create a single subscriber to an upstream event that broadcasts it to
-- | multiple downstream subscribers at once. This creation is effectful because
-- | it creates a subscriber, thus starting some effects.
instantiate :: forall flowIn flowOut a. Stream flowIn a -> Allocar { event :: Stream flowOut a, destroy :: Allocar Unit }
instantiate (Stream _ (AllocateFrom event)) = Allocar do
  { send, commit, event: eventDependingOn, destroy } <- unwrap createStream'
  { sources, unsubscribe } <-
    runEffectFn1 event { receive: send, commit, destroyed: coerce destroy }
  pure { event: eventDependingOn sources, destroy: unsubscribe <> destroy }

-- the reason i created allocar tbh
mailbox :: forall flowIn flowOut k v. Ord k =>
  Stream flowIn { key :: k, value :: v } ->
  Allocar { byKey :: k -> Stream flowOut v, destroy :: Allocar Unit }
mailbox (Stream _ (AllocateFrom upstream)) = Allocar do
  mailboxes <- unwrap ordMap
  needsCommit <- unwrap ordMap
  destroyers <- unwrap accumulator
  let
    destroyDownstream =
      join destroyers.reset <* mailboxes.reset
  { sources, unsubscribe } <- runEffectFn1 upstream
    { receive: mkEffectFn1 \{ key, value } -> unwrap do
        mailboxes.onKey key \downstream -> do
          _ <- needsCommit.set key downstream.commit
          Allocar do runEffectFn1 downstream.send value
    , commit: mkEffectFn1 \id -> do
        prev <- unwrap needsCommit.reset
        for_ prev \commit -> do
          runEffectFn1 commit id
    , destroyed: destroyDownstream
    }
  byKey <- unwrap $ allocLazy $ pure $ AllocateFrom do
    mkEffectFn1 \selected -> unwrap do
      downstream <- createStream'
      Allocar do runEffectFn1 (unwrap destroyers.put) downstream.destroy
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
    write = mkEffectFn1 \a -> void $ liftST (STRef.write (Just a) ref)
    downstream = unsafeMarkHot do
      makeStream \k -> do
        Allocar do liftST (STRef.read ref) >>= traverse_ (runEffectFn1 k)
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
  pure $ AllocateFrom do
    mkEffectFn1 \k -> do
      existing <- liftST (STO.peek k interfaces)
      case existing of
        Just r -> pure r
        Nothing -> do
          io <- unwrap createBehavioral
          io <$ liftST (STO.poke k io interfaces)
