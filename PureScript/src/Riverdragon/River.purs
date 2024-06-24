module Riverdragon.River where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as STRef
import Control.Plus (class Plus, empty)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, runEffectFn1)
import Foreign.Object.ST as STO
import Idiolect (type (-!>))
import Prim.Boolean (False, True)
import Riverdragon.River.Bed (type (-&>), Allocar(..), AllocateFrom(..), accumulator, allocLazy, cleanup, globalId, ordMap, subscriptions, threshold)
import Safe.Coerce (coerce)

type Id = Int

-- | Transactions, especially for event deduplication (DAG).
-- | Actor model? “I changed” vs “you changed”?
-- | Errors/stream cancelation like RxJS?
data Event (flow :: Boolean) a = Event (Conj Boolean)
  ( { receive :: a -!> Unit, commit :: Id -!> Unit, destroyed :: Allocar Unit } -&>
    { sources :: Array Id, unsubscribe :: Allocar Unit }
  )

instance functorEvent :: Functor (Event flow) where
  map f (Event t (AllocateFrom g)) = Event t do
    AllocateFrom do
      mkEffectFn1 \cbs ->
        runEffectFn1 g do
          cbs { receive = mkEffectFn1 \a -> runEffectFn1 cbs.receive (f a) }
-- | The `alt` of two events forwards all data and commits, and waits until
-- | both are destroyed to destroy downstream.
instance altEvent :: Alt (Event flow) where
  alt (Event t1 (AllocateFrom e1)) (Event t2 (AllocateFrom e2)) = Event (t1 <> t2) do
    AllocateFrom do
      mkEffectFn1 \cbs -> unwrap do
        -- only run it once both have been destroyed
        destroyed <- threshold 2 cbs.destroyed
        -- and only count each once (just in case)
        cbs1 <- cbs { destroyed = _ } <$> cleanup destroyed
        cbs2 <- cbs { destroyed = _ } <$> cleanup destroyed
        Allocar do runEffectFn1 e1 cbs1 <> runEffectFn1 e2 cbs2
-- | The `empty` event destroys itself immediately upon subscription.
instance plusEvent :: Plus (Event flow) where
  empty = Event mempty do
    AllocateFrom do
      mkEffectFn1 \cbs -> do
        unwrap cbs.destroyed
        pure { sources: mempty, unsubscribe: mempty }
-- instance applyEvent :: Apply Event where
--   apply (Event r1) (Event r2) = Event
--     { sources: r1.sources <> r2.sources
--     , subscribe: AllocateFrom do
--         mkEffectFn1 \cb -> do
--           ?help
--     }
-- instance applicativeEvent :: Applicative Event where
--   pure a = Event
--     { sources: []
--     , subscribe: AllocateFrom do
--         mkEffectFn1 \cb -> do
--           mempty <$ runEffectFn1 cb a
--     }

-- inheritCommitFrom :: forall a b. Event a -> Event b -> Event b
-- inheritCommitFrom (Event (AllocateFrom e1)) (Event (AllocateFrom e2)) =
--   Event do
--     AllocateFrom do
--       mkEffectFn1 \cbs -> do
--         e1


-- | Create a (hot) event as a message channel, that sends any message to all of
-- | its subscribers (and then commits).
createEvent :: forall flow a. Allocar { send :: a -!> Unit, event :: Event flow a, destroy :: Allocar Unit }
createEvent = do
  id <- globalId
  { push: AllocateFrom push, notify, destroy: AllocateFrom destroy } <- subscriptions
  pure
    { send: mkEffectFn1 \a -> do
        runEffectFn1 notify do
          mkEffectFn1 \{ receive } -> runEffectFn1 receive a
        runEffectFn1 notify do
          mkEffectFn1 \{ commit } -> runEffectFn1 commit id
    , event: Event mempty $ AllocateFrom $ mkEffectFn1 \cbs -> do
        runEffectFn1 push cbs <#> { sources: [id], unsubscribe: _ }
    , destroy: Allocar do
        runEffectFn1 destroy do
          mkEffectFn1 \{ destroyed: Allocar destroyed } -> destroyed
    }

createEvent' :: forall flow a. Allocar { send :: a -!> Unit, commit :: Id -!> Unit, event :: Array Id -> Event flow a, destroy :: Allocar Unit }
createEvent' = do
  { push: AllocateFrom push, notify, destroy: AllocateFrom destroy } <- subscriptions
  pure
    { send: mkEffectFn1 \a -> do
        runEffectFn1 notify do
          mkEffectFn1 \{ receive } -> runEffectFn1 receive a
    , commit: mkEffectFn1 \id -> do
        runEffectFn1 notify do
          mkEffectFn1 \{ commit } -> runEffectFn1 commit id
    , event: \sources -> Event mempty $ AllocateFrom $ mkEffectFn1 \cbs -> do
        runEffectFn1 push cbs <#> { sources, unsubscribe: _ }
    , destroy: Allocar do
        runEffectFn1 destroy do
          mkEffectFn1 \{ destroyed: Allocar destroyed } -> destroyed
    }

-- | Make a (cold) event that gets allocated per subscriber, e.g. for listeners
-- | or timeouts or the like.
makeEvent :: forall a. ((a -!> Unit) -> Allocar (Allocar Unit)) -> Event False a
makeEvent eventTemplate = Event (Conj false) do
  AllocateFrom do
    mkEffectFn1 \cbs -> do
      id <- unwrap globalId
      unsubscribe <- coerce eventTemplate do
        mkEffectFn1 \a -> do
          runEffectFn1 cbs.receive a
          runEffectFn1 cbs.commit id
      pure { sources: [id], unsubscribe }

-- | Unsafe.
unsafeMarkHot :: forall flowIn flowOut a. Event flowIn a -> Event flowOut a
unsafeMarkHot (Event _ f) = Event (Conj true) f

chill :: forall flowIn a. Event flowIn a -> Event False a
chill (Event _ f) = Event (Conj false) f

-- | Subscribe to an event with a specific callback. Returns an unsubscribe
-- | procedure (`Allocar Unit`).
subscribe :: forall flow a. Event flow a -> (a -!> Unit) -> Allocar (Allocar Unit)
subscribe (Event _ (AllocateFrom event)) receive = coerce do
  _.unsubscribe <$> runEffectFn1 event { receive, commit: mempty, destroyed: mempty }

-- | Create a single subscriber to an upstream event that broadcasts it to
-- | multiple downstream subscribers at once. This creation is effectful because
-- | it creates a subscriber, thus starting some effects.
instantiate :: forall flowIn flowOut a. Event flowIn a -> Allocar { event :: Event flowOut a, destroy :: Allocar Unit }
instantiate (Event _ (AllocateFrom event)) = Allocar do
  { send, commit, event: eventDependingOn, destroy } <- unwrap createEvent'
  { sources, unsubscribe } <-
    runEffectFn1 event { receive: send, commit, destroyed: coerce destroy }
  pure { event: eventDependingOn sources, destroy: unsubscribe <> destroy }

-- the reason i created allocar tbh
mailbox :: forall flowIn flowOut k v. Ord k =>
  Event flowIn { key :: k, value :: v } ->
  Allocar { byKey :: k -> Event flowOut v, destroy :: Allocar Unit }
mailbox (Event _ (AllocateFrom upstream)) = Allocar do
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
      downstream <- createEvent'
      Allocar do runEffectFn1 (unwrap destroyers.put) downstream.destroy
      _ <- mailboxes.set selected downstream
      pure $ downstream.event sources
  pure { byKey, destroy: unsubscribe <> destroyDownstream }


-- | An interface to a mutable value, stored in one location.
type Interface a =
  { send :: a -!> Unit
  , receive :: Event True a
  , loopback :: Event True a
  , mailbox :: a -> Event True Unit
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
  upstream <- createEvent
  -- destroyed by upstream.destroy
  m <- mailbox (map { key: _, value: unit } upstream.event)
  let
    write = mkEffectFn1 \a -> void $ liftST (STRef.write (Just a) ref)
    downstream = unsafeMarkHot do
      makeEvent \k -> do
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
