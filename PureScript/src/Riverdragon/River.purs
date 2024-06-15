module Riverdragon.River where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as STRef
import Control.Plus (class Plus, empty)
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, runEffectFn1)
import Foreign.Object.ST as STO
import Idiolect (type (-!>))
import Node.Stream (destroy)
import Riverdragon.River.Bed (type (-&>), Allocar(..), AllocateFrom(..), accumulator, allocLazy, globalId, subscriptions)
import Safe.Coerce (coerce)

type Id = Int

-- | Transactions, especially for event deduplication (DAG).
-- | Actor model? “I changed” vs “you changed”?
-- | Errors/stream cancelation like RxJS?
newtype Event a = Event
  ( { receive :: a -!> Unit, commit :: Effect Unit, destroyed :: Allocar Unit } -&>
    { sources :: Array Id, unsubscribe :: Allocar Unit }
  )

instance functorEvent :: Functor Event where
  map f (Event (AllocateFrom g)) = Event do
    AllocateFrom do
      mkEffectFn1 \cbs ->
        runEffectFn1 g do
          cbs { receive = mkEffectFn1 \a -> runEffectFn1 cbs.receive (f a) }
instance altEvent :: Alt Event where
  alt (Event r1) (Event r2) = Event (r1 <> r2)
instance plusEvent :: Plus Event where
  empty = Event mempty
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

-- | Create a (hot) event as a message channel, that sends any message to all of
-- | its subscribers (and then commits).
createEvent :: forall a. Allocar { send :: a -!> Unit, event :: Event a, destroy :: Allocar Unit }
createEvent = createEvent' <#> \r ->
  { send: mkEffectFn1 \a -> do
      runEffectFn1 r.send a
      r.commit
  , event: r.event []
  , destroy: r.destroy
  }

createEvent' :: forall a. Allocar { send :: a -!> Unit, commit :: Effect Unit, event :: Array Id -> Event a, destroy :: Allocar Unit }
createEvent' = do
  id <- globalId
  { push: AllocateFrom push, notify, destroy: AllocateFrom destroy } <- subscriptions
  pure
    { send: mkEffectFn1 \a -> do
        runEffectFn1 notify do
          mkEffectFn1 \{ receive } -> runEffectFn1 receive a
    , commit:
        runEffectFn1 notify do
          mkEffectFn1 \{ commit } -> commit
    , event: \moreSources -> Event $ AllocateFrom $ mkEffectFn1 \cbs -> do
        runEffectFn1 push cbs <#> { sources: moreSources <> [id], unsubscribe: _ }
    , destroy: Allocar do
        runEffectFn1 destroy do
          mkEffectFn1 \{ destroyed: Allocar destroyed } -> destroyed
    }

-- | Make a (cold) event that gets allocated per subscriber, e.g. for listeners
-- | or timeouts or the like.
makeEvent :: forall a. ((a -!> Unit) -> Allocar (Allocar Unit)) -> Event a
makeEvent eventTemplate = Event do
  AllocateFrom do
    mkEffectFn1 \cbs -> do
      id <- unwrap globalId
      unsubscribe <- coerce eventTemplate do
        mkEffectFn1 \a -> do
          runEffectFn1 cbs.receive a
          cbs.commit
      pure { sources: [id], unsubscribe }

-- | Subscribe to an event with a specific callback. Returns an unsubscribe
-- | procedure (`Allocar Unit`).
subscribe :: forall a. Event a -> (a -!> Unit) -> Allocar (Allocar Unit)
subscribe (Event (AllocateFrom event)) receive = coerce do
  _.unsubscribe <$> runEffectFn1 event { receive, commit: mempty, destroyed: mempty }

-- | Create a single subscriber to an upstream event that broadcasts it to
-- | multiple downstream subscribers at once. This creation is effectful because
-- | it creates a subscriber, thus starting some effects.
instantiate :: forall a. Event a -> Allocar { event :: Event a, destroy :: Allocar Unit }
instantiate (Event (AllocateFrom event)) = do
  { send, commit, event: eventDependingOn, destroy } <- createEvent'
  { sources, unsubscribe } <- Allocar do
    runEffectFn1 event { receive: send, commit, destroyed: coerce destroy }
  pure { event: eventDependingOn sources, destroy: unsubscribe <> destroy }

-- the reason i created allocar tbh
-- mailbox :: forall k v. Event { key :: k, value :: v } -> Allocar { byKey :: k -> Event v, destroy :: Allocar Unit }
-- mailbox (Event (AllocateFrom upstream)) = do
--   mailboxes <- liftST do STRef.new Map.empty
--   needsCommit <- liftST do STRef.new Map.empty
--   destroyers <- accumulator
--   { sources, unsubscribe } <- Allocar do
--     runEffectFn1 upstream
--       { receive: mempty
--       , commit: mempty
--       , destroyed: join destroyers.read
--       }
--   byKey <- allocLazy $ pure $ AllocateFrom do
--     mkEffectFn1 \selected -> unwrap do
--       downstream <- createEvent'
--       Allocar do runEffectFn1 (unwrap destroyers.put) downstream.destroy
--       ?help
--   pure { byKey, destroy: unsubscribe }


-- | An interface to a mutable value, stored in one location.
type Interface a =
  { send :: a -!> Unit
  , receive :: Event a
  , loopback :: Event a
  , mailbox :: Allocar (a -> Event Unit)
  , current :: Effect (Maybe a)
  }
disconnected :: forall a. Interface a
disconnected =
  { send: mempty
  , receive: empty
  , loopback: empty
  , mailbox: pure (const empty)
  , current: pure Nothing
  }

-- Each new subscription gets the latest value immediately.
createBehavioral :: forall a. Ord a => Allocar (Interface a)
createBehavioral = do
  ref <- liftST do STRef.new Nothing
  upstream <- createEvent
  let
    write = mkEffectFn1 \a -> void $ liftST (STRef.write (Just a) ref)
    downstream = makeEvent \k -> do
      Allocar do liftST (STRef.read ref) >>= traverse_ (runEffectFn1 k)
      subscribe upstream.event k
  pure
    { send: write <> upstream.send
    , receive: downstream
    , loopback: downstream
    , mailbox: pure (const empty) -- mailboxed (map {payload: unit, address: _} downstream) identity
    , current: liftST (STRef.read ref)
    }


-- type KeyedInterface = String -> Interface Json
-- We adjoin integers for tracking sources so we do not send to the source
-- it came from
-- type DataShare = STObject Global (Int /\ (String -> Interface (Int /\ Json)))

-- makeKeyedInterface :: forall a. Ord a => Allocar (String -> Interface a)
-- makeKeyedInterface = allocLazy do
--   interfaces <- liftST STO.new
--   pure $ AllocateFrom do
--     mkEffectFn1 \k -> do
--       existing <- liftST (STO.peek k interfaces)
--       case existing of
--         Just r -> pure r
--         Nothing -> do
--           io <- createBehavioral
--           io <$ liftST (STO.poke k io interfaces)
