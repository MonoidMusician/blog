-- | This module is all about helpers for managing the lifecycles of variables
-- | with various semantics (replacement, accumulation, thresholds, and so on).
-- |
-- | It is sort of “OOP but done better”: each allocation of a variable runs in
-- | `Allocar` and returns a bunch of instantiated methods encapsulated in a
-- | record. It offers great abstraction, no manual handling of refs and such.
-- |
-- | It actually does a decent job of being inlined by the backend-optimizer,
-- | with some help from inlining directives. It does not quite *look* like
-- | something a dev would write (there are spurious constant declarations),
-- | but it should perform similarly or identically.
-- |
-- | It is just really cute and nice and convenient!
--
-- @inline export loadingBurst arity=1
-- @inline export storeLast always
-- @inline export ordMap arity=1
-- @inline export ordSet arity=1
-- @inline export loading arity=1
-- @inline export accumulator arity=1
-- @inline export threshold arity=2
-- @inline export rolling always
-- @inline export subscriptions always
-- @inline export cleanup' arity=1
-- @inline export cleanup arity=1
module Riverdragon.River.Bed where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as STR
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STA
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Safe.Coerce (coerce)
import Web.Event.Event as Web
import Web.Event.EventPhase (EventPhase(..))
import Web.Event.EventTarget (EventTarget)
import Web.Event.EventTarget as Event


-- | Benign effects of allocation and deallocation.
-- | (Originally implemented as a newtype, but the syntactic noise was
-- | unbearable.)
type Allocar = Effect

unsafeAllocate :: forall a. Allocar a -> a
unsafeAllocate = coerce (unsafePerformEffect :: Effect a -> a)

type AllocateFrom a b = a -> Effect b
infixr 1 type AllocateFrom as -&>
infixr 1 type AllocateFrom as -!>

unsafeAllocateFrom :: forall a b. AllocateFrom a b -> a -> b
unsafeAllocateFrom f i = unsafePerformEffect (f i)

-- | Promise that you could have allocated the resources ahead of time, though
-- | it is more convenient/efficient to do it lazily.
allocLazy :: forall i o. Allocar (i -&> o) -> Allocar (i -> o)
allocLazy = map unsafeAllocateFrom


-- | Meant for allocating fresh ints or other sequential data. Does not support
-- | deallocation.
allocRefLoop :: forall s o.
  { start :: s
  , incr :: Int -> s -> s
  , out :: Int -> s -> o
  } ->
  Allocar (Allocar o)
allocRefLoop spec = do
  Ref.new (0 /\ spec.start) <#> Ref.modify'
    \s -> { value: uncurry spec.out s, state: (add 1 <<< fst &&& uncurry spec.incr) s }

freshId :: Allocar (Allocar Int)
freshId = allocRefLoop { start: unit, incr: mempty, out: const }

-- Marked unsafe for Erlang
globalId :: Allocar Int
globalId = unsafeAllocate freshId

whenMM :: forall m a. Monad m => Monoid a => m Boolean -> m a -> m a
whenMM c e = c >>= if _ then e else pure mempty

whenRef :: forall s m a. MonadST s m => STRef.STRef s a -> (a -> Boolean) -> m Unit -> m Unit
whenRef ref pred = whenM (pred <$> liftST (STRef.read ref))

iteM :: forall m r. Monad m => m Boolean -> m r -> m r -> m r
iteM pred ifTrue ifFalse =
  pred >>= if _ then ifTrue else ifFalse

iteRef :: forall s m a r. MonadST s m => STRef.STRef s a -> (a -> Boolean) -> m r -> m r -> m r
iteRef ref pred ifTrue ifFalse =
  (pred <$> liftST (STRef.read ref)) >>= if _ then ifTrue else ifFalse


-- | Cleanup means it only runs once!
cleanup' :: Allocar Unit -> Allocar
  { cleanup :: Allocar Unit
  , running :: Allocar Boolean
  }
cleanup' act = do
  needsToRun <- liftST do STR.new true
  pure
    { cleanup: whenRef needsToRun identity do
        _ <- liftST do STR.write false needsToRun
        act
    , running: liftST do STR.read needsToRun
    }

cleanup :: Allocar Unit -> Allocar (Allocar Unit)
cleanup = cleanup' >>> map _.cleanup

-- | Runs the last action each time a new action is set. Useful for when you are
-- | replacing subscriptions or swapping out other resources.
rolling :: Allocar (Allocar Unit -> Allocar Unit)
rolling = do
  past <- liftST do STR.new mempty
  pure \next -> do
    act <- liftST do STR.read past
    _ <- liftST do STR.write mempty past
    act
    _ <- liftST do STR.write next past
    pure unit

cleanupFn' :: forall d.
  (d -&> Unit) -> Allocar
  { cleanup :: d -&> Unit
  , running :: Allocar Boolean
  , finished :: Allocar (Maybe d)
  }
cleanupFn' act = do
  cleanupArg <- liftST do STR.new Nothing
  let
    shouldRun = isNothing <$> liftST do STR.read cleanupArg
  pure
    { cleanup: \d -> do
        whenM shouldRun do
          _ <- liftST do STR.write (Just d) cleanupArg
          act d
    , running: isNothing <$> liftST do STR.read cleanupArg
    , finished: liftST do STR.read cleanupArg
    }


-- | Act as a circuit breaker: normally pass events through, but disable and
-- | re-enable flow as necessary.
breaker :: forall d.
  (d -!> Unit) -> Allocar
  { run :: d -!> Unit
  , trip :: Allocar Unit
  , reset :: Allocar Unit
  , running :: Allocar Boolean
  }
breaker act = do
  needsToRun <- liftST do STR.new true
  pure
    { run: whenRef needsToRun identity <<< act
    , trip: void do liftST do STR.write false needsToRun
    , reset: void do liftST do STR.write true needsToRun
    , running: liftST do STR.read needsToRun
    }



-- | Track subscriptions. When you add a subscription, you get back a
-- | `Allocar Unit` procedure to remove it. You may traverse all existing
-- | subscriptions with `notify` and you may shut down the subscriptions (and
-- | refuse new ones) by calling `destroy`, which also lets you traverse them
-- | one last time to inform them of the loss.
subscriptions :: forall o. Allocar
  { push :: o -&> Allocar Unit
  , notify :: (o -!> Unit) -!> Unit
  , destroy :: (o -!> Unit) -&> Unit
  }
subscriptions = do
  listenersST <- liftST STA.new
  ids <- freshId
  { cleanup: destroy, running } <- cleanupFn' \onDestroy -> do
    -- sorry
    len <- liftST do STA.pushAll [] listenersST
    destroyed <- liftST do STA.splice 0 len [] listenersST
    foreachE destroyed \(_ /\ o) -> onDestroy o
    pure unit
  pure
    { notify: \cb -> do
        -- Freeze so we do not modify the array as we are iterating over it?
        -- I suppose we could catch new ones ... idk.
        listeners <- liftST do STA.freeze listenersST
        foreachE listeners \(_ /\ o) -> cb o
    , push: \o -> whenMM running do
        id <- ids
        _ <- liftST do STA.push (id /\ o) listenersST
        cleanup $ liftST do
          listeners <- STA.freeze listenersST
          case Array.findIndex (fst >>> eq id) listeners of
            Nothing -> pure unit -- ??
            Just i -> void do
              STA.splice i 1 [] listenersST
    , destroy
    }

-- | Register an event listener on a target. Returns a cleanup procedure.
eventListener ::
  { eventType :: Web.EventType
  , eventPhase :: EventPhase
  , eventTarget :: EventTarget
  } ->
  (Web.Event -> Effect Unit) -&> Allocar Unit
eventListener { eventType, eventPhase, eventTarget } cb = do
  let capture = eventPhase == Capturing
  listener <- Event.eventListener cb
  Event.addEventListener eventType listener capture eventTarget $>
    Event.removeEventListener eventType listener capture eventTarget


-- | Allocate an accumulator, that always adds in from the monoid. Useful for
-- | accumulating cleanup procedures, for example.
accumulator :: forall m. Monoid m => Allocar { read :: Allocar m, put :: m -&> Unit, reset :: Allocar m }
accumulator = liftST (STRef.new mempty) <#> \acc ->
  { read: liftST do STRef.read acc
  , reset: liftST do STRef.read acc <* STRef.write mempty acc
  , put: \m -> do
      _ <- liftST do STRef.modify (_ <> m) acc
      pure unit
  }

-- | Manage a “mutable” `Data.Map`, by manipulating specific keys and running
-- | functions on all keys currently in the map. All methods return the previous
-- | value where that makes sense (`set`, `remove`, `reset`).
ordMap :: forall k v. Ord k => Allocar
  { read :: Allocar (Map k v)
  , set :: k -> v -> Allocar (Maybe v)
  , get :: k -> Allocar (Maybe v)
  , remove :: k -> Allocar (Maybe v)
  , traverse :: (k -> v -> Allocar Unit) -> Allocar Unit
  , onKey :: k -> (v -> Allocar Unit) -> Allocar Unit
  , reset :: Allocar (Map k v)
  -- , destroy :: Allocar (Map k v)
  }
ordMap = liftST (STRef.new Map.empty) <#> \ref ->
  { read: liftST do STRef.read ref
  , set: \k v -> liftST do STRef.modify' (\m -> { value: Map.lookup k m, state: Map.insert k v m }) ref
  , get: \k -> liftST do Map.lookup k <$> STRef.read ref
  , remove: \k -> liftST do STRef.modify' (\m -> { value: Map.lookup k m, state: Map.delete k m }) ref
  , traverse: \f -> (liftST do STRef.read ref) >>= traverseWithIndex_ f
  , onKey: \k f -> do
      mv <- liftST do Map.lookup k <$> STRef.read ref
      case mv of
        Just v -> f v
        Nothing -> pure unit
  , reset: liftST do STRef.modify' (\value -> { value, state: Map.empty }) ref
  }

ordSet :: forall k. Ord k => Allocar
  { read :: Allocar (Set k)
  , set :: k -> Allocar Boolean
  , get :: k -> Allocar Boolean
  , remove :: k -> Allocar Boolean
  , traverse :: (k -> Allocar Unit) -> Allocar Unit
  , whenPresent :: k -> Allocar Unit -> Allocar Unit
  , reset :: Allocar (Set k)
  -- , destroy :: Allocar (Set k)
  }
ordSet = liftST (STRef.new Set.empty) <#> \ref ->
  { read: liftST do STRef.read ref
  , set: \k -> liftST do STRef.modify' (\m -> { value: Set.member k m, state: Set.insert k m }) ref
  , get: \k -> liftST do Set.member k <$> STRef.read ref
  , remove: \k -> liftST do STRef.modify' (\m -> { value: Set.member k m, state: Set.delete k m }) ref
  , traverse: \f -> (liftST do STRef.read ref) >>= traverse_ f
  , whenPresent: \k f -> do
      mv <- liftST do Set.member k <$> STRef.read ref
      case mv of
        true -> f
        false -> pure unit
  , reset: liftST do STRef.modify' (\value -> { value, state: Set.empty }) ref
  }

-- | Only run it on the nth time.
threshold :: Int -> Allocar Unit -> Allocar (Allocar Unit)
threshold 0 act = act $> mempty
threshold n act = liftST (STRef.new n) <#> \count -> do
  _ <- liftST do STRef.modify (_ - 1) count
  whenRef count (eq 0) act

data Threshold a = BeforeTsh | AtTsh a | AfterTsh
derive instance eqThreshold :: Eq a => Eq (Threshold a)
derive instance ordThreshold :: Ord a => Ord (Threshold a)

threshold' :: forall r. Int -> Allocar r -> Allocar (Allocar (Threshold r))
threshold' 0 act = act $> pure AfterTsh
threshold' n act = liftST (STRef.new n) <#> \count -> do
  _ <- liftST do STRef.modify (_ - 1) count
  left <- liftST do STRef.read count
  case compare left 0 of
    LT -> pure AfterTsh
    EQ -> AtTsh <$> act
    GT -> pure BeforeTsh

-- | A push array, stored using a snoc list under the hood.
pushArray :: forall a. Allocar
  { read :: Allocar (Array a)
  , push :: a -> Allocar Unit
  , reset :: Allocar (Array a)
  -- , destroy :: Allocar (Array a)
  }
pushArray = liftST (STRef.new List.Nil) <#> \acc ->
  { read: Array.reverse <<< List.toUnfoldable <$> liftST (STRef.read acc)
  , push: \a -> void $ liftST (STRef.modify (List.Cons a) acc)
  , reset: Array.reverse <<< List.toUnfoldable <$> liftST (STRef.read acc <* STRef.write List.Nil acc)
  }

-- | A single mutable cell, initialized with the specified default value.
prealloc :: forall a. a -> Allocar { get :: Allocar a, set :: a -&> a }
prealloc default =
  liftST do
    STRef.new default <#> \ref ->
      { get: liftST do STRef.read ref
      , set: \v -> liftST do
          STRef.read ref <* STRef.write v ref
      }

prealloc2 :: forall a b. a -> b -> Allocar { get :: Allocar (Tuple a b), setL :: a -&> a, setR :: b -&> b }
prealloc2 defaultL defaultR =
  liftST ado
    refL <- STRef.new defaultL
    refR <- STRef.new defaultR
    in
      { get: liftST do Tuple <$> STRef.read refL <*> STRef.read refR
      , setL: \v -> liftST do
          STRef.read refL <* STRef.write v refL
      , setR: \v -> liftST do
          STRef.read refR <* STRef.write v refR
      }

-- | Just a single memory cell storing `Just` the most recent value, or
-- | `Nothing` at the start.
storeLast :: forall a. Allocar
  { get :: Allocar (Maybe a)
  , run :: (a -!> Unit) -!> Unit
  , set :: a -&> Maybe a
  }
storeLast =
  liftST do
    STRef.new Nothing <#> \ref ->
      { get: liftST do STRef.read ref
      , run: \f -> liftST (STRef.read ref) >>= traverse_ f
      , set: \v -> liftST do
          STRef.read ref <* STRef.write (Just v) ref
      }

-- | The variable is only `true` for the lifetime of the function.
loading :: forall r. (Allocar Boolean -> Allocar r) -> Allocar r
loading f = do
  isLoading <- liftST (STRef.new true)
  r <- f (liftST (STRef.read isLoading))
  _ <- liftST (STRef.write false isLoading)
  pure r

-- | Capture an array of values during the initial loading, otherwise let the
-- | `Allocar Unit` callback handle them. Used for the `burst` field of streams.
loadingBurst ::
  forall a r.
  ((a -> Allocar Unit -> Allocar Unit) -> Allocar (Array a -> r)) ->
  Allocar r
loadingBurst f = do
  bursts <- pushArray
  r <- loading \isLoading -> f \a whenDone ->
    iteM isLoading (bursts.push a) whenDone
  burst <- bursts.reset
  pure (r burst)
