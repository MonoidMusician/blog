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
-- @inline export cleanupFn' arity=1
-- @inline export prealloc arity=1
-- @inline export prealloc2 arity=2
-- @inline export storeUnsubscriber always
-- @inline export lazyAlloc arity=1
module Riverdragon.River.Bed ( module Riverdragon.River.Bed, module ReExports ) where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal (STRef)
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
import Idiolect (type (-!>))
import Idiolect (type (-!>)) as ReExports
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

whenRef :: forall s m a. MonadST s m => STRef s a -> (a -> Boolean) -> m Unit -> m Unit
whenRef ref pred = whenM (liftST do pred <$> STRef.read ref)

iteM :: forall m r. Monad m => m Boolean -> m r -> m r -> m r
iteM pred ifTrue ifFalse =
  pred >>= if _ then ifTrue else ifFalse

iteRef :: forall s m a r. MonadST s m => STRef s a -> (a -> Boolean) -> m r -> m r -> m r
iteRef ref pred ifTrue ifFalse =
  (liftST do pred <$> STRef.read ref) >>= if _ then ifTrue else ifFalse

newSTR :: forall v. v -> Allocar (STRef Global v)
newSTR v = liftST do STRef.new v
getSTR :: forall v. STRef Global v -> Allocar v
getSTR r = liftST do STRef.read r
runSTR :: forall v. STRef Global (Allocar v) -> Allocar v
runSTR r = join (getSTR r)
setSTR :: forall v. STRef Global v -> v -> Allocar Unit
setSTR r v = liftST do void do STRef.write v r
infix 1 setSTR as &=
swapSTR :: forall v. STRef Global v -> v -> Allocar v
swapSTR r v = liftST do STRef.modify' (\value -> { state: v, value }) r
infix 1 swapSTR as <&=
modifySTR :: forall v. STRef Global v -> (v -> v) -> Allocar Unit
modifySTR r fn = liftST do void do STRef.modify fn r
infix 1 modifySTR as &~
swapifySTR :: forall v. STRef Global v -> (v -> v) -> Allocar Unit
swapifySTR r fn = liftST do
  void do STRef.modify' (\value -> { state: fn value, value }) r
infix 1 swapifySTR as <&~

-- | Cleanup means it only runs once!
cleanup' :: Allocar Unit -> Allocar
  { cleanup :: Allocar Unit
  , running :: Allocar Boolean
  }
cleanup' act = do
  needsToRun <- newSTR true
  pure
    { cleanup: whenRef needsToRun identity do
        needsToRun &= false
        act
    , running: getSTR needsToRun
    }

cleanup :: Allocar Unit -> Allocar (Allocar Unit)
cleanup = cleanup' >>> map _.cleanup

-- | Runs the last action each time a new action is set. Useful for when you are
-- | replacing subscriptions or swapping out other resources.
rolling :: Allocar (Allocar Unit -> Allocar Unit)
rolling = do
  past <- newSTR mempty
  pure \next -> do
    act <- getSTR past
    past &= mempty
    act
    past &= next
    pure unit

cleanupFn' :: forall d.
  (d -&> Unit) -> Allocar
  { cleanup :: d -&> Unit
  , running :: Allocar Boolean
  , finished :: Allocar (Maybe d)
  }
cleanupFn' act = do
  cleanupArg <- newSTR Nothing
  let
    shouldRun = isNothing <$> getSTR cleanupArg
  pure
    { cleanup: \d -> do
        whenM shouldRun do
          cleanupArg &= Just d
          act d
    , running: isNothing <$> getSTR cleanupArg
    , finished: getSTR cleanupArg
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
  needsToRun <- newSTR true
  pure
    { run: whenRef needsToRun identity <<< act
    , trip: needsToRun &= false
    , reset: needsToRun &= true
    , running: getSTR needsToRun
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
  , size :: Allocar Int
  , running :: Allocar Boolean
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
    , size: liftST do STA.pushAll [] listenersST
    , running
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
accumulator :: forall m. Monoid m => Allocar { get :: Allocar m, put :: m -&> Unit, reset :: Allocar m }
accumulator = newSTR mempty <#> \acc ->
  { get: getSTR acc
  , reset: acc <&= mempty
  , put: \m -> acc &~ (_ <> m)
  }

-- | Manage a “mutable” `Data.Map`, by manipulating specific keys and running
-- | functions on all keys currently in the map. All methods return the previous
-- | value where that makes sense (`set`, `remove`, `reset`).
ordMap :: forall k v. Ord k => Allocar
  { read :: Allocar (Map k v)
  , set :: k -> v -> Allocar Unit
  , swap :: k -> v -> Allocar (Maybe v)
  , get :: k -> Allocar (Maybe v)
  , remove :: k -> Allocar (Maybe v)
  , traverse :: (k -> v -> Allocar Unit) -> Allocar Unit
  , onKey :: k -> (v -> Allocar Unit) -> Allocar Unit
  , reset :: Allocar (Map k v)
  -- , destroy :: Allocar (Map k v)
  }
ordMap = newSTR Map.empty <#> \ref ->
  { read: getSTR ref
  , set: \k v -> ref &~ Map.insert k v
  , swap: \k v -> liftST do STRef.modify' (\m -> { value: Map.lookup k m, state: Map.insert k v m }) ref
  , get: \k -> Map.lookup k <$> getSTR ref
  , remove: \k -> liftST do STRef.modify' (\m -> { value: Map.lookup k m, state: Map.delete k m }) ref
  , traverse: \f -> getSTR ref >>= traverseWithIndex_ f
  , onKey: \k f -> do
      mv <- Map.lookup k <$> getSTR ref
      case mv of
        Just v -> f v
        Nothing -> pure unit
  , reset: ref <&= Map.empty
  }

ordSet :: forall k. Ord k => Allocar
  { read :: Allocar (Set k)
  , set :: k -> Allocar Unit
  , swap :: k -> Allocar Boolean
  , get :: k -> Allocar Boolean
  , remove :: k -> Allocar Boolean
  , traverse :: (k -> Allocar Unit) -> Allocar Unit
  , whenPresent :: k -> Allocar Unit -> Allocar Unit
  , reset :: Allocar (Set k)
  -- , destroy :: Allocar (Set k)
  }
ordSet = newSTR Set.empty <#> \ref ->
  { read: getSTR ref
  , set: \k -> ref &~ Set.insert k
  , swap: \k -> liftST do STRef.modify' (\m -> { value: Set.member k m, state: Set.insert k m }) ref
  , get: \k -> Set.member k <$> getSTR ref
  , remove: \k -> liftST do STRef.modify' (\m -> { value: Set.member k m, state: Set.delete k m }) ref
  , traverse: \f -> getSTR ref >>= traverse_ f
  , whenPresent: \k f -> do
      mv <- Set.member k <$> getSTR ref
      case mv of
        true -> f
        false -> pure unit
  , reset: ref <&= Set.empty
  }

-- | Only run it on the nth time.
threshold :: Int -> Allocar Unit -> Allocar (Allocar Unit)
threshold 0 act = act $> mempty
threshold n act = newSTR n <#> \count -> do
  count &~ (_ - 1)
  whenRef count (eq 0) act

data Threshold a = BeforeTsh | AtTsh a | AfterTsh
derive instance eqThreshold :: Eq a => Eq (Threshold a)
derive instance ordThreshold :: Ord a => Ord (Threshold a)

threshold' :: forall r. Int -> Allocar r -> Allocar (Allocar (Threshold r))
threshold' 0 act = act $> pure AfterTsh
threshold' n act = newSTR n <#> \count -> do
  count &~ (_ - 1)
  left <- getSTR count
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
pushArray = newSTR List.Nil <#> \acc ->
  { read: Array.reverse <<< List.toUnfoldable <$> getSTR acc
  , push: \a -> acc &~ List.Cons a
  , reset: Array.reverse <<< List.toUnfoldable <$> swapSTR acc List.Nil
  }

-- | A single mutable cell, initialized with the specified default value.
prealloc :: forall a. a -> Allocar
  { get :: Allocar a
  , set :: a -&> Unit
  , swap :: a -&> a
  }
prealloc default = newSTR default <#> \ref ->
  { get: getSTR ref
  , set: setSTR ref
  , swap: swapSTR ref
  }

prealloc2 :: forall a b. a -> b -> Allocar
  { get :: Allocar (Tuple a b)
  , setL :: a -&> Unit
  , setR :: b -&> Unit
  , swapL :: a -&> a
  , swapR :: b -&> b
  }
prealloc2 defaultL defaultR =
  ado
    refL <- newSTR defaultL
    refR <- newSTR defaultR
    in
      { get: Tuple <$> getSTR refL <*> getSTR refR
      , setL: \v -> setSTR refL v
      , setR: \v -> setSTR refR v
      , swapL: \v -> swapSTR refL v
      , swapR: \v -> swapSTR refR v
      }

lazyAlloc :: forall a. Allocar a -> Allocar (Allocar a)
lazyAlloc how = prealloc Nothing <#> \alloced ->
  alloced.get >>= case _ of
    Nothing -> do
      created <- how
      alloced.set (Just created)
      pure created
    Just created -> pure created

-- | Just a single memory cell storing `Just` the most recent value, or
-- | `Nothing` at the start.
storeLast :: forall a. Allocar
  { get :: Allocar (Maybe a)
  , run :: (a -!> Unit) -!> Unit
  , set :: a -&> Unit
  , swap :: a -&> Maybe a
  }
storeLast = newSTR Nothing <#> \ref ->
  { get: getSTR ref
  , run: \f -> getSTR ref >>= traverse_ f
  , set: \v -> setSTR ref (Just v)
  , swap: \v -> swapSTR ref (Just v)
  }

-- | The variable is only `true` for the lifetime of the function.
loading :: forall r. (Allocar Boolean -> Allocar r) -> Allocar r
loading f = do
  isLoading <- newSTR true
  r <- f (getSTR isLoading)
  isLoading &= false
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

storeUnsubscriber :: Allocar
  { set :: Allocar Unit -&> Unit
  , wrapDestroy :: Allocar Unit -> Allocar Unit
  , unsub :: Allocar Unit
  }
storeUnsubscriber = do
  unsubscriber <- prealloc (mempty :: Allocar Unit)
  { cleanup: setter } <- cleanupFn' unsubscriber.set
  let clear = join unsubscriber.get *> setter mempty
  pure
    { set: setter
    , wrapDestroy: (clear *> _)
    , unsub: clear
    }
