module Riverdragon.River.Bed where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Global as ST
import Control.Monad.ST.Internal as STR
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STA
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (fst, uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Idiolect (type (-!>))
import Safe.Coerce (coerce)
import Web.Event.Event as Web
import Web.Event.EventPhase (EventPhase(..))
import Web.Event.EventTarget (EventTarget)
import Web.Event.EventTarget as Event


-- | Benign effects of allocation and deallocation.
newtype Allocar a = Allocar (Effect a)

derive instance newtypeAllocar :: Newtype (Allocar a) _
derive newtype instance functorAllocar :: Functor Allocar
derive newtype instance applyAllocar :: Apply Allocar
derive newtype instance applicativeAllocar :: Applicative Allocar
derive newtype instance bindAllocar :: Bind Allocar
derive newtype instance monadAllocar :: Monad Allocar
derive newtype instance monadSTAllocar :: MonadST ST.Global Allocar
derive newtype instance semigroupAllocar :: Semigroup a => Semigroup (Allocar a)
derive newtype instance monoidAllocar :: Monoid a => Monoid (Allocar a)

unsafeAllocate :: forall a. Allocar a -> a
unsafeAllocate = coerce (unsafePerformEffect :: Effect a -> a)

newtype AllocateFrom a b = AllocateFrom (EffectFn1 a b)
infixr 1 type AllocateFrom as -&>

derive instance newtypeAllocateFrom :: Newtype (AllocateFrom a b) _
instance functorAllocateFrom :: Functor (AllocateFrom a) where
  map f (AllocateFrom g) = AllocateFrom do
    mkEffectFn1 \i -> f <$> runEffectFn1 g i
instance profunctorAllocateFrom :: Profunctor AllocateFrom where
  dimap f g (AllocateFrom h) = AllocateFrom do
    mkEffectFn1 \i -> g <$> runEffectFn1 h (f i)
derive newtype instance semigroupAllocateFrom :: Semigroup b => Semigroup (AllocateFrom a b)
derive newtype instance monoidAllocateFrom :: Monoid b => Monoid (AllocateFrom a b)

unsafeAllocateFrom :: forall a b. AllocateFrom a b -> a -> b
unsafeAllocateFrom (AllocateFrom f) i = unsafePerformEffect (runEffectFn1 f i)

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
allocRefLoop spec = Allocar do
  Ref.new (0 /\ spec.start) <#> Allocar <<< Ref.modify'
    \s -> { value: uncurry spec.out s, state: (add 1 <<< fst &&& uncurry spec.incr) s }

freshId :: Allocar (Allocar Int)
freshId = allocRefLoop { start: unit, incr: mempty, out: const }

-- Marked unsafe for Erlang
globalId :: Allocar Int
globalId = unsafeAllocate freshId

whenMM :: forall m a. Monad m => Monoid a => m Boolean -> m a -> m a
whenMM c e = c >>= if _ then e else pure mempty



-- | Cleanup means it only runs once!
cleanup' :: Allocar Unit -> Allocar
  { cleanup :: Allocar Unit
  , running :: Allocar Boolean
  , runningE :: Effect Boolean
  }
cleanup' act = do
  needsToRun <- liftST do STR.new true
  let
    shouldRun = liftST do STR.read needsToRun
  pure
    { cleanup: whenM shouldRun do
        _ <- liftST do STR.write false needsToRun
        act
    , running: shouldRun
    , runningE: liftST do STR.read needsToRun
    }

cleanup :: Allocar Unit -> Allocar (Allocar Unit)
cleanup = cleanup' >>> map _.cleanup

cleanupFn' :: forall d.
  (d -&> Unit) -> Allocar
  { cleanup :: d -&> Unit
  , running :: Allocar Boolean
  , runningE :: Effect Boolean
  , finished :: Allocar (Maybe d)
  , finishedE :: Effect (Maybe d)
  }
cleanupFn' (AllocateFrom act) = do
  cleanupArg <- liftST do STR.new Nothing
  let
    shouldRun = isNothing <$> liftST do STR.read cleanupArg
  pure
    { cleanup: AllocateFrom do
        mkEffectFn1 \d -> do
          whenM shouldRun do
            _ <- liftST do STR.write (Just d) cleanupArg
            runEffectFn1 act d
    , running: isNothing <$> liftST do STR.read cleanupArg
    , runningE: isNothing <$> liftST do STR.read cleanupArg
    , finished: liftST do STR.read cleanupArg
    , finishedE: liftST do STR.read cleanupArg
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
  { cleanup: destroy, running } <- cleanupFn' do
    AllocateFrom do
      mkEffectFn1 \onDestroy -> do
        -- sorry
        len <- liftST do STA.pushAll [] listenersST
        destroyed <- liftST do STA.splice 0 len [] listenersST
        foreachE destroyed \(_ /\ o) -> runEffectFn1 onDestroy o
        pure unit
  pure
    { notify: mkEffectFn1 \cb -> do
        -- Freeze so we do not modify the array as we are iterating over it?
        -- I suppose we could catch new ones ... idk.
        listeners <- liftST do STA.freeze listenersST
        foreachE listeners \(_ /\ o) -> runEffectFn1 cb o
    , push: AllocateFrom do
        mkEffectFn1 \o -> unwrap $ whenMM running do
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
eventListener { eventType, eventPhase, eventTarget } = coerce do
  let capture = eventPhase == Capturing
  mkEffectFn1 \(cb :: Web.Event -> Effect Unit) -> do
    listener <- Event.eventListener cb
    Event.addEventListener eventType listener capture eventTarget $>
      Event.removeEventListener eventType listener capture eventTarget


-- | Allocate an accumulator, that always adds in from the monoid. Useful for
-- | accumulating cleanup procedures, for example.
accumulator :: forall m. Monoid m => Allocar { read :: Allocar m, put :: m -&> Unit, reset :: Allocar m }
accumulator = liftST (STRef.new mempty) <#> \acc -> do
  { read: liftST do STRef.read acc
  , reset: liftST do STRef.read acc <* STRef.write mempty acc
  , put: AllocateFrom do
      mkEffectFn1 \m -> do
        _ <- liftST do STRef.modify (_ <> m) acc
        pure unit
  }



-- prealloc :: forall a. a -> Allocar { get :: Allocar a, set :: a -&> Unit }
-- prealloc default = do
--   liftST do STRef.new default

-- preallocMaybe
