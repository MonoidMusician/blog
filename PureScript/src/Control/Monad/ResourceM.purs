module Control.Monad.ResourceM where

import Control.Monad.ResourceT

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.MutStateT (MutStateT, mapMutStateT)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.State (StateT, mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, mapWriterT)
import Data.Functor.App (App(..))
import Data.Rational (Rational, fromInt)
import Effect (Effect)
import Effect.Aff (Aff, joinFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Monoid, type (~>), Unit, map, pure, void, zero, (*>), (<#>), (<$), (<$>), (<*), (<<<), (<=<), (=<<), (>>=), (>>>))

class MonadEffect m <= MonadResource m where
  selfScope :: m Scope
  -- Somewhat unsafe, e.g. with `mempty :: Scope`
  _inScope :: Scope -> m ~> m

instance MonadEffect m => MonadResource (ResourceT m) where
  selfScope = ResourceT \scope -> pure scope
  _inScope scope (ResourceT f) = ResourceT \_ -> f scope

_addDestructor :: forall m. MonadResource m => Effect Unit -> m Unit
_addDestructor d = selfScope >>= \(Scope { putDestructor }) -> liftEffect (putDestructor d)
_addWaiters :: forall m. MonadResource m => Waiters -> m Unit
_addWaiters w = selfScope >>= \(Scope { putWaiters }) -> liftEffect (putWaiters w)

-- Safe for appended/nested scopes
addDestructor :: forall m. MonadResource m => Effect Unit -> m Unit
addDestructor = _addDestructor <=< liftEffect <<< _destructor

getAddDestructor :: forall m. MonadResource m => m (Effect Unit -> Effect Unit)
getAddDestructor = selfScope <#> \(Scope { putDestructor }) ->
  putDestructor <=< liftEffect <<< _destructor

-- Shorter synonym
destr = addDestructor :: forall m. MonadResource m => Effect Unit -> m Unit

-- Safe for appended/nested scopes
addWaiters :: forall m. MonadResource m => Waiters -> m Unit
addWaiters = _addWaiters <=< liftEffect <<< _shareWaiters

subScope :: forall m. MonadResource m => m Scope
subScope = selfScope >>= mkSubscope >>> liftEffect

addWaiter :: forall m. MonadResource m => Rational -> Dir -> Aff Unit -> m Unit
addWaiter lvl dir waiter = do
  _addWaiters <<< _mkWaiter lvl dir =<< liftEffect do _share waiter

waitr :: forall m. MonadResource m => Aff Unit -> m Unit
waitr = addWaiter zero Par

-- Function to run when everything in scope is really ready (level 100)
whenReady :: forall m. MonadResource m => Effect Unit -> m Unit
whenReady = addWaiter (fromInt 100) Seq <<< liftEffect

-- Wait for everything to be ready
wait :: forall m. MonadResource m => MonadAff m => m Unit
wait = selfScope >>= \(Scope { wait: App waiting }) -> liftAff (joinFiber waiting)

-- Destroy self
selfDestruct :: forall m. MonadResource m => m Unit
selfDestruct = selfScope >>= \(Scope { destroy }) -> liftEffect destroy

-- Get the destructor for this scope
selfDestructor :: forall m. MonadResource m => m (Effect Unit)
selfDestructor = selfScope >>= \(Scope { destroy }) -> pure destroy

-- Caution: should not neglect inner destroy
inSubScope :: forall m. MonadResource m => m ~> m
inSubScope m = subScope >>= \scope -> _inScope scope m

-- Kind of like `with` in Python: destroys resources on exit
scoped :: forall e m. MonadResource m => MonadError e m => m ~> m
scoped act = inSubScope do
  catchError (act <* selfDestruct) \e -> selfDestruct *> throwError e

-- Actually like `async with` in Python (although it does not wait for destructors)
with ::
  forall e m resource return.
    MonadResource m => MonadAff m => MonadError e m =>
  m resource -> (resource -> m return) -> m return
with mk cont = scoped do
  mk >>= \r -> wait *> cont r

-- Use around `inSubScope` (or `scoped` or `with`)
noWait :: forall m. MonadResource m => m ~> m
noWait act = selfScope >>= noWaitScope >>> \scope -> _inScope scope act

trackM :: forall r m. MonadResource m => m { destroy :: Effect Unit | r } -> m { destroy :: Effect Unit | r }
trackM m = m >>= \r -> r <$ destr r.destroy

track :: forall r m. MonadResource m => Effect { destroy :: Effect Unit | r } -> m { destroy :: Effect Unit | r }
track = trackM <<< liftEffect

trackA :: forall r m. MonadAff m => MonadResource m => Aff { destroy :: Effect Unit | r } -> m { destroy :: Effect Unit | r }
trackA m =
  selfScope >>= \scope ->
    trackM do liftAff do monitor scope m

trackM_ :: forall m. MonadResource m => m (Effect Unit) -> m Unit
trackM_ = void <<< trackM <<< map { destroy: _ }

track_ :: forall m. MonadResource m => Effect (Effect Unit) -> m Unit
track_ = trackM_ <<< liftEffect

trackA_ :: forall m. MonadAff m => MonadResource m => Aff (Effect Unit) -> m Unit
trackA_ = void <<< trackA <<< map { destroy: _ }

liftResourceM :: forall m. MonadResource m => ResourceM ~> m
liftResourceM act = selfScope >>= \scope ->
  _.result <$> liftEffect do scopedStart scope act

instance MonadResource m => MonadResource (ReaderT r m) where
  selfScope = lift selfScope
  _inScope scope = mapReaderT (_inScope scope)
instance (Monoid w, MonadResource m) => MonadResource (WriterT w m) where
  selfScope = lift selfScope
  _inScope scope = mapWriterT (_inScope scope)
instance MonadResource m => MonadResource (StateT s m) where
  selfScope = lift selfScope
  _inScope scope = mapStateT (_inScope scope)
instance MonadResource m => MonadResource (ExceptT e m) where
  selfScope = lift selfScope
  _inScope scope = mapExceptT (_inScope scope)
instance MonadResource m => MonadResource (MutStateT r m) where
  selfScope = lift selfScope
  _inScope scope = mapMutStateT (_inScope scope)
