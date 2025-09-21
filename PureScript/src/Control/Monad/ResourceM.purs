module Control.Monad.ResourceM where

import Control.Monad.ResourceT

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, mapWriterT)
import Data.Functor.App (App(..))
import Data.Rational (Rational, fromInt)
import Effect (Effect)
import Effect.Aff (Aff, joinFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (class Monoid, type (~>), Unit, pure, zero, (*>), (<$), (<*), (<<<), (<=<), (=<<), (>>=), (>>>))

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

track :: forall r m. MonadResource m => m { destroy :: Effect Unit | r } -> m { destroy :: Effect Unit | r }
track m = m >>= \r -> r <$ destr r.destroy

trackE :: forall r m. MonadResource m => Effect { destroy :: Effect Unit | r } -> m { destroy :: Effect Unit | r }
trackE = track <<< liftEffect

instance MonadResource m => MonadResource (ReaderT r m) where
  selfScope = lift selfScope
  _inScope scope = mapReaderT (_inScope scope)
instance (Monoid w, MonadResource m) => MonadResource (WriterT w m) where
  selfScope = lift selfScope
  _inScope scope = mapWriterT (_inScope scope)
instance MonadResource m => MonadResource (ExceptT e m) where
  selfScope = lift selfScope
  _inScope scope = mapExceptT (_inScope scope)
