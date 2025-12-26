module Control.Monad.ResourceT where

import Prelude

import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Cont (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.State (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, class MonadWriter, listen, pass, tell)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Distributive (class Distributive, collect, distribute)
import Data.Foldable (traverse_)
import Data.Functor.App (App(..))
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Newtype (unwrap)
import Data.Rational (Rational)
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, ParAff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Riverdragon.Dragon.Breath (microtask)
import Safe.Coerce (coerce)

newtype ResourceT :: (Type -> Type) -> (Type -> Type)
newtype ResourceT m a = ResourceT (Scope -> m a)
type ResourceM = ResourceT Effect

-- TODO: catch error? cancel self if Aff? supervise?
start :: forall m r. MonadEffect m => String -> ResourceT m r -> m { result :: r, wait :: Fiber Unit, destroy :: Effect Unit, scope :: Scope }
start name computation = do
  scope <- liftEffect (mkSubscope name mempty)
  scopedStart name scope computation <#> \{ result, wait, destroy } -> { result, wait, destroy, scope }

start_ :: forall m r. MonadEffect m => String -> ResourceT m r -> m { result :: r, destroy :: Effect Unit, scope :: Scope }
start_ name computation = do
  { result, wait, destroy, scope } <- start name computation
  liftEffect do Aff.launchAff_ do Aff.joinFiber wait
  pure { result, destroy, scope }

run :: forall m r. MonadAff m => String -> ResourceT m r -> m { result :: r, destroy :: Effect Unit, scope :: Scope }
run name = start name >=> \{ result, wait, destroy, scope } -> { result, destroy, scope } <$ liftAff (Aff.joinFiber wait)

scopedStart :: forall m r. MonadEffect m => String -> Scope -> ResourceT m r -> m { result :: r, wait :: Fiber Unit, destroy :: Effect Unit }
scopedStart name here@(Scope { wait: App wait, destroy, destroyed }) (ResourceT computation) = do
  liftEffect do
    whenM (coerce destroyed) do
      throw ("ResourceM already destroyed: " <> name)
  result <- computation here
  pure { result, wait, destroy }

scopedStart_ :: forall m r. MonadEffect m => String -> Scope -> ResourceT m r -> m { result :: r, destroy :: Effect Unit }
scopedStart_ name here computation = do
  { result, wait, destroy } <- scopedStart name here computation
  liftEffect do Aff.launchAff_ do Aff.joinFiber wait
  pure { result, destroy }

scopedRun :: forall m r. MonadAff m => String -> Scope -> ResourceT m r -> m { result :: r, destroy :: Effect Unit }
scopedRun name scope = scopedStart name scope >=> \{ result, wait, destroy } -> { result, destroy } <$ liftAff (Aff.joinFiber wait)

ignoreDestroyed :: Aff Unit -> Aff Unit
ignoreDestroyed = Aff.catchError <@> \err ->
  case String.stripPrefix (String.Pattern "ResourceM destroyed") (Aff.message err) of
    Nothing -> Aff.throwError err
    Just _ -> pure unit

-- The `Aff` is killed when the scope is destroyed
monitor :: String -> Scope -> (Aff ~> Aff)
monitor name (Scope { putDestructor }) act = do
  -- wish `Aff` could get its own canceler
  fiber <- liftEffect do Aff.launchSuspendedAff act
  liftEffect $ putDestructor $ microtask do
    Aff.launchAff_ do Aff.killFiber (Aff.error ("ResourceM destroyed: " <> name)) fiber
  Aff.joinFiber fiber

-- Destroy the scope when the `Aff` exits (does not `monitor`)
link :: Scope -> (Aff ~> Aff)
link (Scope { destroy }) = Aff.finally (liftEffect destroy)

-- Destroy the scope when no more `Aff`s from this are running
-- FIXME: if first is synchronous?
linkN :: Scope -> Effect (Aff ~> Aff)
linkN (Scope { destroy }) = do
  ref <- Ref.new 0
  (pure :: (Aff ~> Aff) -> Effect (Aff ~> Aff)) \act -> do
    Aff.bracket
      do liftEffect (Ref.modify_ (_ + 1) ref)
      do
        const $ liftEffect do
          whenM (eq 0 <$> Ref.modify (_ - 1) ref) do
            destroy
      do const act

newtype Scope = Scope
  { putDestructor :: Effect Unit -> Effect Unit
  , destroy :: Effect Unit
  , destroyed :: Effect (Disj Boolean)
  , waitDestroyed :: App Fiber Unit
  , putWaiters :: Waiters -> Effect Unit
  , wait :: App Fiber Unit
  }
derive newtype instance Monoid Scope
derive newtype instance Semigroup Scope

oneSubScopeAtATime :: forall m. MonadEffect m => String -> Scope -> m (Effect Scope)
oneSubScopeAtATime name parent = liftEffect do
  lastDestroy <- Ref.new mempty
  pure do
    join do Ref.read lastDestroy
    scope@(Scope { destroy }) <- mkSubscope name parent
    Ref.write destroy lastDestroy
    pure scope

type ResourceState =
  { destructors :: Dual (Effect Unit)
  , destroyed :: Disj Boolean
  , waiters :: Waiters
  , waiting :: App Fiber Unit
  , waited :: Disj Boolean
  }
_noop = mempty :: ResourceState

noWaitScope :: Scope -> Scope
noWaitScope (Scope s) = Scope s { wait = mempty }

mkSubscope :: forall m. MonadEffect m => String -> Scope -> m Scope
mkSubscope name (Scope parent) = liftEffect do
  whenM (coerce parent.destroyed) do
    throw ("ResourceM already destroyed: " <> name)
  ref <- Ref.new (mempty :: ResourceState)
  whenDestroyed <- Ref.new (mempty :: Effect Unit)
  let
    put update = ref # Ref.modify_ \st -> st <> update _noop
    waited = Ref.read ref <#> _.waited >>> unwrap
    destroyed = Ref.read ref <#> _.destroyed >>> unwrap
    notDestroyed act = do
      whenM destroyed do
        throw ("ResourceM already destroyed: " <> name)
      act
  waitDestroyed <- Aff.launchAff $ Aff.makeAff \cb ->
    mempty <$ Ref.write (cb (pure unit)) whenDestroyed
  wait <- Aff.launchSuspendedAff do
    Aff.joinFiber $ unwrap parent.wait
    _runWaiters ref
    liftEffect do put _ { waited = Disj true }
  destroy <- _destructor $ do
    r@{ destructors: Dual toRun } <- Ref.read ref
    Ref.write r { destructors = mempty, destroyed = Disj true } ref
    microtask do Aff.launchAff_ do Aff.killFiber (Aff.error ("ResourceM destroyed: " <> name)) wait
    -- FIXME: catch errors
    toRun
    join do Ref.read whenDestroyed
  scope <- pure $ Scope
    { putDestructor: _destructor >=> \d -> notDestroyed do
        put _ { destructors = Dual d }
    , destroy, destroyed: coerce destroyed, waitDestroyed: App waitDestroyed
    , putWaiters: \waiters -> notDestroyed $ waited >>= if _
        then do
          waiting <- _launchWaiters waiters
          put _ { waiting = App waiting }
          parent.putWaiters waiters
        else do
          put _ { waiters = waiters }
          parent.putWaiters waiters
    , wait: App wait
    }
  parent.putDestructor destroy
  pure scope

type Waiters =
  SemigroupMap Rational
    { seq :: Aff Unit, qes :: Dual (Aff Unit), par :: ParAff Unit }
data Dir = Seq | Qes | Par

_runWaiter :: { seq :: Aff Unit, qes :: Dual (Aff Unit), par :: ParAff Unit } -> Aff Unit
_runWaiter { seq, qes: Dual qes, par } = seq *> qes *> Aff.sequential par

_runWaiters :: Ref ResourceState -> Aff Unit
_runWaiters ref = do
  st <- liftEffect do Ref.read ref
  Aff.joinFiber (unwrap st.waiting) -- not exactly sure how to sequence this
  -- read again, just in case
  go =<< liftEffect do Ref.read ref
  where
  go r@{ waiters: SemigroupMap waiters } = case Map.findMin waiters of
    Just first -> do
      liftEffect do
        Ref.write r { waiters = SemigroupMap $ Map.delete first.key waiters } ref
      _runWaiter first.value
      go =<< liftEffect do Ref.read ref
    Nothing -> pure unit

-- TODO: some kind of MonadSupervisor?
_launchWaiters :: Waiters -> Effect (Fiber Unit)
_launchWaiters = Aff.launchAff <<< traverse_ _runWaiter

_shareWaiters :: Waiters -> Effect Waiters
_shareWaiters = traverse \r -> do
  seq <- _share r.seq
  qes <- coerce _share r.qes
  par <- Aff.parallel <$> _share do Aff.sequential r.par
  pure { seq, qes, par }

_mkWaiter :: Rational -> Dir -> Aff Unit -> Waiters
_mkWaiter lvl dir waiter =
  SemigroupMap $ Map.singleton lvl $ case dir of
    Seq -> { seq: waiter, qes: mempty, par: mempty }
    Qes -> { seq: mempty, qes: Dual waiter, par: mempty }
    Par -> { seq: mempty, qes: mempty, par: Aff.parallel waiter }

-- Runs once
_share :: Aff Unit -> Effect (Aff Unit)
_share = Aff.launchSuspendedAff >>> map Aff.joinFiber

-- Runs once
_destructor :: Effect Unit -> Effect (Effect Unit)
_destructor = Ref.new >=> \ref -> pure $
  join $ Ref.read ref <* Ref.write mempty ref

derive instance Functor m => Functor (ResourceT m)
instance Apply m => Apply (ResourceT m) where
  apply (ResourceT l) (ResourceT r) = ResourceT do lift2 apply l r
instance Applicative m => Applicative (ResourceT m) where
  pure a = ResourceT do pure (pure a)
instance Bind m => Bind (ResourceT m) where
  bind (ResourceT m) f = ResourceT \scope ->
    m scope >>= f >>> case _ of ResourceT c -> c scope
instance Monad m => Monad (ResourceT m)
instance (Apply m, Semigroup a) => Semigroup (ResourceT m a) where
  append = lift2 append
instance (Applicative m, Monoid a) => Monoid (ResourceT m a) where
  mempty = pure mempty
instance Alt m => Alt (ResourceT m) where
  alt (ResourceT m) (ResourceT n) = ResourceT \r -> m r <|> n r
instance Plus m => Plus (ResourceT m) where
  empty = ResourceT (const empty)
instance Alternative m => Alternative (ResourceT m)
instance MonadPlus m => MonadPlus (ResourceT m)

mapResourceT :: forall m1 m2 a b. (m1 a -> m2 b) -> ResourceT m1 a -> ResourceT m2 b
mapResourceT f (ResourceT m) = ResourceT (f <<< m)

instance MonadTrans ResourceT where
  lift = ResourceT <<< pure
instance MonadEffect m => MonadEffect (ResourceT m) where
  liftEffect = lift <<< liftEffect
instance MonadAff m => MonadAff (ResourceT m) where
  liftAff = lift <<< liftAff
instance MonadAsk r m => MonadAsk r (ResourceT m) where
  ask = lift ask
instance MonadReader r m => MonadReader r (ResourceT m) where
  local f (ResourceT m) = ResourceT (local f <<< m)
instance MonadTell w m => MonadTell w (ResourceT m) where
  tell = lift <<< tell
instance MonadWriter w m => MonadWriter w (ResourceT m) where
  listen = mapResourceT listen
  pass = mapResourceT pass
instance MonadState s m => MonadState s (ResourceT m) where
  state = lift <<< state
instance MonadThrow e m => MonadThrow e (ResourceT m) where
  throwError = lift <<< throwError
instance MonadError e m => MonadError e (ResourceT m) where
  catchError (ResourceT m) h =
    ResourceT \r -> catchError (m r) (\e -> case h e of ResourceT f -> f r)
instance MonadCont m => MonadCont (ResourceT m) where
  callCC f = ResourceT \r -> callCC \c ->
    case f (ResourceT <<< const <<< c) of ResourceT f' -> f' r
instance Distributive g => Distributive (ResourceT g) where
  distribute a = ResourceT \e -> collect (\r -> case r of ResourceT r' -> r' e) a
  collect f = distribute <<< map f
instance MonadRec m => MonadRec (ResourceT m) where
  tailRecM k a = ResourceT \r -> tailRecM (k' r) a
    where
    k' r a' = case k a' of ResourceT f -> pure =<< f r
