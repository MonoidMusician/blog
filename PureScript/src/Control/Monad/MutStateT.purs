module Control.Monad.MutStateT where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader, class MonadTrans, ReaderT(..), ask, lift, local)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Safe.Coerce (coerce)

newtype MutStateT :: Type -> (Type -> Type) -> Type -> Type
newtype MutStateT s m a = MutStateT (ReaderT (Ref s) m a)

mutStateT :: forall s m a. (Ref s -> m a) -> MutStateT s m a
mutStateT = coerce

runMutStateT :: forall s m a. MonadEffect m => s -> MutStateT s m a -> m (Tuple s a)
runMutStateT s (MutStateT (ReaderT sma)) = do
  ref <- liftEffect $ Ref.new s
  a <- sma ref
  s' <- liftEffect $ Ref.read ref
  pure (Tuple s' a)

derive newtype instance functorMutStateT :: Functor m => Functor (MutStateT s m)
derive newtype instance applyMutStateT :: Monad m => Apply (MutStateT s m)
derive newtype instance applicativeMutStateT :: Monad m => Applicative (MutStateT s m)
derive newtype instance bindMutStateT :: Monad m => Bind (MutStateT s m)
derive newtype instance monadMutStateT :: Monad m => Monad (MutStateT s m)
derive newtype instance monadTransMutStateT :: MonadTrans (MutStateT s)

derive newtype instance monadEffectMutStateT :: MonadEffect m => MonadEffect (MutStateT s m)
derive newtype instance monadAffMutStateT :: MonadAff m => MonadAff (MutStateT s m)
derive newtype instance monadTellMutStateT :: MonadTell w m => MonadTell w (MutStateT s m)
derive newtype instance monadWriterMutStateT :: MonadWriter w m => MonadWriter w (MutStateT s m)
derive newtype instance monadThrowMutStateT :: MonadThrow e m => MonadThrow e (MutStateT s m)
derive newtype instance monadErrorMutStateT :: MonadError e m => MonadError e (MutStateT s m)
instance monadAskMutStateT :: MonadAsk r m => MonadAsk r (MutStateT s m) where
  ask = lift ask
instance monadReaderMutStateT :: MonadReader r m => MonadReader r (MutStateT s m) where
  local f (MutStateT (ReaderT sma)) = MutStateT (ReaderT (local f <<< sma))

instance monadStateMutStateT :: MonadEffect m => MonadState s (MutStateT s m) where
  state f = MutStateT $ ReaderT \ref -> liftEffect do
    Ref.modify' (\s -> let Tuple value state = f s in { state, value }) ref

derive newtype instance semigroupMutStateT :: (Semigroup a, Apply m) => Semigroup (MutStateT s m a)
derive newtype instance monoidMutStateT :: (Monoid a, Applicative m) => Monoid (MutStateT s m a)
