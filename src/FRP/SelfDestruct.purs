module FRP.SelfDestruct where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as Ref
import FRP.Event (AnEvent, makeEvent, subscribe)

selfDestruct :: forall s m a. MonadST s m => (a -> Boolean) -> m Unit -> AnEvent m a -> AnEvent m a
selfDestruct pred onEnd e = makeEvent \k -> do
  s <-liftST $ Ref.new (pure unit)
  u <- subscribe e \a -> do
    if (pred a) then (join $ (liftST $ Ref.read s)) *> onEnd else k a
  void $ (liftST $ Ref.write u s)
  pure do
    join $ (liftST $ Ref.read s)