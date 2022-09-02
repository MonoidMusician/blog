module FRP.SelfDestruct where

import Prelude

import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as Ref
import Effect (Effect)
import FRP.Event (Event, makeEvent, subscribe)

selfDestruct :: forall a. (a -> Boolean) -> Effect Unit -> Event a -> Event a
selfDestruct pred onEnd e = makeEvent \k -> do
  s <-liftST $ Ref.new (pure unit)
  u <- subscribe e \a -> do
    if (pred a) then (join $ (liftST $ Ref.read s)) *> onEnd else k a
  void $ (liftST $ Ref.write u s)
  pure do
    join $ (liftST $ Ref.read s)