module FRP.Memoize where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as STRef
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import FRP.Event (AnEvent, bang, create, fold, makeEvent, memoize, subscribe)

bangFold :: forall m a b t. Applicative m => MonadST t m => (a -> b -> b) -> AnEvent m a -> b -> AnEvent m b
bangFold folder event start = bang start <|> fold folder event start

memoBangFold :: forall m a b t r. Applicative m => MonadST t m => (a -> b -> b) -> AnEvent m a -> b -> (AnEvent m b -> r) -> AnEvent m r
memoBangFold folder event start doWithIt = memoize (fold folder event start)
  \folded -> doWithIt (bang start <|> folded)

memoBang :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> a -> (AnEvent m a -> r) -> AnEvent m r
memoBang event start doWithIt = memoize event
  \memoized -> doWithIt (bang start <|> memoized)

memoBeh :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> a -> (AnEvent m a -> r) -> AnEvent m r
memoBeh e a f = makeEvent \k -> do
  { push, event } <- create
  current <- liftST (STRef.new a)
  let
    writeVal v = liftST (STRef.write v current) :: m a
    event' = makeEvent \k' -> do
      liftST (STRef.read current) >>= k'
      subscribe event k'
  k (f event')
  subscribe e (\v -> writeVal v *> push v)

memoLast :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> (AnEvent m a -> r) -> AnEvent m r
memoLast e f = makeEvent \k -> do
  { push, event } <- create
  current <- liftST (STRef.new Nothing)
  let
    writeVal v = void $ liftST (STRef.write (Just v) current) :: m Unit
    event' = makeEvent \k' -> do
      liftST (STRef.read current) >>= traverse_ k'
      subscribe event k'
  k (f event')
  subscribe e (\v -> writeVal v *> push v)

memoBehFold :: forall m a b t r. Applicative m => MonadST t m => (a -> b -> b) -> AnEvent m a -> b -> (AnEvent m b -> r) -> AnEvent m r
memoBehFold folder event start = memoBeh (fold folder event start) start
