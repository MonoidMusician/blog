module FRP.Memoize where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Internal as STRef
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import FRP.Event (Event, createPure, fold, makeLemmingEvent, memoize)

pureFold :: forall a b. (b -> a -> b) -> b -> Event a -> Event b
pureFold folder start event = pure start <|> fold folder start event

memopureFold :: forall a b r. (b -> a -> b) -> b -> Event a -> (Event b -> r) -> Event r
memopureFold folder start event doWithIt = memoize (fold folder start event)
  \folded -> doWithIt (pure start <|> folded)

memopure :: forall a r. Event a -> a -> (Event a -> r) ->  Event r
memopure event start doWithIt = memoize event
  \memoized -> doWithIt (pure start <|> memoized)

memoBeh :: forall a r. Event a -> a -> (Event a -> r) ->  Event r
memoBeh e a f = makeLemmingEvent \mySub k -> do
  { push, event } <- createPure
  current <-  (STRef.new a)
  let
    writeVal v =  (STRef.write v current)
    event' = makeLemmingEvent \mySub' k' -> do
       (STRef.read current) >>= k'
       mySub' event k'
  k (f event')
  mySub e (\v -> writeVal v *> push v)

memoLast :: forall a r. Event a -> (Event a -> r) -> Event r
memoLast e f = makeLemmingEvent \mySub k -> do
  { push, event } <- createPure
  current <-  (STRef.new Nothing)
  let
    writeVal v = void $  (STRef.write (Just v) current)
    event' = makeLemmingEvent \mySub' k' -> do
       (STRef.read current) >>= traverse_ k'
       mySub' event k'
  k (f event')
  mySub e (\v -> writeVal v *> push v)

memoBehFold :: forall a b r. (b -> a -> b) -> b -> Event a -> (Event b -> r) -> Event r
memoBehFold folder start event = memoBeh (fold folder start event) start
