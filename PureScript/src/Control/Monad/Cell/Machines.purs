module Control.Monad.Cell.Machines where

import Prelude

import Control.Monad.Cell.Basics (class BasicAsync, class Cellular, mintCell, schedule, waiting)
import Control.Monad.Cell.Interface (Interface)

import Data.Either (Either, either)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))


newtype Machine m i o = Machine (i -> m (Tuple (Machine m i o) o))
type PureMachine i o = forall m. Applicative m => Machine m i o

data KMachine :: forall kind. (Type -> Type) -> (kind -> Type) -> (kind -> Type) -> Type
data KMachine m i o = KMachine (forall k. i k -> m (Tuple (KMachine m i o) (o k)))

mkMachine :: forall m s i o. Applicative m =>
  s ->
  (s -> i -> Tuple s o) ->
  Machine m i o
mkMachine initial evolve = encapsulate initial
  where
  encapsulate state = Machine \input -> pure
    let Tuple updated output = evolve state input
    in Tuple (encapsulate updated) output

monadicMachine :: forall m s i o. Applicative m =>
  s ->
  (s -> i -> m (Tuple s o)) ->
  Machine m i o
monadicMachine initial evolve = encapsulate initial
  where
  encapsulate state = Machine \input -> ado
    Tuple updated output <- evolve state input
    in Tuple (encapsulate updated) output

transitionMachine :: forall m s i o. Applicative m =>
  s ->
  (s -> i -> m (Tuple (Either (Machine m i o) s) o)) ->
  Machine m i o
transitionMachine initial evolve = encapsulate initial
  where
  encapsulate state = Machine \input -> ado
    Tuple updated output <- evolve state input
    in Tuple (either identity encapsulate updated) output





-- | A synchronous machine is really simple: it maintains its current internal
-- | state (by maintaining the current machine) and it updates it for each
-- | request. It cannot call itself and it assumes that no requests can come in
-- | while it is processing on an input, otherwise they will see old state
-- | and potentially clobber it.
syncMachine ::
  forall m i o.
    Cellular m =>
  Machine m i o -> m (Interface m i o)
syncMachine initial = do
  cell <- mintCell initial
  pure \input -> do
    Machine feed <- cell.get
    Tuple updated output <- feed input
    output <$ cell.set updated


actionMachine :: forall m s o. Cellular m =>
  s -> (s -> m (Tuple s o)) -> m (m o)
actionMachine initial act = do
  cell <- mintCell initial
  pure do
    s <- cell.get
    Tuple s' o <- act s
    o <$ cell.set s'

progressMachine :: forall m s. Cellular m =>
  s -> (s -> m s) -> m (m Unit)
progressMachine initial act = do
  cell <- mintCell initial
  pure do
    s <- cell.get
    s' <- act s
    cell.set s'


mkQueue :: forall @t m. Cellular m => m
  { push :: t -> m Unit
  , pop :: m (Maybe t)
  }
mkQueue = ado
  qPush <- mintCell List.Nil
  qPop <- mintCell List.Nil
  in
    { push: \t -> void $ qPush.update (List.Cons t)
    , pop: do
        info <- qPop.get >>= case _ of
          List.Nil -> Nothing <$ qPop.set List.Nil
          List.Cons t ts -> Just t <$ qPop.set ts
        case info of
          Just t -> pure (Just t)
          Nothing -> do
            queued <- qPush.swap List.Nil
            case List.reverse queued of
              List.Nil -> pure Nothing
              List.Cons t ts -> Just t <$ qPop.set ts
    }


-- | A work queue coordinating exclusive access to a resource `t`. The callers
-- | will wait on the state via `awaitState`, leaving a callback on the queue
-- | if it is not available immediately, which `returnState` will schedule
-- | as needed.
mkResourceQueue :: forall @t m. Cellular m => BasicAsync m =>
  t -> m
  { awaitState :: m t
  , returnState :: t -> m Unit
  }
mkResourceQueue initial = do
  cell <- mintCell (Just initial :: Maybe t)
  queue <- mkQueue @(t -> m Unit)
  let
    awaitState :: m t
    awaitState = do
      cell.swap Nothing >>= case _ of
        -- available
        Just state -> pure state
        -- busy
        Nothing -> waiting \fulfill ->
          queue.push fulfill
    returnState :: t -> m Unit
    returnState updatedState = do
      -- check the queue
      queue.pop >>= case _ of
        -- kick off the first request that arrived in the meantime
        Just worker -> schedule (worker updatedState)
        -- nothing pending, the machine will be available for the next input
        Nothing -> cell.set (Just updatedState)
  pure { awaitState, returnState }

-- | An async machine supports queueing inputs. Its state is no longer persistent:
-- | it checks out the state to process an input, and requests that arrive while
-- | it is doing so will be placed on the queue.
asyncMachine :: forall m i o.
  Cellular m => BasicAsync m =>
  Machine m i o -> m (Interface m i o)
asyncMachine initial = do
  cell <- mintCell (Just initial :: Maybe (Machine m i o))
  queue <- mkQueue @(Tuple i (o -> m Unit))
  let
    checkQueue :: Machine m i o -> m Unit
    checkQueue updated = do
      queue.pop >>= case _ of
        -- the machine is available for the next input
        Nothing -> cell.set (Just updated)
        -- kick off the first request that arrived in the meantime
        Just (Tuple arrived callback) -> do
          schedule do
            process arrived >>= callback
    process :: i -> m o
    process input = do
      cell.swap Nothing >>= case _ of
        -- available
        Just (Machine feed) -> do
          Tuple updated output <- feed input
          checkQueue updated
          pure output
        -- busy
        Nothing -> do
          waiting \fulfill -> do
            queue.push (Tuple input fulfill)
  pure process
