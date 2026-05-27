module Control.Monad.Cell.Processors where

import Prelude

import Control.Monad.Cell.Basics (class BasicAsync, class Cellular, mintCell, schedule, waiting)
import Control.Monad.Cell.Interface (Interface, KIOPair, KInterface(..), mkKIOPair, runKIOPair, toSimple, unKIOPair)
import Control.Monad.Cell.Machines (mkResourceQueue, monadicMachine, syncMachine)
import Control.Plus (empty)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Queue (Queue(..), popQueue, pushQueue)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

data Processing m s o
  = Processed s o
  | ProcessedSoon s (m o) -- state is available, still waiting for output
  | ProcessedBusy (m s) o -- output is available, still waiting for state
  | Processing s
    -- this `m` can block for a while and then return,
    -- either returning the output immediately after all
    -- or asking for a new state to continue processing
    (m (Either o (s -> m (Processing m s o))))

derive instance Functor m => Functor (Processing m s)

-- | We can process it all at once, with no interleaving of state.
processSync :: forall m s o. Monad m => Processing m s o -> m (Tuple s o)
processSync (Processed s o) = pure (Tuple s o)
processSync (ProcessedSoon s mo) = Tuple s <$> mo
processSync (ProcessedBusy ms o) = Tuple <$> ms <@> o
processSync (Processing s willContinueWithState) = do
  willContinueWithState >>= case _ of
    Right continueWithState ->
      processSync =<< continueWithState s
    Left outputImmediately ->
      pure $ Tuple s outputImmediately

-- | This makes a simple synchronous processor.
syncProcessor ::
  forall m s i o.
    Cellular m =>
  s -> (s -> i -> m (Processing m s o)) ->
  m (Interface m i o)
syncProcessor initial stepProcessor =
  syncMachine $ monadicMachine initial \state input ->
    processSync =<< stepProcessor state input

-- | This steps up the game by allowing the processor to pause its computation
-- | by checking in a state while, say, a long-running I/O computation runs,
-- | then being able to queue resuming the computation by checking out a new
-- | state and finally returning an output (or more processing).
-- |
-- | This allows interleaving requests, at the consequence of keeping the state
-- | parameter `s` around while constructing the processor (not hiding it
-- | behind an abstract `Machine m i o` which could even change its internal
-- | state type as it runs).
asyncProcessor ::
  forall m s i o.
    Cellular m => BasicAsync m =>
  s -> (s -> i -> m (Processing m s o)) ->
  m (Interface m i o)
asyncProcessor initial stepProcessor =
  toSimple <$> asyncKProcessor initial
    \s (Const i) -> map Const <$> stepProcessor s i

-- | This is the same principle, but with the input and output types keyed
-- | together for each request, to support typesafe methods and stuff.
asyncKProcessor ::
  forall m s i o.
    Cellular m => BasicAsync m =>
  s -> (forall k. s -> i k -> m (Processing m s (o k))) ->
  m (KInterface m i o)
asyncKProcessor initial stepProcessor = do
  queue <- mkResourceQueue initial
  let
    process :: forall k. i k -> m (o k)
    process input = do
      state <- queue.awaitState
      stepProcessor state input >>= pumpResult
    pumpResult :: forall k. Processing m s (o k) -> m (o k)
    pumpResult = case _ of
      Processed updatedState output -> do
        output <$ queue.returnState updatedState
      ProcessedBusy pendingState output -> do
        output <$ schedule (queue.returnState =<< pendingState)
      ProcessedSoon updatedState finishGeneratingOutput -> do
        queue.returnState updatedState
        finishGeneratingOutput
      Processing checkpoint willResume -> do
        queue.returnState checkpoint
        willResume >>= case _ of
          Right continueWithState -> do
            state <- queue.awaitState
            continueWithState state >>= pumpResult
          Left outputImmediately -> do
            pure outputImmediately
  pure $ KInterface process



newtype SwitchProcessor :: forall kind. (Type -> Type) -> (kind -> Type) -> (kind -> Type) -> Type
newtype SwitchProcessor m i o = SwitchProcessor
  (forall r. (forall s. s -> (forall k. s -> i k -> m (SwitchProcessing m (SwitchProcessor m i o) s (o k))) -> r) -> r)


mkSwitchProcessor :: forall m s i o.
  s -> (forall k. s -> i k -> m (SwitchProcessing m (SwitchProcessor m i o) s (o k))) ->
  SwitchProcessor m i o
mkSwitchProcessor initial evolve = SwitchProcessor \provide -> provide initial evolve


data SwitchProcess m switch stay
  = ProcessStay stay
  | ProcessTrySwitch (m switch) (m stay)
  | SwitchLater (m (SwitchProcess m switch stay))

data SwitchProcessing m switch stay o
  = SwitchProcessed (SwitchProcess m switch stay) (m o)
  | SwitchProcessing stay
    -- this `m` can block for a while and then return,
    -- either returning the output immediately or
    -- asking for a new state to continue processing
    (m (Either o (stay -> m (SwitchProcessing m switch stay o))))



type ProcessorQueue :: forall k. (Type -> Type) -> (k -> Type) -> (k -> Type) -> Type
type ProcessorQueue m input output = Queue (KIOPair m input output)

data SwitchQueue :: forall k. (Type -> Type) -> Type -> (k -> Type) -> (k -> Type) -> Type
data SwitchQueue m stay input output
  = SQAtRest stay
  | SQSwitchable (ProcessorQueue m input output)
  | SQLocked (Queue (stay -> m Unit)) (List.List (KIOPair m input output))


-- | An async processor that queues inputs until the state is available, and
-- | also allows switching out the whole processor for one with an interface
-- | of the same type.
-- |
-- | It maintains a queue of input-output pairs or threads waiting on state.
-- | It will transfer inputs over, but it can only switch if there are no
-- | threads waiting on the state.
-- |
-- | Switching allows adding or discarding items from state without dealing
-- | with optionality: they can be always present (or captured in the closure),
-- | no need to check if they are present on each operation and deal with errors.
-- |
-- | It also can help with directional state machines, where previous states
-- | become unreachable. Although there is nothing that prevents it from
-- | switching back to another instance of the previous machine later.
asyncSwitchKProcessor ::
  forall m i o.
    Cellular m => BasicAsync m =>
  SwitchProcessor m i o ->
  m (KInterface m i o)
asyncSwitchKProcessor initialProcessor = do
  currentInterface <- mintCell @(KInterface m i o) $
    KInterface \_ -> unsafeCrashWith "SwitchProcessor not initialized"
  let
    process :: forall @k. i k -> m (o k)
    process i = currentInterface.get >>= \(KInterface iface) -> iface i

    switchTo :: SwitchProcessor m i o -> ProcessorQueue m i o -> m Unit
    switchTo newProcessor (Queue preload1 preload2) = do
      case newProcessor of
        SwitchProcessor useSwitchProcessor ->
          useSwitchProcessor (asyncSwitchKProcessor1 switchTo)
            >>= currentInterface.set
      for_ (preload1 <> List.reverse preload2)
        do runKIOPair \input callback -> process input >>= callback
  switchTo initialProcessor empty
  pure $ KInterface process

-- | This sets up the processor loop, with a callback to switch it out.
asyncSwitchKProcessor1 ::
  forall m s i o.
    Cellular m => BasicAsync m =>
  (SwitchProcessor m i o -> ProcessorQueue m i o -> m Unit) ->
  s -> (forall k. s -> i k -> m (SwitchProcessing m (SwitchProcessor m i o) s (o k))) ->
  m (KInterface m i o)
asyncSwitchKProcessor1 switchTo initial stepProcessor = do
  queue <- mintCell (SQAtRest initial :: SwitchQueue m s i o)
  let
    -- Process an input. It will wait for state to be available then
    -- run the processor algorithm.
    process :: forall @k. i k -> m (o k)
    process input = queue.get >>= case _ of
      -- The state is checked in, we can process it immediately
      SQAtRest state -> do
        queue.set $ SQSwitchable mempty
        stepProcessor state input >>= pumpResult
      -- Maintain a switchable queue
      SQSwitchable q -> do
        waiting \callback ->
          queue.set $ SQSwitchable $ pushQueue (mkKIOPair input callback) q
      -- Put it on the switchable tail of the locked queue
      SQLocked q p -> do
        waiting \callback ->
          queue.set $ SQLocked q $ List.Cons (mkKIOPair input callback) p
    -- Check the state back in after processing an output, or in the middle
    -- of a long running operation. This will schedule callbacks of pending
    -- inputs on a separate thread if needed.
    returnState :: s -> m Unit
    returnState state = queue.get >>= case _ of
      SQAtRest _ -> unsafeCrashWith "Queue was not empty"
      SQSwitchable q -> case popQueue q of
        Nothing -> queue.set (SQAtRest state)
        Just (Tuple iopair q') -> do
          queue.set (SQSwitchable q')
          schedule do
            unKIOPair iopair \input callback -> do
              process input >>= callback
      SQLocked q p -> case popQueue q of
        Nothing -> queue.set (SQAtRest state)
        Just (Tuple callback q') -> do
          queue.set case q' of
            -- The last locked item, now it is switchable
            Queue List.Nil List.Nil -> SQSwitchable (Queue List.Nil p)
            _ -> SQLocked q' p
          schedule do callback state
    -- Await the state directly. This locks the queue, so the processor cannot
    -- switch until this resolves.
    awaitState :: m s
    awaitState = queue.get >>= case _ of
      SQAtRest s -> pure s
      SQLocked q p -> waiting \callback -> do
        -- Convert the push queue of switchable callbacks to locked ones
        queue.set $ fullyLocked $ pushQueue callback $ q <> Queue List.Nil (switchableToLocked <$> p)
      SQSwitchable q -> waiting \callback -> do
        queue.set $ fullyLocked $ pushQueue callback $ switchableToLocked <$> q
    fullyLocked = SQLocked <@> List.Nil
    switchableToLocked iopair state = unKIOPair iopair \input callback -> do
      stepProcessor state input >>= pumpResult >>= callback
    -- Figure out what to do with the result
    pumpResult :: forall k. SwitchProcessing m (SwitchProcessor m i o) s (o k) -> m (o k)
    pumpResult = case _ of
      SwitchProcessed switching finishGeneratingOutput -> do
        getToState schedule switching
        finishGeneratingOutput
      SwitchProcessing checkpoint willResume -> do
        -- Check the state in
        returnState checkpoint
        -- With further processing
        willResume >>= case _ of
          -- Get an updated state back
          Right continueWithState -> do
            state <- awaitState
            continueWithState state >>= pumpResult
          -- It did not end up needing access to state again
          Left outputImmediately -> do
            pure outputImmediately
    getToState :: (m Unit -> m Unit) -> SwitchProcess m (SwitchProcessor m i o) s -> m Unit
      {- only the first iteration needs to schedule, so output can go through immediately -}
    getToState maySchedule = case _ of
      ProcessStay updatedState -> returnState updatedState
      SwitchLater later -> maySchedule do
        later >>= getToState identity
      ProcessTrySwitch switch stay -> do
        queue.get >>= case _ of
          SQAtRest _ -> unsafeCrashWith "Queue was not empty"
          SQLocked _ _ -> maySchedule do
            stay >>= returnState
          SQSwitchable q -> maySchedule do
            newProcessor <- switch
            switchTo newProcessor q
  pure $ KInterface process


