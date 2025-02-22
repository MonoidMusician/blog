module Riverdragon.River.Beyond where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..), hush, isLeft)
import Data.Filterable (compact, partitionMap)
import Data.Foldable (fold)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect, foreachE, whileE)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Now (now)
import Effect.Timer (clearInterval, clearTimeout, setInterval, setTimeout)
import Idiolect ((#..))
import Partial.Unsafe (unsafePartial)
import Riverdragon.Dragon.Breath (microtask)
import Riverdragon.River (Allocar, Lake, River, Stream, allStreams, dam, fixPrjBurst, foldStream, mailboxRiver, makeLake, makeLake', mapAl, oneStream, singleShot, statefulStream, subscribeIsh, unsafeCopyFlowing, withInstantiated, (<?*>), (>>~))
import Riverdragon.River.Bed (breaker, eventListener, freshId, ordMap, prealloc, pushArray, requestAnimationFrame)
import Web.DOM.Element as Element
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventPhase (EventPhase(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

-- | Requires its delay function to be predictable, especially if the result
-- | is used as a river.
delayWith :: forall flow.
  (Effect Unit -> Allocar (Allocar Unit)) ->
  Stream flow ~> Stream flow
delayWith delaying stream =
  unsafeCopyFlowing stream $ makeLake' \selfDestruct cb -> do
    isDestroyed <- prealloc false
    ids <- freshId
    inflight <- ordMap
    let
      upstreamDestroyed = do
        isDestroyed.set true
        -- if it is not destroyed, the last in flight callback will trigger it
        whenM (Map.isEmpty <$> inflight.read) selfDestruct
      receive value = whenM (not <$> isDestroyed.get) do
        id <- ids
        cancelIt <- delaying do
          void $ inflight.remove id
          cb value
          whenM isDestroyed.get do
            whenM (Map.isEmpty <$> inflight.read) do
              selfDestruct
        inflight.set id cancelIt
    unsub <- subscribeIsh upstreamDestroyed stream receive
    pure $ unsub <> inflight.traverse (const identity)

delay :: forall flow. Milliseconds -> Stream flow ~> Stream flow
delay (Milliseconds ms) = delayWith \cb -> do
  clearTimeout <$> setTimeout (Int.round ms) cb

delayMicro :: forall flow. Stream flow ~> Stream flow
delayMicro = delayWith \cb -> do
  -- Microtasks have no canceler, so we have to drop events ourselves
  brk <- breaker $ const cb
  brk.trip <$ microtask (brk.run unit)

delayAnim :: forall flow. Stream flow ~> Stream flow
delayAnim = delayWith requestAnimationFrame

data AFBState
  = Idle
  | Requested (Effect Unit)
  | Draining (Maybe { start :: Instant, timeout :: Milliseconds })
  | Destroyed

mkAnimFrameBuffer :: Allocar
  { buffering :: forall flow. Stream flow ~> Stream flow
  , destroy :: Effect Unit
  , drain :: Allocar Unit
  , setDrainTimeout :: Maybe Milliseconds -> Allocar Unit
  }
mkAnimFrameBuffer = mkBufferedDelayer requestAnimationFrame

mkBufferedDelayer ::
  (Allocar Unit -> Allocar (Allocar Unit)) ->
  Allocar
    { buffering :: forall flow. Stream flow ~> Stream flow
    , drain :: Effect Unit
    , destroy :: Allocar Unit
    , setDrainTimeout :: Maybe Milliseconds -> Allocar Unit
    }
mkBufferedDelayer delayFn = do
  pending <- pushArray
  state <- prealloc Idle
  drained <- prealloc true
  currentDrainTimeout <- prealloc Nothing
  let
    notDestroyed = case _ of
      Destroyed -> false
      _ -> true
    drain :: Effect Unit
    drain = whenM (notDestroyed <$> state.get) do
      timeout <- currentDrainTimeout.get
      case timeout of
        Nothing -> do
          state.set (Draining Nothing)
          whileE (not <$> drained.get) do
            drained.set true
            pending.reset >>= \items -> foreachE items identity
        Just drainTimeout -> do
          drainStart <- now
          state.set (Draining (Just { start: drainStart, timeout: drainTimeout }))
          whileE (not <$> drained.get) do
            drained.set true
            pending.reset >>= \items -> foreachE items \item -> do
              drainTime <- now
              when (Instant.diff drainTime drainStart < drainTimeout) do
                item
      state.set Idle
    animFrameBuffer :: forall flow. Stream flow ~> Stream flow
    animFrameBuffer = delayWith \cb0 -> do
      { trip, run } <- breaker \_ -> cb0
      let cb = run unit
      state.get >>= case _ of
        Idle -> do
          canceler <- delayFn drain
          state.set (Requested canceler)
          drained.set false
          pending.push cb
        Requested _ -> do
          drained.set false
          pending.push cb
        Draining _ -> do
          drained.set false
          pending.push cb
        Destroyed -> pure unit
      pure trip
    r ::
      { buffering :: forall flow. Stream flow ~> Stream flow
      , drain :: Effect Unit
      , destroy :: Allocar Unit
      , setDrainTimeout :: Maybe Milliseconds -> Effect Unit
      }
    r =
      { buffering: animFrameBuffer
      , drain
      , destroy: do
          state.get >>= case _ of
            Requested canceler -> canceler
            _ -> pure unit
          state.set Destroyed
      , setDrainTimeout: currentDrainTimeout.set
      }
  pure r

counter :: forall flow a. Stream flow a -> Lake (a /\ Int)
counter = statefulStream 0 <@> \b a ->
  { state: (b + 1), emit: Just (a /\ b) }

debounce :: forall flow. Milliseconds -> Stream flow ~> Lake
debounce timing input =
  let
    tagged = counter input
    tags = map snd tagged
    delayLine = delay timing tagged
    gateLatest requested (datum /\ arriving) =
      if requested > arriving then Nothing else Just datum
  in compact $ gateLatest <$> tags <?*> delayLine

withLast ::
  forall flow a.
  Stream flow a ->
  Lake
    { last :: Maybe a
    , next :: a
    }
withLast = statefulStream Nothing <@> \last next ->
  { emit: Just { last, next }, state: Just next }

dedupBy :: forall flow a. (a -> a -> Boolean) -> Stream flow a -> Lake a
dedupBy method = statefulStream Nothing <@> case _, _ of
  Just last, next | method last next ->
    { state: Just next, emit: Nothing }
  _, next ->
    { state: Just next, emit: Just next }

dedupOn :: forall flow a b. Eq b => (a -> b) -> Stream flow a -> Lake a
dedupOn f = statefulStream Nothing <@> case _, _ of
  Just last, next | f next == last ->
    { state: Just (f next), emit: Nothing }
  _, next ->
    { state: Just (f next), emit: Just next }

dedup :: forall flow a. Eq a => Stream flow a -> Lake a
dedup = dedupBy eq

-- dedupStore :: forall flow a. Eq a => River a -> River a
-- dedupStore (Stream _ f) = memoize $ Stream Flowing \cbs -> do


affToLake :: forall a. Aff a -> Lake (Maybe a)
affToLake aff = makeLake' \finished cb -> do
  fiber <- Aff.runAff (\result -> cb (hush result) <* finished) aff
  pure $ Aff.launchAff_ $ Aff.killFiber (Aff.error "event unsubscribed") fiber

interval :: Milliseconds -> Lake Int
interval (Milliseconds ms) = makeLake \cb -> do
  counted <- freshId
  clearInterval <$> setInterval (Int.floor ms) (cb =<< counted)

everyFrame :: Lake Int
everyFrame = makeLake \cb -> do
  counted <- freshId
  cancelRequest <- prealloc Nothing
  let
    next = cancelRequest.set <<< Just =<< requestAnimationFrame do
      hasRequest <- isJust <$> cancelRequest.get
      when hasRequest do
        cb =<< counted
        next
    destroy = do
      fold =<< cancelRequest.get
      cancelRequest.set Nothing
  destroy <$ next

animationLoop :: forall state out.
  state -> out ->
  Array (Lake (state -> Lake { state :: state, emit :: Maybe out })) ->
  Lake out
animationLoop s0 out actions =
  fixPrjBurst
    -- initial state, state projection
    (Just s0) (Just <<< _.state)
    -- initial output, output projection
    (Just out) _.emit
    -- process the state stream
    \prevState ->
      -- Keep track of the last state every time an action comes in
      (/\) <$> prevState <?*> oneStream actions
        -- And follow that action, until a new one arrives
        >>~ \(state /\ action) -> action state


-- | `risingFalling` maintains a `Set` internally so it can discard duplicate
-- | `keydown` events, and then this can be partitioned into the rising and
-- | falling events exactly once it is instantiated
risingFalling ::
  forall flow k r. Ord k =>
  ({ key :: k | r } -> Boolean) ->
  Stream flow { key :: k | r } ->
  Lake { key :: k | r }
risingFalling predicate = statefulStream Set.empty <@> \state r@{ key } ->
  let risen = predicate r in
  case Set.member key state == risen of
    true -> { state, emit: Nothing }
    false ->
      { state: (if risen then Set.insert else Set.delete) key state
      , emit: Just r
      }

data JoinLeave m = Join Int m | Leave Int

-- | `joinLeave` has a sparse array internally (`Map Int`), and it updates it to
-- | maintain a list of active values (e.g. audio nodes)
joinLeave ::
  forall flow1 flow2 m.
  Stream flow1 { value :: m, leave :: Stream flow2 Unit } ->
  Lake (Array m)
joinLeave upstream =
  let numbering = counter upstream in
  withInstantiated numbering \_ numbered -> do
    let
      leaves = allStreams numbered \({ leave } /\ i) ->
        Leave i <$ singleShot leave
      joins = numbered <#> \({ value } /\ i) -> do
        Join i value
      joinsLeaves = dam joins <|> leaves
    Array.fromFoldable <$> foldStream Map.empty joinsLeaves
      case _, _ of
        kvs, Join i m -> Map.insert i m kvs
        kvs, Leave i -> Map.delete i kvs

-- | `fallingLeaves` combines them and has a little glue to allocate values
-- | for each rising event (`false` -> `true`), and even lets it leave on its
-- | own (before a `keyup` event). The result is a running state of audio nodes
-- | that join and leave in response to external events.
fallingLeaves ::
  forall flow1 flow2 k r x y m. Ord k =>
  ({ key :: k | r } -> Either x y) ->
  Stream flow1 { key :: k | r } ->
  ({ key :: k | r } -> x -> River y -> Allocar { value :: m, leave :: Stream flow2 Unit }) ->
  Lake (Array m)
fallingLeaves p upstream f =
  withInstantiated (risingFalling (isLeft <<< p) upstream) \_ edges ->
    let
      p' r = case p r of
        Left x -> Left (r /\ x)
        Right y -> Right { key: r.key, value: y }
      { left: rising, right: falling } = partitionMap p' edges
      waitFor = mailboxRiver falling
      tracked = rising # mapAl \(r /\ x) -> do
        f r x (waitFor r.key)
    in joinLeave tracked

fallingLeavesAff ::
  forall flow1 flow2 k r x y m. Ord k =>
  ({ key :: k | r } -> Either x y) ->
  Stream flow1 { key :: k | r } ->
  ({ key :: k | r } -> x -> River y -> Aff { value :: m, leave :: Stream flow2 Unit }) ->
  Lake (Array m)
fallingLeavesAff p upstream f =
  withInstantiated (risingFalling (isLeft <<< p) upstream) \_ edges ->
    let
      p' r = case p r of
        Left x -> Left (r /\ x)
        Right y -> Right { key: r.key, value: y }
      { left: rising, right: falling } = partitionMap p' edges
      waitFor = mailboxRiver falling
      tracked = allStreams rising \(r /\ x) -> do
        compact $ affToLake $ f r x $ waitFor r.key
    in joinLeave tracked


documentEvent :: forall e.
  EventType ->
  (Event.Event -> Maybe e) ->
  (e -> Effect Unit) ->
  Allocar (Allocar Unit)
documentEvent eventType conv handle = do
  doc <- window >>= document
  eventListener
    { eventPhase: Bubbling
    , eventTarget: HTMLDocument.toEventTarget doc
    , eventType
    } \event -> case conv event of
        Just e -> handle e
        Nothing -> pure unit

data KeyPhase = KeyDown | KeyRepeat | KeyUp
derive instance Eq KeyPhase
derive instance Ord KeyPhase

type KeyEvent =
  { phase :: KeyPhase
  , key :: String
  , code :: String
  , location :: KeyboardEvent.KeyLocation
  , composing :: Boolean
  , event :: KeyboardEvent.KeyboardEvent
  , baseEvent :: Event.Event
  , mod :: { ctrl :: Boolean, alt :: Boolean, shift :: Boolean, meta :: Boolean }
  , targetType :: String
  , preventDefault :: Effect Unit
  , stopPropagation :: Effect Unit
  , stopImmediatePropagation :: Effect Unit
  }

keyPhase :: KeyboardEvent.KeyboardEvent -> KeyPhase
keyPhase event =
  case Event.type_ (KeyboardEvent.toEvent event), KeyboardEvent.repeat event of
    EventType "keydown", true -> KeyRepeat
    EventType "keydown", false -> KeyDown
    _, _ -> KeyUp

keyEvents ::
  (KeyEvent -> Effect Unit) ->
  Allocar (Allocar Unit)
keyEvents cb =
  [ "keydown", "keyup" ] #.. \ty ->
    documentEvent (EventType ty) KeyboardEvent.fromEvent \event -> do
      let baseEvent = KeyboardEvent.toEvent event
      cb
        { phase: keyPhase event
        , key: KeyboardEvent.key event
        , code: KeyboardEvent.code event
        , location: unsafePartial KeyboardEvent.location event
        , composing: KeyboardEvent.isComposing event
        , event
        , baseEvent
        , mod:
          { ctrl: KeyboardEvent.ctrlKey event
          , alt: KeyboardEvent.altKey event
          , shift: KeyboardEvent.shiftKey event
          , meta: KeyboardEvent.metaKey event
          }
        , targetType: Event.target baseEvent >>= Element.fromEventTarget # maybe "" (unwrap <<< Element.localName)
        , preventDefault: Event.preventDefault baseEvent
        , stopPropagation: Event.stopPropagation baseEvent
        , stopImmediatePropagation: Event.stopImmediatePropagation baseEvent
        }
