module Riverdragon.Dragon.Wings where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldMap, for_, traverse_)
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Effect.Timer (clearTimeout, setTimeout)
import Effect.Uncurried (EffectFn3)
import Idiolect ((==<))
import Prim.Boolean (False, True)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Breath (microtask)
import Riverdragon.River (Stream, createStreamBurst, instantiate, limitTo, makeStream, mapCb, subscribe)
import Riverdragon.River.Bed (Allocar(..), breaker, eventListener, ordSet, storeLast, whenMM)
import Safe.Coerce (coerce)
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.ElementId (ElementId(..))
import Web.DOM.Node (Node, parentNode, removeChild)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.Event.EventPhase (EventPhase(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLInputElement as InputElement
import Web.HTML.Window (cancelAnimationFrame, document, requestAnimationFrame)

listenInput :: Boolean -> ElementId -> Stream False String
listenInput includeFirst id = makeStream \cb -> do
  mel <- getElementById id <<< HTMLDocument.toNonElementParentNode =<< document =<< window
  mel >>= InputElement.fromElement # foldMap \el -> do
    when includeFirst do cb =<< InputElement.value el
    eventListener
      { eventType: EventType "input"
      , eventPhase: None
      , eventTarget: InputElement.toEventTarget el
      } \_ -> do
        cb =<< InputElement.value el

instantiateListenInput :: Boolean -> ElementId -> Allocar (Stream True String)
instantiateListenInput includeFirst id = _.stream <$> instantiate (listenInput includeFirst id)

-- delay :: Milliseconds -> Stream False ~> Stream False
-- delay (Milliseconds ms) stream = makeStream \cb -> do
--   subscribe stream do void <<< setTimeout (Int.round ms) <<< cb


delay :: forall flow. Milliseconds -> Stream flow ~> Stream flow
delay (Milliseconds ms) = mapCb \upstream -> do
  -- TODO: arena
  inflight <- ordSet
  let
    receive value = do
      idStore <- storeLast
      id <- setTimeout (Int.round ms) do
        idStore.get >>= traverse_ inflight.remove
        upstream.receive value
      _ <- inflight.set id
      _ <- idStore.set id
      pure unit
  for_ upstream.burst receive
  pure
    { receive: receive
    , unsubscribe: mempty $ inflight.reset >>= traverse_ clearTimeout
    , burst: []
    , destroyed: void do
        -- We do not care about canceling this
        setTimeout (Int.round ms) upstream.destroyed
    }

-- delayMicro :: forall flow. Stream flow ~> Stream flow
-- delayMicro = overCb \cb -> do
--   -- Microtasks have no canceler, so we have to drop events ourselves
--   brk <- breaker cb
--   pure
--     { receive: microtask <<< brk.run
--     , unsubscribe: brk.trip
--     }

-- delayAnim :: forall flow. Stream flow ~> Stream flow
-- delayAnim = overCb \cb -> do
--   -- TODO: arena
--   inflight <- ordSet
--   w <- window
--   pure
--     { receive: \value -> do
--         idStore <- storeLast
--         id <- flip requestAnimationFrame w do
--           idStore.get >>= traverse_ inflight.remove
--           cb value
--         _ <- inflight.set id
--         _ <- idStore.set id
--         pure unit
--     , unsubscribe: inflight.reset >>= traverse_ (cancelAnimationFrame <@> w)
--     }

vanishing :: Milliseconds -> Stream False Dragon -> Stream False Dragon
vanishing ms stream = stream <#> \element -> Replacing do
  pure element <|> limitTo 1 (delay ms (pure mempty))
