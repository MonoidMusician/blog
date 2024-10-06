module Riverdragon.Dragon.Wings where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldMap, for_, traverse_)
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn3)
import Prim.Boolean (False, True)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.River (Stream, createStreamBurst, makeStream, subscribe)
import Riverdragon.River.Bed (Allocar(..), eventListener)
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
import Web.HTML.Window (document)

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

instantiateListenInput :: Boolean -> ElementId -> Effect (Stream True String)
instantiateListenInput includeFirst id = do
  mel <- getElementById id <<< HTMLDocument.toNonElementParentNode =<< document =<< window
  { event, send } <- createStreamBurst do
    mel >>= InputElement.fromElement # foldMap \el -> do
      if includeFirst then pure <$> InputElement.value el else pure []
  mel >>= InputElement.fromElement # traverse_ \el -> do
    eventListener
      { eventType: EventType "input"
      , eventPhase: None
      , eventTarget: InputElement.toEventTarget el
      } \_ -> do
        send =<< InputElement.value el
  pure event

delay :: Milliseconds -> Stream False ~> Stream False
delay (Milliseconds ms) stream = makeStream \cb -> do
  subscribe stream do void <<< setTimeout (Int.round ms) <<< cb

vanishing :: Milliseconds -> Stream False Dragon -> Stream False Dragon
vanishing ms stream = stream <#> \element -> Replaceable do
  pure element <|> mempty <$ delay ms (pure unit)
