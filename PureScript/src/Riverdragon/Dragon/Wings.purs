module Riverdragon.Dragon.Wings where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldMap, for_, traverse_)
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn3)
import Idiolect ((==<))
import Prim.Boolean (False, True)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.River (Stream, ambivalent, createStreamBurst, limitTo, makeStream, subscribe)
import Riverdragon.River.Bed (Allocar(..), eventListener, whenMM)
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

listenInputEither ::
  Boolean ->
  ElementId ->
  Tuple
    (Stream False String)
    (Effect
      { destroy :: Effect Unit
      , stream :: Stream True String
      }
    )
listenInputEither includeFirst id = ambivalent do
  mel <- getElementById id <<< HTMLDocument.toNonElementParentNode =<< document =<< window
  mel >>= InputElement.fromElement # foldMap \el -> pure
    { burst: whenMM (pure includeFirst) do pure <$> InputElement.value el
    , subscribe: \cb -> do
        eventListener
          { eventType: EventType "input"
          , eventPhase: None
          , eventTarget: InputElement.toEventTarget el
          } \_ -> do
            cb =<< InputElement.value el
    }

listenInput :: Boolean -> ElementId -> Stream False String
listenInput includeFirst id = fst $ listenInputEither includeFirst id

instantiateListenInput :: Boolean -> ElementId -> Allocar (Stream True String)
instantiateListenInput includeFirst id = map _.stream $ snd $ listenInputEither includeFirst id

delay :: Milliseconds -> Stream False ~> Stream False
delay (Milliseconds ms) stream = makeStream \cb -> do
  subscribe stream do void <<< setTimeout (Int.round ms) <<< cb

vanishing :: Milliseconds -> Stream False Dragon -> Stream False Dragon
vanishing ms stream = stream <#> \element -> Replacing do
  pure element <|> limitTo 1 (delay ms (pure mempty))
