module Riverdragon.Dragon.Wings where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ResourceM (class MonadResource, destr)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Idiolect (filterFst, nonEmpty)
import Riverdragon.Dragon (AttrProp, Dragon(..), renderElSt)
import Riverdragon.Dragon.Bones (($$), ($~~), (.$), (.$$), (.$$~), (.<>), (:.), (:~), (<:>), (=!=), (=:=), (=?=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (Lake, Stream, createRiverStore, instantiate, limitTo, makeLake, oneStream)
import Riverdragon.River.Bed (eventListener, rolling)
import Riverdragon.River.Beyond (delay)
import Web.DOM.ElementId (ElementId)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.Event.EventPhase (EventPhase(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLInputElement as InputElement
import Web.HTML.Window (document)
import Widget (Interface)


listenInput :: Boolean -> ElementId -> Lake String
listenInput includeFirst id = makeLake \cb -> do
  mel <- liftEffect do getElementById id <<< HTMLDocument.toNonElementParentNode =<< document =<< window
  for_ (mel >>= InputElement.fromElement) \el -> do
    when includeFirst do liftEffect do cb =<< InputElement.value el
    eventListener
      { eventType: EventType "input"
      , eventPhase: None
      , eventTarget: InputElement.toEventTarget el
      } \_ -> do
        cb =<< InputElement.value el

instantiateListenInput :: forall flow m. MonadResource m => Boolean -> ElementId -> m (Stream flow String)
instantiateListenInput includeFirst id = _.stream <$> instantiate (listenInput includeFirst id)

vanishing :: Milliseconds -> Lake Dragon -> Lake Dragon
vanishing ms stream = stream <#> \element -> Replacing do
  pure element <|> limitTo 1 (delay ms (pure (mempty :: Dragon)))

inputValidated ::
  String ->
  String ->
  String ->
  String ->
  Lake String ->
  (String -> Effect Unit) ->
  Dragon
inputValidated cls label placeholder initialValue valid onInput =
  D.label
    [ D.className <:> "input-wrapper text" .<>
        (eq "" >>> if _ then "" else " invalid") <$> (pure "" <|> valid)
    ] $~~
    [ D.span.$$ label
    , D.input
        [ D.attr "placeholder" =?= nonEmpty placeholder
        , D.attr "value" =?= nonEmpty initialValue
        , D.onInputValue =:= onInput
        , D.className =:= cls
        ]
    , D.span :."error" .$$~ valid
    ]

-- this is opinionated, so it does not live in Bones
sourceCode :: forall flow. String -> Array (Stream flow AttrProp) -> Dragon -> Dragon
sourceCode lang attrs content =
  D.div :."sourceCode side-label "<>String.toLower lang :~
    [ D.data_"lang" =:= lang, oneStream attrs ] $
      D.pre.$ D.code.$ content


pushButtonRadio :: forall v. Eq v =>
  Interface v -> Array (v /\ Dragon) -> Dragon
pushButtonRadio { send, loopback } options =
  D.div :."tab-switcher-header".$ D.Fragment $ options <#> \(value /\ content) -> do
    D.button :."tab-switcher-tab":~
      [ D.onClick =!= send value
      , D.data_"selected" <:> eq value <$> loopback
      ] $ content

tabSwitcher :: Maybe String -> Array (String /\ Dragon) -> Dragon
tabSwitcher initial tabs = Egg do
  destroyLast <- liftEffect rolling
  destr do destroyLast mempty
  { stream: mounted, send: sendMounted } <- createRiverStore Nothing
  { stream: selected, send: select } <- createRiverStore initial
  let
    onMounted el = do
      sendMounted el
      for_ initial \tab -> do
        for_ (Array.findMap (filterFst (eq tab)) tabs) \content -> do
          destroyLast =<< renderElSt mounted content
  pure $ D.div :."tab-switcher".$ D.Fragment
    [ D.div :."tab-switcher-header".$ D.Fragment $ tabs <#> \(name /\ content) -> do
        D.button :."tab-switcher-tab":~
          [ D.onClick =!= destroyLast =<< renderElSt mounted content <* select name
          , D.data_"selected" <:> eq name <$> selected
          ] $$ name
    , D.div :."tab-switcher-contents":~ [ D.Self =:= \el -> mempty <$ onMounted el ] $ mempty
    ]

