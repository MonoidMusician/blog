module Riverdragon.Dragon.Wings where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Idiolect (filterFst, nonEmpty, (#..), (==<))
import Riverdragon.Dragon (AttrProp, Dragon(..), renderElSt)
import Riverdragon.Dragon.Bones (($$), ($~~), (.$), (.$$), (.$$~), (.<>), (:.), (:~), (<:>), (=!=), (=:=), (=?=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (Lake, Stream, createRiverStore, instantiate, limitTo, makeLake, oneStream)
import Riverdragon.River as River
import Riverdragon.River.Bed (Allocar, accumulator, eventListener, rolling)
import Riverdragon.River.Bed as Bed
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

type Shell =
  { track :: forall r.
      Allocar { destroy :: Allocar Unit | r } ->
      Allocar { destroy :: Allocar Unit | r }
  , inst :: forall flowIn flowOut a. Stream flowIn a -> Allocar (Stream flowOut a)
  , store :: forall flowIn flowOut a. Stream flowIn a -> Allocar (Stream flowOut a)
  , storeLast :: forall flow a. a -> Stream flow a -> Allocar (Allocar a)
  , subscribe :: forall flow a. Stream flow a -> (a -> Effect Unit) -> Allocar Unit
  , destructor :: Allocar Unit -> Allocar Unit
  }

hatching :: (Shell -> Allocar Dragon) -> Dragon
hatching cont = Egg do
  destructors <- accumulator
  let
    track :: forall r.
      Allocar { destroy :: Allocar Unit | r } ->
      Allocar { destroy :: Allocar Unit | r }
    track act = act >>= \r -> r <$ destructors.put r.destroy

    inst :: forall flowIn flowOut a. Stream flowIn a -> Allocar (Stream flowOut a)
    inst = _.stream ==< track <<< River.instantiate

    store :: forall flowIn flowOut a. Stream flowIn a -> Allocar (Stream flowOut a)
    store = _.stream ==< track <<< River.store

    storeLast :: forall flow a. a -> Stream flow a -> Allocar (Allocar a)
    storeLast df stream = do
      lastValue <- Bed.prealloc df
      destructors.put =<< River.subscribe stream lastValue.set
      pure lastValue.get

    subscribe :: forall flow a. Stream flow a -> (a -> Effect Unit) -> Allocar Unit
    subscribe stream cb = destructors.put =<< River.subscribe stream cb

    release dragon = destructors.get <#> { destroy: _, dragon }
  release =<< cont
    { track
    , inst
    , store
    , storeLast
    , subscribe
    , destructor: destructors.put
    }



listenInput :: Boolean -> ElementId -> Lake String
listenInput includeFirst id = makeLake \cb -> do
  mel <- getElementById id <<< HTMLDocument.toNonElementParentNode =<< document =<< window
  mel >>= InputElement.fromElement #.. \el -> do
    when includeFirst do cb =<< InputElement.value el
    eventListener
      { eventType: EventType "input"
      , eventPhase: None
      , eventTarget: InputElement.toEventTarget el
      } \_ -> do
        cb =<< InputElement.value el

instantiateListenInput :: forall flow. Boolean -> ElementId -> Allocar (Stream flow String)
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
tabSwitcher initial tabs = hatching \shell -> do
  destroyLast <- rolling
  { stream: mounted, send: sendMounted } <- shell.track $ createRiverStore Nothing
  { stream: selected, send: select } <- shell.track $ createRiverStore initial
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

