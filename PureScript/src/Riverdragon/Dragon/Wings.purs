module Riverdragon.Dragon.Wings where

import Prelude

import Control.Alt ((<|>))
import Control.Extend (extend)
import Control.Monad.ResourceM (class MonadResource, destr)
import Control.Plus (empty)
import Data.Array as Array
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String as String
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Idiolect (filterFst, nonEmpty, withIndices, (>==))
import Riverdragon.Dragon (AttrProp, Dragon(..), renderElSt)
import Riverdragon.Dragon.Bones (($$), ($~~), (.$), (.$$), (.$$~), (.<>), (:.), (:~), (<:>), (=!=), (=:=), (=?=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (Lake, River, Stream, createRiverStore, createStore, limitTo, makeLake, oneStream, singleShot)
import Riverdragon.River as River
import Riverdragon.River.Bed (eventListener, rolling)
import Riverdragon.River.Beyond (delay, withLast)
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
      } \_ -> cb =<< InputElement.value el

storeListenInput :: forall flow m. MonadResource m => Boolean -> ElementId -> m (Stream flow String)
storeListenInput includeFirst id = _.stream <$> River.store (listenInput includeFirst id)

vanishing :: Milliseconds -> Lake Dragon -> Lake Dragon
vanishing ms stream = stream <#> \element -> Replacing do
  pure element <|> singleShot (delay ms (pure (mempty :: Dragon)))

deletable :: forall m. MonadResource m => Dragon -> m Dragon
deletable = createStore >=> \{ stream, send, destroy } -> do
  destr $ send mempty *> destroy
  pure $ D.Replacing stream

-- | View an array stream as a live array of DOM nodes, such that they are only
-- | created and deleted at the end of the DOM fragment, and each update goes to
-- | the individual nodeʼs persistent renderer.
-- |
-- | This is basically a mini version of VDOM diffing, just for arrays. It does
-- | not have any logic for referential identity/keyed children, it just treats
-- | elements by index.
liveArray :: forall flow r. Stream flow (Array r) -> (Int -> River r -> Dragon) -> Dragon
liveArray itemsStream render = Egg do
  -- Create a `River` here since we will listen to it multiple times
  -- and want to see the same events
  { burst, stream } <- River.store itemsStream
  let
    -- | Include the burst only here, to kick off `addingItems`.
    startItems :: River (Array r)
    startItems = maybe empty pure (Array.last burst) <|> stream

    -- | The event stream of new items, with a dedicated listener.
    addingItems :: Lake (Tuple Int (River (Maybe r)))
    addingItems = withLast startItems # River.mapArray do
      newItems >== extend \(Tuple i added) ->
        pure (Just added) <|> (stream <#> (_ Array.!! i))

    -- | Look for new items at the end of the array.
    newItems :: { last :: Maybe (Array r), next :: Array r } -> Array (Tuple Int r)
    newItems { last, next } = Array.drop (maybe 0 Array.length last) $ withIndices next

    -- | Render the river of `Just`s until it returns `Nothing`.
    renderUntilNothing :: Tuple Int (River (Maybe r)) -> Dragon
    renderUntilNothing (Tuple i itemUpdates) =
      Replacing $ pure (render i stillPresent) <|> limitTo 1 (mempty <$ wentAway)
      where
      stillPresent = compact itemUpdates :: River r
      wentAway = filter isNothing itemUpdates

  -- The combination of `Appending` with inner `Replacing` produces a list
  -- we can append and delete items from.
  pure $ Appending $ renderUntilNothing <$> addingItems

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

