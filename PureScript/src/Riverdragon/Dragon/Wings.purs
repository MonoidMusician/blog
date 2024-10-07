module Riverdragon.Dragon.Wings where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (foldMap)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Idiolect ((==<))
import Prim.Boolean (False, True)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (Lake, Stream, instantiate, instantiateStore, limitTo, makeStream, subscribe)
import Riverdragon.River.Bed (Allocar, accumulator, eventListener)
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

type Shell =
  { track :: forall r.
      Allocar { destroy :: Allocar Unit | r } ->
      Allocar { destroy :: Allocar Unit | r }
  , inst :: forall flow a. Lake a -> Allocar (Stream flow a)
  , instStore :: forall flow a. Lake a -> Allocar (Stream flow a)
  , storeLast :: forall a. a -> Lake a -> Allocar (Allocar a)
  , destructor :: Allocar Unit -> Allocar Unit
  }

eggy ::
  ( Shell -> Allocar Dragon
  ) -> Dragon
eggy cont = Egg do
  destructors <- accumulator
  let
    track :: forall r.
      Allocar { destroy :: Allocar Unit | r } ->
      Allocar { destroy :: Allocar Unit | r }
    track act = act >>= \r -> r <$ destructors.put r.destroy
    inst :: forall flow a. Lake a -> Allocar (Stream flow a)
    inst = _.stream ==< track <<< instantiate
    instStore :: forall flow a. Lake a -> Allocar (Stream flow a)
    instStore = _.stream ==< track <<< instantiateStore
    storeLast :: forall a. a -> Lake a -> Allocar (Allocar a)
    storeLast df stream = do
      lastValue <- Bed.prealloc df
      destructors.put =<< subscribe stream (void <<< lastValue.set)
      pure lastValue.get
    release dragon = destructors.read <#> { destroy: _, dragon }
  release =<< cont { track, inst, instStore, storeLast, destructor: destructors.put }



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

vanishing :: Milliseconds -> Stream False Dragon -> Stream False Dragon
vanishing ms stream = stream <#> \element -> Replacing do
  pure element <|> limitTo 1 (delay ms (pure mempty))

inputValidated ::
  String ->
  String ->
  String ->
  String ->
  Lake String ->
  (String -> Effect Unit) ->
  Dragon
inputValidated cls label placeholder initialValue valid onInput =
  D.label'
    [ D.className' $ (append "input-wrapper text" <<< (eq "" >>> if _ then "" else " invalid")) <$> (pure "" <|> valid) ]
    $ Fragment
    [ D.span (D.text label)
    , D.input'
        [ D.attr' "placeholder" $ if placeholder == "" then empty else pure placeholder
        , D.attr' "value" $ if initialValue == "" then empty else pure initialValue
        , D.onInputValue onInput
        , D.className cls
        ]
    , D.span' [ D.className "error" ] (Text valid)
    ]
