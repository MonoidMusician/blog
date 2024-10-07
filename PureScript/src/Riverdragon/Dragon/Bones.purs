module Riverdragon.Dragon.Bones where

import Prelude

import Control.Plus (empty)
import Data.Foldable (for_, oneOf)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Riverdragon.Dragon (AttrProp(..), Dragon(..), PropVal(..))
import Riverdragon.River (Lake)
import Web.Event.Event as Event
import Web.Event.Internal.Types as Web
import Web.HTML.HTMLInputElement as HTMLInput

_el :: String -> Dragon -> Dragon
_el name = Element Nothing name empty
_el' :: String -> Array (Lake AttrProp) -> Dragon -> Dragon
_el' name = Element Nothing name <<< oneOf

text :: String -> Dragon
text = Text <<< pure

button' :: Array (Lake AttrProp) -> Dragon -> Dragon
button' = _el' "button"
button :: Dragon -> Dragon
button = _el "button"
input' :: Array (Lake AttrProp) -> Dragon
input' = _el' "input" <@> mempty
label' :: Array (Lake AttrProp) -> Dragon -> Dragon
label' = _el' "label"
code :: Dragon -> Dragon
code = _el "code"
span' :: Array (Lake AttrProp) -> Dragon -> Dragon
span' = _el' "span"
span :: Dragon -> Dragon
span = _el "span"
pre' :: Array (Lake AttrProp) -> Dragon -> Dragon
pre' = _el' "pre"
div :: Dragon -> Dragon
div = _el "div"
pre :: Dragon -> Dragon
pre = _el "pre"
br' :: Array (Lake AttrProp) -> Dragon
br' = _el' "br" <@> mempty
ol :: Dragon -> Dragon
ol = _el "ol"
li :: Dragon -> Dragon
li = _el "li"
b :: Dragon -> Dragon
b = _el "b"
i :: Dragon -> Dragon
i = _el "i"

attr :: String -> String -> AttrProp
attr name val = Attr Nothing name val

className :: Lake String -> Lake AttrProp
className = map $ Prop "className" <<< PropString

style :: Lake String -> Lake AttrProp
style = map $ Prop "style" <<< PropString

onClickF :: (Web.Event -> Effect Unit) -> Lake AttrProp
onClickF e = pure $ Listener "click" $ Just e
onClickE :: Effect Unit -> Lake AttrProp
onClickE e = pure $ Listener "click" $ Just $ const e

onInputF :: (Web.Event -> Effect Unit) -> Lake AttrProp
onInputF e = pure $ Listener "input" $ Just e
onInputE :: Effect Unit -> Lake AttrProp
onInputE e = pure $ Listener "input" $ Just $ const e

onValueInput :: (String -> Effect Unit) -> Lake AttrProp
onValueInput fn = pure $ Listener "input" $ Just \e -> for_
  ( Event.target e
      >>= HTMLInput.fromEventTarget
  )
  (HTMLInput.value >=> fn)

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"
xhtmlNS :: String
xhtmlNS = "http://www.w3.org/1999/xhtml"
xlinkNS :: String
xlinkNS = "http://www.w3.org/1999/xlink"
xmlNS :: String
xmlNS = "http://www.w3.org/XML/1998/namespace"
xmlnsNS :: String
xmlnsNS = "http://www.w3.org/2000/xmlns/"
