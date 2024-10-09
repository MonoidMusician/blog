module Riverdragon.Dragon.Bones ( module Riverdragon.Dragon.Bones, module ReExports ) where

import Prelude

import Control.Plus (empty)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Riverdragon.Dragon (AttrProp(..), Dragon(..), PropVal(..))
import Riverdragon.Dragon (AttrProp(..), Dragon(..), PropVal(..)) as ReExports
import Riverdragon.River (Lake, oneStream)
import Web.Event.Event as Event
import Web.Event.Internal.Types as Web
import Web.HTML.HTMLInputElement as HTMLInput
import Web.HTML.HTMLTextAreaElement as HTMLTextArea

replacingLeft f s = Replacing (f <$> s)
infixr 3 replacingLeft as @<

replacingRight s f = Replacing (f <$> s)
infixl 1 replacingRight as >@

appendingLeft f s = Appending (f <$> s)
infixr 3 appendingLeft as ~~<
appendingRight s f = Appending (f <$> s)
infixl 1 appendingRight as >~~


_el :: String -> Dragon -> Dragon
_el name = Element Nothing name empty
_el' :: String -> Array (Lake AttrProp) -> Dragon -> Dragon
_el' name = Element Nothing name <<< oneStream

text :: String -> Dragon
text = Text <<< pure

button' :: Array (Lake AttrProp) -> Dragon -> Dragon
button' = _el' "button"
button :: Dragon -> Dragon
button = _el "button"
input' :: Array (Lake AttrProp) -> Dragon
input' = _el' "input" <@> mempty
textarea' :: Array (Lake AttrProp) -> Dragon
textarea' = _el' "textarea" <@> mempty
code :: Dragon -> Dragon
code = _el "code"
code' :: Array (Lake AttrProp) -> Dragon -> Dragon
code' = _el' "code"
span' :: Array (Lake AttrProp) -> Dragon -> Dragon
span' = _el' "span"
span :: Dragon -> Dragon
span = _el "span"
pre' :: Array (Lake AttrProp) -> Dragon -> Dragon
pre' = _el' "pre"
div' :: Array (Lake AttrProp) -> Dragon -> Dragon
div' = _el' "div"
div :: Dragon -> Dragon
div = _el "div"
pre :: Dragon -> Dragon
pre = _el "pre"
br' :: Array (Lake AttrProp) -> Dragon
br' = _el' "br" <@> mempty
br :: Dragon
br = _el "br" mempty
ul :: Dragon -> Dragon
ul = _el "ul"
ol :: Dragon -> Dragon
ol = _el "ol"
li :: Dragon -> Dragon
li = _el "li"
ul' = _el' "ul"
ol' = _el' "ol"
li' = _el' "li"
dl = _el "dl"
dt = _el "dt"
dd = _el "dd"
dl' = _el' "dl"
dt' = _el' "dt"
dd' = _el' "dd"
b :: Dragon -> Dragon
b = _el "b"
i :: Dragon -> Dragon
i = _el "i"
em = _el "em"
p = _el "p"
p' = _el' "p"
sub = _el "sub"
sup = _el "sup"
label = _el "label"
label' = _el' "label"

table = _el "table"
table' = _el' "table"
tbody = _el "tbody"
tbody' = _el' "tbody"
thead = _el "thead"
thead' = _el' "thead"
tr = _el "tr"
tr' = _el' "tr"
th = _el "th"
th' = _el' "th"
td = _el "td"
td' = _el' "td"

aN :: String -> Array (Lake AttrProp) -> Dragon -> Dragon
aN href attrs = _el' "a" [ attr "href" href, oneStream attrs ]

script' :: Array (Lake AttrProp) -> Dragon
script' = _el' "script" <@> mempty

attr :: String -> String -> Lake AttrProp
attr name val = pure $ Attr Nothing name val

attr' :: String -> Lake String -> Lake AttrProp
attr' name val = Attr Nothing name <$> val

data_ :: String -> String -> Lake AttrProp
data_ name val = pure $ Attr Nothing ("data-"<>name) val

data' :: String -> Lake String -> Lake AttrProp
data' name val = Attr Nothing ("data-"<>name) <$> val

aria_ :: String -> String -> Lake AttrProp
aria_ name val = pure $ Attr Nothing ("aria-"<>name) val

aria' :: String -> Lake String -> Lake AttrProp
aria' name val = Attr Nothing ("aria-"<>name) <$> val

prop :: forall t. AutoProp t => String -> t -> Lake AttrProp
prop name val = pure $ Prop name $ autoProp val

prop' :: forall t. AutoProp t => String -> Lake t -> Lake AttrProp
prop' name val = Prop name <<< autoProp <$> val

class AutoProp t where
  autoProp :: t -> PropVal
instance AutoProp String where autoProp = PropString
instance AutoProp Number where autoProp = PropNumber
instance AutoProp Int where autoProp = PropInt
instance AutoProp Boolean where autoProp = PropBoolean

id :: String -> Lake AttrProp
id = attr "id"

className :: String -> Lake AttrProp
className = pure <<< Prop "className" <<< PropString

className' :: Lake String -> Lake AttrProp
className' = map $ Prop "className" <<< PropString

style :: String -> Lake AttrProp
style = pure <<< Prop "style" <<< PropString

style' :: Lake String -> Lake AttrProp
style' = map $ Prop "style" <<< PropString

value :: String -> Lake AttrProp
value = pure <<< Prop "value" <<< PropString

value' :: Lake String -> Lake AttrProp
value' = map $ Prop "value" <<< PropString

placeholder :: String -> Lake AttrProp
placeholder = pure <<< Prop "placeholder" <<< PropString

placeholder' :: Lake String -> Lake AttrProp
placeholder' = map $ Prop "placeholder" <<< PropString

on_ :: String -> (Web.Event -> Effect Unit) -> Lake AttrProp
on_ ty fn = pure $ Listener ty $ Just fn

onClickF :: (Web.Event -> Effect Unit) -> Lake AttrProp
onClickF e = pure $ Listener "click" $ Just e
onClickE :: Effect Unit -> Lake AttrProp
onClickE e = pure $ Listener "click" $ Just $ const e
onClickE' :: Lake (Effect Unit) -> Lake AttrProp
onClickE' = map $ Listener "click" <<< Just <<< const

onInputF :: (Web.Event -> Effect Unit) -> Lake AttrProp
onInputF e = pure $ Listener "input" $ Just e
onInputE :: Effect Unit -> Lake AttrProp
onInputE e = pure $ Listener "input" $ Just $ const e

onInputValue :: (String -> Effect Unit) -> Lake AttrProp
onInputValue fn = pure $ Listener "input" $ Just \e -> do
  for_
    ( Event.target e
        >>= HTMLInput.fromEventTarget
    )
    (HTMLInput.value >=> fn)
  for_
    ( Event.target e
        >>= HTMLTextArea.fromEventTarget
    )
    (HTMLTextArea.value >=> fn)

onInputNumber :: (Number -> Effect Unit) -> Lake AttrProp
onInputNumber fn = pure $ Listener "input" $ Just \e -> do
  for_
    ( Event.target e
        >>= HTMLInput.fromEventTarget
    )
    (HTMLInput.valueAsNumber >=> fn)

onInputNumber' :: Lake (Number -> Effect Unit) -> Lake AttrProp
onInputNumber' = map \fn -> Listener "input" $ Just \e -> do
  for_
    ( Event.target e
        >>= HTMLInput.fromEventTarget
    )
    (HTMLInput.valueAsNumber >=> fn)

onChangeF :: (Web.Event -> Effect Unit) -> Lake AttrProp
onChangeF e = pure $ Listener "change" $ Just e
onChangeE :: Effect Unit -> Lake AttrProp
onChangeE e = pure $ Listener "change" $ Just $ const e

onChangeValue :: (String -> Effect Unit) -> Lake AttrProp
onChangeValue fn = pure $ Listener "change" $ Just \e -> do
  for_
    ( Event.target e
        >>= HTMLInput.fromEventTarget
    )
    (HTMLInput.value >=> fn)
  for_
    ( Event.target e
        >>= HTMLTextArea.fromEventTarget
    )
    (HTMLTextArea.value >=> fn)

buttonN :: String -> Effect Unit -> String -> Dragon
buttonN cls cb descr = button'
  [ className cls
  , onClickE cb
  ]
  (text descr)

buttonN' :: String -> Effect Unit -> Lake String -> Dragon
buttonN' cls cb descr = button'
  [ className cls
  , onClickE cb
  ]
  (Text descr)

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
