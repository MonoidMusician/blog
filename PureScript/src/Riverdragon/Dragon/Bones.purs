module Riverdragon.Dragon.Bones ( module Riverdragon.Dragon.Bones, module ReExports ) where

import Prelude

import Control.Apply (lift2)
import Control.Plus (class Plus, empty, (<|>))
import Data.Array (fold)
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, traverse_)
import Data.Functor.App (App(..))
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.JSDate (JSDate)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Idiolect (type (-!>), (>==))
import Prelude as Prelude
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Riverdragon.Dragon (AttrProp(..), Dragon(..), PropVal(..))
import Riverdragon.Dragon (AttrProp(..), Dragon(..), PropVal(..)) as ReExports
import Riverdragon.River (Stream, dam, oneStream)
import Safe.Coerce (coerce)
import Stylish.Record (class WithSep, withSep, withoutSep)
import Stylish.Types (Classy(..), Stylish(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.ClassName (ClassName(..))
import Web.DOM.Element as Element
import Web.DOM.ElementId (ElementId(..))
import Web.DOM.ElementName (ElementName(..))
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.Internal.Types as Web
import Web.HTML.HTMLInputElement as HTMLInput
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Web.Util.TextCursor (TextCursor)
import Web.Util.TextCursor.Element as TC
import Web.Util.TextCursor.Element as TextCursorElement

--------------------------------------------------------------------------------
-- | ## Combinators georg                                                   | --
--------------------------------------------------------------------------------

replacingLeft :: forall a flow. (a -> Dragon) -> Stream flow a -> Dragon
replacingLeft f s = Replacing (f <$> dam s)
infixr 3 replacingLeft as @<

replacingRight :: forall a flow. Stream flow a -> (a -> Dragon) -> Dragon
replacingRight s f = Replacing (f <$> dam s)
infixl 1 replacingRight as >@

appendingLeft :: forall a flow. (a -> Dragon) -> Stream flow a -> Dragon
appendingLeft f s = Appending (f <$> dam s)
infixr 3 appendingLeft as ~~<
appendingRight :: forall a flow. Stream flow a -> (a -> Dragon) -> Dragon
appendingRight s f = Appending (f <$> dam s)
infixl 1 appendingRight as >~~

-- | `D.button[] $$~ "Some " .<> changing <>. " text"`
appendMap :: forall f b. Functor f => Semigroup b => b -> f b -> f b
appendMap m fm = (m <> _) <$> fm
infixr 0 appendMap as .<>
-- | `D.button[] $$~ "Some " .<> changling <>. " text"`
mapAppend :: forall f b. Functor f => Semigroup b => f b -> b -> f b
mapAppend fm m = fm <#> (_ <> m)
infixl 1 mapAppend as <>.

-- | `D.button[] $$~ "Some " $<> changingContent <>$ " and text"`
textAppendMap :: forall f. Functor f => String -> f Dragon -> f Dragon
textAppendMap m fm = (text m <> _) <$> fm
infixr 0 textAppendMap as $<>
-- | `D.button[] $$~ "Some " $<> changlingContent <>$ " and text"`
mapAppendText :: forall f. Functor f => f Dragon -> String -> f Dragon
mapAppendText fm m = fm <#> (_ <> text m)
infixl 1 mapAppendText as <>$


textAppend :: String -> Dragon -> Dragon
textAppend m fm = text m <> fm
infixr 0 textAppend as $<
appendText :: Dragon -> String -> Dragon
appendText fm m = fm <> text m
infixl 1 appendText as >$

-- | `D.ol [ D.id =:= "container" ] $~~ D.li[] <$> items`
ofFragment :: forall r. (Dragon -> r) -> Array Dragon -> r
ofFragment parent children = parent $ Fragment children
infixr 0 ofFragment as $~~ -- made to work with $ on left and <#> on right

-- | `D.span [ D.className =:= "myClass" ] $$ "its text content"`
_ofText :: forall r. (Dragon -> r) -> String -> r
_ofText fn = fn <<< text
infixr 0 _ofText as $$
-- | `D.span [] $$~ inputValueStream`
_ofTexting :: forall flow r. (Dragon -> r) -> Stream flow String -> r
_ofTexting fn = fn <<< Text <<< dam
infixr 0 _ofTexting as $$~

-- | `D.span.$ contents`
_noAttrs :: forall f a r. Plus f => (f a -> r) -> r
_noAttrs fn = fn empty
infixr 0 _noAttrs as .$
-- | `D.span.$$ "just a plain span"`
_noAttrsText :: forall f a r. Plus f => (f a -> Dragon -> r) -> String -> r
_noAttrsText fn = fn empty <<< text
infixr 0 _noAttrsText as .$$
-- | `D.span.$$~ "its text can " .<> change`
_noAttrsTexting :: forall flow f a r. Plus f => (f a -> Dragon -> r) -> Stream flow String -> r
_noAttrsTexting fn = fn empty <<< Text <<< dam
infixr 0 _noAttrsTexting as .$$~
-- | `D.div.$~~ [ x, y ]`
_noAttrsFragment :: forall f a r. Plus f => (f a -> Dragon -> r) -> Array Dragon -> r
_noAttrsFragment fn children = fn empty $~~ children
infixr 0 _noAttrsFragment as .$~~

-- | `D.span :."interestingClass" .$$ "its text content"`
_classy :: forall f r. Applicative f => (Array (f AttrProp) -> r) -> String -> Array (f AttrProp) -> r
_classy fn cls = fn <<< Array.cons (className =:= cls)
infixl 4 _classy as :. -- over <>
-- | `D.input :# "itsVeryImportant" :~[]`
_id'd :: forall f r. Applicative f => (Array (f AttrProp) -> r) -> String -> Array (f AttrProp) -> r
_id'd fn itsID = fn <<< Array.cons (id =:= itsID)
infixl 4 _id'd as :# -- over <>
-- | `D.a :@ "veritates.love" .$$ "Love, Verity"`
_hrefy :: forall f r. Applicative f => (Array (f AttrProp) -> r) -> String -> Array (f AttrProp) -> r
_hrefy fn whither = fn <<< Array.cons (attr "href" =:= whither)
infixl 4 _hrefy as :@ -- over <>
-- | `D.div :% "display: flow" $ contents`
_stylish :: forall f r. Applicative f => (Array (f AttrProp) -> r) -> String -> Array (f AttrProp) -> r
_stylish fn whither = fn <<< Array.cons (style =:= whither)
infixl 4 _stylish as :% -- over <>
-- | For ending a series of `:.`, `:#`, `:@`, `:!`, and so on, into an array
-- | of more attributes
-- |
-- | `D.a :@ "example.com" :~[ D.attr "target" "_blank" ] $$ "Example in new tab!"`
_moreAttrs :: forall i o. (i -> o) -> i -> o
_moreAttrs fn attrs = fn attrs
infixl 4 _moreAttrs as :~

--------------------------------------------------------------------------------
-- | ## Main functions                                                      | --
--------------------------------------------------------------------------------

-- | Static text node.
text :: String -> Dragon
text = Text <<< pure

-- | Show a static value.
show :: forall s. Show s => s -> Dragon
show = text <<< Prelude.show

-- | More polymorphic version of `Text`, for dynamic text nodes.
texting :: forall flow. Stream flow String -> Dragon
texting = Text <<< dam

--------------------------------------------------------------------------------
-- | ## Standard elements (attributes and children)                         | --
--------------------------------------------------------------------------------

html_ :: forall flow. String -> Array (Stream flow AttrProp) -> Dragon -> Dragon
html_ name attrs = Element Nothing name (dam (oneStream attrs))

p = html_"p" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
b = html_"b" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
i = html_"i" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
em = html_"em" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
sub = html_"sub" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
sup = html_"sup" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
strong = html_"strong" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

div = html_"div" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
pre = html_"pre" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
code = html_"code" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
span = html_"span" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

table = html_"table" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
tbody = html_"tbody" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
thead = html_"thead" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
tr = html_"tr" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
th = html_"th" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
td = html_"td" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

--------------------------------------------------------------------------------
-- | ## Elements with useful additions                                      | --
--------------------------------------------------------------------------------

ul = html_"ul" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
ol = html_"ol" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
li = html_"li" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

ulW :: forall flow. Array (Stream flow AttrProp) -> Array Dragon -> Dragon
ulW attrs items = ul attrs $~~ li[] <$> items
olW :: forall flow. Array (Stream flow AttrProp) -> Array Dragon -> Dragon
olW attrs items = ol attrs $~~ li[] <$> items

dl = html_"dl" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
dt = html_"dt" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
dd = html_"dd" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

dlW :: forall flow.
  Array (Stream flow AttrProp) ->
  Array { term :: Dragon, desc :: Dragon } -> Dragon
dlW attrs items = dl attrs $~~ items >>= \{ term, desc } -> [ dt.$ term, dd.$ desc ]

button = html_"button" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

-- | Class, description, and effect.
buttonW :: String -> String -> Effect Unit -> Dragon
buttonW cls descr cb = button :.cls :!cb .$$ descr

-- | Dynamic class and description, but still static effect.
buttonW' :: forall flow1 flow2. Stream flow1 String -> Stream flow2 String -> Effect Unit -> Dragon
buttonW' cls descr cb = button [ className <:> cls, onClick =!= cb ] $$~ descr

details = html_"details" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
summary = html_"summary" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

-- | Your details should include a summary.
detailsW :: forall flow. String -> Array (Stream flow AttrProp) -> Dragon -> Dragon
detailsW desc attrs contents = details attrs $~~ [ summary.$$ desc, contents ]

-- | Your details might include a dynamic summary.
detailsW' :: forall flow1 flow2. Stream flow1 String -> Array (Stream flow2 AttrProp) -> Dragon -> Dragon
detailsW' desc attrs contents = details attrs $~~ [ summary.$$~ desc, contents ]

a = html_"a" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

-- | Your `<a>` should probably include an `href=`. Or you could use `:@`.
aW :: forall flow. String -> Array (Stream flow AttrProp) -> Dragon -> Dragon
aW href attrs = a :@href :~attrs

--------------------------------------------------------------------------------
-- | ## Elements with attributes but no children                            | --
--------------------------------------------------------------------------------

br = html_"br" <@> mempty :: forall flow. Array (Stream flow AttrProp) -> Dragon

label = html_"label" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
input = html_"input" <@> mempty :: forall flow. Array (Stream flow AttrProp) -> Dragon
textarea = html_"textarea" <@> mempty :: forall flow. Array (Stream flow AttrProp) -> Dragon
script = html_"script" <@> mempty :: forall flow. Array (Stream flow AttrProp) -> Dragon

--------------------------------------------------------------------------------
-- | ## Attributes, properties, and listeners                               | --
--------------------------------------------------------------------------------

attr :: forall t. AttrType t => String -> t -> AttrProp
attr name val = Attr Nothing name $ attrType val

data_ :: forall t. AttrType t => String -> t -> AttrProp
data_ name val = Attr Nothing ("data-"<>name) $ attrType val

aria_ :: String -> String -> AttrProp
aria_ name val = Attr Nothing ("aria-"<>name) val

prop :: forall t. PropType t => String -> t -> AttrProp
prop name val = Prop name $ propType val


--------------------------------------------------------------------------------
-- | ## Combinators for attributes, properties, and listeners               | --
--------------------------------------------------------------------------------

-- | `D.button [ D.className =:= "big", D.onClick =:= someFunction ] $$ "Please click!"`
_tooPure :: forall t f a. Applicative f => (t -> a) -> t -> f a
_tooPure fn val = pure (fn val)
infixr 0 _tooPure as =:=
-- | `D.button [ D.className <:> loading <#> if _ then "disabled" else "" ] $$ "Load me"`
_alwaysWithYou :: forall f a b. Functor f => (a -> b) -> f a -> f b
_alwaysWithYou fn val = fn <$> val
infixr 0 _alwaysWithYou as <:>
_showsUp :: forall t f a. Plus f => Applicative f => (t -> a) -> Maybe t -> f a
_showsUp fn = maybe empty (_tooPure fn)
infixr 0 _showsUp as =?=
-- | `D.button [ D.onClick =!= someEfect ] $$ "Yeah"`
_listen :: forall f a b i. Applicative f => ((i -> a) -> b) -> a -> f b
_listen to it = pure (to (const it))
infixr 0 _listen as =!=
-- | `D.button [ D.onClick <!> setTo <$> valueStream ] $$ "Update"`
_listening :: forall f a b t. Functor f => ((t -> a) -> b) -> f a -> f b
_listening to it = to <<< const <$> it
infixr 0 _listening as <!>
_listenMaybe :: forall f a b i. Plus f => Applicative f => ((i -> a) -> b) -> Maybe a -> f b
_listenMaybe to = maybe empty \it -> pure (to (const it))
infixr 0 _listenMaybe as =!?=
-- | `D.button :!someEffect $$ "mhm"`
_listenClick :: forall f r. Applicative f => (Array (f AttrProp) -> r) -> Effect Unit -> Array (f AttrProp) -> r
_listenClick fn handler = fn <<< Array.cons (onClick =!= handler)
infixl 4 _listenClick as :!

-- | `HTMLInput.fromEventTarget ?. HTMLInput.value`
_gather :: forall a b m. Bind m => Applicative m => (Web.EventTarget -> Maybe a) -> (a -> m b) -> (b -> m Unit) -> Web.Event -> m Unit
_gather coe get = \fn e ->
  case Event.target e >>= coe of
    Just r -> r # (get >=> fn)
    Nothing -> pure unit
infix 6 _gather as ?. -- under <>
-- | `(Tuple HTMLInput.fromEventTarget HTMLInput.value) ?~> effectFnWithInput`
_gathering :: forall a b m. Bind m => Applicative m => Tuple (Web.EventTarget -> Maybe a) (a -> m b) -> (b -> m Unit) -> Web.Event -> m Unit
_gathering (Tuple coe get) fn = _gather coe get fn
infix 6 _gathering as ?~> -- under <>

__textcursor :: (TextCursor -!> Unit) -> Web.Event -!> Unit
__textcursor =
  (TextCursorElement.read <=< Element.fromEventTarget) ?. TextCursorElement.textCursor
__value :: (String -!> Unit) -> Web.Event -!> Unit
__value = fold
  -- idk, not really a better way to do this?
  [ HTMLInput.fromEventTarget ?. HTMLInput.value
  , HTMLTextArea.fromEventTarget ?. HTMLTextArea.value
  ]
__number :: (Number -!> Unit) -> Web.Event -!> Unit
__number = HTMLInput.fromEventTarget ?. HTMLInput.valueAsNumber
__int :: (Int -!> Unit) -> Web.Event -!> Unit
__int = HTMLInput.fromEventTarget ?. HTMLInput.valueAsNumber >== Int.round
__date :: (JSDate -!> Unit) -> Web.Event -!> Unit
__date = HTMLInput.fromEventTarget ?. HTMLInput.valueAsDate
__checked :: (Boolean -!> Unit) -> Web.Event -!> Unit
__checked = HTMLInput.fromEventTarget ?. HTMLInput.checked
__open :: (Boolean -!> Unit) -> Web.Event -!> Unit
__open = Element.fromEventTarget ?. Element.hasAttribute (AttrName "open")

_listenTextCursor :: forall f a b. Applicative f => ((b -> Web.Event -!> Unit) -> a) -> (TextCursor -!> Unit) -> f a
_listenTextCursor to it = to =!= __textcursor it
infixr 0 _listenTextCursor as =?|=
_listeningTextCursor :: forall f b t. Functor f => ((t -> Web.Event -!> Unit) -> b) -> f (TextCursor -!> Unit) -> f b
_listeningTextCursor to it = to <!> __textcursor <$> it
infixr 0 _listeningTextCursor as <?|>

_listenValue :: forall f a b. Applicative f => ((b -> Web.Event -!> Unit) -> a) -> (String -!> Unit) -> f a
_listenValue to it = to =!= __value it
infixr 0 _listenValue as =?$=
_listeningValue :: forall f b t. Functor f => ((t -> Web.Event -!> Unit) -> b) -> f (String -!> Unit) -> f b
_listeningValue to it = to <!> __value <$> it
infixr 0 _listeningValue as <?$>

_listenNumber :: forall f a b. Applicative f => ((b -> Web.Event -!> Unit) -> a) -> (Number -!> Unit) -> f a
_listenNumber to it = to =!= __number it
infixr 0 _listenNumber as =?#=
_listeningNumber :: forall f b t. Functor f => ((t -> Web.Event -!> Unit) -> b) -> f (Number -!> Unit) -> f b
_listeningNumber to it = to <!> __number <$> it
infixr 0 _listeningNumber as <?#>

_listenChecked :: forall f a b. Applicative f => ((b -> Web.Event -!> Unit) -> a) -> (Boolean -!> Unit) -> f a
_listenChecked to it = to =!= __checked it
infixr 0 _listenChecked as =??=
_listeningChecked :: forall f b t. Functor f => ((t -> Web.Event -!> Unit) -> b) -> f (Boolean -!> Unit) -> f b
_listeningChecked to it = to <!> __checked <$> it
infixr 0 _listeningChecked as <??>

class AttrType t where
  attrType :: t -> String
instance AttrType String where attrType = identity
instance AttrType Number where attrType = Prelude.show
instance AttrType Int where attrType = Prelude.show
instance AttrType Boolean where attrType = Prelude.show
instance AttrType ClassName where attrType (ClassName cls) = cls
instance AttrType Classy where attrType (Classy cls) = cls
instance AttrType Stylish where attrType (Stylish styl) = styl

class PropType t where
  propType :: t -> PropVal
instance PropType String where propType = PropString
instance PropType ClassName where propType (ClassName cls) = PropString cls
instance PropType Classy where propType (Classy cls) = PropString cls
instance PropType Stylish where propType (Stylish styl) = PropString styl
instance PropType Number where propType = PropNumber
instance PropType Int where propType = PropInt
instance PropType Boolean where propType = PropBoolean
instance PropType PropVal where propType = identity

class AutoDOM t r | t -> r where
  auto :: t -> r
instance AutoDOM ClassName AttrProp where
  auto = prop "className"
instance AutoDOM Classy AttrProp where
  auto = prop "className"
instance AutoDOM Stylish AttrProp where
  auto = prop "style"
instance TypeEquals i String => AutoDOM (i -> ClassName) (i -> AttrProp) where
  auto f = auto <<< f
instance AutoDOM ElementId AttrProp where
  auto = coerce id
instance TypeEquals i String => AutoDOM (i -> ElementId) (i -> AttrProp) where
  auto f = auto <<< f
instance AutoDOM EventType ((Web.Event -!> Unit) -> AttrProp) where
  auto = coerce on_
instance AutoDOM ElementName (Array (Stream flow AttrProp) -> Dragon -> Dragon) where
  auto = coerce html_
instance AutoDOM AttrName (String -> AttrProp) where
  auto = coerce (Attr Nothing)
instance AutoDOM String Dragon where
  auto = text
instance AutoDOM t r => AutoDOM (Stream flow t) (Stream flow r) where
  auto = map auto
instance AutoDOM (Array Dragon) Dragon where
  auto = Fragment

infixl 4 auto as &~

_autoClassy :: forall t f r. AutoDOM t (Array (f AttrProp) -> r) => Applicative f => t -> String -> Array (f AttrProp) -> r
_autoClassy fn cls = auto fn <<< Array.cons (className =:= cls)
infixl 4 _autoClassy as &. -- over <>
_autoId'd :: forall t f r. AutoDOM t (Array (f AttrProp) -> r) => Applicative f => t -> String -> Array (f AttrProp) -> r
_autoId'd fn itsID = auto fn <<< Array.cons (id =:= itsID)
infixl 4 _autoId'd as &# -- over <>
_autoHrefy :: forall t f r. AutoDOM t (Array (f AttrProp) -> r) => Applicative f => t -> String -> Array (f AttrProp) -> r
_autoHrefy fn whither = auto fn <<< Array.cons (attr "href" =:= whither)
infixl 4 _autoHrefy as &@ -- over <>
_autoStylish :: forall t f r. AutoDOM t (Array (f AttrProp) -> r) => Applicative f => t -> String -> Array (f AttrProp) -> r
_autoStylish fn whither = auto fn <<< Array.cons (style =:= whither)
infixl 4 _autoStylish as &% -- over <>

_pureAuto :: forall t f a i. Applicative f => AutoDOM t (i -> a) => t -> i -> f a
_pureAuto l r = pure (auto l r)
infixr 0 _pureAuto as =&=
_mapAuto :: forall t f i o. Functor f => AutoDOM t (i -> o) => t -> f i -> f o
_mapAuto = map <<< auto
infixr 0 _mapAuto as <&>

_test_auto0 = ElementName "custom" &~ [] $~~ [] :: Dragon
_test_auto1 = a [ AttrName "href" =&= "example.com" ] :: Dragon -> Dragon
_test_auto2 = span [ auto =:= ClassName "cls" ] mempty :: Dragon
_test_auto3 = input [ ClassName =&= mempty, ElementId =&= "theInput" ] :: Dragon

--------------------------------------------------------------------------------
-- | ## Standard attributes and properties                                  | --
--------------------------------------------------------------------------------


id = attr "id" :: String -> AttrProp
title = attr "title" :: String -> AttrProp
className = prop "className" <<< PropString :: String -> AttrProp
classy = prop "className" :: Classy -> AttrProp
style = prop "style" <<< PropString :: String -> AttrProp
stylish = prop "style" :: Stylish -> AttrProp
value = prop "value" :: forall t. PropType t => t -> AttrProp
placeholder = prop "placeholder" <<< PropString :: String -> AttrProp
checked = prop "checked" <<< PropBoolean :: Boolean -> AttrProp

textCursor :: TextCursor -> AttrProp
textCursor tc = Self \e -> do
  traverse_ (TC.setTextCursor tc) (TextCursorElement.read e)
  pure mempty

--------------------------------------------------------------------------------
-- | ## Standard listeners                                                  | --
--------------------------------------------------------------------------------

on_ :: String -> (Web.Event -!> Unit) -> AttrProp
on_ ty fn = Listener ty $ Just fn

off_ :: forall a. String -> a -> AttrProp
off_ ty _ = Listener ty Nothing

onClick = on_"click" :: (Web.Event -!> Unit) -> AttrProp

onInput = on_"input" :: (Web.Event -!> Unit) -> AttrProp
onInputTextCursor = onInput <<< __textcursor :: (TextCursor -!> Unit) -> AttrProp
onInputValue = onInput <<< __value :: (String -!> Unit) -> AttrProp
onInputNumber = onInput <<< __number :: (Number -!> Unit) -> AttrProp
onInputInt = onInput <<< __int :: (Int -!> Unit) -> AttrProp
onInputChecked = onInput <<< __checked :: (Boolean -!> Unit) -> AttrProp
onInputDate = onInput <<< __date :: (JSDate -!> Unit) -> AttrProp

onChange = on_"change" :: (Web.Event -!> Unit) -> AttrProp
onChangeTextCursor = onChange <<< __textcursor :: (TextCursor -!> Unit) -> AttrProp
onChangeValue = onChange <<< __value :: (String -!> Unit) -> AttrProp
onChangeNumber = onChange <<< __number :: (Number -!> Unit) -> AttrProp
onChangeInt = onChange <<< __int :: (Int -!> Unit) -> AttrProp
onChangeChecked = onChange <<< __checked :: (Boolean -!> Unit) -> AttrProp
onChangeDate = onChange <<< __date :: (JSDate -!> Unit) -> AttrProp

onToggle = on_"toggle" :: (Web.Event -!> Unit) -> AttrProp
-- | For `<details>` elements.
onToggleOpen = onToggle <<< __open :: (Boolean -!> Unit) -> AttrProp

--------------------------------------------------------------------------------
-- | ## Standard Namespace URIs                                             | --
--------------------------------------------------------------------------------

-- (yoinked from d3 i think?)

svgNS = "http://www.w3.org/2000/svg" :: String
xhtmlNS = "http://www.w3.org/1999/xhtml" :: String
xlinkNS = "http://www.w3.org/1999/xlink" :: String
xmlNS = "http://www.w3.org/XML/1998/namespace" :: String
xmlnsNS = "http://www.w3.org/2000/xmlns/" :: String

--------------------------------------------------------------------------------
-- | ## SVG stuff                                                           | --
--------------------------------------------------------------------------------

svg_ :: forall flow. String -> Array (Stream flow AttrProp) -> Dragon -> Dragon
svg_ name attrs = Element (Just svgNS) name (dam (oneStream attrs))

svg = svg_"svg" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon
g = svg_"g" :: forall flow. Array (Stream flow AttrProp) -> Dragon -> Dragon

-- `path` can have non-graphics children
path = svg_"path" <@> mempty :: forall flow. Array (Stream flow AttrProp) -> Dragon
d = attr "d" :: String -> AttrProp

pathW :: String -> Dragon
pathW pathD = path [ d =:= pathD ]

pathW' :: forall flow. Stream flow String -> Dragon
pathW' pathD = path [ d <:> pathD ]

circleW' :: forall flow1 flow2.
  Stream flow1
    { cx :: Number
    , cy :: Number
    , r :: Number
    } ->
  Array (Stream flow2 AttrProp) ->
  Dragon -> Dragon
circleW' measurements attrs = svg_"circle"
  [ dam measurements <#> attrsM
  , dam (oneStream attrs)
  ]

--------------------------------------------------------------------------------
-- | ## More typeclasses                                                    | --
--------------------------------------------------------------------------------

attrsM :: forall i o. HFoldlWithIndex MultiAttrs AttrProp i o => i -> o
attrsM = hfoldlWithIndex MultiAttrs (mempty :: AttrProp)

data MultiAttrs = MultiAttrs
instance multiAttrs :: (AttrType t, IsSymbol sym) =>
  FoldingWithIndex MultiAttrs (Proxy sym) AttrProp t AttrProp where
  foldingWithIndex MultiAttrs proxy acc val =
    acc <> attr (reflectSymbol proxy) val

propsM :: forall i o. HFoldlWithIndex MultiProps AttrProp i o => i -> o
propsM = hfoldlWithIndex MultiProps (mempty :: AttrProp)

data MultiProps = MultiProps
instance multiProps :: (PropType t, IsSymbol sym) =>
  FoldingWithIndex MultiProps (Proxy sym) AttrProp t AttrProp where
  foldingWithIndex MultiProps proxy acc val =
    acc <> prop (reflectSymbol proxy) val



withSeps :: forall o. WithSep o => List String -> o -> o
withSeps List.Nil = identity
withSeps (List.Cons "" r) = withSeps r
withSeps (List.Cons s r) = withSeps r <<< withSep s

withSeps1 :: forall o. WithSep o => List String -> o
withSeps1 List.Nil = withoutSep ""
withSeps1 (List.Cons "" r) = withSeps1 r
withSeps1 (List.Cons s r) = withoutSep s # withSeps r

smarts :: forall i o. Attrable i Identity o => i -> o
smarts = un Identity <<< smarties

smarties :: forall i m o. Attrable i m o => i -> m o
smarties = mkAttr List.Nil

class Attrable i (m :: Type -> Type) o where
  mkAttr :: List String -> i -> m o

instance attrableUnit :: (WithSep o, Applicative m) => Attrable Unit m o where
  mkAttr k (_ :: Unit) = pure (withSeps1 k)

instance attrableBoolean :: (Monoid o, WithSep o, Applicative m) => Attrable Boolean m o where
  mkAttr k = if _
    then mkAttr k unit
    else pure mempty

instance attrableWithStep :: (WithSep o, Applicative m) => Attrable String m o where
  mkAttr k v = pure (withSeps k (withoutSep v))

instance attrableStylish :: (Applicative m) => Attrable Stylish m Stylish where
  mkAttr k v = pure (withSeps k v)

instance attrableClassy :: (Applicative m) => Attrable Classy m Classy where
  mkAttr k v = pure (withSeps k v)

instance attrableRecord :: (RowToList is irl, AttrableRecord is irl m o) => Attrable (Record is) m o where
  mkAttr k = mkAttrRecord (Proxy :: Proxy irl) k

else instance attrableStream :: (Monoid o, TypeEquals flow1 flow2, Attrable i Identity o) => Attrable (Stream flow1 i) (Stream flow2) o where
  mkAttr k mi = pure mempty <|> do
    (coerce mi :: Stream flow2 i) <#> un Identity <<< mkAttr k

else instance attrableFunctor :: (Functor m, Attrable i Identity o) => Attrable (m i) m o where
  mkAttr k mi = mi <#> un Identity <<< mkAttr k

else instance attrableFoldable :: (Foldable f, Applicative m, Monoid o, Attrable i m o) => Attrable (f i) m o where
  mkAttr k = un App <<< foldMap (App <<< mkAttr k)

class AttrableRecord :: Row Type -> RowList Type -> (Type -> Type) -> Type -> Constraint
class AttrableRecord is irl m o where
  mkAttrRecord :: Proxy irl -> List String -> Record is -> m o

instance attrableRecordNil :: (Applicative m, Monoid o) => AttrableRecord is RL.Nil m o where
  mkAttrRecord _ _ _ = pure mempty

instance attrableRecordCons :: (IsSymbol k, Row.Cons k i is' is, AttrableRecord is irl m o, Attrable i m o, Applicative m, Monoid o) => AttrableRecord is (RL.Cons k i irl) m o where
  mkAttrRecord _ ks is = lift2 append
    do
      mkAttr (List.Cons (reflectSymbol (Proxy :: Proxy k)) ks) (Record.get (Proxy :: Proxy k) is :: i)
    do
      mkAttrRecord (Proxy :: Proxy irl) ks is

