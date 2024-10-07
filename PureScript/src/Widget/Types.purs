module Widget.Types where

import Prelude

import Control.Plus (empty)
import Data.Foldable (oneOfMap)
import Data.Traversable (traverse)
import Deku.Core (Nut, envy, fixed)
import Effect (Effect)
import FRP.Event (Event)
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones as D
import Safe.Coerce (coerce)
import Web.DOM as Web
import Web.DOM.Attr (getValue)
import Web.DOM.Attr as Attr
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.Element (attributes)
import Web.DOM.Element as Element
import Web.DOM.ElementName (ElementName(..))
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.NamedNodeMap (getAttributes)
import Web.DOM.NamespaceURI (NamespaceURI(..))
import Web.DOM.ParentNode (children)

newtype SafeNut = SafeNut Nut

instance Semigroup SafeNut where
  append (SafeNut n1) (SafeNut n2) = SafeNut (n1 <> n2)
instance Monoid SafeNut where
  mempty = SafeNut mempty

widgetFromEvent :: forall a. Event a -> SafeNut
widgetFromEvent e = SafeNut (envy (fixed empty <$ e))

snapshot :: Web.Element -> Effect Dragon
snapshot e = do
  attrs <- attributes e >>= getAttributes >>= traverse \attr -> do
    value <- getValue attr
    pure $ D.Attr (coerce Attr.namespaceURI attr) (coerce Attr.localName attr) value
  kids <- children (Element.toParentNode e) >>= HTMLCollection.toArray >>= traverse snapshot
  pure $ D.Element
    (coerce Element.namespaceURI e)
    (coerce Element.tagName e)
    (oneOfMap pure attrs)
    (D.Fragment kids)
