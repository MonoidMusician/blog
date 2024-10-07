module Widget.Types where

import Prelude

import Data.Foldable (oneOfMap)
import Data.Traversable (traverse)
import Effect (Effect)
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (Lake)
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

widgetFromEvent :: forall a. Lake a -> Dragon
widgetFromEvent e = D.Replacing $ mempty <$ e

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
