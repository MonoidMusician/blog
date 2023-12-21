module Widget.Types where

import Prelude

import Bolson.Core (Entity(..))
import Bolson.Core as BC
import Control.Plus (empty)
import Data.Foldable (oneOfMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Deku.Attribute (AttributeValue(..), unsafeAttribute)
import Deku.Control as DC
import Deku.Core (Domable(..), Nut, envy, fixed, unsafeSetPos)
import Effect (Effect)
import FRP.Event (Event)
import Web.DOM as Web
import Web.DOM.Attr (getValue, localName)
import Web.DOM.Element (attributes)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.NamedNodeMap (getAttributes)
import Web.DOM.Node (nodeName)
import Web.DOM.ParentNode (children)

newtype SafeNut = SafeNut Nut

instance Semigroup SafeNut where
  append (SafeNut n1) (SafeNut n2) = SafeNut (n1 <> n2)
instance Monoid SafeNut where
  mempty = SafeNut mempty

widgetFromEvent :: forall a. Event a -> SafeNut
widgetFromEvent e = SafeNut (envy (fixed empty <$ e))

snapshot :: Web.Element -> Effect SafeNut
snapshot e = do
  let n = Element.toNode e
  attrs <- attributes e >>= getAttributes >>= traverse \attr -> do
    value <- getValue attr
    pure $ unsafeAttribute { key: unwrap (localName attr), value: Prop' value }
  kids <- children (Element.toParentNode e) >>= HTMLCollection.toArray >>= traverse snapshot
  pure $ SafeNut do
    Domable $ Element' $ DC.elementify (nodeName n) (oneOfMap pure attrs) $
      Domable $ BC.fixed $ kids #
        mapWithIndex \i (SafeNut kid) -> do
          unwrap (unsafeSetPos i kid)
