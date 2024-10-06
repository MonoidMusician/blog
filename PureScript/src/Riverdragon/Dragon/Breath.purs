module Riverdragon.Dragon.Breath where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, EffectFn4, runEffectFn4)
import Web.DOM (Element)
import Web.DOM.AttrName (AttrName)
import Web.DOM.NamespaceURI (NamespaceURI)
import Web.DOM.Node (Node, parentNode, removeChild)

removeSelf :: Node -> Effect Unit
removeSelf node = do
  parentNode node >>= traverse_ (removeChild node)

foreign import unsafeSetProperty :: forall v. EffectFn3 Element String v Unit

setAttributeNS :: Maybe NamespaceURI -> AttrName -> String -> Element -> Effect Unit
setAttributeNS = runEffectFn4 _setAttributeNS <<< toNullable
foreign import _setAttributeNS :: EffectFn4 (Nullable NamespaceURI) AttrName String Element Unit

foreign import microtask :: Effect Unit -> Effect Unit
