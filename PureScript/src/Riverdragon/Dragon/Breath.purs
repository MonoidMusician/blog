module Riverdragon.Dragon.Breath where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Uncurried (EffectFn3)
import Web.DOM (Element)
import Web.DOM.Node (Node, parentNode, removeChild)

removeSelf :: Node -> Effect Unit
removeSelf node = do
  parentNode node >>= traverse_ (removeChild node)

foreign import unsafeSetProperty :: forall v. EffectFn3 Element String v Unit
