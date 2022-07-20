module FRP.Deku where

import Prelude

import Control.Plus (empty)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Deku.Attribute (class Attr, Attribute, Cb, cb, (:=))
import Effect (Effect)
import FRP.Event (AnEvent, bang)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

bangAttr :: forall m a b e. Applicative m => Attr e a b => a -> b -> AnEvent m (Attribute e)
bangAttr a b = bang (a := b)

infixr 5 bangAttr as !:=

maybeAttr :: forall m a b e. Applicative m => Attr e a b => a -> Maybe b -> AnEvent m (Attribute e)
maybeAttr a (Just b) = bang (a := b)
maybeAttr _ Nothing = empty

infix 5 maybeAttr as ?:=

mapAttr :: forall m a b e. Functor m => Attr e a b => a -> m b -> m (Attribute e)
mapAttr a b = (a := _) <$> b

infix 5 mapAttr as <:=>

withValue :: (String -> Effect Unit) -> Cb
withValue fn = cb \e -> for_
  ( target e
      >>= fromEventTarget
  )
  (value >=> fn)
