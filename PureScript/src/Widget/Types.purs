module Widget.Types where

import Prelude

import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (Lake)

widgetFromEvent :: forall a. Lake a -> Dragon
widgetFromEvent e = D.Replacing $ mempty <$ e
