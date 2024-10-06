module Riverdragon.Dragon.Bones where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Riverdragon.Dragon (Dragon(..))

div = Element Nothing "div" empty
text = Text <<< pure
ol = Element Nothing "ol" empty
li = Element Nothing "li" empty
b = Element Nothing "b" empty
i = Element Nothing "i" empty
