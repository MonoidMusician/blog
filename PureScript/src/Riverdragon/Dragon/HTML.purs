module Riverdragon.Dragon.HTML where

import Prelude

import Data.Foldable (foldMap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Riverdragon.Dragon (Dragon)

-- dragonToHTML :: Dragon -> String
-- dragonToHTML = case _ of
--   Fragment fragment -> foldMap dragonToHTML fragment
--   Element mns el (Lake AttrProp) Dragon
--   | Text (Lake String)

escapeHTML :: String -> String
escapeHTML = identity
  >>> String.replaceAll (Pattern "&") (Replacement "&amp;")
  >>> String.replaceAll (Pattern "<") (Replacement "&lt;")
  >>> String.replaceAll (Pattern ">") (Replacement "&gt;")
  >>> String.replaceAll (Pattern "\"") (Replacement "&quot;")
  >>> String.replaceAll (Pattern "'") (Replacement "&#039;")
