module Sillylect where

import Prelude

import Control.Semigroupoid (composeFlipped)
import Data.Either (Either(..))
import Data.Ord (greaterThanOrEq, lessThanOrEq)
import Data.Tuple (fst)

-- π̀ = fst
-- π́ = snd
ὶ = Left
ί = Right

infixr 9 compose as ∘
infixr 9 composeFlipped as ⨟
infixr 9 compose as ⋘
infixr 9 composeFlipped as ⋙

infixl 4 lessThanOrEq as ≤
infixl 4 greaterThanOrEq as ≥
