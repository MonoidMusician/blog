module Conversions.Index where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Dodo (Doc)
import Parser.Comb.Comber (Comber)

conversions :: Map String (Comber (Doc Void))
conversions = Map.empty
