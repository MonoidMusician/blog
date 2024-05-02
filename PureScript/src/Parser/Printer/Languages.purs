module Parser.Printer.Languages where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Number as Number
import Data.String as S
import Dodo as O
import Idiolect ((<$?>))
import Parser.Comb.Comber as I
import Parser.Printer.Types as IO

jsonNumber :: IO.PP Number
jsonNumber = IO.named "number" $ IO.printerParser'
  do O.text <<< apply fromMaybe (S.stripSuffix (S.Pattern ".0")) <<< show
  do Number.fromString <$?> I.rawr "[-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?"

-- O.break
