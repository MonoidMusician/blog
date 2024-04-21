module Parser.Printer.Basics where

import Prelude

import Data.Number as Number
import Dodo as O
import Idiolect ((<$?>))
import Parser.Comb.Comber as I
import Parser.Printer.Types (PP, PrinterParser, printerParser')
import Parser.Printer.Types as IO

jsonNumber :: PP Number
jsonNumber = printerParser' (O.text <<< show) $
  Number.fromString <$?> I.rawr "[-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?"

haskellComma :: forall ann i o. String -> PrinterParser ann i o -> PrinterParser ann (Array i) (Array o)
haskellComma name pp = IO.manySepBy name sep pp
  where
  sep = IO.printerParser sepO sepI
  sepO = const $ O.softBreak <> O.text ", "
  sepI = void $ I.ws *> I.token "," <* I.ws
