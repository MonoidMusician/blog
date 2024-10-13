module Parser.Printer.Basics where

import Prelude

import Ansi.Codes as Ansi
import Data.Int (binary, hexadecimal, octal)
import Data.Int as Int
import Data.Number as Number
import Dodo as O
import Idiolect ((<$?>))
import Parser.Comb.Comber as I
import Parser.Printer.Types (PP, PrinterParser, newAnn, printerParser')
import Parser.Printer.Types as IO

jsonNumber :: PP Number
jsonNumber = printerParser' (O.text <<< show) $
  Number.fromString <$?> I.rawr "[-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?"

intDec :: PP Int
intDec = printerParser' (O.text <<< show) $
  Int.fromString <$?> I.rawr "\\d+"

intHex :: PP Int
intHex = printerParser' (O.text <<< Int.toStringAs hexadecimal) $
  Int.fromStringAs hexadecimal <$?> I.rawr "[0-9a-fA-F]+"

intBin :: PP Int
intBin = printerParser' (O.text <<< Int.toStringAs binary) $
  Int.fromStringAs binary <$?> I.rawr "[01]+"

intOct :: PP Int
intOct = printerParser' (O.text <<< Int.toStringAs octal) $
  Int.fromStringAs octal <$?> I.rawr "[0-7]+"


haskellComma :: forall i o. String -> PrinterParser i o -> PrinterParser (Array i) (Array o)
haskellComma name pp = IO.manySepBy name sep pp
  where
  sep = IO.printerParser sepO sepI
  sepO = const $ O.softBreak <> O.text ", "
  sepI = void $ I.ws *> I.token "," <* I.ws

ansi :: Ansi.GraphicsParam -> forall i o. PrinterParser i o -> PrinterParser i o
ansi cmd = IO._doc $ O.annotate $ newAnn _ { ansi = pure cmd }

colorful :: Ansi.Color -> forall i o. PrinterParser i o -> PrinterParser i o
colorful = ansi <<< Ansi.PForeground
