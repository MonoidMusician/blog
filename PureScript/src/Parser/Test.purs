module Parser.Test where

import Prelude

import Ansi.Codes as Ansi
import Ansi.Output (withGraphics)
import Data.Argonaut (Json)
import Data.Argonaut as J
import Data.Either (Either(..), either)
import Data.Enum (toEnum)
import Data.Foldable (fold, oneOf, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Number as Num
import Data.Number as Number
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Idiolect ((<#?>), (<$?>), (>==))
import Parser.Comb.Comber (Comber, arrayOf, delim, many, named, namedRec, objectOf, parse, parse', piggyback, printGrammarWsn, printStateTable, rawr, toAnsi, token, tokenPrecL, tokenPrecR, ws, wsOf, wsws)
import Parser.Debug (thingy)
import Safe.Coerce (coerce)
import Whitespace (ParseWS(..), mkParseWS)

main :: Effect Unit
main = do
  let space = mkParseWS { allowed_newline: false, allowed_space: true, required: false }
  let a_b = token "a" *> wsOf space <* token "b"
  log $ either identity show $ parse a_b "a b"
  pure unit

{-
space = wsOf $ WS.mkParseWS tt
nospace = wsOf $ WS.mkParseWS ff
spaceOpt = wsOf one

map (D.code[] <<< D.show) $ sourceOf $ (token "x") *> space *> do sourceOf do "R"#: token "y"
-}
{-
map (D.code[] <<< D.show) $ sourceOf $ (token "x") *> do
  sourceOf do space *> token "z" <* nospace <|> nospace *> token "z" <* space
-}
{-
map (D.code[] <<< D.show) $ sourceOf $ (\x -> spaceOpt *> x <* spaceOpt) $
  "expr" #-> \rec -> rawr "\\d+" <|>
    rec <> (spaceOpt *> (tokenPrecL "+" zero <|> tokenPrecR "*" one) <* spaceOpt) <> rec
-}
{-
import Parser.Languages as L
widget = livePrinterParser

newtype J = J (Json.Json)
instance Show J where
  show (J j) = Json.stringify j
dimap (\(J j) -> j) J $ IOL.json
-- IOL.jsonString

-- map (D.text <<< show) $ I.many "test" $ I.token "x"
-}


{-
T.text <$> "P2"#-> \p2 -> token "1" <|> (token "0" <> token "0" <> p2 <> p2) <|> (token "0" <> token "1" <> token "0" <> p2)

0000
-}
