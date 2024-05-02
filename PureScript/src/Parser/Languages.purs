module Parser.Languages where

import Prelude

import Ansi.Codes as Ansi
import Ansi.Output (withGraphics)
import Data.Argonaut (Json)
import Data.Argonaut as J
import Data.Either (Either(..))
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
import Parser.Comb.Comber (Comber, arrayOf, delim, many, named, namedRec, objectOf, parse', piggyback, printGrammarWsn, printStateTable, rawr, toAnsi, token, tokenPrecL, tokenPrecR, ws, wsws)
import Parser.Debug (thingy)

digit :: Comber Int
digit = named "digit" $ oneOf $ mapWithIndex (<$) $
  token <$> String.toCodePointArray "0123456789"

int :: Comber Int
int = Int.fromString <$?> rawr "\\d+"

number :: Comber Number
number = named "number" do
  Number.fromString <$?> rawr "[-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?"

test :: Boolean -> Comber String -> Array String -> Effect Unit
test false _ _ = pure unit
test _enabled parser testData = do
  log ""
  log "Grammar:"
  log $ printGrammarWsn toAnsi parser
  let (stateTable /\ doParse) = parse' parser
  when false do
    log $ printStateTable toAnsi stateTable
  log ""
  log "Examples:"
  traverse_ (\s -> pure unit >>= \_ -> log (colorful Ansi.BrightYellow (show s) <> "\n  " <> result (doParse s))) testData

colorful :: Ansi.Color -> String -> String
colorful c = withGraphics (pure (Ansi.PForeground c))

result :: Either String String -> String
result (Left e) = colorful Ansi.Red "✗ " <> String.replaceAll (String.Pattern "\n") (String.Replacement "\n    ") e
result (Right r) = colorful Ansi.Green "✓ " <> String.replaceAll (String.Pattern "\n") (String.Replacement "\n    ") r

main :: Effect Unit
main = do
  _ <- pure thingy
  test true (show <$> number)
    [ "0"
    , "00"
    , "0.1"
    , "12340.234"
    , "12e5"
    ]
  test true (J.stringify <$> json)
    [ "true"
    , "false"
    , "null"
    , """ 123 """
    , """ "" """
    , """ "é✗" """
    , """
      "1234é"
      """
    , """
      "1234é✗"
      """
    , """ "\t\\\n" """
    , """ "\z" """
    , """ "\u" """
    , """ "\" """
    , "\"\"\""
    , "[ ]"
    , "[ 1, 2 ,3]"
    , "{}"
    , """{"a":1, "b":[1,23,4,5]}"""
    , "[,]"
    ]
  test true (show <$> arithmetic)
    [ "1 + 2"
    , "4 - 2 + 3"
    , "5 +3* 2"
    , "3^4 \\/ 5"
    ]

stringParser :: Comber String
stringParser = join delim "\"" $
  map fold $ many "string_contents" $ oneOf
    -- `rawr "[\\u0020-\\u10FFFF]"` also works
    -- the only reason we need to exclude \ U+005C and " U+0022 is because of
    -- the repetition operator, which is good for efficiency
    [ rawr "[\\u0020-\\u0021\\u0023-\\u005B\\u005D-\\u{10FFFF}]+"
    , token "\\" *> oneOf
      [ token "\"" $> "\""
      , token "\\" $> "\\"
      , token "/" $> "/"
      , token "n" $> "\n"
      , token "r" $> "\r"
      , token "t" $> "\t"
      , token "b" $> "\x8"
      , token "f" $> "\xC"
      , token "u" *> rawr "[0-9a-fA-F]{4}" <#?> do
          Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
      ]
    ]

string :: Comber String
string = piggyback
  { pattern: rawr "\"([^\\\\\"]|\\\\.)*\""
  , errors: ["Invalid string literal"]
  , name: "string_parser"
  } stringParser

infixr 2 named as #:
infixr 5 namedRec as #->

json :: Comber Json
json = namedRec "value" \value -> wsws $ oneOf
  [ string <#> J.fromString
  , number <#> J.fromNumber
  , token "true" $> J.fromBoolean true
  , token "false" $> J.fromBoolean false
  , token "null" $> J.jsonNull
  , "array"#: arrayOf "array_contents" value <#> J.fromArray
  , "object"#: objectOf "object_contents" "object_entry" string value <#> Object.fromFoldable >>> J.fromObject
  ]

arithmetic :: Comber Number
arithmetic = ws *> namedRec "arithmetic" \expr -> oneOf
  [ number <* ws
  , token '(' *> ws *> expr <* token ')' <* ws
  , ado
      x <- expr
      op <- oneOf
        [ (+) <$ tokenPrecL '+' zero
        , (-) <$ tokenPrecL '-' zero
        , (*) <$ tokenPrecL '*' one
        , (/) <$ tokenPrecL '/' one
        , Num.pow <$ tokenPrecR '^' (one + one)
        , Num.min <$ tokenPrecL "/\\" (negate one)
        , Num.max <$ tokenPrecL "\\/" (negate one + negate one)
        ] <* ws
      y <- expr
      in op x y
  ]
