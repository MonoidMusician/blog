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
import Data.Number as Number
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Idiolect ((<#?>), (<$?>), (>==))
import Parser.Comb.Comber (Comber, delim, many, manyBaseSepBy, named, namedRec, parse, printGrammarWsn, rawr, toAnsi, token, ws, wsws)

digit :: Comber Int
digit = named "digit" $ oneOf $ mapWithIndex (<$) $
  token <$> String.toCodePointArray "0123456789"

int :: Comber Int
int = Int.fromString <$?> rawr "\\d+"

number :: Comber Number
number = named "number" do
  Number.fromString <$?> rawr "[-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?"

test :: Comber String -> Array String -> Effect Unit
test parser testData = do
  log ""
  log "Grammar:"
  log $ printGrammarWsn toAnsi parser
  let doParse = parse parser
  log ""
  log "Examples:"
  traverse_ (\s -> pure unit >>= \_ -> log (colorful Ansi.BrightYellow (show s) <> "\n  " <> result (doParse s))) testData

colorful :: Ansi.Color -> String -> String
colorful c = withGraphics (pure (Ansi.PForeground c))

result :: Either String String -> String
result (Left e) = colorful Ansi.Red "✗ " <> e
result (Right r) = colorful Ansi.Green "✓ " <> r

main :: Effect Unit
main = do
  test (show <$> number)
    [ "0"
    , "00"
    , "0.1"
    , "12340.234"
    , "12e5"
    ]
  test (J.stringify <$> json)
    [ "true"
    , "false"
    , "null"
    , """ 123 """
    -- , """ "" """
    -- , """ "é✗" """
    -- , """
    --   "1234é"
    --   """
    -- , """
    --   "1234é✗"
    --   """
    , """ "\t\\\n" """
    , """ "\" """
    , "\"\"\""
    , "[ ]"
    , "[ 1, 2 ,3]"
    , "{}"
    , """{"a":1, "b":[1,23,4,5]}"""
    , "[,]"
    ]

string :: Comber String
string = named "string" $ join delim "\"" $
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

infixr 2 named as #:
infixr 5 namedRec as #->

arrayOf :: forall a. String -> Comber a -> Comber (Array a)
arrayOf name value = delim "[" "]" do
  manyBaseSepBy name ws (token ",") value

objectOf :: forall k a. String -> String -> Comber k -> Comber a -> Comber (Array (k /\ a))
objectOf name entry keyP value = delim "{" "}" do
  manyBaseSepBy name ws (token ",") do
    entry#: Tuple <$> wsws keyP <* token ":" <*> value

-- Comb Combinator

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
