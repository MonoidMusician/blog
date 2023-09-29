module Parser.Languages where

import Prelude

import Ansi.Codes as Ansi
import Ansi.Output (withGraphics)
import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Data.Argonaut (Json)
import Data.Argonaut as J
import Data.Array (fromFoldable, nub, toUnfoldable)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Compactable (class Compactable, compact)
import Data.Either (Either(..), choose)
import Data.Enum (toEnum)
import Data.Foldable (fold, intercalate, oneOf, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Parser.Comb (Comb(..), named, namedRec, parseRegex, printSyntax, tokenRawr, tokenStr)
import Parser.Comb.Types (Rec)
import Parser.Examples (coalesce, showPart)
import Parser.Lexing (class ToString, type (~), Rawr, Similar(..))
import Parser.Types (Fragment, OrEOF, Part(..), Zipper(..))

type Comber = Comb (Rec String (OrEOF String) String) String (String ~ Rawr) String

rawr :: String -> Comber String
rawr = tokenRawr

key :: forall s. ToString s => s -> Comber String
key = tokenStr

mainName :: String
mainName = "main"

printPretty :: forall o a. Comb (Rec String (OrEOF String) String) String (String ~ Rawr) o a -> Effect Unit
printPretty (Comb { prettyGrammar }) = nub prettyGrammar #
  traverse_ \(name /\ msyntax) ->
    msyntax # traverse_ \syntax ->
      log $ showPart (NonTerminal name) <> " = " <> printSyntax showFragment syntax <> " ."

showFragment' :: Fragment String (String ~ Rawr) -> String
showFragment' =
  toUnfoldable >>> coalesce >>> fromFoldable >>>
    map showPart >>> intercalate " "

showFragment :: Fragment String (String ~ Rawr) -> String
showFragment = showFragment' >>> case _ of
  "" -> showPart (Terminal (Similar (Left "")))
  r -> r

showZipper :: Zipper String (String ~ Rawr) -> String
showZipper (Zipper l r) = intercalate " • " [ showFragment' l, showFragment' r ]

digit :: Comber Int
digit = named "digit" $ oneOf $ mapWithIndex (<$) $
  tokenStr <$> String.toCodePointArray "0123456789"

int :: Comber Int
int = Int.fromString <$?> rawr "\\d+"

number :: Comber Number
number = named "number" do
  Number.fromString <$?> rawr "[-]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?"

compactMap :: forall f a b. Compactable f => Functor f => (a -> Maybe b) -> f a -> f b
compactMap = map compact <<< map
infixl 4 compactMap as <$?>

compactMapFlipped :: forall f a b. Compactable f => Functor f => f a -> (a -> Maybe b) -> f b
compactMapFlipped = flip compactMap
infixl 1 compactMapFlipped as <#?>

-- optional whitespace needs to be optional at the parser combinator level, not
-- the regex level (where it would both conflict with nonempty whitespace, and
-- more seriously allow empty parses), nor the LR level (where it would be
-- locked behind a reduction even though the point is that it does not matter
-- to anything consuming it)
ws :: Comber Unit
ws = wss <|> pure unit

wss :: Comber Unit
wss = "wss"#: void (rawr "\\s+")

delim :: forall s a. ToString s => s -> s -> Comber a -> Comber a
delim x y z = key x *> z <* key y

wsws :: Comber ~> Comber
wsws a = ws *> a <* ws

wsws' :: forall a. Comber a -> Comber (Maybe a)
wsws' a = oneOf
  [ Nothing <$ wss
  , Just <$> wsws a
  ]

test :: Comber String -> Array String -> Effect Unit
test parser testData = do
  log ""
  log "Grammar:"
  printPretty (named mainName parser)
  let doParse = parseRegex mainName parser
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

--many :: forall a. String -> Comber a -> Comber (Array a)
many :: forall t308 rec309 nt310 cat311 o312. Ord nt310 => nt310 -> Comb rec309 nt310 cat311 o312 t308 -> Comb rec309 nt310 cat311 o312 (Array t308)
many n p = Array.fromFoldable <$> n #-> \more ->
  pure Nil <|> lift2 Cons p more

manyL :: forall t308 rec309 nt310 cat311 o312. Ord nt310 => nt310 -> Comb rec309 nt310 cat311 o312 t308 -> Comb rec309 nt310 cat311 o312 (Array t308)
manyL n p = Array.reverse <<< Array.fromFoldable <$> n #-> \more ->
  pure Nil <|> lift2 (flip Cons) more p


manySepBy :: forall x a. String -> Comber x -> Comber a -> Comber (Array a)
manySepBy n s p = map Array.fromFoldable $
  pure Nil <|> lift2 Cons p do
    n #-> \more -> pure Nil <|> s *> lift2 Cons p more

manyBaseSepBy :: forall x y a. String -> Comber x -> Comber y -> Comber a -> Comber (Array a)
manyBaseSepBy n b s p = map Array.fromFoldable $
  Nil <$ b <|> lift2 Cons p do
    n #-> \more -> pure Nil <|> s *> lift2 Cons p more

many1 :: forall a. String -> Comber a -> Comber (NonEmptyArray a)
many1 n p = NEA.fromFoldable1 <$> n #-> \more ->
  lift2 NEL.cons' p (pure Nil <|> NEL.toList <$> more)

many1SepBy :: forall x a. String -> Comber x -> Comber a -> Comber (NonEmptyArray a)
many1SepBy n s p = NEA.fromFoldable1 <$> n #-> \more ->
  lift2 NEL.cons' p (pure Nil <|> NEL.toList <$> (s *> more))

opt :: forall a. Comber a -> Comber (Maybe a)
opt a = pure Nothing <|> Just <$> a

mopt :: forall a. Monoid a => Comber a -> Comber a
mopt a = pure mempty <|> a

string :: Comber String
string = named "string" $ join delim "\"" $
  map fold $ many "string_contents" $ oneOf
    -- rawr "[\\u0020-\\u10FFFF]" also works
    -- the only reason we need to exclude \ and " is because of the
    -- repetition operator, which is good for efficiency
    [ rawr "[\\u0020-\\u0021\\u0023-\\u005B\\u005C-\\u10FFFF]+"
    , key "\\" *> oneOf
      [ key "\"" $> "\""
      , key "\\" $> "\\"
      , key "/" $> "/"
      , key "n" $> "\n"
      , key "r" $> "\r"
      , key "t" $> "\t"
      , key "b" $> "\x8"
      , key "f" $> "\xC"
      , key "u" *> rawr "[0-9a-fA-F]{4}" <#?> do
          Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
      ]
    ]

infixr 2 named as #:
infixr 5 namedRec as #->

composeMap :: forall f a b c. Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
composeMap f g = map f <<< g
infixr 9 composeMap as ==<

composeMapFlipped :: forall f a b c. Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
composeMapFlipped f g = f >>> map g
infixr 9 composeMapFlipped as >==

infixr 5 choose as \|/

tupling :: forall f a b. Applicative f => f a -> f b -> f (a /\ b)
tupling = lift2 Tuple
infixr 6 tupling as /|\

theseing :: forall f a b. Alternative f => f a -> f b -> f (a /\/ b)
theseing a b = This <$> a <|> That <$> b <|> Both <$> a <*> b
infixr 6 theseing as /\\/
infixr 6 type These as /\/


arrayOf :: forall a. Comber a -> Comber (Array a)
arrayOf value = delim "[" "]" do
  manyBaseSepBy "array_contents" ws (key ",") value

objectOf :: forall a. Comber a -> Comber (Array (String /\ a))
objectOf value = delim "{" "}" do
  manyBaseSepBy "object_contents" ws (key ",") do
    "object_entry"#: Tuple <$> wsws string <* key ":" <*> value

-- Comb Combinator

json :: Comber Json
json = namedRec "value" \value -> wsws $ oneOf
  [ string <#> J.fromString
  , number <#> J.fromNumber
  , key "true" $> J.fromBoolean true
  , key "false" $> J.fromBoolean false
  , key "null" $> J.jsonNull
  , "array"#: arrayOf value <#> J.fromArray
  , "object"#: objectOf value <#> Object.fromFoldable >>> J.fromObject
  ]
