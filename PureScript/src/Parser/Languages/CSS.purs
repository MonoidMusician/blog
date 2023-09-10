module Parser.Languages.CSS where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Argonaut as J
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BooleanAlgebra.CSS (AttrMatch(..), MatchValue(..), MatchValueType(..), Relation(..), Select(..))
import Data.Enum (toEnum)
import Data.Foldable (fold, oneOf)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Parser.Languages (type (/\/), Comber, delim, key, many, many1, many1SepBy, mopt, opt, rawr, test, wss, wsws, (#:), (/\\/), (/|\), (<#?>), (<<>>), (>==))
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  test (unsafeCoerce J.stringify <$> compound_selector)
    [ "::before"
    , ".haskell"
    , "h1#title"
    , "h1 a"
    , "blockquote > p"
    , "body"
    ]

-- Lexer definitions

comment :: Comber String
comment = "comment"#: delim "/*" "*/" do rawr "."
newline :: Comber String
newline = "newline"#: rawr "\\r\\n|[\\n\\r\\f]"
escape :: Comber String
escape = "escape"#: key "\\" *> oneOf
  [ rawr "[^0-9a-fA-F\\r\\n\\f]"
  , rawr "[0-9a-fA-F]{1,6}\\s?" <#?> do
      String.trim >>> Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
  ]
ident :: Comber String
ident = "ident"#:
  oneOf
    [ key "--"
    , mopt (key "-") <<>>
      (rawr "[a-zA-Z_]" <|> const empty escape)
    ]
  <<>> ident_continue
ident_continue :: Comber String
ident_continue = fold <$> many "ident-continue" do
  rawr "[-a-zA-Z0-9_]+" <|> const empty escape
function :: Comber String
function = ident <* key "("
at_keyword :: Comber String
at_keyword = key "@" *> ident
hash :: Comber String
hash = key "#" *> ident_continue
string :: Comber String
string = "string"#: oneOf
  [ join delim "\"" $ fold <$> many "double-string-contents" do
      rawr "[^\\\\\"\\n\\r\\f]+" <|> escape <|> key "\\" *> newline
  , join delim "\'" $ fold <$> many "single-string-contents" do
      rawr "[^\\\\'\\n\\r\\f]+" <|> escape <|> key "\\" *> newline
  ]
url :: Comber String
url = "url"#: key "url" *> delim "(" ")" do
  wsws $ fold <$> many "url-contents" do
    -- TODO: non-printable
    rawr "[^\\\\\"'()\\s]+" <|> escape

number :: Comber String
number = "number"#: rawr "[-+]?(?:\\d+(?:\\.\\d+)|\\.\\d+)(?:[eE][-+]?\\d+)?"

dimension :: Comber (String /\ String)
dimension = "dimension"#: number /|\ ident

percentage :: Comber String
percentage = "percentage"#: number <<>> key "%"

-- Parser definitions


-- Uhh restart

many1Comma :: forall a. String -> Comber a -> Comber (NonEmptyArray a)
many1Comma = many1SepBy <@> key ","

selector_list :: Comber (NonEmptyArray _)
selector_list = "selector-list"#: complex_selector_list
complex_selector_list :: Comber (NonEmptyArray _)
complex_selector_list = many1Comma "complex-selector-list" complex_selector
compound_selector_list :: Comber (NonEmptyArray _)
compound_selector_list = many1Comma "compound-selector-list" compound_selector
simple_selector_list :: Comber (NonEmptyArray _)
simple_selector_list = many1Comma "simple-selector-list" simple_selector
relative_selector_list :: Comber (NonEmptyArray _)
relative_selector_list = many1Comma "relative-selector-list" relative_selector
complex_selector :: Comber (NonEmptyArray _)
complex_selector = many1SepBy "complex-selector" combinator compound_selector
relative_selector :: Comber _
relative_selector = "relative-selector"#: combinator /|\ complex_selector
compound_selector :: Comber (String /\/ NonEmptyArray Select /\/ NonEmptyArray (Select /\ Array Select))
compound_selector = "compound-selector"#:
  type_selector
    /\\/ many1 "subclass_selectors" subclass_selector
    /\\/ many1 "pseudo_selectors"
      ( "pseudo_selector"#: pseudo_element_selector
        /|\ many "pseudo_class_selectors" pseudo_class_selector
      )
simple_selector :: Comber Select
simple_selector = Element <$> type_selector <|> subclass_selector
combinator :: Comber Relation
combinator = "combinator"#: oneOf
  [ wsws $ oneOf
    [ Child <$ key ">"
    , Next <$ key "+"
    , Later <$ key "~"
    ]
  , Child <$ wss
  ]
type_selector = "type-selector"#: wq_name <|> mopt ns_prefix <<>> key "*" :: Comber String
ns_prefix = "ns-prefix"#: mopt (ident <|> key "*") <<>> key "|" :: Comber String
wq_name = "wq-name"#: mopt ns_prefix <<>> ident :: Comber String
subclass_selector :: Comber Select
subclass_selector = "subclass-selector"#: oneOf
  [ id_selector
  , class_selector
  , Attribute <$> attribute_selector
  , pseudo_class_selector
  ]
id_selector :: Comber Select
id_selector = "id-selector"#: hash <#> \value ->
  Attribute $ AttrMatch
    { attr: "id"
    , match: Just $ MatchValue
      { matchType: Exact
      , value
      , insensitive: false
      }
    }
class_selector :: Comber Select
class_selector = "class-selector"#: Class <$> do key "." *> ident
attribute_selector :: Comber AttrMatch
attribute_selector = "attribute-selector"#: delim "[" "]" ado
  attr <- wq_name
  match <- opt ado
    matchType <- attr_matcher
    _ <- key "="
    value <- string <|> ident
    insensitive <- pure false <|> false <$ key "s" <|> true <$ key "i"
    in MatchValue { matchType, value, insensitive }
  in AttrMatch { attr, match }
attr_matcher :: Comber MatchValueType
attr_matcher = "attr-matcher"#: oneOf
  [ pure Exact
  , ListContains <$ key "~"
  , Contains <$ key "*"
  , StartsWith <$ key "^"
  , EndsWith <$ key "$"
  , LangCode <$ key "|"
  ]
pseudo_class_selector :: Comber Select
pseudo_class_selector = "pseudo-class-selector"#: PseudoCls <$> do key ":" *> ident
pseudo_element_selector :: Comber Select
pseudo_element_selector = "pseudo-element-selector"#: PseudoEl <$> do key "::" *> ident
