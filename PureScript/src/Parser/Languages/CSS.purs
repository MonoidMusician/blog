module Parser.Languages.CSS where

import Prelude

import Ansi.Codes as Ansi
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Plus (empty)
import Data.Argonaut as J
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (bifoldMap, bisequence)
import Data.BooleanAlgebra.CSS (AttrMatch(..), MatchValue(..), MatchValueType(..), Relation(..), Select(..))
import Data.Enum (toEnum)
import Data.Foldable (fold, foldMap, for_, oneOf, traverse_)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as M
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Parser.Comb (compile, parseRegex, parseRegex')
import Parser.Examples (showPart)
import Parser.Languages (type (/\/), Comber, colorful, delim, key, mainName, many, many1, many1SepBy, mopt, opt, printPretty, rawr, result, showZipper, ws, wss, wsws, (#->), (#:), (/\\/), (/|\), (<#?>), (<<>>), (>==))
import Parser.Types (OrEOF(..), Part(..), ShiftReduce(..), States(..), Zipper(..), decisionUnique)
import Unsafe.Coerce (unsafeCoerce)

test :: Comber String -> Array String -> Effect Unit
test parser testData = do
  log ""
  log "Grammar:"
  printPretty (mainName #: parser)
  let (_ /\ States states) /\ doParse = parseRegex' mainName parser
  log ""
  log "Conflicts:"
  let
    normal frag = for frag case _ of
      Terminal (Continue a) -> Just (Terminal a)
      NonTerminal (Just a) -> Just (NonTerminal a)
      _ -> Nothing
    showItems pre items =
      for_ (unwrap items) \{ pName, rName, rule: Zipper l' r' } -> do
        for_ (normal l' /|\ normal r') \(l /\ r) ->
          log $ pre <> showPart (NonTerminal (fold pName)) <> "." <> foldMap show rName <> " = " <> showZipper (Zipper l r)
  conflicts <- states # foldMap \{ items, advance } ->
    advance # foldMap \sr -> M.guard (not decisionUnique sr) do
      log $ "- " <> show sr
      case sr of
        ShiftReduces s rs | Just st <- states !! s -> do
          log "  Shift to"
          showItems "  -> " st.items
          log "  Or reduce"
          for_ rs $ bisequence >>> traverse_ \(name /\ num) -> do
            log $ "  <- " <> showPart (NonTerminal name) <> "." <> show num
        _ -> pure unit
      log "  With items"
      showItems "  - " items
      pure (Additive 1)
  log $ show $ unwrap conflicts
  log ""
  log "Examples:"
  for_ testData \s -> do
    pure unit
    log (colorful Ansi.BrightYellow (show s) <> "\n  " <> result (doParse s))

main :: Effect Unit
main = do
  test (unsafeCoerce J.stringify <$> complex_selector_list)
    [ "::before"
    , ".haskell"
    , "h1#title"
    , "h1 span"
    , "blockquote > p"
    , "blockquote>p"
    , "body"
    , "html,body"
    , "html, body"
    , "html , body"
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
many1Comma = many1SepBy <@> (key "," <* ws)

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
complex_selector :: Comber (NonEmptyArray _) -- FIXME
complex_selector = (_ <* ws) $ NEA.fromFoldable1 <$> "complex-selector" #-> \more ->
  -- lift2 NEL.cons' compound_selector (Nil <$ ws <|> NEL.toList <$> (combinator *> more))
  lift2 (flip NEL.cons')
    do
      pure Nil <|> NEL.toList <$> (more <* combinator)
    do compound_selector
relative_selector :: Comber (Relation /\ _)
relative_selector = "relative-selector"#: combinator /|\ complex_selector
compound_selector :: Comber (Array Select)
compound_selector = "compound-selector"#:
  type_selector
    /\\/ many1 "subclass_selectors" subclass_selector
    /\\/ many1 "pseudo_selectors"
      ( "pseudo_selector"#: pseudo_element_selector
        /|\ many "pseudo_class_selectors" pseudo_class_selector
      )
  <#> bifoldMap Array.fromFoldable (bifoldMap NEA.toArray (foldMap (bifoldMap pure identity)))
simple_selector :: Comber (Maybe Select)
simple_selector = type_selector <|> Just <$> subclass_selector
combinator :: Comber Relation
combinator = "combinator"#: oneOf
  [ Descendant <$ wss
  , wsws $ oneOf
    [ Child <$ key ">"
    , Next <$ key "+"
    , Later <$ key "~"
    ]
  ]
-- Inline wq_name to avoid a shift-reduce conflict?
type_selector :: Comber (Maybe Select)
type_selector = "type-selector"#: oneOf
  [ opt ns_prefix <<>> (Just <$> ident <|> Nothing <$ key "*")
  ] <#> map Element
ns_prefix = "ns-prefix"#: mopt (ident <|> key "*") <<>> key "|" :: Comber String
-- Inline wq_name to avoid a shift-reduce conflict?
wq_name = mopt ns_prefix <<>> ident :: Comber String
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
