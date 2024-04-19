module Parser.Languages.CSS where

import Prelude

import Ansi.Codes as Ansi
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Argonaut as Json
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (bifoldMap)
import Data.BooleanAlgebra.CSS (Atom(..), AttrMatch(..), MatchValue(..), MatchValueType(..), Relation(..), Select(..), Several(..), Single(..), Vert, distribute, fromNF, idMatch, printVerts, selectToMatch)
import Data.BooleanAlgebra.NormalForm (NormalForm, free)
import Data.Either (Either(..), either, hush, isRight)
import Data.Either.Nested (type (\/))
import Data.Enum (toEnum)
import Data.Foldable (all, and, any, fold, foldMap, oneOf, sum)
import Data.FoldableWithIndex (allWithIndex, forWithIndex_)
import Data.HeytingAlgebra (tt)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.InterTsil (InterTsil(..))
import Data.Lens (review)
import Data.Maybe (Maybe(..), optional)
import Data.String as String
import Data.Traversable (for, sequence)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Idiolect ((/\\/), (/|\), (<#?>), (>==))
import Parser.Comb.Comber (Comber, Parsing, delim, many, many1, many1SepBy, parse', printConflicts, printGrammarWsn, rawr, sourceOf, thaw, toAnsi, token, withReparserFor, ws, wss, wsws, wsws', (#->), (#:))
import Parser.Debug (thingy)
import Parser.Languages (colorful, result)
import Parser.Types (States(..))

mkCSSParser :: Maybe String -> String -> String \/ Array Vert
mkCSSParser json = thaw selector_list (Json.parseJson (fold json))

test :: Array (Comber String) -> Array (Either String String) -> Effect Unit
test parsers testData = do
  void $ pure thingy
  doParses <- forWithIndex parsers \i parser -> do
    log ""
    log $ "Grammar " <> show (i+1) <> ":"
    log $ printGrammarWsn toAnsi parser
    let { states: States states } /\ doParse = parse' parser
    log ""
    log $ show (Array.length states) <> " states"
    log ""
    log $ printConflicts toAnsi (States states)
    pure doParse
  log ""
  log "Examples:"
  results <- for testData \ex -> do
    pure unit
    let s = either identity identity ex
    log $ colorful Ansi.BrightYellow $ show s
    for doParses \doParse -> do
      let parsed = doParse s
      let r = either identity identity parsed
      let res = if isRight parsed == isRight ex then Right r else Left r
      log $ "  " <> result res
      pure if isRight res then 1 else 0
  forWithIndex_ doParses \i _ -> do
    let parserResults = results # Array.mapMaybe (_ !! i)
    log $ show (sum parserResults) <> " / " <> show (Array.length parserResults)

main :: Effect Unit
main = do
  test [printVerts <$> selector_list]
    [ Right "::before"
    , Right ".haskell"
    , Right "h1#title"
    , Right "h1 span"
    , Right "blockquote > p"
    , Right "blockquote>p"
    , Right "body"
    , Right "html,body"
    , Right "html, body"
    , Right "html , body"
    -- , Right ":has(> [data-lang=\"en\"])"
    ]

-- Lexer definitions

comment :: Comber String
comment = "comment"#: delim "/*" "*/" do rawr "."
newline :: Comber String
newline = "newline"#: rawr "\\r\\n|[\\n\\r\\f]"
escape :: Comber String
escape = "escape"#: token "\\" *> oneOf
  [ rawr "[^0-9a-fA-F\\r\\n\\f]"
  , rawr "[0-9a-fA-F]{1,6}\\s?" <#?> do
      String.trim >>> Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
  ]
ident :: Comber String
ident = "ident"#:
  oneOf
    [ token "--"
    , (map fold <<< optional) (token "-") <>
      (rawr "[a-zA-Z_]" <|> const empty escape)
    ]
  <> ident_continue
ident_continue :: Comber String
ident_continue = fold <$> many "ident-continue" do
  rawr "[-a-zA-Z0-9_]+" <|> const empty escape
function :: Comber String
function = ident <* token "("
at_keyword :: Comber String
at_keyword = token "@" *> ident
hash :: Comber String
hash = token "#" *> ident_continue
string :: Comber String
string = "string"#: oneOf
  [ join delim "\"" $ fold <$> many "double-string-contents" do
      rawr "[^\\\\\"\\n\\r\\f]+" <|> escape <|> token "\\" *> newline
  , join delim "\'" $ fold <$> many "single-string-contents" do
      rawr "[^\\\\'\\n\\r\\f]+" <|> escape <|> token "\\" *> newline
  ]
url :: Comber String
url = "url"#: token "url" *> delim "(" ")" do
  map fold $ wsws' $ fold <$> many1 "url-contents" do
    -- TODO: non-printable
    rawr "[^\\\\\"'()\\s]+" <|> escape

any_value :: Comber String
any_value = sourceOf $ "any-value" #-> \rec ->
  pure unit <|>
    oneOf
      [ void do rawr "."
      -- Needs to be 2+ chars to not overlap with "."
      -- Since there is no mechanism to disambiguate that
      , void do rawr "[-a-zA-Z0-9_][-a-zA-Z0-9_]+"
      , void string
      , void url
      , void comment
      , delim "(" ")" rec
      , delim "{" "}" rec
      , delim "[" "]" rec
      ]
    *> rec

number :: Comber String
number = "number"#: rawr "[-+]?(?:\\d+(?:\\.\\d+)|\\.\\d+)(?:[eE][-+]?\\d+)?"

dimension :: Comber (String /\ String)
dimension = "dimension"#: number /|\ ident

percentage :: Comber String
percentage = "percentage"#: number <> token "%"

-- Parser definitions


-- Uhh restart

many1Comma :: forall a. String -> Comber a -> Comber (NonEmptyArray a)
many1Comma = many1SepBy <@> (token "," <* ws)

type ParseCompoundSelectors =
  Parsing (NonEmptyArray (Array Select))

convSelects :: ParseCompoundSelectors -> Array Select -> NormalForm Select
convSelects parser selects = all (selectToMatch'' parser) selects

selectToMatch'' :: ParseCompoundSelectors -> Select -> NormalForm Select
selectToMatch'' parser (PseudoCls cls) | Just negated <- parseNegated parser cls = do
  not negated
selectToMatch'' _ sel = atomToNFSelect (selectToMatch sel)

atomToNFSelect :: Atom -> NormalForm Select
atomToNFSelect (Atom a) = and
  [ single Element a.element
  , several Class a.classes
  , several PseudoCls a.pseudoCls
  , several Attribute a.attrs
  , single PseudoEl a.pseudoEl
  ]
  where
  invertIf true = not
  invertIf false = identity
  single :: forall a. (a -> Select) -> Single a -> NormalForm Select
  single f (Single (Just { inverted, value })) =
    invertIf inverted (free (f value))
  single _ (Single Nothing) = tt
  several :: forall a. Ord a => (a -> Select) -> Several a -> NormalForm Select
  several f (Several items) =
    items # allWithIndex \value inverted -> single f (Single (Just { inverted, value }))

parseNegated :: ParseCompoundSelectors -> String -> Maybe (NormalForm Select)
parseNegated parser value = (":" <> value) # parser >>> hush >== any (convSelects parser)

convRel :: ParseCompoundSelectors -> InterTsil Relation (Array Select) -> Array Vert
convRel parser = map distribute <<< sequence <<< map (fromNF <<< convSelects parser)

not_aux :: Comber (NonEmptyArray (Array Select))
not_aux = ado
  token ":"
  token "not"
  token "("
  r <- compound_selector_list
  token ")"
  in r

selector_list :: Comber (Array Vert)
selector_list = "selector-list"#:
  withReparserFor "not_aux" not_aux complex_selector_list
    \parser -> foldMap (convRel parser)
complex_selector_list :: Comber (NonEmptyArray (InterTsil Relation (Array Select)))
complex_selector_list = many1Comma "complex-selector-list" complex_selector
compound_selector_list :: Comber (NonEmptyArray (Array Select))
compound_selector_list = many1Comma "compound-selector-list" compound_selector
simple_selector_list :: Comber (NonEmptyArray (Maybe Select))
simple_selector_list = many1Comma "simple-selector-list" simple_selector
relative_selector_list :: Comber (NonEmptyArray (Relation /\ InterTsil Relation (Array Select)))
relative_selector_list = many1Comma "relative-selector-list" relative_selector
complex_selector :: Comber (InterTsil Relation (Array Select)) -- FIXME
complex_selector = (_ <* ws) $ "complex-selector" #-> \more ->
  (optional (more /|\ combinator) /|\ compound_selector)
  <#> case _ of
    Nothing /\ s -> One s
    Just (m /\ c) /\ s -> More m c s
relative_selector :: Comber (Relation /\ InterTsil Relation (Array Select))
relative_selector = "relative-selector"#: combinator /|\ complex_selector
compound_selector :: Comber (Array Select)
compound_selector = "compound-selector"#:
  type_selector
    -- TODO: permutations???
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
    [ Child <$ token ">"
    , Next <$ token "+"
    , Later <$ token "~"
    ]
  ]
-- Inline wq_name to avoid a shift-reduce conflict?
type_selector :: Comber (Maybe Select)
type_selector = "type-selector"#: oneOf
  [ optional ns_prefix <> (Just <$> ident <|> Nothing <$ token "*")
  ] <#> map Element
ns_prefix = "ns-prefix"#: (map fold <<< optional) (ident <|> token "*") <> token "|" :: Comber String
-- Inline wq_name to avoid a shift-reduce conflict?
wq_name = (map fold <<< optional) ns_prefix <> ident :: Comber String
subclass_selector :: Comber Select
subclass_selector = "subclass-selector"#: oneOf
  [ id_selector
  , class_selector
  , Attribute <$> attribute_selector
  , pseudo_class_selector
  ]
id_selector :: Comber Select
id_selector = "id-selector"#: hash <#> review idMatch >>> Attribute
class_selector :: Comber Select
class_selector = "class-selector"#: Class <$> do token "." *> ident
attribute_selector :: Comber AttrMatch
attribute_selector = "attribute-selector"#: delim "[" "]" ado
  attr <- wq_name
  match <- optional ado
    matchType <- attr_matcher
    _ <- token "="
    value <- string <|> ident
    insensitive <- pure false <|> false <$ token "s" <|> true <$ token "i"
    in MatchValue { matchType, value, insensitive }
  in AttrMatch { attr, match }
attr_matcher :: Comber MatchValueType
attr_matcher = "attr-matcher"#: oneOf
  [ pure Exact
  , ListContains <$ token "~"
  , Contains <$ token "*"
  , StartsWith <$ token "^"
  , EndsWith <$ token "$"
  , LangCode <$ token "|"
  ]
pseudo_class_selector :: Comber Select
pseudo_class_selector = "pseudo-class-selector"#:
  PseudoCls <$> do
    token ":" *> ident
      <> (map fold <<< optional) do token "(" <> any_value <> token ")"
pseudo_element_selector :: Comber Select
pseudo_element_selector = "pseudo-element-selector"#: PseudoEl <$> do token "::" *> ident
