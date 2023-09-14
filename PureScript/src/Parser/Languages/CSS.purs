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
import Data.Bitraversable (bifoldMap, bitraverse)
import Data.BooleanAlgebra.CSS (Atom(..), AttrMatch(..), MatchValue(..), MatchValueType(..), Relation(..), Select(..), Several(..), Single(..), Vert, distribute, fromNF, idMatch, printVerts, selectToMatch)
import Data.BooleanAlgebra.NormalForm (NormalForm, free)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..), hush)
import Data.Either.Nested (type (\/))
import Data.Enum (toEnum)
import Data.Foldable (all, and, any, fold, foldMap, for_, oneOf, traverse_)
import Data.FoldableWithIndex (allWithIndex)
import Data.HeytingAlgebra (tt)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.InterTsil (InterTsil(..))
import Data.Lens (review)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid as M
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.String as String
import Data.Traversable (for, sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Console (log)
import Parser.Codecs (_n, shiftReduceCodec)
import Parser.Comb (execute, parseRegex, parseRegex', sourceOf)
import Parser.Comb.Run (Parsing, resultantsOf, withReparser)
import Parser.Examples (showPart)
import Parser.Languages (Comber, colorful, delim, key, mainName, many, many1, many1SepBy, mopt, opt, printPretty, rawr, result, showZipper, ws, wss, wsws, wsws', (#->), (#:), (/\\/), (/|\), (<#?>), (>==))
import Parser.Lexing (type (~), Rawr, bestRegexOrString, unRawr)
import Parser.Lexing as Lex
import Parser.Types (Fragment, OrEOF(..), Part(..), ShiftReduce(..), State, States(..), Zipper(..), decisionUnique, notEOF)
import Type.Proxy (Proxy(..))

mkCSSParser :: Maybe String -> String -> String \/ Array Vert
mkCSSParser (Just json)
  | Right (Right states) <- CA.decode codec <$> Json.parseJson json =
    execute { best: bestRegexOrString } { states, resultants: mainName /\ resultantsOf selector_list }
mkCSSParser _ = parseRegex mainName selector_list

token :: JsonCodec (OrEOF (String ~ Rawr))
token = dimap notEOF (maybe EOF Continue) $ CAC.maybe $
  _Newtype $ CAC.either CA.string $ dimap unRawr Lex.rawr CA.string

fragmentCodec :: JsonCodec (Array (Part (Either String String) (OrEOF (String ~ Rawr))))
fragmentCodec = CA.array $ CAV.variantMatch
  { "Terminal": Right token
  , "NonTerminal": Right (CAC.either CA.string CA.string)
  } # dimap
    do
      case _ of
        Terminal s -> Variant.inj (Proxy :: Proxy "Terminal") s
        NonTerminal r -> Variant.inj (Proxy :: Proxy "NonTerminal") r
    do
      Variant.match
        { "Terminal": Terminal
        , "NonTerminal": NonTerminal
        }

indexed :: forall k a. Ord k => JsonCodec k -> JsonCodec a -> JsonCodec (Map.Map k a)
indexed k a = dimap Map.toUnfoldable Map.fromFoldable $ CA.array $ CAC.tuple k a

codec :: JsonCodec (Map String Int /\ Int /\ States Int (Either String String) (Maybe Int) (OrEOF (String ~ Rawr)))
codec = CAC.tuple (indexed CA.string CA.int) $ CAC.tuple CAC.int $
  _Newtype $ CA.array $
    CAR.object "State"
      { sName: CA.int
      , items: _n $ CA.array $ CAR.object "StateItem"
        { rName: CAC.maybe CA.int
        , pName: CAC.either CA.string CA.string
        , rule: CA.indexedArray "Zipper" $ Zipper
            <$> (\(Zipper before _) -> before) CA.~ CA.index 0 fragmentCodec
            <*> (\(Zipper _ after) -> after) CA.~ CA.index 1 fragmentCodec
        , lookahead: CA.array token
        }
      , advance: _n $ indexed token $ shiftReduceCodec CA.int $ CAC.tuple (CAC.either CA.string CA.string) (CAC.maybe CA.int)
      , receive: indexed (CAC.either CA.string CA.string) CA.int
      }

test :: Comber String -> Array String -> Effect Unit
test parser testData = do
  log ""
  log "Grammar:"
  printPretty (mainName #: parser)
  -- json <- FS.Sync.readTextFile UTF8 "states.compact.json"
  -- dat@(_ /\ States states) <- maybe' (const (throw "Could not decode file")) pure $
  --   hush (Json.parseJson json) >>= CA.decode codec >>> hush
  -- let doParse = execute { best: bestRegexOrString } { states: dat, resultants: mainName /\ resultantsOf parser }
  let dat@(_ /\ _ /\ States states) /\ doParse = parseRegex' mainName parser
  log ""
  -- launchAff_ $ FS.Aff.writeTextFile UTF8 "states.compact.json" $ Json.stringify $ CA.encode codec dat
  -- log $ show states
  log $ show (Array.length states) <> " states"
  log ""
  log "Conflicts:"
  let
    normal :: Fragment (Either _ _) _ -> Maybe (Fragment _ _)
    normal frag = for frag case _ of
      Terminal (Continue a) -> Just (Terminal a)
      NonTerminal (Right a) -> Just (NonTerminal a)
      _ -> Nothing
    showItems :: String -> State (Either _ _) (Maybe _) (OrEOF _) -> _
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
          for_ rs $ bitraverse hush identity >>> traverse_ \(name /\ num) -> do
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
  test (printVerts <$> selector_list)
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
    -- , ":has(> [data-lang=\"en\"])"
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
    , mopt (key "-") <>
      (rawr "[a-zA-Z_]" <|> const empty escape)
    ]
  <> ident_continue
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
  map fold $ wsws' $ fold <$> many1 "url-contents" do
    -- TODO: non-printable
    rawr "[^\\\\\"'()\\s]+" <|> escape

any_value :: Comber String
any_value = sourceOf $ "any-value" #-> \rec ->
  pure unit <|>
    oneOf
      [ void do rawr "[-a-zA-Z0-9_]+"
      , void string
      , void url
      , void comment
      , delim "(" ")" rec
      , delim "{" "}" rec
      , delim "[" "]" rec
      , void do rawr "."
      ]
    *> rec

number :: Comber String
number = "number"#: rawr "[-+]?(?:\\d+(?:\\.\\d+)|\\.\\d+)(?:[eE][-+]?\\d+)?"

dimension :: Comber (String /\ String)
dimension = "dimension"#: number /|\ ident

percentage :: Comber String
percentage = "percentage"#: number <> key "%"

-- Parser definitions


-- Uhh restart

many1Comma :: forall a. String -> Comber a -> Comber (NonEmptyArray a)
many1Comma = many1SepBy <@> (key "," <* ws)

type ParseCompoundSelectors =
  -- (Parsing String ((NonEmptyArray (InterTsil Relation (Array Select)))))
  Parsing String (NonEmptyArray (Array Select))

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
  single f (Single Nothing) = tt
  several :: forall a. Ord a => (a -> Select) -> Several a -> NormalForm Select
  several f (Several items) =
    items # allWithIndex \value inverted -> single f (Single (Just { inverted, value }))

parseNegated :: ParseCompoundSelectors -> String -> Maybe (NormalForm Select)
parseNegated parser = parser >>> hush >== any (convSelects parser)

convRel :: ParseCompoundSelectors -> InterTsil Relation (Array Select) -> Array Vert
convRel parser = map distribute <<< sequence <<< map (fromNF <<< convSelects parser)

not_aux :: Comber (NonEmptyArray (Array Select))
not_aux = ado
  key ":"
  key "not"
  key "("
  r <- compound_selector_list
  key ")"
  in r

selector_list :: Comber (Array Vert)
selector_list = "selector-list"#:
  withReparser "not_aux" not_aux complex_selector_list
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
  (opt (more /|\ combinator) /|\ compound_selector)
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
    [ Child <$ key ">"
    , Next <$ key "+"
    , Later <$ key "~"
    ]
  ]
-- Inline wq_name to avoid a shift-reduce conflict?
type_selector :: Comber (Maybe Select)
type_selector = "type-selector"#: oneOf
  [ opt ns_prefix <> (Just <$> ident <|> Nothing <$ key "*")
  ] <#> map Element
ns_prefix = "ns-prefix"#: mopt (ident <|> key "*") <> key "|" :: Comber String
-- Inline wq_name to avoid a shift-reduce conflict?
wq_name = mopt ns_prefix <> ident :: Comber String
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
pseudo_class_selector = "pseudo-class-selector"#:
  PseudoCls <$> do
    key ":" *> ident
      <> mopt do key "(" <> any_value <> key ")"
pseudo_element_selector :: Comber Select
pseudo_element_selector = "pseudo-element-selector"#: PseudoEl <$> do key "::" *> ident
