-- | Easy to use version of the parser combinator, specialized to strings and
-- | regexes.
module Parser.Comb.Comber ( module Parser.Comb.Comber, module ReExports ) where

import Prelude

import Ansi.Codes as Ansi
import Ansi.Output as Ansi.Output
import Control.Alt (class Alt, (<|>))
import Control.Alt (class Alt, (<|>)) as ReExports
import Control.Alternative (class Alternative, class Plus) as ReExports
import Control.Alternative (class Alternative, class Plus, empty, guard)
import Control.Apply (lift2)
import Control.Apply (lift2) as ReExports
import Control.Comonad (extract)
import Control.Comonad.Store (Store, StoreT(..), store)
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Array (intercalate, (!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Compactable (class Compactable)
import Data.Compactable (class Compactable, compact) as ReExports
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap, oneOf, traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (optional) as ReExports
import Data.Newtype (class Newtype, over, un, unwrap)
import Data.Rational (Rational)
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Fetch (fetch)
import Idiolect (intercalateMap)
import Parser.Comb (Comb(..), execute)
import Parser.Comb as Comb
import Parser.Comb.Codec (stateTableCodec)
import Parser.Comb.Combinators (buildTree, namedRec')
import Parser.Comb.Run (gatherPrecedences, resultantsOf)
import Parser.Comb.Run as CombR
import Parser.Comb.Syntax (printSyntax')
import Parser.Comb.Types (Associativity(..))
import Parser.Comb.Types as CombT
import Parser.Lexing (class ToString, type (~), FailReason(..), FailedStack(..), Rawr, Similar(..), toString, bestRegexOrString, errorName, len, userErrors)
import Parser.Lexing (class ToString, type (~), Rawr) as ReExports
import Parser.Selective (class Casing, class Select)
import Parser.Types (OrEOF) as ReExports
import Parser.Types (ShiftReduce(..), unShift)
import Parser.Types as P
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, to, from)
import Type.Proxy (Proxy(..))

type ParseError = String
type UserError = String

newtype Comber a = Comber
  (Comb Rec UserError Rational String (String ~ Rawr) String a)
derive instance Newtype (Comber a) _
derive newtype instance functorComber :: Functor Comber
derive newtype instance applyComber :: Apply Comber
derive newtype instance applicativeComber :: Applicative Comber
derive newtype instance altComber :: Alt Comber
derive newtype instance plusComber :: Plus Comber
derive newtype instance alternativeComber :: Alternative Comber
derive newtype instance compactableComber :: Compactable Comber
derive newtype instance selectComber :: Select Comber
derive newtype instance casingComber :: Casing Comber
derive newtype instance semigroupComber :: Semigroup a => Semigroup (Comber a)
derive newtype instance monoidComber :: Monoid a => Monoid (Comber a)
derive newtype instance heytingAlgebraComber :: HeytingAlgebra a => HeytingAlgebra (Comber a)

type Parsing a = String -> Either String a

type Part = P.Part String (String ~ Rawr)
type Fragment = P.Fragment String (String ~ Rawr)
type Zipper = P.Zipper String (String ~ Rawr)

-- | Parse a regex.
rawr :: String -> Comber String
rawr = Comber <<< Comb.tokenRawr

-- | Parse a literal token (`String` or `Char` or `CodePoint`). This takes
-- | priority over regexes that match the same substring.
token :: forall s. ToString s => s -> Comber String
token = Comber <<< Comb.tokenStr

tokenPrecA :: forall s. ToString s => s -> Associativity -> Rational -> Comber String
tokenPrecA cat assoc prec = Comber (Comb.tokenPrecA (Similar (Left (toString cat))) assoc prec)

tokenPrecL :: forall s. ToString s => s -> Rational -> Comber String
tokenPrecL = tokenPrecA <@> AssocL
tokenPrecR :: forall s. ToString s => s -> Rational -> Comber String
tokenPrecR = tokenPrecA <@> AssocR
tokenPrec :: forall s. ToString s => s -> Rational -> Comber String
tokenPrec = tokenPrecA <@> NoAssoc

-- | Name a nonterminal production, this allows recursion.
namedRec :: forall a. String -> (Comber a -> Comber a) -> Comber a
namedRec name = Comber <<< Comb.namedRec name <<< coerce

-- | Name a parser. This may introduce ambiguity into the grammar.
named :: forall a. String -> Comber a -> Comber a
named name = Comber <<< Comb.named name <<< coerce

infixr 2 named as #:
infixr 5 namedRec as #->

rulePrec :: forall a. Rational -> Comber a -> Comber a
rulePrec = over Comber <<< Comb.rulePrec

-- | Return the source parsed by the given parser, instead of whatever its
-- | applicative result was.
sourceOf :: forall a. Comber a -> Comber String
sourceOf = over Comber Comb.sourceOf

withSourceOf :: forall a. Comber a -> Comber (String /\ a)
withSourceOf = over Comber Comb.withSourceOf

-- | Return the source tokens parsed by the given parser, instead of whatever
-- | its applicative result was.
tokensSourceOf :: forall a. Comber a -> Comber (Array String)
tokensSourceOf = over Comber Comb.tokensSourceOf

withTokensSourceOf :: forall a. Comber a -> Comber (Array String /\ a)
withTokensSourceOf = over Comber Comb.withTokensSourceOf


piggyback ::
  forall a b.
  { pattern :: Comber a
  , errors :: Array UserError
  , name :: String
  } ->
  Comber b -> Comber b
piggyback { errors, name, pattern } dataParser =
  mapEither_ (lmap (\e -> errors <> [e])) $
    withReparserFor name dataParser (sourceOf pattern) ($)

topName :: String
topName = "TOP"

-- | Run the parser on the given string input. Please partially apply this since
-- | it has to build up an LR(1) table first, and this can be shared between
-- | inputs if it is partially applied.
parse :: forall a. Comber a -> (String -> Either ParseError a)
parse = convertingParseError <<< Comb.parseRegex topName <<< un Comber

parse' :: forall a. Comber a -> StateTable /\ (String -> Either ParseError a)
parse' = map convertingParseError <<< Comb.parseRegex' topName <<< un Comber

type Conf = CombR.CConf UserError Int String (String ~ Rawr) String String

parseWith :: forall a. Conf -> Comber a -> (String -> Either ParseError a)
parseWith conf = convertingParseError <<< Comb.parseWith conf topName <<< un Comber

parseWith' :: forall a. Conf -> Comber a -> StateTable /\ (String -> Either ParseError a)
parseWith' conf = map convertingParseError <<< Comb.parseWith' conf topName <<< un Comber

convertingParseError :: forall a.
  (String -> Either FullParseError a) ->
  String -> Either ParseError a
convertingParseError = map $ lmap case _ of
  CrashedStack s -> "Internal parser error: " <> s
  FailedStack info@{ lookupState, initialInput, currentInput, failedStack } ->
    fold
      [ case info.failReason of
          StackInvariantFailed -> "Internal parser error: Stack invariant failed"
          UnknownState { state } -> "Internal parser error: Unknown state " <> show state
          Uhhhh { token: cat } -> "Internal parser error: Uhhhh " <> show cat
          UnknownReduction { reduction: nt /\ r } -> "Internal parser error: UnknownReduction " <> show nt <> "#" <> show r

          UserRejection { userErr } -> fold
            [ "Parse error: User rejection"
            , userErr # foldMap \err ->
                "\n  - " <> indenting "    " err
            ]
          NoTokenMatches { advance } -> fold
            [ "Parse error: No token matches"
            , ", expected"
            , case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , advance # foldMapWithIndex \cat _ -> do
                "\n  - " <> showCat cat
            ]
          UnexpectedToken { advance, matched }
            | [ cat0 /\ o /\ _i ] <- NEA.toArray matched -> fold
            [ "Parse error: Unexpected token "
            , showCatTok cat0 o
            , ", expected"
            , case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , advance # foldMapWithIndex \cat _ -> do
                "\n  - " <> showCat cat
            ]
          UnexpectedToken { advance, matched: matched } -> fold
            [ "Parse error: Unexpected+ambiguous token "
            , matched # foldMap \(cat0 /\ o /\ _i) -> fold
              [ "\n  - "
              , showCatTok cat0 o
              ]
            , "\nExpected"
            , case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , advance # foldMapWithIndex \cat _ -> do
                "\n  - " <> showCat cat
            ]
          AllActionsRejected { possibilities } -> fold
            [ "Parse error: All actions rejected"
            , possibilities # foldMap \(poss /\ errs) -> fold
                [ "\n  - " <> show poss
                , errs # foldMap \err -> fold
                    [ "\n    - " <> errorName err
                    , userErrors err # foldMap \x ->
                        "\n      - " <> indenting "        " x
                    ]
                ]
            ]
          Ambiguity { filtered, userErr } -> fold
            [ "Parse error: LR(1) ambiguity"
            , filtered # foldMap \(Tuple sr _) -> fold
                [ "\n  - "
                , case sr of
                    Shift s -> "Shift to " <> show s
                    ShiftReduces s rs -> "Shift to " <> show s <> ", or reduce " <>
                      intercalateMap " or " (uncurry showNTR <<< snd) rs
                    Reduces rs -> "Reduce " <>
                      intercalateMap " or " (uncurry showNTR <<< snd) rs
                , (unShift sr >>= lookupState) # foldMap \state ->
                    unwrap state.items # foldMap \item -> fold
                      [ "\n    - "
                      , showNTR item.pName item.rName
                      , " = "
                      , case item.rule of
                          P.Zipper parsed toParse -> fold
                            [ intercalateMap " " part parsed
                            , " • "
                            , intercalateMap " " part toParse
                            ]
                      ]
                ]
            , userErr # foldMap \err ->
                "\n    - " <> indenting "      " err
            ]
      , "\nat character " <> show (1 + len initialInput - len currentInput)
      , case initialInput of
          P.Continue s -> "\n  " <> show (String.drop (len initialInput - len currentInput) s)
          P.EOF -> ""
      -- , "\n"
      -- , show info.failedState
      -- , "\n"
      -- , "Stack: " <> show (stackSize failedStack)
      ]
  where
  indenting x = String.replaceAll (String.Pattern "\n") (String.Replacement ("\n" <> x))

  showNTR (Left x) Nothing = x <> "#"
  showNTR (Right x) (Just r) = x <> "#" <> show r
  showNTR _ _ = "???"

  showCat = part <<< P.Terminal
  showCatTok = case _, _ of
    P.EOF, P.EOF -> "$ (EOF)"
    P.Continue (Similar (Left s)), P.Continue s' | s == s' -> show s
    P.Continue (Similar (Right r)), P.Continue s -> show s <> " (" <> show r <> ")"
    _, _ -> "??"

  part = case _ of
    P.NonTerminal (Right v) -> v
    P.NonTerminal (Left v) -> v <> "#"
    P.Terminal P.EOF -> "$"
    P.Terminal (P.Continue (Similar (Left s))) -> show s
    P.Terminal (P.Continue (Similar (Right r))) -> show r

--------------------------------------------------------------------------------

freeze :: forall a. Comber a -> Json
freeze = parse' >>> fst >>> C.encode stateTableCodec

freezeTable :: StateTable -> Json
freezeTable = C.encode stateTableCodec

thaw :: forall e a. Comber a -> Either e Json -> (String -> Either ParseError a)
thaw comber json = case thaw' comber <$> json of
  Right (Right (_ /\ run)) -> run
  _ -> parse comber

fetchAndThaw :: forall a. Comber a -> String -> Aff (String -> Either ParseError a)
fetchAndThaw comber url =
  fetch url {} >>= _.text >>> map (Json.parseJson >>> thaw comber)

thaw' :: forall a. Comber a -> Json -> Either CA.JsonDecodeError (StateTable /\ (String -> Either ParseError a))
thaw' (Comber comber) = C.decode stateTableCodec >>> map \states ->
  Tuple states $ convertingParseError $ execute { best: bestRegexOrString }
    { states
    , resultants: topName /\ resultantsOf comber
    , options: buildTree topName comber
    , precedences: gatherPrecedences comber
    }

thawWith :: forall e a. Conf -> Comber a -> Either e Json -> (String -> Either ParseError a)
thawWith conf comber json = case thawWith' conf comber <$> json of
  Right (Right (_ /\ run)) -> run
  _ -> parseWith conf comber

fetchAndThawWith :: forall a. Conf -> Comber a -> String -> Aff (String -> Either ParseError a)
fetchAndThawWith conf comber url =
  fetch url {} >>= _.text >>> map (Json.parseJson >>> thawWith conf comber)

thawWith' :: forall a. Conf -> Comber a -> Json -> Either CA.JsonDecodeError (StateTable /\ (String -> Either ParseError a))
thawWith' conf (Comber comber) = C.decode stateTableCodec >>> map \states ->
  Tuple states $ convertingParseError $ execute conf
    { states
    , resultants: topName /\ resultantsOf comber
    , options: buildTree topName comber
    , precedences: gatherPrecedences comber
    }

fetchAndThaw' ::
  forall a.
  Comber a ->
  String ->
  Aff (Either CA.JsonDecodeError (StateTable /\ (String -> Either String a)))
fetchAndThaw' comber url =
  fetch url {} >>= _.text >>> map do
    (Json.parseJson >>> lmap conv) >=> thaw' comber
  where
  conv = case _ of
    Json.TypeMismatch msg -> CA.TypeMismatch msg
    Json.UnexpectedValue j -> CA.UnexpectedValue j
    Json.AtIndex i e -> CA.AtIndex i $ conv e
    Json.AtKey k e -> CA.AtKey k $ conv e
    Json.Named s e -> CA.Named s $ conv e
    Json.MissingValue -> CA.MissingValue

type FullParseError = CombR.ParseError UserError String (String ~ Rawr) String String
type Rec = CombR.Rec UserError String (String ~ Rawr) String String
type States = CombR.CStates String (String ~ Rawr)
type StateInfo = CombR.CStateInfo String (String ~ Rawr)
type State = CombR.CState String (String ~ Rawr)
type Resultant = CombT.CResultant Rec UserError String String
type Options = CombT.COptions Rec UserError String (String ~ Rawr) String
type StateTable =
  { stateMap :: Map String Int
  , start :: Int
  , states :: States
  }
type Compiled a =
  { states :: StateTable
  , options :: Options
  , resultants :: String /\ Array (Resultant a)
  }

--------------------------------------------------------------------------------

-- | Name the first parser so it can be used (recursively!) while handling the
-- | result of the second parser.
-- |
-- | This is pretty niche but it is useful for parsing CSS as specified:
-- | see https://www.w3.org/TR/css-syntax-3/#any-value.
-- |
-- | > In some grammars, it is useful to accept any reasonable input in the
-- | grammar, and do more specific error-handling on the contents manually
-- | (rather than simply invalidating the construct, as grammar mismatches
-- | tend to do).
withReparserFor ::
  forall a b c.
  String ->
  Comber a ->
  Comber b ->
  ((String -> Either UserError a) -> b -> c) ->
  Comber c
withReparserFor name (Comber aux) (Comber body) f =
  Comber $ CombR.withReparserFor name aux body $ f <<< convertingParseError


mapEither :: forall a b. (a -> Either (Array UserError) b) -> Comber a -> Comber b
mapEither f = over Comber $ Comb.mapEither f

mapEither_ :: forall a b. (a -> Either (Array UserError) b) -> Comber a -> Comber b
mapEither_ f = over Comber $ Comb.mapEither_ f

--------------------------------------------------------------------------------

-- | Surround the parse with literal tokens.
delim :: forall s a. ToString s => s -> s -> Comber a -> Comber a
delim x y z = token x *> z <* token y

many :: forall a. String -> Comber a -> Comber (Array a)
many n p = Array.fromFoldable <$> n #-> \more ->
  pure Nil <|> lift2 Cons p more

foldMany :: forall m. Monoid m => String -> Comber m -> Comber m
foldMany n p = fold <$> n #-> \more ->
  pure Nil <|> lift2 Cons p more

manyL :: forall a. String -> Comber a -> Comber (Array a)
manyL n p = Array.reverse <<< Array.fromFoldable <$> n #-> \more ->
  pure Nil <|> lift2 (flip Cons) more p


manySepBy :: forall x a. String -> Comber x -> Comber a -> Comber (Array a)
manySepBy n s p = map Array.fromFoldable $
  pure Nil <|> lift2 Cons p do
    n #-> \more -> pure Nil <|> lift2 Cons (s *> p) more

-- | Have a specific parser for the `Nil` base case (usually `ws`).
manyBaseSepBy :: forall x y a. String -> Comber x -> Comber y -> Comber a -> Comber (Array a)
manyBaseSepBy n b s p = map Array.fromFoldable $
  Nil <$ b <|> lift2 Cons p do
    n #-> \more -> pure Nil <|> lift2 Cons (s *> p) more

many1 :: forall a. String -> Comber a -> Comber (NonEmptyArray a)
many1 n p = NEA.fromFoldable1 <$> n #-> \more ->
  lift2 NEL.cons' p (pure Nil <|> NEL.toList <$> more)

foldMany1 :: forall m. Semigroup m => String -> Comber m -> Comber m
foldMany1 n p = NEL.fold1 <$> n #-> \more ->
  lift2 NEL.cons' p (pure Nil <|> NEL.toList <$> more)

many1SepBy :: forall x a. String -> Comber x -> Comber a -> Comber (NonEmptyArray a)
many1SepBy n s p = NEA.fromFoldable1 <$> n #-> \more ->
  lift2 NEL.cons' p (pure Nil <|> NEL.toList <$> (s *> more))

-- | For `Monoid`s. Use `optional` if you want to return a `Maybe`.
opt :: forall a. Monoid a => Comber a -> Comber a
opt a = pure mempty <|> a

--------------------------------------------------------------------------------

-- | Optional whitespace.
-- optional whitespace needs to be optional at the parser combinator level, not
-- the regex level (where it would both conflict with nonempty whitespace, and
-- more seriously allow empty parses), nor the LR level (where it would be
-- locked behind a reduction even though the point is that it does not matter
-- to anything consuming it)
ws :: Comber Unit
ws = wss <|> pure unit

-- | Required whitespace.
wss :: Comber Unit
wss = "wss"#: void (rawr "\\s+")

-- | Surround the parser with optional whitespace. Will cause issues if the
-- | parser can parse the empty string.
wsws :: Comber ~> Comber
wsws a = ws *> a <* ws

-- | Surround a non-empty parser with whitespace or allow it to match just
-- | whitespace and return `Nothing`.
wsws' :: forall a. Comber a -> Comber (Maybe a)
wsws' a = oneOf
  [ Nothing <$ wss
  , Just <$> wsws a
  ]


--------------------------------------------------------------------------------

-- | Parse syntax like `[v1,v2,v3]` or `[   ]`
arrayOf :: forall a. String -> Comber a -> Comber (Array a)
arrayOf name value = delim "[" "]" do
  manyBaseSepBy name ws (token ",") value

-- | Parse syntax like `{k1:v1,k2:v2}` or `{  }`
objectOf :: forall k a. String -> String -> Comber k -> Comber a -> Comber (Array (k /\ a))
objectOf name entry key value = delim "{" "}" do
  manyBaseSepBy name ws (token ",") do
    entry#: Tuple <$> wsws key <* token ":" <*> value


--------------------------------------------------------------------------------


type Printer m =
  { nonTerminal :: String -> m
  , rule :: Int -> m
  , literal :: String -> m
  , regex :: String -> m
  , meta :: String -> m
  , lines :: Array m -> m
  }

type PrintFn t = forall m. Monoid m => Printer m -> t -> m

printToString :: Printer String
printToString =
  { nonTerminal: identity
  , rule: show
  , literal: show
  , regex: identity
  , meta: identity
  , lines: intercalate "\n"
  }

toAnsi :: Printer String
toAnsi =
  { nonTerminal: colorful Ansi.Blue <<< printToString.nonTerminal
  , rule: colorful Ansi.Cyan <<< printToString.rule
  , literal: colorful Ansi.Yellow <<< printToString.literal
  , regex: colorful Ansi.Green <<< printToString.regex
  , meta: identity <<< printToString.meta
  , lines: printToString.lines
  }
  where
  colorful c = Ansi.Output.withGraphics (pure (Ansi.PForeground c))

toConsole :: forall m. MonadEffect m => Monoid (m String) => Printer (m String)
toConsole =
  { nonTerminal: pure <<< toAnsi.nonTerminal
  , rule: pure <<< toAnsi.rule
  , literal: pure <<< toAnsi.literal
  , regex: pure <<< toAnsi.regex
  , meta: pure <<< toAnsi.meta
  , lines: NEA.fromArray >>> case _ of
      Nothing -> mempty
      Just nea -> do
        traverse_ (_ >>= log) (NEA.init nea)
        NEA.last nea
  }

printRules ::
  forall m a.
    Monoid m =>
  Printer m -> Comber a -> Array (String /\ Maybe m)
printRules p = named topName >>>
  \(Comber (Comb { prettyGrammar })) ->
    Array.nub prettyGrammar # (map <<< map <<< map) \syntax ->
      printSyntax' p.meta (spaced (printPart p)) syntax
  where
  spaced :: forall b. (b -> m) -> Array b -> m
  spaced f = intercalate (p.meta " ") <<< map f


printPart :: PrintFn Part
printPart p (P.NonTerminal nt) = p.nonTerminal nt
printPart p (P.Terminal (Similar (Left tok))) = p.literal tok
printPart p (P.Terminal (Similar (Right r))) = p.regex $ show r

printFragment' :: PrintFn Fragment
printFragment' p = intercalate (p.meta " ") <<< map (printPart p)

printFragment :: PrintFn Fragment
printFragment p [] = p.literal ""
printFragment p fragment =
  printFragment' p fragment

printZipper :: PrintFn Zipper
printZipper p (P.Zipper l r) =
  intercalate (p.meta " • ") [ printFragment' p l, printFragment' p r ]

printZipper' :: PrintFn (P.Zipper (Either String String) (P.OrEOF (String ~ Rawr)))
printZipper' p (P.Zipper l r) =
  intercalate (p.meta " • ") [ printFragment'' p l, printFragment'' p r ]

printPart' :: PrintFn (P.Part (Either String String) (P.OrEOF (String ~ Rawr)))
printPart' p (P.NonTerminal (Left nt)) = p.nonTerminal nt <> p.meta "$"
printPart' p (P.NonTerminal (Right nt)) = p.nonTerminal nt
printPart' p (P.Terminal P.EOF) = p.meta "$"
printPart' p (P.Terminal (P.Continue (Similar (Left tok)))) = p.literal tok
printPart' p (P.Terminal (P.Continue (Similar (Right r)))) = p.regex $ show r

printFragment'' :: PrintFn (P.Fragment (Either String String) (P.OrEOF (String ~ Rawr)))
printFragment'' p = intercalate (p.meta " ") <<< map (printPart' p)


printGrammarWsn :: forall a. PrintFn (Comber a)
printGrammarWsn p comber =
  p.lines $
    printRules p comber <#> case _ of
      name /\ Nothing ->
        p.nonTerminal name <> p.meta " = " <> p.literal "" <> p.meta "."
      name /\ (Just rule) ->
        p.nonTerminal name <> p.meta " = " <> rule <> p.meta "."

printState :: forall m. Monoid m => Printer m -> State -> Array m
printState p (P.State items) =
  {-
    printState: Printer m -> State -> *m
    printState/ p items => result:
      let
        | ["Terminal" ["Continue" a]] => ["Terminal" a]
        | ["NonTerminal" ["R" a]] => ["NonTerminal" a]
        ! => normal
      (# will catch pattern failure and substitute `mempty` #)
      foldMatching items (: * :) (# coerces to list, specifically `*(?)` #)
      | { pName: ["R" pName], rName: [rName], rule: [l' r'] } => [itemResult]:
        map normal l' => l
        map normal r' => r
        (# TODO: use effects #)
        p.nonTerminal pName => o1
        p.meta "." => o2
        p.rule rName => o3
        p.meta " = " => o4
        printZipper p [l r] => o5
        fold [ o1 o2 o3 o4 o5 ] => itemResult
      ! => result
  -}
  items # foldMap \{ pName, rName, rule } ->
    [ fold
      [ printPart' p (P.NonTerminal pName)
      , p.meta "."
      , case rName of
          Just a -> p.rule a
          Nothing -> p.meta "$"
      , p.meta " = "
      , printZipper' p rule
      ]
    ]

printReduction :: PrintFn (Either String String /\ Maybe Int)
printReduction p (Right name /\ Just num) =
  p.nonTerminal name <> p.meta "." <> p.rule num
printReduction _ _ = mempty


printConflicts :: PrintFn States
printConflicts p states@(P.States index) =
  case P.conflicts states of
    [] -> mempty
    conflicts -> p.lines
      [ p.rule (Array.length conflicts) <> p.meta " Conflicts:"
      , p.lines $ conflicts <#> \{ sName, items, advance, conflict } ->
          p.lines
            [ fold
              [ p.meta "- "
              , p.meta (show conflict)
              , p.meta " ("
              , p.rule sName
              , p.meta " @ "
              , case advance of
                  P.Continue a -> printPart p (P.Terminal a)
                  P.EOF -> p.meta "EOF"
              , p.meta ")"
              ]
            , fold do
                s <- P.unShift conflict
                st <- index !! s
                pure $ p.lines $ Array.cons (p.meta "  Shift to") $
                  (p.meta "  -> " <> _) <$> printState p st.items
            , p.meta "  Or reduce"
            , p.lines $ P.reductions conflict <#> \(_ /\ r) ->
                p.meta "  <- " <> printReduction p r
            , p.meta "  With items"
            , p.lines $ (p.meta "  - " <> _) <$> printState p items
            ]
      ]

printStateTable :: PrintFn StateTable
printStateTable p { states: P.States states } =
  p.lines $ states <#> \{ sName, items, advance: Map.SemigroupMap advance, receive } -> p.lines $ Array.catMaybes
    [ Just $ p.meta $ "- " <> show sName <> ":"
    , Just $ p.lines $ (p.meta "  - " <> _) <$> printState p items
    , guard (not Map.isEmpty advance) $> do
        p.lines $ advance # foldMapWithIndex \cat sr -> fold
          [ pure $ p.meta "  ? " <> case cat of
              P.Continue a -> printPart p (P.Terminal a)
              P.EOF -> p.meta "EOF"
          , P.unShift sr # foldMap \s -> [ p.meta $ "    -> " <> show s ]
          , P.reductions sr # foldMap \r -> pure $ fold
              [ p.meta "    <- "
              , p.nonTerminal (either identity identity (fst (snd r)))
              , p.meta "."
              , foldMap (p.meta <<< show) (snd (snd r))
              ]
          ]
    , guard (not Map.isEmpty receive) $> do
        p.lines $ receive # foldMapWithIndex \nt s ->
          [ p.meta "  <- " <> p.nonTerminal (either identity identity nt) <> p.meta (" -> " <> show s) ]
    ]

--------------------------------------------------------------------------------

-- | Define mutually recursive parsers all at once, through a fixpoint of
-- | a record of fields of type `Comber`. Each parser may return its own type.
mutual ::
  forall r rl.
    RL.RowToList r rl =>
    MutualCombers rl r =>
  (Record r -> Record r) -> Record r
mutual defineParsers =
  extract $ fst $ mutual'
    (Tuple <$> defineParsers <@> unit)

mutual' ::
  forall r rl o.
    RL.RowToList r rl =>
    MutualCombers rl r =>
  (Record r -> Record r /\ o) ->
  Store (Comber Void) (Record r) /\ o
mutual' =  mutualRL (Proxy :: Proxy rl)

class MutualCombers :: RL.RowList Type -> Row Type -> Constraint
class MutualCombers rl r | rl -> r where
  mutualRL :: forall o. Proxy rl ->
    (Record r -> Record r /\ o) ->
    -- We store added rules in `Comber Void`, which we can `<|>` onto it safely
    -- to accumulate grammar and other info, without adding extra rules to match
    (Store (Comber Void) (Record r) /\ o)

instance mutualNil :: MutualCombers RL.Nil () where
  mutualRL _ f =
    let r /\ o = f {} in
    store (const r) empty /\ o

instance mutualCons ::
  ( MutualCombers rl' r'
  , IsSymbol sym
  , Row.Lacks sym r'
  , Row.Cons sym c r' r
  , TypeEquals (Comber a) c
  ) => MutualCombers (RL.Cons sym c rl') r where
    mutualRL _ f = rOut /\ o
      where
      sym = Proxy :: Proxy sym
      -- Save the recursive invocation of the parser
      defineParser prsRec = (#)
        -- Tell the typeclass we are handling this field
        -- if it will handle the rest of them
        do mutualRL (Proxy :: Proxy rl') \r'Rec ->
            let
              -- Accumulate it in the other recursive invocations
              rRec = Record.insert sym (to (Comber prsRec) :: c) r'Rec
              -- Call the parser definition
              rDef /\ o = f rRec
              -- Split out the current parser definition
              r'Def = Record.delete sym rDef
              Comber prsDef = from (Record.get sym rDef)
            in r'Def /\ Tuple o prsDef
        -- Shuffle outputs
        do \(r'Def /\ Tuple o prsDef) -> prsDef /\ Tuple r'Def o
      -- Dispatch through `namedRec'`
      prsOut /\ Tuple r'Out o = namedRec' (reflectSymbol sym) defineParser
      -- Insert the external invocation of the parser into the record to return
      -- Accumulate the added grammar rules, through `Comber Void`, and wait
      -- until they are all accumulated to add it to the parser and insert
      -- it into the record
      rOut = r'Out #
        storeMore (Comber prsOut) \allRules ->
          Record.insert sym
            (to (Comber prsOut <|> map absurd allRules))

storeMore ::
  forall w f a b c r.
    Functor w =>
    Apply f =>
  f c ->
  (f b -> a -> r) ->
  StoreT (f b) w a ->
  StoreT (f b) w r
storeMore moreRules finalResult =
  over StoreT case _ of
    Tuple contW rulesSoFar -> Tuple
      (apply finalResult <$> contW)
      (moreRules *> rulesSoFar)
