-- | Easy to use version of the parser combinator, specialized to strings and
-- | regexes.
module Parser.Comb.Comber ( module Parser.Comb.Comber, module ReExports ) where

import Prelude

import Ansi.Codes as Ansi
import Ansi.Output as Ansi.Output
import Control.Alt (class Alt, (<|>))
import Control.Alt (class Alt, (<|>)) as ReExports
import Control.Alternative (class Alternative, class Plus)
import Control.Alternative (class Alternative, class Plus) as ReExports
import Control.Apply (lift2)
import Control.Apply (lift2) as ReExports
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
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Maybe (optional) as ReExports
import Data.Newtype (class Newtype, over, un)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Fetch (fetch)
import Parser.Comb (Comb(..), execute)
import Parser.Comb as Comb
import Parser.Comb.Codec (stateTableCodec)
import Parser.Comb.Combinators (buildTree, namedRec')
import Parser.Comb.Run (resultantsOf)
import Parser.Comb.Run as CombR
import Parser.Comb.Syntax (printSyntax')
import Parser.Comb.Types (ParseError)
import Parser.Comb.Types (ParseError) as ReExports
import Parser.Comb.Types as CombT
import Parser.Lexing (class ToString, type (~), Rawr) as ReExports
import Parser.Lexing (class ToString, type (~), Rawr, Similar(..), bestRegexOrString)
import Parser.Types (OrEOF)
import Parser.Types (OrEOF) as ReExports
import Parser.Types as P
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, to, from)
import Type.Proxy (Proxy(..))

newtype Comber a = Comber
  (Comb Rec String (String ~ Rawr) String a)
derive instance Newtype (Comber a) _
derive newtype instance functorComber :: Functor Comber
derive newtype instance applyComber :: Apply Comber
derive newtype instance applicativeComber :: Applicative Comber
derive newtype instance altComber :: Alt Comber
derive newtype instance plusComber :: Plus Comber
derive newtype instance alternativeComber :: Alternative Comber
derive newtype instance compactableComber :: Compactable Comber
derive newtype instance semigroupComber :: Semigroup a => Semigroup (Comber a)
derive newtype instance monoidComber :: Monoid a => Monoid (Comber a)
derive newtype instance heytingAlgebraComber :: HeytingAlgebra a => HeytingAlgebra (Comber a)

type Parsing a = CombR.Parsing String a

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

-- | Name a nonterminal production, this allows recursion.
namedRec :: forall a. String -> (Comber a -> Comber a) -> Comber a
namedRec name = Comber <<< Comb.namedRec name <<< coerce

-- | Name a parser. This may introduce ambiguity into the grammar.
named :: forall a. String -> Comber a -> Comber a
named name = Comber <<< Comb.named name <<< coerce

infixr 2 named as #:
infixr 5 namedRec as #->

-- | Return the source parsed by the given parser, instead of whatever its
-- | applicative result was.
sourceOf :: forall a. Comber a -> Comber String
sourceOf = over Comber Comb.sourceOf

-- | Return the source tokens parsed by the given parser, instead of whatever
-- | its applicative result was.
tokensSourceOf :: forall a. Comber a -> Comber (Array String)
tokensSourceOf = over Comber Comb.tokensSourceOf

topName :: String
topName = "TOP"

-- | Run the parser on the given string input. Please partially apply this since
-- | it has to build up an LR(1) table first, and this can be shared between
-- | inputs if it is partially applied.
parse :: forall a. Comber a -> (String -> Either String a)
parse = Comb.parseRegex topName <<< un Comber

parse' :: forall a. Comber a -> StateTable /\ (String -> Either String a)
parse' = Comb.parseRegex' topName <<< un Comber

type Conf = CombR.CConf String (String ~ Rawr) String String

parseWith :: forall a. Conf -> Comber a -> (String -> Either String a)
parseWith conf = Comb.parseWith conf topName <<< un Comber

parseWith' :: forall a. Conf -> Comber a -> StateTable /\ (String -> Either String a)
parseWith' conf = Comb.parseWith' conf topName <<< un Comber

--------------------------------------------------------------------------------

freeze :: forall a. Comber a -> Json
freeze = parse' >>> fst >>> C.encode stateTableCodec

freezeTable :: StateTable -> Json
freezeTable = C.encode stateTableCodec

thaw :: forall e a. Comber a -> Either e Json -> (String -> Either String a)
thaw comber json = case thaw' comber <$> json of
  Right (Right (_ /\ run)) -> run
  _ -> parse comber

fetchAndThaw :: forall a. Comber a -> String -> Aff (String -> Either String a)
fetchAndThaw comber url =
  fetch url {} >>= _.text >>> map (Json.parseJson >>> thaw comber)

thaw' :: forall a. Comber a -> Json -> Either CA.JsonDecodeError (StateTable /\ (String -> Either String a))
thaw' (Comber comber) = C.decode stateTableCodec >>> map \states ->
  states /\ execute { best: bestRegexOrString }
    { states
    , resultants: topName /\ resultantsOf comber
    , options: buildTree topName comber
    }

thawWith :: forall e a. Conf -> Comber a -> Either e Json -> (String -> Either String a)
thawWith conf comber json = case thawWith' conf comber <$> json of
  Right (Right (_ /\ run)) -> run
  _ -> parseWith conf comber

fetchAndThawWith :: forall a. Conf -> Comber a -> String -> Aff (String -> Either String a)
fetchAndThawWith conf comber url =
  fetch url {} >>= _.text >>> map (Json.parseJson >>> thawWith conf comber)

thawWith' :: forall a. Conf -> Comber a -> Json -> Either CA.JsonDecodeError (StateTable /\ (String -> Either String a))
thawWith' conf (Comber comber) = C.decode stateTableCodec >>> map \states ->
  states /\ execute conf
    { states
    , resultants: topName /\ resultantsOf comber
    , options: buildTree topName comber
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

type Rec = CombT.Rec String (OrEOF String) String
type States = CombR.CStates String (String ~ Rawr)
type StateInfo = CombR.CStateInfo String (String ~ Rawr)
type State = CombR.CState String (String ~ Rawr)
type Resultant = CombT.CResultant Rec String String
type Options = CombT.COptions Rec String (String ~ Rawr) String
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
  ((String -> Either ParseError a) -> b -> c) ->
  Comber c
withReparserFor name (Comber aux) (Comber body) f =
  Comber (CombR.withReparserFor name aux body f)


--------------------------------------------------------------------------------

-- | Surround the parse with literal tokens.
delim :: forall s a. ToString s => s -> s -> Comber a -> Comber a
delim x y z = token x *> z <* token y

many :: forall a. String -> Comber a -> Comber (Array a)
many n p = Array.fromFoldable <$> n #-> \more ->
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

toString :: Printer String
toString =
  { nonTerminal: identity
  , rule: show
  , literal: show
  , regex: identity
  , meta: identity
  , lines: intercalate "\n"
  }

toAnsi :: Printer String
toAnsi =
  { nonTerminal: colorful Ansi.Blue <<< toString.nonTerminal
  , rule: colorful Ansi.Cyan <<< toString.rule
  , literal: colorful Ansi.Yellow <<< toString.literal
  , regex: colorful Ansi.Green <<< toString.regex
  , meta: identity <<< toString.meta
  , lines: toString.lines
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
  items # foldMap case _ of
    { pName: Right pName, rName: Just rName, rule: P.Zipper l' r' }
      | Just l <- normal l', Just r <- normal r' ->
        [ fold
          [ p.nonTerminal pName
          , p.meta "."
          , p.rule rName
          , p.meta " = "
          , printZipper p (P.Zipper l r)
          ]
        ]
    _ -> mempty
  where
  normal :: P.Fragment (Either _ _) _ -> Maybe (P.Fragment _ _)
  normal frag = for frag case _ of
    P.Terminal (P.Continue a) -> Just (P.Terminal a)
    P.NonTerminal (Right a) -> Just (P.NonTerminal a)
    _ -> Nothing

printReduction :: PrintFn (Either String String /\ Maybe Int)
printReduction p (Right name /\ Just num) =
  p.nonTerminal name <> p.meta "." <> p.rule num
printReduction _ _ = mempty


printConflicts :: PrintFn StateTable
printConflicts p { states: states@(P.States index) } =
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
                  _ -> mempty
              , p.meta ")"
              ]
            , fold do
                s <- P.unShift conflict
                st <- index !! s
                pure $ p.lines $ Array.cons (p.meta "  Shift to") $
                  (p.meta "  -> " <> _) <$> printState p st.items
            , p.meta "  Or reduce"
            , p.lines $ P.reductions conflict <#> \r ->
                p.meta "  <- " <> printReduction p r
            , p.meta "  With items"
            , p.lines $ (p.meta "  - " <> _) <$> printState p items
            ]
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
  fst $ mutual' (Tuple <$> defineParsers <@> unit)

mutual' ::
  forall r rl o.
    RL.RowToList r rl =>
    MutualCombers rl r =>
  (Record r -> Record r /\ o) -> Record r /\ o
mutual' = mutualRL (Proxy :: Proxy rl)

class MutualCombers :: RL.RowList Type -> Row Type -> Constraint
class MutualCombers rl r | rl -> r where
  mutualRL :: forall o. Proxy rl -> (Record r -> Record r /\ o) -> Record r /\ o

instance mutualNil :: MutualCombers RL.Nil () where
  mutualRL _ f = f {}

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
      -- Accumulate the external invocation of the parser to return
      rOut = Record.insert sym (to (Comber prsOut)) r'Out
