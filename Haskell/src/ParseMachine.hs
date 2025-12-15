-- A parsing machine inspired by “Comonads as Spaces” by Phil Freeman
-- (https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html)
{-# LANGUAGE AllowAmbiguousTypes, ApplicativeDo, ScopedTypeVariables, OverloadedStrings, DerivingVia, GeneralizedNewtypeDeriving, LambdaCase, BlockArguments, DataKinds #-}
module ParseMachine where

import Prelude hiding (filter, lex)
import qualified Data.Map as Map
import Control.Applicative (Alternative(..), asum)
import Control.Monad (join, void)
import Data.Function ((&))
import Data.Void (absurd)
import Witherable (Filterable (mapMaybe, filter))
import Data.Maybe (isNothing)
import Data.Char (isControl)
import GHC.Generics (Generic)
import Data.Foldable (for_, traverse_)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownChar, charVal)
import Data.String (IsString (fromString))

--------------------------------------------------------------------------------
-- Tutorial / introduction                                                    --
--------------------------------------------------------------------------------

-- A monadic parser encoded as an infinite lazy tree, which branches on a token
-- classification `ix`, receives token values of type `tok` along the way, and
-- ultimately returns a result `r` or fails.
--
-- The basic idea going on here: at each state (position) in the parse machine
-- we can accept a token from the map (where tokens are indexed by `ix`: the
-- category of token you want to accept, before you receive the actual data
-- via `tok`).
--
-- This does not include any backtracking: there is only one possible walk
-- through the `ProtoParseMachine` for a given input, and it is a _linear_ walk,
-- with strictly increasing depth. The real `ParseMachine` below adds
-- backtracking (which requires a bit more complexity than you would expect),
-- `Coyoneda` (to handle maps/binds more efficiently), and fallbacks for
-- when `ix` fails to match the current token: there is no reason for the `Map`
-- to be exhaustive, it just makes it more efficient, theoretically.
data ProtoParseMachine ix tok r = ProtoParseMachine
  { acceptP :: Map.Map ix (tok -> ProtoParseMachine ix tok r)
  , finishP :: Maybe r -- only called on EOF in this proto parser, unlike
                       -- below where it is used for stopping early for staging
  } deriving (Functor)

protoParse :: Ord ix => (tok -> ix) -> ProtoParseMachine ix tok r -> [tok] -> Maybe r
protoParse _        (ProtoParseMachine { finishP }) [] = finishP
protoParse classify (ProtoParseMachine { acceptP }) (tok : toks) = do
  -- We classify the token by `ix`, which we look up in the map
  Map.lookup (classify tok) acceptP
    -- Then we let it receive the actual `tok` value, and continue onwards
    >>= \receive -> protoParse classify (receive tok) toks

instance Ord ix => Alternative (ProtoParseMachine ix tok) where
  empty = ProtoParseMachine Map.empty Nothing
  -- Merge the tokens to parse, and the EOF behaviors
  ProtoParseMachine steps1 final1 <|> ProtoParseMachine steps2 final2 =
    ProtoParseMachine (Map.union steps1 steps2) (final1 <|> final2)

-- `>>=` is interesting because it accumulates the tokens to expect before
-- it runs (in the case of monadic parsing, this is generally interleaved
-- with actual parsing thanks to laziness)
instance Ord ix => Monad (ProtoParseMachine ix tok) where
  ProtoParseMachine steps final >>= cont =
    -- If a step succeeds, it binds the continuation... eventually
    ProtoParseMachine ((fmap . fmap) (>>= cont) steps) Nothing
      -- Otherwise we are allowed to start the continuation on the fallback
      <|> maybe empty cont final

instance Ord ix => Applicative (ProtoParseMachine ix tok) where
  pure = ProtoParseMachine Map.empty . Just
  mf <*> ma = mf >>= (<$> ma)


--------------------------------------------------------------------------------
-- Full parser type and parser interpreter                                    --
--------------------------------------------------------------------------------


-- The full parser, with backtracking and a catchall case after the `Map`.
--
-- It proceeds by gathering indexed tokens into a `Map ix`, one layer of
-- backtracking at a time, and then matching it on the stream. If it fails,
-- backtracking proceeds at the next `MatchTrack`. Finally, when the stream
-- is exhausted, the `Maybe r` at the end of the tunnel is returned.
--
-- Interestingly this statically asserts part of the backtracking behavior of
-- `Parsec`: for a parser with singleton `MatchTrack`s, there is no backtracking
-- since each map only has one behavior for the `ix` that it sees.
--
-- I think using the `Coyoneda` trick to factor out the implicit state should
-- allow finite applicative parsers (for context-free grammars) to be finite data
-- structures in memory. Otherwise it is just lazy for absolutely no reason, and
-- repeated usages of the same parser instance would leak memory.
data ParseMachine ix tok r = ParseStep
  { accept ::
    -- `MatchTrack` gives backtracking behavior by allowing several `Map`s`
    -- to propose the tokens that should match (without backtracking, the
    -- first result for a token wins, via left-biased `Map.union`)
   !( MatchTrack
      -- We include a catchall, to match any single token
      ( WithCatchall
        -- The `Map`s index what _kind_ of tokens to accept and contain
        -- continuations which receive the particular _value_ of the token
        (Coyoneda (Map.Map ix))
        -- The continuation is staged so that the first stage can include
        -- backtracking to the next `Map` in the `MatchTrack`, while the second
        -- stage commits locally to the parse (notwithstanding `try`s further back)
        (tok -> ParseMachine ix tok (ParseMachine ix tok r))
    ) )
  , finish ::
    -- Finally there is a pure value: either a result for EOF, hopefully, or,
    -- for intermediate steps, a fallback that is used to call the next continuation
    Maybe r
  }
  deriving (Functor)
-- A monoid (with suggestive names) that overlaps elements by one when appending
data MatchTrack m = NoMatch | MatchTrack m (MatchTrack m)
  deriving (Functor)
type ParseMatchTrack ix tok r = MatchTrack
  ( WithCatchall
    (Coyoneda (Map.Map ix))
    (tok -> ParseMachine ix tok (ParseMachine ix tok r))
  )

classified :: (tok -> ix) -> [tok] -> [(ix, tok)]
classified classifier = fmap \(!tok) -> (classifier tok, tok)

-- Basic parsing through the above type (obviously it would be good to add state
-- and errors and so on: pass down the currently expected tokens during
-- backtracking, for example)
parseTrailing :: Ord ix => ParseMachine ix tok r -> [(ix, tok)] -> Maybe (r, [(ix, tok)])
parseTrailing (ParseStep _ mr) [] = (,[]) <$> mr                     -- EOF, expected or not
parseTrailing (ParseStep NoMatch mr) remaining = (,remaining) <$> mr -- Ran out of tokens
-- Specialize this case to avoid leaking tokens when no backtracking is possible
parseTrailing (ParseStep (MatchTrack (WithCatchall (Coyoneda fromMap ixMap) catchall) more) Nothing) ((!ix, tok) : continue) | hasNoMatches more =
  case fromMap <$> Map.lookup ix ixMap <|> catchall of
    Just receive -> parseTrailing (join (receive tok)) continue
    Nothing -> Nothing -- Unexpected token
parseTrailing (ParseStep (MatchTrack (WithCatchall (Coyoneda fromMap ixMap) catchall) more) mr) backtrack@((!ix, tok) : continue) =
  case fromMap <$> Map.lookup ix ixMap <|> catchall of
    -- Here we try to shallowly parse the matching case, up to the next bind that
    -- is not covered by the current `try`
    Just receive | Just (committed, remaining) <- parseTrailing (receive tok) continue ->
      -- Now we are hopefully back on a happy path with no backtracking for a while
      parseTrailing committed remaining
    -- If not, we have to backtrack
    _ -> parseTrailing (ParseStep more mr) backtrack

-- Ensure that the parser reached EOF before returning the result
parse :: Ord ix => ParseMachine ix tok a -> [(ix, tok)] -> Maybe a
parse parser toks = case parseTrailing parser toks of
  Just (r, []) -> Just r
  _ -> Nothing

-- Specialize it to `Char`s
type StringParseMachine = ParseMachine Char Char
stringparseTrailing :: StringParseMachine r -> String -> Maybe (r, String)
stringparseTrailing m = (fmap . fmap . fmap) snd . parseTrailing m . map (join (,))

stringParse :: StringParseMachine r -> String -> Maybe r
stringParse m = parse m . map (join (,))


--------------------------------------------------------------------------------
-- Basic combinators                                                          --
--------------------------------------------------------------------------------


-- The `try` combinator extends the `MatchTrack` with one additional `Map`
-- to allow backtracking: that forces the next `<|>` to start anew instead of
-- trying to add to the existing matches
--
-- `try . try = try`
try :: ParseMachine ix tok r -> ParseMachine ix tok r
try (ParseStep matchtrack mr) = ParseStep (extend $ compress matchtrack) mr where
  -- Extend with a fresh `MatchTrack`
  extend NoMatch = MatchTrack (WithCatchall (Coyoneda absurd Map.empty) Nothing) NoMatch
  extend (MatchTrack possibilities more) =
    MatchTrack possibilities (extend more)
  -- Compress both stages into the first stage, so they trigger backtracking:
  -- further binds will go on the fresh second stage and not trigger backtracking
  compress = fmap . fmap . fmap $ _pure . (`tryBind` id) -- `pure . tryJoin`

-- The opposite of `try`: if it consumes one token, it fails hard instead of
-- backtracking into the next alternative. (Could still be caught in higher
-- backtracking, of course.)
--
-- They aren't exactly inverses: `commit . try . commit = commit`, since
-- `commit` is a little more destructive: it undoes all `try`s
commit :: ParseMachine ix tok r -> ParseMachine ix tok r
commit (ParseStep matchtrack mr) = ParseStep (trim $ compress matchtrack) mr where
  -- Extend with a fresh `MatchTrack`
  trim possibilities | hasNoMatches possibilities = NoMatch
  trim (MatchTrack possibilities more) = MatchTrack possibilities (trim more)
  trim NoMatch = NoMatch
  -- Compress both stages into the *second* stage, so they *do not* trigger
  -- backtracking
  compress = fmap . fmap . fmap $ (`tryBind` id) . _pure -- `tryJoin . pure`

-- Accepts a single kind of token and calls the continuation with its value
token :: ix -> ParseMachine ix tok tok
token ix = ParseStep
  (MatchTrack
    (WithCatchall step Nothing)
    NoMatch
  ) Nothing where
  step = Coyoneda
    ((_pure . _pure) .)
    (Map.singleton ix id)

-- Note: this consumes the token like `token` does, preventing backtracking
-- without an explicit `try`.
anyToken :: ParseMachine ix tok tok
anyToken = ParseStep
  (MatchTrack
    (WithCatchall
      (Coyoneda absurd Map.empty)
      (Just (_pure . _pure))
    )
    NoMatch
  ) Nothing

satisfies :: (tok -> Bool) -> ParseMachine ix tok tok
satisfies p = try $ filter p anyToken


--------------------------------------------------------------------------------
-- Instances / implementation                                                 --
--------------------------------------------------------------------------------


-- First we implement `Monoid (MatchTrack m)`
instance Monoid m => Monoid (MatchTrack m) where
  mempty = NoMatch
-- This appends the lists together with *one* element of overlap
instance Semigroup m => Semigroup (MatchTrack m) where
  NoMatch <> r = r
  l <> NoMatch = l
  MatchTrack trySome NoMatch <> MatchTrack more remaining =
    MatchTrack (trySome <> more) remaining
  MatchTrack priority trailing <> r =
    MatchTrack priority (trailing <> r)

-- Now we get to the parser instances

instance Ord ix => Alternative (ParseMachine ix tok) where
  empty = ParseStep NoMatch Nothing
  -- If there is a catchall already, do nothing
  l@(ParseStep _ (Just _)) <|> _ = l
  -- Otherwise append the steps, with one element of overlap as discussed above,
  -- to avoid needless backtracking
  ParseStep steps1 Nothing <|> ParseStep steps2 final =
    ParseStep (steps1 <> steps2) final

instance Ord ix => Applicative (ParseMachine ix tok) where
  pure = ParseStep NoMatch . Just
  mf <*> ma = mf >>= (<$> ma) -- this might need better sharing?

-- `>>=` is interesting because it accumulates the tokens to expect before
-- it runs (in the case of monadic parsing, this is generally interleaved
-- with actual parsing thanks to laziness)
instance Ord ix => Monad (ParseMachine ix tok) where
  ParseStep steps final >>= cont =
    -- If a step succeeds, it binds the continuation... eventually.
    -- The last `fmap` is to nestle it in the nested parser
    -- `ParseMachine ix tok (ParseMachine ix tok r)`, so that it sequences
    -- after the alternatives in `(try x <|> y) >>= f`, instead of distributing
    -- inside as `(try (x >>= f) <|> (y >>= f))`.
    ParseStep ((fmap . fmap . fmap . fmap) (>>= cont) steps) Nothing
      -- Otherwise we are allowed to start the continuation on the fallback
      <|> maybe empty cont final

-- `tryAlt l r = try l <|> r` which does not need `Ord ix`
tryAlt :: ParseMachine ix tok r -> ParseMachine ix tok r -> ParseMachine ix tok r
tryAlt (ParseStep NoMatch final1) (ParseStep steps2 final2) =
    ParseStep steps2 (final1 <|> final2)
tryAlt (ParseStep (MatchTrack step steps) final) r =
  let ParseStep steps' final' = tryAlt (ParseStep steps final) r in
  ParseStep (MatchTrack step steps') final'

-- Avoid `Ord ix` for different semantics: `tryBind m f = try $ m >>= f`
tryBind :: ParseMachine ix tok i -> (i -> ParseMachine ix tok r) -> ParseMachine ix tok r
tryBind (ParseStep steps final) cont =
  ParseStep ((fmap . fmap . fmap) (\m -> _pure $ tryBind m id `tryBind` cont) steps) Nothing
    `tryAlt` maybe (ParseStep NoMatch Nothing) cont final

-- Avoids an `Ord ix` constraint
_pure :: r -> ParseMachine ix tok r
_pure = ParseStep NoMatch . Just


-- Does not call `try` (that would violate `mapMaybe pure = id`)
instance Filterable (ParseMachine ix tok) where
  mapMaybe p (ParseStep steps final) = ParseStep steps' (mapMaybe p final) where
    steps' = (fmap . fmap . fmap . fmap) (mapMaybe p) steps
  filter p (ParseStep steps final) = ParseStep steps' (filter p final) where
    steps' = (fmap . fmap . fmap . fmap) (filter p) steps

--------------------------------------------------------------------------------
-- Usage as a lexer (tokenizer): prefer longest matches, more backtracking    --
--------------------------------------------------------------------------------

-- Measure the depth down the parse machine tree (that is, the number of tokens
-- it consumed)
depth :: (Int -> i -> o) -> ParseMachine ix tok i -> ParseMachine ix tok o
depth = go 0 where
  -- We need bounded polymorphic recursion to handle the staged continuations
  go :: forall ix tok x y. Int -> (Int -> x -> y) -> ParseMachine ix tok x -> ParseMachine ix tok y
  go !i f (ParseStep steps finish) = ParseStep
    ((fmap . fmap . fmap) (go (i+1) \j -> go j f) steps)
    (f i <$> finish)

withDepth :: ParseMachine ix tok r -> ParseMachine ix tok (Int, r)
withDepth = depth (,)

-- Return the tokens parsed during the parser
withSource :: ParseMachine ix tok r -> ParseMachine ix tok ([tok], r)
withSource = fmap (\(acc, r) -> (reverse acc, r)) . go [] (,) where
  -- We need bounded polymorphic recursion to handle the staged continuations
  go :: forall ix tok x y. [tok] -> ([tok] -> x -> y) -> ParseMachine ix tok x -> ParseMachine ix tok y
  go !acc f (ParseStep steps finish) = ParseStep
    ((fmap . fmap) (\cont tok -> go (tok : acc) (\j -> go j f) (cont tok)) steps)
    -- We can't reverse here, because it is called many times
    -- during staging and such, just reverse once at the top level
    (f acc <$> finish)

readSource :: forall r ix void. Read r => ParseMachine ix Char void -> ParseMachine ix Char r
readSource = fmap (\(src, _) -> read src) . withSource

newtype Lexer ix tok out r = Lexer (ParseMachine ix tok ([out], r))
  deriving (Functor)

emit :: out -> Lexer ix tok out ()
emit = parseToken . _pure

parseToken :: ParseMachine ix tok out -> Lexer ix tok out ()
parseToken p = Lexer ((\out -> ([out], ())) <$> p)

scanToken :: ParseMachine ix tok any -> Lexer ix tok [tok] ()
scanToken p = parseToken (fst <$> withSource p)

skipToken :: ParseMachine ix tok any -> Lexer ix tok out ()
skipToken p = Lexer (([], ()) <$ p)

instance Ord ix => Alternative (Lexer ix tok out) where
  empty = Lexer empty
  -- More backtracking: use `tryAlt l r = try l <|> r` instead of the derived `<|>`
  Lexer l <|> Lexer r = Lexer (l `tryAlt` r)

instance Ord ix => Applicative (Lexer ix tok out) where
  pure = Lexer . pure . ([],)
  mf <*> ma = mf >>= (<$> ma)

instance Ord ix => Monad (Lexer ix tok out) where
  -- Nothing special(? use `tryAlt`?), just accumulate
  Lexer mi >>= f = Lexer do
    (!acc, i) <- mi
    let Lexer mr = f i
    (\(!more, r) -> (acc ++ more, r)) <$> mr

lexTrailing :: forall ix tok out r. Ord ix => Lexer ix tok out r -> [(ix, tok)] -> Maybe (([out], r), [(ix, tok)])
lexTrailing (Lexer (ParseStep _ mr)) [] = (,[]) <$> mr
lexTrailing (Lexer (ParseStep NoMatch mr)) remaining = (,remaining) <$> mr
-- Specialize this case to avoid leaking tokens when no backtracking is possible
lexTrailing (Lexer (ParseStep (MatchTrack (WithCatchall (Coyoneda fromMap ixMap) catchall) more) Nothing)) ((!ix, tok) : continue) | hasNoMatches more =
  case fromMap <$> Map.lookup ix ixMap <|> catchall of
    Just receive -> lexTrailing (Lexer (join (receive tok))) continue
    Nothing -> Nothing -- Unexpected token
lexTrailing (Lexer (ParseStep shallow mr)) backtrack@((!ix, tok) : continue) =
    scanning Nothing shallow
  where
  -- Apply *all* of the backtracking and take the longest result, before the
  -- next bind. Then commit to it and run whichever bind continuation was chosen.
  scanning ::
    Maybe (Int, (ParseMachine ix tok ([out], r), [(ix, tok)])) ->
    ParseMatchTrack ix tok ([out], r) -> Maybe (([out], r), [(ix, tok)])
  scanning !maybeLongest (MatchTrack (WithCatchall (Coyoneda fromMap ixMap) catchall) more) =
    case fromMap <$> Map.lookup ix ixMap <|> catchall of
      Just receive | Just ((!parsedDepth, committed), remaining) <- parseTrailing (withDepth (receive tok)) continue ->
        scanning (longer maybeLongest (parsedDepth, (committed, remaining))) more
      _ -> scanning maybeLongest more
  scanning Nothing NoMatch = (,backtrack) <$> mr
  scanning (Just (_parsedDepth, (committed, remaining))) NoMatch =
    lexTrailing (Lexer committed) remaining

  longer :: forall d. Maybe (Int, d) -> (Int, d) -> Maybe (Int, d)
  longer Nothing d = Just d
  longer (Just (l1, d1)) (l2, d2) =
    if l1 >= l2 then Just (l1, d1) else Just (l2, d2)

lex :: forall ix tok out r. Ord ix => Lexer ix tok out r -> [(ix, tok)] -> Maybe ([out], r)
lex lexer input = case lexTrailing lexer input of
  Just (r, []) -> Just r
  _ -> Nothing

lexAndParse ::
  forall ix1 ix2 tok1 tok2 r.
    Ord ix1 => Ord ix2 =>
  (tok1 -> ix1, tok2 -> ix2) -> Lexer ix1 tok1 tok2 (ParseMachine ix2 tok2 r) -> [tok1] -> Maybe r
lexAndParse (classify1, classify2) lexer input = do
  (tokens, parser) <- lex lexer $ classified classify1 input
  parse parser $ classified classify2 tokens

--------------------------------------------------------------------------------
-- Extra helpers                                                              --
--------------------------------------------------------------------------------

-- Map every index of `ParseMachine` (destroys sharing)
fullMap ::
  Ord ix' => (ix -> ix') -> (tok' -> tok) -> (i -> o) ->
  ParseMachine ix tok i -> ParseMachine ix' tok' o
fullMap f g h (ParseStep possibilities final) =
  ParseStep
    (fmap mapContinuation . mapCoyoneda <$> possibilities)
    (h <$> final)
  where
  mapCoyoneda (WithCatchall (Coyoneda x y) z) =
    WithCatchall (Coyoneda x (Map.mapKeys f y)) z
  mapContinuation z = fullMap f g (fullMap f g h) . z . g

noSharing :: Ord ix => ParseMachine ix tok r -> ParseMachine ix tok r
noSharing = fullMap id id id

hasNoMatches :: MatchTrack (WithCatchall (Coyoneda (Map.Map k)) a) -> Bool
hasNoMatches (MatchTrack (WithCatchall (Coyoneda _ ixMap) catchall) more) =
  Map.null ixMap && isNothing catchall && hasNoMatches more
hasNoMatches NoMatch = True

--------------------------------------------------------------------------------
-- Boring helper types                                                        --
--------------------------------------------------------------------------------

data WithCatchall f a = WithCatchall !(f a) (Maybe a)
  deriving (Functor)

instance (Monoid (f a)) => Monoid (WithCatchall f a) where
  mempty = WithCatchall mempty Nothing
instance (Semigroup (f a)) => Semigroup (WithCatchall f a) where
  WithCatchall l (Just catchall) <> WithCatchall r _ =
    WithCatchall (l <> r) (Just catchall)
  WithCatchall l Nothing <> WithCatchall r catchall =
    WithCatchall (l <> r) catchall


data Coyoneda f a = forall b. Coyoneda (b -> a) !(f b)

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g v) = Coyoneda (f . g) v

-- Note: this means that applicatives really need to be finite, to fit in
-- finite space! because we can share the `Map`s across several invocations of
-- `fmap` via `Coyoneda`, but we cannot share them across infinite chained
-- `try <|>`. See this blog post for an explanation of how that would allow some
-- context sensitive languages to be parsed by otherwise applicative parsers:
-- https://byorgey.wordpress.com/2012/01/05/parsing-context-sensitive-languages-with-applicative/
instance Ord ix => Semigroup (Coyoneda (Map.Map ix) v) where
  Coyoneda _ l <> r | Map.null l = r
  l <> Coyoneda _ r | Map.null r = l
  Coyoneda f l <> Coyoneda g r = Coyoneda (either f g)
    ((Left <$> l) `Map.union` (Right <$> r))


--------------------------------------------------------------------------------
-- Sugar                                                                      --
--------------------------------------------------------------------------------

-- Whitespace
ws :: ParseMachine Char a [a]
ws = many (token ' ' <|> token '\n' <|> token '\r' <|> token '\t')

-- Append with whitespace in between
(.>) :: ParseMachine Char tok x -> ParseMachine Char tok y -> ParseMachine Char tok y
p .> q = p *> ws *> q
(<.) :: ParseMachine Char tok x -> ParseMachine Char tok y -> ParseMachine Char tok x
p <. q = p <* ws <* q
(<.>) :: ParseMachine Char tok (x -> y) -> ParseMachine Char tok x -> ParseMachine Char tok y
p <.> q = p <* ws <*> q

-- Why shouldn't a string literal be a literal parser?
instance (ix ~ Char, tok ~ Char, r ~ ()) => IsString (ParseMachine ix tok r) where
  fromString = traverse_ token

-- Sugar
class Q c t where
  qq :: t

instance KnownChar c => Q (c :: Char) (StringParseMachine ()) where
  qq = void $ token $ charVal $ Proxy @c

--------------------------------------------------------------------------------
-- EXAMPLES: JSON                                                             --
--------------------------------------------------------------------------------

type JSONLexer = Lexer Char Char JTok
type JSONL r = StringParseMachine ([JTok], r)
type JSONParser = ParseMachine JCls JTok

data JTok = TLBrace | TRBrace | TLBracket | TRBracket | TComma | TColon | TTrue | TFalse | TNull | TString String | TNumber !Double
  deriving (Eq, Ord, Generic, Show)
data JCls =  LBrace |  RBrace |  LBracket |  RBracket |  Comma |  Colon | CTrue | CFalse |  Null |  String        |  Number
  deriving (Eq, Ord, Generic, Show)
data JSON = JString String | JNumber !Double | JArray [JSON] | JObject [(String, JSON)] | JBool Bool | JNull
  deriving (Eq, Ord, Generic, Show)

instance Q 'String (JSONParser String) where
  qq = token String & mapMaybe \case { TString v -> Just v; _ -> Nothing }
instance Q 'Number (JSONParser Double) where
  qq = token Number & mapMaybe \case { TNumber v -> Just v; _ -> Nothing }

parseJSON :: String -> Maybe JSON
parseJSON = lexAndParse (id, jsonClassify) (jsonParser <$ jsonLexer)

-- I'm disappointed this isn't turned into a tag coercion...
jsonClassify :: JTok -> JCls
jsonClassify = \case
  TLBrace -> LBrace
  TRBrace -> RBrace
  TLBracket -> LBracket
  TRBracket -> RBracket
  TComma -> Comma
  TColon -> Colon
  TTrue -> CTrue
  TFalse -> CFalse
  TNull -> Null
  TString _ -> String
  TNumber _ -> Number

jsonParser :: JSONParser JSON
jsonParser = asum
  [ JString <$> qq @'String
  , JNumber <$> qq @'Number
  , JObject <$> do token LBrace   *> separated member     <* token RBrace
  , JArray  <$> do token LBracket *> separated jsonParser <* token RBracket
  , JBool   <$> do True <$ token CTrue <|> False <$ token CFalse
  , JNull   <$  token Null
  ]
  where
  comma = token Comma
  separated :: forall r. JSONParser r -> JSONParser [r]
  separated p = liftA2 (:) p (many (comma *> p)) <|> pure []

  member = (\k _ v -> (k, v))
    <$> do qq @'String
    <*> do token Colon
    <*> do jsonParser

jsonDirect :: StringParseMachine JSON
jsonDirect =  (\p -> pure () .> p <. pure ()) $ asum
  [ JString <$> jsonString
  , JNumber <$> jsonNumber
  , JObject <$> do "{" .> separated member     <. "}"
  , JArray  <$> do "[" .> separated jsonDirect <. "]"
  , JBool   <$> do True <$ "true" <|> False <$ "false"
  , JNull   <$ "null"
  ]
  where
  separated :: forall r. StringParseMachine r -> StringParseMachine [r]
  separated p = liftA2 (:) p (many ("," *> p)) <|> pure []

  member = (,) <$> jsonString <* ":" <*> jsonDirect

jsonLexer :: JSONLexer ()
jsonLexer =
  wsTok *> do
    mconcat <$> many (jsonTok <* wsTok)
  where
  wsTok = skipToken $ many (token ' ' <|> token '\n' <|> token '\r' <|> token '\t')

  jsonTok = asum
    [ Lexer symbols
    , parseToken $ TString <$> jsonString
    , parseToken $ TNumber <$> jsonNumber
    ]

  symbols :: JSONL ()
  symbols = foldr (\(str, val) more -> keyword str val <|> more) empty symbolTable
  symbolTable :: [(String, JTok)]
  symbolTable =
    [ ("{", TLBrace)
    , ("}", TRBrace)
    , ("[", TLBracket)
    , ("]", TRBracket)
    , (",", TComma)
    , (":", TColon)
    , ("true", TTrue)
    , ("false", TFalse)
    , ("null", TNull)
    ]

tokMap :: forall ix tok r. Ord ix => [(ix, r)] -> ParseMachine ix tok r
tokMap = foldr (\(chr, val) more -> val <$ token chr <|> more) empty

-- A keyword is a series of tokens
keyword :: (Foldable t, Ord ix) => t ix -> a1 -> ParseMachine ix a2 ([a1], ())
keyword cs val = foldr (\c -> (token c *>)) (pure ([val], ())) cs

base :: Int -> [Int] -> Int
base b = fst . foldr (\d (!acc, !pl) -> (d*pl + acc, pl*b)) (0, 1)

hexDigits :: [(Char, Int)]
hexDigits = decDigits <> zip ['a'..'f'] [10..15] <> zip ['A'..'F'] [10..15]

hex :: ParseMachine Char tok Int
hex = tokMap hexDigits

decDigits :: [(Char, Int)]
decDigits = zip ['0'..'9'] [0..9]

jsonEscapes :: [(Char, Char)]
jsonEscapes =
  [ ('"', '"')
  , ('\\', '\\')
  , ('/', '/')
  , ('b', '\b')
  , ('f', '\f')
  , ('n', '\n')
  , ('r', '\r')
  , ('t', '\t')
  ]

jsonNumber :: StringParseMachine Double
jsonNumber = readSource do
  let digit = tokMap decDigits
  let t = void . token
  let opt p = p <|> pure ()
  opt do t '-' <|> pure ()
  t '0' <|> void (some digit)
  opt do t '.' <* some digit
  opt do (t 'e' <|> t 'E') <* opt (t '+' <|> t '-') <* some digit

jsonString :: StringParseMachine String
jsonString = token '"' *> charList
  where
  -- TODO: fix the fact that `token '"'` needs to be in front?
  charList = [] <$ token '"' <|> (:) <$> char <*> charList
  char = (token '\\' *> escape)
    <|> satisfies (not . isControl)
  escape = tokMap jsonEscapes <|> uXXXX

uXXXX :: ParseMachine Char a Char
uXXXX = token 'u' *> do
  (\m n o p -> toEnum (base 16 [m,n,o,p]) :: Char)
    <$> hex <*> hex <*> hex <*> hex

main :: IO ()
main = do
  let
    examples =
      [ "null"
      , "{}"
      , "[true]"
      , "\"\""
      , "\"x\""
      , "\"xy\""
      , "\"\\\"\""
      , "\"a\\u0304\""
      , "\"string\""
      , "0"
      , "[0,1,2]"
      , "[0 ,1, 2]"
      , "45"
      , " 45.2  "
      , "-0.34e12"
      , "{\"x\":true}"
      ]
  for_ examples \s -> do
    print s
    print $ lex jsonLexer $ join (,) <$> s
    print $ parseJSON s
    print $ parse jsonDirect $ join (,) <$> s
