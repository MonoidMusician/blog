module Parser.Comb.Run where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Filterable (filterMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Parser.Algorithms (getResultCM', getResultICM', revertCST', revertICST')
import Parser.Comb.Combinators (buildTree, named)
import Parser.Comb.Types (Associativity, CCST, CGrammar, CGrammarRule, COptions, CResultant, CSyntax, Comb(..), Options(..), Resultant(..), fullMapOptions, matchRule)
import Parser.Lexing (class Tokenize, Best, FailReason(..), FailedStack(..), Rawr, ResolvePrec, Similar, bestRegexOrString, contextLexingParse, happyPrecedenceOrd, longest, screenPrecedence, (?), (??))
import Parser.Optimized.Algorithms (statesNumberedMany)
import Parser.Optimized.Types (unsafeFromJust)
import Parser.Proto (topOf)
import Parser.Types (Fragment, Grammar(..), OrEOF(..), Part(..), State, StateInfo, States, notEOF)

type CBest :: forall k. Type -> k -> Type -> Type -> Type -> Type -> Type -> Type -> Type
type CBest err prec meta air nt cat i o =
  Best err Int (Fragment meta (Either nt nt) (OrEOF cat) /\ Either nt nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
type CConf :: forall k. Type -> k -> Type -> Type -> Type -> Type -> Type -> Type -> Type
type CConf err prec meta air nt cat i o =
  { best :: CBest err prec meta air nt cat i o
  }
type CStates meta nt cat =
  States Int meta (Either nt nt) (Maybe Int) (OrEOF cat)
type CStateInfo meta nt cat =
  StateInfo Int meta (Either nt nt) (Maybe Int) (OrEOF cat)
type CState meta nt cat =
  State meta (Either nt nt) (Maybe Int) (OrEOF cat)

type ParseError err meta air nt cat i o =
  FailedStack err Int (Rec err meta air nt cat i o) meta air (Either nt nt) (Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
newtype Rec err meta air nt cat i o = Rec
  ( forall a.
      nt -> i ->
      COptions (Rec err meta air nt cat i o) err meta air nt cat o ->
      (CCST air nt o -> Either (Array err) a) ->
      Either (ParseError err meta air nt cat i o) a
  )

type StateTable meta nt cat =
  { stateMap :: Map nt Int
  , start :: Int
  , states :: CStates meta nt cat
  }

-- | Parse an input. You should partially apply it, and reuse that same
-- | partially applied function for multiple inputs.
parse ::
  forall err prec meta air nt cat i o a.
    Ord prec =>
    Semiring meta =>
    Ord meta =>
    Eq air =>
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Tokenize meta (OrEOF i) air =>
    Eq o =>
  nt -> Comb (Rec err meta air nt cat i o) err prec meta air nt cat o a ->
  (i -> Either (ParseError err meta air nt cat i o) a)
parse = parseWith { best: longest }

-- | Parsing optimized for regexes and string literals, where the string
-- | literals take precedence over the regexes always (and not just when they
-- | match a longer string).
parseRegex ::
  forall err prec meta air nt a.
    Semiring meta =>
    Ord meta =>
    Eq air =>
    Ord prec =>
    Ord nt =>
    Tokenize meta (OrEOF String) air =>
  nt -> Comb (Rec err meta air nt (Similar String Rawr) String String) err prec meta air nt (Similar String Rawr) String a ->
  (String -> Either (ParseError err meta air nt (Similar String Rawr) String String) a)
parseRegex = parseWith { best: bestRegexOrString }

parseWith ::
  forall err prec meta air nt cat i o a.
    Ord prec =>
    Semiring meta =>
    Ord meta =>
    Eq air =>
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Tokenize meta (OrEOF i) air =>
    Eq o =>
  CConf err prec meta air nt cat i o -> nt -> Comb (Rec err meta air nt cat i o) err prec meta air nt cat o a ->
  (i -> Either (ParseError err meta air nt cat i o) a)
parseWith conf name parser = snd (parseWith' conf name parser)

parseWith' ::
  forall err prec meta air nt cat i o a.
    Ord prec =>
    Semiring meta =>
    Ord meta =>
    Eq air =>
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Tokenize meta (OrEOF i) air =>
    Eq o =>
  CConf err prec meta air nt cat i o -> nt -> Comb (Rec err meta air nt cat i o) err prec meta air nt cat o a ->
  StateTable meta nt cat /\ (i -> Either (ParseError err meta air nt cat i o) a)
parseWith' conf name parser = do
  -- First we build the LR(1) parsing table
  let compiled = compile name parser
  -- Then we can run it on an input
  compiled.states /\ execute conf compiled

parseRegex' ::
  forall err prec meta air nt a.
    Semiring meta =>
    Ord meta =>
    Eq air =>
    Ord prec =>
    Ord nt =>
    Tokenize meta (OrEOF String) air =>
  nt -> Comb (Rec err meta air nt (Similar String Rawr) String String) err prec meta air nt (Similar String Rawr) String a ->
  StateTable meta nt (Similar String Rawr) /\ (String -> Either (ParseError err meta air nt (Similar String Rawr) String String) a)
parseRegex' = parseWith' { best: bestRegexOrString }

-- | Compile the LR(1) state table for the grammar
compile ::
  forall rec err prec meta air nt cat o a.
    Semiring meta =>
    Ord meta =>
    Ord prec =>
    Ord nt =>
    Ord cat =>
  nt -> Comb rec err prec meta air nt cat o a ->
  { states :: StateTable meta nt cat
  , resultants :: nt /\ Array (CResultant rec err air nt o a)
  , options :: COptions rec err meta air nt cat o
  , precedences :: Map cat (prec /\ Associativity) /\ Map (nt /\ Int) prec
  }
compile name parser = do
  let Comb { grammar: MkGrammar initialWithPrec, entrypoints } = named name parser
  let initial = initialWithPrec <#> \r -> r { rName = snd r.rName }
  let grammar = MkGrammar (Array.nub initial)
  let stateMap /\ generated = statesNumberedMany grammar $ Array.nub $ [ name ] <> entrypoints
  let start = Map.lookup name stateMap # unsafeFromJust "Map.lookup name stateMap"
  { states: { stateMap, start, states: generated }
  , options: buildTree name parser
  , resultants: name /\ resultantsOf parser
  , precedences: gatherPrecedences parser
  }

gatherPrecedences ::
  forall rec err prec meta air nt cat o a.
    Ord nt =>
    Ord cat =>
  Comb rec err prec meta air nt cat o a ->
  Map cat (prec /\ Associativity) /\ Map (nt /\ Int) prec
gatherPrecedences (Comb { grammar: MkGrammar initialWithPrec, tokenPrecedence }) =
  gatherPrecedences' initialWithPrec tokenPrecedence

gatherPrecedences' ::
  forall prec meta nt cat.
    Ord nt =>
    Ord cat =>
  Array (CGrammarRule prec meta nt cat) ->
  Array (cat /\ (prec /\ Associativity)) ->
  Map cat (prec /\ Associativity) /\ Map (nt /\ Int) prec
gatherPrecedences' initialWithPrec tokenPrecedence = Tuple
  do Map.fromFoldable tokenPrecedence
  do
    Map.fromFoldable $ initialWithPrec # filterMap
      case _ of
        { pName, rName: Just prec /\ rName } ->
          Just ((pName /\ rName) /\ prec)
        _ -> Nothing

lookupTuple :: forall a b. Eq a => a -> a /\ b -> Maybe b
lookupTuple a1 (a2 /\ b) | a1 == a2 = Just b
lookupTuple _ _ = Nothing

resultantsOf :: forall rec err prec meta air nt cat o a. Comb rec err prec meta air nt cat o a -> Array (CResultant rec err air nt o a)
resultantsOf (Comb { rules: cases }) = cases <#> _.resultant

-- | Execute the parse.
execute ::
  forall err prec meta air nt cat i o a.
    Ord prec =>
    Semiring meta =>
    Ord meta =>
    Eq air =>
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Tokenize meta (OrEOF i) air =>
    Eq o =>
  CConf err prec meta air nt cat i o ->
  { states :: StateTable meta nt cat
  , resultants :: nt /\ Array (CResultant (Rec err meta air nt cat i o) err air nt o a)
  , options :: COptions (Rec err meta air nt cat i o) err meta air nt cat o
  , precedences :: Map cat (prec /\ Associativity) /\ Map (nt /\ Int) prec
  } ->
  (i -> Either (ParseError err meta air nt cat i o) a)
execute conf0 { states: { stateMap, start, states }, resultants, options, precedences } = do
  let
    xx = fullMapOptions
      { rec: identity
      , nt: pure
      , r: pure
      , cat: Continue
      , o: Continue
      , cst: note [] <<< revertICST'
      }
    options' ::
      nt ->
      Options (Rec err meta air nt cat i o) err meta air nt Int cat o ->
      Options (Rec err meta air nt cat i o) err meta air (Either nt nt) (Maybe Int) (OrEOF cat) (OrEOF o)
    options' name opts =
      let
        x = xx opts
      in Options
          [ { pName: Left name
            , rName: Nothing
            , rule: [NonTerminal (pure x /\ Right name), Terminal EOF]
            , logicParts: mempty
            , advanced: []
            }
          ]
    conf :: CConf err prec meta air nt cat i o
    conf = conf0 { best = \x -> ((<=<) conf0.best $ screenPrecedence $ combPrecedence precedences) x }
    rec = Rec \name input optionsHere result -> do
      state <- Map.lookup name stateMap?? CrashedStack "Could not find parser in table"
      stack <- contextLexingParse conf (state /\ states) (options' name optionsHere) rec (Continue input)
      cst <- getResultICM' stack?? CrashedStack "Failed to extract result"
      result cst? \userErr -> FailedStack
        { states
        , lookupState: Array.index (unwrap states)
        , initialInput: Continue input
        , failedStack: stack
        , failedState: unwrap states !! snd (topOf stack)
        , currentInput: EOF
        , failReason: UserRejection { userErr }
        }
  \(input :: i) -> do
    stack <- contextLexingParse conf (start /\ states) (options' (fst resultants) options) rec (Continue input)
    cst <- getResultICM' stack?? CrashedStack "Failed to extract result"
    -- Apply the function that parses from the CST to the desired result type
    uncurry (matchRule rec) resultants cst? \userErr -> FailedStack
      { states
      , lookupState: Array.index (unwrap states)
      , initialInput: Continue input
      , failedStack: stack
      , failedState: unwrap states !! snd (topOf stack)
      , currentInput: EOF
      , failReason: UserRejection { userErr }
      }


combPrecedence ::
  forall m prec meta cat nt.
    Monad m =>
    Ord prec =>
    Ord cat =>
    Ord nt =>
  Tuple (Map cat (Tuple prec Associativity)) (Map (nt /\ Int) prec) ->
  ResolvePrec m (OrEOF cat) (Fragment meta (Either nt nt) (OrEOF cat) /\ Either nt nt /\ Maybe Int)
combPrecedence precedences =
  happyPrecedenceOrd
    (notEOF >=> flip Map.lookup (fst precedences))
    case _ of
      Right nt /\ Just r -> Map.lookup (nt /\ r) (snd precedences)
      _ -> Nothing

-- Compile multiple parsers together into one LR(1) table with multiple
-- entrypoints.
type Compiled prec meta nt cat =
  -- Could almost be an existential type? But literally no reason to do that
  { entrypoints :: Map nt Int
  , states :: CStates meta nt cat
  , precedences :: Map cat (prec /\ Associativity) /\ Map (nt /\ Int) prec
  }
type Parsing err meta air nt cat i o a = i -> Either (ParseError err meta air nt cat i o) a

newtype Coll err prec meta air nt cat i o parsers = Coll
  { grammar :: CGrammar prec meta nt cat
  , prettyGrammar :: Array (Tuple nt (Maybe (CSyntax meta nt cat)))
  , entrypoints :: Array nt
  , tokenPrecedence :: Array (Tuple cat (Tuple prec Associativity))
  , compilation :: Compiled prec meta nt cat -> CConf err prec meta air nt cat i o -> parsers
  }

derive instance functorColl :: Functor (Coll err prec meta air nt cat i o)
instance applyColl :: Apply (Coll err prec meta air nt cat i o) where
  apply (Coll c1) (Coll c2) = Coll
    { grammar: c1.grammar <> c2.grammar
    , prettyGrammar: c1.prettyGrammar <|> c2.prettyGrammar
    , entrypoints: c1.entrypoints <|> c2.entrypoints
    , tokenPrecedence: c1.tokenPrecedence <|> c2.tokenPrecedence
    , compilation: \x y -> c1.compilation x y (c2.compilation x y)
    }
instance applicativeColl :: Applicative (Coll err prec meta air nt cat i o) where
  pure r = Coll
    { grammar: mempty
    , prettyGrammar: empty
    , entrypoints: mempty
    , tokenPrecedence: empty
    , compilation: \_ _ -> r
    }
-- a Monad instance is soooo tempting, but does not make sense:
-- the grammar needs to be fully built before compilation can be received
-- maybe there is a way to encode this in the types

coll ::
  forall err prec meta air nt cat i o a.
    Ord prec =>
    Semiring meta =>
    Ord meta =>
    Eq air =>
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Tokenize meta (OrEOF i) air =>
    Eq o =>
  nt -> Comb (Rec err meta air nt cat i o) err prec meta air nt cat o a -> Coll err prec meta air nt cat i o (Parsing err meta air nt cat i o a)
coll name parser@(Comb c) = Coll
  { grammar: c.grammar
  , prettyGrammar: c.prettyGrammar
  , entrypoints: pure name <> c.entrypoints
  , tokenPrecedence: c.tokenPrecedence
  , compilation: \{ entrypoints, states, precedences } ->
      case Map.lookup name entrypoints of
        Nothing -> \_ _ ->
          Left (CrashedStack "Internal error: Failed to find entrypoint for parser")
        Just state ->
          \conf -> execute conf
            { states: { stateMap: entrypoints, start: state, states }
            , resultants: name /\ resultantsOf parser
            , options: buildTree name parser
            , precedences
            }
  }

collect ::
  forall err prec meta air nt cat i o parsers.
    Semiring meta =>
    Ord meta =>
    Eq air =>
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Tokenize meta (OrEOF i) air =>
  CConf err prec meta air nt cat i o -> Coll err prec meta air nt cat i o parsers -> parsers
collect conf (Coll { grammar: MkGrammar initialWithPrec, entrypoints, compilation, tokenPrecedence }) = do
  let initial = initialWithPrec <#> \r -> r { rName = snd r.rName }
  let grammar = MkGrammar (Array.nub initial)
  let names = Array.nub entrypoints
  let entrypoints /\ states = statesNumberedMany grammar names
  compilation
    { entrypoints
    , states
    , precedences: gatherPrecedences' initialWithPrec tokenPrecedence
    } conf




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
  forall err prec meta air nt cat i o a b c.
    Ord nt =>
  nt ->
  Comb (Rec err meta air nt cat i o) err prec meta air nt cat o a ->
  Comb (Rec err meta air nt cat i o) err prec meta air nt cat o b ->
  ((i -> Either (ParseError err meta air nt cat i o) a) -> b -> c) ->
  Comb (Rec err meta air nt cat i o) err prec meta air nt cat o c
withReparserFor name aux (Comb cb) f = do
  let Comb ca = named name aux
  Comb
    { grammar: cb.grammar <> ca.grammar
    , entrypoints: pure name <|> ca.entrypoints <|> cb.entrypoints
    , pretty: cb.pretty
    , prettyGrammar: cb.prettyGrammar <|> ca.prettyGrammar
    -- TODO
    , tokenPrecedence: cb.tokenPrecedence <|> ca.tokenPrecedence
    , rules: cb.rules <#> \r@{ resultant: Resultant rr } -> r
      { resultant = Resultant rr
        { result = \(Rec rec) csts ->
            let
              parser input = rec name input (buildTree name aux) $
                matchRule (Rec rec) name (resultantsOf aux)
            in (\x -> f parser x) <$> rr.result (Rec rec) csts
        }
      }
    }
