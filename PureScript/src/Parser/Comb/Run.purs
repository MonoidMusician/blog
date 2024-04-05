module Parser.Comb.Run where

import Prelude

import Control.Plus ((<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Filterable (filterMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Parser.Algorithms (getResultCM', revertCST', statesNumberedByMany)
import Parser.Comb.Combinators (buildTree, named)
import Parser.Comb.Types (CCST, CGrammar, COptions, CResultant, CSyntax, Comb(..), Options(..), Resultant(..), CGrammarRule, fullMapOptions, matchRule)
import Parser.Lexing (class Tokenize, Best, FailReason(..), FailedStack(..), Rawr, Similar, bestRegexOrString, contextLexingParse, longest, (?), (??))
import Parser.Proto (topOf)
import Parser.Types (Grammar(..), OrEOF(..), Part(..), State, StateInfo, States, Fragment)
import Unsafe.Coerce (unsafeCoerce)

type CBest :: forall k. Type -> k -> Type -> Type -> Type -> Type -> Type
type CBest err prec nt cat i o =
  Best err Int (Fragment (Either nt nt) (OrEOF cat) /\ Either nt nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
type CConf :: forall k. Type -> k -> Type -> Type -> Type -> Type -> Type
type CConf err prec nt cat i o =
  { best :: CBest err prec nt cat i o
  }
type CStates nt cat =
  States Int (Either nt nt) (Maybe Int) (OrEOF cat)
type CStateInfo nt cat =
  StateInfo Int (Either nt nt) (Maybe Int) (OrEOF cat)
type CState nt cat =
  State (Either nt nt) (Maybe Int) (OrEOF cat)

type ParseError err nt cat i o =
  FailedStack err Int (Rec err nt cat i o) (Either nt nt) (Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
newtype Rec err nt cat i o = Rec
  ( forall a.
      nt -> i ->
      (CCST nt o -> Either (Array err) a) ->
      Either (ParseError err nt cat i o) a
  )

type StateTable nt cat =
  { stateMap :: Map nt Int
  , start :: Int
  , states :: CStates nt cat
  }

-- | Parse an input. You should partially apply it, and reuse that same
-- | partially applied function for multiple inputs.
parse ::
  forall err prec nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  nt -> Comb (Rec err nt cat i o) err prec nt cat o a ->
  (i -> Either (ParseError err nt cat i o) a)
parse = parseWith { best: longest }

-- | Parsing optimized for regexes and string literals, where the string
-- | literals take precedence over the regexes always (and not just when they
-- | match a longer string).
parseRegex ::
  forall err prec nt a.
    Ord nt =>
  nt -> Comb (Rec err nt (Similar String Rawr) String String) err prec nt (Similar String Rawr) String a ->
  (String -> Either (ParseError err nt (Similar String Rawr) String String) a)
parseRegex = parseWith { best: bestRegexOrString }

parseWith ::
  forall err prec nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  CConf err prec nt cat i o -> nt -> Comb (Rec err nt cat i o) err prec nt cat o a ->
  (i -> Either (ParseError err nt cat i o) a)
parseWith conf name parser = snd (parseWith' conf name parser)

parseWith' ::
  forall err prec nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  CConf err prec nt cat i o -> nt -> Comb (Rec err nt cat i o) err prec nt cat o a ->
  StateTable nt cat /\ (i -> Either (ParseError err nt cat i o) a)
parseWith' conf name parser = do
  -- First we build the LR(1) parsing table
  let compiled = compile name parser
  -- Then we can run it on an input
  compiled.states /\ execute conf compiled

parseRegex' ::
  forall err prec nt a.
    Ord nt =>
  nt -> Comb (Rec err nt (Similar String Rawr) String String) err prec nt (Similar String Rawr) String a ->
  StateTable nt (Similar String Rawr) /\ (String -> Either (ParseError err nt (Similar String Rawr) String String) a)
parseRegex' = parseWith' { best: bestRegexOrString }

-- | Compile the LR(1) state table for the grammar
compile ::
  forall rec err prec nt cat o a.
    Ord nt =>
    Ord cat =>
  nt -> Comb rec err prec nt cat o a ->
  { states :: StateTable nt cat
  , resultants :: nt /\ Array (CResultant rec err nt o a)
  , options :: COptions rec err nt cat o
  , precedences :: Map (nt /\ Int) prec
  }
compile name parser = do
  let Comb { grammar: MkGrammar initialWithPrec, entrypoints } = named name parser
  let initial = initialWithPrec <#> \r -> r { rName = snd r.rName }
  let grammar = MkGrammar (Array.nub initial)
  let stateAssoc /\ generated = statesNumberedByMany identity grammar $ Array.nub $ [ name ] <> entrypoints
  let stateMap = Map.fromFoldable stateAssoc
  let start = Map.lookup name stateMap # fromMaybe 0
  { states: { stateMap, start, states: generated }
  , options: buildTree name parser
  , resultants: name /\ resultantsOf parser
  , precedences: gatherPrecedences parser
  }

gatherPrecedences ::
  forall rec err prec nt cat o a.
    Ord nt =>
  Comb rec err prec nt cat o a ->
  Map (nt /\ Int) prec
gatherPrecedences (Comb { grammar: MkGrammar initialWithPrec }) =
  gatherPrecedences' initialWithPrec

gatherPrecedences' ::
  forall prec nt cat.
    Ord nt =>
  Array (CGrammarRule prec nt cat) ->
  Map (nt /\ Int) prec
gatherPrecedences' initialWithPrec =
  Map.fromFoldable $ initialWithPrec # filterMap
    case _ of
      { pName, rName: Just prec /\ rName } ->
        Just ((pName /\ rName) /\ prec)
      _ -> Nothing

lookupTuple :: forall a b. Eq a => a -> a /\ b -> Maybe b
lookupTuple a1 (a2 /\ b) | a1 == a2 = Just b
lookupTuple _ _ = Nothing

resultantsOf :: forall rec err prec nt cat o a. Comb rec err prec nt cat o a -> Array (CResultant rec err nt o a)
resultantsOf (Comb { rules: cases }) = cases <#> _.resultant

-- | Execute the parse.
execute ::
  forall err prec nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  CConf err prec nt cat i o ->
  { states :: StateTable nt cat
  , resultants :: nt /\ Array (CResultant (Rec err nt cat i o) err nt o a)
  , options :: COptions (Rec err nt cat i o) err nt cat o
  , precedences :: Map (nt /\ Int) prec
  } ->
  (i -> Either (ParseError err nt cat i o) a)
execute conf { states: { stateMap, start, states }, resultants, options } = do
  let
    options' ::
      nt -> Options (Rec err nt cat i o) err (Either nt nt) (Maybe Int) (OrEOF cat) (OrEOF o)
    options' =
      let
        x = fullMapOptions
          { rec: identity
          , nt: pure
          , r: pure
          , cat: Continue
          , o: Continue
          , cst: note [unsafeCoerce "revertCST'"] <<< revertCST'
          } options
      in \name -> Options
          [ { pName: Left name
            , rName: Nothing
            -- FIXME: pure x is wrong here
            , rule: [NonTerminal (pure x /\ Right name), Terminal EOF]
            , logicParts: mempty
            , advanced: []
            }
          ]
    rec = Rec \name input result -> do
      state <- Map.lookup name stateMap?? CrashedStack "Could not find parser in table"
      stack <- contextLexingParse conf (state /\ states) (options' name) rec (Continue input)
      cst <- getResultCM' stack?? CrashedStack "Failed to extract result"
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
    stack <- contextLexingParse conf (start /\ states) (options' (fst resultants)) rec (Continue input)
    cst <- getResultCM' stack?? CrashedStack "Failed to extract result"
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


-- Compile multiple parsers together into one LR(1) table with multiple
-- entrypoints.
type Compiled prec nt cat =
  -- Could almost be an existential type? But literally no reason to do that
  { entrypoints :: Map nt Int
  , states :: States Int (Either nt nt) (Maybe Int) (OrEOF cat)
  , precedences :: Map (nt /\ Int) prec
  }
type Parsing err nt cat i o a = i -> Either (ParseError err nt cat i o) a

newtype Coll err prec nt cat i o parsers = Coll
  { grammar :: CGrammar prec nt cat
  , prettyGrammar :: Array (Tuple nt (Maybe (CSyntax nt cat)))
  , entrypoints :: Array nt
  , compilation :: Compiled prec nt cat -> CConf err prec nt cat i o -> parsers
  }

derive instance functorColl :: Functor (Coll err prec nt cat i o)
instance applyColl :: Apply (Coll err prec nt cat i o) where
  apply (Coll c1) (Coll c2) = Coll
    { grammar: c1.grammar <> c2.grammar
    , prettyGrammar: c1.prettyGrammar <> c2.prettyGrammar
    , entrypoints: c1.entrypoints <> c2.entrypoints
    , compilation: \x y -> c1.compilation x y (c2.compilation x y)
    }
instance applicativeColl :: Applicative (Coll err prec nt cat i o) where
  pure r = Coll { grammar: mempty, prettyGrammar: mempty, entrypoints: mempty, compilation: \_ _ -> r }
-- a Monad instance is soooo tempting, but does not make sense:
-- the grammar needs to be fully built before compilation can be received
-- maybe there is a way to encode this in the types

coll ::
  forall err prec nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  nt -> Comb (Rec err nt cat i o) err prec nt cat o a -> Coll err prec nt cat i o (Parsing err nt cat i o a)
coll name parser@(Comb c) = Coll
  { grammar: c.grammar
  , prettyGrammar: c.prettyGrammar
  , entrypoints: pure name <> c.entrypoints
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
  forall err prec nt cat i o parsers.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
  CConf err prec nt cat i o -> Coll err prec nt cat i o parsers -> parsers
collect conf (Coll { grammar: MkGrammar initialWithPrec, entrypoints, compilation }) = do
  let initial = initialWithPrec <#> \r -> r { rName = snd r.rName }
  let grammar = MkGrammar (Array.nub initial)
  let names = Array.nub entrypoints
  let states = statesNumberedByMany identity grammar names
  compilation
    { entrypoints: Map.fromFoldable (fst states)
    , states: snd states
    , precedences: gatherPrecedences' initialWithPrec
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
  forall err prec nt cat i o a b c.
    Ord nt =>
  nt ->
  Comb (Rec err nt cat i o) err prec nt cat o a ->
  Comb (Rec err nt cat i o) err prec nt cat o b ->
  ((i -> Either (ParseError err nt cat i o) a) -> b -> c) ->
  Comb (Rec err nt cat i o) err prec nt cat o c
withReparserFor name aux (Comb cb) f = do
  let Comb ca = named name aux
  Comb
    { grammar: cb.grammar <> ca.grammar
    , entrypoints: pure name <|> ca.entrypoints <|> cb.entrypoints
    , pretty: cb.pretty
    , prettyGrammar: cb.prettyGrammar <|> ca.prettyGrammar
    , rules: cb.rules <#> \r@{ resultant: Resultant rr } -> r
      { resultant = Resultant rr
        { result = \(Rec rec) csts ->
            let
              parser input = rec name input $
                matchRule (Rec rec) name (resultantsOf aux)
            in (\x -> f parser x) <$> rr.result (Rec rec) csts
        }
      }
    }
