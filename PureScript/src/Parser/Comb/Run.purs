module Parser.Comb.Run where

import Prelude

import Control.Plus ((<|>))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple, fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Parser.Algorithms (getResultCM', revertCST', statesNumberedByMany)
import Parser.Comb.Combinators (buildTree, named)
import Parser.Comb.Types (CGrammar, COptions, CResultant, CSyntax, Comb(..), Options(..), ParseError, Rec(..), Resultant(..), fullMapOptions, matchRule)
import Parser.Lexing (class Tokenize, Best, Rawr, Similar, bestRegexOrString, contextLexingParse, longest, (?))
import Parser.Types (Grammar(..), OrEOF(..), Part(..), State, StateInfo, States)

type CBest nt cat i o =
  Best Int (Either nt nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
type CConf nt cat i o =
  { best :: CBest nt cat i o
  }
type CStates nt cat =
  States Int (Either nt nt) (Maybe Int) (OrEOF cat)
type CStateInfo nt cat =
  StateInfo Int (Either nt nt) (Maybe Int) (OrEOF cat)
type CState nt cat =
  State (Either nt nt) (Maybe Int) (OrEOF cat)

type StateTable nt cat =
  { stateMap :: Map nt Int
  , start :: Int
  , states :: CStates nt cat
  }

-- | Parse an input. You should partially apply it, and reuse that same
-- | partially applied function for multiple inputs.
parse ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  nt -> Comb (Rec nt (OrEOF i) o) nt cat o a -> (i -> Either ParseError a)
parse = parseWith { best: longest }

-- | Parsing optimized for regexes and string literals, where the string
-- | literals take precedence over the regexes always (and not just when they
-- | match a longer string).
parseRegex ::
  forall nt a.
    Ord nt =>
  nt -> Comb (Rec nt (OrEOF String) String) nt (Similar String Rawr) String a -> (String -> Either ParseError a)
parseRegex = parseWith { best: bestRegexOrString }

parseWith ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  CConf nt cat i o -> nt -> Comb (Rec nt (OrEOF i) o) nt cat o a -> (i -> Either ParseError a)
parseWith conf name parser = snd (parseWith' conf name parser)

parseWith' ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  CConf nt cat i o -> nt -> Comb (Rec nt (OrEOF i) o) nt cat o a ->
  StateTable nt cat /\ (i -> Either ParseError a)
parseWith' conf name parser = do
  -- First we build the LR(1) parsing table
  let compiled = compile name parser
  -- Then we can run it on an input
  compiled.states /\ execute conf compiled

parseRegex' ::
  forall nt a.
    Ord nt =>
  nt -> Comb (Rec nt (OrEOF String) String) nt (Similar String Rawr) String a ->
  StateTable nt (Similar String Rawr) /\ (String -> Either ParseError a)
parseRegex' = parseWith' { best: bestRegexOrString }

-- | Compile the LR(1) state table for the grammar
compile ::
  forall rec nt cat o a.
    Ord nt =>
    Ord cat =>
  nt -> Comb rec nt cat o a ->
  { states :: StateTable nt cat
  , resultants :: nt /\ Array (CResultant rec nt o a)
  , options :: COptions rec nt cat o
  }
compile name parser = do
  let Comb { grammar: MkGrammar initial, entrypoints } = named name parser
  let grammar = MkGrammar (Array.nub initial)
  let stateAssoc /\ generated = statesNumberedByMany identity grammar $ Array.nub $ [ name ] <> entrypoints
  let stateMap = Map.fromFoldable stateAssoc
  let start = Map.lookup name stateMap # fromMaybe 0
  { states: { stateMap, start, states: generated }
  , options: buildTree name parser
  , resultants: name /\ resultantsOf parser
  }

lookupTuple :: forall a b. Eq a => a -> a /\ b -> Maybe b
lookupTuple a1 (a2 /\ b) | a1 == a2 = Just b
lookupTuple _ _ = Nothing

resultantsOf :: forall rec nt cat o a. Comb rec nt cat o a -> Array (CResultant rec nt o a)
resultantsOf (Comb { rules: cases }) = cases <#> _.resultant

-- | Execute the parse.
execute ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  { best :: Best Int (Either nt nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
  } ->
  { states :: StateTable nt cat
  , resultants :: nt /\ Array (CResultant (Rec nt (OrEOF i) o) nt o a)
  , options :: COptions (Rec nt (OrEOF i) o) nt cat o
  } ->
  (i -> Either ParseError a)
execute conf { states: { stateMap, start, states }, resultants, options } = do
  let
    options' ::
      nt -> Options (Rec nt (OrEOF i) o) (Either nt nt) (Maybe Int) (OrEOF cat) (OrEOF o)
    options' =
      let
        x = fullMapOptions
          { rec: identity
          , nt: pure
          , r: pure
          , cat: Continue
          , o: Continue
          , cst: revertCST'
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
    rec = Rec \name input -> do
      state <- "Could not find parser in table"? Map.lookup name stateMap
      stack <- lmap snd $ contextLexingParse conf (state /\ states) (options' name) rec input
      "Failed to extract result"? getResultCM' stack
  \(input :: i) -> do
    stack <- lmap snd $ contextLexingParse conf (start /\ states) (options' (fst resultants)) rec (Continue input)
    cst <- "Failed to extract result"? getResultCM' stack
    -- Apply the function that parses from the CST to the desired result type
    "Failed to match rule"? uncurry (matchRule rec) resultants cst


-- Compile multiple parsers together into one LR(1) table with multiple
-- entrypoints.
type Compiled nt cat =
  -- Could almost be an existential type? But literally no reason to do that
  { entrypoints :: Map nt Int
  , states :: States Int (Either nt nt) (Maybe Int) (OrEOF cat)
  }
type Parsing i a = i -> Either ParseError a

newtype Coll nt cat i o parsers = Coll
  { grammar :: CGrammar nt cat
  , prettyGrammar :: Array (Tuple nt (Maybe (CSyntax nt cat)))
  , entrypoints :: Array nt
  , compilation :: Compiled nt cat -> CConf nt cat i o -> parsers
  }

derive instance functorColl :: Functor (Coll nt cat i o)
instance applyColl :: Apply (Coll nt cat i o) where
  apply (Coll c1) (Coll c2) = Coll
    { grammar: c1.grammar <> c2.grammar
    , prettyGrammar: c1.prettyGrammar <> c2.prettyGrammar
    , entrypoints: c1.entrypoints <> c2.entrypoints
    , compilation: \x y -> c1.compilation x y (c2.compilation x y)
    }
instance applicativeColl :: Applicative (Coll nt cat i o) where
  pure r = Coll { grammar: mempty, prettyGrammar: mempty, entrypoints: mempty, compilation: \_ _ -> r }
-- a Monad instance is soooo tempting, but does not make sense:
-- the grammar needs to be fully built before compilation can be received
-- maybe there is a way to encode this in the types

coll ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
    Eq o =>
  nt -> Comb (Rec nt (OrEOF i) o) nt cat o a -> Coll nt cat i o (Parsing i a)
coll name parser@(Comb c) = Coll
  { grammar: c.grammar
  , prettyGrammar: c.prettyGrammar
  , entrypoints: pure name <> c.entrypoints
  , compilation: \{ entrypoints, states } ->
      case Map.lookup name entrypoints of
        Nothing -> \_ _ ->
          Left "Internal error: Failed to find entrypoint for parser"
        Just state ->
          \conf -> execute conf
            { states: { stateMap: entrypoints, start: state, states }
            , resultants: name /\ resultantsOf parser
            , options: buildTree name parser
            }
  }

collect ::
  forall nt cat i o parsers.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
  CConf nt cat i o -> Coll nt cat i o parsers -> parsers
collect conf (Coll { grammar: MkGrammar initial, entrypoints, compilation }) = do
  let grammar = MkGrammar (Array.nub initial)
  let names = Array.nub entrypoints
  let states = statesNumberedByMany identity grammar names
  compilation
    { entrypoints: Map.fromFoldable (fst states)
    , states: snd states
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
  forall nt cat i o a b c.
    Ord nt =>
  nt ->
  Comb (Rec nt (OrEOF i) o) nt cat o a ->
  Comb (Rec nt (OrEOF i) o) nt cat o b ->
  ((i -> Either ParseError a) -> b -> c) ->
  Comb (Rec nt (OrEOF i) o) nt cat o c
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
              parser input =
                rec name (Continue input) >>=
                  matchRule (Rec rec) name (resultantsOf aux) >>>
                    note "Error in reparser"
            in (\x -> f parser x) <$> rr.result (Rec rec) csts
        }
      }
    }
