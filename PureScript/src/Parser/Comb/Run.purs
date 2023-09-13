module Parser.Comb.Run where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Parser.Algorithms (getResultCM', statesNumberedByMany)
import Parser.Comb.Combinators (named)
import Parser.Comb.Types (CGrammar, CResultant, Comb(..), ParseError, Rec(..), matchRule, withRec)
import Parser.Lexing (class Tokenize, Best, Rawr, Similar, bestRegexOrString, lexingParse, longest, (?))
import Parser.Types (Grammar(..), OrEOF(..), States)

type CBest nt cat i o =
  Best Int (Either nt nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
type CConf nt cat i o =
  { best :: CBest nt cat i o
  }
type CStates nt cat =
  States Int (Either nt nt) (Maybe Int) (OrEOF cat)

-- | Parse an input. You should partially apply it, and reuse that same
-- | partially applied function for multiple inputs.
parse ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
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
  CConf nt cat i o -> nt -> Comb (Rec nt (OrEOF i) o) nt cat o a -> (i -> Either ParseError a)
parseWith conf name parser = snd (parseWith' conf name parser)

parseWith' ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
  CConf nt cat i o -> nt -> Comb (Rec nt (OrEOF i) o) nt cat o a ->
  (Map nt Int /\ Int /\ CStates nt cat) /\ (i -> Either ParseError a)
parseWith' conf name parser = do
  -- First we build the LR(1) parsing table
  let { states, resultants } = compile name parser
  -- Then we can run it on an input
  states /\ execute conf { states, resultants }

parseRegex' ::
  forall nt a.
    Ord nt =>
  nt -> Comb (Rec nt (OrEOF String) String) nt (Similar String Rawr) String a ->
  (Map nt Int /\ Int /\ CStates nt (Similar String Rawr)) /\ (String -> Either ParseError a)
parseRegex' = parseWith' { best: bestRegexOrString }

-- | Compile the LR(1) state table for the grammar
compile ::
  forall rec nt cat o a.
    Ord nt =>
    Ord cat =>
  nt -> Comb rec nt cat o a ->
  { states :: Map nt Int /\ Int /\ CStates nt cat
  , resultants :: nt /\ Array (CResultant rec nt o a)
  }
compile name parser = do
  let Comb { grammar: MkGrammar initial } = named name parser
  let grammar = MkGrammar (Array.nub initial)
  let stateAssoc /\ generated = statesNumberedByMany identity grammar [ name ]
  let stateMap = Map.fromFoldable stateAssoc
  let tgt = Map.lookup name stateMap # fromMaybe 0
  { states: stateMap /\ tgt /\ generated
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
  { best :: Best Int (Either nt nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
  } ->
  { states :: Map nt Int /\ Int /\ States Int (Either nt nt) (Maybe Int) (OrEOF cat)
  , resultants :: nt /\ Array (CResultant (Rec nt (OrEOF i) o) nt o a)
  } ->
  (i -> Either ParseError a)
execute conf { states: stateMap /\ tgt /\ states, resultants } = do
  let
    rec = Rec \name input -> do
      state <- "Could not find parser in table"? Map.lookup name stateMap
      stack <- lmap snd $ lexingParse conf (state /\ states) input
      "Failed to extract result"? getResultCM' stack
  \(input :: i) -> do
    stack <- lmap snd $ lexingParse conf (tgt /\ states) (Continue input)
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
  , entrypoints :: Array nt
  , compilation :: Compiled nt cat -> CConf nt cat i o -> parsers
  }

derive instance functorColl :: Functor (Coll nt cat i o)
instance applyColl :: Apply (Coll nt cat i o) where
  apply (Coll c1) (Coll c2) = Coll
    { grammar: c1.grammar <> c2.grammar
    , entrypoints: c1.entrypoints <> c2.entrypoints
    , compilation: \x y -> c1.compilation x y (c2.compilation x y)
    }
instance applicativeColl :: Applicative (Coll nt cat i o) where
  pure r = Coll { grammar: mempty, entrypoints: mempty, compilation: \_ _ -> r }
-- a Monad instance is soooo tempting, but does not make sense:
-- the grammar needs to be fully built before compilation can be received
-- maybe there is a way to encode this in the types

coll ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
  nt -> Comb (Rec nt (OrEOF i) o) nt cat o a -> Coll nt cat i o (Parsing i a)
coll name parser@(Comb c) = Coll
  { grammar: c.grammar
  , entrypoints: pure name
  , compilation: \{ entrypoints, states } ->
      case Map.lookup name entrypoints of
        Nothing -> \_ _ ->
          Left "Internal error: Failed to find entrypoint for parser"
        Just state ->
          \conf -> execute conf
            { states: entrypoints /\ state /\ states
            , resultants: name /\ resultantsOf parser
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




withReparser ::
  forall nt cat i o a b c.
    Ord nt =>
  nt ->
  Comb (Rec nt (OrEOF i) o) nt cat o a ->
  Comb (Rec nt (OrEOF i) o) nt cat o b ->
  ((i -> Either ParseError a) -> b -> c) ->
  Comb (Rec nt (OrEOF i) o) nt cat o c
withReparser name aux cb f = do
  let ca = named name aux
  ca *> flip withRec cb \(Rec rec) ->
    f \input ->
      rec name (Continue input) >>=
        matchRule (Rec rec) name (resultantsOf aux) >>>
          note "Error in reparser"
