module Parser.Comb.Run where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Parser.Algorithms (getResultCM, statesNumberedBy)
import Parser.Comb.Combinators (named)
import Parser.Comb.Types (Comb(..), CResultant, matchRule)
import Parser.Lexing (class Tokenize, Best, Rawr, Similar, bestRegexOrString, lexingParse, longest, (?))
import Parser.Types (Grammar(..), OrEOF(..), States)

-- | Parse an input. You should partially apply it, and reuse that same
-- | partially applied function for multiple inputs.
parse ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
  nt -> Comb nt cat o a -> (i -> Either String a)
parse = parseWith { best: longest }

-- | Parsing optimized for regexes and string literals, where the string
-- | literals take precedence over the regexes always (and not just when they
-- | match a longer string).
parseRegex ::
  forall nt a.
    Ord nt =>
  nt -> Comb nt (Similar String Rawr) String a -> (String -> Either String a)
parseRegex = parseWith { best: bestRegexOrString }

parseWith ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
  { best :: Best Int (Maybe nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
  } -> nt -> Comb nt cat o a -> (i -> Either String a)
parseWith conf name parser = snd (parseWith' conf name parser)

parseWith' ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
  { best :: Best Int (Maybe nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
  } -> nt -> Comb nt cat o a ->
  (Int /\ States Int (Maybe nt) (Maybe Int) (OrEOF cat)) /\ (i -> Either String a)
parseWith' conf name parser = do
  -- First we build the LR(1) parsing table
  let { states, resultants } = compile name parser
  -- Then we can run it on an input
  states /\ execute conf { states, resultants }

parseRegex' ::
  forall nt a.
    Ord nt =>
  nt -> Comb nt (Similar String Rawr) String a ->
  (Int /\ States Int (Maybe nt) (Maybe Int) (OrEOF (Similar String Rawr))) /\ (String -> Either String a)
parseRegex' = parseWith' { best: bestRegexOrString }

-- | Compile the LR(1) state table for the grammar
compile ::
  forall nt cat o a.
    Ord nt =>
    Ord cat =>
  nt -> Comb nt cat o a ->
  { states :: Int /\ States Int (Maybe nt) (Maybe Int) (OrEOF cat)
  , resultants :: nt /\ Array (CResultant nt o a)
  }
compile name parser = do
  let Comb { grammar: MkGrammar initial } = named name parser
  let grammar = MkGrammar (Array.nub initial)
  { states: statesNumberedBy identity grammar name
  , resultants: name /\ resultantsOf parser
  }

resultantsOf :: forall nt cat o a. Comb nt cat o a -> Array (CResultant nt o a)
resultantsOf (Comb { rules: cases }) = cases <#> _.resultant

-- | Execute the parse.
execute ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Monoid i =>
    Tokenize cat i o =>
  { best :: Best Int (Maybe nt /\ Maybe Int) (OrEOF cat) (OrEOF i) (OrEOF o)
  } ->
  { states :: Int /\ States Int (Maybe nt) (Maybe Int) (OrEOF cat)
  , resultants :: nt /\ Array (CResultant nt o a)
  } ->
  (i -> Either String a)
execute conf { states, resultants } =
  let compiled = lexingParse conf states in
  \(input :: i) -> do
      stack <- lmap snd $ compiled (Continue input)
      cst <- "Failed to extract result"? getResultCM stack
      -- Apply the function that parses from the CST to the desired result type
      "Failed to match rule"? uncurry matchRule resultants cst

