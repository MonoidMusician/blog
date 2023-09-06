module Parser.Comb where

import Prelude

import Control.Alternative (class Alternative)
import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Array (head, length, splitAt, zipWith, (!!))
import Data.Compactable (class Compactable, compact, separateDefault)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Parser.Algorithms (addEOF'', getResultCM, indexStates, statesNumberedBy, toTable)
import Parser.Proto as Proto
import Parser.Types (CST(..), Fragment, Grammar(..), Part(..))

type Rule nt = Tuple nt Int
type CGrammar nt tok = Grammar nt (Rule nt) tok
type CFragment nt tok = Fragment nt tok
type CCST nt tok = CST (Rule nt) tok

-- | An applicative parser combinator that builds a grammar and parses a CST.
-- |
-- | `grammar` is a list of all the named rules generated by the syntax so far.
-- | `rules` is a list of all the alternative rules that this syntax will
-- | recognize, along with the function to parse the corresponding CST into
-- | the result type (see `matchRule`).
-- |
-- | The action of `apply` is to take the cartesian product of rules and
-- | concatenate each alternative together; the action of `alt` is to simply
-- | append the lists of rules.
-- |
-- | This means that alternatives are statically flattened, unless they are
-- | named.
-- |
-- | Invariant:
-- | - `rules # all \r -> length r.rule == r.resultant.length`
newtype Comb nt tok a = Comb
  { grammar :: CGrammar nt tok
  , rules :: Array
    { rule :: CFragment nt tok
    , resultant :: CResultant nt tok a
    }
  }

-- | Parses an array (of CSTs), expecting a certain number of elements.
-- | It may still return failure early, though.
-- |
-- | Invariants:
-- | - `length i < r.length => (r.result `elem` [Partial, Failed])`
-- | - `length i == r.length => r.result /= Partial`
-- | - `r.result i == r.result (take r.length i)`
newtype Resultant i a = Resultant
  { length :: Int
  , result :: Array i -> PartialResult a
  }
type CResultant nt tok = Resultant (CCST nt tok)

data PartialResult a
  = Failed
  | Partial
  | Result a


-- | Parse an input. You should partially apply it, and reuse that same
-- | partially applied function for multiple inputs.
parse :: forall nt tok a. Ord nt => Ord tok => nt -> Comb nt tok a -> (Array tok -> Maybe a)
parse name parser = do
  -- First we build the LR(1) parsing table
  let Comb { grammar, rules } = named name parser
  let Tuple initialState states = statesNumberedBy identity grammar name
  let table = toTable states Just
  -- Then we run it on an input
  addEOF'' >>> \input -> do
    stack <- Proto.parse table initialState input
    cst <- getResultCM stack
    -- Apply the function that parses from the CST to the desired result type
    matchRule (rules <#> _.resultant) cst


derive instance functorComb :: Functor (Comb nt tok)
instance applyComb :: Apply (Comb nt tok) where
  apply (Comb l) (Comb r) = Comb
    { grammar: l.grammar <> r.grammar
    , rules: ado
        x <- l.rules
        y <- r.rules
        in
          { rule: x.rule <|> y.rule
          , resultant: x.resultant <*> y.resultant
          }
    }
instance applicativeComb :: Applicative (Comb nt tok) where
  pure a = Comb { grammar: mempty, rules: pure { rule: empty, resultant: pure a } }
instance altComb :: Alt (Comb nt tok) where
  alt (Comb l) (Comb r) = Comb
    { grammar: l.grammar <> r.grammar
    , rules: l.rules <|> r.rules
    }
instance plusComb :: Plus (Comb nt tok) where
  empty = Comb { grammar: mempty, rules: empty }
-- | Distributivity follows from distributivity of `Array`
instance alternativeComb :: Alternative (Comb nt tok)
instance compactableComb :: Compactable (Comb nt tok) where
  compact (Comb c) = Comb c { rules = c.rules <#> \r -> r { resultant = compact r.resultant } }
  separate eta = separateDefault eta

token :: forall nt tok. tok -> Comb nt tok tok
token tok = Comb
  { grammar: mempty
  , rules: pure
    { rule: pure (Terminal tok)
    , resultant: component case _ of
        Leaf t -> Just t
        _ -> Nothing
    }
  }
tokens :: forall nt tok. Array tok -> Comb nt tok (Array tok)
tokens toks = Comb
  { grammar: mempty
  , rules: pure
    { rule: Terminal <$> toks
    , resultant: components $ toks $> case _ of
        Leaf t -> Just t
        _ -> Nothing
    }
  }

-- | Name a nonterminal production, this allows recursion.
namedRec :: forall nt tok a. nt -> (Comb nt tok a -> Comb nt tok a) -> Comb nt tok a
namedRec name rec =
  let
    recursive = rec $ Comb
      { grammar: mempty
      , rules: pure
        { rule: pure (NonTerminal name)
        , resultant: component \cst ->
            let Comb borrowed = recursive in
            matchRule (borrowed.rules <#> _.resultant) cst
        }
      }
    Comb produced = recursive
    newRules = MkGrammar $ produced.rules # mapWithIndex \i { rule } ->
      { pName: name
      , rName: Tuple name i
      , rule
      }
  in Comb
    { grammar: produced.grammar <> newRules
    , rules: pure
        { rule: pure (NonTerminal name)
        , resultant: component $ matchRule (produced.rules <#> _.resultant)
        }
    }

named :: forall nt tok a. nt -> Comb nt tok a -> Comb nt tok a
named name = namedRec name <<< const

matchRule :: forall nt tok a. Array (CResultant nt tok a) -> CCST nt tok -> Maybe a
matchRule resultants (Branch (Tuple _name i) children) = do
  resultant <- resultants !! i
  resultFrom resultant children
matchRule _ _ = Nothing

derive instance functorResultant :: Functor (Resultant i)
instance applyResultant :: Apply (Resultant i) where
  apply (Resultant l) (Resultant r) = Resultant
    { length: l.length + r.length
    , result: \i ->
        let { before: li, after: ri } = splitAt l.length i in
        l.result li <*> r.result ri
    }
instance applicativeResultant :: Applicative (Resultant i) where
  pure a = Resultant { length: 0, result: const (pure a) }
instance compactableResultant :: Compactable (Resultant i) where
  compact (Resultant r) = Resultant r { result = map compact r.result }
  separate eta = separateDefault eta

-- | Apply the resultant.
resultFrom :: forall i a. Resultant i a -> Array i -> Maybe a
resultFrom (Resultant r) i = case r.result i of
  Result a -> Just a
  _ -> Nothing

component :: forall i a. (i -> Maybe a) -> Resultant i a
component c = Resultant
  { length: 1
  , result: \is ->
      case head is of
        Nothing -> Partial
        Just i -> compact (Result (c i))
  }
-- | components = traverse component
components :: forall i a. Array (i -> Maybe a) -> Resultant i (Array a)
components cs = Resultant
  { length: length cs
  , result: \is ->
      if length is < length cs then Partial else
        compact (Result (sequence (zipWith identity cs is)))
  }

derive instance functorPartialResult :: Functor PartialResult
instance applyPartialResult :: Apply PartialResult where
  apply Failed _ = Failed
  apply _ Failed = Failed
  apply Partial _ = Partial
  apply _ Partial = Partial
  apply (Result f) (Result a) = Result (f a)
instance applicativePartialResult :: Applicative PartialResult where
  pure = Result
instance compactablePartialResult :: Compactable PartialResult where
  compact (Result Nothing) = Failed
  compact (Result (Just a)) = Result a
  compact Failed = Failed
  compact Partial = Partial
  separate eta = separateDefault eta
-- instance altResult :: Alt PartialResult where
--   alt Failed r = r
--   alt l Failed = l
--   alt Partial _ = Partial
--   alt l@(Result _) _ = l
