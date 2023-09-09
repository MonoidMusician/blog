module Parser.Comb where

import Prelude

import Control.Alternative (class Alternative)
import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Array (fromFoldable, head, length, splitAt, toUnfoldable, zipWith, (!!))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Compactable (class Compactable, compact, separateDefault)
import Data.Either (Either(..), either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.Traversable (foldl, intercalate, sequence)
import Data.Tuple (Tuple(..), snd)
import Effect.Console (error, log)
import Effect.Unsafe (unsafePerformEffect)
import Parser.Algorithms (getResultCM, statesNumberedBy)
import Parser.Lexing (class Token, guessBest, lexingParse, (?))
import Parser.Proto (Stack)
import Parser.Types (CST(..), Fragment, Grammar(..), OrEOF(..), Part(..))
import Unsafe.Reference (unsafeRefEq)

data Syntax nt tok
  = Conj (Syntax nt tok) (Syntax nt tok)
  | Disj (Syntax nt tok) (Syntax nt tok)
  | Part (Part nt tok)
  | Null

derive instance eqSyntax :: (Eq nt, Eq tok) => Eq (Syntax nt tok)
derive instance ordSyntax :: (Ord nt, Ord tok) => Ord (Syntax nt tok)

printSyntax' :: forall nt tok. (Fragment nt tok -> String) -> Syntax nt tok -> Array (Either String (Fragment nt tok))
printSyntax' f (Conj l r) =
  let
    p x = case x of
      Null -> empty
      Disj _ _ -> pure $ Left $ "(" <> printSyntax f x <> ")"
      _ -> printSyntax' f x
  in join [ p l, p r ]
printSyntax' f (Disj l r) = [ Left $ printSyntax f l <> " | " <> printSyntax f r ]
printSyntax' _ (Part p) = [ Right [ p ] ]
printSyntax' _ Null = mempty

printSyntax :: forall nt tok. (Fragment nt tok -> String) -> Syntax nt tok -> String
printSyntax f syntax =
  intercalate " " $ map (either identity f) $ coalesce $ printSyntax' f syntax

coalesce :: forall x m. Monoid m => Array (Either x m) -> Array (Either x m)
coalesce = toUnfoldable >>> coalesce' >>> fromFoldable

coalesce' :: forall x m. Monoid m => List (Either x m) -> List (Either x m)
coalesce' (Right x : Right y : zs) = coalesce' (Right (x <> y) : zs)
coalesce' (z : zs) = z : coalesce' zs
coalesce' Nil = Nil

type CSyntax = Syntax
type CGrammar nt cat = Grammar nt Int cat
type CFragment nt cat = Fragment nt cat
type CCST nt o = CST (Tuple nt Int) o

-- | An applicative parser combinator that builds a grammar and parses a CST.
-- |
-- | It operates in two parts:
-- | - It builds a rule as a flat list of alternatives, and adds these rules
-- |   to the grammar when they are named (which enables recursion).
-- | - It builds a codec the parses a CST to obtain the arbitrary data out of
-- |   it that parser combinators are known for.
-- |
-- | Parsing then proceeds in two phases:
-- | - First the LR(1) parsing table is built from the grammar. This is cached.
-- | - For a given parse, following the table builds a CST, and then the codec
-- |   is used to parse the ultimate result out of the CST, based on which
-- |   rules were matched and the value of the tokens.
-- |
-- | `grammar` is a list of all the named rules generated by the syntax so far.
-- | `rules` is a list of all the alternative rules that this piece of syntax
-- | will recognize, along with the function to parse the corresponding CST into
-- | the result type (see `matchRule`).
-- |
-- | The action of `apply` is to take the cartesian product of rules and
-- | concatenate each alternative together; the action of `alt` is to simply
-- | append the lists of rules.
-- |
-- | This means that alternatives are statically flattened, unless they are
-- | named.
-- |
-- | `pretty` and `prettyGrammar` serve to keep track of the original tree
-- | structure, instead of the flattened rules.
-- |
-- | Invariant:
-- | - `rules # all \{ rule, resultant } -> length rule == resultant.length`
newtype Comb nt cat o a = Comb
  { grammar :: CGrammar nt cat
  , pretty :: Maybe (CSyntax nt cat)
  , prettyGrammar :: Array (Tuple nt (Maybe (CSyntax nt cat)))
  , rules :: Array
    { rule :: CFragment nt cat
    , resultant :: CResultant nt o a
    }
  }
type Combs = Comb String CodePoint CodePoint

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
type CResultant nt o = Resultant (CCST nt o)

data PartialResult a
  = Failed
  | Partial
  | Result a


-- | Parse an input. You should partially apply it, and reuse that same
-- | partially applied function for multiple inputs.
parse ::
  forall nt cat i o a.
    Ord nt =>
    Ord cat =>
    Token cat i o =>
    nt -> Comb nt cat o a -> (i -> Either String a)
parse name parser = do
  -- First we build the LR(1) parsing table
  -- This is a bit ugly right now ...
  let Comb { rules: cases } = parser
  let Comb { grammar: MkGrammar initial } = named name parser
  let grammar = MkGrammar (Array.nub initial)
  let states = statesNumberedBy identity grammar name
  let compiled = lexingParse { best: guessBest } states :: OrEOF i -> Either (Tuple _ String) (Stack Int (CST (Tuple (Maybe nt) (Maybe Int)) (OrEOF o)))
  -- Then we run it on an input
  \(input :: i) -> do
    stack <- lmap snd $ compiled (Continue input)
    cst <- "Failed to extract result"? getResultCM stack
    -- Apply the function that parses from the CST to the desired result type
    "Failed to match rule"? matchRule name (cases <#> _.resultant) cst

derive instance functorComb :: Functor (Comb nt cat o)
instance applyComb :: Apply (Comb nt cat o) where
  apply (Comb l) (Comb r) = Comb
    { grammar: l.grammar <> r.grammar
    , pretty: Conj <$> l.pretty <*> r.pretty
    , prettyGrammar: l.prettyGrammar <|> r.prettyGrammar
    , rules: ado
        x <- l.rules
        y <- r.rules
        in
          { rule: x.rule <|> y.rule
          , resultant: x.resultant <*> y.resultant
          }
    }
instance applicativeComb :: Applicative (Comb nt cat o) where
  pure a = Comb
    { grammar: mempty
    , pretty: Just Null
    , prettyGrammar: empty
    , rules: pure
      { rule: empty
      , resultant: pure a
      }
    }
instance altComb :: Alt (Comb nt cat o) where
  alt (Comb l) (Comb r) = Comb
    { grammar: l.grammar <> r.grammar
    , pretty: case l.pretty, r.pretty of
        Nothing, Nothing -> Nothing
        Just lp, Nothing -> Just lp
        Nothing, Just rp -> Just rp
        Just lp, Just rp -> Just (Disj lp rp)
    , prettyGrammar: l.prettyGrammar <|> r.prettyGrammar
    , rules: l.rules <|> r.rules
    }
instance plusComb :: Plus (Comb nt cat o) where
  empty = Comb { grammar: mempty, pretty: Nothing, prettyGrammar: empty, rules: empty }
-- | Distributivity follows from distributivity of `Array`
instance alternativeComb :: Alternative (Comb nt cat o)
instance compactableComb :: Compactable (Comb nt cat o) where
  compact (Comb c) = Comb c { rules = c.rules <#> \r -> r { resultant = compact r.resultant } }
  separate eta = separateDefault eta

token :: forall nt cat o. cat -> Comb nt cat o o
token cat = Comb
  { grammar: mempty
  , pretty: Just $ Part $ Terminal cat
  , prettyGrammar: empty
  , rules: pure
    { rule: pure (Terminal cat)
    , resultant: component case _ of
        Leaf t -> Just t
        _ -> Nothing
    }
  }
tokens :: forall nt cat o. Array cat -> Comb nt cat o (Array o)
tokens toks = Comb
  { grammar: mempty
  , pretty: Just $ foldl Conj Null (Part <<< Terminal <$> toks)
  , prettyGrammar: empty
  , rules: pure
    { rule: Terminal <$> toks
    , resultant: components $ toks $> case _ of
        Leaf t -> Just t
        _ -> Nothing
    }
  }

-- | Name a nonterminal production, this allows recursion.
namedRec :: forall nt cat o a. nt -> (Comb nt cat o a -> Comb nt cat o a) -> Comb nt cat o a
namedRec name rec =
  let
    recursive = rec $ Comb
      { grammar: mempty
      , pretty: Just $ Part $ NonTerminal name
      , prettyGrammar: empty
      , rules: pure
        { rule: pure (NonTerminal name)
        , resultant: component \cst ->
            let Comb borrowed = recursive in
            matchRule name (borrowed.rules <#> _.resultant) cst
        }
      }
    Comb produced = recursive
    newRules = MkGrammar $ produced.rules # mapWithIndex \i { rule } ->
      { pName: name
      , rName: i
      , rule
      }
  in Comb
    { grammar: newRules <> produced.grammar
    , prettyGrammar: pure (Tuple name produced.pretty) <> produced.prettyGrammar
    , pretty: Just $ Part $ NonTerminal name
    , rules: pure
        { rule: pure (NonTerminal name)
        , resultant: component $ matchRule name (produced.rules <#> _.resultant)
        }
    }

named :: forall nt cat o a. nt -> Comb nt cat o a -> Comb nt cat o a
named name = namedRec name <<< const

matchRule :: forall nt o a. nt -> Array (CResultant nt o a) -> CCST nt o -> Maybe a
matchRule name resultants (Branch (Tuple _name i) children) = do
  assertM "names" (unsafeRefEq _name name)
  resultant <- nope "missing from resultants" $ resultants !! i
  nope "failed to apply" $ resultFrom resultant children
matchRule _ _ _ = Nothing

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

nope :: String -> Maybe ~> Maybe
nope _ v = v
nope s Nothing = unsafePerformEffect (Nothing <$ log s)

assert :: forall a. String -> Boolean -> a -> a
assert msg false v = unsafePerformEffect (v <$ error msg)
assert _ _ v = v

assertM :: forall m. Applicative m => String -> Boolean -> m Unit
assertM msg b = assert msg b (pure unit)
