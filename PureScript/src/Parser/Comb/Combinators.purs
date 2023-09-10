module Parser.Comb.Combinators where

import Prelude

import Control.Plus (empty)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Parser.Lexing (class ToString, class Token, type (~), Rawr, Similar(..), rawr, rerecognize, toString)
import Parser.Types (CST(..), Grammar(..), Part(..), sourceCST)
import Parser.Comb.Syntax (Syntax(..))
import Parser.Comb.Types (Comb(..), PartialResult(..), component, components, matchRule, withCST')

token :: forall nt cat o. Token cat o => cat -> Comb nt cat o o
token cat = Comb
  { grammar: mempty
  , pretty: Just $ Part $ Terminal cat
  , prettyGrammar: empty
  , rules: pure
    { rule: pure (Terminal cat)
    , resultant: component case _ of
        Leaf t | rerecognize cat t -> Just t
        _ -> Nothing
    }
  }

tokenRawr :: forall nt. String -> Comb nt (String ~ Rawr) String String
tokenRawr = rawr >>> Right >>> Similar >>> token

tokenStr :: forall s nt. ToString s => s -> Comb nt (String ~ Rawr) String String
tokenStr = toString >>> Left >>> Similar >>> token

tokens :: forall nt cat o. Token cat o => Array cat -> Comb nt cat o (Array o)
tokens cats = Comb
  { grammar: mempty
  , pretty: Just $ foldl Conj Null (Part <<< Terminal <$> cats)
  , prettyGrammar: empty
  , rules: pure
    { rule: Terminal <$> cats
    , resultant: components $ cats <#> \cat -> case _ of
        Leaf t | rerecognize cat t -> Just t
        _ -> Nothing
    }
  }

-- | Name a nonterminal production, this allows recursion.
namedRec :: forall nt cat o a. Ord nt => nt -> (Comb nt cat o a -> Comb nt cat o a) -> Comb nt cat o a
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

-- | Name a parser. This may introduce ambiguity into the grammar.
named :: forall nt cat o a. Ord nt => nt -> Comb nt cat o a -> Comb nt cat o a
named name = namedRec name <<< const

-- | Return the source parsed by the given parser, instead of whatever its
-- | applicative result was.
sourceOf :: forall nt cat o a. Monoid o => Comb nt cat o a -> Comb nt cat o o
sourceOf = tokensSourceOf >>> map fold

-- | Return the source tokens parsed by the given parser, instead of whatever
-- | its applicative result was.
tokensSourceOf :: forall nt cat o a. Comb nt cat o a -> Comb nt cat o (Array o)
tokensSourceOf = withCST' \csts _ -> Result (sourceCST =<< csts)
