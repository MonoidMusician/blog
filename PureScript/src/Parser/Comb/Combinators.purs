module Parser.Comb.Combinators where

import Prelude

import Control.Plus (empty)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Parser.Comb.Syntax (Syntax(..))
import Parser.Comb.Types (Comb(..), PartialResult(..), Rec(..), ParseError, component, components, matchRule, withCST')
import Parser.Lexing (class ToString, class Token, type (~), Rawr, Similar(..), rawr, rerecognize, toString)
import Parser.Types (CST(..), Grammar(..), OrEOF, Part(..), sourceCST)

token :: forall rec nt cat o. Token cat o => cat -> Comb rec nt cat o o
token cat = Comb
  { grammar: mempty
  , pretty: Just $ Part $ Terminal cat
  , prettyGrammar: empty
  , rules: pure
    { rule: pure (Terminal cat)
    , resultant: component case _, _ of
        _, Leaf t | rerecognize cat t -> Just t
        _, _ -> Nothing
    }
  }

tokenRawr :: forall rec nt. String -> Comb rec nt (String ~ Rawr) String String
tokenRawr = rawr >>> Right >>> Similar >>> token

tokenStr :: forall rec s nt. ToString s => s -> Comb rec nt (String ~ Rawr) String String
tokenStr = toString >>> Left >>> Similar >>> token

tokens :: forall rec nt cat o. Token cat o => Array cat -> Comb rec nt cat o (Array o)
tokens cats = Comb
  { grammar: mempty
  , pretty: Just $ foldl Conj Null (Part <<< Terminal <$> cats)
  , prettyGrammar: empty
  , rules: pure
    { rule: Terminal <$> cats
    , resultant: components $ cats <#> \cat _ -> case _ of
        Leaf t | rerecognize cat t -> Just t
        _ -> Nothing
    }
  }

-- | Name a nonterminal production, this allows recursion.
namedRec :: forall rec nt cat o a. Ord nt => nt -> (Comb rec nt cat o a -> Comb rec nt cat o a) -> Comb rec nt cat o a
namedRec name parseRec =
  let
    recursive = parseRec $ Comb
      { grammar: mempty
      , pretty: Just $ Part $ NonTerminal name
      , prettyGrammar: empty
      , rules: pure
        { rule: pure (NonTerminal name)
        , resultant: component \rec cst ->
            let Comb borrowed = recursive in
            matchRule rec name (borrowed.rules <#> _.resultant) cst
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
        , resultant: component \rec -> matchRule rec name (produced.rules <#> _.resultant)
        }
    }

-- | Name a parser. This may introduce ambiguity into the grammar.
named :: forall rec nt cat o a. Ord nt => nt -> Comb rec nt cat o a -> Comb rec nt cat o a
named name = namedRec name <<< const

-- withReparser :: forall nt cat i o a b. Ord nt => nt -> Comb (Rec nt (OrEOF i) o) nt cat o a -> ((i -> Either ParseError a) -> Either ParseError b) -> Comb (Rec nt (OrEOF i) o) nt cat o b
-- withReparser name (Comb c) f =
--   Comb
--     {
--     }

-- | Return the source parsed by the given parser, instead of whatever its
-- | applicative result was.
sourceOf :: forall rec nt cat o a. Monoid o => Comb rec nt cat o a -> Comb rec nt cat o o
sourceOf = tokensSourceOf >>> map fold

-- | Return the source tokens parsed by the given parser, instead of whatever
-- | its applicative result was.
tokensSourceOf :: forall rec nt cat o a. Comb rec nt cat o a -> Comb rec nt cat o (Array o)
tokensSourceOf = withCST' \csts _ -> Result (sourceCST =<< csts)
