module Parser.Comb.Combinators where

import Prelude

import Control.Plus (empty)
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Idiolect ((>==))
import Parser.Comb.Syntax (Syntax(..))
import Parser.Comb.Types (Comb(..), Options(..), Resultant(..), component, components, matchRule, withCST_)
import Parser.Lexing (class ToString, class Token, type (~), Rawr, Similar(..), rawr, rerecognize, toString)
import Parser.Types (CST(..), Grammar(..), Part(..), sourceCST)

token :: forall rec err nt cat o. Token cat o => cat -> Comb rec err nt cat o o
token cat = Comb
  { grammar: mempty
  , entrypoints: empty
  , pretty: Just $ Part $ Terminal cat
  , prettyGrammar: empty
  , rules: pure
    { rule: pure (Terminal cat)
    , resultant: component case _, _ of
        _, Leaf t | rerecognize cat t -> Right t
        _, _ -> Left []
    }
  }

tokenRawr :: forall rec err nt. String -> Comb rec err nt (String ~ Rawr) String String
tokenRawr = rawr >>> Right >>> Similar >>> token

tokenStr :: forall s rec err nt. ToString s => s -> Comb rec err nt (String ~ Rawr) String String
tokenStr = toString >>> Left >>> Similar >>> token

tokens :: forall rec err nt cat o. Token cat o => Array cat -> Comb rec err nt cat o (Array o)
tokens cats = Comb
  { grammar: mempty
  , entrypoints: empty
  , pretty: Just $ foldl Conj Null (Part <<< Terminal <$> cats)
  , prettyGrammar: empty
  , rules: pure
    { rule: Terminal <$> cats
    , resultant: components $ cats <#> \cat _ -> case _ of
        Leaf t | rerecognize cat t -> Right t
        _ -> Left []
    }
  }

buildTree ::
  forall rec err nt cat o a.
  nt -> Comb rec err nt cat o a -> Options rec err nt Int cat o
buildTree name (Comb c) = Options $
  c.rules # mapWithIndex \i { resultant: Resultant { accepting }, rule } ->
    { pName: name
    , rName: i
    , rule: rule
    , logicParts: accepting
    , advanced: []
    }

-- | Name a nonterminal production, this allows recursion.
namedRec ::
  forall rec err nt cat o a.
    Ord nt =>
  nt ->
  (Comb rec err nt cat o a -> Comb rec err nt cat o a) ->
  Comb rec err nt cat o a
namedRec name defineParser = fst $
  namedRec' name (Tuple <$> defineParser <@> unit)

namedRec' ::
  forall rec err nt cat o a r.
    Ord nt =>
  nt ->
  (Comb rec err nt cat o a -> Comb rec err nt cat o a /\ r) ->
  Comb rec err nt cat o a /\ r
namedRec' name defineParser =
  let
    recursive = defineParser $ Comb
      { grammar: mempty
      , entrypoints: empty
      , pretty: Just $ Part $ NonTerminal name
      , prettyGrammar: empty
      , rules: pure
        { rule: pure $ NonTerminal $ Tuple (defer \_ -> buildTree name (fst recursive)) name
        , resultant: component \rec cst ->
            let Comb borrowed = fst recursive in
            matchRule rec name (borrowed.rules <#> _.resultant) cst
        }
      }
    Comb produced = fst recursive
    newRules = MkGrammar $ produced.rules # mapWithIndex \i { rule } ->
      { pName: name
      , rName: i
      , rule: lmap snd <$> rule
      }
  in Comb
    { grammar: newRules <> produced.grammar
    , entrypoints: produced.entrypoints
    , prettyGrammar: pure (Tuple name produced.pretty) <> produced.prettyGrammar
    , pretty: Just $ Part $ NonTerminal name
    , rules: pure
        -- not quite sure why defer is needed here?
        { rule: pure $ NonTerminal $ Tuple (defer \_ -> buildTree name (fst recursive)) name
        , resultant: component \rec -> matchRule rec name (produced.rules <#> _.resultant)
        }
    } /\ snd recursive

-- | Name a parser. This may introduce ambiguity into the grammar.
named :: forall rec err nt cat o a. Ord nt => nt -> Comb rec err nt cat o a -> Comb rec err nt cat o a
named name = namedRec name <<< const

-- | Return the source parsed by the given parser, instead of whatever its
-- | applicative result was.
sourceOf :: forall rec err nt cat o a. Monoid o => Comb rec err nt cat o a -> Comb rec err nt cat o o
sourceOf = tokensSourceOf >== fold

-- | Return the source tokens parsed by the given parser, instead of whatever
-- | its applicative result was.
tokensSourceOf :: forall rec err nt cat o a. Comb rec err nt cat o a -> Comb rec err nt cat o (Array o)
tokensSourceOf = withCST_ \_ csts _ -> csts >>= sourceCST
