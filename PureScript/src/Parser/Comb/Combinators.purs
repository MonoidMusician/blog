module Parser.Comb.Combinators where

import Prelude

import Control.Plus (empty)
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (oneOfMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Idiolect ((>==))
import Parser.Comb.Syntax (Syntax(..))
import Parser.Comb.Types (Associativity(..), Comb(..), Options(..), Resultant(..), component, components, matchRule, withCST_)
import Parser.Lexing (class ToString, class Token, type (~), Rawr, Similar(..), rawr, rerecognize, toString)
import Parser.Types (CST(..), Grammar(..), Part(..), sourceCST)

setPrecA :: forall rec err prec nt cat o. cat -> Associativity -> prec -> Comb rec err prec nt cat o Unit
setPrecA cat assoc prec = case pure unit of
  Comb c -> Comb c { tokenPrecedence = [cat /\ (prec /\ assoc)] }

setPrecL :: forall rec err prec nt cat o. cat -> prec -> Comb rec err prec nt cat o Unit
setPrecL = setPrecA <@> AssocL
setPrecR :: forall rec err prec nt cat o. cat -> prec -> Comb rec err prec nt cat o Unit
setPrecR = setPrecA <@> AssocR
setPrec :: forall rec err prec nt cat o. cat -> prec -> Comb rec err prec nt cat o Unit
setPrec = setPrecA <@> NoAssoc

token :: forall rec err prec nt cat o. Token cat o => cat -> Comb rec err prec nt cat o o
token cat = Comb
  { grammar: mempty
  , entrypoints: empty
  , pretty: Just $ Part $ Terminal cat
  , prettyGrammar: empty
  , tokenPrecedence: empty
  , rules: pure
    { rule: pure (Terminal cat)
    , resultant: component case _, _ of
        _, Leaf t | rerecognize cat t -> Right t
        _, _ -> Left []
    }
  }

tokenPrecA :: forall rec err prec nt cat o. Token cat o => cat -> Associativity -> prec -> Comb rec err prec nt cat o o
tokenPrecA cat assoc prec = setPrecA cat assoc prec *> token cat

tokenPrecL :: forall rec err prec nt cat o. Token cat o => cat -> prec -> Comb rec err prec nt cat o o
tokenPrecL = tokenPrecA <@> AssocL
tokenPrecR :: forall rec err prec nt cat o. Token cat o => cat -> prec -> Comb rec err prec nt cat o o
tokenPrecR = tokenPrecA <@> AssocR
tokenPrec :: forall rec err prec nt cat o. Token cat o => cat -> prec -> Comb rec err prec nt cat o o
tokenPrec = tokenPrecA <@> NoAssoc

tokenRawr :: forall rec err prec nt. String -> Comb rec err prec nt (String ~ Rawr) String String
tokenRawr = rawr >>> Right >>> Similar >>> token

tokenStr :: forall s rec err prec nt. ToString s => s -> Comb rec err prec nt (String ~ Rawr) String String
tokenStr = toString >>> Left >>> Similar >>> token

tokens :: forall rec err prec nt cat o. Token cat o => Array cat -> Comb rec err prec nt cat o (Array o)
tokens cats = Comb
  { grammar: mempty
  , entrypoints: empty
  , pretty: Just $ foldl Conj Null (Part <<< Terminal <$> cats)
  , prettyGrammar: empty
  , tokenPrecedence: empty
  , rules: pure
    { rule: Terminal <$> cats
    , resultant: components $ cats <#> \cat _ -> case _ of
        Leaf t | rerecognize cat t -> Right t
        _ -> Left []
    }
  }

buildTree ::
  forall rec err prec nt cat o a.
  nt -> Comb rec err prec nt cat o a -> Options rec err nt Int cat o
buildTree name (Comb c) = Options $
  c.rules # mapWithIndex \i { resultant: Resultant { accepting }, rule } ->
    { pName: name
    , rName: i
    , rule: rule
    , logicParts: accepting
    , advanced: []
    }

type WithPrec prec a = Array (Maybe prec /\ a)

withPrec :: forall prec a. prec -> a -> Maybe prec /\ a
withPrec prec a = Just prec /\ a

noPrec :: forall prec a. a -> Maybe prec /\ a
noPrec a = Nothing /\ a

noPrecs :: forall prec a. a -> WithPrec prec a
noPrecs a = [Nothing /\ a]

-- | Name a nonterminal production, this allows recursion.
namedRec ::
  forall rec err prec nt cat o a.
    Ord nt =>
  nt ->
  (Comb rec err prec nt cat o a -> Comb rec err prec nt cat o a) ->
  Comb rec err prec nt cat o a
namedRec name defineParser = namedPrecRec name (noPrecs <<< defineParser)

namedPrecRec ::
  forall rec err prec nt cat o a.
    Ord nt =>
  nt ->
  (Comb rec err prec nt cat o a -> WithPrec prec (Comb rec err prec nt cat o a)) ->
  Comb rec err prec nt cat o a
namedPrecRec name defineParser = fst $
  namedPrecRec' name (Tuple <$> defineParser <@> unit)

namedRec' ::
  forall rec err prec nt cat o a r.
    Ord nt =>
  nt ->
  (Comb rec err prec nt cat o a -> Comb rec err prec nt cat o a /\ r) ->
  Comb rec err prec nt cat o a /\ r
namedRec' name defineParser = namedPrecRec' name (lmap noPrecs <<< defineParser)

namedPrecRec' ::
  forall rec err prec nt cat o a r.
    Ord nt =>
  nt ->
  (Comb rec err prec nt cat o a -> WithPrec prec (Comb rec err prec nt cat o a) /\ r) ->
  Comb rec err prec nt cat o a /\ r
namedPrecRec' name defineParser =
  let
    recursive = defineParser $ Comb
      { grammar: mempty
      , entrypoints: empty
      , pretty: Just $ Part $ NonTerminal name
      , prettyGrammar: empty
      , tokenPrecedence: empty
      , rules: pure
        { rule: pure $ NonTerminal $ Tuple (defer \_ -> buildTree name (oneOfMap snd (fst recursive))) name
        , resultant: component \rec cst ->
            let Comb borrowed = oneOfMap snd (fst recursive) in
            matchRule rec name (borrowed.rules <#> _.resultant) cst
        }
      }
    Comb produced = oneOfMap snd (fst recursive)
    newRules = MkGrammar $ fst recursive >>= \(prec /\ Comb { rules }) ->
      rules # mapWithIndex \i { rule } ->
        { pName: name
        , rName: prec /\ i
        , rule: lmap snd <$> rule
        }
  in Comb
    { grammar: newRules <> produced.grammar
    , entrypoints: produced.entrypoints
    , prettyGrammar: pure (Tuple name produced.pretty) <> produced.prettyGrammar
    , pretty: Just $ Part $ NonTerminal name
    , tokenPrecedence: produced.tokenPrecedence
    , rules: pure
        { rule: pure $ NonTerminal $ Tuple (defer \_ -> buildTree name (oneOfMap snd (fst recursive))) name
        , resultant: component \rec -> matchRule rec name (produced.rules <#> _.resultant)
        }
    } /\ snd recursive

-- | Name a parser. This may introduce ambiguity into the grammar where it
-- | otherwise did not exist.
named :: forall rec err prec nt cat o a. Ord nt => nt -> Comb rec err prec nt cat o a -> Comb rec err prec nt cat o a
named name = namedPrec name <<< noPrecs

namedPrec :: forall rec err prec nt cat o a. Ord nt => nt -> WithPrec prec (Comb rec err prec nt cat o a) -> Comb rec err prec nt cat o a
namedPrec name = namedPrecRec name <<< const

-- | Return the source parsed by the given parser, instead of whatever its
-- | applicative result was.
sourceOf :: forall rec err prec nt cat o a. Monoid o => Comb rec err prec nt cat o a -> Comb rec err prec nt cat o o
sourceOf = tokensSourceOf >== fold

-- | Return the source tokens parsed by the given parser, instead of whatever
-- | its applicative result was.
tokensSourceOf :: forall rec err prec nt cat o a. Comb rec err prec nt cat o a -> Comb rec err prec nt cat o (Array o)
tokensSourceOf = withCST_ \_ csts _ -> csts >>= sourceCST
