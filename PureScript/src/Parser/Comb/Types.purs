module Parser.Comb.Types where

import Prelude

import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Array (head, length, splitAt, zipWith, (!!))
import Data.Bifunctor (bimap, lmap)
import Data.Compactable (class Compactable, compact, separateDefault)
import Data.Either (Either)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.String (CodePoint)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Parser.Comb.Syntax (Syntax(..))
import Parser.Types (CST(..), Fragment, Grammar)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Util (memoizeEq)

type CSyntax = Syntax
type CGrammar nt cat = Grammar nt Int cat
type CFragment nt cat = Fragment nt cat
type CCST nt o = CST (nt /\ Int) o

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
-- |
-- | Type parameters:
-- | - `rec`: Parameter to enable recursively calling (sub-)parsers.
-- | - `nt`: Names for non-terminals. Must have an `Ord` instance.
-- | - `cat`: Categories of tokens, including literals. Also requires `Ord`.
-- |   Commonly `Similar String Rawr` to match with literal keywords and regexes.
-- | - `o`: Tokenize values output by the lexer. These appear in the CST.
-- | - `a`: Parser result, can be any type.
newtype Comb rec nt cat o a = Comb
  { grammar :: CGrammar nt cat
  , entrypoints :: Array nt
  , pretty :: Maybe (CSyntax nt cat)
  , prettyGrammar :: Array (Tuple nt (Maybe (CSyntax nt cat)))
  , rules :: Array
    -- Each nonterminal embedded in the rule comes with a way to determine
    -- how it is allowed to fail
    { rule :: CFragment (Lazy (Options rec nt Int cat o) /\ nt) cat
    -- The only part that deals with the parser result type `a`, essentially
    -- a codec for deserializing CSTs into application types.
    , resultant :: CResultant rec nt o a
    }
  }
type Combs = Comb Unit String CodePoint CodePoint

rerec :: forall rec1 rec2 nt cat o a. (rec2 -> rec1) -> Comb rec1 nt cat o a -> Comb rec2 nt cat o a
rerec f (Comb c) = Comb c
  { rules = c.rules <#> \rule ->
    rule
      { rule = lmap (lmap (map (cmapOptions f))) <$> rule.rule
      , resultant = lcmap f rule.resultant
      }
  }

newtype Rec nt i o = Rec (nt -> i -> Either ParseError (CCST nt o))

type ParseError = String

-- | Parses an array (of CSTs), expecting a certain number of elements.
-- | It may still return failure early, though.
-- |
-- | Invariants:
-- | - `length i < r.length => (r.result `elem` [Partial, Failed])`
-- | - `length i == r.length => r.result /= Partial`
-- | - `r.result i == r.result (take r.length i)`
newtype Resultant r tok rec a = Resultant
  { length :: Int
  , accepting :: LogicParts r tok rec
  , result :: rec -> Array (CST r tok) -> PartialResult a
  }
type CResultant rec nt o = Resultant (nt /\ Int) o rec

data PartialResult a
  = Failed
  | Partial
  | Result a

newtype Options rec nt r cat o = Options
  (Array (Option rec nt r cat o))
type Option rec nt r cat o =
  { pName :: nt
  , rName :: r
  , rule :: Fragment (Lazy (Options rec nt r cat o) /\ nt) cat
  , logicParts :: LogicParts (nt /\ r) o rec
  , advanced :: Array (CST (nt /\ r) o)
  }
type COptions rec nt cat o = Options rec nt Int cat o

derive newtype instance semigroupOptions :: Semigroup (Options rec nt r cat o)
derive newtype instance monoidOptions :: Monoid (Options rec nt r cat o)
derive instance newtypeOptions :: Newtype (Options rec nt r cat o) _

newtype LogicParts r tok rec = LogicParts
  { necessary :: Array
    { start :: Int
    , length :: Int
    -- RefEq
    , logic :: Logicful (CST r tok) rec
    , parents :: Array (Tuple (Array (CST r tok)) r)
    }
  }
derive newtype instance semigroupLogicParts :: Semigroup (LogicParts r tok rec)
derive newtype instance monoidLogicParts :: Monoid (LogicParts r tok rec)
derive instance contravariantLogicParts :: Contravariant (LogicParts r tok)
derive instance newtypeLogicParts :: Newtype (LogicParts r tok rec) _
derive newtype instance eqLogicParts :: (Eq r, Eq tok) => Eq (LogicParts r tok rec)

cmapOptions :: forall rec rec' nt r cat o. (rec' -> rec) -> Options rec nt r cat o -> Options rec' nt r cat o
cmapOptions f (Options options) = Options $ options <#> \option ->
  option
    { rule = lmap (lmap (map (cmapOptions f))) <$> option.rule
    , logicParts = cmap f option.logicParts
    }

fullMapOptions :: forall rec rec' nt nt' r r' cat cat' o o'.
  { rec :: rec' -> rec
  , nt :: nt -> nt'
  , r :: r -> r'
  , cat :: cat -> cat'
  , o :: o -> o'
  , cst :: CST (nt' /\ r') o' -> Maybe (CST (nt /\ r) o)
  } -> Options rec nt r cat o -> Options rec' nt' r' cat' o'
fullMapOptions fs =
  let
    mapLogicful = memoizeEq $ overLogic $
      \f r is ->
        case traverse fs.cst is of
          Just is' -> f (fs.rec r) is'
          Nothing -> false
    mapLogicParts = over LogicParts \lp ->
      { necessary: lp.necessary <#> \n -> n
        { logic = mapLogicful n.logic
        , parents = bimap (map (bimap (bimap fs.nt fs.r) fs.o)) (bimap fs.nt fs.r) <$> n.parents
        }
      }
    go = over Options $ map \opt ->
      { pName: fs.nt opt.pName
      , rName: fs.r opt.rName
      , rule: bimap (bimap (map go) fs.nt) fs.cat <$> opt.rule
      , logicParts: mapLogicParts opt.logicParts
      , advanced: bimap (bimap fs.nt fs.r) fs.o <$> opt.advanced
      }
  in go

foreign import data Logicful :: Type -> Type -> Type
logicful :: forall i r. (r -> Array i -> Boolean) -> Logicful i r
logicful = unsafeCoerce
applyLogic :: forall i r. Logicful i r -> r -> Array i -> Boolean
applyLogic = unsafeCoerce
overLogic ::
  forall p393 t398 t399 t400 t401.
    Profunctor p393 =>
  p393 (t399 -> Array t398 -> Boolean) (t401 -> Array t400 -> Boolean) ->
  p393 (Logicful t398 t399) (Logicful t400 t401)
overLogic = dimap applyLogic logicful
over2Logic ::
  forall i384 r385 i386 r387 i389 r390.
  ((r387 -> Array i386 -> Boolean) -> (r390 -> Array i389 -> Boolean) -> r385 -> Array i384 -> Boolean) ->
  Logicful i386 r387 -> Logicful i389 r390 -> Logicful i384 r385
over2Logic op f g = logicful (op (applyLogic f) (applyLogic g))
instance eqLogicful :: Eq (Logicful i r) where
  eq = unsafeRefEq
instance heytingAlgebraLogicful :: HeytingAlgebra (Logicful i r) where
  tt = logicful tt
  ff = logicful ff
  not = overLogic not
  disj = over2Logic disj
  conj = over2Logic conj
  implies = over2Logic implies
instance contravariantLogicful :: Contravariant (Logicful i) where
  cmap f = overLogic (lcmap f)

derive instance functorComb :: Functor (Comb rec nt cat o)
instance applyComb :: Apply (Comb rec nt cat o) where
  apply (Comb l) (Comb r) = Comb
    { grammar: l.grammar <> r.grammar
    , entrypoints: l.entrypoints <|> r.entrypoints
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
instance applicativeComb :: Applicative (Comb rec nt cat o) where
  pure a = Comb
    { grammar: mempty
    , entrypoints: empty
    , pretty: Just Null
    , prettyGrammar: empty
    , rules: pure
      { rule: empty
      , resultant: pure a
      }
    }
instance altComb :: Alt (Comb rec nt cat o) where
  alt (Comb l) (Comb r) = Comb
    { grammar: l.grammar <> r.grammar
    , entrypoints: l.entrypoints <|> r.entrypoints
    , pretty: case l.pretty, r.pretty of
        Nothing, Nothing -> Nothing
        Just lp, Nothing -> Just lp
        Nothing, Just rp -> Just rp
        Just lp, Just rp -> Just (Disj lp rp)
    , prettyGrammar: l.prettyGrammar <|> r.prettyGrammar
    , rules: l.rules <|> r.rules
    }
instance plusComb :: Plus (Comb rec nt cat o) where
  empty = Comb { grammar: mempty, entrypoints: empty, pretty: Nothing, prettyGrammar: empty, rules: empty }
-- | Distributivity follows from distributivity of `Array`
instance alternativeComb :: Alternative (Comb rec nt cat o)
instance compactableComb :: Compactable (Comb rec nt cat o) where
  compact (Comb c) = Comb c { rules = c.rules <#> \r -> r { resultant = compact r.resultant } }
  separate eta = separateDefault eta
instance semigroupComb :: Semigroup a => Semigroup (Comb rec nt cat o a) where
  append = lift2 append
instance monoidComb :: Monoid a => Monoid (Comb rec nt cat o a) where
  mempty = pure mempty
instance heytingAlgebraComb :: HeytingAlgebra a => HeytingAlgebra (Comb rec nt cat o a) where
  tt = pure tt
  ff = pure ff
  not = map not
  disj = lift2 disj
  conj = lift2 conj
  implies = lift2 implies

-- | Matched a named rule against a CST, with codecs for each production.
matchRule :: forall rec nt o a. Ord nt => rec -> nt -> Array (CResultant rec nt o a) -> CCST nt o -> Maybe a
matchRule rec name resultants (Branch (Tuple _name i) children) | name == _name = do
  resultant <- resultants !! i
  resultFrom resultant rec children
matchRule _ _ _ _ = Nothing

-- | Modify the result of the parser based on the CST fragment it receives.
withCST :: forall rec nt cat o a b. (Array (CCST nt o) -> PartialResult (a -> b)) -> Comb rec nt cat o a -> Comb rec nt cat o b
withCST f = withCST' \_ csts prev -> f csts <*> prev unit

acceptResult :: forall r tok rec a. Int -> (rec -> Array (CST r tok) -> PartialResult a) -> LogicParts r tok rec
acceptResult length f = LogicParts
  { necessary: pure
    { start: 0
    , length
    , logic: logicful \r i -> case f r i of
      Failed -> false
      _ -> true
    , parents: []
    }
  }

shiftAccepting :: forall r tok rec. Int -> LogicParts r tok rec -> LogicParts r tok rec
shiftAccepting length (LogicParts l) = LogicParts $
  l { necessary = l.necessary <#> \n -> n { start = n.start + length } }

-- | Modify the result of the parser based on the CST fragment it receives.
withCST' :: forall rec nt cat o a b. (rec -> Array (CCST nt o) -> (Unit -> PartialResult a) -> PartialResult b) -> Comb rec nt cat o a -> Comb rec nt cat o b
withCST' f (Comb c) = Comb c
  { rules = c.rules <#> \r@{ resultant: Resultant { length, result } } -> r
    { resultant = Resultant
      { length
      , accepting: acceptResult length \rec csts -> f rec csts (\_  -> result rec csts)
      , result: \rec csts -> f rec csts (\_  -> result rec csts)
      }
    }
  }

withCST_ :: forall rec nt cat o a b. (rec -> Array (CCST nt o) -> (Unit -> PartialResult a) -> b) -> Comb rec nt cat o a -> Comb rec nt cat o b
withCST_ f (Comb c) = Comb c
  { rules = c.rules <#> \r@{ resultant: Resultant { length, result } } -> r
    { resultant = Resultant
      { length
      , accepting: mempty
      , result: \rec csts -> Result (f rec csts (\_  -> result rec csts))
      }
    }
  }

mapMaybe :: forall rec nt cat o a b. (a -> Maybe b) -> Comb rec nt cat o a -> Comb rec nt cat o b
mapMaybe f = withCST' \_ _ da -> da unit >>= \a ->
  case f a of
    Nothing -> Failed
    Just r -> Result r

-- | Get access to the recursive parser machinery. (Advanced.)
withRec :: forall rec nt cat o a b. (rec -> a -> b) -> Comb rec nt cat o a -> Comb rec nt cat o b
withRec f = withCST' \rec _ prev -> f rec <$> prev unit

derive instance functorResultant :: Functor (Resultant r tok rec)
derive instance profunctorResultant :: Profunctor (Resultant r tok)
instance applyResultant :: Apply (Resultant r tok rec) where
  apply (Resultant l) (Resultant r) = Resultant
    { length: l.length + r.length
    , accepting: l.accepting <> shiftAccepting l.length r.accepting
    , result: \rec i ->
        let { before: li, after: ri } = splitAt l.length i in
        l.result rec li <*> r.result rec ri
    }
instance applicativeResultant :: Applicative (Resultant r tok rec) where
  pure a = Resultant { length: 0, accepting: mempty, result: \_ _ -> pure a }
instance compactableResultant :: Compactable (Resultant r tok rec) where
  compact (Resultant r) = Resultant r { result = map (map compact) r.result }
  separate eta = separateDefault eta

-- | Apply the resultant.
resultFrom :: forall r tok rec a. Resultant r tok rec a -> rec -> Array (CST r tok) -> Maybe a
resultFrom (Resultant res) r i = case res.result r i of
  Result a -> Just a
  _ -> Nothing

component :: forall r tok rec a. (rec -> CST r tok -> Maybe a) -> Resultant r tok rec a
component c = Resultant
  { length: 1
  , accepting: mempty
  , result: \r is ->
      case head is of
        Nothing -> Partial
        Just i -> compact (Result (c r i))
  }
-- | components = traverse component
components :: forall r tok rec a. Array (rec -> CST r tok -> Maybe a) -> Resultant r tok rec (Array a)
components cs = Resultant
  { length: length cs
  , accepting: mempty
  , result: \r is ->
      if length is < length cs then Partial else
        compact (Result (sequence (zipWith identity (cs <@> r) is)))
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
instance bindPartialResult :: Bind PartialResult where
  bind Failed _ = Failed
  bind Partial _ = Partial
  bind (Result a) f = f a
instance monadPartialResult :: Monad PartialResult
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
