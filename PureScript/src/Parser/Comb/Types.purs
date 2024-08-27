module Parser.Comb.Types where

import Prelude

import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Array (head, length, splitAt, zipWith, (!!))
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Compactable (class Compactable, compact, separateDefault)
import Data.Either (Either(..), blush, hush, note)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Newtype (class Newtype, over)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.String (CodePoint)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Parser.Comb.Syntax (Syntax(..))
import Parser.Selective (class Casing, class Select, casingOn)
import Parser.Types (Fragment, Grammar, GrammarRule, ICST(..))
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Util (memoizeEq)

data Associativity = AssocL | AssocR | NoAssoc
derive instance eqAssociativity :: Eq Associativity

type CSyntax = Syntax
type CGrammar prec meta nt cat = Grammar meta nt (Maybe prec /\ Int) cat
type CGrammarRule prec meta nt cat = GrammarRule meta nt (Maybe prec /\ Int) cat
type CFragment meta nt cat = Fragment meta nt cat
type CCST air nt o = ICST air (nt /\ Int) o

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
newtype Comb rec err prec meta air nt cat o a = Comb
  { grammar :: CGrammar prec meta nt cat
  , entrypoints :: Array nt
  , pretty :: Maybe (CSyntax meta nt cat)
  , prettyGrammar :: Array (Tuple nt (Maybe (CSyntax meta nt cat)))
  , tokenPrecedence :: Array (Tuple cat (Tuple prec Associativity))
  , rules :: Array
    -- Each nonterminal embedded in the rule comes with a way to determine
    -- how it is allowed to fail
    { rule :: CFragment meta (Lazy (Options rec err meta air nt Int cat o) /\ nt) cat
    -- The only part that deals with the parser result type `a`, essentially
    -- a codec for deserializing CSTs into application types.
    , resultant :: CResultant rec err air nt o a
    , prec :: Last prec
    }
  }
type Combs = Comb Unit String Int CodePoint CodePoint

rerec :: forall rec1 rec2 err prec meta air nt cat o a. (rec2 -> rec1) -> Comb rec1 err prec meta air nt cat o a -> Comb rec2 err prec meta air nt cat o a
rerec f (Comb c) = Comb c
  { rules = c.rules <#> \rule ->
    rule
      { rule = lmap (lmap (map (cmapOptions f))) <$> rule.rule
      , resultant = lcmap f rule.resultant
      }
  }

-- | Parses an array (of CSTs), expecting a certain number of elements.
-- | It may still return failure early, though.
-- |
-- | Invariants:
-- | - `length i < r.length => (r.result `elem` [Partial, Failed])`
-- | - `length i == r.length => r.result /= Partial`
-- | - `r.result i == r.result (take r.length i)`
newtype Resultant err air r tok rec a = Resultant
  { length :: Int
  , accepting :: LogicParts err air r tok rec
  , result :: rec -> Array (ICST air r tok) -> PartialResult err a
  }
type CResultant rec err air nt o = Resultant err air (nt /\ Int) o rec

data PartialResult err a
  = Failed (Array err)
  | Partial
  | Result a

newtype Options rec err meta air nt r cat o = Options
  (Array (Option rec err meta air nt r cat o))
type Option rec err meta air nt r cat o =
  { pName :: nt
  , rName :: r
  , rule :: Fragment meta (Lazy (Options rec err meta air nt r cat o) /\ nt) cat
  , logicParts :: LogicParts err air (nt /\ r) o rec
  , advanced :: Array (ICST air (nt /\ r) o)
  }
type COptions rec err meta air nt cat o = Options rec err meta air nt Int cat o

derive newtype instance semigroupOptions :: Semigroup (Options rec err meta air nt r cat o)
derive newtype instance monoidOptions :: Monoid (Options rec err meta air nt r cat o)
derive instance newtypeOptions :: Newtype (Options rec err meta air nt r cat o) _

newtype LogicParts err air r tok rec = LogicParts
  { necessary :: Array
    { start :: Int
    , length :: Int
    -- RefEq
    , logic :: Logicful err (ICST air r tok) rec
    , parents :: Array (Tuple (Array (ICST air r tok)) r)
    }
  }
derive newtype instance semigroupLogicParts :: Semigroup (LogicParts err air r tok rec)
derive newtype instance monoidLogicParts :: Monoid (LogicParts err air r tok rec)
derive instance contravariantLogicParts :: Contravariant (LogicParts err air r tok)
derive instance newtypeLogicParts :: Newtype (LogicParts err air r tok rec) _
derive newtype instance eqLogicParts :: (Eq air, Eq r, Eq tok) => Eq (LogicParts err air r tok rec)

cmapOptions :: forall rec rec' err meta air nt r cat o. (rec' -> rec) -> Options rec err meta air nt r cat o -> Options rec' err meta air nt r cat o
cmapOptions f (Options options) = Options $ options <#> \option ->
  option
    { rule = lmap (lmap (map (cmapOptions f))) <$> option.rule
    , logicParts = cmap f option.logicParts
    }

fullMapOptions :: forall rec rec' err meta air nt nt' r r' cat cat' o o'.
  { rec :: rec' -> rec
  , nt :: nt -> nt'
  , r :: r -> r'
  , cat :: cat -> cat'
  , o :: o -> o'
  , cst :: ICST air (nt' /\ r') o' -> Either (Array err) (ICST air (nt /\ r) o)
  } -> Options rec err meta air nt r cat o -> Options rec' err meta air nt' r' cat' o'
fullMapOptions fs =
  let
    mapLogicful = memoizeEq $ overLogic $
      \f r is ->
        case traverse fs.cst is of
          Right is' -> f (fs.rec r) is'
          Left e -> Failed e
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

foreign import data Logicful :: Type -> Type -> Type -> Type
logicful :: forall err i r. (r -> Array i -> PartialResult err Void) -> Logicful err i r
logicful = unsafeCoerce
applyLogic :: forall err i r. Logicful err i r -> r -> Array i -> PartialResult err Void
applyLogic = unsafeCoerce
overLogic ::
  forall err p393 t398 t399 t400 t401.
    Profunctor p393 =>
  p393 (t399 -> Array t398 -> PartialResult err Void) (t401 -> Array t400 -> PartialResult err Void) ->
  p393 (Logicful err t398 t399) (Logicful err t400 t401)
overLogic = dimap applyLogic logicful
over2Logic ::
  forall err i384 r385 i386 r387 i389 r390.
  ((r387 -> Array i386 -> PartialResult err Void) -> (r390 -> Array i389 -> PartialResult err Void) -> r385 -> Array i384 -> PartialResult err Void) ->
  Logicful err i386 r387 -> Logicful err i389 r390 -> Logicful err i384 r385
over2Logic op f g = logicful (op (applyLogic f) (applyLogic g))
instance eqLogicful :: Eq (Logicful err i r) where
  eq = unsafeRefEq
-- instance heytingAlgebraLogicful :: HeytingAlgebra (Logicful err i r) where
--   tt = logicful tt
--   ff = logicful ff
--   not = overLogic not
--   disj = over2Logic disj
--   conj = over2Logic conj
--   implies = over2Logic implies
instance contravariantLogicful :: Contravariant (Logicful err i) where
  cmap f = overLogic (lcmap f)

derive instance functorComb :: Functor (Comb rec err prec meta air nt cat o)
instance applyComb :: Apply (Comb rec err prec meta air nt cat o) where
  apply (Comb l) (Comb r) = Comb
    { grammar: l.grammar <> r.grammar
    , entrypoints: l.entrypoints <|> r.entrypoints
    , pretty: Conj <$> l.pretty <*> r.pretty
    , prettyGrammar: l.prettyGrammar <|> r.prettyGrammar
    , tokenPrecedence: l.tokenPrecedence <|> r.tokenPrecedence
    , rules: ado
        x <- l.rules
        y <- r.rules
        in
          { rule: x.rule <|> y.rule
          , resultant: x.resultant <*> y.resultant
          , prec: x.prec <> y.prec
          }
    }
instance applicativeComb :: Applicative (Comb rec err prec meta air nt cat o) where
  pure a = Comb
    { grammar: mempty
    , entrypoints: empty
    , pretty: Just Null
    , prettyGrammar: empty
    , tokenPrecedence: empty
    , rules: pure
      { rule: empty
      , resultant: pure a
      , prec: mempty
      }
    }
instance altComb :: Alt (Comb rec err prec meta air nt cat o) where
  alt (Comb l) (Comb r) = Comb
    { grammar: l.grammar <> r.grammar
    , entrypoints: l.entrypoints <|> r.entrypoints
    , pretty: case l.pretty, r.pretty of
        Nothing, Nothing -> Nothing
        Just lp, Nothing -> Just lp
        Nothing, Just rp -> Just rp
        Just lp, Just rp -> Just (Disj lp rp)
    , prettyGrammar: l.prettyGrammar <|> r.prettyGrammar
    , tokenPrecedence: l.tokenPrecedence <|> r.tokenPrecedence
    , rules: l.rules <|> r.rules
    }
instance plusComb :: Plus (Comb rec err prec meta air nt cat o) where
  empty = Comb
    { grammar: mempty
    , entrypoints: empty
    , pretty: Nothing
    , prettyGrammar: empty
    , tokenPrecedence: empty
    , rules: empty
    }
-- | Distributivity follows from distributivity of `Array`
instance alternativeComb :: Alternative (Comb rec err prec meta air nt cat o)
instance compactableComb :: Compactable (Comb rec err prec meta air nt cat o) where
  compact (Comb c) = Comb c { rules = c.rules <#> \r -> r { resultant = compact r.resultant } }
  separate eta = separateDefault eta
instance semigroupComb :: Semigroup a => Semigroup (Comb rec err prec meta air nt cat o a) where
  append = lift2 append
instance monoidComb :: Monoid a => Monoid (Comb rec err prec meta air nt cat o a) where
  mempty = pure mempty
instance heytingAlgebraComb :: HeytingAlgebra a => HeytingAlgebra (Comb rec err prec meta air nt cat o a) where
  tt = pure tt
  ff = pure ff
  not = map not
  disj = lift2 disj
  conj = lift2 conj
  implies = lift2 implies

instance selectComb :: Select (Comb rec err prec meta air nt cat o) where
  select l r =
    ((#) <$> compact (blush <$> l) <*> r) <|> compact (hush <$> l)

instance casingComb :: Casing (Comb rec err prec meta air nt cat o) where
  caseTreeOn = casingOn

-- | Matched a named rule against a CST, with codecs for each production.
matchRule :: forall rec err air nt o a. Ord nt => rec -> nt -> Array (CResultant rec err air nt o a) -> CCST air nt o -> Either (Array err) a
matchRule rec name resultants (IBranch (Tuple _name i) children) | name == _name = do
  resultant <- note [] (resultants !! i)
  resultFrom resultant rec children
matchRule _ _ _ _ = Left [] -- this is bad

-- | Modify the result of the parser based on the CST fragment it receives.
withCST :: forall rec err prec meta air nt cat o a b. (Array (CCST air nt o) -> PartialResult err (a -> b)) -> Comb rec err prec meta air nt cat o a -> Comb rec err prec meta air nt cat o b
withCST f = withCST' \_ csts prev -> f csts <*> prev unit

acceptResult :: forall err air r tok rec a. Int -> (rec -> Array (ICST air r tok) -> PartialResult err a) -> LogicParts err air r tok rec
acceptResult length f = LogicParts
  { necessary: pure
    { start: 0
    , length
    , logic: logicful \r i -> case f r i of
        Failed e -> Failed e
        _ -> Partial
    , parents: []
    }
  }

shiftAccepting :: forall err air r tok rec. Int -> LogicParts err air r tok rec -> LogicParts err air r tok rec
shiftAccepting length (LogicParts l) = LogicParts $
  l { necessary = l.necessary <#> \n -> n { start = n.start + length } }

-- | Modify the result of the parser based on the CST fragment it receives.
withCST' :: forall rec err prec meta air nt cat o a b. (rec -> Array (CCST air nt o) -> (Unit -> PartialResult err a) -> PartialResult err b) -> Comb rec err prec meta air nt cat o a -> Comb rec err prec meta air nt cat o b
withCST' f (Comb c) = Comb c
  { rules = c.rules <#> \r@{ resultant: Resultant { length, result } } -> r
    { resultant = Resultant
      { length
      , accepting: acceptResult length \rec csts -> f rec csts (\_  -> result rec csts)
      , result: \rec csts -> f rec csts (\_  -> result rec csts)
      }
    }
  }

withCST'_ :: forall rec err prec meta air nt cat o a b. (rec -> Array (CCST air nt o) -> (Unit -> PartialResult err a) -> PartialResult err b) -> Comb rec err prec meta air nt cat o a -> Comb rec err prec meta air nt cat o b
withCST'_ f (Comb c) = Comb c
  { rules = c.rules <#> \r@{ resultant: Resultant { length, accepting, result } } -> r
    { resultant = Resultant
      { length
      , accepting
      , result: \rec csts -> f rec csts (\_  -> result rec csts)
      }
    }
  }

withCST_ :: forall rec err prec meta air nt cat o a b. (rec -> Array (CCST air nt o) -> (Unit -> PartialResult err a) -> b) -> Comb rec err prec meta air nt cat o a -> Comb rec err prec meta air nt cat o b
withCST_ f (Comb c) = Comb c
  { rules = c.rules <#> \r@{ resultant: Resultant { length, result } } -> r
    { resultant = Resultant
      { length
      , accepting: mempty
      , result: \rec csts -> Result (f rec csts (\_  -> result rec csts))
      }
    }
  }

mapMaybe :: forall rec err prec meta air nt cat o a b. (a -> Maybe b) -> Comb rec err prec meta air nt cat o a -> Comb rec err prec meta air nt cat o b
mapMaybe f = mapEither $ note [] <<< f

-- | Soft rejection (prunes branches)
mapEither :: forall rec err prec meta air nt cat o a b. (a -> Either (Array err) b) -> Comb rec err prec meta air nt cat o a -> Comb rec err prec meta air nt cat o b
mapEither f = withCST' \_ _ da -> case da unit of
  Result a ->
    case f a of
      Left e -> Failed e
      Right r -> Result r
  Failed e -> Failed e
  Partial -> Partial

-- | Hard rejection (does not prune branches)
mapEither_ :: forall rec err prec meta air nt cat o a b. (a -> Either (Array err) b) -> Comb rec err prec meta air nt cat o a -> Comb rec err prec meta air nt cat o b
mapEither_ f = withCST'_ \_ _ da -> case da unit of
  Result a ->
    case f a of
      Left e -> Failed e
      Right r -> Result r
  Failed e -> Failed e
  Partial -> Partial

-- | Get access to the recursive parser machinery. (Advanced.)
withRec :: forall rec err prec meta air nt cat o a b. (rec -> a -> b) -> Comb rec err prec meta air nt cat o a -> Comb rec err prec meta air nt cat o b
withRec f = withCST' \rec _ prev -> f rec <$> prev unit

derive instance functorResultant :: Functor (Resultant err air r tok rec)
derive instance profunctorResultant :: Profunctor (Resultant err air r tok)
instance applyResultant :: Apply (Resultant err air r tok rec) where
  apply (Resultant l) (Resultant r) = Resultant
    { length: l.length + r.length
    , accepting: l.accepting <> shiftAccepting l.length r.accepting
    , result: \rec i ->
        let { before: li, after: ri } = splitAt l.length i in
        l.result rec li <*> r.result rec ri
    }
instance applicativeResultant :: Applicative (Resultant err air r tok rec) where
  pure a = Resultant { length: 0, accepting: mempty, result: \_ _ -> pure a }
instance compactableResultant :: Compactable (Resultant err air r tok rec) where
  compact (Resultant r) = Resultant r { result = map (map compact) r.result }
  separate eta = separateDefault eta

-- | Apply the resultant.
resultFrom :: forall err air r tok rec a. Resultant err air r tok rec a -> rec -> Array (ICST air r tok) -> Either (Array err) a
resultFrom (Resultant res) r i = case res.result r i of
  Result a -> Right a
  Failed e -> Left e
  Partial -> Left []

component :: forall err air r tok rec a. (rec -> ICST air r tok -> Either (Array err) a) -> Resultant err air r tok rec a
component c = Resultant
  { length: 1
  , accepting: mempty
  , result: \r is ->
      case head is of
        Nothing -> Partial
        Just i -> case c r i of
          Left err -> Failed err
          Right a -> Result a
  }
-- | components = traverse component
components :: forall err air r tok rec a. Array (rec -> ICST air r tok -> Either (Array err) a) -> Resultant err air r tok rec (Array a)
components cs = Resultant
  { length: length cs
  , accepting: mempty
  , result: \r is ->
      if length is < length cs then Partial else
        case sequence (zipWith identity (cs <@> r) is) of
          Left err -> Failed err
          Right a -> Result a
  }

derive instance functorPartialResult :: Functor (PartialResult err)
derive instance bifunctorPartialResult :: Bifunctor PartialResult
instance applyPartialResult :: Apply (PartialResult err) where
  apply (Failed e1) (Failed e2) = Failed (e1 <> e2)
  apply (Failed e) _ = Failed e
  apply _ (Failed e) = Failed e
  apply Partial _ = Partial
  apply _ Partial = Partial
  apply (Result f) (Result a) = Result (f a)
instance applicativePartialResult :: Applicative (PartialResult err) where
  pure = Result
-- instance bindPartialResult :: Bind (PartialResult err) where
--   bind (Failed e) _ = Failed e
--   bind Partial _ = Partial
--   bind (Result a) f = f a
-- instance monadPartialResult :: Monad (PartialResult err)
instance compactablePartialResult :: Compactable (PartialResult err) where
  compact (Result Nothing) = Failed []
  compact (Result (Just a)) = Result a
  compact (Failed e) = Failed e
  compact Partial = Partial
  separate eta = separateDefault eta
-- instance altResult :: Alt PartialResult where
--   alt Failed r = r
--   alt l Failed = l
--   alt Partial _ = Partial
--   alt l@(Result _) _ = l
