-- @inline export record arity=2
-- @inline export fields arity=2
-- @inline export tupled arity=1
-- @inline export adt arity=3
-- @inline export jsonError never
-- @inline export noneCodec always
-- @inline export profunctorOneCodec.dimap arity=3
-- @inline export profunctorMapCodec.dimap arity=3
-- @inline export profunctorArrCodec.dimap arity=3
-- @inline export profunctorArrDecodec.dimap arity=3
-- @inline export autoJSONRLNil(..).autoJRRL always
-- @inline export autoJSONRLNil(..).autoJVRL always
-- @inline export autoJSONRLCons(..).autoJRRL always
-- @inline export autoJSONRLCons(..).autoJVRL always
-- @inline export assocObjField(..).assoc arity=2
-- @inline export assocObjFieldCons(..).assoc arity=3
-- @inline export assocArrFieldCons(..).assoc arity=3
-- @inline export recfieldO arity=7
-- @inline export applicativeStar(..).pure arity=1

-- @inline export disjuxtOneCodec(..)._disjuxt2 arity=5
-- @inline export disjuxtOneCodec(..)._disjuxt0 arity=1
-- @inline export disjuxtArrCodec(..)._disjuxt2 arity=5
-- @inline export disjuxtArrCodec(..)._disjuxt0 arity=1
-- @inline export disjuxtMapCodec(..)._disjuxt2 arity=5
-- @inline export disjuxtMapCodec(..)._disjuxt0 arity=1
-- @inline export conjuxtArrCodec(..)._conjuxt2 arity=5
-- @inline export conjuxtArrCodec(..)._conjuxt0 arity=1
-- @inline export conjuxtMapCodec(..)._conjuxt2 arity=5
-- @inline export conjuxtMapCodec(..)._conjuxt0 arity=1

-- @inline export disjuxtOneCodec1 always
-- @inline export disjuxtOneCodec1 always
-- @inline export disjuxtArrCodec1 always
-- @inline export disjuxtArrCodec1 always
-- @inline export disjuxtMapCodec1 always
-- @inline export disjuxtMapCodec1 always
-- @inline export conjuxtArrCodec1 always
-- @inline export conjuxtArrCodec1 always
-- @inline export conjuxtMapCodec1 always
-- @inline export conjuxtMapCodec1 always

-- -- @inline export gitem arity=2
-- -- @inline export functorArrDecodec(..).map arity=2
-- -- @inline export applyArrDecodec(..).apply arity=2
module Parser.Printer.JSON where

import Prelude

import Control.Apply (lift2)
import Control.Comonad (class Comonad, class Extend, extract)
import Control.Monad.ST as ST
import Control.Plus (class Alt, class Plus, empty, (<|>))
import Data.Argonaut (JCursor(..), JsonDecodeError(..))
import Data.Argonaut as J
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Array.ST as STA
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Compactable (class Compactable, compact, separateDefault)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Filterable (class Filterable, filter, filterDefault, filterMap, partitionDefault, partitionMapDefault)
import Data.Identity (Identity(..))
import Data.Int as I
import Data.Int as Int
import Data.Lazy (defer)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First as M.F
import Data.Maybe.Last as M.L
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (class Newtype, over, un, unwrap)
import Data.Pair (Pair(..))
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Semigroup.First as S.F
import Data.Semigroup.Last as S.L
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty as NES
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.These (These)
import Data.Traversable (class Foldable, class Traversable, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Data.Variant (Variant)
import Data.Variant as Variant
import Data.Witherable (class Witherable, wiltDefault)
import Foreign.Object (Object)
import Foreign.Object as Obj
import Foreign.Object as Object
import Idiolect (type (/\/), JSON, only, (+?), (/|\), (?+))
import Parser.Printer.Juxt (class Conjuxt, class Disjuxt, class DisjuxtEnum, class GuideFlow, class IConjuxtRow, class IDisjuxtRow, class SelectVariantRow, CaseTree(..), _EitherRow, _MaybeL, _MaybeRow, _TheseRow, _disjuxt2, casesSplit, cleaveCases, conjuxt0, conjuxt2, disjuxt0, disjuxtEnum, iconjuxtRow, idisjuxtRow, rec0, recjuxt, selectVariant, var0, (!!!), (!>), (/!\), (>$<$>), (???), (\!/))
import Parser.Printer.Juxt as Juxt
import Parser.Selective (class Casing, applyCaseTree)
import Parser.Selective as Sel
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList as RL
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, from, to)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

data StarP f i o x y = StarP (i -> x) (x -> f y) (y -> o)
foreign import data Star :: (Type -> Type) -> Type -> Type -> Type
type role Star representational representational representational

_unStar :: forall f i o. Star f i o -> forall r. (forall x y. StarP f i o x y -> r) -> r
_unStar star f = f (unsafeCoerce star)

unStar :: forall f i o. Functor f => Star f i o -> i -> f o
unStar star v = _unStar star \(StarP i f o) -> v # i # f # map o

_mkStar :: forall f i o. forall x y. StarP f i o x y -> Star f i o
_mkStar = unsafeCoerce

mkStar :: forall f i o. (i -> f o) -> Star f i o
mkStar f = _mkStar (StarP identity f identity)

overStar :: forall f i o i' o'. Functor f => ((i -> f o) -> (i' -> f o')) -> Star f i o -> Star f i' o'
overStar f = mkStar <<< f <<< unStar

instance Functor (Star f i) where
  map o' star = _unStar star \(StarP i f o) -> _mkStar (StarP i f (o >>> o'))
instance profunctorStar :: Profunctor (Star f) where
  dimap i' o' star = _unStar star \(StarP i f o) -> _mkStar (StarP (i' >>> i) f (o >>> o'))
instance Apply f => Apply (Star f i) where
  apply l r = mkStar \i -> unStar l i <*> unStar r i
instance Applicative f => Applicative (Star f i) where
  pure a = mkStar \_ -> pure a
instance Casing f => Casing (Star f ctx) where
  caseTreeOn fi cases = mkStar \ctx ->
    Sel.caseTreeOn (unStar fi ctx) (cases # Sel.hoistCaseTree \f -> unStar f ctx)
instance Alt f => Alt (Star f i) where
  alt l r = mkStar \i -> unStar l i <|> unStar r i
instance Plus f => Plus (Star f i) where
  empty = mkStar \_ -> empty
instance Functor f => Disjuxt (Star f) where
  _disjuxt0 = mkStar
  _disjuxt2 i o1 o2 f g = mkStar \x -> i (unStar (rmap o1 f)) (unStar (rmap o2 g)) x

lstarCases ::
  forall f r p u x v y.
    Functor f =>
  (forall i unit. p i unit -> Star f i r) ->
  CaseTree p u x v y ->
  Tuple (v -> u) (Star f v r)
lstarCases p2s = case _ of
  ZeroCases vu _x -> Tuple (absurd <<< vu) $ mkStar $ vu >>> absurd
  OneCase vu pxy -> Tuple vu $ p2s pxy
  TwoCases cases -> Juxt.splitCases cases
    \(Juxt.CasesSplit wuv _jxy pixuz piyvz) ->
      let
        Tuple ui sui = lstarCases p2s pixuz
        Tuple vi svi = lstarCases p2s piyvz
      in Tuple (wuv >>> either ui vi) $ _mkStar $ StarP wuv (either (unStar sui) (unStar svi)) identity




-- | Defer all appends.
data RoseArray a = Array (Array a) | RoseArray Int (Array (RoseArray a))
derive instance Functor RoseArray
instance Monoid (RoseArray a) where
  mempty = Array []
instance Semigroup (RoseArray a) where
  append (Array []) r = r
  append l (Array []) = l
  append (Array l) (Array r) | Array.length l + Array.length r <= 20 = Array (l <> r)
  append (RoseArray _ []) r = r
  append l (RoseArray _ []) = l
  append (RoseArray l1 l) (RoseArray l2 r) | Array.length l + Array.length r <= 20 = RoseArray (l1 + l2) (l <> r)
  append l r = RoseArray (totallength l + totallength r) [l, r]
instance Alt RoseArray where alt = append
instance Plus RoseArray where empty = mempty
instance Apply RoseArray where
  apply (Array [f]) as = f <$> as
  apply fs (Array [a]) = fs <@> a
  apply (Array fs) as = RoseArray (Array.length fs * totallength as) $ fs <#> \f -> f <$> as
  apply (RoseArray l fs) as = RoseArray (l * totallength as) $ fs <#> (_ <*> as)
-- instance Bind RoseArray where
--   bind (Array [a]) f = f a
--   bind (Array as) f = RoseArray $ f <$> as
--   bind (RoseArray asz) f = RoseArray $ asz <#> (_ >>= f)
instance Applicative RoseArray where
  pure a = Array [a]
-- instance Monad RoseArray

-- | Apply the deferred appends.
flatten :: RoseArray ~> Array
flatten (Array as) = as
flatten (RoseArray _ []) = []
flatten (RoseArray _ [asz]) = flatten asz
flatten top = STA.run do
  accum <- STA.new
  let
    go (Array items) = ST.foreach items (\i -> void $ STA.push i accum)
    go (RoseArray _ branches) = ST.foreach branches go
  go top
  pure accum

totallength :: forall a. RoseArray a -> Int
totallength (Array as) = Array.length as
totallength (RoseArray l _) = l

-- | Tree of errors for applicative validation.
data ErrorTree e
  = ErrorLeaf e
  | ErrorSeq (NonEmptyArray (ErrorTree e))
  | ErrorPar (Array (ErrorTree e))
derive instance Functor ErrorTree

firstError :: ErrorTree ~> Maybe
firstError (ErrorLeaf e) = Just e
firstError (ErrorSeq es) = NEA.findMap firstError es
firstError (ErrorPar es) = Array.findMap firstError es

errorSeq :: forall e. ErrorTree e -> ErrorTree e -> ErrorTree e
errorSeq (ErrorSeq e1) (ErrorSeq e2) = ErrorSeq (e1 <> e2)
errorSeq (ErrorSeq e1) e2 = ErrorSeq (NEA.snoc e1 e2)
errorSeq e1 (ErrorSeq e2) = ErrorSeq (NEA.cons e1 e2)
errorSeq e1 e2 = ErrorSeq (NEA.cons e1 (pure e2))
errorPar :: forall e. ErrorTree e -> ErrorTree e -> ErrorTree e
errorPar (ErrorPar e1) (ErrorPar e2) = ErrorPar (e1 <> e2)
errorPar (ErrorPar e1) e2 = ErrorPar (Array.snoc e1 e2)
errorPar e1 (ErrorPar e2) = ErrorPar (Array.cons e1 e2)
errorPar e1 e2 = ErrorPar [e1, e2]

-- | CPSified applicative validation, with extra context for errors.
data VI i e a = VE (i -> ErrorTree e) | VA a
derive instance Functor (VI i e)
derive instance Bifunctor (VI i)

instance Apply (VI i e) where
  apply (VA f) (VA a) = VA (f a)
  apply (VA _) (VE e) = VE e
  apply (VE e) (VA _) = VE e
  apply (VE e1) (VE e2) = VE (errorSeq <$> e1 <*> e2)
instance Applicative (VI i e) where
  pure = VA
instance Alt (VI i e) where
  alt l@(VA _) _ = l
  alt _ r@(VA _) = r
  alt (VE e1) (VE e2) = VE (errorPar <$> e1 <*> e2)
instance Plus (VI i e) where
  empty = VE $ pure $ ErrorPar []
instance Casing (VI i e) where
  caseTreeOn (VA a) cases = applyCaseTree cases a
  caseTreeOn (VE e) cases = VE \i -> errorSeq (e i) $ ErrorPar $
    cases # Sel.summarizeCaseTree case _ of
      VA _ -> mempty
      VE f -> case f i of
        ErrorPar es -> es
        e2 -> [e2]
derive instance Foldable (VI i e)
derive instance Traversable (VI i e)
instance Compactable (VI i e) where
  compact (VE e) = VE e
  compact (VA Nothing) = empty
  compact (VA (Just a)) = VA a
  separate va = separateDefault va
instance Filterable (VI i e) where
  filterMap _ (VE e) = VE e
  filterMap f (VA a) = (empty ?+ VA) (f a)
  filter f = filterDefault f
  partitionMap f = partitionMapDefault f
  partition f = partitionDefault f
instance Witherable (VI i e) where
  wither _ (VE e) = pure (VE e)
  wither f (VA a) = (empty ?+ VA) <$> f a
  wilt = wiltDefault
instance Semigroup a => Semigroup (VI i e a) where append = lift2 append
instance Monoid a => Monoid (VI i e a) where mempty = pure mempty

mkErrorTree :: forall i e a. (i -> ErrorTree e) -> VI i e a
mkErrorTree = VE

mkError :: forall i e a. (i -> e) -> VI i e a
mkError f = VE (ErrorLeaf <<< f)

validated :: forall i e a. i -> VI i e a -> Either (ErrorTree e) a
validated _ (VA a) = Right a
validated i (VE e) = Left (e i)

staged :: forall i e a b. VI i e a -> (a -> VI i e b) -> VI i e b
staged (VE e) _ = VE e
staged (VA a) f = f a

rectx :: forall i' i e a. (i' -> i) -> VI i e a -> VI i' e a
rectx _ (VA a) = VA a
rectx contextualize (VE e) = VE (e <<< contextualize)

-- | CPSified applicative validation, with extra context for errors.
-- newtype VI i e a = VI (forall r. ((i -> ErrorTree e) -> r) -> (a -> r) -> r)
-- instance Functor (VI i e) where
--   map f (VI va) = VI \err suc -> va err (suc <<< f)
-- instance Bifunctor (VI i) where
--   bimap f g (VI va) = VI \err suc -> va (err <<< map (map f)) (suc <<< g)

-- instance Apply (VI i e) where
--   apply (VI vf) (VI va) = VI \err suc ->
--     case vf Left Right of
--       Left e -> va (\e' -> err (errorSeq <$> e <*> e')) (\_ -> err e)
--       Right f -> va err (suc <<< f)
-- instance Applicative (VI i e) where
--   pure a = VI \_err suc -> suc a
-- instance Alt (VI i e) where
--   alt (VI vf) (VI va) = VI \err suc ->
--     vf
--       do \e -> va (\e' -> err (errorPar <$> e <*> e')) suc
--       do suc
-- instance Plus (VI i e) where
--   empty = VI \err _ -> err \_ -> ErrorPar []
-- instance Casing (VI i e) where
--   caseTreeOn = caseTreeOnPlus
-- instance Foldable (VI i e) where
--   foldMap f (VI va) = va mempty f
--   foldl f z (VI va) = va (const z) (f z)
--   foldr f z (VI va) = va (const z) (f <@> z)
-- instance Traversable (VI i e) where
--   traverse f (VI va) = va (pure <<< mkErrorTree) (pure ==< f)
--   sequence = traverse identity
-- instance Compactable (VI i e) where
--   compact (VI va) = VI \err suc -> va err (suc +? err (const $ ErrorPar []))
--   separate va = separateDefault va
-- instance Filterable (VI i e) where
--   filterMap f (VI va) = VI \err suc -> va err (suc +? err (const $ ErrorPar []) <<< f)
--   filter f = filterDefault f
--   partitionMap f = partitionMapDefault f
--   partition f = partitionDefault f
-- instance Witherable (VI i e) where
--   wither f (VI va) = va
--     (pure <<< mkErrorTree)
--     (pure +? empty ==< f)
--   wilt = wiltDefault
-- instance Semigroup a => Semigroup (VI i e a) where append = lift2 append
-- instance Monoid a => Monoid (VI i e a) where mempty = pure mempty

-- mkErrorTree :: forall i e a. (i -> ErrorTree e) -> VI i e a
-- mkErrorTree f = VI \err _ -> err f

-- mkError :: forall i e a. (i -> e) -> VI i e a
-- mkError f = VI \err _ -> err (ErrorLeaf <<< f)

-- validated :: forall i e a. i -> VI i e a -> Either (ErrorTree e) a
-- validated i (VI va) = va (\e -> Left (e i)) Right

-- staged :: forall i e a b. VI i e a -> (a -> VI i e b) -> VI i e b
-- staged (VI va) f = VI \err suc -> va err \a ->
--   case f a of VI vb -> vb err suc

-- rectx :: forall i' i e a. (i' -> i) -> VI i e a -> VI i' e a
-- rectx contextualize (VI va) = VI \err suc -> va (err <<< lcmap contextualize) suc

data Register r a = Register (Set r) (Map r Int -> a)
derive instance Functor (Register r)
instance applyRegister :: Ord r => Apply (Register r) where
  apply (Register r1 rf) (Register r2 ra) = Register (r1 <> r2) (apply rf ra)
instance applicativeRegister :: Ord r => Applicative (Register r) where
  pure = Register Set.empty <<< pure
instance (Ord r, Semigroup m) => Semigroup (Register r m) where append = lift2 append
instance (Ord r, Monoid m) => Monoid (Register r m) where mempty = pure mempty

type Registering r = { len :: Int, rec :: List.List r, pos :: Map r Int }

startRegistering :: forall r. Registering r
startRegistering = { len: 0, rec: List.Nil, pos: Map.empty }

register :: forall r. Ord r => r -> RegisterM r Int
register r = RegisterM \state@{ len, rec, pos } ->
  case Map.lookup r pos of
    Just idx -> Tuple state idx
    Nothing -> Tuple
      { len: len + 1
      , rec: List.Cons r rec
      , pos: Map.insert r len pos
      } len

data RegisterM r a = RegisterM (Registering r -> Tuple (Registering r) a)
derive instance Functor (RegisterM r)
instance applyRegisterM :: Ord r => Apply (RegisterM r) where
  apply (RegisterM rf) (RegisterM ra) = RegisterM \s0 ->
    case rf s0 of
      Tuple s1 f -> case ra s1 of
        Tuple s2 a -> Tuple s2 (f a)
instance applicativeRegisterM :: Ord r => Applicative (RegisterM r) where
  pure a = RegisterM \s -> Tuple s a
instance bindRegisterM :: Ord r => Bind (RegisterM r) where
  bind (RegisterM ra) f = RegisterM \s ->
    case ra s of
      Tuple s' a -> case f a of
        RegisterM f' -> f' s'
instance Ord r => Monad (RegisterM r)
instance (Ord r, Semigroup m) => Semigroup (RegisterM r m) where append = lift2 append
instance (Ord r, Monoid m) => Monoid (RegisterM r m) where mempty = pure mempty

-- instance Ord r => Parallel (Register r) (RegisterM r) where
--   sequential (Register items result) = RegisterM \existing -> do
--     added <- scanl (\x _ -> x + 1) (Map.size existing) $ Set.toMap items
--     let current = existing <|> added
--     pure $ Tuple current (result current)
--   parallel (RegisterM act) =
--     let Tuple added result = act
--     Register

data Registered r a = Registered (Array r) a
derive instance Functor (Registered r)
instance Extend (Registered r) where
  extend f r@(Registered rs _) = Registered rs (f r)
instance Comonad (Registered r) where
  extract (Registered _ a) = a
instance (AutoJSON r reg, AutoJSON a reg) => AutoJSON (Registered r a) reg where
  autoJ = (\(Registered rs a) -> Tuple rs a) >$<$> uncurry Registered
    !!! object
    !!! "cached" := autoJ
    /!\ "data" := autoJ

-- `arr` requires `Monad f`
starr :: forall f i o. Applicative f => (i -> o) -> Star f i o
starr f = mkStar (pure <<< f)


-- | A bidirectional codec with a single target (e.g. `JSON`), meaning that it
-- | supports disjunction but not conjunction.
data OneCodec ctx err reg s t i o = OneCodec
  (Star (RegisterM reg) i t)
  (Star (VI ctx err) (Registered reg s) o)

composeCodec :: forall ctx err reg s t x y i o. Ord reg => OneCodec ctx err reg y x i o -> OneCodec ctx err reg s t x y -> OneCodec ctx err reg s t i o
composeCodec (OneCodec f g) (OneCodec h k) = OneCodec
  do mkStar \i -> unStar f i >>= unStar h
  do mkStar \s@(Registered items _) -> staged (unStar k s) \y -> unStar g (Registered items y)

infixr 8 composeCodec as <~<

oneCodec :: forall ctx err reg s t i o. (i -> RegisterM reg t) -> (Registered reg s -> VI ctx err o) -> OneCodec ctx err reg s t i o
oneCodec f g = OneCodec (mkStar f) (mkStar g)

noneCodec :: forall ctx err reg s t. OneCodec ctx err reg s t t s
noneCodec =
  let
    noEncoding = \i -> RegisterM (Tuple <@> i)
    noDecoding = \(Registered _ s) -> pure s
  in oneCodec noEncoding noDecoding

noRegister :: forall ctx err reg s t i o. (i -> t) -> (s -> VI ctx err o) -> OneCodec ctx err reg s t i o
noRegister it so = oneCodec (RegisterM <<< (\i s -> Tuple s (it i))) (so <<< extract)

unRegistered :: forall ctx err s t i o. OneCodec ctx err Void s t i o -> Tuple (i -> t) (s -> VI ctx err o)
unRegistered (OneCodec f g) = Tuple
  (unStar f >>> \(RegisterM withRegistered) -> extract $ withRegistered startRegistering)
  (unStar g <<< Registered [])

derive instance Functor (OneCodec ctx err reg s t i)
derive instance profunctorOneCodec :: Profunctor (OneCodec ctx err reg s t)
instance (Ord reg, Semigroup t, Semigroup o) => Semigroup (OneCodec ctx err reg s t i o) where
  append (OneCodec f1 g1) (OneCodec f2 g2) = OneCodec (lift2 append f1 f2) (lift2 append g1 g2)
instance (Ord reg, Monoid t, Monoid o) => Monoid (OneCodec ctx err reg s t i o) where
  mempty = OneCodec (pure mempty) (pure mempty)

instance disjuxtOneCodec :: Disjuxt (OneCodec ctx err reg s t) where
  _disjuxt0 f = OneCodec (mkStar f) empty
  _disjuxt2 i o1 o2 (OneCodec f1 g1) (OneCodec f2 g2) = OneCodec
    (_disjuxt2 i identity identity f1 f2) (o1 <$> g1 <|> o2 <$> g2)
instance Alt (OneCodec ctx err reg s t i) where
  alt (OneCodec f1 g1) (OneCodec _ g2) = OneCodec f1 (starVI g1 g2)

-- | A decodec with multiple outputs, meaning that it supports disjunction
-- | and conjunction.
newtype ArrDecodec ctx err reg s o = ArrDecodec
  (Map Int {- length -} (Star (VI ctx err) (Registered reg (Tuple Int {- offset -} (Array s))) o))
derive instance Newtype (ArrDecodec ctx err reg s o) _
derive instance functorArrDecodec :: Functor (ArrDecodec ctx err reg s)
derive instance profunctorArrDecodec :: Profunctor (ArrDecodec ctx err reg)

starVI :: forall ctx err i o. Star (VI ctx err) i o -> Star (VI ctx err) i o -> Star (VI ctx err) i o
starVI f g = mkStar \i -> case unStar f i of
  VA a -> VA a
  VE e -> case unStar g i of
    VA a -> VA a
    VE e' -> VE e <|> VE e'

instance applyArrDecodec :: Apply (ArrDecodec ctx err reg s) where
  apply (ArrDecodec g1s) (ArrDecodec g2s) = ArrDecodec $
    mapProduct g1s g2s \(Tuple l1 g1) (Tuple l2 g2) ->
      Tuple (l1 + l2) (g1 <*> shift l1 g2)
    where
    shift amount = (lcmap <<< map <<< lmap) (_ + amount)
    -- Cartesian product of maps
    mapProduct m1 m2 f = Map.fromFoldableWith starVI $
      (f <$> Map.toUnfoldable m1 <*> Map.toUnfoldable m2 :: Array _)
instance applicativeArrDecodec :: Applicative (ArrDecodec ctx err reg s) where
  pure o = ArrDecodec $ Map.singleton 0 $ pure o
instance Alt (ArrDecodec ctx err reg s) where
  alt (ArrDecodec g1s) (ArrDecodec g2s) = ArrDecodec $
    Map.unionWith starVI g1s g2s
instance Plus (ArrDecodec ctx err reg s) where
  empty = ArrDecodec Map.empty
instance Compactable (ArrDecodec ctx err reg s) where
  compact = (over ArrDecodec <<< map <<< overStar <<< map) compact
  separate gs = separateDefault gs
instance Filterable (ArrDecodec ctx err reg s) where
  filterMap = (over ArrDecodec <<< map <<< overStar <<< map) <<< filterMap
  filter = (over ArrDecodec <<< map <<< overStar <<< map) <<< filter
  partition f = partitionDefault f
  partitionMap f = partitionMapDefault f
instance Casing (ArrDecodec ctx err reg s) where
  caseTreeOn (ArrDecodec g1s) cases = ArrDecodec
    let
      shift :: Int ->
        Registered reg (Tuple Int (Array s)) ->
        Registered reg (Tuple Int (Array s))
      shift amount = (map <<< lmap) (_ + amount)
      g2s :: Map Int (Array (Sel.Accepting (Star _ _) _ _))
      g2s = un SemigroupMap $ Sel.onCaseTreeBranches Just cases \f (ArrDecodec gs) ->
        SemigroupMap $ gs <#> \g -> [Sel.accepts f g]
    in mapProduct g1s g2s \(Tuple l1 g1) (Tuple l2 branches) ->
      Tuple (l1 + l2) $ mkStar \s -> case unStar g1 s of
        VE e -> VE \i -> errorSeq (e i) $ ErrorPar $ branches >>=
          \branch -> Sel.accepting branch \_ g2 ->
            case unStar g2 (shift l1 s) of
              VE e2 -> [e2 i]
              _ -> []
        VA a1 -> Array.foldr (<|>) empty $ branches >>=
          \branch -> Sel.accepting branch \f g2 ->
            case unStar g2 (shift l1 s) of
              VA a2 -> case f a1 of
                Just y -> [VA (a2 y)]
                Nothing -> []
              VE e -> [VE e]
    where
    mapProduct m1 m2 f = Map.fromFoldableWith starVI $
      (f <$> Map.toUnfoldable m1 <*> Map.toUnfoldable m2 :: Array _)
instance Semigroup o => Semigroup (ArrDecodec ctx err reg s o) where append = lift2 append
instance Monoid o => Monoid (ArrDecodec ctx err reg s o) where mempty = pure mempty

-- | A bidirectional codec with multiple outputs, meaning that it supports
-- | conjunction in addition to disjunction.
data ArrCodec ctx err reg s t i o = ArrCodec
  (Star (RegisterM reg) i (RoseArray t))
  (ArrDecodec ctx err reg s o)

gitem :: forall ctx' ctx err reg s t i o.
  (Int -> ctx -> ctx') ->
  OneCodec ctx' err reg s t i o ->
  ArrCodec ctx err reg s t i o
gitem contextualize (OneCodec f g) = ArrCodec (map pure f) $ ArrDecodec $
  Map.singleton 1 $ mkStar case _ of
    Registered reg (Tuple idx items)
      | Just s <- items Array.!! idx ->
        rectx (contextualize idx) $ unStar g (Registered reg s)
    _ -> empty

-- | A codec for a single item.
item :: forall reg i o. JRCodec' reg i o -> JRACodec' reg i o
item = gitem JIndex

itemConjuxt :: forall reg i1 i2 o1 o2. Ord reg => JRCodec' reg i1 o1 -> JRACodec' reg i2 o2 -> JRACodec' reg (Tuple i1 i2) (Tuple o1 o2)
itemConjuxt c1 c2 = item c1 /!\ c2

infixr 2 itemConjuxt as <|



-- example :: forall i1 o1 i2 o2 i3 o3.
--   JCodec' i1 o1 -> JCodec' i2 o2 -> JCodec' i3 o3 ->
--   JACodec' (i1 /\ i2 /\ i3 /\ Unit) (o1 /\ o2 /\ o3 /\ Unit)
-- example c1 c2 c3 = c1 <| c2 <| c3 <| conjuxt0

derive instance Functor (ArrCodec ctx err reg s t i)
derive instance profunctorArrCodec :: Profunctor (ArrCodec ctx err reg s t)

instance conjuxtArrCodec :: Ord reg => Conjuxt (ArrCodec ctx err reg s t) where
  _conjuxt0 x = ArrCodec (pure empty) (pure x)
  _conjuxt2 i1 i2 o (ArrCodec f1 g1s) (ArrCodec f2 g2s) =
    ArrCodec (lift2 append (lcmap i1 f1) (lcmap i2 f2)) (o <$> g1s <*> g2s)

instance disjuxtArrCodec :: Ord reg => Disjuxt (ArrCodec ctx err reg s t) where
  _disjuxt0 f = ArrCodec (mkStar f) empty
  _disjuxt2 i o1 o2 (ArrCodec f1 g1s) (ArrCodec f2 g2s) =
    ArrCodec (_disjuxt2 i identity identity f1 f2) (o1 <$> g1s <|> o2 <$> g2s)

instance Ord reg => Apply (ArrCodec ctx err reg s t i) where
  apply f a = dimap (join Tuple) (uncurry ($)) $ conjuxt2 f a
instance Ord reg => Applicative (ArrCodec ctx err reg s t i) where
  pure a = dimap (const unit) (const a) conjuxt0
-- | Uses the left encoder
instance Ord reg => Alt (ArrCodec ctx err reg s t i) where
  alt (ArrCodec f1 g1s) (ArrCodec _ g2s) = ArrCodec f1 (g1s <|> g2s)
instance Ord reg => GuideFlow (ArrCodec ctx err reg s t) where
  branchCases (ArrCodec fi gjs) cases =
    let
      Tuple _ bwd = cleaveCases
        (\(ArrCodec _ grs) -> grs)
        (TwoCases (casesSplit cases))
      Tuple fwd fwt = lstarCases
        (\(ArrCodec f _) -> f)
        (TwoCases (casesSplit cases))
    in ArrCodec (lift2 (<|>) (lcmap fwd fi) fwt) (Sel.caseTreeOn gjs bwd)
instance (Ord reg, Semigroup o) => Semigroup (ArrCodec ctx err reg s t i o) where append = lift2 append
instance (Ord reg, Monoid o) => Monoid (ArrCodec ctx err reg s t i o) where mempty = pure mempty

-- | A bidirectional codec with keyed outputs, meaning that it supports
-- | conjunction in addition to disjunction.
newtype MapCodec k ctx err reg s t i o =
  MapCodec (OneCodec ctx err reg (Map k s) (Map k t) i o)
derive instance Newtype (MapCodec k ctx err reg s t i o) _
type ObjCodec = MapCodec String

derive instance Functor (MapCodec k ctx err reg s t i)
derive instance profunctorMapCodec :: Profunctor (MapCodec k ctx err reg s t)

-- | Left biased encoding for overlapping keys
instance conjuxtMapCodec :: (Ord k, Ord reg) => Conjuxt (MapCodec k ctx err reg s t) where
  _conjuxt0 x = MapCodec $ OneCodec (pure Map.empty) (pure x)
  _conjuxt2 i1 i2 o (MapCodec (OneCodec f1 g1)) (MapCodec (OneCodec f2 g2)) =
    MapCodec $ OneCodec
      do lift2 (<|>) (lcmap i1 f1) (lcmap i2 f2)
      do o <$> g1 <*> g2

-- | Left biased decoding for overlapping matches
derive newtype instance disjuxtMapCodec :: (Ord k, Ord reg) => Disjuxt (MapCodec k ctx err reg s t)

instance (Ord k, Ord reg) => GuideFlow (MapCodec k ctx err reg s t) where
  branchCases (MapCodec (OneCodec fi gj)) cases =
    let
      Tuple _ bwd = cleaveCases
        (\(MapCodec (OneCodec _ g)) -> g)
        (TwoCases (casesSplit cases))
      Tuple fwd fwt = lstarCases
        (\(MapCodec (OneCodec f _)) -> f)
        (TwoCases (casesSplit cases))
    in MapCodec $ OneCodec (lift2 (<|>) (lcmap fwd fi) fwt) (Sel.caseTreeOn gj bwd)

instance (Ord k, Ord reg) => Apply (MapCodec k ctx err reg s t i) where
  apply f a = dimap (join Tuple) (uncurry ($)) $ conjuxt2 f a
instance (Ord k, Ord reg) => Applicative (MapCodec k ctx err reg s t i) where
  pure a = dimap (const unit) (const a) conjuxt0
-- | Uses the left encoder
derive newtype instance Ord reg => Alt (MapCodec k ctx err reg s t i)

gfield :: forall k ctx' ctx err reg s t i o. Ord k =>
  (k -> ctx -> ctx') ->
  k -> OneCodec ctx' err reg s t i o ->
  MapCodec k ctx err reg s t i o
gfield contextualize k (OneCodec f g) =
  MapCodec $ OneCodec (map (Map.singleton k) f) $
    mkStar case _ of
      Registered reg jsonFields
        | Just s <- Map.lookup k jsonFields ->
          rectx (contextualize k) $ unStar g (Registered reg s)
      _ -> empty

-- | A single field, with its static key and a codec for its value.
field :: forall reg i o. String -> JRCodec' reg i o -> JROCodec' reg i o
field = gfield JField


class Assoc s i o where
  assoc :: s -> i -> o

infix 8 assoc as :=

instance assocObjField :: (TypeEquals part (JRCodec' reg i o)) => Assoc String part (JROCodec' reg i o) where
  assoc name part = field name (to part)


-- | A single field, with its static key and a codec for its value.
recfieldO ::
  forall @s @a @b reg ra' ra rb' rb.
    IsSymbol s =>
    Row.Cons s a ra' ra =>
    Row.Cons s b rb' rb =>
    Row.Lacks s rb' =>
    Ord reg =>
  Proxy s -> JRCodec' reg a b -> JROCodec' reg (Record ra) (Record rb') -> JROCodec' reg (Record ra) (Record rb)
recfieldO s = recjuxt @s <<< field (reflectSymbol s)

instance assocObjFieldCons ::
  ( TypeEquals part (JRCodec' reg a b)
  , TypeEquals asdf (JROCodec' reg (Record ra) (Record rb'))
  , TypeEquals (ObjCodec a2 b2 c2 d2 e2 f2 g2) (JROCodec' reg (Record ra) (Record rb))
  , IsSymbol s
  , Row.Cons s a ra' ra
  , Row.Cons s b rb' rb
  , Row.Lacks s rb'
  , Ord reg
  ) => Assoc (Proxy s) part (asdf -> ObjCodec a2 b2 c2 d2 e2 f2 g2) where
    assoc s part = dimap to from $ recfieldO s (to part)

instance assocArrFieldCons ::
  ( TypeEquals part (JRCodec' reg a b)
  , TypeEquals asdf (JRACodec' reg (Record ra) (Record rb'))
  , TypeEquals (ArrCodec a2 b2 c2 d2 e2 f2 g2) (JRACodec' reg (Record ra) (Record rb))
  , IsSymbol s
  , Row.Cons s a ra' ra
  , Row.Cons s b rb' rb
  , Row.Lacks s rb'
  , Ord reg
  ) => Assoc (Proxy s) part (asdf -> ArrCodec a2 b2 c2 d2 e2 f2 g2) where
    assoc _ part = dimap to from $ recjuxt @s (item (to part))

type OneJRCodec' = OneCodec JCursor JsonDecodeError
type OneJCodec' = OneJRCodec' JSON
type Conv x y = (forall reg. OneJRCodec' reg x x y y)

type JCodec' = OneJCodec' JSON JSON
type JCodec t = JCodec' t t
type JACodec' = ArrCodec JCursor JsonDecodeError JSON JSON JSON
type JACodec t = JACodec' t t
type JOCodec' = ObjCodec JCursor JsonDecodeError JSON JSON JSON
type JOCodec t = JOCodec' t t

type JRCodec' reg = OneJRCodec' reg JSON JSON
type JRCodec reg t = JRCodec' reg t t
type JRACodec' reg = ArrCodec JCursor JsonDecodeError reg JSON JSON
type JRACodec reg t = JRACodec' reg t t
type JROCodec' reg = ObjCodec JCursor JsonDecodeError reg JSON JSON
type JROCodec reg t = JROCodec' reg t t

-- encode :: forall i o. JCodec' i o -> i -> JSON
encode :: forall ctx err s t i o. OneCodec ctx err Void s t i o -> i -> t
encode (OneCodec enc _) i =
  let RegisterM f = unStar enc i in extract $ f startRegistering

encodeR :: forall i o. JCodec' i o -> i -> JSON
encodeR = encode <<< registration autoJ

decode' :: forall ctx err s t i o. Monoid ctx => OneCodec ctx err Void s t i o -> s -> Either (ErrorTree err) o
decode' (OneCodec _ dec) json =
  validated (mempty :: ctx) $ unStar dec $ Registered [] json

decode :: forall i o. JRCodec' Void i o -> JSON -> Either JsonDecodeError o
decode codec json =
  lmap (fromMaybe (UnexpectedValue json) <<< firstError) $ decode' codec json

decodeR :: forall i o. JCodec' i o -> JSON -> Either JsonDecodeError o
decodeR = decode <<< registration autoJ

type JV = VI JCursor JsonDecodeError
jsonError :: forall a. JsonDecodeError -> JV a
jsonError e0 = mkError \i -> go i e0
  where
  go :: JCursor -> JsonDecodeError -> JsonDecodeError
  go JCursorTop e = e
  go (JIndex idx c) e = go c (AtIndex idx e)
  go (JField name c) e = go c (AtKey name e)

-- registration' :: forall i o.
--   (Array JSON -> JSON -> JSON) ->
--   (JSON -> JV (Registered JSON JSON)) ->
--   JCodec' i o ->
--   JCodec' i o
registration' ::
  forall ctx err reg noReg s t s' t' i o.
  (Array reg -> t -> t') ->
  (s' -> VI ctx err (Registered reg s)) ->
  OneCodec ctx err reg s t i o ->
  OneCodec ctx err noReg s' t' i o
registration' bundle unbundle (OneCodec enc dec) = noRegister
  (enc' <<< unStar enc)
  (\i -> staged (unbundle i) (unStar dec))
  where
  enc' (RegisterM stateful) =
    let Tuple { rec } result = stateful startRegistering in
    bundle (Array.reverse $ List.toUnfoldable rec) result
    -- bundle (Set.toUnfoldable items) $ withIndices $
    --   scanl (\x _ -> x + 1) 0 $ Set.toMap items

-- registration :: forall i o. JCodec (Registered JSON JSON) -> JCodec' i o -> JCodec' i o
registration ::
  forall ctx err reg noMoreReg s t s' t' i o.
  OneCodec ctx err Void s' t' (Registered reg t) (Registered reg s) ->
  OneCodec ctx err reg s t i o ->
  OneCodec ctx err noMoreReg s' t' i o
registration regCodec =
  let Tuple enc dec = unRegistered regCodec in
  registration' (\reg json -> enc (Registered reg json)) dec

jsonPrimCodec :: forall reg i o. String -> (JSON -> Maybe o) -> (i -> JSON) -> JRCodec' reg i o
jsonPrimCodec ty dec enc = noRegister enc (dec >>> pure +? jsonError (TypeMismatch ty))

exactly :: forall reg. JSON -> JRCodec reg Unit
exactly value = jsonPrimCodec "exact" (\v -> if v == value then Just unit else Nothing) (const value)

tag :: forall @t reg. AutoJSON t Void => t -> JRCodec reg Unit
tag = exactly <<< encode autoJ

tag_ :: forall @t reg. AutoJSON t Void => t -> JRACodec reg Unit
tag_ = item <<< tag

ctorAs :: String -> forall @name reg. JRACodec reg (Proxy name)
ctorAs renamed = dimap one one (tag_ renamed)

ctor :: forall @name reg. IsSymbol name => JRACodec reg (Proxy name)
ctor = ctorAs (reflectSymbol (Proxy @name))

cached :: forall t. JCodec t -> JCodec t
cached (OneCodec enc dec) = oneCodec
  do \t -> do
      encoded <- unStar enc t
      register encoded <#> \pos -> J.fromNumber $ Int.toNumber pos
  do \(Registered items json) ->
      jsonError (UnexpectedValue json) ?+ (unStar dec <<< Registered items) $
        J.toNumber json >>= Int.fromNumber >>= Array.(!!) items

class Ord reg <= TryCached reg where
  tryCached :: forall t. JRCodec reg t -> JRCodec reg t
instance doesCache :: TryCached JSON where
  tryCached = cached
instance doesn'tCache :: TryCached Void where
  tryCached = identity

prismatic :: forall reg i o j u. String -> (i -> j) -> (u -> Maybe o) -> JRCodec' reg j u -> JRCodec' reg i o
prismatic name f g (OneCodec enc dec) =
  OneCodec (lcmap f enc) $ mkStar \j ->
    let err = jsonError $ Named name $ UnexpectedValue $ extract j in
    staged (unStar dec j) $ g >>> pure +? err

-- named :: forall i o. String -> JCodec' i o -> JCodec' i o
-- named name (OneCodec enc dec) = OneCodec enc (hoistStar (rectx (Named name)) dec)

fix :: forall ctx err reg s t i o.
  (OneCodec ctx err reg s t i o -> OneCodec ctx err reg s t i o) ->
  OneCodec ctx err reg s t i o
fix mk = codec
  where
  codec = OneCodec
    (mkStar \i -> unStar (extract enc) i)
    (mkStar \j -> unStar (extract dec) j)
  enc = defer \_ -> let OneCodec f _ = mk codec in f
  dec = defer \_ -> let OneCodec _ g = mk codec in g

-- | A codec for `null` values in `JSON`.
null :: forall reg. JRCodec reg Unit
null = jsonPrimCodec "Null" J.toNull (const J.jsonNull)

nullable :: forall reg i o. JRCodec' reg i o -> JRCodec' reg (Maybe i) (Maybe o)
nullable codec = _MaybeL !!! null \!/ codec

-- | A codec for `Boolean` values in `JSON`.
boolean :: forall reg. JRCodec reg Boolean
boolean = jsonPrimCodec "Boolean" J.toBoolean J.fromBoolean

-- | A codec for `Number` values in `JSON`.
number :: forall reg. JRCodec reg Number
number = jsonPrimCodec "Number" J.toNumber J.fromNumber

-- | A codec for `Int` values in `JSON`.
int :: forall reg. JRCodec reg Int
int = jsonPrimCodec "Int" (I.fromNumber <=< J.toNumber) (J.fromNumber <<< I.toNumber)

-- | Represent `Int`s as JSON strings.
intS :: forall reg. JRCodec reg Int
intS = prismatic "IntAsString" show Int.fromString string

-- | A codec for `String` values in `JSON`.
string :: forall reg. JRCodec reg String
string = jsonPrimCodec "String" J.toString J.fromString

nestring :: forall reg. JRCodec reg NES.NonEmptyString
nestring = prismatic "NonEmptyString" NES.toString NES.fromString string

-- | A codec for `Codepoint` values in `JSON`.
codePoint :: forall reg. JRCodec reg S.CodePoint
codePoint = jsonPrimCodec "CodePoint" (only <<< S.toCodePointArray <=< J.toString) (J.fromString <<< S.singleton)

-- | A codec for `Array Codepoint` values in `JSON`.
codePoints :: forall reg. JRCodec reg (Array S.CodePoint)
codePoints = dimap S.fromCodePointArray S.toCodePointArray string

-- | A codec for `Char` values in `JSON`.
char :: forall reg. JRCodec reg Char
char = jsonPrimCodec "Char" (SCU.toChar <=< J.toString) (J.fromString <<< SCU.singleton)

-- | A codec for homogeneous `Array` values in `JSON`.
array :: forall reg i o. Ord reg => JRCodec' reg i o -> JRCodec' reg (Array i) (Array o)
array (OneCodec enc dec) = oneCodec
  (map J.fromArray <<< traverse (unStar enc))
    \(Registered rs json) ->
      case J.toArray json of
        Just encoded -> traverseWithIndex (\i -> rectx (JIndex i) <<< unStar dec <<< Registered rs) encoded
        Nothing -> jsonError (TypeMismatch "Array")

nearray :: forall reg i o. Ord reg => JRCodec' reg i o -> JRCodec' reg (NEA.NonEmptyArray i) (NEA.NonEmptyArray o)
nearray = prismatic "NonEmptyArray" NEA.toArray NEA.fromArray <<< array

-- | Assemble codecs for heterogeneous `Array` values in `JSON`.
tupled :: forall reg i o. JRACodec' reg i o -> JRCodec' reg i o
tupled (ArrCodec f codecs) =
  OneCodec (map (J.fromArray <<< flatten) f) $
    mkStar \(Registered reg json) ->
      case J.toArray json of
        Just encoded
          | Just g <- Map.lookup (Array.length encoded) (unwrap codecs) ->
            unStar g (Registered reg (Tuple 0 encoded))
          | otherwise -> jsonError (UnexpectedValue json)
        Nothing -> jsonError (TypeMismatch "Array")

-- | A codec for `Tuple` values in `JSON`, using a two-element array.
tuple :: forall reg i1 i2 o1 o2. Ord reg => JRCodec' reg i1 o1 -> JRCodec' reg i2 o2 -> JRCodec' reg (Tuple i1 i2) (Tuple o1 o2)
tuple c1 c2 = tupled (item c1 /!\ item c2)

infixr 8 tuple as @/\

-- | A codec for homogeneous `Object` values in `JSON`.
keyed :: forall reg i o. Ord reg => JRCodec' reg i o -> JRCodec' reg (Object i) (Object o)
keyed (OneCodec enc dec) = oneCodec
  (map J.fromObject <<< traverse (unStar enc)) $
    \(Registered rs json) ->
      case J.toObject json of
        Just encoded -> traverseWithIndex (\k -> rectx (JField k) <<< unStar dec <<< Registered rs) encoded
        Nothing -> jsonError (TypeMismatch "Object")

-- | Assemble codecs for heterogeneous `Object` values in `JSON`.
object :: forall reg i o. JROCodec' reg i o -> JRCodec' reg i o
object (MapCodec (OneCodec f g)) =
  OneCodec (map (J.fromObject <<< Obj.fromFoldableWithIndex) f) $
    mkStar \(Registered reg json) ->
      case Map.fromFoldableWithIndex <$> J.toObject json of
        Just encoded -> unStar g (Registered reg encoded)
        Nothing -> jsonError (TypeMismatch "Object")

-- | Assemble codecs for a record in `JSON`.
record ::
  forall reg cs is os.
    IConjuxtRow cs (JRCodec' reg) (JROCodec' reg) is os =>
  Record cs -> JRCodec' reg (Record is) (Record os)
record = object <<< fields

fields ::
  forall reg cs is os.
    IConjuxtRow cs (JRCodec' reg) (JROCodec' reg) is os =>
  Record cs -> JROCodec' reg (Record is) (Record os)
fields = iconjuxtRow field

-- example :: JCodec { null :: Unit, string :: String }
-- example = record { null: null, string: string }

simpleVariant ::
  forall reg cs is os.
    Ord reg =>
    IDisjuxtRow cs (JRCodec' reg) (JROCodec' reg) is os =>
  Record cs -> JRCodec' reg (Variant is) (Variant os)
simpleVariant =
  object <<< idisjuxtRow \name (codec :: JRCodec' reg _ _) ->
    { type: unit, data: _ } >$<$> _.data
    !!! fields { type: tag name, data: codec }

-- example :: JCodec (Variant ( int :: Int, string :: String ))
-- example = simpleVariant { int: int, string: string }

variant ::
  forall reg cases enum is os.
    Ord reg =>
    DisjuxtEnum (JRCodec' reg) enum =>
    SelectVariantRow (JROCodec' reg) cases enum is os =>
  Record cases -> JRCodec' reg (Variant is) (Variant os)
variant cases = object $
  "type" := disjuxtEnum @_ @enum (\name -> exactly (J.fromString name))
  ??? selectVariant cases

-- example :: JCodec (Variant ( int :: Int, string :: String ))
-- example = variant { int: "int" := int, string: "string" := string }

-- example ::
--   forall int string.
--   { int :: JACodec int
--   , string :: JACodec string
--   } -> JACodec
--   (Variant ( int :: Tuple Int int, string :: Tuple String string ))
-- example cases =
--   item do variant { int: "int" := int, string: "string" := string }
--   ??? caseVariant cases

adt ::
  forall reg cases enum is os.
    Ord reg =>
    DisjuxtEnum (JRCodec' reg) enum =>
    SelectVariantRow (JRACodec' reg) cases enum is os =>
  Record cases -> JRCodec' reg (Variant is) (Variant os)
adt cases = tupled $
  item do disjuxtEnum @_ @enum (\name -> exactly (J.fromString name))
  ??? selectVariant cases

-- null | [X]
maybeA :: forall reg i o. JRCodec' reg i o -> JRCodec' reg (Maybe i) (Maybe o)
maybeA codec = _MaybeL !!! null \!/ tupled (item codec)

-- { type: "Nothing" } | { type: "Just", data: X }
maybeO :: forall reg i o. Ord reg => JRCodec' reg i o -> JRCodec' reg (Maybe i) (Maybe o)
maybeO codec = _MaybeRow !!! variant { "Nothing": conjuxt0, "Just": "data" := codec }

-- { type: "Left", data: L } | { type: "Right", data: R }
eitherO :: forall reg i1 o1 i2 o2. Ord reg => JRCodec' reg i1 o1 -> JRCodec' reg i2 o2 -> JRCodec' reg (i1 \/ i2) (o1 \/ o2)
eitherO codecL codecR = _EitherRow !!! simpleVariant { "Left": codecL, "Right": codecR }

-- ["L", L] | ["R", R]
eitherA :: forall reg i1 o1 i2 o2. Ord reg => JRCodec' reg i1 o1 -> JRCodec' reg i2 o2 -> JRCodec' reg (i1 \/ i2) (o1 \/ o2)
eitherA codecL codecR = tupled
  !!! tag_ "L" !> item codecL
  \!/ tag_ "R" !> item codecR

infixr 8 eitherA as @\/

-- { type: "This", data: L } | { type: "That", data: R } | { type: "Both", data: [L, R] }
theseO :: forall reg i1 o1 i2 o2. Ord reg => JRCodec' reg i1 o1 -> JRCodec' reg i2 o2 -> JRCodec' reg (i1 /\/ i2) (o1 /\/ o2)
theseO codecL codecR = _TheseRow !!! simpleVariant
  { "This": codecL, "That": codecR
  , "Both": tupled
    !!! Proxy @"left" := codecL
    !!! Proxy @"right" := codecR
    !!! rec0
  }

-- ["This", L] | ["That", R] | ["Both", L, R]
theseA :: forall reg i1 o1 i2 o2. Ord reg => JRCodec' reg i1 o1 -> JRCodec' reg i2 o2 -> JRCodec' reg (i1 /\/ i2) (o1 /\/ o2)
theseA codecL codecR = _TheseRow !!! adt
  { "This": item codecL, "That": item codecR
  , "Both": identity
    !!! Proxy @"left" := codecL
    !!! Proxy @"right" := codecR
    !!! rec0
  }

infixr 8 theseA as @/\/

set :: forall reg @a. Ord a => Ord reg => JRCodec reg a -> JRCodec reg (Set a)
set = dimap Set.toUnfoldable Set.fromFoldable <<< array

mapO :: forall reg @k @v. Ord k => JRCodec Void k -> JRCodec Void v -> JRCodec reg (Map k v)
mapO key value = noRegister enc dec
  where
  Tuple keyEnc keyDec = unRegistered key
  Tuple valueEnc valueDec = unRegistered value

  enc = dimap Map.toUnfoldable (J.fromObject <<< Object.fromFoldable) $
    Array.mapMaybe \(Tuple k v) -> J.toString (keyEnc k) /|\ pure (valueEnc v)
  dec = J.toObject >>> case _ of
    Nothing -> jsonError (TypeMismatch "Object")
    Just items -> Map.fromFoldable <$> process (Object.toUnfoldable items)
  process :: Array (Tuple String JSON) -> JV (Array (Tuple k v))
  process = traverse \(Tuple k v) -> rectx (JField k) do
    keyDec (J.fromString k) /|\ valueDec v

mapA :: forall reg @k @a. Ord k => Ord reg => JRCodec reg k -> JRCodec reg a -> JRCodec reg (Map k a)
mapA key value = dimap Map.toUnfoldable Map.fromFoldable $ array $ tuple key value

list :: forall reg i o. Ord reg => JRCodec' reg i o -> JRCodec' reg (List.List i) (List.List o)
list = dimap List.toUnfoldable Array.toUnfoldable <<< array

nelist :: forall reg i o. Ord reg => JRCodec' reg i o -> JRCodec' reg (NonEmptyList i) (NonEmptyList o)
nelist = prismatic "NonEmptyList" NEL.toList NEL.fromList <<< list


class Ord reg <= AutoJSON t reg where
  autoJ :: JRCodec reg t

instance Ord reg => AutoJSON Void reg where autoJ = disjuxt0
instance Ord reg => AutoJSON Unit reg where autoJ = null
instance Ord reg => AutoJSON Boolean reg where autoJ = boolean
instance Ord reg => AutoJSON Number reg where autoJ = number
instance Ord reg => AutoJSON Int reg where autoJ = int
instance Ord reg => AutoJSON String reg where autoJ = string
instance Ord reg => AutoJSON NES.NonEmptyString reg where autoJ = nestring
instance Ord reg => AutoJSON S.CodePoint reg where autoJ = codePoint
instance Ord reg => AutoJSON Char reg where autoJ = char
instance Ord reg => AutoJSON JSON reg where autoJ = noneCodec
instance (AutoJSON t reg) => AutoJSON (Array t) reg where
  autoJ = array autoJ
instance (AutoJSON t reg) => AutoJSON (NEA.NonEmptyArray t) reg where
  autoJ = nearray autoJ
instance (AutoJSON t reg, AutoJSON d reg) => AutoJSON (Tuple t d) reg where
  autoJ = tuple autoJ autoJ
instance (AutoJSON t reg) => AutoJSON (Object t) reg where
  autoJ = keyed autoJ
instance AutoJSON t reg => AutoJSON (Maybe t) reg where
  autoJ = maybeA autoJ
instance (AutoJSON t reg, AutoJSON d reg) => AutoJSON (Either t d) reg where
  autoJ = eitherA autoJ autoJ
instance (AutoJSON t reg, AutoJSON d reg) => AutoJSON (These t d) reg where
  autoJ = theseA autoJ autoJ
instance (Ord t, AutoJSON t reg) => AutoJSON (Set t) reg where
  autoJ = set autoJ
instance (Ord k, AutoJSON k reg, AutoJSON v reg) => AutoJSON (Map k v) reg where
  autoJ = mapA autoJ autoJ
instance (Ord k, AutoJSON k reg, AutoJSON v reg) => AutoJSON (SemigroupMap k v) reg where
  autoJ = _N autoJ
instance (AutoJSON t reg) => AutoJSON (List.List t) reg where
  autoJ = list autoJ
instance (AutoJSON t reg) => AutoJSON (NEL.NonEmptyList t) reg where
  autoJ = nelist autoJ
instance (AutoJSON t reg) => AutoJSON (Pair t) reg where
  autoJ = (\(Pair l r) -> Tuple l r) >$<$> (uncurry Pair) !!! tuple autoJ autoJ

class (ListToRow rl os, Ord reg) <= AutoJSONRL rl is os reg | rl -> os where
  autoJRRL :: JROCodec' reg (Record is) (Record os)
  autoJVRL :: JROCodec' reg (Variant os) (Variant is)

instance autoJSONRLNil :: Ord reg => AutoJSONRL RL.Nil is () reg where
  autoJRRL = rec0
  autoJVRL = var0
instance autoJSONRLCons ::
  ( IsSymbol s
  , AutoJSON t reg
  , Row.Cons s t is' is
  , Row.Cons s t os' os
  , Row.Lacks s os'
  , AutoJSONRL rl is os' reg
  ) => AutoJSONRL (RL.Cons s t rl) is os reg where
    autoJRRL = (Proxy @s := autoJ) (autoJRRL @rl)
    autoJVRL | s <- Proxy @s = _disjuxt2
      (\l r -> Variant.on s (l <<< { type: unit, data: _ }) r)
      (Variant.inj s <<< _.data) identity
      (fields { type: tag (reflectSymbol s), data: autoJ @t })
      (autoJVRL @rl :: JROCodec' reg (Variant os') (Variant is))

instance
  ( RL.RowToList fields rl
  , AutoJSONRL rl fields fields reg
  ) => AutoJSON (Record fields) reg where
    autoJ = object (autoJRRL @rl)
instance
  ( RL.RowToList fields rl
  , AutoJSONRL rl fields fields reg
  ) => AutoJSON (Variant fields) reg where
    autoJ = object (autoJVRL @rl)

_N :: forall p x y. Newtype y x => Coercible (p x x) (p y y) => p x x -> p y y
_N = coerce

instance AutoJSON t reg => AutoJSON (Identity t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (Conj t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (Disj t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (Additive t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (Multiplicative t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (M.L.Last t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (S.L.Last t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (M.F.First t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (S.F.First t) reg where autoJ = _N autoJ
instance AutoJSON t reg => AutoJSON (Dual t) reg where autoJ = _N autoJ
