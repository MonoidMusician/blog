-- @inline export multiplexing arity=3
-- @inline export tupling arity=3
module Idiolect where

import Prelude

import Control.Alternative (class Alt, class Alternative, (<|>))
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Align (class Align, align, aligned)
import Data.Argonaut (Json)
import Data.Array as Array
import Data.CodePoint.Unicode as U
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Filterable (class Filterable, filter, filterMap, partitionMap)
import Data.Foldable (class Foldable, fold, foldMap, intercalate, oneOfMap)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex)
import Data.Function (on)
import Data.Functor.Day (Day, day, runDay)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), maybe, optional)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype)
import Data.Number as Number
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.These (These(..))
import Data.Traversable (class Traversable, for, traverse)
import Data.TraversableWithIndex (forWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

type EffectArrow a b = a -> Effect b
-- | it's an arrow! that does! stuff!
infixr 1 type EffectArrow as -!>

type JSON = Json

type EndoFn a = a -> a

type Id :: forall k. k -> k
type Id a = a
infixl 0 type Id as $

infixr 8 Number.pow as **

morph :: forall f g b. Foldable f => Plus g => Applicative g => f b -> g b
morph = oneOfMap pure

intercalateMap :: forall f a m. Foldable f => Monoid m => Functor f => m -> (a -> m) -> f a -> m
intercalateMap sep f = intercalate sep <<< map f

infixr 0 foldMap as ..$

foldMapFlipped :: forall f a m. Foldable f => Monoid m => f a -> (a -> m) -> m
foldMapFlipped = flip foldMap
infixl 1 foldMapFlipped as #..

infixr 0 foldMapWithIndex as ..:$

foldMapWithIndexFlipped :: forall f i a m. FoldableWithIndex i f => Monoid m => f a -> (i -> a -> m) -> m
foldMapWithIndexFlipped = flip foldMapWithIndex
infixl 1 foldMapWithIndexFlipped as #:..

infixl 4 traverse as <..$>
infixl 1 for as <#..>
infixl 4 traverseWithIndex as <..:$>
infixl 1 forWithIndex as <#:..>

-- ..<
-- >..

infixl 4 mapWithIndex as :<$>

mapWithIndexFlipped :: forall f i a b. FunctorWithIndex i f => f a -> (i -> a -> b) -> f b
mapWithIndexFlipped = flip mapWithIndex
infixl 1 mapWithIndexFlipped as <#>:

infixl 4 filterMap as <$?>
infixl 4 filter as $?>

filterMapFlipped :: forall f a b. Filterable f => f a -> (a -> Maybe b) -> f b
filterMapFlipped = flip filterMap
infixl 1 filterMapFlipped as <#?>

filterFlipped :: forall f a. Filterable f => f a -> (a -> Boolean) -> f a
filterFlipped = flip filter
infixl 1 filterFlipped as <#?

infixl 4 partitionMap as /$?\

partitionMapFlipped :: forall f a l r. Filterable f => f a -> (a -> Either l r) -> { left :: f l, right :: f r }
partitionMapFlipped = flip partitionMap
infixl 1 partitionMapFlipped as /#?\

composeMap :: forall f a b c. Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
composeMap f g = map f <<< g
infixr 8 composeMap as ==<

composeMapFlipped :: forall f a b c. Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
composeMapFlipped f g = f >>> map g
infixr 8 composeMapFlipped as >==

composeTraverse :: forall f g a b c. Traversable f => Applicative g => (b -> g c) -> (a -> f b) -> (a -> g (f c))
composeTraverse f g = traverse f <<< g
infixr 8 composeTraverse as <==<

composeTraverseFlipped :: forall f g a b c. Traversable f => Applicative g => (a -> f b) -> (b -> g c) -> (a -> g (f c))
composeTraverseFlipped f g = f >>> traverse g
infixr 8 composeTraverseFlipped as >==>

multiplexing :: forall f a b. Alt f => f a -> f b -> f (a \/ b)
multiplexing a b = Left <$> a <|> Right <$> b
infixr 5 multiplexing as \|/

tupling :: forall f a b. Apply f => f a -> f b -> f (a /\ b)
tupling = lift2 Tuple
infixr 6 tupling as /|\

theseing :: forall f a b. Alternative f => f a -> f b -> f (a /\/ b)
theseing a b = lift2 maybe This Both <$> a <*> optional b <|> That <$> b
infixr 6 theseing as /\\/
infixr 6 type These as /\/

infixr 10 Maybe.maybe as ?+

maybeFlipped :: forall a b. (a -> b) -> b -> Maybe a -> b
maybeFlipped = flip Maybe.maybe
infixl 10 maybeFlipped as +?

tripleQuoted :: String -> String
tripleQuoted input =
  let
    lines = withPrefix <$> String.split (String.Pattern "\n") input
    wsPrefix = String.takeWhile U.isSpace
    withPrefix l = let p = wsPrefix l in
      if p == l then Nothing else Just (Tuple (CommonPrefix p) l)
    maxP = Array.foldMap (map fst) lines
    trimmed = lines <#> fold <<< case maxP, _ of
      Just (CommonPrefix p), Just (Tuple _ l) ->
        String.stripPrefix (String.Pattern p) l
      _, _ -> Nothing
    trimFirst = case _ of
      ls | Just "" <- Array.head ls -> Array.drop 1 ls
      ls -> ls
    trimLast = case _ of
      ls | Just "" <- Array.last ls
         , Just _ <- maxP -> Array.dropEnd 1 ls
      ls -> ls
  in String.joinWith "\n" $ trimmed # trimFirst # trimLast

newtype CommonPrefix = CommonPrefix String
derive instance newtypeCommonPrefix :: Newtype CommonPrefix _
instance semigroupCommonPrefix :: Semigroup CommonPrefix where
  append (CommonPrefix l) (CommonPrefix r) = CommonPrefix
    case (compare `on` String.length) l r of
      EQ | l == r -> l
      LT | Just _ <- String.stripPrefix (String.Pattern l) r -> l
      GT | Just _ <- String.stripPrefix (String.Pattern r) l -> r
      _ -> ""

only :: forall a. Array a -> Maybe a
only [ a ] = Just a
only _ = Nothing

nonEmpty :: forall m. Monoid m => Eq m => m -> Maybe m
nonEmpty m | m /= mempty = Just m
nonEmpty _ = Nothing

filterFst :: forall a b. (a -> Boolean) -> Tuple a b -> Maybe b
filterFst p (a /\ b) | p a = Just b
filterFst _ _ = Nothing

filterSnd :: forall a b. (b -> Boolean) -> Tuple a b -> Maybe a
filterSnd p (a /\ b) | p b = Just a
filterSnd _ _ = Nothing

filterKey ::
  forall k t r' r k' t' r''.
    IsSymbol k =>
    Row.Cons k t r' r =>
    RL.RowToList r' (RL.Cons k' t' RL.Nil) =>
    IsSymbol k' =>
    Row.Cons k' t' r'' r =>
  Proxy k ->
  (t -> Boolean) ->
  Record r ->
  Maybe t'
filterKey _ p r
  | p (Record.get (Proxy :: Proxy k) r)
  = Just (Record.get (Proxy :: Proxy k') r)
filterKey _ _ _ = Nothing

between :: forall a. Ord a => a -> a -> (a -> Boolean)
between lo hi val = lo <= val && val <= hi

unsafeFromMaybe :: forall a. String -> Maybe a -> a
unsafeFromMaybe _ (Just a) = a
unsafeFromMaybe reason Nothing = unsafeCrashWith reason

theseop :: forall @x. (x -> x -> x) -> These x x -> x
theseop binop (Both x1 x2) = binop x1 x2
theseop _ (This x) = x
theseop _ (That x) = x

slip2 :: forall @f @x. Align f => (x -> x -> x) -> f x -> f x -> f x
slip2 = align <<< theseop

slip2sgn :: forall @f @x. Align f => (x -> x -> x) -> (x -> x) -> f x -> f x -> f x
slip2sgn binop unop = align case _ of
  Both x1 x2 -> binop x1 x2
  This x -> x
  That x -> unop x

slipadd :: forall @f @x. Align f => Semiring x => f x -> f x -> f x
slipadd = slip2 add
slipsub :: forall @f @x. Align f => Ring x => f x -> f x -> f x
slipsub = slip2sgn sub negate
slipmul :: forall @f @x. Align f => Semiring x => f x -> f x -> f x
slipmul = slip2 mul
slipdiv :: forall @f @x. Align f => EuclideanRing x => DivisionRing x => f x -> f x -> f x
slipdiv = slip2sgn div recip

incorporate :: forall @x. Semigroup x => x -> Maybe x -> x
incorporate x Nothing = x
incorporate x (Just y) = x <> y

sqre :: forall @x. Semiring x => x -> x
sqre x = x * x

cube :: forall @x. Semiring x => x -> x
cube x = x * x * x

sgn :: forall @i @o. Semiring i => Ord i => Ring o => i -> o
sgn i = if i == zero then zero else if i > zero then one else negate one
