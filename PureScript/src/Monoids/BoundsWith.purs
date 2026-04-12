module Monoids.BoundsWith where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Distributive (class Distributive, collect)
import Data.Foldable (class Foldable, any, fold)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (class Foldable1, fold1)
import Data.Set as Set
import Data.Set.NonEmpty as NES
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), uncurry)
import Idiolect (withIndices, (/|\))

data MinWith ord meta = MinWith ord meta
data MaxWith ord meta = MaxWith ord meta

instance (Semigroup meta, Ord ord) => Semigroup (MinWith ord meta) where
  append l@(MinWith o1 m1) r@(MinWith o2 m2) = case compare o1 o2 of
    EQ -> MinWith o1 (m1 <> m2)
    LT -> l
    GT -> r

instance (Semigroup meta, Ord ord) => Semigroup (MaxWith ord meta) where
  append l@(MaxWith o1 m1) r@(MaxWith o2 m2) = case compare o1 o2 of
    EQ -> MaxWith o1 (m1 <> m2)
    GT -> l
    LT -> r

data BoundsWith ord meta = BoundsWith (MinWith ord meta) (MaxWith ord meta)

instance (Semigroup meta, Ord ord) => Semigroup (BoundsWith ord meta) where
  append (BoundsWith x1 y1) (BoundsWith x2 y2) =
    BoundsWith (x1 <> x2) (y1 <> y2)

derive instance Bifunctor MinWith
derive instance Bifunctor MaxWith
derive instance Bifunctor BoundsWith
derive instance Functor (MinWith ord)
derive instance Functor (MaxWith ord)
derive instance Functor (BoundsWith ord)
derive instance Foldable (MinWith ord)
derive instance Foldable (MaxWith ord)
derive instance Foldable (BoundsWith ord)
derive instance Traversable (MinWith ord)
derive instance Traversable (MaxWith ord)
derive instance Traversable (BoundsWith ord)

derive instance (Eq ord, Eq meta) => Eq (MinWith ord meta)
derive instance (Eq ord, Eq meta) => Eq (MaxWith ord meta)
derive instance (Eq ord, Eq meta) => Eq (BoundsWith ord meta)

derive instance (Ord ord, Ord meta) => Ord (MinWith ord meta)
derive instance (Ord ord, Ord meta) => Ord (MaxWith ord meta)
derive instance (Ord ord, Ord meta) => Ord (BoundsWith ord meta)

mkBound :: forall @meta @ord. meta -> ord -> BoundsWith ord meta
mkBound meta ord = BoundsWith (MinWith ord meta) (MaxWith ord meta)

type BoundsWithSet ord meta = BoundsWith ord (Set.Set meta)
mkBoundSet :: forall @meta @ord. meta -> ord -> BoundsWith ord (Set.Set meta)
mkBoundSet = Set.singleton >>> mkBound

type BoundsWithNES ord meta = BoundsWith ord (NES.NonEmptySet meta)
mkBoundNES :: forall @meta @ord. meta -> ord -> BoundsWith ord (NES.NonEmptySet meta)
mkBoundNES = NES.singleton >>> mkBound

inBounds :: forall @meta @ord. Ord ord => BoundsWith ord meta -> ord -> Boolean
inBounds (BoundsWith (MinWith bmin _) (MaxWith bmax _)) value = bmin <= value && value <= bmax

disjointBounds ::
  forall @f @ord @meta.
    Distributive f =>
    Apply f =>
    Foldable f =>
    Ord ord =>
  f (BoundsWith ord meta) -> f (BoundsWith ord meta) -> Boolean
disjointBounds ls rs = (ls /|\ rs) # any
  \(Tuple (BoundsWith (MinWith lmin _) (MaxWith lmax _)) (BoundsWith (MinWith rmin _) (MaxWith rmax _))) ->
    lmax < rmin || lmin > rmax

getBounds ::
  forall @f @g @meta @ord @item.
    Distributive f =>
    Functor g =>
    Foldable1 g =>
    Ord ord =>
    Semigroup meta =>
  (item -> meta) ->
  (item -> ord) ->
  g (f item) -> f (BoundsWith ord meta)
getBounds meta ord = collect (map \item -> mkBound (meta item) (ord item)) >>> map fold1

getBoundsWithIndex ::
  forall @f @g @meta @ord @item idx.
    Distributive f =>
    FunctorWithIndex idx g =>
    Foldable1 g =>
    Ord ord =>
    Semigroup meta =>
  (idx -> item -> meta) ->
  (idx -> item -> ord) ->
  g (f item) -> f (BoundsWith ord meta)
getBoundsWithIndex meta ord = map fold1 <<< withIndices >>> collect
  (uncurry \idx -> map \item -> mkBound (meta idx item) (ord idx item))

getBounds_ ::
  forall @f @g @meta @ord @item.
    Distributive f =>
    Functor g =>
    Foldable g =>
    Ord ord =>
    Semigroup meta =>
  (item -> meta) ->
  (item -> ord) ->
  g (f item) -> f (Maybe (BoundsWith ord meta))
getBounds_ meta ord = collect (map \item -> Just $ mkBound (meta item) (ord item)) >>> map fold

getBoundsWithIndex_ ::
  forall @f @g @meta @ord @item idx.
    Distributive f =>
    FunctorWithIndex idx g =>
    Foldable1 g =>
    Ord ord =>
    Semigroup meta =>
  (idx -> item -> meta) ->
  (idx -> item -> ord) ->
  g (f item) -> f (Maybe (BoundsWith ord meta))
getBoundsWithIndex_ meta ord = map fold <<< withIndices >>> collect
  (uncurry \idx -> map \item -> Just $ mkBound (meta idx item) (ord idx item))
