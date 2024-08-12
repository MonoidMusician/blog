module Data.Pair where

import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Traversable (class Traversable1)
import Data.Traversable (class Traversable)

data Pair a = Pair a a

derive instance functorPair :: Functor Pair
derive instance foldablePair :: Foldable Pair
instance foldable1Pair :: Foldable1 Pair where
  foldr1 f (Pair l r) = f l r
  foldl1 f (Pair l r) = f l r
  foldMap1 f (Pair l r) = f l <> f r
derive instance traversablePair :: Traversable Pair
instance traversable1Pair :: Traversable1 Pair where
  traverse1 f (Pair l r) = Pair <$> f l <*> f r
  sequence1 (Pair l r) = Pair <$> l <*> r
derive instance eqPair :: Eq a => Eq (Pair a)
derive instance eq1Pair :: Eq1 Pair
derive instance ordPair :: Ord a => Ord (Pair a)
derive instance ord1Pair :: Ord1 Pair
instance semigroupPair :: Semigroup a => Semigroup (Pair a) where
  append (Pair l1 r1) (Pair l2 r2) = Pair (l1 <> l2) (r1 <> r2)
instance monoidPair :: Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty
