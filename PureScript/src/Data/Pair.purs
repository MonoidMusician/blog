module Data.Pair where

import Prelude

import Control.Apply (lift2)
import Data.Align (class Align)
import Data.Distributive (class Distributive, collectDefault)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Traversable (class Traversable1)
import Data.These (These(..))
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)

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

instance applyPair :: Apply Pair where
  apply (Pair f g) (Pair a b) = Pair (f a) (g b)
instance applicativePair :: Applicative Pair where
  pure a = Pair a a
instance bindPair :: Bind Pair where
  bind (Pair a b) f = Pair
    (case f a of Pair c _ -> c)
    (case f b of Pair _ d -> d)
instance monadPair :: Monad Pair

instance distributivePair ::  Distributive Pair where
  collect = collectDefault
  distribute g = Pair
    (g <#> \(Pair s _) -> s)
    (g <#> \(Pair _ s) -> s)

instance alignPair :: Align Pair where align f = lift2 \x y -> f (Both x y)

instance FunctorWithIndex Boolean Pair where
  mapWithIndex f (Pair a b) = Pair (f false a) (f true b)
instance FoldableWithIndex Boolean Pair where
  foldrWithIndex f i (Pair a b) = f false a $ f true b $ i
  foldlWithIndex f i (Pair a b) = f false (f true i b) a
  foldMapWithIndex f (Pair a b) = f false a <> f true b
instance TraversableWithIndex Boolean Pair where
  traverseWithIndex f (Pair a b) = Pair <$> f false a <*> f true b

instance (Show t) => Show (Pair t) where
  show (Pair a b) = "(Pair " <> show a <> " " <> show b <> ")"
