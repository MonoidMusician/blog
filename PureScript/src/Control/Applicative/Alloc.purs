module Control.Applicative.Alloc where

import Prelude

import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Monoid.Additive (Additive(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)

data Alloc s a = Alloc s (s -> a)

type AllocAdd s = Alloc (Additive s)

derive instance functorAlloc :: Functor (Alloc s)
instance applyAlloc :: Semigroup s => Apply (Alloc s) where
  apply (Alloc s1 f1) (Alloc s2 f2) = Alloc (s1 <> s2) \s ->
    f1 s (f2 (s <> s1))
instance applicativeAlloc :: Monoid s => Applicative (Alloc s) where
  pure = Alloc mempty <<< pure

allocating :: forall s a. Monoid s => Alloc s a -> a
allocating (Alloc _ f) = f mempty

allocatingFrom :: forall s a. s -> Alloc s a -> a
allocatingFrom s (Alloc _ f) = f s

allocated :: forall s a. Alloc s a -> s
allocated (Alloc amt _) = amt

alloc :: forall s a. s -> (s -> a) -> Alloc s a
alloc = Alloc

allocN :: forall s a. s -> (s -> a) -> AllocAdd s a
allocN amt f = alloc (Additive amt) (coerce f)

alloc1 :: forall s a. Semiring s => (s -> a) -> AllocAdd s a
alloc1 = allocN one

allocPart ::
  forall part value r' r a.
    Semiring (Record r) =>
    Semiring value =>
    IsSymbol part =>
    Row.Cons part value r' r =>
  Proxy part ->
  (value -> a) ->
  AllocAdd (Record r) a
allocPart p f =
  Alloc (_p one mempty) (f <<< view _p)
  where
  _p :: Lens' (Additive (Record r)) value
  _p = _Newtype <<< prop p
