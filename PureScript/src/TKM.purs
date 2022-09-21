module TestKindMatch where

import Prelude
import Data.Maybe (Maybe)

class Functor1 :: ((Type -> Type) -> Type) -> Constraint
class Functor1 t where
  map1 :: forall f g. (f ~> g) -> t f -> t g

data Proxy :: forall k. k -> Type
data Proxy t = Proxy

type GetKind :: forall k. k -> Type
type GetKind (a :: k) = k

class Something :: forall k. (k -> Type) -> Constraint
class Something f where
  something :: forall a. f a -> f a

{-
instance somethingFunctor1 :: Functor1 t => Something t where
  something = map1 identity

else
instance somethingFunctor :: Functor f => Something f where
  something = map identity

-}

class SomethingHelper :: forall k. (k -> Type) -> Type -> Constraint
class SomethingHelper f k where
  somethingP :: forall a. Proxy k -> f a -> f a

instance somethingHelper :: SomethingHelper f (GetKind f) => Something f where
  something = somethingP (Proxy :: Proxy (GetKind f))

instance somethingFunctor :: Functor f => SomethingHelper f (Type -> Type) where
  somethingP _ = map identity

instance somethingFunctor1 :: Functor1 t => SomethingHelper t ((Type -> Type) -> Type) where
  somethingP _ = map1 identity

test1 :: Maybe Int -> Maybe Int
test1 = something

newtype Higher f = Higher (f Unit)
instance functor1Higher :: Functor1 Higher where
  map1 fg (Higher fa) = Higher (fg fa)

test2 :: Higher Maybe -> Higher Maybe
test2 = something
