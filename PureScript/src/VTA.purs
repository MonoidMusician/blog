module VTA where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect.Console (log)

data Tuple a b = Tuple a b
{-
infixr 5 type Tuple as /\

tuple :: forall @a @b. a -> b -> Tuple a b
tuple = Tuple

t :: forall a27 b28. a27 -> b28 -> Tuple a27 b28
t = Tuple

-- test = t @Int

thingy :: forall f. (forall a b. a -> b -> f a b) -> f Unit Int
thingy mk = mk unit 1

-- surprising: @_ higher order
thang = thingy (Tuple @_)

-- bug:
-- :t Tuple

-- expected:
-- :t identity @(->) @(forall @a @b. a -> b -> Tuple a b) tuple
-- An expression of type:
--
--   t6 t7 t7
--
-- cannot be applied to the type:
--
--   forall @a @b. a -> b -> Tuple a b
--
-- as it has no visible type abstraction.


data TProxy :: forall @k. k -> Type
data TProxy a = CProxy

example :: TProxy @Type String
example = CProxy @String
-}
