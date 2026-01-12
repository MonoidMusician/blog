module Math.Poly where

import Math.Matrix
import Prelude

import Control.Apply (lift2)
import Data.Newtype (unwrap)
import Data.Number as Math
import Data.Pair (Pair(..))

padd :: forall @p @s. Apply p => Semiring s => p s -> p s -> p s
padd = lift2 (+)
psub :: forall @p @s. Apply p => Ring s => p s -> p s -> p s
psub = lift2 (-)

infixl 6 padd as :+:
infixl 6 psub as :-:

class PMul i j o where
  pmul :: i -> j -> o

infixl 7 pmul as :*:

instance Semiring s => PMul s s s where
  pmul = mul
else instance PMul i j o => PMul (Poly0 i) (Poly0 j) (Poly0 o) where
  pmul (Poly0 ci) (Poly0 cj) = Poly0 (pmul ci cj)
else instance PMul i j o => PMul (Poly0 i) (Poly1 j) (Poly1 o) where
  pmul (Poly0 ci) (Poly1 cj0 cj1) = Poly1 (pmul ci cj0) (pmul ci cj1)
else instance PMul i j o => PMul (Poly0 i) (Poly2 j) (Poly2 o) where
  pmul (Poly0 ci) (Poly2 cj0 cj1 cj2) = Poly2 (pmul ci cj0) (pmul ci cj1) (pmul ci cj2)
else instance PMul i j o => PMul (Poly0 i) (Poly3 j) (Poly3 o) where
  pmul (Poly0 ci) (Poly3 cj0 cj1 cj2 cj3) = Poly3 (pmul ci cj0) (pmul ci cj1) (pmul ci cj2) (pmul ci cj3)
else instance PMul i j o => PMul (Poly0 i) (Poly4 j) (Poly4 o) where
  pmul (Poly0 ci) (Poly4 cj0 cj1 cj2 cj3 cj4) = Poly4 (pmul ci cj0) (pmul ci cj1) (pmul ci cj2) (pmul ci cj3) (pmul ci cj4)
else instance PMul i j o => PMul (Poly1 i) (Poly0 j) (Poly1 o) where
  pmul (Poly1 cj0 cj1) (Poly0 ci) = Poly1 (pmul cj0 ci) (pmul cj1 ci)
else instance PMul i j o => PMul (Poly2 i) (Poly0 j) (Poly2 o) where
  pmul (Poly2 cj0 cj1 cj2) (Poly0 ci) = Poly2 (pmul cj0 ci) (pmul cj1 ci) (pmul cj2 ci)
else instance PMul i j o => PMul (Poly3 i) (Poly0 j) (Poly3 o) where
  pmul (Poly3 cj0 cj1 cj2 cj3) (Poly0 ci) = Poly3 (pmul cj0 ci) (pmul cj1 ci) (pmul cj2 ci) (pmul cj3 ci)
else instance PMul i j o => PMul (Poly4 i) (Poly0 j) (Poly4 o) where
  pmul (Poly4 cj0 cj1 cj2 cj3 cj4) (Poly0 ci) = Poly4 (pmul cj0 ci) (pmul cj1 ci) (pmul cj2 ci) (pmul cj3 ci) (pmul cj4 ci)

else instance (PMul i j o, Semigroup o) => PMul (Poly1 i) (Poly1 j) (Poly2 o) where
  pmul (Poly1 ci0 ci1) (Poly1 cj0 cj1) = Poly2 (pmul ci0 cj0) (pmul ci0 cj1 <> pmul ci1 cj0) (pmul ci1 cj1)
else instance (PMul i j o, Semigroup o) => PMul (Poly1 i) (Poly2 j) (Poly3 o) where
  pmul (Poly1 ci0 ci1) (Poly2 cj0 cj1 cj2) = Poly3 (pmul ci0 cj0) (pmul ci0 cj1 <> pmul ci1 cj0) (pmul ci0 cj2 <> pmul ci1 cj1) (pmul ci1 cj2)
else instance (PMul i j o, Semigroup o) => PMul (Poly1 i) (Poly3 j) (Poly4 o) where
  pmul (Poly1 ci0 ci1) (Poly3 cj0 cj1 cj2 cj3) = Poly4 (pmul ci0 cj0) (pmul ci0 cj1 <> pmul ci1 cj0) (pmul ci0 cj2 <> pmul ci1 cj1) (pmul ci0 cj3 <> pmul ci1 cj2) (pmul ci1 cj3)
else instance (PMul i j o, Semigroup o) => PMul (Poly2 i) (Poly1 j) (Poly3 o) where
  pmul (Poly2 cj0 cj1 cj2) (Poly1 ci0 ci1) = Poly3 (pmul cj0 ci0) (pmul cj1 ci0 <> pmul cj0 ci1) (pmul cj2 ci0 <> pmul cj1 ci1) (pmul cj2 ci1)
else instance (PMul i j o, Semigroup o) => PMul (Poly3 i) (Poly1 j) (Poly4 o) where
  pmul (Poly3 cj0 cj1 cj2 cj3) (Poly1 ci0 ci1) = Poly4 (pmul cj0 ci0) (pmul cj1 ci0 <> pmul cj0 ci1) (pmul cj2 ci0 <> pmul cj1 ci1) (pmul cj3 ci0 <> pmul cj2 ci1) (pmul cj3 ci1)

else instance (PMul i j o, Semigroup o) => PMul (Poly2 i) (Poly2 j) (Poly4 o) where
  pmul (Poly2 ci0 ci1 ci2) (Poly2 cj0 cj1 cj2) = Poly4
    (pmul ci0 cj0) (pmul ci0 cj1 <> pmul ci1 cj0)
    (pmul ci0 cj2 <> pmul ci1 cj1 <> pmul ci2 cj0)
    (pmul ci1 cj2 <> pmul ci2 cj1) (pmul ci2 cj2)

else instance Semiring s => PMul s (Vec1 s) (Vec1 s) where
  pmul = smul
else instance Semiring s => PMul (Vec1 s) s (Vec1 s) where
  pmul = flip smul
else instance PMul i j o => PMul (Vec1 i) (Vec1 j) (Vec1 o) where
  pmul (V1 ci) (V1 cj) = V1 (pmul ci cj)

else instance Semiring s => PMul s (Vec2 s) (Vec2 s) where
  pmul = smul
else instance Semiring s => PMul (Vec2 s) s (Vec2 s) where
  pmul = flip smul
else instance Semiring s => PMul s (Vec3 s) (Vec3 s) where
  pmul = smul
else instance Semiring s => PMul (Vec3 s) s (Vec3 s) where
  pmul = flip smul

else instance Semiring s => PMul s s (Vec1 s) where
  pmul l r = V1 (l * r)

class Deriv i o | i -> o where
  deriv :: i -> o

instance Monoid c => Deriv (Poly0 c) (Poly0 c) where
  deriv = const (Poly0 mempty)
instance Semigroup c => Deriv (Poly1 c) (Poly0 c) where
  deriv (Poly1 _ c1) = Poly0 c1
instance Semigroup c => Deriv (Poly2 c) (Poly1 c) where
  deriv (Poly2 _ c1 c2) = Poly1 c1 (c2 <> c2)
instance Semigroup c => Deriv (Poly3 c) (Poly2 c) where
  deriv (Poly3 _ c1 c2 c3) = Poly2 c1 (c2 <> c2) (c3 <> c3 <> c3)
instance Semigroup c => Deriv (Poly4 c) (Poly3 c) where
  deriv (Poly4 _ c1 c2 c3 c4) = Poly3 c1 (c2 <> c2) (c3 <> c3 <> c3) (let c42 = c4 <> c4 in c42 <> c42)

instance Group c => Deriv (Bez3 c) (Bez2 c) where
  deriv = pairs >>> map \(Pair p0 p1) -> gmul 3 (p1 <>- p0)
instance Group c => Deriv (Bez2 c) (Bez1 c) where
  deriv = pairs >>> map \(Pair p0 p1) -> gmul 2 (p1 <>- p0)
instance Group c => Deriv (Bez1 c) (Bez0 c) where
  deriv = pairs >>> map \(Pair p0 p1) -> p1 <>- p0

class IsPoly p where
  evalP :: forall @s @v. SModule s v => p v -> s -> v

infixl 9 evalP as @@

evalPS :: forall @p @s. IsPoly p => Semiring s => p (Scalar s) -> s -> s
evalPS poly t = unwrap (evalP poly t)

infixl 9 evalPS as @@.

instance IsPoly Poly0 where
  evalP (Poly0 c) _ = c
instance IsPoly Poly1 where
  evalP (Poly1 c0 c1) t = c0 <> t .* c1
instance IsPoly Poly2 where
  evalP (Poly2 c0 c1 c2) t = c0 <> t .* (c1 <> t .* c2)
instance IsPoly Poly3 where
  evalP (Poly3 c0 c1 c2 c3) t = c0 <> t .* (c1 <> t .* (c2 <> t .* c3))
instance IsPoly Poly4 where
  evalP (Poly4 c0 c1 c2 c3 c4) t = c0 <> t .* (c1 <> t .* (c2 <> t .* (c3 <> t .* c4)))

class IsPoly p <= Solve p solutions | p -> solutions where
  solve :: p Number -> solutions Number
  solveN :: p Number -> SolveN Number

data SolveN c = EverywhereN | SolveN (Array c)

data Solve1 c = Everywhere1 | Nowhere1 | Solve1 c

solve1 :: forall c. Field c => Eq c => Poly1 c -> Solve1 c
solve1 (Poly1 c0 c1) | c1 == zero =
  if c0 == zero then Everywhere1 else Nowhere1
solve1 (Poly1 c0 c1) = Solve1 (- c0 / c1)

instance Solve Poly1 Solve1 where
  solve = solve1
  solveN = solve1 >>> case _ of
    Everywhere1 -> EverywhereN
    Nowhere1 -> SolveN []
    Solve1 c -> SolveN [c]

data Solve2 c = Everywhere2 | Nowhere2 | Solve21 c | Solve22 c c

discr2 :: Poly2 Number -> Number
discr2 (Poly2 c0 c1 c2) =
  c1 * c1 - 4.0 * c2 * c0

solve2 :: Poly2 Number -> Solve2 Number
solve2 (Poly2 c0 c1 c2) | c2 == zero =
  case solve1 (Poly1 c0 c1) of
    Everywhere1 -> Everywhere2
    Nowhere1 -> Nowhere2
    Solve1 c -> Solve21 c
solve2 p@(Poly2 _ c1 c2) =
  let
    d = discr2 p
    rest root_d = (negate c1 + root_d) / (2.0 * c2)
  in case d `compare` 0.0 of
    LT -> Nowhere2
    EQ -> Solve21 (rest (Math.sqrt d))
    GT -> Solve22 (rest (Math.sqrt d)) (rest (-Math.sqrt d))

instance Solve Poly2 Solve2 where
  solve = solve2
  solveN = solve2 >>> case _ of
    Everywhere2 -> EverywhereN
    Nowhere2 -> SolveN []
    Solve21 c -> SolveN [c]
    Solve22 c1 c2 -> SolveN [c1, c2]

data Solve3 c = Everywhere3 | Nowhere3 | Solve31 c | Solve32 c c | Solve33 c c c

data Solve4 c = Everywhere4 | Nowhere4 | Solve41 c | Solve42 c c | Solve43 c c c | Solve44 c c c c
