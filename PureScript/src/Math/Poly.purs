module Math.Poly where

import Math.Matrix
import Prelude

import Data.Align (class Align)
import Data.Foldable (class Foldable, foldMap)
import Data.Int as Int
import Data.Newtype (unwrap)
import Data.Number (sqrt)
import Data.Number as Math
import Data.Ord (abs)
import Data.Pair (Pair(..))
import Data.Traversable (class Traversable)
import Idiolect (cube, sgn, slipadd, slipsub, sqre)

padd :: forall @p @s. Align p => Semiring s => p s -> p s -> p s
padd = slipadd
psub :: forall @p @s. Align p => Ring s => p s -> p s -> p s
psub = slipsub

infixl 6 padd as :+:
infixl 6 psub as :-:

class PMul i j o where
  pmul :: i -> j -> o

infixl 7 pmul as :*:

class CrossP i j o | i j -> o where
  crossP :: i -> j -> o

infix 7 crossP as :×:

crossPS :: forall @i @j @s. CrossP i j (Scalar s) => i -> j -> s
crossPS i j = unwrap (crossP i j)

instance (PMul i j o, Group o) => CrossP (Vec2 i) (Vec2 j) (Vec1 o) where
  crossP (V2 lx ly) (V2 rx ry) = V1 (lx :*: ry <>- ly :*: rx)
instance (PMul i j o, Group o) => CrossP (Vec3 i) (Vec3 j) (Vec3 o) where
  crossP l r = dims <#> \d -> unwrap $
    crossP (without d l) (without d r)

norm2P :: forall @f @i @o. Foldable f => PMul i i o => Monoid o => f i -> o
norm2P = foldMap \s -> s :*: s

-- norm :: forall @f. Foldable f => f Number -> Number
-- norm = Math.sqrt <<< norm2

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

class Traversable solutions <= Solve p solutions | p -> solutions where
  solve :: p Number -> solutions Number
  solveN :: p Number -> SolveN Number

data SolveN c = EverywhereN | SolveN (Array c)
derive instance Functor SolveN
derive instance Foldable SolveN
derive instance Traversable SolveN

data Solve1 c = Everywhere1 | Nowhere1 | Solve1 c
derive instance Functor Solve1
derive instance Foldable Solve1
derive instance Traversable Solve1

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
instance Solve Bez1 Solve1 where
  solve = solve <<< bez2polyS
  solveN = solveN <<< bez2polyS

data Solve2 c = Everywhere2 | Nowhere2 | Solve21 c | Solve22 c c
derive instance Functor Solve2
derive instance Foldable Solve2
derive instance Traversable Solve2

discr2 :: Poly2 Number -> Number
discr2 (Poly2 c0 c1 c2) =
  c1 * c1 - 4.0 * c2 * c0

solve2 :: Poly2 Number -> Solve2 Number
solve2 (Poly2 c0 c1 c2) | c2 == zero =
  case solve1 (Poly1 c0 c1) of
    Everywhere1 -> Everywhere2
    Nowhere1 -> Nowhere2
    Solve1 c -> Solve21 c
solve2 (Poly2 c0 c1 c2) | c0 == zero =
  case solve1 (Poly1 c1 c2) of
    Everywhere1 -> Everywhere2
    Nowhere1 -> Solve21 zero
    Solve1 c -> Solve22 zero c
solve2 p@(Poly2 c0 c1 c2) =
  let
    d = discr2 p
    rest_c2 root_d = (-c1 + root_d) / (2.0 * c2)
    rest_c0 root_d = (2.0 * c0) / (-c1 - root_d)
    -- For numeric precision, avoid subtracting like signs
    rest root_d | eq @Int (sgn root_d) (sgn c1) = rest_c0 root_d
    rest root_d | otherwise = rest_c2 root_d
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
instance Solve Bez2 Solve2 where
  solve = solve <<< bez2polyS
  solveN = solveN <<< bez2polyS

data Solve3 r = Everywhere3 | Nowhere3 | Solve31 r | Solve32 r r | Solve33 r r r
derive instance Functor Solve3
derive instance Foldable Solve3
derive instance Traversable Solve3

solve3 :: Poly3 Number -> Solve3 Number
solve3 (Poly3 d c b a) | a == zero =
  case solve2 (Poly2 d c b) of
    Everywhere2 -> Everywhere3
    Nowhere2 -> Nowhere3
    Solve21 r1 -> Solve31 r1
    Solve22 r1 r2 -> Solve32 r1 r2
solve3 (Poly3 d c b a) | d == zero =
  case solve2 (Poly2 c b a) of
    Everywhere2 -> Everywhere3
    Nowhere2 -> Solve31 zero
    Solve21 r1 -> Solve32 zero r1
    Solve22 r1 r2 -> Solve33 zero r1 r2
solve3 (Poly3 d c b a) =
  case disc of
  _ | disc == 0.0 ->
      if p == 0.0
        then Solve31 o
        else Solve32
          (o - 1.5*q/p) -- double root
          (o + 3.0*q/p)
    | disc > 0.0 -> -- implies p < 0
        Solve33 0 1 2 <#>
          let
            rotate k = Math.pi * Int.toNumber k / 1.5
            angle = Math.acos (1.5 * q / p / rootp3) / 3.0
          in \k -> o + 2.0 * rootp3 * Math.cos (angle + rotate k)
    | p < 0.0 -> Solve31 $
        o - 2.0 * sgn q * rootp3 * cosh (acosh (-1.5 * abs q / p / rootp3) / 3.0)
    | otherwise -> Solve31 $
        o - 2.0 * rootp3 * sinh (asinh (1.5 * q / p / rootp3) / 3.0)
  where
  -- Offset for roots of non-depressed cubic
  o = - b / (3.0 * a)
  -- Coefficients for depressed cubic, x^3 + p x + q = 0
  p = (3.0 * a * c - sqre(b)) / (3.0 * sqre(a))
  q = (2.0 * cube(b) - 9.0*a*b*c + 27.0*sqre(a)*d) / (27.0 * cube(a))
  disc = - (4.0 * cube(p) + 27.0 * sqre(q))
  -- root p over three
  rootp3 = sqrt (abs p / 3.0)

  cosh x = (Math.exp x + Math.exp (-x)) / 2.0
  sinh x = (Math.exp x - Math.exp (-x)) / 2.0 -- TODO: accuracy
  acosh x = Math.log (x + Math.sqrt (x*x + 1.0))
  asinh x = Math.log (x + Math.sqrt (x*x - 1.0))

data Solve4 c = Everywhere4 | Nowhere4 | Solve41 c | Solve42 c c | Solve43 c c c | Solve44 c c c c
derive instance Functor Solve4
derive instance Foldable Solve4
derive instance Traversable Solve4

solve4 :: Poly4 Number -> Solve4 Number
solve4 (Poly4 e d c b a) | a == zero =
  case solve3 (Poly3 e d c b) of
    Everywhere3 -> Everywhere4
    Nowhere3 -> Nowhere4
    Solve31 r1 -> Solve41 r1
    Solve32 r1 r2 -> Solve42 r1 r2
    Solve33 r1 r2 r3 -> Solve43 r1 r2 r3
solve4 (Poly4 e d c b a) | e == zero =
  case solve3 (Poly3 d c b a) of
    Everywhere3 -> Everywhere4
    Nowhere3 -> Solve41 zero
    Solve31 r1 -> Solve42 zero r1
    Solve32 r1 r2 -> Solve43 zero r1 r2
    Solve33 r1 r2 r3 -> Solve44 zero r1 r2 r3
-- https://www.geometrictools.com/GTE/Mathematics/RootsQuartic.h
solve4 (Poly4 e d c b a) = Nowhere4
