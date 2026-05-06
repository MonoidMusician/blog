module Math.Matrix where

import Prelude

import Control.Apply (lift2)
import Data.Align (class Align, align)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap)
import Data.Distributive (class Distributive, collect, collectDefault, distribute)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, any, foldMap, foldl, foldr, sum)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Number as Math
import Data.Ord (class Ord1)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..))
import Data.Profunctor (dimap)
import Data.Semigroup.Foldable (class Foldable1, fold1)
import Data.Semigroup.Traversable (class Traversable1)
import Data.Show.Generic (genericShow)
import Data.These (These(..), these)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy, spyWith)
import Idiolect (neighbors, sqre, theseop, (/|\), (<#>:))
import Monoids.BoundsWith (BoundsWith(..), MaxWith(..), MinWith(..))
import Parser.Optimized.Types (unsafeFromJust)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

type Degrees = Number
type Radians = Number
type Pixels = Number
type Time = Number -- time 0.0 to 1.0 on a Bézier curve
type Fraction = Number -- fraction from 0.0 to 1.0 on some scale

-- | Groups are monoids with inverses such that `inv m <> m = mempty = m <> inv m`
class Monoid m <= Group m where
  inv :: m -> m

leftInv :: forall m. Group m => m -> m -> m
leftInv m1 m2 = inv m1 <> m2
rightInv :: forall m. Group m => m -> m -> m
rightInv m1 m2 = m1 <> inv m2
infixr 6 leftInv as -<>
infixl 6 rightInv as <>-
infixr 6 leftInv as -<>+
infixl 6 rightInv as +<>-
doubleInv :: forall m. Group m => m -> m -> m
doubleInv m1 m2 = inv (m2 <> m1)
infix 6 doubleInv as -<>-

infixr 6 append as +<>
infixl 6 append as <>+
infix 6 append as +<>+

gpower :: forall m. Group m => m -> Int -> m
gpower _ 0 = mempty
gpower m n | n < 0 = inv (power m (negate n))
gpower m n = power m n

gmul :: forall m. Group m => Int -> m -> m
gmul = flip gpower

mmul :: forall m. Monoid m => Int -> m -> m
mmul = flip power

instance Ring r => Group (Additive r) where
  inv = coerce (negate :: r -> r)
instance (Group a, Group b) => Group (Tuple a b) where
  inv = bimap inv inv

-- | Scalar modules, where `s` is scalar and `v` is vector.
-- |
-- | Laws:
-- | - Multiplicative monoid action (multiplication): `smul (a * b) m = smul a (smul b m)`
-- | - Multiplicative monoid action (one): `smul one m = m`
-- | - Linearity (addition): `smul (a + b) m = smul a m <> smul b m`
-- | - Linearity (zero): `smul zero m = mempty`
-- | - Linearity (append): `smul c (m1 <> m2) = smul c m1 <> smul c m2`
-- | - Linearity (mempty): `smul c mempty = mempty`
class (Semiring s, Monoid v) <= SModule s v | v -> s where
  smul :: s -> v -> v

infixr 7 smul as .*

sdiv :: forall s v. Field s => SModule s v => v -> s -> v
sdiv v s = smul (recip s) v

infixl 7 sdiv as /.


-- x +<t>+ y
affComb :: forall @s @v. SModule s v => Ring s => v -> s -> v -> v
affComb x t y = x <> t .* (y <> (negate one) .* x)
  -- (one - t) .* x <> t .* y

interp :: forall @s @v. SModule s v => Ring s => v -> v -> (s -> v)
interp = flip <<< affComb

infixl 5 affComb as +<
infixl 5 identity as >+
infixl 5 interp as +<*>+

interpS :: forall @s. Ring s => s -> s -> (s -> s)
interpS x y = unwrap <<< interp (V1 x) (V1 y)

affCombS :: forall @s. Ring s => s -> s -> s -> s
affCombS x t y = unwrap $ affComb (V1 x) t (V1 y)

infixl 5 affCombS as .+<
infixl 5 identity as >+.

interpsWith :: forall x y z s. (x -> y -> s -> z) -> Array x -> Array y -> s -> Array z
interpsWith f xs ys t = Array.zipWith (\x y -> f x y t) xs ys

multiinterpWith :: forall @x. (x -> x -> Number -> x) -> NonEmptyArray (Tuple Number x) -> Number -> x
multiinterpWith f = NEA.sortWith fst >>> \points ->
  let fallback = NEA.last points # snd in
  case NEA.fromArray (pairs points) of
    -- Just one item
    Nothing -> \_ -> fallback
    Just items -> \t ->
      items
      -- Find a pair in the right range
      # NEA.findMap (inRange t)
      -- Fall back to the last item
      # fromMaybe fallback
  where
  inRange t (Pair (Tuple t0 x0) (Tuple t1 x1))
    | t0 <= t && t <= t1 =
      Just $ f x0 x1 $ uninterp t0 t1 t
    -- Catch t before the items
    | t <= t0 = Just x0
  inRange _ _ = Nothing

uninterp :: forall @s. Field s => s -> s -> s -> s
uninterp t0 t1 t = (t - t0) / (t1 - t0)

triinterpWith :: forall @v. (v -> v -> Number -> v) -> NonEmptyArray { t :: Number, r :: Number, p :: v } -> Number -> v
triinterpWith i = NEA.sortWith _.t >>> \points ->
  case NEA.toArray points of
    [{ p }] -> const p
    [{ p: p0 }, { p: p1 }] -> i p0 p1
    pointz | bends <- spyWith "bends" (map _seeBend) $ mkBend <$> neighbors pointz, _ <- spyWith "straights" (pairsWith _seeStraight) bends ->
      case _ of
        t | Just r <- inBend t bends -> r
        t | Just r <- onStraight t bends -> r
        t | t <= (NEA.head points).t -> (NEA.head points).p
        _ -> (NEA.last points).p
  where
  mkBend = case _ of
    { prev: Just prev, here: here@{ r }, next: Just next }
      | dt <- r * (min (here.t - prev.t) (next.t - here.t))
      , t0 <- here.t - dt, t1 <- here.t + dt
      , p0 <- i prev.p here.p $ uninterp prev.t here.t t0
      , p1 <- i here.p next.p $ uninterp here.t next.t t1
      , bend <- { dt, t0, t1, p0, p1 } ->
      { prev: Just prev, here, next: Just next, bend: { dt, t0, t1, p0, p1 } }
    { prev, here, next } -> { prev, here, next, bend: { dt: 0.0, t0: here.t, t1: here.t, p0: here.p, p1: here.p } }
  _seeBend = \{ here: { p }, bend: { dt, t0, t1, p0, p1 } } -> Array.fold
    let
      ht = Array.fold [ "(", "t", "-", unsafeCoerce t0, ")", "/", "(", unsafeCoerce t1, "-", unsafeCoerce t0, ")" ]
      x y z t = Array.fold [ unsafeCoerce y, "*", "(", "1", "-", unsafeCoerce t, ")", "+", unsafeCoerce z, "*", unsafeCoerce t ]
    in
    [ unsafeCoerce t0, " < t < ", unsafeCoerce t1, ": "
    , x (unsafeCoerce $ "(" <> x p0 p ht <> ")") (unsafeCoerce $ "(" <> x p p1 ht <> ")") ht
    ]
  _seeStraight = \{ here: { p: p0, t: t0 }, bend: { t1: r0 } } { here: { p: p1, t: t1 }, bend: { t0: r1 } } -> Array.fold
    let t = Array.fold [ "(", "t", "-", unsafeCoerce t0, ")", "/", "(", unsafeCoerce t1, "-", unsafeCoerce t0, ")" ] in
    [ unsafeCoerce r0, " < t < ", unsafeCoerce r1, ": "
    , unsafeCoerce p0, "*", "(", "1", "-", unsafeCoerce t, ")", "+", unsafeCoerce p1, "*", unsafeCoerce t
    ]
  inBend t = Array.findMap case _ of
    { here, bend: { t0, t1, p0, p1 } }
      | t0 < t && t < t1, ht <- uninterp t0 t1 t -> Just $
        i (i p0 here.p ht) (i here.p p1 ht) ht
    _ -> Nothing
  onStraight t = Array.findMap case _ of
    { prev: Just prev, here, bend }
      | prev.t < t && t < bend.t0 -> Just $
        i prev.p here.p $ uninterp prev.t here.t t
    { here, next: Just next, bend }
      | bend.t1 < t && t < next.t -> Just $
        i here.p next.p $ uninterp here.t next.t t
    _ -> Nothing

triinterp :: forall @v. SModule Number v => NonEmptyArray { t :: Number, r :: Number, p :: v } -> Number -> v
triinterp = triinterpWith interp

frac :: Int -> Int -> Number
frac n d = Int.toNumber n / Int.toNumber d

pointsBetween :: Number -> Number -> Int -> Array Number
pointsBetween p0 p1 n = Array.range 0 n <#> \i ->
  interpS p0 p1 (frac i n)

pointsBounds :: Bounds Number -> Int -> Array Number
pointsBounds { min: Min bmin, max: Max bmax } = pointsBetween bmin bmax

norm2 :: forall @f @s. Foldable f => Semiring s => f s -> s
norm2 = un Additive <<< foldMap \s -> Additive (s * s)

norm :: forall @f. Foldable f => f Number -> Number
norm = Math.sqrt <<< norm2

normalize :: forall @f. Traversable f => f Number -> f Number
normalize vec = vec <#> (_ / norm vec)

dot :: forall @f @s. Align f => Foldable f => Semiring s => f s -> f s -> s
dot l r = un Additive (foldMap Additive (align (these zero zero (*)) l r))

infix 5 dot as .*.

-- | Matrix transformations over vectors.
-- |
-- | Laws:
-- | - `tf (tfC a b) m = tf a (tf b m)`
-- | - `tf tfI m = m`
-- | - `smul s (tf t m) = tf (smul s t) m`
-- |
-- | For linear transformations,
-- | - `tf t (m1 <> m2) = tf t m1 <> tf t m2`
-- | - `tf t mempty = mempty`
class (Monoid v) <= Transforms t v | t -> v where
  tf :: t -> v -> v
  tfI :: t
  tfC :: t -> t -> t
-- | - `tfC (tfR t) t = tfI = tfC t (tfR t)` if `det t /= zero`
class (Transforms t v) <= Untransforms t v | t -> v where
  tfR :: t -> t
-- | - `det (smul s t) = s^(dim @t) * det t`
class (Transforms t v) <= STransforms s t v | t -> v s where
  det :: t -> s

infixr 2 tf as $*

tfS :: forall @t @s. Transforms t (Vec1 s) => t -> s -> s
tfS t s = unwrap (tf t (V1 s))
infixr 2 tfS as $.

infixr 3 tfC as <.
infixr 3 tfCFlipped as .>
tfCFlipped :: forall @t @v. Transforms t v => t -> t -> t
tfCFlipped = flip tfC

infix 3 tfCR as <^
infix 3 tfCRFlipped as ^>
infix 3 tfRC as ^<
infix 3 tfRCFlipped as >^
infix 3 tfRCR as ^<^
infix 3 tfRCRFlipped as ^>^
tfCR :: forall @t @v. Untransforms t v => t -> t -> t
tfCR t1 t2 = tfC t1 (tfR t2)
tfCRFlipped :: forall @t @v. Untransforms t v => t -> t -> t
tfCRFlipped = flip tfCR
tfRC :: forall @t @v. Untransforms t v => t -> t -> t
tfRC t1 t2 = tfC (tfR t1) t2
tfRCFlipped :: forall @t @v. Untransforms t v => t -> t -> t
tfRCFlipped = flip tfRC
tfRCR :: forall @t @v. Untransforms t v => t -> t -> t
tfRCR t1 t2 = tfR (tfC t2 t1)
tfRCRFlipped :: forall @t @v. Untransforms t v => t -> t -> t
tfRCRFlipped = flip tfRCR

-- | Newtypes                                                               | --

newtype NatModule m = NatModule m
derive instance Newtype (NatModule m) _
derive instance Functor NatModule
derive instance Eq1 NatModule
derive instance Ord1 NatModule
derive instance Eq m => Eq (NatModule m)
derive instance Ord m => Ord (NatModule m)
derive newtype instance Semigroup m => Semigroup (NatModule m)
derive newtype instance Monoid m => Monoid (NatModule m)

instance Monoid m => SModule Int (NatModule m) where
  smul n (NatModule m) = NatModule (power m n)

newtype IntModule m = IntModule m
derive instance Newtype (IntModule m) _
derive instance Functor IntModule
derive instance Eq1 IntModule
derive instance Ord1 IntModule
derive instance Eq m => Eq (IntModule m)
derive instance Ord m => Ord (IntModule m)
derive newtype instance Semigroup m => Semigroup (IntModule m)
derive newtype instance Monoid m => Monoid (IntModule m)

instance Group m => SModule Int (IntModule m) where
  smul n (IntModule m) = IntModule (gpower m n)

-- | Transformation as a `Monoid`/`Group` (and still a `SModule`).
newtype TF t = TF t
derive instance Newtype (TF t) _
derive instance Functor TF
derive instance Eq1 TF
derive instance Ord1 TF
derive instance Eq t => Eq (TF t)
derive instance Ord t => Ord (TF t)
instance Transforms t v => Semigroup (TF t) where
  append (TF t1) (TF t2) = TF (tfC t1 t2)
instance Transforms t v => Monoid (TF t) where
  mempty = TF tfI
instance Untransforms t v => Group (TF t) where
  inv (TF t) = TF (tfR t)
instance (SModule s t, SModule s v, Transforms t v) => SModule s (TF t) where
  smul s (TF t) = TF (smul s t)

-- | Transformation lifted over a functor (Bézier or vector or such).
newtype LTF :: (Type -> Type) -> Type -> Type
newtype LTF f t = LTF t
derive instance Newtype (LTF f t) _
derive instance Functor (LTF f)
derive instance Eq1 (LTF f)
derive instance Ord1 (LTF f)
derive instance Eq t => Eq (LTF f t)
derive instance Ord t => Ord (LTF f t)
derive newtype instance Semigroup t => Semigroup (LTF f t)
derive newtype instance Monoid t => Monoid (LTF f t)
derive newtype instance Group t => Group (LTF f t)
instance (Functor f, Monoid (f v), Transforms t v) => Transforms (LTF f t) (f v) where
  tf (LTF t) f = tf t <$> f
  tfI = LTF tfI
  tfC (LTF t1) (LTF t2) = LTF (tfC t1 t2)
instance (Functor f, Monoid (f v), Untransforms t v) => Untransforms (LTF f t) (f v) where
  tfR (LTF t) = LTF (tfR t)
instance (Functor f, Monoid (f v), STransforms s t v) => STransforms s (LTF f t) (f v) where
  det (LTF t) = det t

ltf :: forall @f @t @v. Functor f => Monoid (f v) => Transforms t v => t -> f v -> f v
ltf = tf <<< LTF

-- | Main vector/matrix/Bézier types                                        | --

data Vec3 s = V3 s s s
data Vec2 s = V2 s s
newtype Vec1 s = V1 s
type Scalar = Vec1
derive instance Newtype (Vec1 s) _
derive newtype instance Semiring s => Semiring (Vec1 s)
derive newtype instance Ring s => Ring (Vec1 s)
derive newtype instance CommutativeRing s => CommutativeRing (Vec1 s)
derive newtype instance EuclideanRing s => EuclideanRing (Vec1 s)
derive newtype instance DivisionRing s => DivisionRing (Vec1 s)

data Dim = X | Y | Z
derive instance Eq Dim
derive instance Ord Dim
dims = V3 X Y Z :: Vec3 Dim

dir :: forall @s. Semiring s => Dim -> Vec3 s
dir xyz = dims <#> \d -> if d == xyz then one else zero

without :: forall s. Dim -> Vec3 s -> Vec2 s
without X (V3 _ y z) = V2 y z
without Y (V3 x _ z) = V2 x z
without Z (V3 x y _) = V2 x y

withouts :: forall s. Vec3 s -> Vec3 (Vec2 s)
withouts vs = dims <#> \d -> without d vs

_X1 = V1 one :: forall @s. Semiring s => Vec1 s
_X2 = V2 one zero :: forall @s. Semiring s => Vec2 s
_Y2 = V2 zero one :: forall @s. Semiring s => Vec2 s
_X3 = V3 one zero zero :: forall @s. Semiring s => Vec3 s
_Y3 = V3 zero one zero :: forall @s. Semiring s => Vec3 s
_Z3 = V3 zero zero one :: forall @s. Semiring s => Vec3 s

v1X = (\(V1 x) -> x) :: forall @s. Vec1 s -> s
v2X = (\(V2 x _) -> x) :: forall @s. Vec2 s -> s
v2Y = (\(V2 _ y) -> y) :: forall @s. Vec2 s -> s
v3X = (\(V3 x _ _) -> x) :: forall @s. Vec3 s -> s
v3Y = (\(V3 _ y _) -> y) :: forall @s. Vec3 s -> s
v3Z = (\(V3 _ _ z) -> z) :: forall @s. Vec3 s -> s

type V3 = Vec3 Number
type V2 = Vec2 Number
type V1 = Vec1 Number

instance FunctorWithIndex Dim Vec3 where
  mapWithIndex f (V3 x y z) = V3 (f X x) (f Y y) (f Z z)
instance FoldableWithIndex Dim Vec3 where
  foldrWithIndex f i (V3 x y z) = f X x $ f Y y $ f Z z $ i
  foldlWithIndex f i (V3 x y z) = f X (f Y (f Z i z) y) x
  foldMapWithIndex f (V3 x y z) = f X x <> f Y y <> f Z z
instance TraversableWithIndex Dim Vec3 where
  traverseWithIndex f (V3 x y z) = V3 <$> f X x <*> f Y y <*> f Z z

instance FunctorWithIndex Unit Vec1 where
  mapWithIndex f = map (f unit)
instance FoldableWithIndex Unit Vec1 where
  foldrWithIndex f = foldr (f unit)
  foldlWithIndex f = foldl (f unit)
  foldMapWithIndex f = foldMap (f unit)
instance TraversableWithIndex Unit Vec1 where
  traverseWithIndex f = traverse (f unit)

-- | Linear transformations
data Lin3 s = Lin3 s s s s s s s s s
data Lin2 s = Lin2 s s s s
data Lin1 s = Lin1 s
-- | Affine transformations (linear transformation followed by translation)
data Afn3 s = Afn3 s s s s s s s s s s s s
data Afn2 s = Afn2 s s s s s s
data Afn1 s = Afn1 s s

-- | Cubic Bézier curve
data Bez3 p = B3 p p p p
-- | Quadratic Bézier curve
data Bez2 p = B2 p p p
-- | Linear Bézier curve (a line between endpoints)
data Bez1 p = B1 p p
type Bez0 = Poly0

type B33 = Bez3 V3
type B32 = Bez3 V2
type B31 = Bez3 V1
type B23 = Bez2 V3
type B22 = Bez2 V2
type B21 = Bez2 V1
type B13 = Bez1 V3
type B12 = Bez1 V2
type B11 = Bez1 V1
type B03 = Bez0 V3
type B02 = Bez0 V2
type B01 = Bez0 V1

-- | Quartic polynomial (over a vector field)
data Poly4 c = Poly4 c c c c c
-- | Cubic polynomial (over a vector field)
data Poly3 c = Poly3 c c c c
-- | Quadratic polynomial (over a vector field)
data Poly2 c = Poly2 c c c
-- | Linear polynomial (over a vector field)
data Poly1 c = Poly1 c c
-- | Constant polynomial (over a vector field)
newtype Poly0 c = Poly0 c
derive instance Newtype (Poly0 c) _
derive newtype instance Semiring s => Semiring (Poly0 s)
derive newtype instance Ring s => Ring (Poly0 s)
derive newtype instance CommutativeRing s => CommutativeRing (Poly0 s)
derive newtype instance EuclideanRing s => EuclideanRing (Poly0 s)
derive newtype instance DivisionRing s => DivisionRing (Poly0 s)

instance FunctorWithIndex Unit Poly0 where
  mapWithIndex f = map (f unit)
instance FoldableWithIndex Unit Poly0 where
  foldrWithIndex f = foldr (f unit)
  foldlWithIndex f = foldl (f unit)
  foldMapWithIndex f = foldMap (f unit)
instance TraversableWithIndex Unit Poly0 where
  traverseWithIndex f = traverse (f unit)

newtype PolyN c = PolyN (Array c)
derive newtype instance Functor PolyN
derive newtype instance Foldable PolyN
derive newtype instance Traversable PolyN
derive newtype instance FunctorWithIndex Int PolyN
derive newtype instance FoldableWithIndex Int PolyN
derive newtype instance TraversableWithIndex Int PolyN

instance Semiring s => SModule s (PolyN s) where
  smul x (PolyN ys) = PolyN ((x * _) <$> ys)

instance Semiring s => Monoid (PolyN s) where mempty = zero
instance Semiring s => Semigroup (PolyN s) where append = add

instance Semiring s => Semiring (PolyN s) where
  zero = PolyN []
  one = PolyN [one]
  add (PolyN xs) (PolyN ys) = PolyN $ align (theseop add) xs ys
  mul (PolyN xs) ys = sum $ xs <#>: \i x -> smul x $ shift i ys
    where shift i (PolyN cs) = PolyN $ power [zero] i <> cs
instance Ring s => Ring (PolyN s) where
  sub (PolyN xs) (PolyN ys) = PolyN $ align (theseop sub) xs ys
instance CommutativeRing s => CommutativeRing (PolyN s)

type PolyS4 s = Poly4 (Scalar s)
type PolyS3 s = Poly3 (Scalar s)
type PolyS2 s = Poly2 (Scalar s)
type PolyS1 s = Poly1 (Scalar s)
type PolyS0 s = Poly0 (Scalar s)

components :: forall @vec @f @s. Distributive vec => Functor f => f (vec s) -> vec (f (Scalar s))
components = collect (map V1)

type P43 = Poly4 V3
type P42 = Poly4 V2
type P41 = Poly4 V1
type P33 = Poly3 V3
type P32 = Poly3 V2
type P31 = Poly3 V1
type P23 = Poly2 V3
type P22 = Poly2 V2
type P21 = Poly2 V1
type P13 = Poly1 V3
type P12 = Poly1 V2
type P11 = Poly1 V1
type P03 = Poly0 V3
type P02 = Poly0 V2
type P01 = Poly0 V1

-- | Bounds for an axis
type Bounds s = { min :: Min s, max :: Max s }
type BBox1 s = Vec1 (Bounds s)
type BBox2 s = Vec2 (Bounds s)
type BBox3 s = Vec3 (Bounds s)

mkBound :: forall s. s -> Bounds s
mkBound s = { min: Min s, max: Max s }

justBound :: forall s. s -> Maybe (Bounds s)
justBound = Just <<< mkBound

overBounds :: forall s s'. (s -> s') -> Bounds s -> Bounds s'
overBounds f { min: Min s0, max: Max s1 } = { min: Min (f s0), max: Max (f s1) }

-- corners :: forall f. Foldable1 f => f (Bounds s) -> NonEmptyArray (f Number)
-- corners = ?help

-- tfBounds :: forall t s. Transforms t s => t -> Bounds s -> Bounds s
-- tfBounds t = overBounds (tf t)

corners :: forall s. Vec2 (Bounds s) -> NonEmptyArray (Vec2 s)
corners (V2 { min: Min mx, max: Max mX } { min: Min my, max: Max mY }) =
  NEA.cons' (V2 mx my) [ V2 mx mY, V2 mX my, V2 mX mY ]

tfBounds :: forall t s. Transforms t (Vec2 s) => Ord s => t -> Vec2 (Bounds s) -> Vec2 (Bounds s)
tfBounds t = getBounds <<< map (tf t) <<< corners

getBounds :: forall @f @g @s. Distributive f => Functor g => Foldable1 g => Ord s => g (f s) -> f (Bounds s)
getBounds = collect (map mkBound) >>> map fold1

inBounds :: forall @s. Ord s => Bounds s -> s -> Boolean
inBounds { min: Min bmin, max: Max bmax } value = bmin <= value && value <= bmax

disjointBounds :: forall @f @s. Distributive f => Apply f => Foldable f => Ord s => f (Bounds s) -> f (Bounds s) -> Boolean
disjointBounds ls rs = (ls /|\ rs) # any
  \(Tuple { min: Min lmin, max: Max lmax } { min: Min rmin, max: Max rmax }) ->
    lmax < rmin || lmin > rmax

boundsWithout :: forall @meta @ord. BoundsWith ord meta -> Bounds ord
boundsWithout (BoundsWith (MinWith bmin _) (MaxWith bmax _)) = { min: Min bmin, max: Max bmax }

extent :: forall @s. Ring s => Bounds s -> s
extent { min: Min bmin, max: Max bmax } = bmax - bmin

extents :: forall @f @s. Functor f => Ring s => f (Bounds s) -> f s
extents = map extent

center :: forall @s. Field s => Bounds s -> s
center { min: Min bmin, max: Max bmax } = (bmin + bmax) / (one + one)

bounds2bez :: forall @s. Bounds s -> Bez1 s
bounds2bez { min: Min bmin, max: Max bmax } = B1 bmin bmax

padBounds :: forall @s. Ring s => s -> Bounds s -> Bounds s
padBounds amt { min: Min bmin, max: Max bmax } = { min: Min (bmin - amt), max: Max (bmax + amt) }

boundsNorm :: forall @s. Semiring s => Bounds s
boundsNorm = { min: Min zero, max: Max one }

-- | Helper functions                                                       | --

mkAfn3 :: forall s. Tuple (Vec3 s) (Lin3 s) -> Afn3 s
mkAfn3 (Tuple (V3 t1 t2 t3) (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9)) =
  Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3
unAfn3 :: forall s. Afn3 s -> Tuple (Vec3 s) (Lin3 s)
unAfn3 (Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3) =
  Tuple (V3 t1 t2 t3) (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9)
mkAfn2 :: forall s. Tuple (Vec2 s) (Lin2 s) -> Afn2 s
mkAfn2 (Tuple (V2 t1 t2) (Lin2 c1 c2 c3 c4)) =
  Afn2 c1 c2 c3 c4 t1 t2
unAfn2 :: forall s. Afn2 s -> Tuple (Vec2 s) (Lin2 s)
unAfn2 (Afn2 c1 c2 c3 c4 t1 t2) =
  Tuple (V2 t1 t2) (Lin2 c1 c2 c3 c4)

mkLin3 :: forall s. Vec3 (Vec3 s) -> Lin3 s
mkLin3 (V3 (V3 c1 c2 c3) (V3 c4 c5 c6) (V3 c7 c8 c9)) =
  Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9
unLin3 :: forall s. Lin3 s -> Vec3 (Vec3 s)
unLin3 (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9) =
  V3 (V3 c1 c2 c3) (V3 c4 c5 c6) (V3 c7 c8 c9)
mkLin2 :: forall s. Vec2 (Vec2 s) -> Lin2 s
mkLin2 (V2 (V2 c1 c2) (V2 c3 c4)) = Lin2 c1 c2 c3 c4
unLin2 :: forall s. Lin2 s -> Vec2 (Vec2 s)
unLin2 (Lin2 c1 c2 c3 c4) = V2 (V2 c1 c2) (V2 c3 c4)

scale :: forall @t @s @v. SModule s t => Transforms t v => s -> t
scale s = s .* tfI

translate2 :: forall @s. Semiring s => Vec2 s -> Afn2 s
translate2 v = mkAfn2 (Tuple v tfI)

lineangle :: V2 -> Radians
lineangle (V2 x y) = Math.atan2 y x

rotl2 :: Radians -> Lin2 Number
rotl2 angle = Lin2 (Math.cos angle) (-Math.sin angle) (Math.sin angle) (Math.cos angle)

rota2 :: Radians -> Afn2 Number
rota2 = mkAfn2 <<< Tuple mempty <<< rotl2

d2r = (Math.pi / 180.0) :: Number
r2d = 180.0 / Math.pi :: Number

radiansAround2 :: Radians -> Vec2 Number -> Afn2 Number
radiansAround2 angle = translate2 >>> \t ->
  (t <. rota2 angle) <^ t

degreesAround2 :: Degrees -> Vec2 Number -> Afn2 Number
degreesAround2 = (_ * d2r) >>> radiansAround2

-- Transform one line to the other, while preserving aspect ratio
-- (Rotating and scaling, no skewing)
line2line2 :: B12 -> B12 -> Afn2 Number
line2line2 (B1 p0 p1) (B1 q0 q1) =
  let
    unP = translate2 (inv p0)
    toQ = translate2 q0
    angle = lineangle (q1 <>- q0) - lineangle (p1 <>- p0)
    rot = rota2 angle
    ratio = norm (q1 <>- q0) / norm (p1 <>- p0)
  in unP .> rot .> scale ratio .> toQ

bounds2bounds1 :: Bounds Number -> Bounds Number -> Afn1 Number
bounds2bounds1 { min: Min fmin, max: Max fmax } { min: Min tmin, max: Max tmax } =
  let s = (tmax - tmin)/(fmax - fmin) in
  Afn1 s (tmin - s * fmin)

submat3 :: forall s. Lin3 s -> Lin3 (Lin2 s)
submat3 (Lin3 c1 c2 c3  c4 c5 c6  c7 c8 c9) = Lin3
  (Lin2 c5 c6
        c8 c9)
  (Lin2 c4 c6
        c7 c9)
  (Lin2 c4 c5
        c7 c8)
  ------------
  (Lin2 c2 c3
        c8 c9)
  (Lin2 c1 c3
        c7 c9)
  (Lin2 c1 c2
        c7 c9)
  ------------
  (Lin2 c2 c3
        c5 c6)
  (Lin2 c1 c3
        c4 c6)
  (Lin2 c1 c2
        c4 c5)
cofactors3 :: forall s. Ring s => Lin3 s -> Lin3 s
cofactors3 t =
  let Lin3 c1 c2 c3  c4 c5 c6  c7 c8 c9 = det <$> submat3 t in
  Lin3 (c1) (-c2) (c3)  (-c4) (c5) (-c6)  (c7) (-c8) (c9)


class (Group i, Group o) <= Cross i o | i -> o where
  cross :: i -> i -> o

infix 7 cross as ×

crossS :: forall @i @s. Cross i (Scalar s) => i -> i -> s
crossS i j = unwrap (cross i j)

instance Ring s => Cross (Vec2 s) (Vec1 s) where
  cross (V2 lx ly) (V2 rx ry) = V1 (lx * ry - rx * ly)
instance Ring s => Cross (Vec3 s) (Vec3 s) where
  cross l r = dims <#> \d -> unwrap $
    cross (without d l) (without d r)


class (Align b, Align p) <= Bez2Poly b p | b -> p where
  bez2poly :: forall @v. Group v => b v -> p v
  poly2bez :: forall @v @s. Field s => SModule s v => p v -> b v

bez2polyS :: forall @b @p @v. Bez2Poly b p => Ring v => b v -> p v
bez2polyS = map unwrap <<< bez2poly <<< map V1

poly2bezS :: forall @b @p @v. Bez2Poly b p => Field v => p v -> b v
poly2bezS = map unwrap <<< poly2bez <<< map V1

instance Bez2Poly Bez1 Poly1 where
  bez2poly (B1 p0 p1) = Poly1 p0 (p0 -<> p1)
  poly2bez (Poly1 c0 c1) = B1 c0 (c0 <> c1)
instance Bez2Poly Bez2 Poly2 where
  bez2poly (B2 p0 p1 p2) = Poly2 p0
    (gmul 2 (p0 -<> p1))
    (p0 <>- p1 <> p1 -<> p2)
  poly2bez (Poly2 c0 c1 c2) = B2 c0 (halfOf (c0 <> c1)) (c0 <> c1 <> c2)
    where halfOf = smul (one / (one + one))
instance Bez2Poly Bez3 Poly3 where
  bez2poly (B3 p0 p1 p2 p3) = Poly3
    p0
    (gmul 3 (p0 -<> p1))
    (gmul 3 (p0 <>- p1 <> p1 -<> p2))
    (p0 -<> gmul 3 p1 <> gmul 3 p2 -<> p3)
  poly2bez (Poly3 c0 c1 c2 c3) = B3
    c0
    (c0 <> thirdOf c1)
    (c0 <> mmul 2 (thirdOf c1) <> thirdOf c2)
    (c0 <> c1 <> c2 <> c3)
    where thirdOf = smul (one / (one + one + one))

instance Bez2Poly Poly0 Poly0 where
  bez2poly = identity
  poly2bez = identity
instance Bez2Poly Poly1 Poly1 where
  bez2poly = identity
  poly2bez = identity
instance Bez2Poly Poly2 Poly2 where
  bez2poly = identity
  poly2bez = identity
instance Bez2Poly Poly3 Poly3 where
  bez2poly = identity
  poly2bez = identity
instance Bez2Poly Poly4 Poly4 where
  bez2poly = identity
  poly2bez = identity


-- | Monoid and module instances                                            | --

instance Semiring s => Semigroup (Vec3 s) where append = lift2 (+)
instance Semiring s => Semigroup (Vec2 s) where append = lift2 (+)
instance Semiring s => Semigroup (Vec1 s) where append = lift2 (+)
instance Semiring s => Semigroup (Lin3 s) where append = lift2 (+)
instance Semiring s => Semigroup (Lin2 s) where append = lift2 (+)
instance Semiring s => Semigroup (Lin1 s) where append = lift2 (+)
instance Semiring s => Semigroup (Afn3 s) where append = lift2 (+)
instance Semiring s => Semigroup (Afn2 s) where append = lift2 (+)
instance Semiring s => Semigroup (Afn1 s) where append = lift2 (+)
instance Semiring s => Monoid (Vec3 s) where mempty = pure zero
instance Semiring s => Monoid (Vec2 s) where mempty = pure zero
instance Semiring s => Monoid (Vec1 s) where mempty = pure zero
instance Semiring s => Monoid (Lin3 s) where mempty = pure zero
instance Semiring s => Monoid (Lin2 s) where mempty = pure zero
instance Semiring s => Monoid (Lin1 s) where mempty = pure zero
instance Semiring s => Monoid (Afn3 s) where mempty = pure zero
instance Semiring s => Monoid (Afn2 s) where mempty = pure zero
instance Semiring s => Monoid (Afn1 s) where mempty = pure zero
instance Ring s => Group (Vec3 s) where inv = map negate
instance Ring s => Group (Vec2 s) where inv = map negate
instance Ring s => Group (Vec1 s) where inv = map negate
instance Ring s => Group (Lin3 s) where inv = map negate
instance Ring s => Group (Lin2 s) where inv = map negate
instance Ring s => Group (Lin1 s) where inv = map negate
instance Ring s => Group (Afn3 s) where inv = map negate
instance Ring s => Group (Afn2 s) where inv = map negate
instance Ring s => Group (Afn1 s) where inv = map negate

instance Semiring s => SModule s (Vec3 s) where smul s = map (mul s)
instance Semiring s => SModule s (Vec2 s) where smul s = map (mul s)
instance Semiring s => SModule s (Vec1 s) where smul s = map (mul s)
instance Semiring s => SModule s (Lin3 s) where smul s = map (mul s)
instance Semiring s => SModule s (Lin2 s) where smul s = map (mul s)
instance Semiring s => SModule s (Lin1 s) where smul s = map (mul s)
instance Semiring s => SModule s (Afn3 s) where smul s = map (mul s)
instance Semiring s => SModule s (Afn2 s) where smul s = map (mul s)
instance Semiring s => SModule s (Afn1 s) where smul s = map (mul s)

instance Semigroup v => Semigroup (Bez3 v) where append = lift2 append
instance Semigroup v => Semigroup (Bez2 v) where append = lift2 append
instance Semigroup v => Semigroup (Bez1 v) where append = lift2 append
instance Semigroup v => Semigroup (Poly4 v) where append = lift2 append
instance Semigroup v => Semigroup (Poly3 v) where append = lift2 append
instance Semigroup v => Semigroup (Poly2 v) where append = lift2 append
instance Semigroup v => Semigroup (Poly1 v) where append = lift2 append
instance Semigroup v => Semigroup (Poly0 v) where append = lift2 append
instance Monoid v => Monoid (Bez3 v) where mempty = pure mempty
instance Monoid v => Monoid (Bez2 v) where mempty = pure mempty
instance Monoid v => Monoid (Bez1 v) where mempty = pure mempty
instance Monoid v => Monoid (Poly4 v) where mempty = pure mempty
instance Monoid v => Monoid (Poly3 v) where mempty = pure mempty
instance Monoid v => Monoid (Poly2 v) where mempty = pure mempty
instance Monoid v => Monoid (Poly1 v) where mempty = pure mempty
instance Monoid v => Monoid (Poly0 v) where mempty = pure mempty
instance Group v => Group (Bez3 v) where inv = map inv
instance Group v => Group (Bez2 v) where inv = map inv
instance Group v => Group (Bez1 v) where inv = map inv
instance Group v => Group (Poly4 v) where inv = map inv
instance Group v => Group (Poly3 v) where inv = map inv
instance Group v => Group (Poly2 v) where inv = map inv
instance Group v => Group (Poly1 v) where inv = map inv
instance Group v => Group (Poly0 v) where inv = map inv
instance SModule s v => SModule s (Bez3 v) where smul s = map (smul s)
instance SModule s v => SModule s (Bez2 v) where smul s = map (smul s)
instance SModule s v => SModule s (Bez1 v) where smul s = map (smul s)
instance SModule s v => SModule s (Poly4 v) where smul s = map (smul s)
instance SModule s v => SModule s (Poly3 v) where smul s = map (smul s)
instance SModule s v => SModule s (Poly2 v) where smul s = map (smul s)
instance SModule s v => SModule s (Poly1 v) where smul s = map (smul s)
instance SModule s v => SModule s (Poly0 v) where smul s = map (smul s)

-- | Matrix algebra instances                                               | --

instance Semiring s => Transforms (Lin3 s) (Vec3 s) where
  tf (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9) (V3 s1 s2 s3) = V3
    (c1 * s1 + c2 * s2 + c3 * s3)
    (c4 * s1 + c5 * s2 + c6 * s3)
    (c7 * s1 + c8 * s2 + c9 * s3)
  tfI = Lin3
    one  zero zero
    zero one  zero
    zero zero one
  tfC (Lin3 b1 b2 b3 b4 b5 b6 b7 b8 b9) (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9) = Lin3
    (b1 * c1 + b2 * c4 + b3 * c7)
    (b1 * c2 + b2 * c5 + b3 * c8)
    (b1 * c3 + b2 * c6 + b3 * c9)
    (b4 * c1 + b5 * c4 + b6 * c7)
    (b4 * c2 + b5 * c5 + b6 * c8)
    (b4 * c3 + b5 * c6 + b6 * c9)
    (b7 * c1 + b8 * c4 + b9 * c7)
    (b7 * c2 + b8 * c5 + b9 * c8)
    (b7 * c3 + b8 * c6 + b9 * c9)
instance Ring s => STransforms s (Lin3 s) (Vec3 s) where
  det (Lin3 c1 c2 c3  c4 c5 c6  c7 c8 c9) =
    c1 * (c5 * c9 - c6 * c8) -
    c2 * (c4 * c9 - c6 * c7) +
    c3 * (c4 * c8 - c5 * c7)
instance Field s => Untransforms (Lin3 s) (Vec3 s) where
  tfR t = smul (recip (det t)) $ cofactors3 t

instance Semiring s => Transforms (Afn3 s) (Vec3 s) where
  tf (Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3) (V3 s1 s2 s3) = V3
    (t1 + c1 * s1 + c2 * s2 + c3 * s3)
    (t2 + c4 * s1 + c5 * s2 + c6 * s3)
    (t3 + c7 * s1 + c8 * s2 + c9 * s3)
  tfI = Afn3
    one  zero zero
    zero one  zero
    zero zero one
    zero zero zero
  tfC (Afn3 b1 b2 b3 b4 b5 b6 b7 b8 b9 s1 s2 s3) (Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3) = Afn3
    (b1 * c1 + b2 * c4 + b3 * c7)
    (b1 * c2 + b2 * c5 + b3 * c8)
    (b1 * c3 + b2 * c6 + b3 * c9)
    (b4 * c1 + b5 * c4 + b6 * c7)
    (b4 * c2 + b5 * c5 + b6 * c8)
    (b4 * c3 + b5 * c6 + b6 * c9)
    (b7 * c1 + b8 * c4 + b9 * c7)
    (b7 * c2 + b8 * c5 + b9 * c8)
    (b7 * c3 + b8 * c6 + b9 * c9)
    (s1 + u1) (s2 + u2) (s3 + u3)
    where
    V3 u1 u2 u3 = Lin3 b1 b2 b3 b4 b5 b6 b7 b8 b9 `tf` V3 t1 t2 t3
instance Ring s => STransforms s (Afn3 s) (Vec3 s) where
  det = det <<< snd <<< unAfn3
instance Field s => Untransforms (Afn3 s) (Vec3 s) where
  tfR = dimap unAfn3 mkAfn3 \(Tuple v t) ->
    let s = tfR t in Tuple (inv (tf s v)) s

instance Semiring s => Transforms (Lin2 s) (Vec2 s) where
  tf (Lin2 c1 c2 c3 c4) (V2 s1 s2) = V2
    (c1 * s1 + c2 * s2)
    (c3 * s1 + c4 * s2)
  tfI = Lin2
    one  zero
    zero one
  tfC (Lin2 b1 b2 b3 b4) (Lin2 c1 c2 c3 c4) = Lin2
    (b1 * c1 + b2 * c3)
    (b1 * c2 + b2 * c4)
    (b3 * c1 + b4 * c3)
    (b3 * c2 + b4 * c4)
instance Ring s => STransforms s (Lin2 s) (Vec2 s) where
  det (Lin2 c1 c2 c3 c4) = c1 * c4 - c2 * c3
instance Field s => Untransforms (Lin2 s) (Vec2 s) where
  tfR t@(Lin2 c1 c2 c3 c4) = smul (recip (det t)) $ Lin2
    (c4) (-c2)
    (-c3) (c1)

instance Semiring s => Transforms (Lin1 s) (Vec1 s) where
  tf (Lin1 c1) (V1 s1) = V1 (c1 * s1)
  tfI = Lin1 one
  tfC (Lin1 b1) (Lin1 c1) = Lin1 (b1 * c1)
instance Ring s => STransforms s (Lin1 s) (Vec1 s) where
  det (Lin1 c1) = c1
instance Field s => Untransforms (Lin1 s) (Vec1 s) where
  tfR (Lin1 c1) = Lin1 (recip c1)

instance Semiring s => Transforms (Afn2 s) (Vec2 s) where
  tf (Afn2 c1 c2 c3 c4 t1 t2) (V2 s1 s2) = V2
    (t1 + c1 * s1 + c2 * s2)
    (t2 + c3 * s1 + c4 * s2)
  tfI = Afn2
    one  zero
    zero one
    zero zero
  tfC (Afn2 b1 b2 b3 b4 s1 s2) (Afn2 c1 c2 c3 c4 t1 t2) = Afn2
    (b1 * c1 + b2 * c3)
    (b1 * c2 + b2 * c4)
    (b3 * c1 + b4 * c3)
    (b3 * c2 + b4 * c4)
    (s1 + u1) (s2 + u2)
    where
    V2 u1 u2 = Lin2 b1 b2 b3 b4 `tf` V2 t1 t2
instance Ring s => STransforms s (Afn2 s) (Vec2 s) where
  det = det <<< snd <<< unAfn2
instance Field s => Untransforms (Afn2 s) (Vec2 s) where
  tfR = dimap unAfn2 mkAfn2 \(Tuple v t) ->
    let s = tfR t in Tuple (inv (tf s v)) s

instance Semiring s => Transforms (Afn1 s) (Vec1 s) where
  tf (Afn1 c1 t1) (V1 s1) = V1 (t1 + c1 * s1)
  tfI = Afn1 one zero
  tfC (Afn1 b1 s1) (Afn1 c1 t1) = Afn1 (b1 * c1) (s1 + b1 * t1)
instance Ring s => STransforms s (Afn1 s) (Vec1 s) where
  det (Afn1 c1 _) = c1
instance Field s => Untransforms (Afn1 s) (Vec1 s) where
  tfR (Afn1 c1 t1) = Afn1 (recip c1) (negate $ recip c1 * t1)

-- | Vector translation
instance Semiring s => Transforms (Vec3 s) (Vec3 s) where
  tf = (<>)
  tfI = mempty
  tfC = (<>)
instance Ring s => Untransforms (Vec3 s) (Vec3 s) where
  tfR = inv
-- | Vector translation
instance Semiring s => Transforms (Vec2 s) (Vec2 s) where
  tf = (<>)
  tfI = mempty
  tfC = (<>)
instance Ring s => Untransforms (Vec2 s) (Vec2 s) where
  tfR = inv
-- | Vector translation
instance Semiring s => Transforms (Vec1 s) (Vec1 s) where
  tf = (<>)
  tfI = mempty
  tfC = (<>)
instance Ring s => Untransforms (Vec1 s) (Vec1 s) where
  tfR = inv


-- | Boring structural instances                                            | --

class (Traversable t, Traversable t') <= Pairs t t' | t -> t' where
  pairsWith :: forall v r. (v -> v -> r) -> t v -> t' r
  extendL :: forall v. v -> t' v -> t v
  extendR :: forall v. t' v -> v -> t v

pairs :: forall @t @t' v. Pairs t t' => t v -> t' (Pair v)
pairs = pairsWith Pair

infixr 3 extendL as <:
infixl 3 extendR as :>

instance Pairs Bez3 Bez2 where
  pairsWith pair (B3 p0 p1 p2 p3) = B2 (pair p0 p1) (pair p1 p2) (pair p2 p3)
  extendL p0 (B2 p1 p2 p3) = B3 p0 p1 p2 p3
  extendR (B2 p0 p1 p2) p3 = B3 p0 p1 p2 p3
instance Pairs Bez2 Bez1 where
  pairsWith pair (B2 p0 p1 p2) = B1 (pair p0 p1) (pair p1 p2)
  extendL p0 (B1 p1 p2) = B2 p0 p1 p2
  extendR (B1 p0 p1) p2 = B2 p0 p1 p2
instance Pairs Bez1 Bez0 where
  pairsWith pair (B1 p0 p1) = Poly0 (pair p0 p1)
  extendL p0 (Poly0 p1) = B1 p0 p1
  extendR (Poly0 p0) p1 = B1 p0 p1

instance Pairs Poly4 Poly3 where
  pairsWith pair (Poly4 c0 c1 c2 c3 c4) = Poly3 (pair c0 c1) (pair c1 c2) (pair c2 c3) (pair c3 c4)
  extendL c0 (Poly3 c1 c2 c3 c4) = Poly4 c0 c1 c2 c3 c4
  extendR (Poly3 c0 c1 c2 c3) c4 = Poly4 c0 c1 c2 c3 c4
instance Pairs Poly3 Poly2 where
  pairsWith pair (Poly3 c0 c1 c2 c3) = Poly2 (pair c0 c1) (pair c1 c2) (pair c2 c3)
  extendL c0 (Poly2 c1 c2 c3) = Poly3 c0 c1 c2 c3
  extendR (Poly2 c0 c1 c2) c3 = Poly3 c0 c1 c2 c3
instance Pairs Poly2 Poly1 where
  pairsWith pair (Poly2 c0 c1 c2) = Poly1 (pair c0 c1) (pair c1 c2)
  extendL p0 (Poly1 p1 p2) = Poly2 p0 p1 p2
  extendR (Poly1 p0 p1) p2 = Poly2 p0 p1 p2
instance Pairs Poly1 Poly0 where
  pairsWith pair (Poly1 c0 c1) = Poly0 (pair c0 c1)
  extendL p0 (Poly0 p1) = Poly1 p0 p1
  extendR (Poly0 p0) p1 = Poly1 p0 p1

instance Pairs Array Array where
  pairsWith pair vs = Array.zipWith pair vs (Array.drop 1 vs)
  extendL = Array.cons
  extendR = Array.snoc
instance Pairs NonEmptyArray Array where
  pairsWith pair vs = Array.zipWith pair (NEA.toArray vs) (NEA.drop 1 vs)
  extendL = NEA.cons'
  extendR = NEA.snoc'
instance Pairs PolyN PolyN where
  pairsWith f = coerce (pairsWith @Array @Array f)
  extendL :: forall v. v -> PolyN v -> PolyN v
  extendL = coerce (extendL :: v -> Array v -> Array v)
  extendR :: forall v. PolyN v -> v -> PolyN v
  extendR = coerce (extendR :: Array v -> v -> Array v)

derive instance Eq t => Eq (Vec3 t)
derive instance Eq t => Eq (Vec2 t)
derive instance Eq t => Eq (Vec1 t)
derive instance Eq t => Eq (Lin3 t)
derive instance Eq t => Eq (Lin2 t)
derive instance Eq t => Eq (Lin1 t)
derive instance Eq t => Eq (Afn3 t)
derive instance Eq t => Eq (Afn2 t)
derive instance Eq t => Eq (Afn1 t)
derive instance Eq t => Eq (Bez3 t)
derive instance Eq t => Eq (Bez2 t)
derive instance Eq t => Eq (Bez1 t)
derive instance Eq t => Eq (Poly4 t)
derive instance Eq t => Eq (Poly3 t)
derive instance Eq t => Eq (Poly2 t)
derive instance Eq t => Eq (Poly1 t)
derive instance Eq t => Eq (Poly0 t)

derive instance Ord t => Ord (Vec3 t)
derive instance Ord t => Ord (Vec2 t)
derive instance Ord t => Ord (Vec1 t)
derive instance Ord t => Ord (Lin3 t)
derive instance Ord t => Ord (Lin2 t)
derive instance Ord t => Ord (Lin1 t)
derive instance Ord t => Ord (Afn3 t)
derive instance Ord t => Ord (Afn2 t)
derive instance Ord t => Ord (Afn1 t)
derive instance Ord t => Ord (Bez3 t)
derive instance Ord t => Ord (Bez2 t)
derive instance Ord t => Ord (Bez1 t)
derive instance Ord t => Ord (Poly4 t)
derive instance Ord t => Ord (Poly3 t)
derive instance Ord t => Ord (Poly2 t)
derive instance Ord t => Ord (Poly1 t)
derive instance Ord t => Ord (Poly0 t)

derive instance Eq1 Vec3
derive instance Eq1 Vec2
derive instance Eq1 Vec1
derive instance Eq1 Lin3
derive instance Eq1 Lin2
derive instance Eq1 Lin1
derive instance Eq1 Afn3
derive instance Eq1 Afn2
derive instance Eq1 Afn1
derive instance Eq1 Bez3
derive instance Eq1 Bez2
derive instance Eq1 Bez1
derive instance Eq1 Poly4
derive instance Eq1 Poly3
derive instance Eq1 Poly2
derive instance Eq1 Poly1
derive instance Eq1 Poly0

derive instance Ord1 Vec3
derive instance Ord1 Vec2
derive instance Ord1 Vec1
derive instance Ord1 Lin3
derive instance Ord1 Lin2
derive instance Ord1 Lin1
derive instance Ord1 Afn3
derive instance Ord1 Afn2
derive instance Ord1 Afn1
derive instance Ord1 Bez3
derive instance Ord1 Bez2
derive instance Ord1 Bez1
derive instance Ord1 Poly4
derive instance Ord1 Poly3
derive instance Ord1 Poly2
derive instance Ord1 Poly1
derive instance Ord1 Poly0

derive instance Functor Vec3
derive instance Functor Vec2
derive instance Functor Vec1
derive instance Functor Lin3
derive instance Functor Lin2
derive instance Functor Lin1
derive instance Functor Afn3
derive instance Functor Afn2
derive instance Functor Afn1
derive instance Functor Bez3
derive instance Functor Bez2
derive instance Functor Bez1
derive instance Functor Poly4
derive instance Functor Poly3
derive instance Functor Poly2
derive instance Functor Poly1
derive instance Functor Poly0

derive instance Foldable Vec3
derive instance Foldable Vec2
derive instance Foldable Vec1
derive instance Foldable Lin3
derive instance Foldable Lin2
derive instance Foldable Lin1
derive instance Foldable Afn3
derive instance Foldable Afn2
derive instance Foldable Afn1
derive instance Foldable Bez3
derive instance Foldable Bez2
derive instance Foldable Bez1
derive instance Foldable Poly4
derive instance Foldable Poly3
derive instance Foldable Poly2
derive instance Foldable Poly1
derive instance Foldable Poly0

derive instance Traversable Vec3
derive instance Traversable Vec2
derive instance Traversable Vec1
derive instance Traversable Lin3
derive instance Traversable Lin2
derive instance Traversable Lin1
derive instance Traversable Afn3
derive instance Traversable Afn2
derive instance Traversable Afn1
derive instance Traversable Bez3
derive instance Traversable Bez2
derive instance Traversable Bez1
derive instance Traversable Poly4
derive instance Traversable Poly3
derive instance Traversable Poly2
derive instance Traversable Poly1
derive instance Traversable Poly0

instance Monad Vec3
instance Monad Vec2
instance Monad Vec1
instance Monad Lin3
instance Monad Lin2
instance Monad Lin1
instance Monad Afn3
instance Monad Afn2
instance Monad Afn1
instance Monad Bez3
instance Monad Bez2
instance Monad Bez1
instance Monad Poly4
instance Monad Poly3
instance Monad Poly2
instance Monad Poly1
instance Monad Poly0

derive instance Generic (Vec3 t) _
derive instance Generic (Vec2 t) _
derive instance Generic (Vec1 t) _
derive instance Generic (Lin3 t) _
derive instance Generic (Lin2 t) _
derive instance Generic (Lin1 t) _
derive instance Generic (Afn3 t) _
derive instance Generic (Afn2 t) _
derive instance Generic (Afn1 t) _
derive instance Generic (Bez3 t) _
derive instance Generic (Bez2 t) _
derive instance Generic (Bez1 t) _
derive instance Generic (Poly4 t) _
derive instance Generic (Poly3 t) _
derive instance Generic (Poly2 t) _
derive instance Generic (Poly1 t) _
derive instance Generic (Poly0 t) _

instance Show t => Show (Vec3 t) where show = genericShow
instance Show t => Show (Vec2 t) where show = genericShow
instance Show t => Show (Vec1 t) where show = genericShow
instance Show t => Show (Lin3 t) where show = genericShow
instance Show t => Show (Lin2 t) where show = genericShow
instance Show t => Show (Lin1 t) where show = genericShow
instance Show t => Show (Afn3 t) where show = genericShow
instance Show t => Show (Afn2 t) where show = genericShow
instance Show t => Show (Afn1 t) where show = genericShow
instance Show t => Show (Bez3 t) where show = genericShow
instance Show t => Show (Bez2 t) where show = genericShow
instance Show t => Show (Bez1 t) where show = genericShow
instance Show t => Show (Poly4 t) where show = genericShow
instance Show t => Show (Poly3 t) where show = genericShow
instance Show t => Show (Poly2 t) where show = genericShow
instance Show t => Show (Poly1 t) where show = genericShow
instance Show t => Show (Poly0 t) where show = genericShow


instance Foldable1 Vec3 where
  foldl1 f (V3 s1 s2 s3) = s1 `f` s2 `f` s3
  foldr1 f (V3 s1 s2 s3) = f s1 $ f s2 s3
  foldMap1 f (V3 s1 s2 s3) = f s1 <> f s2 <> f s3
instance Foldable1 Vec2 where
  foldl1 f (V2 s1 s2) = f s1 s2
  foldr1 f (V2 s1 s2) = f s1 s2
  foldMap1 f (V2 s1 s2) = f s1 <> f s2
instance Foldable1 Vec1 where
  foldl1 _ (V1 s1) = s1
  foldr1 _ (V1 s1) = s1
  foldMap1 f (V1 s1) = f s1
instance Foldable1 Lin3 where
  foldl1 f (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9) = c1 `f` c2 `f` c3 `f` c4 `f` c5 `f` c6 `f` c7 `f` c8 `f` c9
  foldr1 f (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9) = f c1 $ f c2 $ f c3 $ f c4 $ f c5 $ f c6 $ f c7 $ f c8 c9
  foldMap1 f (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9) = f c1 <> f c2 <> f c3 <> f c4 <> f c5 <> f c6 <> f c7 <> f c8 <> f c9
instance Foldable1 Lin2 where
  foldl1 f (Lin2 c1 c2 c3 c4) = c1 `f` c2 `f` c3 `f` c4
  foldr1 f (Lin2 c1 c2 c3 c4) = f c1 $ f c2 $ f c3 c4
  foldMap1 f (Lin2 c1 c2 c3 c4) = f c1 <> f c2 <> f c3 <> f c4
instance Foldable1 Lin1 where
  foldl1 _ (Lin1 c1) = c1
  foldr1 _ (Lin1 c1) = c1
  foldMap1 f (Lin1 c1) = f c1
instance Foldable1 Afn3 where
  foldl1 f (Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3) = c1 `f` c2 `f` c3 `f` c4 `f` c5 `f` c6 `f` c7 `f` c8 `f` c9 `f` t1 `f` t2 `f` t3
  foldr1 f (Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3) = f c1 $ f c2 $ f c3 $ f c4 $ f c5 $ f c6 $ f c7 $ f c8 $ f c9 $ f t1 $ f t2 t3
  foldMap1 f (Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3) = f c1 <> f c2 <> f c3 <> f c4 <> f c5 <> f c6 <> f c7 <> f c8 <> f c9 <> f t1 <> f t2 <> f t3
instance Foldable1 Afn2 where
  foldl1 f (Afn2 c1 c2 c3 c4 t1 t2) = c1 `f` c2 `f` c3 `f` c4 `f` t1 `f` t2
  foldr1 f (Afn2 c1 c2 c3 c4 t1 t2) = f c1 $ f c2 $ f c3 $ f c4 $ f t1 t2
  foldMap1 f (Afn2 c1 c2 c3 c4 t1 t2) = f c1 <> f c2 <> f c3 <> f c4 <> f t1 <> f t2
instance Foldable1 Afn1 where
  foldl1 f (Afn1 c1 t1) = f c1 t1
  foldr1 f (Afn1 c1 t1) = f c1 t1
  foldMap1 f (Afn1 c1 t1) = f c1 <> f t1
instance Foldable1 Bez3 where
  foldl1 f (B3 p0 p1 p2 p3) = p0 `f` p1 `f` p2 `f` p3
  foldr1 f (B3 p0 p1 p2 p3) = f p0 $ f p1 $ f p2 p3
  foldMap1 f (B3 p0 p1 p2 p3) = f p0 <> f p1 <> f p2 <> f p3
instance Foldable1 Bez2 where
  foldl1 f (B2 p0 p1 p2) = p0 `f` p1 `f` p2
  foldr1 f (B2 p0 p1 p2) = f p0 $ f p1 p2
  foldMap1 f (B2 p0 p1 p2) = f p0 <> f p1 <> f p2
instance Foldable1 Bez1 where
  foldl1 f (B1 p0 p1) = p0 `f` p1
  foldr1 f (B1 p0 p1) = f p0 p1
  foldMap1 f (B1 p0 p1) = f p0 <> f p1

instance Traversable1 Vec3 where
  traverse1 f (V3 s1 s2 s3) = V3 <$> f s1 <*> f s2 <*> f s3
  sequence1 (V3 s1 s2 s3) = V3 <$> s1 <*> s2 <*> s3
instance Traversable1 Vec2 where
  traverse1 f (V2 s1 s2) = V2 <$> f s1 <*> f s2
  sequence1 (V2 s1 s2) = V2 <$> s1 <*> s2
instance Traversable1 Vec1 where
  traverse1 f (V1 s1) = V1 <$> f s1
  sequence1 (V1 s1) = V1 <$> s1
instance Traversable1 Lin3 where
  traverse1 f (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9) = Lin3 <$> f c1 <*> f c2 <*> f c3 <*> f c4 <*> f c5 <*> f c6 <*> f c7 <*> f c8 <*> f c9
  sequence1 (Lin3 c1 c2 c3 c4 c5 c6 c7 c8 c9) = Lin3 <$> c1 <*> c2 <*> c3 <*> c4 <*> c5 <*> c6 <*> c7 <*> c8 <*> c9
instance Traversable1 Lin2 where
  traverse1 f (Lin2 c1 c2 c3 c4) = Lin2 <$> f c1 <*> f c2 <*> f c3 <*> f c4
  sequence1 (Lin2 c1 c2 c3 c4) = Lin2 <$> c1 <*> c2 <*> c3 <*> c4
instance Traversable1 Lin1 where
  traverse1 f (Lin1 c1) = Lin1 <$> f c1
  sequence1 (Lin1 c1) = Lin1 <$> c1
instance Traversable1 Afn3 where
  traverse1 f (Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3) = Afn3 <$> f c1 <*> f c2 <*> f c3 <*> f c4 <*> f c5 <*> f c6 <*> f c7 <*> f c8 <*> f c9 <*> f t1 <*> f t2 <*> f t3
  sequence1 (Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3) = Afn3 <$> c1 <*> c2 <*> c3 <*> c4 <*> c5 <*> c6 <*> c7 <*> c8 <*> c9 <*> t1 <*> t2 <*> t3
instance Traversable1 Afn2 where
  traverse1 f (Afn2 c1 c2 c3 c4 t1 t2) = Afn2 <$> f c1 <*> f c2 <*> f c3 <*> f c4 <*> f t1 <*> f t2
  sequence1 (Afn2 c1 c2 c3 c4 t1 t2) = Afn2 <$> c1 <*> c2 <*> c3 <*> c4 <*> t1 <*> t2
instance Traversable1 Afn1 where
  traverse1 f (Afn1 c1 t1) = Afn1 <$> f c1 <*> f t1
  sequence1 (Afn1 c1 t1) = Afn1 <$> c1 <*> t1
instance Traversable1 Bez3 where
  traverse1 f (B3 p0 p1 p2 p3) = B3 <$> f p0 <*> f p1 <*> f p2 <*> f p3
  sequence1 (B3 p0 p1 p2 p3) = B3 <$> p0 <*> p1 <*> p2 <*> p3
instance Traversable1 Bez2 where
  traverse1 f (B2 p0 p1 p2) = B2 <$> f p0 <*> f p1 <*> f p2
  sequence1 (B2 p0 p1 p2) = B2 <$> p0 <*> p1 <*> p2
instance Traversable1 Bez1 where
  traverse1 f (B1 p0 p1) = B1 <$> f p0 <*> f p1
  sequence1 (B1 p0 p1) = B1 <$> p0 <*> p1

instance Distributive Vec3 where
  collect = collectDefault
  distribute g = V3
    (g <#> \(V3 s _ _) -> s)
    (g <#> \(V3 _ s _) -> s)
    (g <#> \(V3 _ _ s) -> s)
instance Distributive Vec2 where
  collect = collectDefault
  distribute g = V2
    (g <#> \(V2 s _) -> s)
    (g <#> \(V2 _ s) -> s)
instance Distributive Vec1 where
  collect = collectDefault
  distribute g = V1
    (g <#> \(V1 s) -> s)
instance Distributive Lin3 where
  collect = collectDefault
  distribute g = Lin3
    (g <#> \(Lin3 c _ _ _ _ _ _ _ _) -> c)
    (g <#> \(Lin3 _ c _ _ _ _ _ _ _) -> c)
    (g <#> \(Lin3 _ _ c _ _ _ _ _ _) -> c)
    (g <#> \(Lin3 _ _ _ c _ _ _ _ _) -> c)
    (g <#> \(Lin3 _ _ _ _ c _ _ _ _) -> c)
    (g <#> \(Lin3 _ _ _ _ _ c _ _ _) -> c)
    (g <#> \(Lin3 _ _ _ _ _ _ c _ _) -> c)
    (g <#> \(Lin3 _ _ _ _ _ _ _ c _) -> c)
    (g <#> \(Lin3 _ _ _ _ _ _ _ _ c) -> c)
instance Distributive Lin2 where
  collect = collectDefault
  distribute g = Lin2
    (g <#> \(Lin2 c _ _ _) -> c)
    (g <#> \(Lin2 _ c _ _) -> c)
    (g <#> \(Lin2 _ _ c _) -> c)
    (g <#> \(Lin2 _ _ _ c) -> c)
instance Distributive Lin1 where
  collect = collectDefault
  distribute g = Lin1
    (g <#> \(Lin1 c) -> c)
instance Distributive Afn3 where
  collect = collectDefault
  distribute g = Afn3
    (g <#> \(Afn3 c _ _ _ _ _ _ _ _ _ _ _) -> c)
    (g <#> \(Afn3 _ c _ _ _ _ _ _ _ _ _ _) -> c)
    (g <#> \(Afn3 _ _ c _ _ _ _ _ _ _ _ _) -> c)
    (g <#> \(Afn3 _ _ _ c _ _ _ _ _ _ _ _) -> c)
    (g <#> \(Afn3 _ _ _ _ c _ _ _ _ _ _ _) -> c)
    (g <#> \(Afn3 _ _ _ _ _ c _ _ _ _ _ _) -> c)
    (g <#> \(Afn3 _ _ _ _ _ _ c _ _ _ _ _) -> c)
    (g <#> \(Afn3 _ _ _ _ _ _ _ c _ _ _ _) -> c)
    (g <#> \(Afn3 _ _ _ _ _ _ _ _ c _ _ _) -> c)
    (g <#> \(Afn3 _ _ _ _ _ _ _ _ _ t _ _) -> t)
    (g <#> \(Afn3 _ _ _ _ _ _ _ _ _ _ t _) -> t)
    (g <#> \(Afn3 _ _ _ _ _ _ _ _ _ _ _ t) -> t)
instance Distributive Afn2 where
  collect = collectDefault
  distribute g = Afn2
    (g <#> \(Afn2 c _ _ _ _ _) -> c)
    (g <#> \(Afn2 _ c _ _ _ _) -> c)
    (g <#> \(Afn2 _ _ c _ _ _) -> c)
    (g <#> \(Afn2 _ _ _ c _ _) -> c)
    (g <#> \(Afn2 _ _ _ _ t _) -> t)
    (g <#> \(Afn2 _ _ _ _ _ t) -> t)
instance Distributive Afn1 where
  collect = collectDefault
  distribute g = Afn1
    (g <#> \(Afn1 c _) -> c)
    (g <#> \(Afn1 _ t) -> t)
instance Distributive Bez3 where
  collect = collectDefault
  distribute g = B3
    (g <#> \(B3 p _ _ _) -> p)
    (g <#> \(B3 _ p _ _) -> p)
    (g <#> \(B3 _ _ p _) -> p)
    (g <#> \(B3 _ _ _ p) -> p)
instance Distributive Bez2 where
  collect = collectDefault
  distribute g = B2
    (g <#> \(B2 p _ _) -> p)
    (g <#> \(B2 _ p _) -> p)
    (g <#> \(B2 _ _ p) -> p)
instance Distributive Bez1 where
  collect = collectDefault
  distribute g = B1
    (g <#> \(B1 p _) -> p)
    (g <#> \(B1 _ p) -> p)
instance Distributive Poly4 where
  collect = collectDefault
  distribute g = Poly4
    (g <#> \(Poly4 c _ _ _ _) -> c)
    (g <#> \(Poly4 _ c _ _ _) -> c)
    (g <#> \(Poly4 _ _ c _ _) -> c)
    (g <#> \(Poly4 _ _ _ c _) -> c)
    (g <#> \(Poly4 _ _ _ _ c) -> c)
instance Distributive Poly3 where
  collect = collectDefault
  distribute g = Poly3
    (g <#> \(Poly3 c _ _ _) -> c)
    (g <#> \(Poly3 _ c _ _) -> c)
    (g <#> \(Poly3 _ _ c _) -> c)
    (g <#> \(Poly3 _ _ _ c) -> c)
instance Distributive Poly2 where
  collect = collectDefault
  distribute g = Poly2
    (g <#> \(Poly2 c _ _) -> c)
    (g <#> \(Poly2 _ c _) -> c)
    (g <#> \(Poly2 _ _ c) -> c)
instance Distributive Poly1 where
  collect = collectDefault
  distribute g = Poly1
    (g <#> \(Poly1 c _) -> c)
    (g <#> \(Poly1 _ c) -> c)
instance Distributive Poly0 where
  collect = collectDefault
  distribute g = Poly0 (g <#> \(Poly0 c) -> c)

instance Apply Vec3 where
  apply (V3 f1 f2 f3) (V3 a1 a2 a3) = V3 (f1 a1) (f2 a2) (f3 a3)
instance Apply Vec2 where
  apply (V2 f1 f2) (V2 a1 a2) = V2 (f1 a1) (f2 a2)
instance Apply Vec1 where
  apply (V1 f1) (V1 a1) = V1 (f1 a1)
instance Apply Lin3 where
  apply (Lin3 f1 f2 f3 f4 f5 f6 f7 f8 f9) (Lin3 a1 a2 a3 a4 a5 a6 a7 a8 a9) = Lin3 (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9)
instance Apply Lin2 where
  apply (Lin2 f1 f2 f3 f4) (Lin2 a1 a2 a3 a4) = Lin2 (f1 a1) (f2 a2) (f3 a3) (f4 a4)
instance Apply Lin1 where
  apply (Lin1 f1) (Lin1 a1) = Lin1 (f1 a1)
instance Apply Afn3 where
  apply (Afn3 f1 f2 f3 f4 f5 f6 f7 f8 f9 g1 g2 g3) (Afn3 a1 a2 a3 a4 a5 a6 a7 a8 a9 b1 b2 b3) = Afn3 (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6) (f7 a7) (f8 a8) (f9 a9) (g1 b1) (g2 b2) (g3 b3)
instance Apply Afn2 where
  apply (Afn2 f1 f2 f3 f4 g1 g2) (Afn2 a1 a2 a3 a4 b1 b2) = Afn2 (f1 a1) (f2 a2) (f3 a3) (f4 a4) (g1 b1) (g2 b2)
instance Apply Afn1 where
  apply (Afn1 f1 f2) (Afn1 a1 a2) = Afn1 (f1 a1) (f2 a2)
instance Apply Bez3 where
  apply (B3 f0 f1 f2 f3) (B3 a0 a1 a2 a3) = B3 (f0 a0) (f1 a1) (f2 a2) (f3 a3)
instance Apply Bez2 where
  apply (B2 f0 f1 f2) (B2 a0 a1 a2) = B2 (f0 a0) (f1 a1) (f2 a2)
instance Apply Bez1 where
  apply (B1 f0 f1) (B1 a0 a1) = B1 (f0 a0) (f1 a1)
instance Apply Poly4 where
  apply (Poly4 f0 f1 f2 f3 f4) (Poly4 a0 a1 a2 a3 a4) = Poly4 (f0 a0) (f1 a1) (f2 a2) (f3 a3) (f4 a4)
instance Apply Poly3 where
  apply (Poly3 f0 f1 f2 f3) (Poly3 a0 a1 a2 a3) = Poly3 (f0 a0) (f1 a1) (f2 a2) (f3 a3)
instance Apply Poly2 where
  apply (Poly2 f0 f1 f2) (Poly2 a0 a1 a2) = Poly2 (f0 a0) (f1 a1) (f2 a2)
instance Apply Poly1 where
  apply (Poly1 f0 f1) (Poly1 a0 a1) = Poly1 (f0 a0) (f1 a1)
instance Apply Poly0 where
  apply (Poly0 f0) (Poly0 a0) = Poly0 (f0 a0)
instance Applicative Vec3 where
  pure a = V3 a a a
instance Applicative Vec2 where
  pure a = V2 a a
instance Applicative Vec1 where
  pure a = V1 a
instance Applicative Lin3 where
  pure a = Lin3 a a a a a a a a a
instance Applicative Lin2 where
  pure a = Lin2 a a a a
instance Applicative Lin1 where
  pure a = Lin1 a
instance Applicative Afn3 where
  pure a = Afn3 a a a a a a a a a a a a
instance Applicative Afn2 where
  pure a = Afn2 a a a a a a
instance Applicative Afn1 where
  pure a = Afn1 a a
instance Applicative Bez3 where
  pure a = B3 a a a a
instance Applicative Bez2 where
  pure a = B2 a a a
instance Applicative Bez1 where
  pure a = B1 a a
instance Applicative Poly4 where
  pure a = Poly4 a a a a a
instance Applicative Poly3 where
  pure a = Poly3 a a a a
instance Applicative Poly2 where
  pure a = Poly2 a a a
instance Applicative Poly1 where
  pure a = Poly1 a a
instance Applicative Poly0 where
  pure a = Poly0 a
instance Bind Vec3 where
  bind as f = distribute f <*> as
instance Bind Vec2 where
  bind as f = distribute f <*> as
instance Bind Vec1 where
  bind as f = distribute f <*> as
instance Bind Lin3 where
  bind as f = distribute f <*> as
instance Bind Lin2 where
  bind as f = distribute f <*> as
instance Bind Lin1 where
  bind as f = distribute f <*> as
instance Bind Afn3 where
  bind as f = distribute f <*> as
instance Bind Afn2 where
  bind as f = distribute f <*> as
instance Bind Afn1 where
  bind as f = distribute f <*> as
instance Bind Bez3 where
  bind as f = distribute f <*> as
instance Bind Bez2 where
  bind as f = distribute f <*> as
instance Bind Bez1 where
  bind as f = distribute f <*> as
instance Bind Poly4 where
  bind as f = distribute f <*> as
instance Bind Poly3 where
  bind as f = distribute f <*> as
instance Bind Poly2 where
  bind as f = distribute f <*> as
instance Bind Poly1 where
  bind as f = distribute f <*> as
instance Bind Poly0 where
  bind as f = distribute f <*> as
instance Align Vec3 where align f = lift2 \x y -> f (Both x y)
instance Align Vec2 where align f = lift2 \x y -> f (Both x y)
instance Align Vec1 where align f = lift2 \x y -> f (Both x y)
instance Align Lin3 where align f = lift2 \x y -> f (Both x y)
instance Align Lin2 where align f = lift2 \x y -> f (Both x y)
instance Align Lin1 where align f = lift2 \x y -> f (Both x y)
instance Align Afn3 where align f = lift2 \x y -> f (Both x y)
instance Align Afn2 where align f = lift2 \x y -> f (Both x y)
instance Align Afn1 where align f = lift2 \x y -> f (Both x y)
instance Align Bez3 where align f = lift2 \x y -> f (Both x y)
instance Align Bez2 where align f = lift2 \x y -> f (Both x y)
instance Align Bez1 where align f = lift2 \x y -> f (Both x y)
instance Align Poly4 where align f = lift2 \x y -> f (Both x y)
instance Align Poly3 where align f = lift2 \x y -> f (Both x y)
instance Align Poly2 where align f = lift2 \x y -> f (Both x y)
instance Align Poly1 where align f = lift2 \x y -> f (Both x y)
instance Align Poly0 where align f = lift2 \x y -> f (Both x y)
