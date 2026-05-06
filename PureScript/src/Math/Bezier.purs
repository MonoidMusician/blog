module Math.Bezier where

import Math.Matrix
import Math.Poly
import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Apply (lift2)
import Data.Align (class Align)
import Data.Array as Array
import Data.Distributive (class Distributive, collect, distribute)
import Data.Filterable (filter)
import Data.Foldable (all, any, foldMap)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap)
import Data.Number as Math
import Data.Number as Number
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..))
import Data.Semigroup.First (First(..))
import Data.Semigroup.Foldable (class Foldable1, fold1, foldMap1)
import Data.Semigroup.Traversable (class Traversable1)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Debug (spy)
import Idiolect (incorporate, only, unsafeFromMaybe, (/|\), (<#?), (<#?>), (>==))
import Monoids.BoundsWith (BoundsWith(..), MaxWith(..), MinWith(..), BoundsWithNES)
import Monoids.BoundsWith as Bounds
import Safe.Coerce (coerce)

type BoundsAtTimes ord = BoundsWith ord (NonEmptySet Time)

-- | Uses second and third derivatives to compute the signed curvature in 2D.
curvature2D :: V2 -> V2 -> Number
curvature2D dv ddv =
  crossS dv ddv / Math.pow (norm2 dv) 1.5

-- | Polynomial numerator and denominator (pre-radical) for curvature
curvaturePND :: forall @s. Ring s => Bez3 (Vec2 s) -> Tuple (PolyS3 s) (PolyS4 s)
curvaturePND = bez2poly >>> \v ->
  let
    dv = components $ deriv v
    ddv = deriv <$> dv
  in Tuple
    -- Cross product v' x v'' as a polynomial (degree 2 + 1)
    (crossPS dv ddv)
    -- Norm squared ||v'||^2 as a polynomial (degree 2 + 2)
    (norm2P dv)

curvatureAt :: B32 -> Number -> Number
curvatureAt = curvaturePND >>> \(Tuple numer denom) ->
  \t -> evalPS numer t / Math.pow (evalPS denom t) 1.5

-- | Use a pre-computed Bézier derivative to match the slope given by the second argument.
solveTangentD ::
  forall bez poly sol.
    Bez2Poly bez poly =>
    Solve poly sol =>
    SModule Number (poly V1) =>
  bez V2 -> V2 -> SolveN Number
solveTangentD curveDeriv (V2 dx dy) =
  let V2 px py = components $ bez2poly curveDeriv in
  solveN $ map unwrap $ dy .* px :-: dx .* py

-- | Find points on the curve tangent to the specified slope.
solveTangent ::
  forall curve bez poly sol.
    Deriv (curve V2) (bez V2) =>
    Bez2Poly bez poly =>
    Solve poly sol =>
    SModule Number (poly V1) =>
  curve V2 -> V2 -> SolveN Number
solveTangent = deriv @(curve _) @(bez _) >>> solveTangentD

-- | Convert a line into a dx,dy vector to represent a slope (think: projective plane).
pslope :: forall @s. Ring s => Bez1 (Vec2 s) -> Vec2 s
pslope (B1 p0 p1) = p0 -<> p1

radians2slope :: Radians -> Vec2 Number
radians2slope angle = V2 (Math.cos angle) (Math.sin angle)

degrees2slope :: Degrees -> Vec2 Number
degrees2slope = (_ * (Math.pi / 180.0)) >>> radians2slope

class (Align b, Traversable1 b) <= Bezier b where
  endpoints :: b ~> Bez1
  evalB :: forall @s @v. SModule s v => Ring s => b v -> s -> v
  bboxB' :: b V2 -> Vec2 (BoundsAtTimes Number)
  splitB :: forall @s @v. SModule s v => Ring s => b v -> s -> Pair (b v)
  -- https://cs.nyu.edu/~exact/doc/subdiv1.pdf
class DeCasteljau b o | b -> o where
  deCasteljau :: forall @s @v. SModule s v => Ring s => b v -> s ->
    { point :: v, left :: b v, right :: b v, inner :: o v }
  castUp :: forall @s @v. SModule s v => Field s => o v -> b v

_splitB :: forall @b o @s @v. DeCasteljau b o => SModule s v => Ring s => b v -> s -> Pair (b v)
_splitB b t = Pair <$> _.left <*> _.right $ deCasteljau b t

bisect :: forall @b @s @v. Bezier b => SModule s v => Field s => b v -> Pair (b v)
bisect = splitB <@> (one / (one + one))

cut :: forall @b @s @v. Bezier b => SModule s v => Field s => b v -> Bez1 s -> b v
cut b (B1 t0 t1) = from_t0_to_t1 where
  Pair to_t1 _ = splitB b t1 -- 0 <= t1 <= 1
  Pair _ from_t0_to_t1 = splitB to_t1 (t0 / t1) -- 0 <= t0 <= t1

evalBSS :: forall @b @s. Bezier b => Ring s => b s -> s -> s
evalBSS curve t = unwrap $ evalB (map V1 curve :: b (Vec1 s)) t

bboxB's :: forall @b @f. Bezier b => Foldable1 f => Functor f => f (b V2) -> Vec2 (BoundsAtTimes Number)
bboxB's = collect bboxB' >>> map fold1

bboxBp :: forall @b. Bezier b => b V2 -> Vec2 (BoundsWithNES Number V2)
bboxBp curve = bboxB' curve <#> map (NES.map (evalB curve))

bboxBps :: forall @b @f. Bezier b => Foldable1 f => Functor f => f (b V2) -> Vec2 (BoundsWithNES Number V2)
bboxBps = collect bboxBp >>> map fold1

bboxB :: forall @b. Bezier b => b V2 -> Vec2 (Bounds Number)
bboxB = bboxB' >== boundsWithout

bboxBs :: forall @b @f. Bezier b => Foldable1 f => Functor f => f (b V2) -> Vec2 (Bounds Number)
bboxBs = collect bboxB >>> map fold1

atX :: forall @b sol. Bezier b => Solve b sol => b V2 -> Number -> Maybe (Bounds Number)
atX curve x = atX' curve x <#> boundsWithout

atX' :: forall @b sol. Bezier b => Solve b sol => b V2 -> Number -> Maybe (BoundsAtTimes Number)
atX' curve x =
  let V2 curveX curveY = distribute curve in
  case solveN (curveX <#> (_ - x)) of
    EverywhereN -> Just let V2 _ bboxY = bboxB' curve in bboxY
    SolveN ts -> ts
      # filter (between 0.0 1.0)
      # foldMap \t ->
        Just $ Bounds.mkBoundNES t $ evalBSS curveY t

atY' :: forall @b sol. Bezier b => Solve b sol => b V2 -> Number -> Maybe (BoundsAtTimes Number)
atY' curve y =
  let V2 curveX curveY = distribute curve in
  case solveN (curveY <#> (_ - y)) of
    EverywhereN -> Just let V2 bboxX _ = bboxB' curve in bboxX
    SolveN ts -> ts
      # filter (between 0.0 1.0)
      # foldMap \t ->
        Just $ Bounds.mkBoundNES t $ evalBSS curveX t

extremaX :: forall @b @f. Foldable1 f => Bezier b => f (b V2) -> B12
extremaX curves = B1 p0 p1
  where
  getX (V2 x _) = x
  BoundsWith (MinWith _ (First p0)) (MaxWith _ (First p1)) =
    curves # foldMap1 \curve ->
      getX (bboxB' curve) <#> (NES.min >>> evalB curve >>> First)

extremaY :: forall @b @f. Foldable1 f => Bezier b => f (b V2) -> B12
extremaY curves = B1 p0 p1
  where
  getY (V2 _ y) = y
  BoundsWith (MinWith _ (First p0)) (MaxWith _ (First p1)) =
    curves # foldMap1 \curve ->
      getY (bboxB' curve) <#> (NES.min >>> evalB curve >>> First)

instance DeCasteljau Bez0 Bez0 where
  deCasteljau (Poly0 pt) _ = { point: pt, left: Poly0 pt, right: Poly0 pt, inner: Poly0 pt }
  castUp (Poly0 pt) = Poly0 pt
instance DeCasteljau Bez1 Bez0 where
  deCasteljau (B1 p0 p1) t =
    let pt = p0 +<t>+ p1 in
    { point: pt, left: B1 p0 pt, right: B1 pt p1, inner: Poly0 pt }
  castUp (Poly0 pt) = B1 pt pt
instance DeCasteljau Bez2 Bez1 where
  deCasteljau (B2 p0 p1 p2) t =
    let { point: pt, left: B1 p01 _, right: B1 _ p12 } = deCasteljau (B1 (p0 +<t>+ p1) (p1 +<t>+ p2)) t in
    { point: pt, left: B2 p0 p01 pt, right: B2 pt p12 p2, inner: B1 p01 p12 }
  castUp (B1 p0 p1) = B2 p0 (p0 +<(one / (one + one))>+ p1) p1
instance DeCasteljau Bez3 Bez2 where
  deCasteljau (B3 p0 p1 p2 p3) t =
    let inner = B2 (p0 +<t>+ p1) (p1 +<t>+ p2) (p2 +<t>+ p3)
        { point: pt, left: B2    p01 p012 _ , right: B2 _  p123 p23 } = deCasteljau inner t
    in  { point: pt, left: B3 p0 p01 p012 pt, right: B3 pt p123 p23 p3, inner }
  castUp (B2 p0 p1 p2) = B3 p0 (p0 +<third>+ p1) (p2 +<third>+ p1) p2
    where third = one / (one + one + one)

_deCasteljau ::
  forall @b b' o' @s @v.
    Bezier b =>
    Pairs b b' =>
    DeCasteljau b' o' =>
    SModule s v => Ring s =>
  b v -> s ->
  { point :: v, left :: b v, right :: b v, inner :: b' v }
_deCasteljau curve t =
  let B1 p0 p1 = endpoints curve
      inner = pairsWith (\p0 p1 -> p0 +<t>+ p1) curve
      { point, left, right } = deCasteljau inner t
  in  { point, left: p0 <: left, right: right :> p1, inner }


bbox1' :: forall f. Distributive f => Bez1 (f Number) -> f (BoundsAtTimes Number)
bbox1' = distribute >== \(B1 p0 p1) -> Bounds.mkBoundNES 0.0 p0 <> Bounds.mkBoundNES 1.0 p1

bboxN' ::
  forall curve bez poly sol.
    Bezier curve =>
    Deriv (curve V2) (bez V2) =>
    Bez2Poly bez poly =>
    Solve poly sol =>
    SModule Number (poly V1) =>
    Foldable1 curve =>
  curve V2 -> Vec2 (BoundsAtTimes Number)
bboxN' bez = ado
  base <- bbox1' $ endpoints bez
  xs <- bbFromTangent (V2 1.0 0.0)
  ys <- bbFromTangent (V2 0.0 1.0)
  in incorporate base (xs <> ys) where
  tangentsAt :: V2 -> Array (Vec2 { t :: Time, p :: Number })
  tangentsAt angle =
    Array.fromFoldable (solveTangent bez angle)
      <#? between 0.0 1.0
      <#> \t -> { t, p: _ } <$> evalB bez t
  bbFromTangent :: V2 -> Vec2 (Maybe (BoundsAtTimes Number))
  bbFromTangent = tangentsAt
    >>> Bounds.getBounds_ (NES.singleton <<< _.t) _.p

instance Bezier Bez1 where
  endpoints (B1 p0 p1) = B1 p0 p1
  evalB (B1 p0 p1) t = p0 +<t>+ p1
  bboxB' = bbox1'
  splitB (B1 p0 p1) t =
    let pt = p0 +<t>+ p1 in
    Pair (B1 p0 pt) (B1 pt p1)
  -- matchSlope ref this

instance Bezier Bez2 where
  endpoints (B2 p0 _ p2) = B1 p0 p2
  evalB (B2 p0 p1 p2) = evalB =<< evalB (B1 (B1 p0 p1) (B1 p1 p2))
  bboxB' b = bboxN' b
  splitB = _splitB

instance Bezier Bez3 where
  endpoints (B3 p0 _ _ p3) = B1 p0 p3
  evalB (B3 p0 p1 p2 p3) = evalB =<< evalB (B1 (B2 p0 p1 p2) (B2 p1 p2 p3))
  bboxB' b = bboxN' b
  splitB = _splitB


-- TODO: reduce duplicate points
intersect :: B32 -> B32 -> Array (Pair { t :: Number, p :: V2 })
intersect _p _q =
  go
    -- Track the time intervals along with the curve at those spots
    (Tuple (B1 (V1 0.0) (V1 1.0)) _p)
    (Tuple (B1 (V1 0.0) (V1 1.0)) _q)
  where
  -- Largest dimension of box
  mag (V2 bx by) = max (unwrap bx.max - unwrap bx.min) (unwrap by.max - unwrap by.min)
  -- Return middle t
  sample (B1 t0 t1) curve =
    let  t = (unwrap t0 + unwrap t1)/2.0
    in { t, p: evalB curve t }

  go :: Tuple B11 B32 -> Tuple B11 B32 -> Array (Pair { t :: Number, p :: V2 })
  go (Tuple pt p) (Tuple qt q) =
    let
      -- Use approximate bounding box
      bbp = getBounds p
      bbq = getBounds q
    in if disjointBounds bbp bbq then [] else
    if max (mag bbp) (mag bbq) < epsilon / 4.0
    -- Return middle time, sampling the original curve there
    then [Pair (sample pt _p) (sample qt _q)]
    else do
      -- Bisect the time intervals and the curves themselves
      p' <- bisect pt /|\ bisect p # \(Pair p0 p1) -> [p0, p1]
      q' <- bisect qt /|\ bisect q # \(Pair q0 q1) -> [q0, q1]
      go p' q'

doesIntersect :: B32 -> B32 -> Boolean
doesIntersect _p _q =
  go
    -- Track the time intervals along with the curve at those spots
    (Tuple (B1 (V1 0.0) (V1 1.0)) _p)
    (Tuple (B1 (V1 0.0) (V1 1.0)) _q)
  where
  -- Largest dimension of box
  mag (V2 bx by) = max (unwrap bx.max - unwrap bx.min) (unwrap by.max - unwrap by.min)

  go :: Tuple B11 B32 -> Tuple B11 B32 -> Boolean
  go (Tuple pt p) (Tuple qt q) =
    let
      -- Use approximate bounding box
      bbp = getBounds p
      bbq = getBounds q
    in if disjointBounds bbp bbq then false else
    if max (mag bbp) (mag bbq) < epsilon
    then true
    else do
      -- Bisect the time intervals and the curves themselves
      let
        Pair p0 p1 = bisect pt /|\ bisect p
        Pair q0 q1 = bisect qt /|\ bisect q
      go p0 q0 || go p0 q1 || go p1 q0 || go p1 q1

sameCurve :: B32 -> B32 -> Maybe (Afn1 V2)
sameCurve p q =
  let
    bbp = bboxB p
    bbq = bboxB q
  in if disjointBounds bbp bbq then Nothing else let
    bb :: Vec2 (Bounds Number)
    bb = lift2 (coerce (append @{ min :: Max Number, max :: Min Number })) bbp bbq

  in Nothing


-- The implicit equation defining the Minkowski sum of two Béziers
minkowskiImplicit :: B32 -> B32 -> Poly2 (Poly2 V1)
minkowskiImplicit curveP curveQ =
  let
    V2 dpx dpy = components $ deriv $ bez2poly curveP
    V2 dqx dqy = components $ deriv $ bez2poly curveQ
    -- multivariate in p then q
    mvar l r = l <#> unwrap >>> (_ .* r)
  in mvar dpx dqy <>- mvar dpy dqx

solve2Norm :: PolyS2 Number -> Number
solve2Norm = map unwrap >>> solve2 >>> Array.fromFoldable >>> \solutions ->
  unsafeFromMaybe "No solution between 0.0 and 1.0" $
    only (solutions <#? between 0.0 1.0) <|>
    only (solutions <#? between (0.0 - epsilon) (1.0 + epsilon))

solveDeriv :: PolyS2 Number -> PolyS2 Number -> Number
solveDeriv poly deriv =
  let
    q = solve2Norm poly
    Poly2 _   eq1 eq2 = coerce poly
    Poly2 dq0 dq1 dq2 = coerce deriv
  in - (dq2*q*q + dq1*q + dq0) / (2.0*eq2*q + eq1)

minkowskiDerivAt :: B32 -> B32 -> Number -> Number
minkowskiDerivAt curveP curveQ =
  let
    -- a(p)q^2 + b(p)q + c(p)
    imp = minkowskiImplicit curveP curveQ
    -- a'(p)q^2 + b'(p)q + c'(p)
    dimp = deriv <$> imp
  in \p -> let
    -- aq^2 + bq + c
    q_p = evalP imp p
    -- a'q^2 + b'q + c
    dq_p = evalP <$> dimp <@> p
    -- dq/dp
  in solveDeriv q_p dq_p

minkowskiAt :: B32 -> B32 -> Number -> Number
minkowskiAt curveP curveQ p = solve2Norm $
  evalP <$> minkowskiImplicit curveP curveQ <@> p

minkowskiCurvature :: B32 -> B32 -> Number -> Maybe Number -> Number
minkowskiCurvature curveP curveQ =
  let
    dP = deriv $ bez2poly curveP
    dQ = deriv $ bez2poly curveQ
    ddP = deriv dP
    ddQ = deriv dQ
    -- How much is time changing along q relative to time along p,
    -- to keep their tangents
    dq_p = minkowskiDerivAt curveP curveQ
  in \p mq -> let
    dq = dq_p p
    q = fromMaybe' (\_ -> minkowskiAt curveP curveQ p) mq

    -- We know the derivative of the composite curve
    dPQ = dP@@p <> dq .* dQ@@q
    -- And the relevant components of the second derivative
    -- (that are not parallel to the first derivative)
    ddPQ = ddP@@p <> (dq*dq) .* ddQ@@q
  in curvature2D dPQ ddPQ

fit ::
  { p0 :: V2
  , p1 :: V2
  , d0 :: V2
  , d1 :: V2
  , k0 :: Number
  , k1 :: Number
  } -> Array (Bez3 V2)
fit { p0, p1, d0: d0', d1: d1', k0, k1 } = do
  let
    -- These are the chosen tangents
    d0 = normalize d0'
    d1 = normalize d1'
    -- The line segment from the start point to end point
    δp = p0 -<> p1
    -- Cross products
    d0xd1 = d0 × d1 # unwrap
    d0xδp = d0 × δp # unwrap
    δpxd1 = δp × d1 # unwrap
  Pair δ0 δ1 <- if d0xd1 == 0.0 then do
    pure $ Pair (d0xδp/k0) (δpxd1/k1) <#>
      \δ -> Math.sqrt ((2.0/3.0) * δ)
  else do
    let
      -- Optimized by https://herbie.uwplse.org/
      _  = 1.5 * (k0 * δpxd1 * δpxd1) / (d0xδp * d0xd1 * d0xd1)
      r0 = 1.5 * (((δpxd1 / d0xδp) / (d0xd1 / k0)) / (d0xd1 / δpxd1))
      _  = 1.5 * (k1 * d0xδp * d0xδp) / (δpxd1 * d0xd1 * d0xd1)
      r1 = 1.5 * (((d0xδp / δpxd1) / (d0xd1 / k1)) / (d0xd1 / d0xδp))
      c0 = Poly4 (r1 - 1.0) 1.0 (-2.0*r1*r0) 0.0 (r1*(r0*r0))
      c1 = Poly4 (r0 - 1.0) 1.0 (-2.0*r0*r1) 0.0 (r0*(r1*r1))
      ρ0s = Array.nub $ Array.filter (Number.isFinite) $ Array.fromFoldable $ solve4 c0
      ρ1s = Array.nub $ Array.filter (Number.isFinite) $ Array.fromFoldable $ solve4 c1

      ρρs0 = ρ0s <#> \ρ0 -> Pair ρ0 (1.0 - r0*(ρ0*ρ0))
      ρρs1 = ρ1s <#> \ρ1 -> Pair (1.0 - r1*(ρ1*ρ1)) ρ1
      ρρs2 = Pair <$> ρ0s <*> ρ1s

      ρs = ρρs0
      δs = ρs <#> \(Pair ρ0 ρ1) -> Pair (ρ0 * δpxd1 / d0xd1) (ρ1 * d0xδp / d0xd1)
      nonneg = (_ >= 0.0)
      near = (_ >= -epsilon)
      try xs ys = if Array.null xs then ys else xs
      select f = try (Array.filter f δs)
      priorities = Array.foldr select δs
      -- _ = spy "fit" { p0, p1, d0, d1, k0, k1, r0, r1, c0, c1, ρ0s, ρ1s, ρρs0, ρρs1, ρρs2, ρs, δs }
    priorities [ all nonneg, all near, any nonneg, any near ]
  pure $ B3 p0 (p0 <>+ δ0 .* d0) (δ1 .* d1 -<> p1) p1

bezierCircle :: Array B32
bezierCircle =
  [
  --   B3 (V2 1.0 0.0) (V2 1.0 0.265216) (V2 0.894643 0.51957) (V2 Math.sqrt2 Math.sqrt2)
  -- , B3 (V2 Math.sqrt2 Math.sqrt2) (V2 0.51957 0.894643) (V2 0.265216 1.0) (V2 0.0 1.0)
  -- , B3 (V2 0.0 1.0) (V2 (-0.265216) 1.0) (V2 (-0.51957) 0.894643) (V2 (-Math.sqrt2) Math.sqrt2)
  -- , B3 (V2 (-Math.sqrt2) Math.sqrt2) (V2 (-0.894643) 0.51957) (V2 (-1.0) 0.265216) (V2 (-1.0) 0.0)
  -- , B3 (V2 (-1.0) 0.0) (V2 (-1.0) (-0.265216)) (V2 (-0.894643) (-0.51957)) (V2 (-Math.sqrt2) (-Math.sqrt2))
  -- , B3 (V2 (-Math.sqrt2) (-Math.sqrt2)) (V2 (-0.51957) (-0.894643)) (V2 (-0.265216) (-1.0)) (V2 0.0 (-1.0))
  -- , B3 (V2 0.0 (-1.0)) (V2 0.265216 (-1.0)) (V2 0.51957 (-0.894643)) (V2 Math.sqrt2 (-Math.sqrt2))
  -- , B3 (V2 Math.sqrt2 (-Math.sqrt2)) (V2 0.894643 (-0.51957)) (V2 1.0 (-0.265216)) (V2 1.0 0.0)
  seg 0.0 (-1.0) (-0.404462) (-1.0) (-0.769099) (-0.756358) (-0.92388) (-0.382683)
  , seg (-0.92388) (-0.382683) (-1.07866) (-0.00900904) (-0.993105) 0.421109 (-0.707107) 0.707107
  , seg (-0.707107) 0.707107 (-0.421109) 0.993105 0.00900904 1.07866 0.382683 0.92388
  , seg 0.382683 0.92388 0.756358 0.769099 1.0 0.404462 1.0 0.0
  , seg 1.0 0.0 1.0 (-0.265216) 0.894643 (-0.51957) 0.707107 (-0.707107)
  , seg 0.707107 (-0.707107) 0.51957 (-0.894643) 0.265216 (-1.0) 0.0 (-1.0)
  ]
  where
  seg x0 y0 x1 y1 x2 y2 x3 y3 = B3 (V2 x0 y0) (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)

intersectCircle :: { p :: V2, r :: Number } -> B32 -> Array
  { t :: Number, p :: V2, radians :: Radians, degrees :: Degrees }
intersectCircle { p: center, r } curve =
  let
    mkCircle = translate2 center <. scale r
    circle = LTF (LTF mkCircle) $* bezierCircle
  in circle >>= \arc -> do
    Pair _ { p, t } <- intersect arc curve
    let
      V2 dx dy = center -<> p
      radians = Math.atan2 dy dx
    pure { p, t, radians, degrees: radians * r2d }
