module Math.Bezier where

import Math.Matrix
import Math.Poly
import Prelude

import Data.Distributive (class Distributive)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Number as Math
import Data.Tuple (Tuple(..))
import Safe.Coerce (coerce)

-- | Uses second and third derivatives to compute the signed curvature in 2D.
curvature2D :: V2 -> V2 -> Number
curvature2D dv ddv =
  det (mkLin2 (V2 dv ddv)) / Math.pow (norm2 dv) 1.5

-- | Polynomial numerator and denominator (pre-radical) for curvature
curvaturePND :: forall @s. Ring s => Bez3 (Vec2 s) -> Tuple (PolyS3 s) (PolyS4 s)
curvaturePND = bez2poly @Bez3 @Poly3 >>> \v ->
  let
    dv = deriv v
    V2 dx dy = components dv
    ddv = deriv dv
    V2 ddx ddy = components ddv
  in Tuple
    -- Cross product v' x v''
    (dx :*: ddy :-: dy :*: ddx)
    -- Norm squared ||v'||^2
    (dx :*: dx  :+: dy :*: dy)

curvatureAt :: B32 -> Number -> Number
curvatureAt = curvaturePND >>> \(Tuple numer denom) t ->
  evalPS numer t / Math.pow (evalPS denom t) 1.5

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

class Applicative b <= Bezier b where
  endpoints :: b ~> Bez1
  evalB :: forall @s @v. SModule s v => Ring s => b v -> s -> v
class (Applicative b, Applicative p) <= Bez2Poly b p | b -> p where
  bez2poly :: forall @v. Group v => b v -> p v

bbox1 :: forall f. Distributive f => Bez1 (f Number) -> f (Bounds Number)
bbox1 = getBounds

instance Bezier Bez1 where
  endpoints (B1 p0 p1) = B1 p0 p1
  evalB (B1 p0 p1) t = (one - t) .* p0 <> t .* p1
  -- matchSlope ref this
instance Bez2Poly Bez1 Poly1 where
  bez2poly (B1 p0 p1) = Poly1 p0 (p0 -<> p1)

instance Bezier Bez2 where
  endpoints (B2 p0 _ p2) = B1 p0 p2
  evalB (B2 p0 p1 p2) = evalB =<< evalB (B1 (B1 p0 p1) (B1 p1 p2))
instance Bez2Poly Bez2 Poly2 where
  bez2poly (B2 p0 p1 p2) = Poly2 p0
    (gmul 2 (p0 -<> p1))
    (p0 <>- p1 <> p1 -<> p2)

instance Bezier Bez3 where
  endpoints (B3 p0 _ _ p3) = B1 p0 p3
  evalB (B3 p0 p1 p2 p3) = evalB =<< evalB (B1 (B2 p0 p1 p2) (B2 p1 p2 p3))
instance Bez2Poly Bez3 Poly3 where
  bez2poly (B3 p0 p1 p2 p3) = Poly3
    p0
    (gmul 3 (p0 -<> p1))
    (gmul 3 (p0 <>- p1 <> p1 -<> p2))
    (p0 -<> gmul 3 p1 <> gmul 3 p2 -<> p3)

instance Bez2Poly Poly0 Poly0 where bez2poly = identity
instance Bez2Poly Poly1 Poly1 where bez2poly = identity
instance Bez2Poly Poly2 Poly2 where bez2poly = identity
instance Bez2Poly Poly3 Poly3 where bez2poly = identity
instance Bez2Poly Poly4 Poly4 where bez2poly = identity


minkowskiImplicit :: B32 -> B32 -> Poly2 (Poly2 V1)
minkowskiImplicit curveP curveQ =
  let
    V2 dpx dpy = components $ deriv $ bez2poly curveP
    V2 dqx dqy = components $ deriv $ bez2poly curveQ
    -- multivariate in p then q
    mvar l r = l <#> unwrap >>> (_ .* r)
  in mvar dpx dqy <>- mvar dpy dqx

-- solve2Norm :: PolyS2 Number -> Number
-- solve2Norm = solve2 >>> case _ of
--   Solve21 

-- solveDeriv :: PolyS2 Number -> PolyS1 Number -> Number
-- solveDeriv poly deriv = 

-- minkowskiDerivAt :: B32 -> B32 -> Number -> Tuple (PolyS2 Number) (PolyS1 Number) -- Number
-- minkowskiDerivAt curveP curveQ =
--   let
--     imp = minkowskiImplicit curveP curveQ
--     dimp = deriv <$> imp
--   in \p -> let
--     q_p = evalP imp p
--     dq_p = evalP dimp p
--   in solveDeriv q_p dq_p

-- minkowskiCurvature :: B32 -> B32 -> Number -> Maybe Number -> Number
-- minkowskiCurvature curveP curveQ =
--   let
--     dP = deriv $ bez2poly curveP
--     dQ = deriv $ bez2poly curveQ
--     ddP = deriv dP
--     ddQ = deriv dQ
--     dq_ = const (const 0.0) $ minkowskiDerivAt curveP curveQ
--   in \p mq -> let
--     dq = dq_ p
--     dq2 = dq * dq
--     q = fromMaybe 0.0 mq

--     dXY = dP@@p <> dq .* dQ@@q
--     ddXY = ddP@@p <> dq2 .* ddQ@@q
--   in curvature2D dXY ddXY
