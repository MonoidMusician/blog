module Airplane.Math where

import Math.Matrix
import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (fold, foldMap, maximumBy, minimum, minimumBy)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (on)
import Data.HeytingAlgebra (tt)
import Data.Int as Int
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Profunctor (dimap)
import Data.Semigroup.Foldable (foldMap1)
import Data.Set.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Debug (spy, spyWith)
import Idiolect (neighbors, only, (#..), (<#>:), (<#?>), (>==))
import Math.Bezier (atX, atX', bboxB's, bboxBps, bboxBs, degrees2slope, endpoints, evalB, solveTangent, solveTangentD)
import Math.Poly (deriv, epsilon)
import Monoids.BoundsWith (BoundsWith(..), MaxWith(..), MinWith(..))
import Safe.Coerce (coerce)

-- airfoil

stitch ::
  Int {- number of segments -} ->
  Int {- index of segment -} ->
  Time {- time within segment -} ->
  Time {- time within spline -}
stitch n i t = (t + Int.toNumber i) / Int.toNumber n

type Sample p = { t :: Time, p :: p }
type SampleR p r = { t :: Time, p :: p | r }

preferLSamplesBy :: forall p r o. Ord o => (SampleR p r -> o) -> Array (SampleR p r) -> Array (SampleR p r) -> Array (SampleR p r)
preferLSamplesBy f ls rs = ls <> map notWithinLs rs
  where
  minL = apply Tuple f <$> minimumBy (compare `on` f) ls
  maxL = apply Tuple f <$> maximumBy (compare `on` f) ls
  notWithinLs = case minL, maxL, _ of
    Just (Tuple minP minO), Just (Tuple maxP maxO), p | o <- f p ->
      if o >= minO && o <= maxO then maxP else p
    _, _, p -> p

_sampleRadially :: Array Degrees -> (Sample V2 -> Boolean) -> B32 -> Array (Array (Sample V2))
_sampleRadially angles predicate curve
  | curveAt <- evalB curve
  , curveD <- deriv curve =
  angles <#> \angle ->
    solveTangentD curveD (degrees2slope angle)
      # Array.fromFoldable
      # map (\t -> { t, p: curveAt t })
      # Array.filter predicate

sampleRadially :: Array Degrees -> (Sample V2 -> Boolean) -> Array B32 -> Array
  (SampleR V2 ( angle :: Degrees, curve :: Int, ncurves :: Int, tCurve :: Time ))
sampleRadially angles predicate curves =
  let
    predicate' = predicate && \{ t } -> -epsilon <= t && t <= 1.0+epsilon
    byCurveAngle = curves <#>: \i curve -> { i, samplesByAngle: _ } $
      _sampleRadially angles predicate' curve
  in Array.catMaybes $ angles <#>: \angleIndex angle ->
    byCurveAngle # Array.findMap \{ i, samplesByAngle } ->
      (samplesByAngle !! angleIndex) >>= only >== \{ p, t: tCurve } ->
        { p, t: stitch (Array.length curves) i tCurve, curve: i, ncurves: Array.length curves, tCurve: tCurve, angle }

_sampleLinearlyX :: Array Fraction -> NonEmptyArray B32 -> Array
  (BoundsWith Number { x :: Number, frac :: Fraction, curve :: Int, ncurves :: Int, tCurve :: Time, t :: Time })
_sampleLinearlyX fracs curves =
  let
    V2 { min: Min minx, max: Max maxx } _ = bboxBs curves
    xs = fracs <#> \frac -> { frac, x: V1 minx +<frac>+ V1 maxx # unwrap }
    visitCurveAt x i curve = atX' curve x <#> map (NES.map (Tuple i))
  in xs <#?> \{ frac, x } ->
    foldMapWithIndex (visitCurveAt x) curves <#> map do
      NES.min >>> \(Tuple i tCurve) ->
        { x, frac, curve: i, ncurves: NEA.length curves
        , tCurve, t: stitch (NEA.length curves) i tCurve
        }

sampleAirfoil ::
  { radius :: Array Degrees, chord :: Array Fraction } ->
  NonEmptyArray B32 ->
  Array (SampleR V2 ( curve :: Int, ncurves :: Int, tCurve :: Time, pos :: Either Degrees Fraction ))
sampleAirfoil samples curves =
  let
    V2 { min: Min minx, max: Max maxx } _ = bboxBs curves
    radially = sampleRadially samples.radius predicate (NEA.toArray curves)
      <#> \{ t, p, angle, curve, ncurves, tCurve } -> { t, p, pos: Left angle, curve, ncurves, tCurve }
    -- Less than 10% chord
    predicate = \{ p: V2 x _ } -> x < (V1 minx +<0.10>+ V1 maxx # unwrap)
    -- Split at minimum x
    minxR = minimum $ radially <#> \{ p: V2 x _ } -> x
    { init: radialUp, rest: radialDn } = radially # Array.span
      \{ p: V2 x _ } -> Just x > minxR

    linearly = _sampleLinearlyX samples.chord curves
    -- Split into lower and upper foils
    linearDn = linearly <#> \(BoundsWith (MinWith y { x, frac, curve, ncurves, tCurve, t }) _) ->
      { t, p: V2 x y, pos: Right frac, curve, ncurves, tCurve }
    linearUp = linearly <#> \(BoundsWith _ (MaxWith y { x, frac, curve, ncurves, tCurve, t })) ->
      { t, p: V2 x y, pos: Right frac, curve, ncurves, tCurve }
    lower = preferLSamplesBy (\{ p: V2 x _ } -> x) radialDn linearDn
    upper = preferLSamplesBy (\{ p: V2 x _ } -> x) (Array.reverse radialUp) linearUp
    around = Array.reverse lower <> upper
  in around


sliceShapeX ::
  { angles :: Array Degrees, samples :: Array Number, spacing :: Number } ->
  Array B32 ->
  SemigroupMap Number (Bounds Number)
sliceShapeX { angles, samples: initialSlices, spacing } curves =
  -- The final result includes all of the prelim samples and the gap fillers
  resample $ Array.nub $ Array.sort $ join $
    pairsWith fillGaps $ trimTriples prelimSamples
  where
  bbMatches :: SemigroupMap Number (Bounds Number)
  bbMatches = case bboxBps <$> NEA.fromArray curves of
    Just (V2 (BoundsWith (MinWith xmin minps) (MaxWith xmax maxps)) _) ->
      SemigroupMap (Map.singleton xmin $ foldMap1 (mkBound <<< v2Y) minps) <>
      SemigroupMap (Map.singleton xmax $ foldMap1 (mkBound <<< v2Y) maxps)
    Nothing -> mempty
  -- Find all X-coords where angles match in any curve
  angleMatches :: SemigroupMap Number (Bounds Number)
  angleMatches = SemigroupMap $ Map.fromFoldableWith (<>) $ join ado
    angle <- angles
    curve <- curves
    in map ((\(V2 x y) -> Tuple x (mkBound y)) <<< evalB curve) $
      filter (between (0.0-epsilon) (1.0+epsilon)) $
      Array.fromFoldable $ solveTangent curve $ degrees2slope angle
  -- All curve endpoints (TODO: filter to tangent discontinuities or path endpoints)
  curveEndpoints = curves #.. endpoints >>> foldMap \(V2 x y) ->
    SemigroupMap $ Map.singleton x $ mkBound y
  alreadySampled = bbMatches <> angleMatches <> curveEndpoints
  initialSamples = initialSlices
  -- All of the special samples we need to include
  prelimSamples = initialSamples <> Array.fromFoldable (Map.keys (unwrap alreadySampled))
  trimTriples = neighbors >>> Array.mapMaybe case _ of
    { prev: Just lo, here, next: Just hi }
      | hi - lo < spacing / 20.0
      , here `not Map.member` unwrap angleMatches -> Nothing
    { here } -> Just here
  -- Fill in the gaps so there are no gaps larger than `spacing`
  fillGaps lo hi = [lo] <> missing <> [hi]
    where
    required = Int.ceil $ (hi - lo) / spacing
    missing = Array.range 1 (required - 1) <#> \i ->
      lo + (hi - lo) * Int.toNumber i / Int.toNumber required
  resample :: Array Number -> SemigroupMap Number (Bounds Number)
  resample = foldMap \x -> foldMap (SemigroupMap <<< Map.singleton x) $
    Map.lookup x (unwrap alreadySampled)
    <> foldMap atX curves x
