module Train.Geometry where

import Prelude

import Control.Monad.State (class MonadState, get)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Distributive (collect)
import Data.Foldable (all, fold, foldMap, intercalate, minimum, sum)
import Data.Functor.App (App(..))
import Data.Functor.Compose (Compose(..))
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (un, unwrap)
import Data.NonEmpty ((:|))
import Data.Number as Math
import Data.Optical (setProp)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..))
import Data.Semigroup.Foldable (fold1)
import Data.Traversable (mapAccumL)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Idiolect (intercalateMap, minimumWith, sgn, withIndices, (<#?>), (>==))
import Math.Bezier as Bezier
import Math.Matrix (B32, BBox2, Bez1(..), Bez3(..), LTF(..), Lin2(..), Vec2(..), bounds2bounds1, d2r, disjointBounds, dot, inv, norm2, normalize, pairs, rotl2, tf, tfI, unAfn2, ($*), ($.), (-<>), (.*), (.+<), (<.), (<>-), (<^), (>+.))
import Math.Poly (deriv)
import Safe.Coerce (coerce)
import Train.Types (Canonized, Direction(..), HitMap, PointOnRoute, Route(..), RoutedTrain(..), Standard(..), canonCurve, canonStrokeBox, routeEnd, routeStart)


-- | The four 90deg rotations in a circle.
rotations :: forall s. Ring s => Array (Lin2 s)
rotations = do
  let rot90 = Lin2 zero (negate one) one zero
  [ tfI
  , rot90
  , rot90 <. rot90
  , rot90 <. rot90 <. rot90
  ]


-- | Check if one path continues on from the other. (They are not considered intersecting.)
continues :: Canonized -> Canonized -> Maybe Boolean
continues (Pair p@{ canon: Standard { key: pK } } _) (Pair q@{ canon: Standard { key: qK } } _) =
  let
    Tuple pStart prot = unAfn2 p.transformI
    Tuple qStart qrot = unAfn2 q.transformI
    pEnd = p.transformI $* pK.delta
    qEnd = q.transformI $* qK.delta
    pFrom = prot $* pK.from
    pTo = prot $* pK.to
    qFrom = qrot $* qK.from
    qTo = qrot $* qK.to
  in Array.head do
    Tuple pAt pDir <- [ Tuple pStart pFrom, Tuple pEnd pTo ]
    Tuple qAt qDir <- [ Tuple qStart qFrom, Tuple qEnd qTo ]
    -- _ <- pure $ spy "continues" { pAt, pDir, qAt, qDir }
    if pAt /= qAt then [] else [(pDir == qDir) == ((pAt == pStart) /= (qAt == qStart))]


-- | Test if two segments intersect, cached in the hitmap.
intersects :: forall m r. MonadState { hitmap :: HitMap | r } m => Pair Canonized -> m Boolean
intersects (Pair p q) | disjointBounds (canonStrokeBox p) (canonStrokeBox q) = pure false
intersects (Pair p q) | Just result <- continues p q = pure $ not result
intersects (Pair (Pair p@{ canon: Standard pC } _) (Pair q@{ canon: Standard qC } _)) = do
  { hitmap } <- get
  -- One can be the origin, so the other is transformed by this
  let relate = p.transform <^ q.transform
  let ids = (Pair pC.id qC.id)
  -- It is keyed in the hitmap by the relation and the pair of IDs
  case Map.lookup relate hitmap >>= Map.lookup ids of
    Just result -> pure result
    Nothing -> do
      let
        -- Each segment consists of two strokes. There is no need to test
        -- interiors, for the grid-based simple curves we are considering.
        Pair p1 p2 = tf (LTF p.transform) <$> pC.strokes
        Pair q1 q2 = tf (LTF q.transform) <$> qC.strokes
        result =
          Bezier.doesIntersectPrec 0.1 p1 q1 ||
          Bezier.doesIntersectPrec 0.1 p1 q2 ||
          Bezier.doesIntersectPrec 0.1 p2 q1 ||
          Bezier.doesIntersectPrec 0.1 p2 q2
      -- Add it into the hitmap
      setProp @"hitmap" $ Map.alter (Just <<< Map.insert ids result <<< fromMaybe Map.empty) relate hitmap
      pure result


-- | Use a set of curves, a starting point, and an array of distances to walk
-- | the route to approximately place a set of train cars on it.
walkPaths ::
  Route -> PointOnRoute ->
  Array Number ->
  NonEmptyArray PointOnRoute
walkPaths (Route { segments, curves }) start distances =
  NEA.cons' start $ Array.catMaybes $ Array.scanl step (Just start) distances
  where
  step Nothing _ = Nothing
  step (Just whence) distance = closestPoint
    where
    -- Search nearby `whence.i`, positive and negative in the same stage
    searchOrder :: Array (Array Int)
    searchOrder = [[whence.i]] <> do
      (1 Array...(NEA.length curves - 1)) <#> \d ->
        [whence.i + d, whence.i - d]
    closestPoint :: Maybe PointOnRoute
    closestPoint = searchOrder # Array.findMap \is ->
      -- Find the closest point in time
      minimumWith (\r -> Math.abs (composite r - composite whence)) $
      -- On the nearest pair of segments
      is >>= \i -> case curves NEA.!! i, segments NEA.!! i of
        Just curve, Just seg -> do
          -- That intersects at the right distance
          { p, t } <- Bezier.intersectCirclePrec 0.2 { p: whence.at, r: distance } curve
          -- And is headed in the right direction
          if dot (whence.at -<> p) whence.to > 0.0 then do
            let
              { segment, pathlength: Pair start _ } = seg
              Pair { canon: Standard { samples } } _ = segment
              tangent = Bezier.evalB22 (deriv curve) t
              to = sgn (dot tangent whence.to) .* tangent

              -- Find the closest sample, and linearly interpolate pathlength within it
              closest = Int.floor $ t * 100.0
              within = t * 100.0 - Int.toNumber closest
              pathlength = start + case samples Array.!! closest of
                Just { pathlength: Pair x y } -> x .+<within>+. y
                Nothing -> 0.0
            pure { at: p, to, t, i, curve, segment, pathlength, curvature: Bezier.curvatureAt curve t }
          else []
        _, _ -> []
    -- Composite time across the whole set of curves
    composite { t, i } = t + Int.toNumber i

-- | Find the point at the length (from 0.0 to route.pathlength).
routeAtTime :: Route -> Number -> Maybe PointOnRoute
routeAtTime (Route { segments, pathlength }) alongRoute = do
  { i, pathlength: Pair segmentStart _, segment: segment@(Pair { canon } _) } <- NEA.toArray segments
    # Array.find \{ pathlength: Pair _ segmentEnd } -> segmentEnd >= alongRoute
  let leftover = alongRoute - segmentStart
  bookends <- (unwrap canon).samples # pairs
    -- Use addition instead of subtraction for reason of precision
    # Array.find \(Pair _ { pathlength: Pair _ endsUp }) -> segmentStart + endsUp >= alongRoute
  case bookends of
    Pair { t: t0, pathlength: Pair l0 _ } { t: t1, pathlength: Pair _ l1 } -> do
      let
        curve = canonCurve segment
        t = bounds2bounds1 (coerce { min: l0, max: l1 }) (coerce { min: t0, max: t1 }) $. leftover
        at = Bezier.evalB32 curve t
        to = normalize $ Bezier.evalB22 (deriv curve) t
        curvature = Bezier.curvatureAt curve t
      Just { at, to, curvature, curve, t, i, segment, pathlength }


dilatePath :: B32 -> Pair B32
dilatePath curve@(B3 p0 _ _ p3) | curve == Bezier.castUp (Bezier.castUp (B1 p0 p3)) =
  let
    delta = 16.0
    d = normalize $ p0 -<> p3
    ninety = rotl2 (90.0 * d2r)
    cross = delta .* (ninety $* d)
  in Pair (LTF cross $* curve) (LTF (inv cross) $* curve)
dilatePath curve =
  let
    delta = 16.0
    outer c@(B3 p0 p1 p2 p3) which =
      let
        d0 = normalize $ p0 -<> p1
        d1 = normalize $ p2 -<> p3
        ninety = rotl2 (which * 90.0 * d2r)
        q0 = p0 <> delta .* (ninety $* d0)
        q3 = p3 <> delta .* (ninety $* d1)
        k0 = Bezier.curvatureAt c 0.0
        k1 = Bezier.curvatureAt c 1.0
        results = Bezier.fit
          { p0: q0, p1: q3, d0, d1
          , k0: 1.0 / ((1.0 / k0) + which * sgn k0 * delta)
          , k1: 1.0 / ((1.0 / k1) + which * sgn k1 * delta)
          } # Array.filter (Compose >>> all Math.isFinite)
        expected t = Bezier.evalB32 c t <> delta .*
          (rotl2 (which * 90.0 * d2r) $* Bezier.evalB22 (deriv c) t)
        score c2 = sum $ ((_ / 12.0) <<< Int.toNumber <$> Array.range 0 12) <#>
          \t -> norm2 (expected t <>- Bezier.evalB32 c2 t)
        scored = (Tuple <*> score) <$> results
      in fromMaybe c $ fst <$> minimumWith snd scored
  in outer curve <$> Pair one (negate one)

-- | Split an array of segments into non-intersecting paths.
-- |
-- | Currently just takes running segments until they self-intersect. This
-- | behavior may change.
separateRoutes :: forall m r. MonadState { hitmap :: HitMap | r } m => Array Canonized -> m (Array (Array Canonized))
separateRoutes =
  let
    test running segment =
      map (un Conj) $ un App $ foldMap (\other -> App $ Conj <<< not <$> intersects (Pair other segment)) running
    addSegment (running :| rest) segment =
      test running segment <#> if _
        then segment : running :| rest
        else pure segment :| running : rest
  in Array.foldM addSegment (Nil :| Nil) >==
    (Array.fromFoldable >>> Array.reverse >== Array.fromFoldable >>> Array.reverse)

-- | Generate SVG paths from an array of segments. Returns the path string
-- | and its bounding box.
routesToPaths :: forall m r. MonadState { hitmap :: HitMap | r } m => Array Canonized -> m (Array { d :: String, bbox :: BBox2 Number })
routesToPaths originalSegments = do
  separated <- separateRoutes originalSegments
  pure $ separated <#?> NEA.fromArray <#> \cs ->
    { d: bezsToPath (canonCurve <$> cs)
    , bbox: fold1 <$> collect canonStrokeBox cs
    }

-- | Render a Bezier array as a path string.
bezsToPath :: NonEmptyArray B32 -> String
bezsToPath segments =
  let
    pt (V2 x y) = show x <> "," <> show y
    seg prev (B3 p0 p1 p2 p3) = { accum: p3, value: _ } $ fold
      [ if p0 /= prev then "M" <> pt p0 else ""
      , "C" <> intercalateMap " " pt [ p1, p2, p3 ]
      ]
  in intercalate " " $ _.value
    $ mapAccumL seg (NEA.head segments # \(B3 p0 _ _ _) -> p0 <>- V2 one one)
    $ segments

mkRoute :: forall m r. MonadState { hitmap :: HitMap | r } m => NonEmptyArray Canonized -> m Route
mkRoute original = do
  let
    mapper l0 (Tuple i segment@(Pair { canon: Standard canon } _)) =
      let l1 = l0 + canon.pathlength in
      { accum: l1, value: { segment, i, pathlength: Pair l0 l1 } }
    { accum: pathlength, value: segments } = mapAccumL mapper 0.0 $ withIndices original
  crossings <- Map.unions <$> forWithIndex segments \j q -> do
    Map.unions <$> forWithIndex (NEA.take (j-7) segments) \i p -> do
      intersects (Pair p.segment q.segment) <#> case _ of
        false -> Map.empty
        true -> Map.singleton (Pair i j) $
          case p.pathlength, q.pathlength of
            Pair _ x, Pair y _ -> { pathlength: y - x }
  let
    curves = canonCurve <$> original
    maxlength = fromMaybe pathlength $ minimum $ _.pathlength <$> crossings
    isLoop = case NEA.head original, NEA.last original of
      Pair { pos: Pair p _ } _, Pair { pos: Pair _ q } _ -> p == q
  pure $ Route
    { pathlength, segments, curves
    , crossings, maxlength
    , isLoop
    }




trainOnRoute :: Route -> Array Number -> RoutedTrain
trainOnRoute route@(Route { pathlength }) consist = RoutedTrain
  { route: route, consist
  , endpoints
  }
  where
  start = max
    do NEA.last $ map _.pathlength $ walkPaths route (routeStart Forward route) consist
    do NEA.last $ map _.pathlength $ walkPaths route (routeStart Forward route) (Array.reverse consist)
  end = pathlength - min
    do NEA.last $ map _.pathlength $ walkPaths route (routeEnd Backward route) consist
    do NEA.last $ map _.pathlength $ walkPaths route (routeEnd Backward route) (Array.reverse consist)
  endpoints = { start, end, buffer: max start end }

