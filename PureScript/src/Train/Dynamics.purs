module Train.Dynamics where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Number as Math
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..))
import Data.Semigroup.Foldable (foldl1)
import Data.Traversable (mapAccumR, maximum)
import Data.TraversableWithIndex (mapAccumLWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Idiolect (type (@::), (**))
import Math.Matrix (Bounds, extent, mkBounds, overBounds)
import Partial.Unsafe (unsafeCrashWith)

planAndSchedule ::
  { traction :: Traction
  , speeds :: Array SpeedPlan
  , extent :: Bounds Number
  } ->
  { plan :: SpeedPlan
  , schedule :: SpeedSchedule
  , extent :: Bounds Number
  , time :: Bounds Number
  , crossover :: Number
  , maxSpeed :: Number

  , animation :: "time" @:: Number -> "dist" @:: Number
  , status :: "time" @:: Number -> { dist :: Number, veloc :: Number, accel :: Number }
  , segment :: "dist" @:: Number -> { dist :: Bounds Number, dur :: Number, plan :: SpeedSegment }
  , byDist :: "dist" @:: Number -> { veloc :: Number, accel :: Number, time :: Number }
  }
planAndSchedule { traction, speeds, extent: domain@{ min: Min e0, max: Max e1 } } =
  { plan, schedule, crossover: crossover traction
  , animation, status, segment, byDist
  , extent: domain, maxSpeed, time: overBounds (_.time <<< byDist) domain
  }
  where
  plan = maybe (planLimit Math.infinity) (foldl1 (combinePlans traction)) $ NEA.fromArray speeds
  schedule@(SpeedSchedule _ _ _ scheduleByDist _) = schedulePlan traction plan
  status = scheduleAtTime schedule
  animation = _.dist <<< status
  segment = plannedSegment plan <#> \r ->
    { dist: r.dist, dur: segmentTime traction r.plan (extent r.dist), plan: r.plan }
  byDist = \dist ->
    let
      { veloc, accel } = planAtDist traction plan dist
      time = case Map.lookupLE dist scheduleByDist of
        Just { value: { dist: { min: Min d0 }, time: { min: Min t0 }, plan: seg } } ->
          t0 + segmentTime traction seg (dist - d0)
        Nothing -> unsafeCrashWith "byDist"
    in { veloc, time, accel }
  maxSpeed = fromMaybe zero $ maximum $ scheduleByDist
    <#> \seg@{ dist: { min: Min d0, max: Max d1 } } ->
      if e0 > d1 || e1 < d0 then zero else max
        (segmentAtDist traction seg (clamp e0 e1 $ d0) # _.veloc)
        (segmentAtDist traction seg (clamp e0 e1 $ d1) # _.veloc)



-- | A very basic characterization of the dynamics of a train:
-- |
-- | - At low speeds, acceleration is limited by the physical grip the wheels
-- |   can provide on the rails.
-- | - At higher speeds, acceleration is limited by the amount of power the
-- |   motors can provide, this acceleration is inversely proportional to
-- |   speed.
-- |
-- | It is assumed that braking power is equal to motive power, to keep things
-- | simple and symmetric.
-- |
-- | These dynamics are characterized independent of weight, since it cancels out.
type Traction =
  { wheels :: "accel" @:: Number -- m/s^2
  , motors :: "power" @:: Number -- erm, power divided by mass: m^2/s^3
  }

-- | The crossover point where physical wheel traction versus motor power output are more important.
crossover :: Traction -> "veloc" @:: Number
crossover { wheels, motors } = motors / wheels

-- | Get the maximum acceleration for the given speed.
maxAccelAtVeloc :: Traction -> "veloc" @:: Number -> "accel" @:: Number
maxAccelAtVeloc { wheels, motors } veloc = min wheels (motors / veloc)

-- | Get the maximum velocity at the distance, and the acceleration then and the time it takes to get there.
maxAtDistance :: Traction -> "veloc" @:: Number -> "dist" @:: Number -> { veloc :: Number, accel :: Number, time :: Number }
maxAtDistance traction veloc =
  let
    dvCrossover = max 0.0 $ crossover traction - veloc
    dtCrossover = dvCrossover / traction.wheels
    dxCrossover = (veloc + dvCrossover / 2.0) * dtCrossover
  in \dist -> if dist <= dxCrossover then let
    -- veloc * t + traction.wheels * t^2 / 2.0 - dx = 0.0
    t = (Math.sqrt(veloc*veloc + 2.0*traction.wheels*dist) - veloc) / traction.wheels
    dv = traction.wheels * t
    _dx = (veloc + traction.wheels * t / 2.0) * t
    v = veloc + dv
    a = maxAccelAtVeloc traction v

    -- _ = spy "maxAtDistance" { veloc, dist, dvc: dvCrossover, dtc: dtCrossover, dxc: dxCrossover, t, dv, _dx, v, a }

    -- _ = spy "maxCheck" { veloc, v, dist, t, check: toVelocity traction (Pair veloc v) }

  in { veloc: v, accel: a, time: t }
  else let
    -- Derivation:
    --   dv/dt = k/v
    --   vdv/k = dt
    --   v^2/2k = t + C
    --   C = v0^2/2k - t0
    --   v = sqrt(2k(t + C))
    --   x = (2k(t + C))^1.5 / 3k
    --   t = (1.5 sqrt(2k) x)^(2/3) / 2k - C

    v0 = veloc + dvCrossover -- starting velocity for high speed regime
    dx = dist - dxCrossover -- distance left
    k = traction.motors
    c = (v0*v0)/(2.0*k) -- hypothetical time to accelerate to v0 from t=0
    x0 = (2.0*k * c) ** 1.5 / (3.0 * k) -- hypothetical distance to accelerate to v0

    -- amount of time it gets to accelerate for (to reach dx)
    t = (-c + (Math.pow (3.0*k * (dx + x0)) (2.0/3.0)) / (2.0*k))
    -- velocity at that time
    v = Math.sqrt (2.0*k * (t + c))
    -- acceleration at that time
    a = maxAccelAtVeloc traction v

    -- _ = spy "maxAtDistance" { veloc, dist, dvc: dvCrossover, dtc: dtCrossover, dxc: dxCrossover, v0, dx, c, x0, t, v, a }

    -- _ = spy "maxCheck" { veloc, v, dist, t: dtCrossover + t, check: toVelocity traction (Pair veloc v) }
  in { veloc: v, accel: a, time: dtCrossover + t }

-- | Get the distance and time required to transition between velocities.
toVelocity :: Traction -> "veloc" @:: Pair Number -> { dist :: Number, time :: Number }
toVelocity traction (Pair v0 v1)
  | v0 == v1 = zero
  | v0 > v1 = toVelocity traction (Pair v1 v0)
  | v1 <= crossover traction =
    -- low-speed regime: constant acceleration
    let
      time = (v1 - v0) / traction.wheels
      dist = time * (v0 + v1) / 2.0
    in { time, dist }
  | crossover traction <= v0 =
    -- high-speed regime: constant power
    let
      -- From above:
      --   C = v0^2/2k - t0
      --   v = sqrt(2k(t + C))
      --   x = (2k(t + C))^1.5 / 1.5
      k = traction.motors
      time = (v1*v1 - v0*v0)/(2.0*k)
      dist = (Math.pow (2.0*k * time + v0*v0) 1.5 - Math.pow (v0*v0) 1.5) / (3.0*k)
    in { time, dist }
  | otherwise = toVelocity traction (Pair v0 (crossover traction)) + toVelocity traction (Pair (crossover traction) v1)

-- | Get the time, distance, and velocity when an acceleration curve and a deceleration curve intersect.
intersect :: Traction -> { veloc :: Pair Number, dist :: Number } -> { dist :: Pair Number, time :: Pair Number, veloc :: Number }
intersect traction { veloc: veloc@(Pair v0 v1), dist } =
  let
    adjust = toVelocity traction veloc
    meeting = (dist + adjust.dist) / 2.0
    mk d0 d1 =
      let
        i0 = maxAtDistance traction v0 d0
        i1 = maxAtDistance traction v1 d1
      in { dist: Pair d0 d1, time: Pair i0.time i1.time, veloc: (i0.veloc + i1.veloc) / 2.0 }
  in case compare v0 v1 of
    EQ -> mk (dist / 2.0) (dist / 2.0)
    LT -> mk meeting (dist - meeting)
    GT -> mk (dist - meeting) meeting

-- | Evaluate the movement curve at a given time, returning the distance traveled when starting at an initial velocity and accelerating.
curve :: Traction -> "veloc" @:: Number -> "time" @:: Number -> { dist :: Number, veloc :: Number, accel :: Number }
curve traction veloc =
  let
    vc = crossover traction
    tc = (vc - veloc) / traction.wheels
  in \time ->
    -- TODO: negative time?
    let
      t1 = clamp 0.0 (max 0.0 tc) $ time
      v1 = veloc + traction.wheels * t1
      x1 = (veloc + v1) / 2.0 * t1

      t2 = time - t1
      k = traction.motors
      x2 = (Math.pow (2.0*k * t2 + v1*v1) 1.5 - Math.pow (v1*v1) 1.5) / (3.0*k)
      v2 = Math.sqrt (2.0*k * t2 + v1*v1)

      -- _ = spy "curve" { veloc, time, tc, t1, v1, x1, t2, x2, v2, x2R: (Math.pow (v1*v1) 1.5) / 1.5, x: x1 + x2, f: \t2 -> ((Math.pow (2.0*k * t2 + v1*v1) 1.5) / 1.5 - (Math.pow (v1*v1) 1.5) / 1.5) / Math.sqrt (2.0*k) }
    in { dist: x1 + x2, veloc: v2, accel: maxAccelAtVeloc traction veloc }




-- | In each segment of a speed plan, the train will either be holding a limit, or accelerating from a starting velocity, or decelerating to a velocity target.
data SpeedSegment
  = Limit ("veloc" @:: Number) -- constant at speed limit
  | Accel ("veloc" @:: Number) -- accelerating from velocity
  | Decel ("veloc" @:: Number) -- decelerating from velocity

-- | A speed plan is segmentwise plan, segmented by distance along the track.
data SpeedPlan =
  SpeedPlan
    -- | Must be Limit or Decel
    SpeedSegment
    -- | All of the segmentwise components, indexed by start
    (Map ("dist" @:: Number) SpeedSegment)

-- | A speed schedule contains time and distance at each checkpoint.
data SpeedSchedule =
  SpeedSchedule
    Traction
    SpeedSegment
    (Map ("time" @:: Number) { dist :: Bounds Number, plan :: SpeedSegment, time :: Bounds Number, dur :: Number })
    (Map ("dist" @:: Number) { dist :: Bounds Number, plan :: SpeedSegment, time :: Bounds Number, dur :: Number })
    ("time_total" @:: Number)

-- | Overall limit for the whole plan.
planLimit :: "veloc" @:: Number -> SpeedPlan
planLimit v = SpeedPlan (Limit v) Map.empty

-- | Plan a restricted speed for a zone or point.
planZone :: "veloc" @:: Number -> "dist" @:: Bounds Number -> SpeedPlan
planZone v { min: Min d0, max: Max d1 } = SpeedPlan
  (Decel v)
  (Map.fromFoldable [ min d0 d1 /\ Limit v, max d0 d1 /\ Accel v ])

-- | A segment may need to be split.
data SpeedSplit
  = SpeedSegment SpeedSegment
  | SpeedSplit SpeedSegment ("dist" @:: Pair Number) SpeedSegment

-- | Combine two segments. This may require splitting at their intersection.
combineLimit ::
  Traction ->
  Pair SpeedSegment ->
  "span" @:: Number ->
  SpeedSplit
combineLimit traction (Pair p0 p1) dist = case p0, p1 of
  Limit l1, Limit l2 -> SpeedSegment $ Limit $ min l1 l2
  Accel v1, Accel v2 -> SpeedSegment $ Accel $ min v1 v2
  Decel v1, Decel v2 -> SpeedSegment $ Decel $ min v1 v2

  -- Accel + Limit
  Accel v, Limit l
    | l <= v -> SpeedSegment $ Limit l
    -- Accelerates to limit
    | otherwise -> splitL (Accel v) (toVelocity traction $ Pair v l) (Limit l)
  Limit l, Accel v
    | l <= v -> SpeedSegment $ Limit l
    -- Accelerates to limit
    | otherwise -> splitL (Accel v) (toVelocity traction $ Pair v l) (Limit l)

  -- Limit + Decel
  Limit l, Decel v
    | l <= v -> SpeedSegment $ Limit l
    -- Decelerates from limit
    | otherwise -> splitR (Limit l) (toVelocity traction $ Pair v l) (Decel v)
  Decel v, Limit l
    | l <= v -> SpeedSegment $ Limit l
    -- Decelerates from limit
    | otherwise -> splitR (Limit l) (toVelocity traction $ Pair v l) (Decel v)

  -- Accel + Decel
  Accel v0, Decel v1 -> SpeedSplit (Accel v0) (_.dist $ intersect traction { veloc: Pair v0 v1, dist }) (Decel v1)
  Decel v1, Accel v0 -> SpeedSplit (Accel v0) (_.dist $ intersect traction { veloc: Pair v1 v0, dist }) (Decel v1)

  where

  splitL l { dist: d } r | d < dist = SpeedSplit l (Pair d (dist - d)) r
  splitL l _ _ = SpeedSegment l

  splitR l { dist: d } r | d < dist = SpeedSplit l (Pair (dist - d) d) r
  splitR _ _ r = SpeedSegment r


-- | Split a segment.
splitSegment :: Traction -> SpeedSegment -> "dist" @:: Number -> Pair SpeedSegment
splitSegment _ (Limit v) _ = pure (Limit v)
splitSegment traction (Accel v) dist = Pair (Accel v) (Accel $ _.veloc $ maxAtDistance traction v dist)
splitSegment traction (Decel v) dist = Pair (Decel $ _.veloc $ maxAtDistance traction v dist) (Decel v)



-- | Get the segment that governs a distance marker, including its endpoints.
plannedSegment :: SpeedPlan -> "dist" @:: Number -> { dist :: Bounds Number, plan :: SpeedSegment }
plannedSegment (SpeedPlan initial segments) dist =
  case Map.lookupLE dist segments, Map.lookupGT dist segments of
    Just { key: d0, value }, Just { key: d1 } ->
      { plan: value, dist: mkBounds d0 d1 }
    Just { key: d0, value }, Nothing ->
      { plan: value, dist: mkBounds d0 Math.infinity }
    Nothing, Just { key: d1 } ->
      { plan: initial, dist: mkBounds (negate Math.infinity) d1 }
    Nothing, Nothing ->
      { plan: initial, dist: mkBounds (negate Math.infinity) Math.infinity }

-- | Evaluate the plan at the exact distance.
planAtDist :: Traction -> SpeedPlan -> "dist" @:: Number -> { veloc :: Number, accel :: Number }
planAtDist traction plan dist = segmentAtDist traction (plannedSegment plan dist) dist
  # \{ veloc, accel } -> { veloc, accel }

-- | Evaluate the segment at the exact distance.
segmentAtDist :: forall r. Traction -> { dist :: Bounds Number, plan :: SpeedSegment | r } -> "dist" @:: Number -> { veloc :: Number, accel :: Number, time :: Number }
segmentAtDist traction segment dist = case segment of
  { plan: Limit v }  -> { accel: zero, veloc: v, time: dist / v }
  { plan: Accel v, dist: { min: Min d0 } } -> maxAtDistance traction v (dist - d0)
  { plan: Decel v, dist: { max: Max d1 } } -> maxAtDistance traction v (d1 - dist) * { accel: negate one, veloc: one, time: one }

-- | Calculate the time needed for a segment.
segmentTime :: Traction -> SpeedSegment -> "dist" @:: Number -> "time" @:: Number
segmentTime traction segment dist =
  case segment of
    Limit v -> dist / v
    Accel v -> maxAtDistance traction v dist # _.time
    Decel v -> maxAtDistance traction v dist # _.time

-- | Turn a plan of speeds into an actual schedule, with timing for each segment.
schedulePlan :: Traction -> SpeedPlan -> SpeedSchedule
schedulePlan traction (SpeedPlan init segments) = SpeedSchedule traction init (Map.fromFoldable byTime) (Map.fromFoldable byDist) accum
  where
  fn = \d0 t segment ->
    let
      d1 = maybe Math.infinity _.key $ Map.lookupGT d0 segments
      dur = if Math.isFinite d1 then segmentTime traction segment (d1 - d0) else 0.0
    in { accum: t + dur, value: Tuple t { dist: mkBounds d0 d1, plan: segment, dur, time: mkBounds t (t + dur) } }
  { value: byTime, accum } = mapAccumLWithIndex fn 0.0 segments
  byDist = byTime <#> \(Tuple _ item@{ dist: { min: Min d } }) -> Tuple d item

-- | Get the total time for a schedule (cached).
totalTime :: SpeedSchedule -> "time" @:: Number
totalTime (SpeedSchedule _ _ _ _ total) = total

-- | Get the distance (and velocity and acceleration) for a schedule at the given time.
scheduleAtTime :: SpeedSchedule -> "time" @:: Number -> { dist :: Number, veloc :: Number, accel :: Number }
scheduleAtTime (SpeedSchedule traction init segments _ _) time =
  case Map.lookupLE time segments of
    Nothing ->
      case init, Map.findMin segments of
        Limit v, Just { value: { dist: { min: Min d0 } } } ->
          { dist: d0 + v * time, veloc: v, accel: 0.0 }
        Decel v, Just { value: { dist: { min: Min d0 } } } ->
          let dist = d0 - (curve traction v (-time)).dist
              { accel, veloc } = maxAtDistance traction v (d0 - dist) * { accel: negate one, veloc: one, time: one }
          in { dist, veloc, accel }
        _, _ -> unsafeCrashWith "scheduleAtTime"
    Just { key: t0, value: value@{ dist: { min: Min d0, max: Max d1 }, plan, dur: duration } } ->
      let
        t = time - t0
        dist = case plan of
          Limit v -> d0 + v * t
          Accel v -> d0 + (curve traction v t).dist
          Decel v -> d1 - (curve traction v (duration - t)).dist
        { veloc, accel } = segmentAtDist traction value dist
      in { dist, veloc, accel }


unfoldPlan :: SpeedPlan -> NonEmpty List { dist :: Bounds Number, plan :: SpeedSegment }
unfoldPlan (SpeedPlan init segments) =
  _.value $ mapAccumR addEndpoint Math.infinity $
    Tuple (negate Math.infinity) init :| Map.toUnfoldable segments
  where
  addEndpoint end (Tuple start segment) =
    { accum: start, value: { dist: mkBounds start end, plan: segment } }

refoldPlan :: NonEmpty List { dist :: Bounds Number, plan :: SpeedSegment } -> SpeedPlan
-- TODO: simplify
refoldPlan ({ plan: init } :| rest) = SpeedPlan init $ Map.fromFoldable $
  rest <#> \{ dist: { min: Min start }, plan } -> Tuple start plan

seg4dbg :: forall r.
  { dist :: Bounds Number, plan :: SpeedSegment | r } ->
  { d0 :: Number, d1 :: Number, plan :: String, v :: Number }
seg4dbg { dist: { min: Min d0, max: Max d1 }, plan } =
  case plan of
    Limit v -> { d0, d1, plan: "Limit", v }
    Accel v -> { d0, d1, plan: "Accel", v }
    Decel v -> { d0, d1, plan: "Decel", v }

-- | Combine two speed plans, taking the minimum allowed velocity at each point on the track.
combinePlans :: Traction -> SpeedPlan -> SpeedPlan -> SpeedPlan
combinePlans traction planL0 planR0 =
  refoldPlan $ zipPlans (unfoldPlan planL0) (unfoldPlan planR0)
  where
  -- TODO: quick scan to make it less quadratic?
  zipPlans (_l@{ dist: { min: Min d0, max: Max d1L }, plan: planL } :| moreL) (_r@{ dist: { max: Max d1R }, plan: planR } :| moreR) =
    -- let _ = spy "l" $ seg4dbg _l in
    -- let _ = spy "r" $ seg4dbg _r in
    case compare d1L d1R of
      EQ -> intoTail d0 d1L moreL moreR $ combineLimit traction (Pair planL planR) (d1L - d0)
      LT ->
        let Pair plan0 plan1 = splitSegment traction planR (d1L - d0) in
        -- let _ = spy "plan0" $ seg4dbg { dist: Pair d0 d1L, plan: plan0 } in
        intoTail d0 d1L moreL ({ dist: mkBounds d1L d1R, plan: plan1 } : moreR) $
          combineLimit traction (Pair plan0 planL) (d1L - d0)
      GT ->
        let Pair plan0 plan1 = splitSegment traction planL (d1R - d0) in
        intoTail d0 d1R ({ dist: mkBounds d1R d1L, plan: plan1 } : moreL) moreR $
          combineLimit traction (Pair plan0 planR) (d1R - d0)

  choose x y | Math.isNaN x = y
  choose x _ = x

  intoTail :: Number -> Number -> List _ -> List _ -> SpeedSplit -> _
  intoTail d0 d2 moreL moreR = case _ of
    SpeedSplit p0 (Pair split0 split2) p1 ->
      let d1 = choose (d0 + split0) (d2 - split2) in
      { dist: mkBounds d0 d1, plan: p0 } :| { dist: mkBounds d1 d2, plan: p1 } : zipTail moreL moreR
    SpeedSegment plan -> { dist: mkBounds d0 d2, plan } :| zipTail moreL moreR

  zipTail (x : xs) (y : ys) = let z :| zs = zipPlans (x :| xs) (y :| ys) in z : zs
  zipTail Nil ys = ys
  zipTail xs Nil = xs
