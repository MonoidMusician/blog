module Train.Track where

import Prelude
import Train.Dynamics (SpeedPlan, SpeedSchedule, SpeedSegment, Traction, planAndSchedule, planLimit, planZone)
import Train.Types (PointOnRoute, Route(..), RoutedTrain(..))

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..))
import Data.Profunctor (dimap)
import Idiolect (type (@::))
import Math.Matrix (Bounds, clampBound, inv, mkBound, mkBounds)
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import Train.Dynamics as Dynamics
import Train.Geometry (routeAtTime, walkPaths)



planAndScheduleRoute ::
  { route :: RoutedTrain
  , traction :: Traction
  , speeds :: Array SpeedPlan
  , limits :: Map ("radius" @:: Int) Number
  } ->
  { route :: RoutedTrain
  , train :: "time" @:: Number -> NonEmptyArray PointOnRoute

  , plan :: SpeedPlan
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
planAndScheduleRoute { route: RoutedTrain rt@{ route: Route r }, traction, speeds, limits } =
  Record.disjointUnion forRoute fromPlan
  where
  trackBounds = mkBounds rt.endpoints.start r.pathlength
  clampDist = clampBound trackBounds
  walkFrom dir dist =
    case routeAtTime rt.route dist of
      Just start -> walkPaths rt.route (start { to = (if dir then identity else inv) start.to }) (Array.reverse rt.consist)
      Nothing -> unsafeCrashWith $ "Dist not found: " <> show dist <> " " <> show trackBounds <> " " <> show (r.segments <#> \{ pathlength: Pair _ end } -> end)
  expandBounds d0 d1 =
    mkBounds d0 (max d1 $ _.pathlength $ NEA.last $ walkFrom true d1)

  endpoints = Array.catMaybes
    [ planLimit <$> Map.lookup 0 limits
    , Just $ planZone 0.0 (mkBound rt.endpoints.start)
    , Just $ planZone 0.0 (mkBound r.pathlength)
    ]

  simpl ({ dist: { min: d0, max: Max m0 }, plan: plan0 } : { dist: { min: Min m1, max: d1 }, plan: plan1 } : more)
    | m0 >= m1 && plan0 == plan1 = simpl ({ dist: { min: d0, max: d1 <> Max m0 }, plan: plan0 } : more)
  simpl (hd : tl) = hd : simpl tl
  simpl Nil = Nil

  -- FIXME: cascade limits
  curveSpeeds =
    map (\{ plan, dist } -> planZone plan dist) $
      dimap Array.toUnfoldable Array.fromFoldable simpl $
      do
        { pathlength: Pair d0 d1, segment: Pair { radius } _ } <- NEA.toArray r.segments
        case Map.lookupGE (abs radius) limits of
          Just { value: limit } | radius /= 0 -> pure { dist: expandBounds d0 d1, plan: limit }
          _ -> []

  extraSpeeds =
    Dynamics.simplify $
      Dynamics.generatePlan traction $
        endpoints <> curveSpeeds

  fromPlan = planAndSchedule { traction, speeds: speeds <> [extraSpeeds], extent: trackBounds }
  clampTime = clampBound fromPlan.time
  forRoute =
    { route: RoutedTrain rt
    , train: \time ->
        walkFrom false $ clampDist $ fromPlan.animation $ clampTime $ time
    }

