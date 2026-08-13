module Train.Track where

import Prelude
import Train.Dynamics
import Train.Types

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe(..))
import Idiolect (type (@::))
import Math.Matrix (Bounds, clampBound, inv, mkBounds)
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import Train.Geometry (routeAtTime, walkPaths)



planAndScheduleRoute ::
  { route :: RoutedTrain
  , traction :: Traction
  , speeds :: Array SpeedPlan
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
planAndScheduleRoute { route: RoutedTrain rt@{ route: Route r }, traction, speeds } =
  Record.disjointUnion forRoute fromPlan
  where
  fromPlan = planAndSchedule { traction, speeds, extent: mkBounds rt.endpoints.start r.pathlength }
  forRoute =
    { route: RoutedTrain rt
    , train: \time ->
        let dist = fromPlan.animation $ clampBound fromPlan.time $ time in
        case routeAtTime rt.route dist of
          Just start -> walkPaths rt.route (start { to = inv start.to }) (Array.reverse rt.consist)
          Nothing -> unsafeCrashWith $ "Dist not found: " <> show dist <> " " <> show fromPlan.extent
    }

