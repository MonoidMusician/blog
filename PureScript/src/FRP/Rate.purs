module FRP.Rate where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over2, unwrap)
import Data.Time.Duration (Milliseconds, Seconds(..))
import Data.Tuple.Nested ((/\))
import FRP.Behavior (Behavior, sampleBy)
import FRP.Behavior.Time (instant)
import FRP.Event (Event, mapAccum)

newtype Beats = Beats Number

derive instance Newtype Beats _

type RateInfo =
  { prevTime :: Maybe Seconds
  , time :: Seconds
  , prevBeats :: Maybe Beats
  , beats :: Beats
  , epochTime :: Milliseconds
  }

timeFromRate
  :: Behavior Number
  -> Event Seconds
  -> Event RateInfo
timeFromRate clengthB afE = mapAccum
  ( \{ prevTime, prevBeats } { behaviors: { clength, epochTime }, acTime } -> do
      let prevAC = fromMaybe (Seconds 0.0) prevTime
      let prevAJ = fromMaybe (Beats 0.0) prevBeats
      let gap = over2 Seconds (-) acTime prevAC
      let (adjGap :: Seconds) = over2 Seconds (/) gap (Seconds clength)
      let beats = Beats ((unwrap adjGap) + (unwrap prevAJ))
      { prevTime: Just $ acTime, prevBeats: Just beats } /\
        { prevTime, prevBeats, time: acTime, beats, epochTime }
  )
  { prevTime: Nothing, prevBeats: Nothing }
  ( sampleBy { behaviors: _, acTime: _ }
      ( { clength: _, epochTime: _ }
          <$> (clengthB)
          <*> (unInstant <$> instant)
      )
      (afE)
  )
