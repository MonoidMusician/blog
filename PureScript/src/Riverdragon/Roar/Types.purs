module Riverdragon.Roar.Types where

import Prelude

import Control.Monad.ResourceM (class MonadResource, destr)
import Data.Compactable (compact)
import Data.Foldable (foldMap, traverse_)
import Data.Maybe (Maybe)
import Data.RecordOverloads (class RecordOverloads, overloads)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Riverdragon.River (Allocar, Lake, Stream, cumulate, dam, mapAl, oneStream, (>>~))
import Riverdragon.River as River
import Riverdragon.River.Bed (diffingArraySet)
import Riverdragon.River.Beyond (affToLake)
import Type.Equality (class TypeEquals, proof, to)
import Web.Audio.Node (AudioDest, AudioSrc, createPeriodicWave)
import Web.Audio.Node as AudioNode
import Web.Audio.Types (AudioContext, AudioParamCmd, Float, OscillatorType(..), SequenceFloat, Time, currentTime)

type Roar = AudioSrc

type RoarI = AudioDest
type RoarO = AudioSrc

type RoarP =
  { default :: Maybe Float
  , ctl :: RoarCtl
  }
type RoarCtl =
  -- Current value
  Allocar Float ->
  Allocar
    -- Produced value is a sum of audio inputs and the current value on
    -- the timeline generated from the commands
    { srcs :: Lake (Array RoarO)
    , cmds :: Lake AudioParamCmd
    }

withCurrentTime :: forall flow a. AudioContext -> Stream flow (Time -> a) -> Stream flow a
withCurrentTime ctx = mapAl \f -> f <$> (currentTime ctx)

connecting :: forall m. MonadResource m => Lake (Array RoarO) -> RoarI -> m Unit
connecting srcsStream dest = do
  srcs <- liftEffect diffingArraySet
  destr $ traverse_ (AudioNode.disConnect true <@> dest) =<< srcs.destroy
  River.subscribe srcsStream \newSrcs -> do
    { added, removed } <- srcs.swap newSrcs
    traverse_ (AudioNode.disConnect true <@> dest) removed
    traverse_ (AudioNode.disConnect false <@> dest) added


class ToRoars i where toRoars :: i -> Lake (Array Roar)
instance ToRoars Roar where toRoars = pure <<< pure
instance ToRoars i => ToRoars (Maybe i) where toRoars = foldMap toRoars
instance ToRoars i => ToRoars (Array i) where toRoars = cumulate <<< map toRoars
instance ToRoars i => ToRoars (Stream flow i)  where toRoars stream = stream >>~ toRoars
instance ToRoars i => ToRoars (Aff i)  where toRoars act = compact (affToLake act) >>~ toRoars

class ToLake i t where toLake :: i -> Lake t
instance ToLake i t => ToLake (Array i) t where toLake = oneStream <<< map toLake
else instance TypeEquals i t => ToLake (Stream flow i) t where toLake = dam <<< proof
else instance TypeEquals i t => ToLake (Aff i) t where toLake = compact <<< affToLake <<< proof
else instance TypeEquals i t => ToLake i t where toLake = pure <<< to

class ToOther i o where thingy :: i -> o

type OscillatorTypeOverloads =
  ( sines :: { sines :: SequenceFloat } -> OscillatorType
  , cosines :: { cosines :: SequenceFloat } -> OscillatorType
  , both :: { sines :: SequenceFloat, cosines :: SequenceFloat } -> OscillatorType
  )

instance
  ( TypeEquals ctx AudioContext
  , RecordOverloads OscillatorTypeOverloads r OscillatorType
  ) => ToOther (Record r) (ctx -> OscillatorType) where
    thingy = flip \ctx -> overloads @OscillatorTypeOverloads
      { sines: \r -> Custom $ createPeriodicWave (to ctx) false (0.0 <$ r.sines) r.sines
      , cosines: \r -> Custom $ createPeriodicWave (to ctx) false r.cosines (0.0 <$ r.cosines)
      , both: \r ->  Custom $ createPeriodicWave (to ctx) false r.cosines r.sines
      }
instance ToOther OscillatorType (ctx -> OscillatorType) where thingy = const

