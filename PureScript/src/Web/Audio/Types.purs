module Web.Audio.Types where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, cardinality)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Reflectable (class Reflectable, class Reifiable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Type.Proxy (Proxy)
import Unsafe.Reference (unsafeRefEq)
import Web.Audio.FFI (class FFI, _undefined, _unsafeGetProperty, _unsafeSetProperty, fromFFI, toFFI)

type Float = Number
type Norm = Float
type Duration = Number
type Time = Number
type Volume = Number
type Frequency = Number
type Cents = Number

type SequenceFloat = Array Float


-- | Audio rate (sample rate) and control rate (per chunk of audio, 128 samples)
data Rate = ARate | KRate
derive instance Eq Rate
foreign import data ARate :: Rate
foreign import data KRate :: Rate
instance Reflectable ARate Rate where reflectType _ = ARate
instance Reflectable KRate Rate where reflectType _ = KRate
instance Reifiable Rate
instance FFI Rate String where
  toFFI ARate = "a-rate"
  toFFI KRate = "k-rate"
  fromFFI "a-rate" = ARate
  fromFFI "k-rate" = KRate
  fromFFI _ = unsafeCrashWith "Invalid Rate"

foreign import data AudioParam :: Rate -> Type
instance Eq (AudioParam rate) where eq = unsafeRefEq

foreign import data AudioContext :: Type

currentTime :: AudioContext -> Effect Time
currentTime ctx = _unsafeGetProperty ctx "currentTime"

foreign import data AudioNode :: Symbol -> Boolean -> Row Type -> Row Type -> Row Rate -> Type
instance Eq (AudioNode name source read write params) where eq = unsafeRefEq

foreign import data AudioBuffer :: Type

instance FFI AudioBuffer AudioBuffer where
  toFFI = identity
  fromFFI = identity

getProp ::
  forall name source read write' write params prop t d.
    FFI t d =>
    IsSymbol prop =>
    Row.Cons prop t write' write =>
  AudioNode name source read write params ->
  Proxy prop -> Effect t
getProp node prop = do
  fromFFI <$> _unsafeGetProperty node (reflectSymbol prop)

setProp ::
  forall name source read write' write params prop t d.
    FFI t d =>
    IsSymbol prop =>
    Row.Cons prop t write' write =>
  AudioNode name source read write params ->
  Proxy prop -> t -> Effect Unit
setProp node prop value = do
  _unsafeSetProperty node (reflectSymbol prop) (toFFI value)

getAttr ::
  forall name source read' read write params prop t d.
    FFI t d =>
    IsSymbol prop =>
    Row.Cons prop t read' read =>
  AudioNode name source read write params ->
  Proxy prop -> t
getAttr node prop = fromFFI $ unsafePerformEffect do
  _unsafeGetProperty node (reflectSymbol prop)

getParam ::
  forall name source read write params' params param rate.
    IsSymbol param =>
    Row.Cons param rate params' params =>
  AudioNode name source read write params ->
  Proxy param ->
  AudioParam rate
getParam node param = unsafePerformEffect do
  _unsafeGetProperty node (reflectSymbol param)


data Ramp
  = NoRamp
  | ExpRamp
  | LinRamp
  | DecayRamp Duration

-- | `Hold` is not supported in Firefox https://bugzilla.mozilla.org/show_bug.cgi?id=1308431
data Cancel = Revert -- | Hold

data AudioParamCmd
  = CmdTarget { time :: Time, target :: Number, ramp :: Ramp }
  | CmdCurve { curve :: SequenceFloat, time :: Time, duration :: Duration }
  | CmdCancel { time :: Time, hold :: Cancel }


data ChannelCountMode
  = ExactChannels Int
  | ClampedChannels Int
  | MaxChannels
instance FFI ChannelCountMode { channelCount :: Int, channelCountMode :: String } where
  toFFI = case _ of
    ExactChannels channelCount -> { channelCount, channelCountMode: "explicit" }
    ClampedChannels channelCount -> { channelCount, channelCountMode: "clamped-max" }
    MaxChannels -> { channelCount: 0, channelCountMode: "max" }
  fromFFI = case _ of
    { channelCount, channelCountMode: "explicit" } -> ExactChannels channelCount
    { channelCount, channelCountMode: "clamped-max" } -> ClampedChannels channelCount
    { channelCountMode: "max" } -> MaxChannels
    _ -> unsafeCrashWith "Invalid ChannelCountMode"

data ChannelInterpretation = Speakers | Discrete
instance FFI ChannelInterpretation String where
  toFFI = case _ of
    Speakers -> "speakers"
    Discrete -> "discrete"
  fromFFI = case _ of
    "speakers" -> Speakers
    "discrete" -> Discrete
    _ -> unsafeCrashWith "Invalid ChannelInterpretation"



data FFTSize
  = FFT32
  | FFT64
  | FFT128
  | FFT256
  | FFT512
  | FFT1024
  | FFT2048
  | FFT4096
  | FFT8192
  | FFT16384
  | FFT32768

data BiquadFilterType
  = Lowpass
  | Highpass
  | Bandpass
  | Lowshelf
  | Highshelf
  | Peaking
  | Notch
  | Allpass

instance FFI BiquadFilterType String where
  toFFI = case _ of
    Lowpass -> "lowpass"
    Highpass -> "highpass"
    Bandpass -> "bandpass"
    Lowshelf -> "lowshelf"
    Highshelf -> "highshelf"
    Peaking -> "peaking"
    Notch -> "notch"
    Allpass -> "allpass"
  fromFFI = case _ of
    "lowpass" -> Lowpass
    "highpass" -> Highpass
    "bandpass" -> Bandpass
    "lowshelf" -> Lowshelf
    "highshelf" -> Highshelf
    "peaking" -> Peaking
    "notch" -> Notch
    "allpass" -> Allpass
    _ -> unsafeCrashWith "Invalid BiquadType"

data Oversample
  = Oversample1x
  | Oversample2x
  | Oversample4x

instance FFI Oversample String where
  toFFI = case _ of
    Oversample1x -> "none"
    Oversample2x -> "2x"
    Oversample4x -> "4x"
  fromFFI = case _ of
    "none" -> Oversample1x
    "2x" -> Oversample2x
    "4x" -> Oversample4x
    _ -> unsafeCrashWith "Invalid Oversample"

data PanningModelType
  = EqualPower
  | HRTF

instance FFI PanningModelType String where
  toFFI = case _ of
    EqualPower -> "equalpower"
    HRTF -> "HRTF"
  fromFFI = case _ of
    "equalpower" -> EqualPower
    "HRTF" -> HRTF
    _ -> unsafeCrashWith "Invalid PanningModelType"

data DistanceModelType
  = LinearModel
  | InverseModel
  | ExponentialModel

instance FFI DistanceModelType String where
  toFFI = case _ of
    LinearModel -> "linear"
    InverseModel -> "inverse"
    ExponentialModel -> "exponential"
  fromFFI = case _ of
    "linear" -> LinearModel
    "inverse" -> InverseModel
    "exponential" -> ExponentialModel
    _ -> unsafeCrashWith "Invalid DistanceModelType"

foreign import data PeriodicWave :: Type

data OscillatorType
  = Sine
  | Square
  | Sawtooth
  | Triangle
  | Custom PeriodicWave

instance FFI OscillatorType { type :: String, periodicWave :: PeriodicWave } where
  toFFI = case _ of
    Sine -> { type: "sine", periodicWave: _undefined }
    Square -> { type: "square", periodicWave: _undefined }
    Sawtooth -> { type: "sawtooth", periodicWave: _undefined }
    Triangle -> { type: "triangle", periodicWave: _undefined }
    Custom periodicWave -> { type: "custom", periodicWave }
  -- This is not spec compliant: the OscillatorNode prioritizes a not-undefined
  -- `periodicWave: PeriodicWave` property, over the `type` (whyyyy lol)
  fromFFI = case _ of
    { type: "sine" } -> Sine
    { type: "square" } -> Square
    { type: "sawtooth" } -> Sawtooth
    { type: "triangle" } -> Triangle
    { periodicWave } -> Custom periodicWave


instance FFI FFTSize Int where
  toFFI = case _ of
    FFT32 -> 32
    FFT64 -> 64
    FFT128 -> 128
    FFT256 -> 256
    FFT512 -> 512
    FFT1024 -> 1024
    FFT2048 -> 2048
    FFT4096 -> 4096
    FFT8192 -> 8192
    FFT16384 -> 16384
    FFT32768 -> 32768
  fromFFI = case _ of
    32 -> FFT32
    64 -> FFT64
    128 -> FFT128
    256 -> FFT256
    512 -> FFT512
    1024 -> FFT1024
    2048 -> FFT2048
    4096 -> FFT4096
    8192 -> FFT8192
    16384 -> FFT16384
    32768 -> FFT32768
    _ -> unsafeCrashWith "Invalid FFTSize"

derive instance Eq FFTSize
derive instance Ord FFTSize
derive instance Generic FFTSize _
instance Enum FFTSize where
  pred = genericPred
  succ = genericSucc
instance Bounded FFTSize where
  bottom = FFT32
  top = FFT32768
instance BoundedEnum FFTSize where
  toEnum = genericToEnum
  fromEnum = genericFromEnum
  cardinality = genericCardinality
