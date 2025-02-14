module Web.Audio.Node where

import Web.Audio.Types

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Prelude (class Eq, Unit)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Audio.FFI (class FFI, _unsafeGetProperty, toFFI)

context :: forall name source read write params. AudioNode name source read write params -> AudioContext
context node = unsafePerformEffect do
  _unsafeGetProperty node "context"

foreign import start :: forall name read write params. AudioNode name True read write params -> Time -> Effect Unit
foreign import startNow :: forall name read write params. AudioNode name True read write params -> Effect Unit
foreign import stop :: forall name read write params. AudioNode name True read write params -> Time -> Effect Unit
foreign import stopNow :: forall name read write params. AudioNode name True read write params -> Effect Unit
foreign import whenEnded :: forall name read write params. AudioNode name True read write params -> Effect Unit -> Effect Unit

foreign import data AudioSrc :: Type
instance Eq AudioSrc where eq = _eqAudioSrc
foreign import _eqAudioSrc :: AudioSrc -> AudioSrc -> Boolean
foreign import data AudioDest :: Type
instance Eq AudioDest where eq = _eqAudioDest
foreign import _eqAudioDest :: AudioDest -> AudioDest -> Boolean

outOfNode ::
  forall name source read write params.
  AudioNode name source read write params -> Int -> AudioSrc
outOfNode src outputIndex = unsafeCoerce { src, outputIndex }

intoNode ::
  forall name source read write params.
  AudioNode name source read write params -> Int -> AudioDest
intoNode dest inputIndex = unsafeCoerce { dest, inputIndex }

intoParam :: forall rate. AudioParam rate -> AudioDest
intoParam dest = unsafeCoerce { dest, inputIndex: 0 }

destination :: AudioContext -> AudioDest
destination ctx = unsafeCoerce
  { dest: unsafePerformEffect do
    _unsafeGetProperty ctx "destination"
  , inputIndex: 0
  }

foreign import disConnect :: Boolean -> AudioSrc -> AudioDest -> Effect Unit

connect ::
  forall name source read write params name' source' read' write' params'.
    Tuple (AudioNode name source read write params) Int ->
    Tuple (AudioNode name' source' read' write' params') Int ->
    Effect Unit
connect (Tuple src outputIndex) (Tuple dest inputIndex) =
  disConnect false (outOfNode src outputIndex) (intoNode dest inputIndex)

disconnect ::
  forall name source read write params name' source' read' write' params'.
    Tuple (AudioNode name source read write params) Int ->
    Tuple (AudioNode name' source' read' write' params') Int ->
    Effect Unit
disconnect (Tuple src outputIndex) (Tuple dest inputIndex) =
  disConnect true (outOfNode src outputIndex) (intoNode dest inputIndex)

control ::
  forall name source read write params rate.
    Tuple (AudioNode name source read write params) Int ->
    AudioParam rate ->
    Effect Unit
control (Tuple src outputIndex) dest =
  disConnect false (outOfNode src outputIndex) (intoParam dest)

discontrol ::
  forall name source read write params rate.
    Tuple (AudioNode name source read write params) Int ->
    AudioParam rate ->
    Effect Unit
discontrol (Tuple src outputIndex) dest =
  disConnect true (outOfNode src outputIndex) (intoParam dest)



foreign import _unsafeCreateNode ::
  forall options name source read write params.
  String -> AudioContext -> Record options -> Effect (AudioNode name source read write params)

unsafeCreateNode ::
  forall name source read write params.
  forall given ffi.
    IsSymbol name =>
    FFI (Record given) (Record ffi) =>
  AudioContext -> Record given -> Effect (AudioNode name source read write params)
unsafeCreateNode ctx opts = _unsafeCreateNode (reflectSymbol (Proxy :: Proxy name)) ctx (toFFI opts)

type CreateNodeType name source options read write params =
  forall given unused ffi.
    IsSymbol name =>
    Row.Union given unused options =>
    FFI (Record given) (Record ffi) =>
  AudioContext -> Record given -> Effect (AudioNode name source read write params)

type AnalyserNode =
  AudioNode "AnalyserNode" False
    ( frequencyBinCount :: Int
    )
    ( fftSize :: FFTSize
    , maxDecibels :: Number
    , minDecibels :: Number
    , smoothingTimeConstant :: Number
    )
    ()
createAnalyserNode ::
  CreateNodeType "AnalyserNode" False
    ( fftSize :: FFTSize
    , maxDecibels :: Number
    , minDecibels :: Number
    , smoothingTimeConstant :: Number
    , channelCountMode :: ChannelCountMode
    , channelInterpretation :: ChannelInterpretation
    )
    ( frequencyBinCount :: Int
    )
    ( fftSize :: FFTSize
    , maxDecibels :: Number
    , minDecibels :: Number
    , smoothingTimeConstant :: Number
    )
    ()
createAnalyserNode = unsafeCreateNode

type BiquadFilterNode =
  AudioNode "BiquadFilterNode" False
    ()
    ( type :: BiquadFilterType )
    ( "Q" :: ARate
    , detune :: ARate
    , frequency :: ARate
    , gain :: ARate
    )
createBiquadFilterNode ::
  CreateNodeType "BiquadFilterNode" False
    ( type :: BiquadFilterType
    , "Q" :: Maybe Float
    , detune :: Maybe Float
    , frequency :: Maybe Float
    , gain :: Maybe Float
    )
    ()
    ( type :: BiquadFilterType )
    ( "Q" :: ARate
    , detune :: ARate
    , frequency :: ARate
    , gain :: ARate
    )
createBiquadFilterNode = unsafeCreateNode

type ChannelMergerNode =
  AudioNode "ChannelMergerNode" False () () ()
createChannelMergerNode ::
  CreateNodeType "ChannelMergerNode" False
    ( numberOfInputs :: Int
    ) () () ()
createChannelMergerNode = unsafeCreateNode

type ChannelSplitterNode =
  AudioNode "ChannelSplitterNode" False () () ()
createChannelSplitterNode ::
  CreateNodeType "ChannelSplitterNode" False
    ( numberOfOutputs :: Int
    ) () () ()
createChannelSplitterNode = unsafeCreateNode

type ConstantSourceNode =
  AudioNode "ConstantSourceNode" True () () ( offset :: ARate )
createConstantSourceNode ::
  CreateNodeType "ConstantSourceNode" True
    ( offset :: Float ) () () ( offset :: ARate )
createConstantSourceNode = unsafeCreateNode

type ConvolverNode =
  AudioNode "ConvolverNode" False () () ()
createConvolverNode ::
  CreateNodeType "ConvolverNode" False
    ( buffer :: AudioBuffer
    , disableNormalization :: Boolean
    )
    () () ()
createConvolverNode = unsafeCreateNode

type DelayNode =
  AudioNode "DelayNode" False () () ( delayTime :: ARate )
createDelayNode ::
  CreateNodeType "DelayNode" False
    ( delayTime :: Duration
    , maxDelayTime :: Duration
    ) () () ( delayTime :: ARate )
createDelayNode = unsafeCreateNode

type DynamicsCompressorNode =
  AudioNode "DynamicsCompressorNode" False
    ( reduction :: Float )
    ()
    ( attack :: KRate
    , knee :: KRate
    , ratio :: KRate
    , release :: KRate
    , threshold :: KRate
    )
createDynamicsCompressorNode ::
  CreateNodeType "DynamicsCompressorNode" False
    ( attack :: Maybe Float
    , knee :: Maybe Float
    , ratio :: Maybe Float
    , release :: Maybe Float
    , threshold :: Maybe Float
    )
    ( reduction :: Float )
    ()
    ( attack :: KRate
    , knee :: KRate
    , ratio :: KRate
    , release :: KRate
    , threshold :: KRate
    )
createDynamicsCompressorNode = unsafeCreateNode

type GainNode =
  AudioNode "GainNode" False () () ( gain :: ARate )
createGainNode ::
  CreateNodeType "GainNode" False
    ( gain :: Maybe Float )
    () ()
    ( gain :: ARate )
createGainNode = unsafeCreateNode

type IIRFilterNode =
  AudioNode "IIRFilterNode" False () () ()
createIIRFilterNode ::
  CreateNodeType "IIRFilterNode" False
    ( feedforward :: SequenceFloat
    , feedback :: SequenceFloat
    ) () () ()
createIIRFilterNode = unsafeCreateNode

type OscillatorNode =
  AudioNode "OscillatorNode" True
    () ()
    ( detune :: ARate
    , frequency :: ARate
    )
createOscillatorNode ::
  CreateNodeType "OscillatorNode" True
    ( type :: OscillatorType
    , detune :: Maybe Float
    , frequency :: Maybe Float
    ) () ()
    ( detune :: ARate
    , frequency :: ARate
    )
createOscillatorNode = unsafeCreateNode

setOscillatorType :: forall read source write params.
  AudioNode "OscillatorNode" source read write params ->
  OscillatorType ->
  Effect Unit
setOscillatorType node ty = _setOscillatorType node (toFFI ty)

foreign import _setOscillatorType :: forall source read write params.
  AudioNode "OscillatorNode" source read write params ->
  { type :: String, periodicWave :: PeriodicWave } ->
  Effect Unit

type PannerNode rate =
  AudioNode "PannerNode" False
    ()
    ( panningModel :: PanningModelType
    , distanceModel :: DistanceModelType
    , refDistance :: Number
    , maxDistance :: Number
    , rolloffFactor :: Number
    , coneInnerAngle :: Number
    , coneOuterAngle :: Number
    , coneOuterGain :: Number
    )
    ( positionX :: rate -- TODO
    , positionY :: rate -- TODO
    , positionZ :: rate -- TODO
    , orientationX :: rate -- TODO
    , orientationY :: rate -- TODO
    , orientationZ :: rate -- TODO
    )
createPannerNode :: forall rate.
  CreateNodeType "PannerNode" False
    ( panningModel :: PanningModelType
    , positionX :: Float
    , positionY :: Float
    , positionZ :: Float
    , orientationX :: Float
    , orientationY :: Float
    , orientationZ :: Float
    , distanceModel :: DistanceModelType
    , refDistance :: Number
    , maxDistance :: Number
    , rolloffFactor :: Number
    , coneInnerAngle :: Number
    , coneOuterAngle :: Number
    , coneOuterGain :: Number
    )
    ()
    ( panningModel :: PanningModelType
    , distanceModel :: DistanceModelType
    , refDistance :: Number
    , maxDistance :: Number
    , rolloffFactor :: Number
    , coneInnerAngle :: Number
    , coneOuterAngle :: Number
    , coneOuterGain :: Number
    )
    ( positionX :: rate -- TODO
    , positionY :: rate -- TODO
    , positionZ :: rate -- TODO
    , orientationX :: rate -- TODO
    , orientationY :: rate -- TODO
    , orientationZ :: rate -- TODO
    )
createPannerNode = unsafeCreateNode

type StereoPannerNode =
  AudioNode "StereoPannerNode" False () () ( pan :: ARate )
createStereoPannerNode ::
  CreateNodeType "StereoPannerNode" False
    ( pan :: Float
    )
    ()
    ()
    ( pan :: ARate
    )
createStereoPannerNode = unsafeCreateNode

type WaveShaperNode =
  AudioNode "WaveShaperNode" False () () ()
createWaveShaperNode ::
  CreateNodeType "WaveShaperNode" False
    ( curve :: SequenceFloat
    , oversample :: Oversample
    ) () () ()
createWaveShaperNode = unsafeCreateNode

-- | `ctx`, `disableNormalization`, `real`, `imag`
-- |
-- | Pure: it is just a struct (no idea why `AudioContext` is required tbh)
foreign import createPeriodicWave :: AudioContext -> Boolean -> SequenceFloat -> SequenceFloat -> PeriodicWave
