module Web.Audio.Worklet where

import Prelude

import Data.Argonaut as Json
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.MediaType (MediaType(..))
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Prim.Boolean (False)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Unsafe.Union as RU
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Audio.FFI (toFFI)
import Web.Audio.Types (AudioContext, AudioNode, Rate, Float)
import Web.File.Blob as Blob
import Web.File.Url (createObjectURL, revokeObjectURL)

-- loadAudioWorkletNode :: AudioContext -> Aff.Aff ({} -> Effect (AudioNode "AudioWorkletNode" False () () ()))
-- loadAudioWorkletNode ctx = Aff.makeAff \cb ->
--   mempty <$ _loadAudioWorkletNode ctx "assets/js/workers/audio/pink.js" "PinkNoiseProcessor" (cb <<< Right) (cb <<< Left)

type CreatedWorklet name params send receive =
  { node :: AudioNode name False () () params
  , send :: send -> Effect Unit
  , receive :: (receive -> Effect Unit) -> Effect (Effect Unit)
  , destroy :: Effect Unit
  }

foreign import _loadAudioWorkletNode ::
  forall options name params send receive.
  AudioContext -> String -> String ->
  ((options -> Effect (CreatedWorklet name params send receive)) -> Effect Unit) ->
  (Error -> Effect Unit) ->
  Effect Unit

foreign import data ParamDesc :: Type

class MkParamDescs
  (ir :: Row Type)
  (il :: RL.RowList Type)
  (or :: Row Rate)
  (vr :: Row Type)
  | il -> ir or vr
  where
    mkParamDescs :: Proxy il -> Record ir -> Array ParamDesc

instance MkParamDescs () RL.Nil () () where
  mkParamDescs _ _ = []
instance
  ( IsSymbol name
  , Row.Cons name (Record given) ir ir'
  , Row.Lacks name ir
  , Row.Cons name rate or or'
  , Row.Cons name (Maybe Float) vr vr'
  , Row.Union given unused
      ( defaultValue :: Float
      , minValue :: Float
      , maxValue :: Float
      )
  , Reflectable rate Rate
  , MkParamDescs ir il or vr
  ) => MkParamDescs ir' (RL.Cons name (Record given) il) or' vr' where
    mkParamDescs _ ir =
      [ unsafeCoerce $ RU.unsafeUnion
          { name: reflectSymbol (Proxy :: Proxy name)
          , animationRate: toFFI (reflectType (Proxy :: Proxy rate) :: Rate)
          } (Record.get (Proxy :: Proxy name) ir)
      ] <> mkParamDescs (Proxy :: Proxy il) (unsafeCoerce ir :: Record ir)

mkAudioWorkletteNode ::
  forall
    given unused
    send receive -- Structured Clone
    parameterOptions il parameterData defaultParams paramVals params
    processorOptions. -- Structured Clone

    Row.Union given unused
      ( numberOfInputs :: Int
      , numberOfOutputs :: Int
      , outputChannelCount :: Array Int
      , parameterData :: Record parameterData
      , processorOptions :: processorOptions
      ) =>
    RL.RowToList parameterOptions il =>
    MkParamDescs parameterOptions il params paramVals =>
    Row.Union parameterData defaultParams paramVals =>
  { name :: String
  , function :: String
  , parameters :: Record parameterOptions
  } ->
  AudioContext -> Aff.Aff
    (Record given -> Effect
      { node :: AudioNode "AudioWorkletNode" False () () params
      , send :: send -> Effect Unit
      , receive :: (receive -> Effect Unit) -> Effect (Effect Unit)
      , destroy :: Effect Unit
      }
    )
mkAudioWorkletteNode { name, function, parameters } ctx = do
  let
    _1 /\ _2 /\ _3 /\ _4 = sourceWrapping
    params = Json.stringifyWithIndent 2 $ unsafeCoerce $
      mkParamDescs (Proxy :: Proxy il) parameters
    nameSource = Json.stringify $ Json.fromString name
    source = _1 <> nameSource <> _2 <> function <> _3 <> params <> _4
  -- url <- liftEffect $ createObjectURL $ Blob.fromString source (MediaType "text/javascript")
  r <- Aff.makeAff \cb ->
    mempty <$ _loadAudioWorkletNode ctx source name (cb <<< Right) (cb <<< Left)
  -- liftEffect $ revokeObjectURL url
  pure r

sourceWrapping :: String /\ String /\ String /\ String
sourceWrapping =
  """
    function makeWorklette(name, fn, parameterDescriptors) {
      registerProcessor(name, class extends AudioWorkletProcessor {
        constructor(options) {
          super(options);
          this.process = this.processor = fn.call(this, options, this.port);
        }
        static parameterDescriptors = parameterDescriptors;
        // Chromium wants to call `process` from the prototype
        process(inputs, outputs, parameters) {
          return this.processor(inputs, outputs, parameters);
        }
      });
    }

    makeWorklette(
  """ /\
  """
    ,
  """ /\
  """
    ,
  """ /\
  """
    )
  """
