module Web.Audio.Context where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Web.Audio.FFI (class FFI, toFFI)
import Web.Audio.Types (AudioContext)

data LatencyHint = Balanced | Interactive | Playback
instance FFI LatencyHint String where
  toFFI Balanced = "balanced"
  toFFI Interactive = "interactive"
  toFFI Playback = "playback"
  fromFFI "balanced" = Balanced
  fromFFI "interactive" = Interactive
  fromFFI "playback" = Playback
  fromFFI _ = unsafeCrashWith "Invalid LatencyHint"

createAudioContext ::
  forall options unused ffi.
    Row.Union options unused
      ( latencyHint :: LatencyHint
      , sampleRate :: Int
      ) =>
    FFI (Record options) ffi =>
  Record options -> Effect AudioContext
createAudioContext opts = _createAudioContext (toFFI opts)

foreign import _createAudioContext :: forall opts. opts -> Effect AudioContext
foreign import close :: AudioContext -> Effect Unit
foreign import suspend :: AudioContext -> Effect Unit
foreign import resume :: AudioContext -> Effect Unit

