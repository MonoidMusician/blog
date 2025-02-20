module Web.Audio.MIDI where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds)
import Effect.Aff as Aff
import Partial.Unsafe (unsafeCrashWith)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Record.Unsafe.Union (unsafeUnion)
import Unsafe.Coerce (unsafeCoerce)
import Web.Audio.FFI (class FFI, fromFFI)

data PermissionStatus = Granted | Denied | Prompt
instance FFI PermissionStatus String where
  toFFI Granted = "granted"
  toFFI Denied = "denied"
  toFFI Prompt = "prompt"
  fromFFI "granted" = Granted
  fromFFI "denied" = Denied
  fromFFI "prompt" = Prompt
  fromFFI _ = unsafeCrashWith "Invalid PermissionStatus"

getPermissionStatusMIDI ::
  forall options unused.
    Row.Union options unused ( sysex :: Boolean ) =>
  Record options ->
  Aff PermissionStatus
getPermissionStatusMIDI opts = Aff.makeAff \cb -> mempty <$ do
  _getPermissionStatusMIDI (unsafeUnion opts { sysex: false })
    (cb <<< Right <<< fromFFI)
    (cb <<< Left)

foreign import _getPermissionStatusMIDI ::
  { sysex :: Boolean } ->
  (String -> Effect Unit) ->
  (Error -> Effect Unit) ->
  Effect Unit

foreign import data MIDIAccess :: Type

foreign import data MIDIPort :: Boolean -> Type
type MIDIInput = MIDIPort True
type MIDIOutput = MIDIPort False

requestMIDI ::
  forall options unused.
    Row.Union options unused ( sysex :: Boolean, software :: Boolean ) =>
  Record options ->
  Aff { access :: MIDIAccess, inputs :: Array MIDIInput, outputs :: Array MIDIOutput }
requestMIDI opts = Aff.makeAff \cb -> mempty <$ do
  _requestMIDI (unsafeUnion opts { sysex: false, software: false })
    (cb <<< Right)
    (cb <<< Left)

foreign import _requestMIDI ::
  { sysex :: Boolean, software :: Boolean } ->
  ({ access :: MIDIAccess, inputs :: Array MIDIInput, outputs :: Array MIDIOutput } -> Effect Unit) ->
  (Error -> Effect Unit) ->
  Effect Unit

foreign import access :: MIDIAccess -> Effect { inputs :: Array MIDIInput, outputs :: Array MIDIOutput }
foreign import open :: forall ty. MIDIPort ty -> Effect Unit
foreign import close :: forall ty. MIDIPort ty -> Effect Unit

foreign import onstatechange :: forall ty. MIDIPort ty -> Effect Unit -> Effect (Effect Unit)
foreign import onmidimessage :: MIDIInput -> (Array Int -> Effect Unit) -> Effect (Effect Unit)
foreign import send :: MIDIOutput -> Array Int -> Effect Unit
foreign import schedule :: MIDIOutput -> (Milliseconds -> Milliseconds) -> Array Int -> Effect Unit

type MIDIInfo =
  { id :: String
  , manufacturer :: String
  , name :: String
  , version :: String
  }

getInfo :: forall ty. MIDIPort ty -> MIDIInfo
getInfo = unsafeCoerce >>> fromFFI

type MIDIStatus =
  { state :: IsConnected
  , connection :: ConnectionStatus
  }

getStatus :: forall ty. MIDIPort ty -> Effect MIDIStatus
getStatus midi = do
  pure unit
  pure (unsafeCoerce midi # fromFFI)

data IsConnected = Connected | Disconnected
instance FFI IsConnected String where
  toFFI Connected = "connected"
  toFFI Disconnected = "disconnected"
  fromFFI "connected" = Connected
  fromFFI "disconnected" = Disconnected
  fromFFI _ = unsafeCrashWith "Invalid IsConnected"

data ConnectionStatus = Open | Closed | Pending
instance FFI ConnectionStatus String where
  toFFI Open = "open"
  toFFI Closed = "closed"
  toFFI Pending = "pending"
  fromFFI "open" = Open
  fromFFI "closed" = Closed
  fromFFI "pending" = Pending
  fromFFI _ = unsafeCrashWith "Invalid ConnectionStatus"



