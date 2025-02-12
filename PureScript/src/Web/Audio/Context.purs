module Web.Audio.Context where

import Prelude

import Effect (Effect)
import Web.Audio.Types (AudioContext)

createAudioContext :: {} -> Effect AudioContext
createAudioContext = _createAudioContext

foreign import _createAudioContext :: forall opts. opts -> Effect AudioContext

