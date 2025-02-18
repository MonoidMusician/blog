module Riverdragon.Roar.Synth where

import Prelude

import Data.Either (Either(..))
import Data.SequenceRecord (class SequenceRecord, sequenceRecord)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Riverdragon.River (Stream, Lake, stillRiver, (/?*\))
import Riverdragon.River as River
import Riverdragon.River.Beyond (fallingLeaves)
import Riverdragon.Roar.Yawn (YawnM, yawnytime)

notesToNoises ::
  forall envStreams envValues key r flow1 flow2 flow3 roar.
    SequenceRecord envStreams envValues (Stream flow1) =>
    Ord key =>
  Record envStreams ->
  Stream flow1 { key :: key, pressed :: Boolean | r } ->
  ( Record envValues ->
    key ->
    Stream flow2 Unit ->
    YawnM { value :: Array roar, leave :: Stream flow3 Unit }
  ) ->
  YawnM (Lake (Array roar))
notesToNoises envStreams noteStream oneVoice = do
  { run: startAudio } <- yawnytime
  let
    -- Passively listen the the environment streams and take their most recent
    -- values
    notesWithEnv =
      sequenceRecord envStreams /?*\ noteStream
        <#> \(env /\ { key, pressed }) -> { key, pressed, env }
    attackOrRelease { pressed, env } = if pressed then Left env else Right unit

    activeSynths = join <$> fallingLeaves attackOrRelease notesWithEnv \{ key } env release -> do
      -- This manual wiring is a bit ugly ...
      { result, destroy } <- startAudio do
        oneVoice env key (stillRiver release)
      -- Need to `destroy` the node when it leaves
      _ <- liftEffect $ River.subscribe result.leave \_ -> destroy
      -- Return the result
      pure result
  pure activeSynths
