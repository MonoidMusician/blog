module Riverdragon.Roar.Viz where

import Prelude

import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Riverdragon.Dragon.Breath (removeSelf)
import Riverdragon.River as River
import Riverdragon.River.Beyond (everyFrame)
import Riverdragon.Roar.Score (ScoreM, scoreElement)
import Riverdragon.Roar.Types (class ToLake, class ToRoars, connecting, toLake, toRoars)
import Web.Audio.FFI (toFFI)
import Web.Audio.Node (AnalyserNode, createAnalyserNode, intoNode)
import Web.Audio.Types (FFTSize(..))
import Web.DOM.Document (createElement)
import Web.DOM.Element as Element
import Web.DOM.ElementName (ElementName(..))
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement as Canvas
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)

oscilloscope ::
  forall flowWidth flowHeight roar.
    ToLake flowWidth Int =>
    ToLake flowHeight Int =>
    ToRoars roar =>
  { width :: flowWidth
  , height :: flowHeight
  } ->
  roar ->
  ScoreM HTMLCanvasElement
oscilloscope config audio = scoreElement \{ ctx } -> do
  rawElement <- createElement (ElementName "canvas") <<<
    HTMLDocument.toDocument =<< document =<< window
  element <- case Canvas.fromElement rawElement of
    Nothing -> unsafeCrashWith "Not a canvas element"
    Just element -> pure element
  let destroy0 = removeSelf (Element.toNode rawElement)
  destroy1 <- River.subscribe (toLake config.width) (Canvas.setWidth <@> element)
  destroy2 <- River.subscribe (toLake config.height) (Canvas.setHeight <@> element)

  fftSize <- map (fromMaybe FFT1024) $ River.burstOf (toLake config.width) <#> do
    Array.last >=> \width ->
      enumFromTo bottom top
        # Array.find \candidate -> toFFI candidate >= width
  node <- createAnalyserNode ctx { fftSize, smoothingTimeConstant: 0.2 }
  destroy3 <- connecting (toRoars audio) (intoNode node 0)
  let render = _oscilloscope node element
  destroy4 <- River.subscribe everyFrame $ const render

  let destroy = fold [ destroy0, destroy1, destroy2, destroy3, destroy4 ]
  pure { result: element, destroy, ready: pure unit }

foreign import _oscilloscope ::
  AnalyserNode ->
  HTMLCanvasElement ->
  (Effect Unit)

spectrogram ::
  forall flowWidth flowHeight roar.
    ToLake flowWidth Int =>
    ToLake flowHeight Int =>
    ToRoars roar =>
  { width :: flowWidth
  , height :: flowHeight
  } ->
  roar ->
  ScoreM HTMLCanvasElement
spectrogram config audio = scoreElement \{ ctx } -> do
  rawElement <- createElement (ElementName "canvas") <<<
    HTMLDocument.toDocument =<< document =<< window
  element <- case Canvas.fromElement rawElement of
    Nothing -> unsafeCrashWith "Not a canvas element"
    Just element -> pure element
  let destroy0 = removeSelf (Element.toNode rawElement)
  destroy1 <- River.subscribe (toLake config.width) (Canvas.setWidth <@> element)
  destroy2 <- River.subscribe (toLake config.height) (Canvas.setHeight <@> element)

  node <- createAnalyserNode ctx { fftSize: FFT1024, smoothingTimeConstant: 0.0 }
  destroy3 <- connecting (toRoars audio) (intoNode node 0)
  let render = _spectrogram node element
  destroy4 <- River.subscribe everyFrame $ const render

  let destroy = fold [ destroy0, destroy1, destroy2, destroy3, destroy4 ]
  pure { result: element, destroy, ready: pure unit }

foreign import _spectrogram ::
  AnalyserNode ->
  HTMLCanvasElement ->
  (Effect Unit)
