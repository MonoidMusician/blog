module Riverdragon.Roar.Viz where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.ResourceM (destr)
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafeCrashWith)
import Riverdragon.Dragon.Breath (removeSelf)
import Riverdragon.River as River
import Riverdragon.River.Beyond (everyFrame)
import Riverdragon.Roar.Score (ScoreM)
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
oscilloscope config audio = do
  { ctx } <- ask
  rawElement <- liftEffect do
    createElement (ElementName "canvas") <<<
      HTMLDocument.toDocument =<< document =<< window
  element <- case Canvas.fromElement rawElement of
    Nothing -> unsafeCrashWith "Not a canvas element"
    Just element -> pure element
  destr do removeSelf (Element.toNode rawElement)
  River.subscribe (toLake config.width) (Canvas.setWidth <@> element)
  River.subscribe (toLake config.height) (Canvas.setHeight <@> element)

  fftSize <- liftEffect $ map (fromMaybe FFT1024) $ River.burstOf (toLake config.width) <#> do
    Array.last >=> \width ->
      enumFromTo bottom top
        # Array.find \candidate -> toFFI candidate >= width
  node <- liftEffect do createAnalyserNode ctx { fftSize, smoothingTimeConstant: 0.2 }
  connecting (toRoars audio) (intoNode node 0)
  let render = _oscilloscope node element
  River.subscribe everyFrame $ const render

  pure element

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
spectrogram config audio = do
  { ctx } <- ask
  rawElement <- liftEffect do
    createElement (ElementName "canvas") <<<
      HTMLDocument.toDocument =<< document =<< window
  element <- case Canvas.fromElement rawElement of
    Nothing -> unsafeCrashWith "Not a canvas element"
    Just element -> pure element
  destr do removeSelf (Element.toNode rawElement)
  River.subscribe (toLake config.width) (Canvas.setWidth <@> element)
  River.subscribe (toLake config.height) (Canvas.setHeight <@> element)

  node <- liftEffect do createAnalyserNode ctx { fftSize: FFT1024, smoothingTimeConstant: 0.0 }
  connecting (toRoars audio) (intoNode node 0)
  let render = _spectrogram node element
  River.subscribe everyFrame $ const render

  pure element

foreign import _spectrogram ::
  AnalyserNode ->
  HTMLCanvasElement ->
  (Effect Unit)
