module Riverdragon.Roar.Pixel where

import Prelude
import Riverdragon.Roar.Sky

import Control.Monad.Reader (ask)
import Control.Monad.ResourceM (addDestructor, selfDestructor, selfScope)
import Control.Monad.ResourceT (ResourceM, scopedRun, scopedStart, scopedStart_)
import Data.Argonaut as Json
import Data.Array as A
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NEA
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor.Compose (Compose(..))
import Data.Int as Int
import Data.Int.Bits as Int.Bits
import Data.List as List
import Data.Map as Map
import Data.Number as Number
import Data.Set as Set
import Data.String as String
import Data.String as String
import Data.String.CodePoints as CP
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.String.CodeUnits as CU
import Data.String.Regex (regex, source, flags, test, match, replace, replace', search, split) as Re
import Data.String.Regex (regex, source, flags, test, match, replace, replace', search, split) as Re
import Data.String.Regex.Flags (dotAll, global, ignoreCase, multiline, noFlags, sticky, unicode) as Re
import Data.String.Regex.Flags (dotAll, global, ignoreCase, multiline, noFlags, sticky, unicode) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Debug (spy, traceM)
import Dodo as T
import Dodo.Common as T
import Effect.Class.Console as Console
import Idiolect (indices)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Riverdragon.Dragon as Dragon
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings as Wings
import Riverdragon.River as River
import Riverdragon.River.Bed as Bed
import Riverdragon.River.Beyond as Beyond
import Riverdragon.River.Streamline (clientRect)
import Riverdragon.River.Streamline as S
import Riverdragon.Roar.Dimensions (temperaments)
import Riverdragon.Roar.Live as Riverdragon.Roar.Live
import Riverdragon.Roar.Roarlette as YY
import Riverdragon.Roar.Score as Y
import Riverdragon.Roar.Viz as Viz
import Web.Audio.Context as AudioContext
import Web.Audio.Context as Context
import Web.Audio.MIDI as MIDI
import Web.Audio.Param as AudioParam
import Web.Audio.Param as Param
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.TouchEvent.Touch as Touch
import Web.TouchEvent.TouchEvent as TouchEvent
import Web.TouchEvent.TouchList as TouchList
import Web.UIEvent.MouseEvent as MouseEvent
import Widget (Widget)
import Widget as Widget

-- .Bonus
colorBonus = "#e86db7" :: String

widget :: Widget
widget _ = do
  levels <- sequence $ A.replicate 121 $ River.createStore 0
  { send: sendScopeParent, stream: scopeParent } <- createRiverStore Nothing
  lazyDragon <- createRiverStore Nothing
  { playPause } <- installSynth \notes -> do
    { iface } <- ask
    let
      height = 20.0
      normalize = Int.toNumber >>> (_ / -height)
      waveShape :: River (Array Float)
      waveShape = traverse (normalize ==< _.stream) levels
    shaped <- YY.wavetable (48000.0 / 256.0) waveShape
    antialiased <- Y.filter
      { type: Lowpass
      , "Q": 0.0
      , detune: 0.0
      , frequency: 4000.0
      , gain: 1.0
      } shaped

    scopeEl1 <- oscilloscope { width: 256, height: 512 } antialiased
    scopeEl2 <- spectrogram { height: 512, width: 400 } antialiased
    River.subscribe scopeParent \el -> do
      Node.appendChild (HTMLCanvasElement.toNode scopeEl1) (Element.toNode el)
      Node.appendChild (HTMLCanvasElement.toNode scopeEl2) (Element.toNode el)

    liftEffect $ lazyDragon.send $ fold
      [ mempty
      , Wings.pushButtonRadio iface.temperament
        [ temperaments.equal /\ D.text "Equal Temperament"
        , temperaments.kirnbergerIII /\ D.text "Kirnberger III"
        , temperaments.pythagorean /\ D.text "Pythagorean"
        ]
      ]

    toRoars <$> Y.gain 0.3 antialiased
  pure $ fold
    [ D.div [] playPause
    , D.Replacing lazyDragon.stream
    , display levels
    , D.div [ D.Self =:= \el -> mempty <$ sendScopeParent el ] mempty
    ]

interpixels :: Int -> Int -> Array { i :: Int, x :: Number }
interpixels i0 i1
  | i0+1 < i1-1 =
    Array.range (i0+1) (i1-1) <#>
      -- traveling right; sample on right edges
      \i -> { i, x: Int.toNumber (i+1) }
  | i1+1 < i0-1 =
    Array.range (i1+1) (i0-1) <#>
      -- traveling left; sample on left edges
      \i -> { i, x: Int.toNumber i }
  | otherwise = []

touchList :: TouchList.TouchList -> Array Touch.Touch
touchList l = indices (Array.replicate (TouchList.length l) unit)
  <#?> \i -> TouchList.item i l


touchStream :: River Event.Event -> ResourceM (River (River Touch.Touch))
touchStream touchstart = do
  touchEnd <- pure $ memoize $ unsafeRiver $ makeLake \cb ->
    Beyond.documentEvent (EventType "touchend") TouchEvent.fromEvent cb
  touchMove <- pure $ memoize $ unsafeRiver $ makeLake \cb ->
    Beyond.documentEvent (EventType "touchmove") TouchEvent.fromEvent cb
  individualTouches <- River.createRiver
  River.subscribeM touchstart $ TouchEvent.fromEvent >>> traverse_ \event0 -> do
    let started = TouchEvent.changedTouches event0 # touchList
    for_ started \touch -> do
      let
        id = Touch.identifier touch
        identify ev = TouchEvent.changedTouches ev # touchList
          # Array.find \t -> Touch.identifier t == id
      { stream: thisTouch } <-
        River.store' touch $
          River.mapLatest identity $
            pure (identify <$?> touchMove)
            <|> empty <$ River.limitTo 1 (identify <$?> touchEnd)
      liftEffect do individualTouches.send thisTouch
  pure individualTouches.stream

display ::
  Array
    { send :: Int -!> Unit
    , stream :: River Int
    , destroy :: Allocar Unit
    , current :: Effect Int
    } ->
  Dragon
display levels = D.Egg do
  let
    width = A.length levels
    height = 2 * 30
    scale = 10.0
    scaled i = Int.toNumber i * scale
    desired = V2 (mkBounds 0.0 (scaled width)) (mkBounds (-scaled (height/2)) (scaled (height/2)))
  mousedown <- River.createRiver
  touchstart <- River.createRiver
  posing <- River.createRiver
  svgRef <- River.createRiverStore Nothing
  svgCoords <- pure $ svgRef.current >>= traverse \svg -> do
    bb <- clientRect svg
    let
      mapper = (bounds2bounds2 bb desired $* _)
      clamper = clampBounds desired <<< mapper
    pure { mapper, clamper }
  River.subscribeM1 mousedown.stream \event0 -> do
    local <- River.createRiverStore =<< unwrap ado
      { clamper } <- Compose $ liftEffect svgCoords
      event <- Compose $ pure $ MouseEvent.fromEvent event0
      let pos = Int.toNumber <$> V2 (MouseEvent.clientX event) (MouseEvent.clientY event)
      in clamper pos
    liftEffect do posing.send local.stream
    Beyond.documentEvent (EventType "mouseup") Just <<< const =<< selfDestructor
    Beyond.documentEvent (EventType "mousemove") MouseEvent.fromEvent \event -> do
      svgCoords >>= traverse_ \{ clamper } -> do
        let pos = Int.toNumber <$> V2 (MouseEvent.clientX event) (MouseEvent.clientY event)
        local.send (clamper pos)
  touches <- touchStream touchstart.stream
  River.subscribeM touches \individual -> do
    { stream: coords } <- River.store $ compact $ individual # River.mapAl \touch ->
      svgCoords >>= traverse \{ clamper } -> do
        let pos = Int.toNumber <$> V2 (Touch.clientX touch) (Touch.clientY touch)
        pure (clamper pos)
    liftEffect do posing.send coords
  River.subscribeM posing.stream \inner -> do
    let
      movements = Beyond.withLast inner <#>
        \{ last, next } -> { last: fromMaybe next last, next }
    River.subscribe movements \{ last: p@(V2 x0 _), next: q@(V2 x1 y1) } -> do
      let i0 = Int.floor (x0 / scale)
      let i1 = Int.floor (x1 / scale)
      let v1 = Int.round (y1 / scale)
      for_ (interpixels i0 i1) \{ i, x } -> do
        for_ (levels A.!! i) \{ send } -> do
          -- traceM { i0, i1, i, l: B1 p q, t: x2y (B1 p q), v: (x2y (B1 p q) $. x * scale) / scale }
          send $ Int.round $ (x2y (B1 p q) $. x * scale) / scale
      for_ (levels A.!! i1) \{ send } -> send v1
  pure $ D.svg
    [ D.viewBox =:= [ 0.0, -scaled (height / 2), scaled width, scaled (height + 1) ]
    , D.style =:= "touch-action: none; fill: currentColor; border: 1px solid " <> colorBonus
    , D.on_"mousedown" =:= mousedown.send
    , D.on_"touchstart" =:= touchstart.send
    , D.Self =:= \v -> svgRef.send v $> mempty
    ] $ fold
    [ D.svg_"g" [] $
        levels # foldMapWithIndex \i { stream } ->
          D.svg_"rect"
            [ D.attr "x" =:= scaled i
            , D.attr "y" <:> scaled <$> stream
            , D.attr "width" =:= scaled 1
            , D.attr "height" =:= scaled 1
            , D.style =:= "stroke: none;"
            ] mempty
    , D.svg_"line"
        [ D.style =:= "fill: none; stroke: " <> colorBonus
        , D.attr "x1" =:= scaled 0
        , D.attr "x2" =:= scaled width
        , D.attr "y1" =:= scale / 2.0
        , D.attr "y2" =:= scale / 2.0
        ] mempty
    , D.svg_"line"
        [ D.style =:= "fill: none; stroke: " <> colorBonus
        , D.attr "x1" =:= scaled width / 2.0
        , D.attr "x2" =:= scaled width / 2.0
        , D.attr "y1" =:= scaled (-height/2)
        , D.attr "y2" =:= scaled (height/2 + 1)
        ] mempty
    ]

