module Riverdragon.Dragon.Components.Envelope where

import Prelude

import Control.Monad.ResourceM (inSubScope, selfDestructor)
import Control.Monad.ResourceT (ResourceM)
import Data.Foldable (fold, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Math.Matrix (V2, Vec2(..), bounds2bounds2, clampBounds, mkBounds, ($*))
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones (smarts, ($~~), (<:>), (=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (River, createRiverStore, (/?*\))
import Riverdragon.River as River
import Riverdragon.River.Beyond (documentEvent)
import Riverdragon.River.Streamline (clientRect)
import Riverdragon.Roar.Knob (Envelope)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent as MouseEvent

envelopeComponent ::
  Envelope ->
  ResourceM
    { ui :: Dragon
    , stream :: River Envelope
    }
envelopeComponent init = do
  { send, stream } <- createRiverStore $ Just init
  svgRef <- liftEffect do Ref.new Nothing

  let
    width = 200.0
    height = 100.0
    padding = 40.0
    pudding = 2.0 * padding
    external = V2 (mkBounds 0.0 (width + pudding)) (mkBounds 0.0 (height + pudding))
    graphExternal = V2 (mkBounds padding (width + padding)) (mkBounds (height + padding) padding)

  { send: mouseDown, stream: dragging } <- createRiverStore
    (Nothing :: Maybe (V2 -> Effect Unit))
  River.subscribeM (stream /?*\ dragging) \(Tuple _current selected) -> inSubScope "envelopeComponent" do
    documentEvent (EventType "mousemove") MouseEvent.fromEvent \event -> do
      Ref.read svgRef >>= traverse_ \svg -> do
        bb <- clientRect svg
        let
          ptExternal = bounds2bounds2 bb external $* clampBounds bb do
            Int.toNumber <$> V2 (MouseEvent.clientX event) (MouseEvent.clientY event)
        selected ptExternal
    destroy <- selfDestructor
    documentEvent (EventType "mouseup") Just \_ -> do
      -- TODO: confirm or cancel? stuff like that
      destroy
    -- TODO: listen for escape key to cancel?
    pure unit

  let
    ui = D.svg
      [ D.stylish =:= smarts
        { "width": show (width + pudding) <> "px"
        , "height": show (height + pudding) <> "px"
        , "stroke": "currentColor"
        , "stroke-width": "2px"
        , "border": "1px solid currentColor"
        , "fill": "none"
        }
      , D.Self =:= \el -> Ref.write Nothing svgRef <$ Ref.write (Just el) svgRef
      ] $~~
      [ D.pathW' $ stream <#> \current ->
          let
            adr = current.attack + current.decay + current.release
            sustain = adr
            totalTime = adr + sustain
            graphInternal = V2 (mkBounds 0.0 totalTime) (mkBounds 0.0 1.0)
            display = bounds2bounds2 graphInternal graphExternal
            pt { time, volume } =
              let
                V2 x y = display $* V2 time volume
              in show x <> " " <> show y
          in fold
            -- M is for Move: set the cursor
            [ "M" <> pt { time: 0.0, volume: 0.0 }
            -- L is for Line: line to the next point
            , "L" <> pt { time: current.attack, volume: 1.0 }
            , "L" <> pt { time: current.attack + current.decay, volume: current.sustain }
            , "L" <> pt { time: totalTime - current.release, volume: current.sustain }
            , "L" <> pt { time: totalTime, volume: 0.0 }
            ]
      , D.g
        [ D.stylish =:= smarts
            { "font-family": "inherit"
            , "font-size": "30px"
            , "fill": "currentColor"
            , "stroke": "none"
            }
        ] $~~
        [ D.svg_"text"
            [ D.attr "x" =:= 100.0
            , D.attr "y" =:= 170.0
            ] $ D.text "Time ->"
        , D.svg_"text"
            [ D.attr "transform" =:= "rotate(-90)"
            , D.attr "transform-origin" =:= "center"
            , D.attr "x" =:= 80.0
            , D.attr "y" =:= -20.0
            ] $ D.text "Volume ->"
        ]
      , D.g [] $~~
        [ D.circleW'
          ( stream <#> \current -> do
              let
                adr = current.attack + current.decay + current.release
                sustain = adr
                totalTime = adr + sustain
                graphInternal = V2 (mkBounds 0.0 totalTime) (mkBounds 0.0 1.0)
                display = bounds2bounds2 graphInternal graphExternal
                pt { time, volume } =
                  let
                    V2 x y = display $* V2 time volume
                  in { cx: x, cy: y, r: 5.0 }
              pt { time: current.attack + current.decay, volume: current.sustain }
          ) []
        , D.circleW'
          ( stream <#> \current -> do
              let
                adr = current.attack + current.decay + current.release
                sustain = adr
                totalTime = adr + sustain
                graphInternal = V2 (mkBounds 0.0 totalTime) (mkBounds 0.0 1.0)
                display = bounds2bounds2 graphInternal graphExternal
                pt { time, volume } =
                  let
                    V2 x y = display $* V2 time volume
                  in { cx: x, cy: y, r: 10.0 }
              pt { time: current.attack + current.decay, volume: current.sustain }
          )
          [ D.stylish =:= smarts
            { "fill": "transparent"
            , "stroke": "transparent"
            }
          , D.on_"mousedown" <:> stream <#> \current _mousedown -> do
              -- TODO: graph initial coordinates? maybe?
              mouseDown \ptExternal -> do
                let
                  adr = current.attack + current.decay + current.release
                  totalTime = adr + adr
                  graphInternal = V2 (mkBounds 0.0 totalTime) (mkBounds 0.0 1.0)
                  -- decay = linmap graphExternal.x graphInternal.x ptExternal.x
                  V2 _ sustain = bounds2bounds2 graphExternal graphInternal $*
                    clampBounds graphExternal ptExternal
                  new = current { sustain = sustain }
                Console.logShow new
                send new
          ]
        ]
      ]
  pure { ui, stream }
