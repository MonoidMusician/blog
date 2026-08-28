module Train.Main where

import Prelude

import Control.Monad.ResourceM (inSubScope, selfDestructor)
import Control.Monad.ResourceT (ResourceM)
import Control.Monad.State (get)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.DateTime.Instant (unInstant)
import Data.Either (either, fromRight)
import Data.Filterable (compact, filter)
import Data.Foldable (fold, foldMap, intercalate, traverse_)
import Data.Functor.App (App(..))
import Data.Int as Int
import Data.Lazy (force)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Number as Math
import Data.Optical ((@~))
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..), unpairy)
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Idiolect (incorporate, neighbors, sgn, sqre, withIndices, (#..), (#:..), (#<>), (<>$), (>==))
import Math.Bezier as Bezier
import Math.Matrix (Bez1(..), Bez3(..), Bounds, V2, Vec2(..), bounds2bez, bounds2bounds2, clampBounds, d2r, extent, mkBound, mkBounds, normalize, overBounds, padBounds, pairs, r2d, rotl2, unit2bounds1, ($*), ($.), (-<>), (.*), (<>+), (<>-))
import Math.Poly (deriv)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones (($<), (.$), (.$$), (.$~~), (:%), (:.), (<:>), (=:=), (>$), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (liveArray, sourceCode, tabSwitcher)
import Riverdragon.River (River, copyBurst, createRiver, createRiverStore, dam, statefulStream, stillRiver, store, (>>~))
import Riverdragon.River as River
import Riverdragon.River.Beyond (dedup, dedupOn, documentEvent, everyFrame)
import Riverdragon.River.Streamline (clientRect)
import Train.Drawing (bounds2viewBox, clone, dragonEither, manageDefs, mask)
import Train.Drawing as Drawing
import Train.Dynamics as Dynamics
import Train.Geometry (mkRoute, routesToPaths, trainOnRoute)
import Train.Geometry as Geo
import Train.Impl (renderCommand)
import Train.Library (standardCurves)
import Train.Logic (analyzeLayout)
import Train.Parser (parseTraintle)
import Train.Track (planAndScheduleRoute)
import Train.Types (Command, InterState, Route(..), Standard(..), TrainMode(..))
import Train.UI (definitions, cfgTraction, railCalc, range)
import Train.UI as UI
import Type.Proxy (Proxy(..))
import Uncurried.RWSE (runRWSE)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent as MouseEvent
import Widget (Widget, autoAdaptInterface, valueInterface)

widget :: Widget
widget { interface } = do
  { stream: valueSet, send: setValue } <- createRiver
  let receiveValue = (autoAdaptInterface @String (interface "traintle-value")).receive
  tractionW <- cfgTraction { wheels: 25.0, motors: 500.0 }
  limit0 <- River.createStore 250.0
  limit7 <- River.createStore 100.0
  limit13 <- River.createStore 180.0
  let
    inputs =
      { traction: tractionW.outputs.traction.loopback
      , limits: ado
          l0 <- limit0.stream
          l7 <- limit7.stream
          l13 <- limit13.stream
          in Map.fromFoldable
            [ Tuple 0 l0
            , Tuple 13 l13
            , Tuple 7 l7
            ]
      }
  { stream: parsed } <- River.store $ parseTraintle <<< snd <$> valueSet
  let cmds = fromRight [] <$> parsed
  traintle <- renderTraintle inputs cmds
  pure $ fold
    [ sourceCode "traintle"
        [ D.style =:= "flex: 0 0 50%; padding-right: 10px; box-sizing: border-box; font-size: 26px"
        -- , D.className <:> done <#> snd >>> isLeft >>> if _ then "sourceCode hatstack invalid" else "sourceCode hatstack"
        ] $ D.textarea
          [ D.onInputValue =:= setValue <<< Tuple false
          , D.onChangeValue =:= setValue <<< Tuple true
          , D.value <:> receiveValue
          , D.style =:= "height: 20svh"
          , D.asCodeInput
          ]
    , dragonEither (D.Text <<< dam) mempty parsed
    , traintle.widget
    , Egg do
        calculator <- railCalc tractionW.outputs.traction.loopback

        pure $ fold
          [ D.div.$ tractionW.widget
          , D.div.$~~
            [ UI.miniHeading.$$ "Speed limits"
            , D.label.$ "Straight: "   $< UI.number 0.0 500.0 5.0 (copyBurst limit0.stream empty) limit0.send []
            , D.nbsp, D.nbsp, D.nbsp, D.nbsp
            , D.label.$ "Radius 7: "  $< UI.number 0.0 500.0 5.0 (copyBurst limit7.stream empty) limit7.send []
            , D.nbsp, D.nbsp, D.nbsp, D.nbsp
            , D.label.$ "Radius 13: " $< UI.number 0.0 500.0 5.0 (copyBurst limit13.stream empty) limit13.send []
            , D.text " dm/s"
            ]
          , calculator.widget
          ]
    , D.html_"hr" [] mempty
    , Replacing $ traintle.outputs.schedule.stream <#> foldMap \schedule ->
        NEA.fromArray (UI.graph 500 (overBounds (_ * 20.0) schedule.time) ((_ / 20.0) >>> schedule.status >>> _.veloc)) # foldMap \graph ->
          (_ >$ "Speed vs Time") $
          D.svg
            [ D.stylish =:= D.smarts
              { "width": "100%"
              , "height": "5em"
              }
            , D.viewBox =:= bounds2viewBox (V2 (overBounds (_ * 20.0) schedule.time) $ mkBounds zero schedule.maxSpeed)
            , D.attr "preserveAspectRatio" =:= "none"
            , D.title =:= "Speed vs Time"
            ] $ D.path
              [ D.attr "d" =:= Geo.bezsToPath graph
              , D.stylish =:= D.smarts
                { "strokeWidth": "2px"
                , "stroke": "currentColor"
                , "fill": "none"
                , "transform":
                    "scaleY(-1) translateY(-" <> show schedule.maxSpeed <> "px)"
                }
              ]
    , Replacing $ traintle.outputs.schedule.stream <#> foldMap \schedule ->
        NEA.fromArray (UI.graph 500 schedule.extent (schedule.byDist >>> _.veloc)) # foldMap \graph ->
          (_ >$ "Speed vs Distance") $
          D.svg
            [ D.stylish =:= D.smarts
              { "width": "100%"
              , "height": "5em"
              }
            , D.viewBox =:= bounds2viewBox (V2 schedule.extent $ mkBounds zero schedule.maxSpeed)
            , D.attr "preserveAspectRatio" =:= "none"
            , D.title =:= "Speed vs Distance"
            ] $ D.path
              [ D.attr "d" =:= Geo.bezsToPath graph
              , D.stylish =:= D.smarts
                { "strokeWidth": "2px"
                , "stroke": "currentColor"
                , "fill": "none"
                , "transform":
                    "scaleY(-1) translateY(-" <> show schedule.maxSpeed <> "px)"
                }
              ]
    , Replacing $ traintle.outputs.schedule.stream <#> foldMap \schedule ->
        range (unwrap (schedule.extent.min)) (unwrap (schedule.extent.max)) 0.0
          (schedule.animation <$> traintle.outputs.looping.stream) mempty
          [ D.prop "disabled" =:= true ]
    , Replacing $ traintle.outputs.schedule.stream <#> foldMap \schedule ->
        let
          inWindow { time: { min: Min p, max: Max q } }
            | { min: Min r, max: Max s } <- schedule.time =
              q > r && p < s
          withSign r = case r.plan of
            Dynamics.Limit _ -> const ""
            Dynamics.Accel _ -> ("+" <> _)
            Dynamics.Decel _ -> ("-" <> _)
        in case schedule.schedule, schedule.time of
          Dynamics.SpeedSchedule _ _ _byTime _byDist _, { min: Min startTime } -> do
            D.div:."h-scroll".$~~
              [ UI.miniHeading.$$ "Timetable"
              , UI.table (map D.text <$> [ 3 /\ "distance", 3 /\ "time", 3 /\ "speed" ]) $
                Array.filter inWindow (Array.fromFoldable _byDist) <#> \r ->
                  map (\c -> D.td:."code":%"text-align: right".$ c) $ join
                    [ Array.fromFoldable $ map (D.text <<< UI.fmt) (bounds2bez r.dist)
                    , [ (D.span:%"opacity: 50%".$$"Δ") <>$ D.text $ UI.fmt $ extent r.dist ]
                    , Array.fromFoldable $ map (D.text <<< UI.fmt) (bounds2bez r.time <#> (_ - startTime))
                    , [ (D.span:%"opacity: 50%".$$"Δ") <>$ D.text $ UI.fmt $ extent r.time ]
                    , Array.fromFoldable $ map (D.text <<< UI.fmt) (unpairy B1 r.veloc)
                    , [ D.text $ withSign r $ UI.fmt $ Math.abs $ unpairy (-) r.veloc ]
                    ]
              ]
    , dedup traintle.outputs.library >@ \library ->
        tabSwitcher Nothing $ Map.toUnfoldable library <#> \(Tuple id (Standard standard)) ->
          Tuple (show id) $ D.show { key: standard.key, radius: standard.radius }
    , Replacing $ stillRiver traintle.outputs.info
    , mempty $ Egg do
        curves <- liftEffect do
          traverse (traverse valueInterface) $ Pair
            (B3 (V2 0.0 0.0) (V2 0.0 8.0) (V2 14.0 24.0) (V2 14.0 32.0))
            (B3 (V2 18.0 0.0) (V2 18.0 8.0) (V2 32.0 24.0) (V2 32.0 32.0))
        let showPoint (V2 x y) = show x <> "," <> show (y :: Number)
        let showPath (B3 p0 p1 p2 p3) = "M" <> showPoint p0 <> "C" <> showPoint p1 <> " " <> showPoint p2 <> " " <> showPoint p3
        { send: mouseDown, stream: dragging } <- createRiverStore
          (Nothing :: Maybe (V2 -> Effect Unit))
        svgRef <- liftEffect do Ref.new Nothing

        River.subscribeM dragging \selected -> inSubScope "envelopeComponent" do
          documentEvent (EventType "mousemove") MouseEvent.fromEvent \event -> do
            Ref.read svgRef >>= traverse_ \svg -> do
              bb <- clientRect svg
              let
                ptExternal = clampBounds (V2 (mkBounds 0.0 32.0) (mkBounds 0.0 32.0)) $
                  bounds2bounds2 bb (V2 (mkBounds (-2.0) 34.0) (mkBounds (-2.0) 34.0)) $*
                    Int.toNumber <$> V2 (MouseEvent.clientX event) (MouseEvent.clientY event)
              selected ptExternal
          destroy <- selfDestructor
          documentEvent (EventType "mouseup") Just \_ -> do
            -- TODO: confirm or cancel? stuff like that
            destroy
          -- TODO: listen for escape key to cancel?
          pure unit

        { stream: intersections } <- store $ (\(Pair c1 c2) -> Bezier.intersect c1 c2) <$> do
          traverse (traverse _.receive) curves

        pure $ fold
          [ D.svg
            [ D.attr "viewBox" =:= "-2 -2 36 36"
            , D.attr "height" =:= 256.0
            , D.attr "fill" =:= "none"
            , D.Self =:= \el -> Ref.write Nothing svgRef <$ Ref.write (Just el) svgRef
            ] $ fold
            [ curves #.. \curve ->
                D.path
                  [ D.d <:> showPath <$> traverse _.receive curve
                  , D.stylish =:= D.smarts
                    { "fill": "none"
                    , "stroke": "blue"
                    , "stroke-width": 0.5
                    }
                  ]
            , D.Replacing $ intersections <#> foldMap \(Pair _ { p: V2 x y }) ->
                D.svg_"circle"
                  [ D.attr "r" =:= 1.0
                  , D.attr "cx" =:= x
                  , D.attr "cy" =:= y + 0.0
                  , D.stylish =:= D.smarts
                    { "fill": "#F0F3"
                    , "stroke": "magenta"
                    , "stroke-width": 0.25
                    }
                  ] mempty
            , curves #:.. \i -> foldMap \point ->
                D.svg_"circle"
                  [ D.attr "r" =:= 1.0
                  , point.receive <#> \(V2 x y) ->
                      D.MultiAttr [ D.attr "cx" x, D.attr "cy" y ]
                  , D.stylish =:= D.smarts
                    { "fill": if i then "#E34D" else "#43ED"
                    , "stroke": if i then "pink" else "skyblue"
                    , "stroke-width": 0.25
                    }
                  , D.on_"mousedown" =:= \_ -> do
                      mouseDown point.send
                  ] mempty
            ]
          , D.Text $ dam $ show <<< Array.length <$> intersections
          ]
    , mempty $ Egg do
        curve <- liftEffect do
          traverse valueInterface
            (B3 (V2 0.0 0.0) (V2 0.0 8.0) (V2 14.0 24.0) (V2 14.0 32.0))
        let showPoint (V2 x y) = show x <> "," <> show (y :: Number)
        let showPath (B3 p0 p1 p2 p3) = "M" <> showPoint p0 <> "C" <> showPoint p1 <> " " <> showPoint p2 <> " " <> showPoint p3
        { send: mouseDown, stream: dragging } <- createRiverStore
          (Nothing :: Maybe (V2 -> Effect Unit))
        svgRef <- liftEffect do Ref.new Nothing

        River.subscribeM dragging \selected -> inSubScope "envelopeComponent" do
          documentEvent (EventType "mousemove") MouseEvent.fromEvent \event -> do
            Ref.read svgRef >>= traverse_ \svg -> do
              bb <- clientRect svg
              let
                ptExternal = clampBounds (V2 (mkBounds 0.0 32.0) (mkBounds 0.0 32.0)) $
                  bounds2bounds2 bb (V2 (mkBounds (-2.0) 34.0) (mkBounds (-2.0) 34.0)) $*
                    Int.toNumber <$> V2 (MouseEvent.clientX event) (MouseEvent.clientY event)
              selected ptExternal
          destroy <- selfDestructor
          documentEvent (EventType "mouseup") Just \_ -> do
            -- TODO: confirm or cancel? stuff like that
            destroy
          -- TODO: listen for escape key to cancel?
          pure unit

        let
          fit f c@(B3 p0 p1 p2 p3) =
            Bezier.fit
              { p0, p1: p3, d0: p0 -<> p1, d1: p2 -<> p3
              , k0: 1.0 / f (1.0 / Bezier.curvatureAt c 0.0), k1: 1.0 / f (1.0 / Bezier.curvatureAt c 1.0)
              }
          delta = 5.0
          inner c@(B3 p0 p1 p2 p3) =
            let
              d0 = normalize $ p0 -<> p1
              d1 = normalize $ p2 -<> p3
              ninety = rotl2 (-90.0 * d2r)
              q0 = p0 <> delta .* (ninety $* d0)
              q3 = p3 <> delta .* (ninety $* d1)
              k0 = Bezier.curvatureAt c 0.0
              k1 = Bezier.curvatureAt c 1.0
            in Bezier.fit
              { p0: q0, p1: q3, d0, d1
              , k0: 1.0 / ((1.0 / k0) - sgn k0 * delta)
              , k1: 1.0 / ((1.0 / k1) - sgn k1 * delta)
              }
          outer c@(B3 p0 p1 p2 p3) =
            let
              d0 = normalize $ p0 -<> p1
              d1 = normalize $ p2 -<> p3
              ninety = rotl2 (90.0 * d2r)
              q0 = p0 <> delta .* (ninety $* d0)
              q3 = p3 <> delta .* (ninety $* d1)
              k0 = Bezier.curvatureAt c 0.0
              k1 = Bezier.curvatureAt c 1.0
            in Bezier.fit
              { p0: q0, p1: q3, d0, d1
              , k0: 1.0 / ((1.0 / k0) + sgn k0 * delta)
              , k1: 1.0 / ((1.0 / k1) + sgn k1 * delta)
              }

        { stream: possibilities } <- store $ (fit identity <> inner <> outer) <$> do
          traverse _.receive curve

        pure $ fold
          [ D.svg
            [ D.attr "viewBox" =:= "-2 -2 36 36"
            , D.attr "height" =:= 256.0
            , D.attr "fill" =:= "none"
            , D.Self =:= \el -> Ref.write Nothing svgRef <$ Ref.write (Just el) svgRef
            ] $ fold
            [ D.path
                [ D.d <:> showPath <$> traverse _.receive curve
                , D.stylish =:= D.smarts
                  { "fill": "none"
                  , "stroke": "#0000ff25"
                  , "stroke-width": 0.5
                  }
                ]
            , D.Replacing $ possibilities <#> foldMap \couldBe ->
                D.path
                  [ D.d =:= showPath couldBe
                  , D.stylish =:= D.smarts
                    { "fill": "none"
                    , "stroke": "#ff0000ab"
                    , "stroke-width": 0.5
                    }
                  ]
            , ((_ / 12.0) <<< Int.toNumber <$> Array.range 0 12) #.. \t ->
                traverse _.receive curve >@ \coords ->
                  let
                    pt = Bezier.evalB coords t
                    normal = rotl2 (90.0 * d2r) $* delta .* normalize (Bezier.evalB (deriv coords) t)
                  in [ pt <>+ normal, pt <>- normal ] #.. \(V2 x y) ->
                    D.svg_"circle"
                      [ D.attr "r" =:= 1.0
                      , D.attr "cx" =:= x
                      , D.attr "cy" =:= y + 0.0
                      , D.stylish =:= D.smarts
                        { "fill": "#F0F3"
                        , "stroke": "magenta"
                        , "stroke-width": 0.25
                        }
                      ] mempty
            , curve #.. \point ->
                D.svg_"circle"
                  [ D.attr "r" =:= 1.0
                  , point.receive <#> \(V2 x y) ->
                      D.MultiAttr [ D.attr "cx" x, D.attr "cy" y ]
                  , D.stylish =:= D.smarts
                    { "fill": "#E34D"
                    , "stroke": "pink"
                    , "stroke-width": 0.25
                    }
                  , D.on_"mousedown" =:= \_ -> do
                      mouseDown point.send
                  ] mempty
            ]
          , D.Text $ dam $ show <<< Array.length <$> possibilities
          ]
    ]

num :: Number -> String
num x = "" <> unsafeCoerce x

int :: Int -> String
int x = "" <> unsafeCoerce x

spaced :: Array (River String) -> River String
spaced = traverse River.alwaysBurstM >== joinWith " "

-- wdassssssdaaxdwwwwwwwwwassaxddsssdxasssssdwaaadxwdaxaddaaxwwdxaa
-- 16dwwwaa16saawww8dxwwwdd16sdd6wdd16sddwww8axwwwaa16saawww
-- @S{8w} &R{@Se3de@Seewwwqq@Sq3aq@S12w@Sq3aq@Sqqwwwee@Se3de@S12w@S}
{-
  @S{14w}
  =O
  O 12w @S 13w
  O ewq @S wwwwwwwwq
  O wwwqwe @S  wwwwwwwwe
  O ewwwq @S qwwwewww
  O &R{wwwqwwwe @S ewwwq}
-}
{-
  r2 # set radius for q/e and a/d

  # Extend each siding
  @EXT{6w}

  @FORK{
    # Start at the (local) origin
    = qwwe 3w    @EXT
    = 6w awd 3w  @EXT
    = 3w ewwq    @EXT
    = 9w dwa     @EXT
    = 16w        @EXT
    # Each gets the extension
  }

  # Draw a big loop
  @LOOPL{
    r3 # set radius
    qq 5w ee8de 5wqww
  }
  @LOOPR{
    r3 # set radius
    ee 5w qq8aq 5weww
  }

  # Fork and rejoin
  @FORK @EXT 16w x
  @FORK @EXT 16w
  # Loop around
  @LOOPR
  16w @EXT @EXT 16w
  2w @LOOPL 2w

  # Finish the implicit route
  qwwe 3w @EXT @EXT ewwq 3w @LOOPR
-}
-- 3(ee6wqqaaaaqq6w10e 4w8e4w)
-- 4q4w4qaaqawe3deewwqq4a11w
-- 2(eewwwqq4awwwewqqq5weee4weedddewwqq13w)
{-
  @S{14w}
  @L{r1 qaq}
  r2
  4(wwqwe @S ewq ww @L ww @L ww @L)
  4(9w 8w @S     9w @L ww @L ww @L)
  4(wwewq @S qwe ww @L ww @L ww @L)
-}
{-
  @S{14w}
  4(wwqwe @S ewq ww qaq ww qaq ww qaq)
  4(9w    @S     9w qaq ww qaq ww qaq)
  4(wwewq @S qwe ww qaq ww qaq ww qaq)
    9w    @S     9w qaq ww qaq
  4(wwqwe @S ewq ww qaq)
  4(9w    @S     9w qaq)
  4(wwewq @S qwe ww qaq)
    ww qaq ww qaq
  4(wwqwe @S ewq ww qaq ww qaq ww qaq)
  4(9w    @S     9w qaq ww qaq ww qaq)
  4(wwewq @S qwe ww qaq ww qaq ww qaq)
    9w    @S     9w qaq ww qaq
  4(wwqwe @S ewq ww qaq)
  4(9w    @S     9w qaq)
  4(wwewq @S qwe ww qaq)
-}
-- eeeeeeeeeeeeeeeeqqxqqqdqqqxqqqqxqqqdqqqxqq qqxqqqdddddddd
renderTraintle :: { | _ } -> River (Array Command) -> ResourceM { widget :: Dragon, outputs :: _ }
renderTraintle inputs cmds = do
  { defs, defL, defineL } <- manageDefs
  { stream: running } <- River.store do
    statefulStream { library: force standardCurves, hitmap: Map.empty } (dedup cmds)
      \s c -> let r = runTraintle s c in { emit: Just r, state: r.state }
  curve <- defineL \id -> D.path [ D.id =:= id, D.attr "d" <:> _.curve <$> running ]

  let routeCmp (Route { pathlength, curves }) = Tuple pathlength curves
  rawRoute <- pure $ dedupOn (map routeCmp) $ running <#> \{ routes } ->
    case _.value <$> Map.findMin routes of
      Nothing -> Nothing
      Just route -> Just route
  { stream: freshRoute } <- River.store $ rawRoute <#> map \route ->
    let consist = [ 16.0 ] <>$ Array.range 0 2 #.. const [ 64.0, 32.0 ] #<> [ 64.0, 16.0 ]
    in trainOnRoute route consist
  scheduleOutput@{ stream: freshSchedule } <- River.store ado
    mroute <- freshRoute
    traction <- inputs.traction
    limits <- inputs.limits
    speeds <- pure []
    in mroute <#> \route ->
      planAndScheduleRoute { route, traction, speeds, limits }
  looping <- River.store $ compact freshSchedule >>~ \schedule -> do
    everyFrame # River.mapAl \_ -> now <#> unInstant >>> \(Milliseconds t) ->
      let
        duration = extent schedule.time * 1_000.0
        loopPos = (t Math.% (2.0 * duration)) / duration
        loopAndBack = if loopPos <= 1.0 then loopPos else 2.0 - loopPos
      in unit2bounds1 schedule.time $. loopAndBack
  { stream: freshTrains } <- River.store $ freshSchedule >>~ maybe mempty
    \{ train } -> NEA.toArray <<< train <$> looping.stream
  let
    railmask = newmask curve
    newmask curve bbox inner outer =
      maskOf (map (padBounds (Int.toNumber (outer + 12))) <$> bbox) $ fold
        [ clone curve
          [ D.stylish =:= D.smarts
            { "stroke": "white"
            , "stroke-width": int outer <> "px"
            }
          ]
        , clone curve
          [ D.stylish =:= D.smarts
            { "stroke": "black"
            , "stroke-width": int inner <> "px"
            }
          ]
        ]
    maskOf bbox contents = mask $
      defL \id -> D.svg_"mask"
        [ D.id =:= id
        , bbox <#> map bounds2bez >>> \(V2 (B1 x0 x1) (B1 y0 y1)) ->
            D.MultiAttr
            [ D.attr "maskUnits" "userSpaceOnUse"
            , D.attr "x" x0
            , D.attr "y" y0
            , D.attr "width" $ x1 - x0
            , D.attr "height" $ y1 - y0
            ]
        ] contents
    withTrainUnits = liveArray $ freshTrains <#> \trains -> do
      -- The first segment is the buffer,
      -- then we take pairs for the two bogies of a car,
      -- then we tack on neighbors
      Array.drop 1 trains # pairs # withIndices
        # filter (Int.even <<< fst)
        # map snd # neighbors
    outputs =
      { schedule: scheduleOutput, info: _.info <$> running, looping, library: running <#> _.state.library }
    defaultStyle :: Array Drawing.RailStyle
    defaultStyle =
      [
      ]
  pure $ { outputs, widget: _ } $ fold
    [ D.svg
      [ D.attr "viewBox" <:> _.viewBox <$> running
      , D.attr "preserveAspectRatio" =:= "xMidYMid meet"
      , D.classy =:= D.smarts
        { "full-width": true
        }
      , D.stylish =:= D.smarts
        { "max-height": "80vh"
        , "background": "light-dark(white,black)"
        , "color-scheme": "light dark"
        , "fill": "none"
        }
      ] $ fold
      [ D.svg_"defs" [] defs
      , clone curve
          [ D.stylish =:= D.smarts
            { "stroke": "#918b85"
            , "stroke-width": "28px"
            }
          ]
      , clone curve
          [ D.stylish =:= D.smarts
            { "stroke": "#361f13"
            , "stroke-dasharray": "2.76,5.28"
            , "stroke-dashoffset": "5.28"
            , "stroke-width": "24px"
            }
          ]
      , running >@ \{ paths } -> paths #.. \path -> Egg do
          thisOne <- defineL \id -> D.path [ D.id =:= id, D.attr "d" =:= path.d ]
          pure $ clone thisOne
            [ D.stylish =:= D.smarts
              { "stroke": "#5a2814"
              , "stroke-width": "16px"
              }
            , newmask thisOne (pure path.bbox) 12 16
            ]
      , D.g [ maskOf (running <#> _.bounds) $ D.g.$~~
          [ clone curve
              [ D.stylish =:= D.smarts
                { "stroke": "white"
                , "stroke-width": "16px"
                }
              ]
          , running >@ \{ paths } -> paths #.. \path -> Egg do
            thisOne <- defineL \id -> D.path [ D.id =:= id, D.attr "d" =:= path.d ]
            pure $ clone thisOne
              [ D.stylish =:= D.smarts
                { "stroke": "black"
                , "stroke-width": "13px"
                }
              , newmask thisOne (pure path.bbox) 11 13
              ]
          ]
        ] $
          running >@ \{ paths } -> paths #.. \path -> Egg do
            thisOne <- defineL \id -> D.path [ D.id =:= id, D.attr "d" =:= path.d ]
            pure $ clone thisOne
              [ D.stylish =:= D.smarts
                { "stroke": "#cbd4d8"
                , "stroke-width": "15px"
                }
              , newmask thisOne (pure path.bbox) 13 15
              ]
      , clone curve
          [ D.stylish =:= D.smarts
            { "stroke": "#cbd4d8"
            , "stroke-width": "15px"
            }
          , railmask (running <#> _.bounds) 13 15
          ]
      , Drawing.posIndicator (running <#> _.pos) (pure "red")
      , D.g [ D.stylish =:= D.smarts { "opacity": 1.0 } ] $
          withTrainUnits \_idx trainUnit ->
            let
              cslope = sqre 9.0 / 2.0
              jog curvature = "translate(" <> num 0.0 <> ", " <> num (curvature * cslope) <> ")"
              awayfrom = trainUnit <#> \{ prev, here: Pair back train } -> case prev of
                Nothing -> back.at -<> train.at
                Just (Pair _ y) -> y.at -<> back.at
              towards = trainUnit <#> \{ here: Pair back train, next } -> case next of
                Nothing -> back.at -<> train.at
                Just (Pair x _) -> train.at -<> x.at
            in D.g.$~~
              [ mempty
              , clone (pure "#g115") -- coupler
                  [ dam $ D.attr "transform" <:> spaced
                    [ trainUnit <#> \{ here: Pair back train, next } -> case back.at of
                      V2 x y -> "translate(" <> num x <> ", " <> num y <> ")"
                    , awayfrom <#> case _ of
                      V2 dx dy -> "rotate(" <> num (Math.atan2 dy dx * r2d) <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> jog back.curvature
                    , pure "translate(64, 0) rotate(-90,-384,208)"
                    ]
                  ]
              , clone (pure "#use115") -- bogie
                  [ dam $ D.attr "transform" <:> spaced
                    [ trainUnit <#> \{ here: Pair back train, next } -> case back.at of
                      V2 x y -> "translate(" <> num x <> ", " <> num y <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> case back.to of
                      V2 dx dy -> "rotate(" <> num (Math.atan2 dy dx * r2d) <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> jog back.curvature
                    , pure "translate(64, 0) rotate(-90,-384,208)"
                    ]
                  ]
              , clone (pure "#g115") -- coupler
                  [ dam $ D.attr "transform" <:> spaced
                    [ trainUnit <#> \{ here: Pair back train, next } -> case train.at of
                      V2 x y -> "translate(" <> num x <> ", " <> num y <> ")"
                    , towards <#> case _ of
                      V2 dx dy -> "rotate(" <> num (Math.atan2 dy dx * r2d) <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> jog $ -train.curvature
                    , pure "rotate(180) translate(64, 0) rotate(-90,-384,208)"
                    ]
                  ]
              , clone (pure "#use115") -- bogie
                  [ dam $ D.attr "transform" <:> spaced
                    [ trainUnit <#> \{ here: Pair back train, next } -> case train.at of
                      V2 x y -> "translate(" <> num x <> ", " <> num y <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> case train.to of
                      V2 dx dy -> "rotate(" <> num (Math.atan2 dy dx * r2d) <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> jog $ -train.curvature
                    , pure "rotate(180) translate(64, 0) rotate(-90,-384,208)"
                    ]
                  ]
              , clone (pure "#g189425-2")
                  [ dam $ D.attr "transform" <:> spaced
                    [ trainUnit <#> \{ here: Pair back train, next } -> case train.at of
                      V2 x y -> "translate(" <> num x <> ", " <> num y <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> case back.at -<> train.at of
                      V2 dx dy -> "rotate(" <> num (Math.atan2 dy dx * r2d) <> ")"
                    , pure "rotate(180, -32, 0)"
                    ]
                  -- , D.attr "opacity" =:= 0.4
                  ]
              ]
      -- , D.Replacing $ freshTrains <#> \trains ->
      --     D.g [ D.stylish =:= D.smarts { "opacity": 0.0 } ] $ fold $ trains #.. \train ->
      --       [ D.svg_"circle"
      --           [ D.attr "r" =:= "4px"
      --           , pure train <#> \{ at: V2 x y } -> D.MultiAttr
      --               [ D.attr "cx" x
      --               , D.attr "cy" y
      --               ]
      --           , D.stylish =:= D.smarts
      --             { "fill": "yellow"
      --             }
      --           ] mempty
      --       , D.svg_"path"
      --           [ D.attr "d" <:> pure train <#> \{ at: V2 x y, to: V2 dx dy } ->
      --               "M" <> show x <> "," <> show y <> "l" <> show dx <> "," <> show dy
      --           , D.stylish =:= D.smarts
      --             { "stroke": "yellow"
      --             , "stroke-width": "2px"
      --             }
      --           ] mempty
      --       ]
      -- , D.Replacing $ running <#> \{ segments } ->
      --     D.g [ D.stylish =:= D.smarts { "opacity": 0.7 } ] $ segments #.. \p ->
      --       (tf (LTF p.transform) <$> p.canon.strokes) #.. \q ->
      --         D.svg_"path"
      --           [ D.attr "d" =:= bezsToPath (pure q)
      --           , D.stylish =:= D.smarts
      --             { "stroke": "purple"
      --             , "stroke-width": "2px"
      --             }
      --           ] mempty
      -- , D.svg_"path"
      --     [ D.d =:= "M0,0" <> do
      --         Bezier.bezierCircle #.. \(B3 p0 p1 p2 p3) ->
      --           "M" <> (\(V2 x y) -> show (64.0 * x) <> "," <> show (64.0 * y)) p0 <>
      --           "C" <> intercalateMap " " (\(V2 x y) -> show (64.0 * x) <> "," <> show (64.0 * y)) [ p1, p2, p3 ]
      --     , D.stylish =:= D.smarts { "stroke": "purple", "stroke-width": 2.0 }
      --     ] mempty
      -- , D.svg_"circle"
      --     [ D.attr "r" =:= "4px"
      --     , River.latestStream running \{ segments } ->
      --         let route = mkRoute segments in
      --         looping.stream <#> \t -> case routeAtTime route t of
      --           Just { at: V2 x y } -> D.MultiAttr
      --             [ D.attr "cx" x
      --             , D.attr "cy" y
      --             ]
      --           Nothing -> D.MultiAttr []
      --     , D.stylish =:= D.smarts
      --       { "fill": "red"
      --       }
      --     ] mempty
      ]
    ]


runTraintle :: InterState -> Array Command -> _ -- { info :: Dragon, curve :: String, pos :: Pos, viewBox :: String, trains :: _, state :: InterState }
runTraintle { library, hitmap } cmds =
  { curve, viewBox, pos: endpoint
  , segments, paths: result.paths
  , state: { library: _st.library, hitmap: _st.hitmap }
  , error: either identity mempty resultSplit
  , routes
  , bounds: overBounds (Int.toNumber <<< (16 * _)) <$> bounds
  , info: _
  } $ definitions $
    (either (\e -> [ D.text "error" /\ D.text e ]) mempty resultSplit) <>
    [ D.text "cmds" /\ D.show unit -- cmds
    , D.text "routes" /\ do
        definitions $ routes #:.. \name (Route route) ->
          [ D.show name /\ D.show
            { pathlength: route.pathlength
            , maxlength: route.maxlength
            , segments: NEA.length route.segments
            , isLoop: route.isLoop
            -- , zcrossings: route.crossings
            }
          ]
    , D.text "paths" /\ D.show (Array.length result.paths)
    -- , D.text "paths" /\ D.show result.paths
    , D.text "segments" /\ D.show (Array.length segments)
    -- , D.text "segments" /\ D.show (segments <#> map _ { canon { samples = unit } })
    , D.text "endpoint" /\ D.show endpoint
    , D.text "bounds" /\ D.show bounds
    -- , D.text "curve" /\ D.show curve
    , D.text "preBounds" /\ D.show preBounds
    -- , D.text "library" /\ D.show _st.library
    , D.text "hitmap" /\ D.show (Map.size _st.hitmap)
    {-
    , D.text "layout" /\ definitions
        let
          renderSwitch = (\(Tuple radius { end, segments }) -> definitionsies [ (D.show radius <> D.text " / " <> renderPos end) /\ (D.show <$> NEA.toArray segments) ])
          renderStraight = (\{ end, segments } -> definitions [ renderPos end /\ intercalateMap (D.text ", ") D.show segments ])
          _components = result.layout.components
          componentsValid = _components # all \c@{ chosen, positions } ->
            Just (unwrap chosen) == Set.findMin positions &&
              do positions # all \pos -> Map.lookup pos _components == Just c
          renderComponent = _.positions >>> Array.fromFoldable >>> intercalateMap (D.text " : ") renderPos
          renderComponents =
            if componentsValid
              then Map.values >>> List.nub >>> map renderComponent >>> Array.fromFoldable >>> ulist
              else renderPosMap renderComponent
        in
          [ D.text "switches" /\ renderPosMapsies (map renderSwitch <<< Map.toUnfoldable) result.layout.switches
          , D.text "straights" /\ renderPosMap renderStraight result.layout.straights
          , D.text "loops" /\ renderPosMap (_.positions >>> Array.fromFoldable >>> intercalateMap (D.text " : ") renderPos) result.layout.loops
          , D.text "logical" /\ D.show result.layout.logical

          , D.text "components" /\ renderComponents result.layout.components

          , D.text "physical" /\ D.show result.layout.physical
          , D.text "clusters" /\ D.show result.layout.clusters
          , D.text "crossings" /\ D.show result.layout.crossings

          -- , D.text "segments" /\ renderPosMap (renderPosMap (D.show <<< unwrap)) result.layout.segments
          ]
    -- -}
    ]
  where
  origin = { at: V2 0 0, to: V2 1 0 }
  action = do
    traverse_ renderCommand cmds
    state <- get
    paths <- routesToPaths state.path.segments
    when (Map.isEmpty state.routes) do
      case NEA.fromArray state.route of
        Nothing -> pure unit
        Just r -> do
          route <- mkRoute r
          Proxy @"routes" @~ Map.insert "default" route
    layout <- analyzeLayout state.path.segments
    pure { paths, layout }
  _st@{ path: { commands: curve, segments }, pos: endpoint, routes } /\ resultSplit /\ { bounds: preBounds } = action
    # runRWSE { origin, mode: Drawing }
      { pos: origin
      , path: mempty
      , locations: Map.empty, stacks: Map.empty
      , subroutines: Map.empty
      , route: empty
      , routes: Map.empty
      , hitmap, library
      , radii: Pair 7 13
      }
  result = either mempty identity resultSplit
  bounds :: Vec2 (Bounds Int)
  bounds = unwrap $ incorporate (App $ mkBound <$> endpoint.at) preBounds <> defaultBounds
  defaultBounds = App $ pure $ mkBound (-4) <> mkBound 4
  viewBox = case bounds of
    V2 { min: Min mx, max: Max mX } { min: Min my, max: Max mY } ->
      intercalate " " $ show <<< (16 * _) <$>
        [ mx - 4, my - 4, mX - mx + 8, mY - my + 8 ]
