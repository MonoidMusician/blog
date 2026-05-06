module Train.Main where

import Prelude

import Control.Alternative (guard)
import Control.Comonad (extract)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.ResourceM (inSubScope, selfDestructor)
import Control.Monad.ResourceT (Dir(..), ResourceM)
import Control.Monad.State (class MonadState, State, execState, get)
import Control.Monad.Writer (class MonadWriter, censor, tell)
import Control.Plus (empty, (<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.BooleanAlgebra.NormalForm (asArray)
import Data.CodePoint.Unicode (isAsciiUpper, isDecDigit)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either, hush, isLeft)
import Data.Filterable (filter, filterMap, separate)
import Data.Foldable (all, any, findMap, fold, foldM, foldMap, foldl, for_, intercalate, oneOfMap, sum, traverse_)
import Data.Foldable as F
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor.App (App(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Interval (Interval)
import Data.Lazy (Lazy, defer, force)
import Data.Lens ((%=))
import Data.Lens as L
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (un, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Number as Math
import Data.Optical ((@<>), (@=))
import Data.Ord (abs)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice ((+++), (|||))
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup.Last (Last(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Debug (spy, spyWith, traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object as FO
import Idiolect (incorporate, insteadOf, intercalateMap, minimumWith, neighbors, sgn, sqre, withIndices, (#..), (#:..), (#<>), (..$), (<#>:), (<#?>), (<>$), (==<), (>==))
import Math.Bezier as Bezier
import Math.Matrix (Afn2(..), B32, BBox2, Bez1(..), Bez3(..), Bounds, LTF(..), Lin2(..), V2, V3, Vec2(..), Vec3(..), bounds2bounds1, d2r, disjointBounds, dot, inv, mkAfn2, mkBound, norm, norm2, normalize, pairs, pairsWith, r2d, rotl2, tf, tfBounds, tfI, translate2, unAfn2, ($*), ($.), (+<), (-<>), (.*), (<.), (<>+), (<>-), (<^), (>+))
import Math.Poly (deriv)
import Monoids.BoundsWith (BoundsWith, MinWith(..))
import Parser.Comb.Dragon (renderParseError)
import Parser.Languages.HFS (Base(..), HFList, HFS(..), IsPointed(..), OpMeta(..), RuntimeError(..), bnBin, bnDec, bnHex, bnOct, bnQua, emptyEnv, hfs, hfsCount, hfsFromInt, hfsToTree, opMeta, stacksInfo, stdlib)
import Parser.Languages.HFS as HFS
import Prim.Row as Row
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones (off_, text, ($$), ($~~), (.$), (.$$), (.$~~), (<:>), (=:=), (>$), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (deletable, liveArray, sourceCode)
import Riverdragon.River (Course(..), Lake, River, coursing, createRiver, createRiverStore, createStore, dam, fix, instantiate, limitTo, makeLake, makeLake', memoize, singleShot, statefulStream, stillRiver, store, subscribe, subscribeM)
import Riverdragon.River as River
import Riverdragon.River.Bed (freshId)
import Riverdragon.River.Beyond (animationLoop, debounce, dedup, documentEvent, everyFrame, instanced, withLast)
import Riverdragon.River.Streamline (Interval(..), bbInterval, clamp2D, linmap2D)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Uncurried.RWSE (RWSE, runRWSE)
import Web.DOM (Element)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.Window (scroll)
import Web.UIEvent.MouseEvent as MouseEvent
import Widget (Widget, autoAdaptInterface, makeKeyedInterface, valueInterface)

widget :: Widget
widget { interface } = do
  { stream: valueSet, send: setValue } <- createRiver
  let receiveValue = (autoAdaptInterface @String (interface "traintle-value")).receive
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
    , dragonEither (D.Text <<< dam) renderTraintle $
        parseTraintle <<< snd <$> valueSet
    , Egg do
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
              bb <- bbInterval svg
              let
                ptExternal = clamp2D { x: Interval 0.0 32.0, y: Interval 0.0 32.0 } $
                  linmap2D bb { x: Interval (-2.0) 34.0, y: Interval (-2.0) 34.0 }
                    { x: Int.toNumber (MouseEvent.clientX event)
                    , y: Int.toNumber (MouseEvent.clientY event)
                    }
              selected $ V2 ptExternal.x ptExternal.y
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
    , Egg do
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
              bb <- bbInterval svg
              let
                ptExternal = clamp2D { x: Interval 0.0 32.0, y: Interval 0.0 32.0 } $
                  linmap2D bb { x: Interval (-2.0) 34.0, y: Interval (-2.0) 34.0 }
                    { x: Int.toNumber (MouseEvent.clientX event)
                    , y: Int.toNumber (MouseEvent.clientY event)
                    }
              selected $ V2 ptExternal.x ptExternal.y
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

type Standard =
  { key :: { from :: Vec2 Int, to :: Vec2 Int, delta :: Vec2 Int }
  , id :: Int
  , radius :: Int
  , strength :: Pair Number
  , curve :: B32
  , strokes :: Pair B32
  , samples :: Array { p :: V2, pathlength :: Pair Number, i :: Int, t :: Number }
  , bbox ::
    { stroke :: BBox2 Number
    , centerline :: BBox2 Number
    }
  -- , curvature ::
  --   { bounds :: BoundsWith Number { t :: Number, p :: V2 }
  --   , endpoints :: Vec2 Number
  --   }
  -- , derivs ::
  --   { inflection :: Array { t :: Number, p :: V2 }
  --   , horizontal :: Array { t :: Number, p :: V2 }
  --   , vertical :: Array { t :: Number, p :: V2 }
  --   }
  , pathlength :: Number
  , beeline :: Number -- norm (p3 <>- p0)
  }

allRadii = [ 7, 13, 21, 27, 41 ]

standardCurves :: Lazy (Map Int Standard)
standardCurves = defer \_ -> _.library $ flip execState { library: Map.empty } do
  _ <- standardize { from: V2 1 0, to: V2 2 1, delta: V2 5 1 } 13 curves."0 to 26"
  _ <- standardize { from: V2 2 1, to: V2 1 1, delta: V2 4 3 } 13 curves."26 to 45"
  -- _ <- standardize { from: V2 1 1, to: V2 1 2, delta: V2 3 4 } 13 curves."45 to 64"
  -- _ <- standardize { from: V2 1 2, to: V2 0 1, delta: V2 1 5 } 13 curves."64 to 90"
  _ <- standardize { from: V2 1 0, to: V2 1 1, delta: V2 5 2 } 7 curves."0 to 45"
  -- _ <- standardize { from: V2 1 1, to: V2 0 1, delta: V2 2 5 } 7 curves."45 to 90"
  _ <- standardize { from: V2 2 1, to: V2 1 2, delta: V2 3 3 } 7 curves."26 to 64"
  _ <- standardize { from: V2 1 2, to: V2 (-1) 2, delta: V2 0 6 } 7 curves."64 to 116"

  for_ [ Tuple 21 curves21, Tuple 27 curves27, Tuple 41 curves41 ] \(Tuple radius curve) -> do
    let
      deltaFor (B3 _ _ _ p3) = Int.round <<< (_ / 16.0) <$> p3
    _ <- standardize { from: V2 1 0, to: V2 2 1, delta: deltaFor curve."0 to 26" } radius curve."0 to 26"
    _ <- standardize { from: V2 2 1, to: V2 1 1, delta: deltaFor curve."26 to 45" } radius curve."26 to 45"
    pure unit
  pure unit
  where
  curves =
    { "0 to 26": B3 mempty (V2 34.536001 0.0) (V2 59.412619 5.70631) (V2 80.0 16.0)
    -- , "64 to 90": B3 mempty (V2 10.29369 20.587381) (V2 16.0 45.463999) (V2 16.0 80.0)
    , "26 to 45": B3 mempty (V2 20.882696 10.44135) (V2 37.658719 21.65872) (V2 64.0 48.0)
    -- , "45 to 64": B3 mempty (V2 26.34128 26.341281) (V2 37.55865 43.117304) (V2 48.0 64.0)
    , "0 to 45": B3 mempty (V2 28.921554 0.0) (V2 57.180985 9.18099) (V2 80.0 32.0)
    -- , "45 to 90": B3 mempty (V2 22.81901 22.81902) (V2 32.0 51.07845) (V2 32.0 80.0)
    , "26 to 64": B3 mempty (V2 20.7716 10.3857) (V2 37.6143 27.2284) (V2 48.0 48.0)
    , "64 to 116": B3 mempty (V2 15.1084 30.2167) (V2 15.1084 65.7833) (V2 0.0 96.0)
    }
  curves21 =
    { "0 to 26": B3 mempty (V2 74.0603 0.0) (V2 121.167 20.0401) (V2 144.0 32.0)
    , "26 to 45": B3 mempty (V2 32.568 16.284) (V2 70.7313 38.7313) (V2 96.0 64.0)
    }
  curves27 =
    { "0 to 26": B3 mempty (V2 71.3258 0.0) (V2 147.999 24.9521) (V2 192.0 48.0)
    , "26 to 45": B3 mempty (V2 46.0583 23.0291) (V2 78.2998 46.2998) (V2 112.0 80.0)
    }
  curves41 =
    { "0 to 26": B3 mempty (V2 91.9714 0.0) (V2 216.015 25.0079) (V2 288.0 64.0)
    , "26 to 45": B3 mempty (V2 72.4365 36.2181) (V2 142.3 94.2998) (V2 176.0 128.0)
    }

tfPos :: Afn2 Int -> Pos -> Pos
tfPos t { at, to } = { at: t $* at, to: r $* to }
  where Tuple _p r = unAfn2 t

rotations :: forall s. Ring s => Array (Lin2 s)
rotations = do
  let rot90 = Lin2 zero (negate one) one zero
  [ tfI
  , rot90
  , rot90 <. rot90
  , rot90 <. rot90 <. rot90
  ]

reverseKey ::
  { from :: Vec2 Int
  , to :: Vec2 Int
  , delta :: Vec2 Int
  } ->
  Lin2 Int ->
  { from :: Vec2 Int
  , to :: Vec2 Int
  , delta :: Vec2 Int
  }
reverseKey { from, to, delta } t =
  { delta: t $* opp delta, from: t $* opp to, to: t $* opp from }
  where opp (V2 x y) = V2 y x

findCurve :: { pos :: Pos, radius :: Int } -> Map Int Standard -> Maybe Canonized
findCurve { pos: { at: start, to: heading }, radius } library = library # findMap \standard -> do
  guard $ standard.radius == abs radius
  rotations # findMap \rot0 -> do
    let rot = rot0 <. if radius > 0 then tfI else Lin2 one zero zero (negate one)
    guard $ heading == (rot $* standard.key.from)
    let end = start <> standard.key.delta
    library # findMap \standard2 -> do
      rotations # findMap \rot2 -> do
        guard $ standard2.key == reverseKey standard.key rot2
        Just $ Pair (makeHere start rot standard) (makeHere end rot2 standard2)
  where
  makeHere :: Vec2 Int -> Lin2 Int -> Standard -> _
  makeHere here rot canon =
    let
      transformI = mkAfn2 $ Tuple here rot
      transform = mkAfn2 $ Tuple (((16.0 * _) <<< Int.toNumber) <$> here) $ Int.toNumber <$> rot
      pos = tfPos transformI <$> Pair
        { at: mempty, to: canon.key.from }
        { at: canon.key.delta, to: canon.key.to }
    in { canon, pos, transformI, transform }

standardize ::
  forall r m.
    MonadState { library :: Map Int Standard | r } m =>
  { from :: Vec2 Int
  , to :: Vec2 Int
  , delta :: Vec2 Int
  } -> Int -> B32 ->
  m Standard
standardize key radius curve@(B3 p0 p1 p2 p3) = do
  { library } <- get
  case F.find (\r -> r.key == key) library of
    Just r -> pure r
    Nothing -> do
      let
        id = Map.size library
        strokes = dilate curve
        bbox = { stroke: Bezier.bboxBs strokes, centerline: Bezier.bboxB curve }
        strength = Pair (norm $ p0 -<> p1) (norm $ p2 -<> p3)
        basesamples = Array.range 0 100 <#> \i ->
          let t = Int.toNumber i / 100.0
          in { i, t, p: Bezier.evalB curve t }
        trackLength { l, p: q0 } { i, t, p: q1 } =
          let l' = l + norm (q0 -<> q1) in
          { accum: { l: l', p: q1 }, value: { pathlength: Pair l l', p: q1, i, t } }
        { accum: { l: pathlength }, value: samples } = mapAccumL trackLength { l: 0.0, p: p0 } basesamples
        beeline = norm (p0 -<> p3)
        r = { key, id, radius, strength, curve, samples, strokes, bbox, pathlength, beeline }
      setProp @"library" $ Map.insert id r library
      case key.delta of
        V2 dx dy | dx /= dy && dx /= 0 && dy /= 0 ->
          void $ standardize (reverseKey key tfI) radius $
            (opp p3 <>- _) <<< opp <$> B3 p3 p2 p1 p0
        _ -> pure unit
      for_ [ key.from, key.to ] \slope@(V2 dx dy) -> do
        standardize { delta: 2 .* slope, from: slope, to: slope } 0 $
          Bezier.castUp $ Bezier.castUp $ B1 mempty $
            V2 (32.0 * Int.toNumber dx) (32.0 * Int.toNumber dy)
      pure r
  where opp (V2 x y) = V2 y x

type Canonized = Pair
  { canon :: Standard
  , pos :: Pair Pos
  , transform :: Afn2 Number
  , transformI :: Afn2 Int -- FIXME
  }

newtype Route = Route
  { segments :: Array { segment :: Canonized, i :: Int, pathlength :: Pair Number }
  , pathlength :: Number
  -- Information about loops, crossings, signals maybe
  -- Time
  }
mkRoute :: Array Canonized -> Route
mkRoute segments =
  let
    mapper l0 (Tuple i segment@(Pair { canon } _)) =
      let l1 = l0 + canon.pathlength in
      { accum: l1, value: { segment, i, pathlength: Pair l0 l1 } }
    { accum: pathlength, value: segments } = mapAccumL mapper 0.0 $ withIndices segments
  in Route { pathlength, segments }

canonCurve :: Canonized -> B32
canonCurve (Pair p _) = tf (LTF p.transform) p.canon.curve

canonStrokeBox :: Canonized -> BBox2 Number
canonStrokeBox (Pair { canon: { bbox: { stroke } }, transform } _) =
  tfBounds transform stroke

type HitMap = Map (Afn2 Number) (Map (Pair Int) Boolean)

continues :: Canonized -> Canonized -> Maybe Boolean
continues (Pair p _) (Pair q _) =
  let
    Tuple pStart prot = unAfn2 p.transformI
    Tuple qStart qrot = unAfn2 q.transformI
    pEnd = p.transformI $* p.canon.key.delta
    qEnd = q.transformI $* q.canon.key.delta
    pFrom = prot $* p.canon.key.from
    pTo = prot $* p.canon.key.to
    qFrom = qrot $* q.canon.key.from
    qTo = qrot $* q.canon.key.to
  in Array.head do
    Tuple pAt pDir <- [ Tuple pStart pFrom, Tuple pEnd pTo ]
    Tuple qAt qDir <- [ Tuple qStart qFrom, Tuple qEnd qTo ]
    -- _ <- pure $ spy "continues" { pAt, pDir, qAt, qDir }
    if pAt /= qAt then [] else [(pDir == qDir) == ((pAt == pStart) /= (qAt == qStart))]

intersects :: forall m r. MonadState { hitmap :: HitMap | r } m => Pair Canonized -> m Boolean
intersects (Pair p q) | disjointBounds (canonStrokeBox p) (canonStrokeBox q) = pure false
intersects (Pair p q) | Just result <- continues p q = pure $ not result
intersects (Pair (Pair p _) (Pair q _)) = do
  { hitmap } <- get
  let relate = p.transform <^ q.transform
  let ids = (Pair p.canon.id q.canon.id)
  case Map.lookup relate hitmap >>= Map.lookup ids of
    Just result -> pure result
    Nothing -> do
      let
        Pair p1 p2 = tf (LTF p.transform) <$> p.canon.strokes
        Pair q1 q2 = tf (LTF q.transform) <$> q.canon.strokes
        result =
          Bezier.doesIntersect p1 q1 ||
          Bezier.doesIntersect p1 q2 ||
          Bezier.doesIntersect p2 q1 ||
          Bezier.doesIntersect p2 q2
      setProp @"hitmap" $ Map.alter (Just <<< Map.insert ids result <<< fromMaybe Map.empty) relate hitmap
      pure result

parseTraintle :: String -> Either String (Array Command)
parseTraintle =
  String.split (String.Pattern "")
  >>> dimap Array.toUnfoldable (map Array.fromFoldable) parseCommands

trackEither :: forall x y. River (Either x y) -> Lake (Either (Tuple x (Lake x)) (Tuple y (Lake y)))
trackEither = memoize >>> \input ->
  let
    { left: xs, right: ys } = separate input
  in withLast input <#?> case _ of
    { last: Just (Left _), next: Left _ } -> Nothing
    { last: Just (Right _), next: Right _ } -> Nothing
    { next: Left x } -> Just (Left (Tuple x xs))
    { next: Right y } -> Just (Right (Tuple y ys))

dragonEither :: forall x y. (River x -> Dragon) -> (River y -> Dragon) -> River (Either x y) -> Dragon
dragonEither f g = trackEither >>> map (withHead f ||| withHead g) >>> D.Replacing
  where
  withHead :: forall z. (River z -> Dragon) -> Tuple z (Lake z) -> Dragon
  withHead h (Tuple z zs) = D.Egg do
    { stream: zz } <- store zs
    pure $ h (pure z <|> zz)

-- trundle

-- wdassssssdaaxdwwwwwwwwwassaxddsssdxasssssdwaaadxwdaxaddaaxwwdxaa
-- 16dwwwaa16saawww8dxwwwdd16sdd6wdd16sddwww8axwwwaa16saawww
renderTraintle :: River (Array Command) -> Dragon
renderTraintle cmds = D.Egg do
  { defs, defL, defineL } <- manageDefs
  { stream: running } <- River.store do
    statefulStream { library: force standardCurves, hitmap: Map.empty } (dedup cmds)
      \s c -> let r = runTraintle s c in { emit: Just r, state: r.state }
  curve <- defineL \id -> D.path [ D.id =:= id, D.attr "d" <:> _.curve <$> running ]
  looping <- River.store do
    everyFrame # River.mapAl \_ -> now <#> unInstant >>> \(Milliseconds t) ->
      let
        duration = 20.0 * 1000.0
        loopPos = (t Math.% (2.0 * duration)) / duration
        loopAndBack = if loopPos <= 1.0 then loopPos else 2.0 - loopPos
      in loopAndBack
  let
    railmask = newmask curve
    newmask curve inner outer = maskOf $ fold
      [ clone curve
        [ D.stylish =:= D.smarts
          { "stroke": "white"
          , "stroke-width": show outer <> "px"
          }
        ]
      , clone curve
        [ D.stylish =:= D.smarts
          { "stroke": "black"
          , "stroke-width": show inner <> "px"
          }
        ]
      ]
    maskOf contents = mask $ defL \id -> D.svg_"mask" [ D.id =:= id ] contents
    freshTrains = River.latestStream running \{ segments } ->
      let
        route = mkRoute segments
        canonCurves = canonCurve <$> segments
        consist = [ 16.0 ] <>$ Array.range 0 2 #.. const [ 64.0, 32.0 ] #<> [ 64.0 ]
        walkFrom point = walkPaths canonCurves point consist
        seedTrain =
          looping.stream <#> \loopTime ->
            case routeAtTime route loopTime of
              Just { at, to, curvature, i, t } -> { at, to: inv to, curvature, i, t, delta: mempty, distance: zero }
              Nothing -> endPath canonCurves
      in walkFrom <$> seedTrain
    withTrainUnits = liveArray $ freshTrains <#> \trains -> do
      NEA.drop 1 trains # pairs # withIndices
        # filter (Int.even <<< fst)
        # map snd # neighbors
  pure $ fold
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
      -- , clone curve
      --     [ D.stylish =:= D.smarts
      --       { "stroke": "#361f13"
      --       , "stroke-dasharray": "2.76,5.28"
      --       , "stroke-dashoffset": "5.28"
      --       , "stroke-width": "24px"
      --       }
      --     ]
      , running >@ \{ paths } -> paths #.. \path -> Egg do
          thisOne <- defineL \id -> D.path [ D.id =:= id, D.attr "d" =:= path ]
          pure $ clone thisOne
            [ D.stylish =:= D.smarts
              { "stroke": "#5a2814"
              , "stroke-width": "16px"
              }
            , newmask thisOne 12 16
            ]
      , D.g [ maskOf $ D.g [] $ fold
          [ clone curve
              [ D.stylish =:= D.smarts
                { "stroke": "white"
                , "stroke-width": "16px"
                }
              ]
          , running >@ \{ paths } -> paths #.. \path -> Egg do
            thisOne <- defineL \id -> D.path [ D.id =:= id, D.attr "d" =:= path ]
            pure $ clone thisOne
              [ D.stylish =:= D.smarts
                { "stroke": "black"
                , "stroke-width": "13px"
                }
              , newmask thisOne 11 13
              ]
          ]
        ] $
          running >@ \{ paths } -> paths #.. \path -> Egg do
            thisOne <- defineL \id -> D.path [ D.id =:= id, D.attr "d" =:= path ]
            pure $ clone thisOne
              [ D.stylish =:= D.smarts
                { "stroke": "#cbd4d8"
                , "stroke-width": "15px"
                }
              , newmask thisOne 13 15
              ]
      , clone curve
          [ D.stylish =:= D.smarts
            { "stroke": "#cbd4d8"
            , "stroke-width": "15px"
            }
          , railmask 13 15
          ]
      , D.g [ D.stylish =:= D.smarts { "opacity": 0.7 } ] $ fold
        [ D.svg_"circle"
            [ D.attr "r" =:= "4px"
            , running <#> \{ pos: { at: V2 x y } } -> D.MultiAttr
                [ D.attr "cx" $ 16*x
                , D.attr "cy" $ 16*y
                ]
            , D.stylish =:= D.smarts
              { "fill": "red"
              }
            ] mempty
        , D.svg_"path"
            [ D.attr "d" <:> running <#> \{ pos: { at: V2 x y, to: V2 dx dy } } ->
                "M" <> show (16*x) <> "," <> show (16*y) <> "l" <> show (16*dx) <> "," <> show (16*dy)
            , D.stylish =:= D.smarts
              { "stroke": "red"
              , "stroke-width": "2px"
              }
            ] mempty
        ]
      , D.g [ D.stylish =:= D.smarts { "opacity": 1.0 } ] $
          withTrainUnits \_idx trainUnit ->
            let
              cslope = -sqre 9.0 / 2.0
              jog curvature = "translate(" <> show 0.0 <> ", " <> show (curvature * cslope) <> ")"
              awayfrom = trainUnit <#> \{ prev, here: Pair back train } -> case prev of
                Nothing -> train.delta
                Just _ -> back.delta
              towards = trainUnit <#> \{ here: Pair back train, next } -> case next of
                Nothing -> train.delta
                Just (Pair { delta } _) -> delta
            in D.g [] $ fold
              [ mempty
              , clone (pure "#g115")
                  [ dam $ D.attr "transform" <:> intercalate (pure " ")
                    [ trainUnit <#> \{ here: Pair back train, next } -> case back.at of
                      V2 x y -> "translate(" <> show x <> ", " <> show y <> ")"
                    , awayfrom <#> case _ of
                      V2 dx dy -> "rotate(" <> show (Math.atan2 dy dx * r2d) <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> jog back.curvature
                    , pure "translate(64, 0) rotate(-90,-384,208)"
                    ]
                  ]
              , clone (pure "#use115")
                  [ dam $ D.attr "transform" <:> intercalate (pure " ")
                    [ trainUnit <#> \{ here: Pair back train, next } -> case back.at of
                      V2 x y -> "translate(" <> show x <> ", " <> show y <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> case back.to of
                      V2 dx dy -> "rotate(" <> show (Math.atan2 dy dx * r2d) <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> jog back.curvature
                    , pure "rotate(180) translate(64, 0) rotate(-90,-384,208)"
                    ]
                  ]
              , clone (pure "#g115")
                  [ dam $ D.attr "transform" <:> intercalate (pure " ")
                    [ trainUnit <#> \{ here: Pair back train, next } -> case train.at of
                      V2 x y -> "translate(" <> show x <> ", " <> show y <> ")"
                    , towards <#> case _ of
                      V2 dx dy -> "rotate(" <> show (Math.atan2 dy dx * r2d) <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> jog train.curvature
                    , pure "rotate(180) translate(64, 0) rotate(-90,-384,208)"
                    ]
                  ]
              , clone (pure "#use115")
                  [ dam $ D.attr "transform" <:> intercalate (pure " ")
                    [ trainUnit <#> \{ here: Pair back train, next } -> case train.at of
                      V2 x y -> "translate(" <> show x <> ", " <> show y <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> case train.to of
                      V2 dx dy -> "rotate(" <> show (Math.atan2 dy dx * r2d) <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> jog train.curvature
                    , pure "rotate(180) translate(64, 0) rotate(-90,-384,208)"
                    ]
                  ]
              , clone (pure "#g189425-2")
                  [ dam $ D.attr "transform" <:> intercalate (pure " ")
                    [ trainUnit <#> \{ here: Pair back train, next } -> case train.at of
                      V2 x y -> "translate(" <> show x <> ", " <> show y <> ")"
                    , trainUnit <#> \{ here: Pair back train, next } -> case train.delta of
                      V2 dx dy -> "rotate(" <> show (Math.atan2 dy dx * r2d) <> ")"
                    , pure "rotate(180, -32, 0)"
                    ]
                  -- , D.attr "opacity" =:= 0.4
                  ]
              ]
      , D.Replacing $ freshTrains <#> \trains ->
          D.g [ D.stylish =:= D.smarts { "opacity": 0.0 } ] $ fold $ trains #.. \train ->
            [ D.svg_"circle"
                [ D.attr "r" =:= "4px"
                , pure train <#> \{ at: V2 x y } -> D.MultiAttr
                    [ D.attr "cx" x
                    , D.attr "cy" y
                    ]
                , D.stylish =:= D.smarts
                  { "fill": "yellow"
                  }
                ] mempty
            , D.svg_"path"
                [ D.attr "d" <:> pure train <#> \{ at: V2 x y, to: V2 dx dy } ->
                    "M" <> show x <> "," <> show y <> "l" <> show dx <> "," <> show dy
                , D.stylish =:= D.smarts
                  { "stroke": "yellow"
                  , "stroke-width": "2px"
                  }
                ] mempty
            ]
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
    , D.Replacing $ _.info <$> running
    ]

setProp :: forall @sym t r r' m. MonadState (Record r) m => IsSymbol sym => Row.Cons sym t r' r => t -> m Unit
setProp v = Proxy @sym @= v

tellR :: forall w m. MonadWriter w m => (w -> w) -> m Unit
tellR f = tell $ f (mempty :: w)

type InterState =
  { library :: Map Int Standard
  , hitmap :: HitMap
  }

runTraintle :: InterState -> Array Command -> _ -- { info :: Dragon, curve :: String, pos :: Pos, viewBox :: String, trains :: _, state :: InterState }
runTraintle { library, hitmap } cmds =
  { curve, viewBox, pos: endpoint
  , trains, segments, paths: result.paths
  , state: { library: _st.library, hitmap: _st.hitmap }
  , error: either identity mempty resultSplit
  , info: _
  } $ definitions $
    (either (\e -> [ D.text "error" /\ D.text e ]) mempty resultSplit) <>
    [ D.text "cmds" /\ D.show cmds
    , D.text "paths" /\ D.show (Array.length result.paths)
    -- , D.text "paths" /\ D.show result.paths
    , D.text "train distances" /\ D.show (trains <#> _.distance)
    -- , D.text "trains" /\ D.show trains
    , D.text "segments" /\ D.show (Array.length segments)
    -- , D.text "segments" /\ D.show (segments <#> map _ { canon { samples = unit } })
    , D.text "endpoint" /\ D.show endpoint
    , D.text "bounds" /\ D.show bounds
    -- , D.text "curve" /\ D.show curve
    , D.text "norm" /\ D.show norm
    , D.text "rotation" /\ D.show rotation
    , D.text "quadrant" /\ D.show (quadrant endpoint.to)
    , D.text "preBounds" /\ D.show preBounds
    -- , D.text "library" /\ D.show _st.library
    , D.text "hitmap" /\ D.show (Map.size _st.hitmap)
    ]
  where
  origin = { at: V2 0 0, to: V2 1 0 }
  action = do
    traverse_ renderCommand cmds
    state <- get
    paths <- routesToPaths state.path.segments
    pure { paths }
  _st@{ path: { commands: curve, segments }, pos: endpoint } /\ resultSplit /\ { bounds: preBounds } = action
    # runRWSE { origin, mode: Drawing }
      { pos: origin
      , path: mempty
      , locations: Map.empty, stacks: Map.empty
      , subroutines: Map.empty
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
  norm = fst $ alignGrid false endpoint
  rotation = (fst $ snd $ alignGrid false endpoint) (V2 1 2)
  trains =
    walkPaths (canonCurve <$> segments) (endPath (canonCurve <$> segments))
      $ [ 16.0 ] <>$ Array.range 0 1 #.. const [ 64.0, 32.0 ] #<> [ 64.0 ]

silent :: TraintleM ~> TraintleM
silent act = do
  { path } <- get
  r <- censor mempty act
  setProp @"path" path
  pure r

renderCommand :: Command -> TraintleM Unit
renderCommand (Silent inner) = do
  silent $ traverse_ renderCommand inner
renderCommand (SetVariable Nothing name) = do
  { pos } <- get
  prop (Proxy @"locations") %= Map.insert name pos
renderCommand (SetVariable (Just extra) name) = do
  { pos } <- get
  prop (Proxy @"stacks") %= Map.insertWith (flip append) name (pos <$ List.range 0 extra)
renderCommand (GetVariable Nothing name) = do
  { locations } <- get
  case Map.lookup name locations of
    Just pos -> setProp @"pos" pos
    Nothing -> throwError $ "Unknown variable " <> show name
renderCommand (GetVariable (Just extra) name) = do
  { stacks } <- get
  case List.drop extra <$> Map.lookup name stacks of
    Just (pos : left) -> do
      setProp @"pos" pos
      prop (Proxy @"stacks") %= Map.insert name left
    Just Nil -> throwError $ "Variable had no values " <> show name
    Nothing -> throwError $ "Unknown variable " <> show name
renderCommand Origin = do
  { origin } <- ask
  setProp @"pos" origin
renderCommand (Subroutine name (Just cmds)) = do
  prop (Proxy @"subroutines") %= Map.insert name cmds
renderCommand (Subroutine name Nothing) = do
  { subroutines } <- get
  case Map.lookup name subroutines of
    Just cmds -> traverse_ renderCommand cmds
    Nothing -> throwError $ "Unknown subroutine " <> show name
renderCommand (SetRadius i) = do
  setProp @"radii" $
    fromMaybe (fromMaybe 0 (Array.last allRadii)) <<< Array.index allRadii <$>
      Pair i (i+1)
renderCommand v = trackBounds (renderAligned v)

renderAligned :: Command -> TraintleM Unit
renderAligned cmd = do
  { radii: Pair sharp shallow } <- get
  case cmd of
    W -> pathCommand false 0
    S -> pathCommand true 0
    Z -> jog \slope -> { delta: 2 .* fromQuadrant 1 slope, slope }
    C -> jog \slope -> { delta: 2 .* toQuadrant 1 slope, slope }
    X -> jog \slope -> { delta: mempty, slope: -1 .* slope }
    E -> pathCommand false shallow
    D -> pathCommand false sharp
    A -> pathCommand false (-sharp)
    Q -> pathCommand false (-shallow)
    _ -> pure unit
  where
  jog f = do
    { pos: { at, to } } <- get
    let { delta, slope } = f to
    setProp @"pos" { at: at <> delta, to: slope }
  pathCommand :: Boolean -> Int -> TraintleM Unit
  pathCommand =
    (if _ then \{ at, to } -> { at, to: inv to } else identity) >>>
    \reversies radius -> do
      { pos, library } <- get
      case findCurve { pos: reversies pos, radius } library of
        Just segment@(Pair fwd _) -> do
          Proxy @"path" /\ Proxy @"commands" @<>
            ("C" <> intercalateMap " " (intercalateMap "," show) (Array.drop 1 $ Array.fromFoldable $ canonCurve segment))
          Proxy @"path" /\ Proxy @"segments" @<> [ segment ]
          Proxy @"pos" @= case fwd of
            { pos: Pair _ arrived } -> reversies arrived
        Nothing -> throwError "Could not find appropriate segment"


type Pos = { at :: Vec2 Int, to :: Vec2 Int }
data TrainMode = Drawing | Routing String
type TraintleM = RWSE
  { origin :: Pos
  , mode :: TrainMode
  }
  { bounds :: Maybe (App Vec2 (Bounds Int))
  }
  { pos :: Pos
  , path ::
    { commands :: String
    , segments :: Array Canonized
    , endpoint :: Maybe (Last Pos)
    , moves :: Set (Pair Pos)
    }
  , locations :: Map String Pos
  , stacks :: Map String (List Pos)
  , library :: Map Int Standard
  , hitmap :: HitMap
  , subroutines :: Map String (Array Command)
  , routes :: Map String Route
  , radii :: Pair Int
  }
  String

data Command
  = Q | W | E | A | S | D | Z | X | C
  | SetRadius Int
  | Silent (Array Command)
  | SetVariable (Maybe Int) String
  | GetVariable (Maybe Int) String
  | Subroutine String (Maybe (Array Command))
  | Teleport (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) | Origin
  | TrainRoute String (Array TrainUnit) (Array Command)
derive instance Eq Command
derive instance Ord Command

derive instance Generic Command _
instance Show Command where show c = genericShow c

parseCommands :: List String -> Either String (List Command)
parseCommands s = s # parseCommandsWith (Right Nil)
  \s' -> Left $ "Error at: " <> show (String.joinWith "" (Array.fromFoldable s'))

parseCommandsWith :: forall f. Functor f => f (List Command) -> (List String -> f (List Command)) -> List String -> f (List Command)
parseCommandsWith finish continue (" " : s) = parseCommandsWith finish continue s
parseCommandsWith finish continue ("\n" : s) = parseCommandsWith finish continue s
parseCommandsWith finish continue ("\t" : s) = parseCommandsWith finish continue s
-- # comment
parseCommandsWith finish continue ("#" : s) = parseCommandsWith finish continue $ List.dropWhile (_ /= "\n") s
-- q/e: shallow turns
-- w/s: forward/back
-- a/d: sharp turns
-- x: reverse direction
parseCommandsWith finish continue ("q" : s) = (Q : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("w" : s) = (W : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("e" : s) = (E : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("a" : s) = (A : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("s" : s) = (S : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("d" : s) = (D : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("z" : s) = (Z : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("x" : s) = (X : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("c" : s) = (C : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("u" : s) = (Silent (pure Q) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("i" : s) = (Silent (pure W) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("o" : s) = (Silent (pure E) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("j" : s) = (Silent (pure A) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("k" : s) = (Silent (pure S) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("l" : s) = (Silent (pure D) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("r" : s)
  | digits <- List.takeWhile (all isDecDigit <<< String.toCodePointArray) s
  , Just num <- Int.fromString (fold digits)
  = (SetRadius (num-1) : _) <$> parseCommandsWith finish continue
      (List.drop (List.length digits) s)
-- @NAME{...}: drawing subroutine
parseCommandsWith finish continue ("@" : s0)
  | Tuple name s1 <- parseNAME $ skipWS s0 =
    case skipWS s1 of
      "{" : s2 -> let
        exitScope = case _ of
          "}" : s3 -> Tuple (Just s3) Nil
          _ -> Tuple Nothing Nil
        in case parseCommandsWith (Tuple (Just Nil) Nil) exitScope s2 of
          Tuple Nothing _ -> continue ("@" : s0)
          Tuple (Just s3) cmds ->
            (Subroutine name (Just (List.toUnfoldable cmds)) : _) <$> parseCommandsWith finish continue s3
      _ -> (Subroutine name Nothing : _) <$> parseCommandsWith finish continue s1
-- ~NAME(...){...}: train and route
--     <==:{=}:{%}:{=}:<==
--     <======={======}{======}{======}=======>
parseCommandsWith finish continue ("~" : s0)
  | Tuple name s1 <- parseNAME $ skipWS s0
  , Tuple train s2 <- parseTrain $ skipWS s1
  , Tuple route s3 <- parseRoute $ skipWS s2
  =
    (TrainRoute name (maybe [] List.toUnfoldable train) (List.toUnfoldable route) : _) <$>
      parseCommandsWith finish continue s3
-- &NAME{...}: route
-- {...}: logic?
-- <x,y>, <x,y:>, <:dx,dy>, <x,y:dx,dy>: relative teleport to:facing
-- $var: procedure variables?
-- *var: multiplicative variables?
-- 0-9, 0-9(...): repeat
-- (?...): without writing paths
parseCommandsWith finish continue ("(" : inner)
  = parseCommandsWith finish continue ("1" : "(" : inner)
parseCommandsWith finish continue s
  | digits <- List.takeWhile (all isDecDigit <<< String.toCodePointArray) s
  , Just num <- Int.fromString (fold digits) =
    case skipWS $ List.drop (List.length digits) s of
      "(" : questionInner -> let
        exitScope = case _ of
          ")" : more -> Tuple (Just (Tuple num more)) Nil
          _ -> Tuple Nothing Nil
        Tuple question inner = case questionInner of
          "?" : inner -> Tuple ((\cmds -> (Silent (List.toUnfoldable cmds) : _))) inner
          inner -> Tuple (<>) inner
        in case parseCommandsWith (Tuple (Just (Tuple 1 mempty)) Nil) exitScope inner of
          Tuple Nothing _ -> continue s
          Tuple (Just (Tuple repeats more)) cmds ->
            question (power cmds repeats) <$> parseCommandsWith finish continue more
      c : more ->
        case parseCommands (pure c) of
          Left _ -> continue (c : more)
          Right cmds -> (power cmds num <> _) <$> parseCommandsWith finish continue more
      Nil -> finish
-- capital, or quotes: load named place
-- =NAME: set named place
-- =: teleport to origin, of function or whole program
-- /NAME: append to named variable's stack (repeat if desired)
-- \NAME: pop from named variable's stack (teleport to location)
parseCommandsWith finish continue s
  | Tuple name more <- parseNAME s
  , name /= "" = (GetVariable Nothing name : _) <$> parseCommandsWith finish continue more
parseCommandsWith finish continue ("=" : s0)
  | Tuple name more <- parseNAME $ skipWS s0
  =
    ((if name == "" then Origin else (SetVariable Nothing name)) : _) <$> parseCommandsWith finish continue more
parseCommandsWith finish continue ("/" : s0)
  | taken <- List.length $ List.takeWhile (_ == "/") s0
  , s1 <- List.drop taken s0
  , Tuple name more <- parseNAME $ skipWS s1
  , name /= ""
  =
    (SetVariable (Just (taken + 1)) name : _) <$>
      parseCommandsWith finish continue more
parseCommandsWith finish continue ("\\" : s0)
  | taken <- List.length $ List.takeWhile (_ == "\\") s0
  , s1 <- List.drop taken s0
  , Tuple name more <- parseNAME $ skipWS s1
  , name /= ""
  =
    (GetVariable (Just (taken + 1)) name : _) <$>
      parseCommandsWith finish continue more

parseCommandsWith finish _ Nil = finish
parseCommandsWith _ continue s = continue s

skipWS :: List String -> List String
skipWS (" " : s) = skipWS s
skipWS ("\n" : s) = skipWS s
skipWS ("\t" : s) = skipWS s
skipWS ("#" : s) = skipWS $ List.dropWhile (_ /= "\n") s
skipWS s = s

parseNAME :: List String -> Tuple String (List String)
parseNAME ("\"" : s) =
  let taken = List.takeWhile (_ /= "\"") s
  in Tuple (fold taken) (List.drop (List.length taken + 1) s)
parseNAME s =
  let taken = List.takeWhile (all (isAsciiUpper || eq (codePointFromChar '_')) <<< String.toCodePointArray) s
  in Tuple (fold taken) (List.drop (List.length taken) s)

data Direction = Forward | Backward | Bothward
derive instance Eq Direction
derive instance Ord Direction
derive instance Generic Direction _
instance Show Direction where show = genericShow
data TrainUnit = Locomotive Direction Int | Car String Int
derive instance Eq TrainUnit
derive instance Ord TrainUnit
derive instance Generic TrainUnit _
instance Show TrainUnit where show = genericShow

parseTrain :: List String -> Tuple (Maybe (List TrainUnit)) (List String)
parseTrain ("(" : s0) =
  let
    go acc s1 = case parseTrainUnit $ skipWS s1 of
      Tuple Nothing s2 -> Tuple (Just acc)
        case skipWS s2 of
          ")" : s3 -> s3
          s3 -> s3
      Tuple (Just u) s2 -> go (u : acc) s2
  in go empty s0
parseTrain s = Tuple Nothing s

-- <== Locomotive Forward 3
-- ==> Locomotive Backward 3
-- <=> Locomotive Bothward 3
-- {=} Car "=" 3
parseTrainUnit :: List String -> Tuple (Maybe TrainUnit) (List String)
parseTrainUnit s0 =
  case skipWS s0 of
    "<" : s1 -> parseLoco true 1 s1
    "=" : s1 -> parseLoco false 1 s1
    "{" : s1 -> parseCar "" s1
    ":" : s1 -> parseTrainUnit s1
    _ -> Tuple Nothing s0
  where
  parseCar acc s1 =
    case s1 of
      "}" : s2 -> Tuple (if acc == "" then Nothing else Just (Car acc (String.length acc + 2))) s2
      s2@(")" : _) -> Tuple (if acc == "" then Nothing else Just (Car acc (String.length acc + 1))) s2
      s2@("]" : _) -> Tuple (if acc == "" then Nothing else Just (Car acc (String.length acc + 1))) s2
      Nil -> Tuple (if acc == "" then Nothing else Just (Car acc (String.length acc + 1))) Nil
      c : s2 -> parseCar (acc <> c) s2
  parseLoco starts acc s1 =
    case s1 of
      ">" : s2 -> Tuple (Just (loco starts true (acc + 1))) s2
      "=" : s2 -> parseLoco starts (acc + 1) s2
      s2 -> Tuple (Just (loco starts false acc)) s2
  loco starts finishes = case starts, finishes of
    true, false -> Locomotive Forward
    true, true -> Locomotive Bothward
    false, true -> Locomotive Backward
    false, false -> Locomotive Bothward

parseRoute :: List String -> Tuple (List Command) (List String)
parseRoute ("{" : s0) =
  let
    go s1 = case parseCommandsWith (Tuple empty empty) (Tuple <@> Nil) s1 of
      Tuple s2 cmds -> Tuple cmds
        case skipWS s2 of
          "}" : s3 -> s3
          s3 -> s3
  in go s0
parseRoute s = Tuple empty s

withAligned :: forall i.
  (i -> Boolean) ->
  (Reorienter -> Vec2 Int -> i -> TraintleM { delta :: Vec2 Int, slope :: Vec2 Int }) ->
  i -> TraintleM Unit
withAligned shouldReverse f i = do
  { pos } <- get
  let Tuple aligned (Tuple (reorienter :: Reorienter) continue) = alignGrid (shouldReverse i) pos
  applied <- f reorienter aligned i
  setProp @"pos" $ continue applied
  pure unit

trackBounds :: TraintleM ~> TraintleM
trackBounds act = do
  { path: cancel, pos: before } <- get
  when (cancel.endpoint /= Just (Last before)) do
    let V2 x y = before.at
    setProp @"path" cancel { commands = cancel.commands <> "M" <> show (16*x) <> "," <> show (16*y) }
  { path: { commands } } <- get
  r <- act
  { path, pos: after } <- get
  let
    reversed = Pair
      { at: after.at, to: inv after.to }
      { at: before.at, to: inv before.to }
    backwards =
      if before.to /= after.to then Pair before after else Pair
        { at: after.at, to: before.to }
        { at: before.at, to: after.to }
    revback =
      if before.to /= after.to then Pair before after else Pair
        { at: before.at, to: inv after.to }
        { at: after.at, to: inv before.to }
  case path.commands == commands || any Set.member [ Pair before after, reversed, backwards, revback ] cancel.moves of
    true -> setProp @"path" cancel
    false -> do
      tellR _ { bounds = Just <<< App <<< map mkBound ..$ [ before.at, after.at ] }
      setProp @"path" path { endpoint = Just (Last after), moves = Set.insert (Pair before after) path.moves }
  pure r

definitions :: Array (Tuple Dragon Dragon) -> Dragon
definitions entries = D.dl.$~~ entries >>= \(Tuple term def) ->
  [ D.dt.$ term, D.dd.$ def ]

type Reorienter = forall x. Ring x => Vec2 x -> Vec2 x

quadrant :: Vec2 Int -> Int
quadrant (V2 sx sy)
  | sx > 0, sy >= 0 = 0
  | sy > 0, sx <= 0 = 1
  | sx < 0, sy <= 0 = 2
  | sy < 0, sx >= 0 = 3
  | otherwise = 0 -- V2 0 0

fromQuadrant :: Int -> Reorienter
fromQuadrant 2 = \(V2 x y) -> V2 (negate x) (negate y)
fromQuadrant 1 = \(V2 x y) -> V2 y (negate x)
fromQuadrant 3 = \(V2 x y) -> V2 (negate y) x
fromQuadrant _ = \xy -> xy

toQuadrant :: Int -> Reorienter
toQuadrant 2 = \(V2 x y) -> V2 (negate x) (negate y)
toQuadrant 3 = \(V2 x y) -> V2 y (negate x)
toQuadrant 1 = \(V2 x y) -> V2 (negate y) x
toQuadrant _ = \xy -> xy

-- Orient to >= 0, < 90
alignGrid :: Boolean -> Pos -> Tuple ({- normalized slope -} Vec2 Int) (Tuple Reorienter ({ delta :: Vec2 Int, slope :: Vec2 Int } -> Pos))
alignGrid shouldReverse { at, to } =
  Tuple (fromQuadrant which $ rev to) $ reorientWith do
    (\xy -> rev (toQuadrant which xy)) :: Reorienter
  where
  which = quadrant (rev to)
  rev :: Reorienter
  rev (V2 x y) | shouldReverse = V2 x (negate y)
  rev xy = xy
  reorientWith :: Reorienter -> (Tuple Reorienter ({ delta :: Vec2 Int, slope :: Vec2 Int } -> Pos))
  reorientWith f = Tuple @Reorienter f \{ delta, slope } ->
    { at: at <> f delta, to: f slope }

resourceMAsLake :: ResourceM ~> Lake
resourceMAsLake create = makeLake \cb -> do
  liftEffect <<< cb =<< create

manageDefs :: ResourceM
  { defs :: Dragon
  , defM :: (String -> Dragon) -> ResourceM String
  , defL :: (String -> Dragon) -> Lake String
  , defineL :: (String -> Dragon) -> ResourceM (Lake String)
  }
manageDefs = do
  thisOne <- liftEffect do randomInt 0 65535
  ids <- liftEffect freshId <#> map \i -> "defs." <> show thisOne <> "-" <> show i
  defs <- createRiver
  allDefs <- coursing History defs.stream
  defsDestroyable <- deletable $ D.Appending allDefs.stream
  let
    registerDef mkObject = do
      newId <- liftEffect ids
      let dragon = mkObject newId
      whilePresent <- deletable $ dragon
      liftEffect do defs.send whilePresent
      pure $ "#" <> newId
  pure
    { defs: defsDestroyable
    , defM: registerDef
    , defL: resourceMAsLake <<< registerDef
    , defineL: \mkObject ->
        resourceMAsLake <$> instanced do
          registerDef mkObject
    }

clone :: Lake String -> Array (Lake D.AttrProp) -> Dragon
clone id attrs = D.svg_"use" ([ D.xlink_href <:> id ] <> attrs) mempty

mask :: Lake String -> Lake D.AttrProp
mask ids = D.attr "mask" <:> ids <#> \id -> "url(" <> id <> ")"

type OnPath =
  { at :: V2
  , to :: V2
  , curvature :: Number
  , t :: Number
  , i :: Int
  , delta :: V2
  , distance :: Number
  }

startPath :: Array B32 -> OnPath
startPath = Array.head >>> case _ of
  Nothing -> { at: mempty, to: V2 0.0 0.0, t: 0.0, i: 0, delta: mempty, distance: 0.0, curvature: 0.0 }
  Just (B3 p0 p1 _ _) -> { at: p0, to: normalize $ p1 <>- p0, t: 0.0, i: 0, delta: mempty, distance: 0.0, curvature: 0.0 }

endPath :: Array B32 -> OnPath
endPath items = case Array.last items of
  Nothing -> { at: mempty, to: V2 0.0 0.0, t: 0.0, i: 0, delta: mempty, distance: 0.0, curvature: 0.0 }
  Just (B3 _ _ p2 p3) -> { at: p3, to: normalize $ p2 <>- p3, t: 1.0, i: Array.length items - 1, delta: mempty, distance: 0.0, curvature: 0.0 }


walkPaths ::
  Array B32 -> OnPath ->
  Array Number ->
  NonEmptyArray OnPath
walkPaths curves start distances =
  NEA.cons' start $ Array.catMaybes $ Array.scanl step (Just start) distances
  where
  step Nothing _ = Nothing
  step (Just whence) distance = closestPoint
    where
    matches :: Array OnPath
    matches = join $ curves <#>: \i curve ->
      Bezier.intersectCirclePrec 0.05 { p: whence.at, r: distance } curve <#> \{ p, t } ->
        let
          tangent = Bezier.evalB (deriv curve) t
          to = sgn (dot tangent whence.to) .* tangent
          delta = whence.at -<> p
        in { at: p, to, t, i, delta, distance: norm delta, curvature: Bezier.curvatureAt curve t }
    rightDirection = Array.filter (\{ delta } -> dot whence.to delta > 0.0) matches
    closestSegments = rightDirection #.. \r -> Just (MinWith (abs (r.i - whence.i)) (NEA.singleton r))
    composite { t, i } = t + Int.toNumber i
    closestPoint = minimumWith (\r -> Math.abs (composite r - composite whence))
      $ asArray $ closestSegments #.. (extract >>> NEA.toArray)


dilate :: B32 -> Pair B32
dilate curve@(B3 p0 _ _ p3) | curve == Bezier.castUp (Bezier.castUp (B1 p0 p3)) =
  let
    delta = 8.0
    d = normalize $ p0 -<> p3
    ninety = rotl2 (90.0 * d2r)
    cross = delta .* (ninety $* d)
  in Pair (LTF cross $* curve) (LTF (inv cross) $* curve)
dilate curve =
  let
    delta = 8.0
    outer c@(B3 p0 p1 p2 p3) which =
      let
        d0 = normalize $ p0 -<> p1
        d1 = normalize $ p2 -<> p3
        ninety = rotl2 (which * 90.0 * d2r)
        q0 = p0 <> delta .* (ninety $* d0)
        q3 = p3 <> delta .* (ninety $* d1)
        k0 = Bezier.curvatureAt c 0.0
        k1 = Bezier.curvatureAt c 1.0
        results = Bezier.fit
          { p0: q0, p1: q3, d0, d1
          , k0: 1.0 / ((1.0 / k0) + which * sgn k0 * delta)
          , k1: 1.0 / ((1.0 / k1) + which * sgn k1 * delta)
          }
        expected t = Bezier.evalB c t <> delta .*
          (rotl2 (which * 90.0 * d2r) $* Bezier.evalB (deriv c) t)
        score c2 = sum $ ((_ / 12.0) <<< Int.toNumber <$> Array.range 0 12) <#>
          \t -> norm2 (expected t <>- Bezier.evalB c2 t)
        scored = (Tuple <*> score) <$> results
      in fromMaybe c $ fst <$> minimumWith snd scored
  in outer curve <$> Pair one (negate one)

separateRoutes :: forall m r. MonadState { hitmap :: HitMap | r } m => Array Canonized -> m (Array (Array Canonized))
separateRoutes =
  let
    test running segment =
      map (un Conj) $ un App $ foldMap (\other -> App $ Conj <<< not <$> intersects (Pair other segment)) running
    addSegment (running :| rest) segment =
      test running segment <#> if _
        then segment : running :| rest
        else pure segment :| running : rest
  in Array.foldM addSegment (Nil :| Nil) >==
    (Array.fromFoldable >>> Array.reverse >== Array.fromFoldable >>> Array.reverse)



routesToPaths :: forall m r. MonadState { hitmap :: HitMap | r } m => Array Canonized -> m (Array String)
routesToPaths originalSegments = do
  separated <- separateRoutes originalSegments
  pure $ separated <#?> NEA.fromArray <#>
    map canonCurve >>> bezsToPath

bezsToPath :: NonEmptyArray B32 -> String
bezsToPath segments =
  let
    pt (V2 x y) = show x <> "," <> show y
    seg prev (B3 p0 p1 p2 p3) = { accum: p3, value: _ } $ fold
      [ if p0 /= prev then "M" <> pt p0 else ""
      , "C" <> intercalateMap " " pt [ p1, p2, p3 ]
      ]
  in intercalate " " $ _.value
    $ mapAccumL seg (NEA.head segments # \(B3 p0 _ _ _) -> p0 <>- V2 one one)
    $ segments

type OnRoute =
  { segment :: Canonized
  , i :: Int
  , t :: Number
  , at :: V2
  , to :: V2
  , curve :: B32
  , curvature :: Number
  }

routeAtTime :: Route -> Number -> Maybe OnRoute
routeAtTime (Route { segments, pathlength }) ofRoute = do
  let alongRoute = pathlength * ofRoute
  { i, pathlength: Pair segmentStart _, segment: segment@(Pair { canon } _) } <- segments
    # Array.find \{ pathlength: Pair _ segmentEnd } -> segmentEnd >= alongRoute
  let leftover = alongRoute - segmentStart
  bookends <- canon.samples # pairs
    # Array.find \(Pair _ { pathlength: Pair _ endsUp }) -> endsUp >= leftover
  case bookends of
    Pair { t: t0, pathlength: Pair l0 _ } { t: t1, pathlength: Pair _ l1 } -> do
      let
        curve = canonCurve segment
        t = bounds2bounds1 (coerce { min: l0, max: l1 }) (coerce { min: t0, max: t1 }) $. leftover
        at = Bezier.evalB curve t
        to = normalize $ Bezier.evalB (deriv curve) t
        curvature = Bezier.curvatureAt curve t
      Just { at, to, curvature, curve, t, i, segment }

-- walkRoute :: Route -> Either Number OnRoute

