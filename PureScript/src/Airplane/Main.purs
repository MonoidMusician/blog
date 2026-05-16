module Airplane.Main where

import Riverdragon.Dragon.Nest

import Airplane.Math (_sampleLinearlyX, sampleAirfoil, sampleRadially, sliceShapeX)
import Control.Extend (extend)
import Control.Monad.Writer (tell)
import Data.Argonaut as Json
import Data.Array as A
import Data.Array as Array
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NEA
import Data.Distributive (collect, distribute)
import Data.Functor.Compose (Compose(..))
import Data.Int as Int
import Data.List (foldl)
import Data.List as List
import Data.Map as Map
import Data.Newtype (over, under)
import Data.Number as Math
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Set as Set
import Data.String as String
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Debug (spy)
import Effect.Class.Console as Console
import Fetch (fetch)
import Idiolect (incorporate, indices, sqre, withIndices, (-$), (<#?))
import Math.Bezier (atX, bboxBs, castUp, extremaX, radians2slope)
import Math.Matrix (Afn1(..), B12, BBox2, Bez1(..), Bounds, Degrees, LTF(..), V1, V2, V3, Vec1(..), Vec2(..), Vec3(..), bounds2bez, bounds2bounds1, boundsNorm, boundsWithout, center, degreesAround2, extent, getBounds, interp, interpS, interpsWith, line2line2, ltf, midpoint, mkBound, multiinterpWith, overBounds, padBounds, pairsWith, pointsBetween, pointsBounds, tf, tfI, tfR, tfS, triinterp, triinterpWith, uninterp, v2X, v2Y, ($*), ($.), (+<), (.+<), (<.), (>+), (>+.))
import Math.Mesh (mkMesh)
import Math.Mesh as Mesh
import Math.SVG (SVGNode, printPathData)
import Math.SVG as SSVG
import Math.SVG as SVG
import Monoids.BoundsWith (BoundsWith(..), MaxWith(..), MinWith(..))
import Parser.Optimized.Types (unsafeFromJust)
import Record as Record
import Riverdragon.Dragon as Dragon
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Nest as River
import Riverdragon.Dragon.Wings (liveArray)
import Riverdragon.Dragon.Wings as Wings
import Riverdragon.River (mapArray)
import Riverdragon.River.Bed as Bed
import Riverdragon.River.Beyond (affToLake)
import Riverdragon.River.Beyond as Beyond
import Runtime as Runtime
import Widget (Widget)

-- https://localhost:6789/airplane.html?live

viewBounds :: BBox2 Number -> Dragon.AttrProp
viewBounds bb = D.viewBox case bb of
  V2
    { min: Min xmin, max: Max xmax }
    { min: Min ymin, max: Max ymax } ->
      [ xmin, ymin, xmax - xmin, ymax - ymin ]

unitX :: B12
unitX = B1 (V2 zero zero) (V2 one zero)

widget :: Widget
widget _ = do
  contents <- pure $ map (SVG.parseSVG <$> _) $ affToLake $
    _.text =<< fetch "assets/misc/AirplaneDetailed.svg" {}
  pure $ contents >@ case _ of
    Nothing -> D.text "Loading…"
    Just Nothing -> D.text "Failed!"
    Just (Just svg) -> D.Egg do
      let
        names = ["s9037", "cr001sm", "sokolov", "wb13535", "ag24"]
        sampleAt =
          { radius: [30.0, 45.0, 60.0, 75.0, 90.0, 105.0, 120.0, 130.0]
          , chord: [0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.10, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.85, 1.0]
          }
        -- airfoils :: Map String { airfoil :: SVGNode, curves :: NonEmptyArray _, samples :: Array _, bb :: _, angles :: Set Degrees, chord :: B12 }
        airfoils = Map.fromFoldable $ names <#?> \name -> Tuple name <$> do
          airfoil <- Array.head $ SVG.findInkscapeLabelPath ["Infos", name] svg
          curves <- NEA.fromArray $ SVG.getCTMBez airfoil
          let
            samples = sampleAirfoil sampleAt curves
            samplePoints = samples <#> _.p
            bb = bboxBs curves
            angles = Set.fromFoldable $ samples <#?> case _ of
              { pos: Left angle } -> Just angle
              _ -> Nothing
            chord = extremaX curves
            toNorm = chord `line2line2` unitX
            fromNorm = tfR toNorm
            normed = LTF toNorm $* chord
            symmetric = samplePoints <#> \(V2 x y) ->
              V2 x $ y -$ midpoint (fromMaybe (mkBound y) $ foldMap atX curves x) - maybe 0.0 midpoint (atX chord x)
          pure { airfoil, curves, samples, samplePoints, symmetric, bb, angles, chord, toNorm, fromNorm, normed }
        airfoilR =
          let find k = unsafeFromJust k $ Map.lookup k airfoils in
          { sokolov: find "sokolov"
          , ag24: find "ag24"
          , cr001sm: find "cr001sm"
          , s9037: find "s9037"
          , wb13535: find "wb13535"
          }
        allAngles = foldl Set.intersection (Set.fromFoldable sampleAt.radius) (airfoils <#> _.angles)
        filterAngles samples = samples <#? case _ of
          { pos: Left angle } | angle `not Set.member` allAngles -> false
          _ -> true
        planform = do
          let
            node = unsafeFromJust "planform missing" $ Array.head $ SVG.findInkscapeLabelPath ["Top View", "wing top L", "planform"] svg
            curve = unsafeFromJust "planform empty" $ NEA.fromArray $ SVG.getCTMBez node
            bb = bboxBs curve
            xs = sliceShapeX
              { angles: [150.0, 140.0, 130.0, 120.0, 110.0, 100.0, 90.0, 80.0, 70.0, 60.0, 0.0]
              , samples: (pointsBetween 0.0 0.075 10 <> pointsBetween 0.0 0.15 10) <#> tfS (boundsNorm `bounds2bounds1` v2X bb)
              , spacing: 2.5 -- 6.5
              } (NEA.toArray curve)
          { curve
          , bb
          , xs
          , samples: Map.toUnfoldable (unwrap xs)
          , flatSamples: Map.toUnfoldable (unwrap xs) >>= \(Tuple x ys) -> V2 x <$> Array.fromFoldable (bounds2bez ys)
          , x2norm: under V1 (v2X bb `bounds2bounds1` boundsNorm $* _)
          }
        gullWing = unsafeFromJust "gull wing missing" $ NEA.fromArray <<< SVG.getCTMBez =<< Array.head do SVG.findInkscapeLabelPath ["Rear View", "wing rear L", "gull wing"] svg
        withNonEmpty :: forall v r. Array v -> (NonEmptyArray v -> Maybe (v -> r)) -> Array r
        withNonEmpty vs f = case NEA.fromArray vs of
          Just vs' | Just f' <- f vs' -> vs <#> f'
          _ -> []

        foilAtX :: Number -> Array V2
        foilAtX = planform.x2norm >>> foils
          where
          foils = triinterpWith (interpsWith interp) $ unsafeFromJust "" $ NEA.fromArray $
            [ { t: 0.0, r: 0.00, p: airfoilR.cr001sm { samplePoints = airfoilR.cr001sm.symmetric } }
            , { t: 0.1, r: 0.50, p: airfoilR.cr001sm }
            , { t: 0.6, r: 0.20, p: airfoilR.sokolov }
            , { t: 1.0, r: 0.00, p: airfoilR.wb13535 }
            ] <#> \r@{ p: { samplePoints, chord } } -> r { p = (chord `line2line2` unitX $* _) <$> samplePoints }

        twist = triinterpWith interpS $ unsafeFromJust "" $ NEA.fromArray $
          [ { t: 0.00, r: 0.00,    p: -10.0 * 0.3 }
          , { t: 0.03, r: 2.0/3.0, p:   0.0 * 0.3 }
          , { t: 0.10, r: 5.0/7.0, p:   5.0 * 0.3 }
          , { t: 0.60, r: 0.25,    p:   5.0 * 0.3 }
          , { t: 0.80, r: 0.25,    p:   7.5 * 0.3 }
          , { t: 1.00, r: 0.00,    p:  10.0 * 0.3 }
          ]
        wingByX :: Tuple Number (Bounds Number) -> Array V3
        wingByX (Tuple x hintY) = withNonEmpty (foilAtX x) \foil -> do
          ys <- pure $ incorporate hintY $ foldMap atX planform.curve x
          zs <- foldMap atX gullWing x
          let
            fitY = foldMap1 (mkBound <<< v2X) foil `bounds2bounds1` ys
            fitZ = foldMap1 (mkBound <<< v2Y) foil `bounds2bounds1` zs

            applyTwist = tf $ twist (planform.x2norm x) `degreesAround2` V2 0.3 0.0
          pure $ applyTwist >>> \(V2 y z) ->
            V3 x (fitY $. y) (fitZ $. z)
        wingMesh = Mesh.mkMesh
          (Array.reverse planform.samples)
          (indices airfoilR.sokolov.samples)
          (wingByX >>> Array.index)
        toOBJ (V3 x y z) = V3 x (-z) y
        wingMeshCentered = fromMaybe wingMesh do
          points <- NEA.fromArray $ Mesh.meshPoints wingMesh
          let
            bounds = getBounds points
            recentering :: V3
            recentering = tfR $ center bounds
          pure $ Mesh.transform recentering wingMesh <#> map toOBJ

        hstab =
          let
            node = SVG.xpath (SVG.XPath ".//*[local-name()='path']") =<< SVG.findInkscapeLabelPath ["Top View", "hstab top L"] svg
            curve = unsafeFromJust "hstab empty" $ NEA.fromArray $ SVG.getCTMBez =<< node
            bbp@(V2 { min: Min minx, max: Max maxx } _) = bboxBs curve
            samples = Map.toUnfoldable $ unwrap $ sliceShapeX
              { angles: []
              , samples: (pointsBetween 0.0 0.05 10 <> pointsBetween 0.0 0.1 10) <#> tfS (boundsNorm `bounds2bounds1` v2X bbp)
              , spacing: 2.0
              } (NEA.toArray curve)
            foil@{ bb, fromNorm, toNorm } = airfoilR.ag24
            opp n = one - n
            mesh = Mesh.mkMesh samples (indices foil.samplePoints)
              \(Tuple x { min: Min leading }) -> Array.(!!)
                let
                  i = uninterp maxx minx x
                  lift = -10.0 * opp (Math.pow (opp i) (1.0/1.7))
                  thisFoil = tailFoil $ Math.pow i 1.8
                  V2 foilBBX foilBBY = getBounds $ unsafeFromJust "" $ NEA.fromArray thisFoil

                  angle = 90.0 - interpS 88.3 45.0 i :: Degrees
                  rot = degreesAround2 angle (V2 x leading)
                  form = incorporate (mkBound leading) $ foldMap (atX <<< ltf rot) curve x
                  _ = spy "asdf" { angle, rot, form }

                  fitY@(Afn1 s _) = foilBBX `bounds2bounds1` form
                  oversize = 1.3
                  fitZ = Afn1 one lift <. Afn1 (negate oversize * s) zero <. Afn1 one (negate (midpoint foilBBY))
                in thisFoil <#> \(V2 y z) ->
                  let V2 x' y' = tfR rot $* V2 x (fitY $. y)
                  in V3 x' y' (fitZ $. z)

            tailFoil i = interpsWith interp foil.samplePoints foil.symmetric i <#> dimap (tf toNorm) (tf fromNorm) \(V2 x y) ->
              let x' = x .+< i >+. (0.55 * do opp $ sqre $ opp x) in V2 x' y
          in { tailFoil, bb, curves: foil.curves, mesh: _ } $ fromMaybe mesh do
            points <- NEA.fromArray $ Mesh.meshPoints mesh
            let
              bounds = getBounds points
              recentering :: V3
              recentering = tfR $ center bounds
            pure $ Mesh.transform recentering mesh <#> map toOBJ
        objFile = Mesh.objFile do
          tell $ tripleQuoted """
            # Aspect of Flight
            mtllib 3d.mtl
          """ <> "\n"
          -- tell $ tripleQuoted """
          --   o wing L
          --   s 1
          --   usemtl Gloss
          -- """ <> "\n"
          -- Mesh.objMesh $ wingMeshCentered
          tell $ tripleQuoted """
            o wing lines L
            s 1
            usemtl Gloss
          """ <> "\n"
          Mesh.objLines $ wingMeshCentered
          -- tell $ tripleQuoted """
          --   o hstab lines L
          --   s 1
          --   usemtl Gloss
          -- """ <> "\n"
          -- -- Mesh.objMesh hstab.mesh
          -- Mesh.objLines hstab.mesh
        twistEx =
          let
            bbX = v2X planform.bb
            bbY = v2Y planform.bb
            mkY v = midpoint bbY + (v / 20.0) * extent bbY
          in 400 # pointsBounds bbX # map
            \x -> V2 x $ mkY $ twist (planform.x2norm x)
      trackI <- River.createStore 0.0
      trickI <- River.createStore 0.0
      liftEffect do
        (Runtime.aSideChannel Proxy "render3d").messageInABottle { element: 0, mesh: objFile }
      pure $ fold
        [ D.svg [ viewBounds =:= padBounds 0.5 <$> planform.bb ] $ fold
            [ planform.flatSamples #.. \(V2 cx cy) ->
                D.svg_"circle" [ D.attr "cx" =:= cx, D.attr "cy" =:= cy, D.attr "r" =:= 0.3 ] mempty
            , D.path [ D.d =:= printPathData (NEA.toArray planform.curve), D.attr "opacity" =:= 0.15 ]
            , D.path [ D.d =:= printPathData (castUp <<< castUp <$> pairsWith B1 twistEx), D.style =:= "fill: none; stroke: green; stroke-width: 0.2px;" ]
            ]
        , D.input
          [ D.attr "type" =:= "range", D.attr "min" =:= 0.0, D.attr "max" =:= 1.0, D.attr "step" =:= "any", D.attr "value" =:= 0.0
          , D.style =:= "width: 99%; margin: auto; display: block"
          , D.onInputNumber =:= \v -> do
              trackI.send v
          ]
        , fromMaybe (D.text "Could not find everything") do
            sokolov <- Map.lookup "sokolov" airfoils
            wb13535 <- Map.lookup "wb13535" airfoils
            let
              reposition = wb13535.chord `line2line2` sokolov.chord
              bb = sokolov.bb `lift2 (<>)` bboxBs ((LTF reposition $* _) <$> wb13535.curves)
              sok = unitX `line2line2` sokolov.chord
              intoPlanform = under V1 (boundsNorm `bounds2bounds1` v2X planform.bb $* _)
              interp :: Number -> Array V2
              interp i = (sok $* _) <$> foilAtX (intoPlanform i)
            pure $ D.svg [ viewBounds =:= padBounds 0.2 <$> bb ] $ fold
              [ liveArray (trackI.stream <#> interp) \_ -> distribute >>> \(V2 cx cy) ->
                  D.svg_"circle" [ D.attr "cx" <:> cx, D.attr "cy" <:> cy, D.attr "r" =:= 0.1 ] mempty
              , D.path [ D.d =:= printPathData (NEA.toArray sokolov.curves), D.attr "opacity" =:= 0.15 ]
              , D.path [ D.d =:= printPathData (NEA.toArray $ (LTF reposition $* _) <$> wb13535.curves), D.attr "opacity" =:= 0.15 ]
              ]
        , D.input
          [ D.attr "type" =:= "range", D.attr "min" =:= 0.0, D.attr "max" =:= 1.0, D.attr "step" =:= "any", D.attr "value" =:= 0.0
          , D.style =:= "width: 99%; margin: auto; display: block"
          , D.onInputNumber =:= \v -> do
              trickI.send v
          ]
        , D.svg [ viewBounds =:= padBounds 1.0 <$> hstab.bb ] $ fold
            [ liveArray (trickI.stream <#> hstab.tailFoil) \_ -> distribute >>> \(V2 cx cy) ->
                D.svg_"circle" [ D.attr "cx" <:> cx, D.attr "cy" <:> cy, D.attr "r" =:= 0.1 ] mempty
            , D.path [ D.d =:= printPathData (NEA.toArray hstab.curves), D.attr "opacity" =:= 0.15 ]
            ]
        -- , D.pre.$ D.code.$$
        , fromMaybe (D.text "Could not find everything") $
          airfoils #:.. \name { airfoil, samples: rawSamples, bb, angles, curves } -> do
            let samples = filterAngles rawSamples
            pure $ fold
              [ D.html_"h3".$$ name <> " (" <> show (Array.length samples) <> ")"
              , D.code.$ D.show $
                _sampleLinearlyX [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0] curves <#> boundsWithout >>> extent
              , D.svg [ viewBounds =:= padBounds 0.2 <$> bb ] $ fold
                  [ samples #.. \{ p: V2 cx cy, t } ->
                      D.svg_"circle" [ D.attr "cx" =:= cx, D.attr "cy" =:= cy, D.attr "r" =:= 0.1, D.data_"t" =:= t ] mempty
                  , D.path [ D.d =:= printPathData (NEA.toArray curves), D.attr "opacity" =:= 0.15 ]
                  ]
              ]
        ]

