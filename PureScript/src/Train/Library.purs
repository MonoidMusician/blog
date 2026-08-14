module Train.Library where

import Prelude

import Control.Alternative (guard)
import Control.Monad.State (class MonadState, execState, get)
import Data.Array as Array
import Data.Foldable (findMap, for_)
import Data.Foldable as F
import Data.Int as Int
import Data.Lazy (Lazy, defer)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Optical (setProp)
import Data.Ord (abs)
import Data.Pair (Pair(..))
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))
import Math.Bezier as Bezier
import Math.Matrix (B32, Bez1(..), Bez3(..), Lin2(..), Vec2(..), mkAfn2, norm, tfI, ($*), (-<>), (.*), (<.), (<>-))
import Train.Geometry (dilatePath, rotations)
import Train.Types (Canonized, Pos, Standard(..), tfPos)

allRadii :: Array Int
allRadii = [ 7, 13, 21, 27, 41 ]

standardCurves :: Lazy (Map Int Standard)
standardCurves = defer \_ -> _.library $ flip execState { library: Map.empty } do
  _ <- standardize { from: V2 1 0, to: V2 2 1, delta: V2 5 1 } 13 curves."0 to 26"
  _ <- standardize { from: V2 2 1, to: V2 1 1, delta: V2 4 3 } 13 curves."26 to 45"
  _ <- standardize { from: V2 1 0, to: V2 1 1, delta: V2 5 2 } 7 curves."0 to 45"
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
    , "26 to 45": B3 mempty (V2 20.882696 10.44135) (V2 37.658719 21.65872) (V2 64.0 48.0)
    , "0 to 45": B3 mempty (V2 28.921554 0.0) (V2 57.180985 9.18099) (V2 80.0 32.0)
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
findCurve { pos: { at: start, to: heading }, radius } library = library # findMap \(Standard standard) -> do
  guard $ standard.radius == abs radius
  rotations # findMap \rot0 -> do
    let rot = rot0 <. if radius > 0 then tfI else Lin2 one zero zero (negate one)
    guard $ heading == (rot $* standard.key.from)
    let end = start <> standard.key.delta
    library # findMap \(Standard standard2) -> do
      rotations # findMap \rot2 -> do
        guard $ standard2.key == reverseKey standard.key rot2
        Just $ Pair
          (makeHere start rot (Standard standard))
          (makeHere end rot2 (Standard standard2))
  where
  makeHere :: Vec2 Int -> Lin2 Int -> Standard -> _
  makeHere here rot canon@(Standard { key }) =
    let
      transformI = mkAfn2 $ Tuple here rot
      transform = mkAfn2 $ Tuple (((16.0 * _) <<< Int.toNumber) <$> here) $ Int.toNumber <$> rot
      pos = tfPos transformI <$> Pair
        { at: mempty, to: key.from }
        { at: key.delta, to: key.to }
    in { radius, canon, pos, transformI, transform }


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
  case F.find (\(Standard r) -> r.key == key) library of
    Just r -> pure r
    Nothing -> do
      let
        id = Map.size library
        strokes = dilatePath curve
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
        r = Standard { key, id, radius, strength, curve, samples, strokes, bbox, pathlength, beeline }
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
