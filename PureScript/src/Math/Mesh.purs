module Math.Mesh where

import Prelude

import Control.Comonad (extract)
import Control.Monad.State.Class (get, gets, put)
import Control.Monad.Writer.Class (tell)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Functor.Day (Day, day, runDay)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number.Format as Format
import Data.Traversable (intercalate, traverse, traverse_)
import Idiolect ((>==))
import Math.Matrix (class Transforms, V3, Vec3, pairs, tf)
import Uncurried.RWSE (RWSE, evalRWSE)

type Mesh = Day Array Array (Maybe V3)

mkMesh :: forall x y. Array x -> Array y -> (x -> y -> Maybe V3) -> Mesh
mkMesh xs ys f = day identity (f <$> xs) ys

meshPoints :: Mesh -> Array V3
meshPoints = runDay \f0 xs ys -> do
  x <- xs
  let f1 = f0 x
  y <- ys
  Array.fromFoldable $ f1 y

transform :: forall @t. Transforms t V3 => t -> Mesh -> Mesh
transform t = map $ map (tf t)

-- Generate faces from a mesh (triangles and quads)
facesOfMesh :: Mesh -> Array (NonEmptyArray V3)
facesOfMesh = runDay \f0 xs ys -> Array.catMaybes ado
  xx <- pairs $ f0 <$> xs
  yy <- pairs ys
  in ensureFace $ Array.fromFoldable xx <*> Array.fromFoldable yy
  where
  ensureFace = Array.catMaybes >>> Array.nub >>>
    case _ of
      [v1, v2, v3, v4] -> Just $ NEA.cons' v1 [v2, v4, v3]
      verts | Array.length verts > 2 -> NEA.fromArray verts
      _ -> Nothing

linesOfMesh :: Mesh -> Array (NonEmptyArray V3)
linesOfMesh = runDay \f0 xs ys ->
  let
    fs = f0 <$> xs
    horiz :: Array (Array (Maybe V3))
    horiz = fs <#> (_ <$> ys)
    verti :: Array (Array (Maybe V3))
    verti = (fs <@> _) <$> ys
  in (horiz <> verti)
    >>= Array.catMaybes
    >>> Array.groupBy eq >>> map NEA.head
    >>> pairs
    >== NEA.fromFoldable1

type OBJ = RWSE Unit String (Map (Vec3 String) Int) Void

objFile :: OBJ Unit -> String
objFile = evalRWSE mempty Map.empty >>> extract

cmd :: String -> String -> OBJ Unit
cmd name dat = tell $ name <> " " <> dat <> "\n"

roundOff :: Number -> String
roundOff = Format.toStringWith (Format.fixed 3)

writeVert :: Vec3 Number -> OBJ (OBJ String)
writeVert = map roundOff >>> \coords -> do
  written <- get
  idx <- case Map.lookup coords written of
    Just idx -> pure idx
    Nothing -> do
      let idx = Map.size written
      cmd "v" $ intercalate " " coords
      idx <$ put (Map.insert coords idx written)
  pure do
    currentIdx <- gets Map.size
    pure $ show $ idx - currentIdx

writeFace :: NonEmptyArray (Vec3 Number) -> OBJ Unit
writeFace verts = do
  getVerts <- traverse writeVert verts
  cmd "f" =<< NEA.intercalate (pure " ") getVerts

objMesh :: Mesh -> OBJ Unit
objMesh = facesOfMesh >>> traverse_ writeFace

writeLine :: NonEmptyArray (Vec3 Number) -> OBJ Unit
writeLine verts = do
  getVerts <- traverse writeVert verts
  cmd "l" =<< NEA.intercalate (pure " ") getVerts

objLines :: Mesh -> OBJ Unit
objLines = linesOfMesh >>> traverse_ writeLine
