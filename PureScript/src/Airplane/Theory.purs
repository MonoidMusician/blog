module Airplane.Math where

import Math.Matrix
import Prelude

import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (foldMap)
import Data.HeytingAlgebra (tt)
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Idiolect (only)
import Math.Bezier (evalB)
import Math.Poly (solve2)
import Partial.Unsafe (unsafePartial)

-- airfoil

type Sample p = { t :: Number, p :: p }

preferLSamples :: forall p. Array (Sample p) -> Array (Sample p) -> Array (Sample p)
preferLSamples ls rs = ls <> filter notWithinLs rs
  where
  notWithinLs = case foldMap (Just <<< mkBound <<< _.t) ls of
    Just { min: Min lmin, max: Max lmax } ->
      \{ t: rt } -> lmin <= rt && rt <= lmax
    Nothing -> tt

preferRSamples :: forall p. Array (Sample p) -> Array (Sample p) -> Array (Sample p)
preferRSamples ls rs = filter notWithinRs ls <> rs
  where
  notWithinRs = case foldMap (Just <<< mkBound <<< _.t) rs of
    Just { min: Min rmin, max: Max rmax } ->
      \{ t: lt } -> rmin <= lt && lt <= rmax
    Nothing -> tt

-- sampleRadially :: forall p. Array Degrees -> (Sample p -> Boolean) -> Bez3 p -> Array (Sample p)
-- sampleRadially angles predicate curve
--   | curveAt <- evalB curve
--   , poly <- bez2poly curve =
--   angles <#> \angle ->
--     Array.fromFoldable (solve2 poly)
--       # map \t -> { t, p: curveAt t }
--       # Array.filter (predicate && \{ t } -> 0 <= t && t <= 1)
--       # only
--       # unsafePartial fromJust

