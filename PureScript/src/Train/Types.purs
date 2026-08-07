module Train.Types where

import Prelude

import Control.Monad.Writer (class MonadWriter, tell)
import Data.Array as Array
import Data.Functor.App (App)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Pair (Pair(..))
import Data.Semigroup.Last (Last)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))
import Idiolect (withIndices)
import Math.Matrix (Afn2, B32, BBox2, Bez3(..), Bounds, LTF(..), V2, Vec2(..), normalize, tf, tfBounds, unAfn2, ($*), (<>-))
import Uncurried.RWSE (RWSE)



-- | A standard prototypical curve with cached information about it.
newtype Standard = Standard
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
derive instance Newtype Standard _


-- | A standard segment placed at a location, recorded as a pair so the reverse
-- | path is available without a library lookup.
type Canonized = Pair
  { canon :: Standard
  , pos :: Pair Pos
  , transform :: Afn2 Number
  , transformI :: Afn2 Int -- FIXME
  }


-- | A route consists of contiguous segments.
newtype Route = Route
  { segments :: Array { segment :: Canonized, i :: Int, pathlength :: Pair Number }
  , pathlength :: Number
  -- Information about loops, crossings, signals maybe
  -- Time
  }
mkRoute :: Array Canonized -> Route
mkRoute segments =
  let
    mapper l0 (Tuple i segment@(Pair { canon: Standard canon } _)) =
      let l1 = l0 + canon.pathlength in
      { accum: l1, value: { segment, i, pathlength: Pair l0 l1 } }
    { accum: pathlength, value: segments } = mapAccumL mapper 0.0 $ withIndices segments
  in Route { pathlength, segments }

-- Get the plain Bezier for the segment
canonCurve :: Canonized -> B32
canonCurve (Pair p _) = tf (LTF p.transform) (unwrap p.canon).curve

-- Get the bounding box for the segment
canonStrokeBox :: Canonized -> BBox2 Number
canonStrokeBox (Pair { canon: Standard { bbox: { stroke } }, transform } _) =
  tfBounds transform stroke


-- | A particular point on the path.
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



type PointOnRoute =
  { segment :: Canonized
  , i :: Int
  , t :: Number
  , at :: V2
  , to :: V2
  , curve :: B32
  , curvature :: Number
  }



-- | A position on the grid is an integer point and a rational direction,
-- | in lowest terms: this gives the straight line segment it should follow.
type Pos = { at :: Vec2 Int, to :: Vec2 Int }


-- | The monad for interpreting a Traintle program.
type TraintleM = RWSE
  { origin :: Pos
  , mode :: TrainMode
  }
  -- Write out bounds
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
  , route :: Array Canonized
  , routes :: Map String Route
  , radii :: Pair Int
  }
  String

data TrainMode = Drawing | Routing String
derive instance Eq TrainMode

-- | The hit map is a cache of transformations and standard curve IDs, recording
-- | whether the curves intersect in that relation to each other.
type HitMap = Map (Afn2 Number) (Map (Pair Int) Boolean)

-- | Persistent state that is maintained in between runs. The hitmap requires
-- | the library to be persistent, or IDs could be lost.
type InterState =
  { library :: Map Int Standard
  , hitmap :: HitMap
  }


-- | The commands parsed by the language.
data Command
  -- Turtle drawing commands.
  = Q | W | E | A | S | D | Z | X | C
  -- Change the pair of radii used for a/d versus q/e
  | SetRadius Int
  -- Run the commands silently: track the position changes but discard the drawing
  | Silent (Array Command)
  -- Variable
  | SetVariable (Maybe Int) String
  | GetVariable (Maybe Int) String
  -- Define or call a subroutine
  | Subroutine String (Maybe (Array Command))
  -- Teleport by literal coordinates, or to the origin
  | Teleport (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) | Origin
  -- Make a train on a route
  | TrainRoute String (Array TrainUnit) (Array Command)
derive instance Eq Command
derive instance Ord Command

derive instance Generic Command _
instance Show Command where show c = genericShow c



data Direction = Forward | Backward | Bothward
derive instance Eq Direction
derive instance Ord Direction
derive instance Generic Direction _
instance Show Direction where show = genericShow

-- | A train unit: a locomotive (length and direction) or a car (length and string)
data TrainUnit = Locomotive Direction Int | Car String Int
derive instance Eq TrainUnit
derive instance Ord TrainUnit
derive instance Generic TrainUnit _
instance Show TrainUnit where show = genericShow



-- | Tell a record with anonymous update notation: `tellR _ { field = add }`.
tellR :: forall w m. MonadWriter w m => (w -> w) -> m Unit
tellR f = tell $ f (mempty :: w)



-- | Transform the position, rotating the direction vector (not translating it).
tfPos :: Afn2 Int -> Pos -> Pos
tfPos t { at, to } = { at: t $* at, to: r $* to }
  where Tuple _p r = unAfn2 t
