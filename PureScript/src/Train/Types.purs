module Train.Types where

import Prelude

import Control.Monad.Writer (class MonadWriter, tell)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Functor.App (App)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Min (Min)
import Data.Pair (Pair(..))
import Data.Semigroup.First (First)
import Data.Semigroup.Last (Last)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Math.Matrix (Afn2, B32, BBox2, Bez3(..), Bounds, LTF(..), V2, Vec2, inv, normalize, tf, tfBounds, unAfn2, ($*), (<>-))
import Uncurried.RWSE (RWSE)


-- | A position on the grid is an integer point and a rational direction,
-- | in lowest terms: this gives the straight line segment it should follow.
type Pos = { at :: Vec2 Int, to :: Vec2 Int }


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
  { radius :: Int
  , canon :: Standard
  , pos :: Pair Pos
  , transform :: Afn2 Number
  , transformI :: Afn2 Int -- FIXME
  }

-- Get the plain Bezier for the segment
canonCurve :: Canonized -> B32
canonCurve (Pair p _) = tf (LTF p.transform) (unwrap p.canon).curve

-- Get the bounding box for the segment
canonStrokeBox :: Canonized -> BBox2 Number
canonStrokeBox (Pair { canon: Standard { bbox: { stroke } }, transform } _) =
  tfBounds transform stroke



-- | A route consists of contiguous segments.
newtype Route = Route
  { segments :: NonEmptyArray { segment :: Canonized, i :: Int, pathlength :: Pair Number }
  , curves :: NonEmptyArray B32
  , pathlength :: Number
  -- Information about loops, crossings, signals maybe

  , isLoop :: Boolean
  , crossings ::
      Map (Pair Int)
      { pathlength :: Number }
  , maxlength :: Number
  -- Time
  }


newtype RoutedTrain = RoutedTrain
  { route :: Route
  , consist ::
      -- Distances
      Array Number
  , endpoints ::
      -- Buffer amounts, computed by placing the consist at the start and end
      { start :: Number
      , end :: Number
      , buffer :: Number -- max of start and end
      }
  }


-- | A particular point along a route.
type PointOnRoute =
  { i :: Int              -- index in route
  , segment :: Canonized  -- the segment it lies on
  , curve :: B32          -- the segment Bezier
  , t :: Number           -- time within segment
  , at :: V2              -- precise point, curve@[t]
  , to :: V2              -- tangent
  , curvature :: Number   -- curvature at the point
  , pathlength :: Number  -- pathlength from start of route
  }

routeStart :: Direction -> Route -> PointOnRoute
routeStart dir (Route r) = case NEA.head r.segments of
  { segment, pathlength: Pair pathlength _ } | curve@(B3 p0 p1 _ _) <- canonCurve segment ->
    { at: p0, to: applyDir $ normalize $ p1 <>- p0, t: 0.0, curve, segment, pathlength, i: 0, curvature: 0.0 }
  where
  applyDir = case dir of
    Backward -> inv
    _ -> identity

routeEnd :: Direction -> Route -> PointOnRoute
routeEnd dir (Route r) = case NEA.last r.segments of
  { segment, pathlength: Pair _ pathlength } | curve@(B3 _ _ p2 p3) <- canonCurve segment ->
    { at: p3, to: applyDir $ normalize $ p2 <>- p3, t: 1.0, curve, segment, pathlength, i: NEA.length r.segments - 1, curvature: 0.0 }
  where
  applyDir = case dir of
    Forward -> inv
    _ -> identity



type Layout =
  -- Array of individual segments as they were drawn (deduplicated)
  { array      :: Array Canonized
  -- Map of all segment connections
  , segments   :: Map Pos (Map Pos (First Int))
  --------------------------------------------
  -- Map of all straight runs of segments (only starting positions are included)
  , straights  :: Map Pos { segments :: NonEmptyArray Int, end :: Pos }
  -- Switches/turnouts, organized by origin and radius (positive = right, negative = left),
  -- combined with information on the following straight
  , switches   :: Map Pos (Map Int { step :: Pos, segments :: NonEmptyArray Int, end :: Pos })
  -- Isolated loops, with a chosen basepoint and a set of all reached positions
  , loops      :: Map Pos { chosen :: Min Pos, positions :: Set Pos }
  -- Connected components (undirected)
  , components :: Map Pos { chosen :: Min Pos, positions :: Set Pos }
  -- Tell what feature each directed position is associated with:
  -- a loop, a switch, or a straight run (which provides its start and end)
  , feature    :: Map Pos (First Feature)
  -- The complete logical layout
  , logical    :: Map Pos (Map Pos (Map Int (Set (NonEmptyArray Int))))
  --------------------------
  , physical   :: Map Pos Pos
  , clusters   :: Set (Set Pos)
  , crossings  :: Set (Set Pos)
  }

data Feature
  = FeatLoop { chosen :: Min Pos, positions :: Set Pos }
  | FeatEndpoints Pos Pos
  | FeatSwitch (Map Int { step :: Pos, segments :: NonEmptyArray Int, end :: Pos })




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
