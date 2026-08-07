module Train.Logic where

import Prelude
import Train.Types

import Control.Alternative (guard)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.ResourceM (inSubScope, selfDestructor)
import Control.Monad.ResourceT (ResourceM)
import Control.Monad.State (class MonadState, execState, get, gets, modify_)
import Control.Monad.Writer (class MonadWriter, censor, tell)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode (isAsciiUpper, isDecDigit)
import Data.DateTime.Instant (unInstant)
import Data.Distributive (collect)
import Data.Either (Either(..), either)
import Data.Filterable (filter, separate)
import Data.Foldable (all, any, findMap, fold, foldMap, for_, intercalate, sum, traverse_)
import Data.Foldable as F
import Data.Functor.App (App(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Lazy (Lazy, defer, force)
import Data.Lens ((%=), (.~))
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Monoid (power)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.NonEmpty ((:|))
import Data.Number as Math
import Data.Optical ((@<>), (@=), (@~))
import Data.Ord (abs)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice ((|||))
import Data.Semigroup.Foldable (fold1)
import Data.Semigroup.Last (Last(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (mapAccumL, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Idiolect (incorporate, intercalateMap, minimumWith, neighbors, sgn, sqre, withIndices, (#..), (#:..), (#<>), (..$), (<#?>), (<>$), (>==))
import Math.Bezier as Bezier
import Math.Matrix (Afn2, B32, BBox2, Bez1(..), Bez3(..), Bounds, LTF(..), Lin2(..), V2, Vec2(..), bounds2bez, bounds2bounds1, bounds2bounds2, clampBounds, d2r, disjointBounds, dot, inv, mkAfn2, mkBound, mkBounds, norm, norm2, normalize, overBounds, padBounds, pairs, r2d, rotl2, tf, tfBounds, tfI, unAfn2, ($*), ($.), (-<>), (.*), (<.), (<>+), (<>-), (<^))
import Math.Poly (deriv)
import Prim.Row as Row
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones ((.$), (.$~~), (<:>), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (deletable, liveArray, sourceCode)
import Riverdragon.River (Course(..), Lake, River, coursing, createRiver, createRiverStore, dam, makeLake, memoize, statefulStream, store, store')
import Riverdragon.River as River
import Riverdragon.River.Bed (freshId)
import Riverdragon.River.Beyond (dedup, documentEvent, everyFrame, instanced, withLast)
import Riverdragon.River.Streamline (clientRect)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Uncurried.RWSE (RWSE, runRWSE)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent as MouseEvent
import Widget (Widget, autoAdaptInterface, valueInterface)
