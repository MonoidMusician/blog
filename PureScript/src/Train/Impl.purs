module Train.Impl where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.State (get, gets, modify_)
import Control.Monad.Writer (censor)
import Data.Array as Array
import Data.Foldable (any, traverse_)
import Data.Functor.App (App(..))
import Data.Lens ((%=), (.~))
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Optical (setProp, (@<>), (@=), (@~))
import Data.Pair (Pair(..))
import Data.Semigroup.Last (Last(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Idiolect (intercalateMap, (..$))
import Math.Matrix (Vec2(..), inv, mkBound, (.*))
import Train.Library (allRadii, findCurve)
import Train.Types (Command(..), TrainMode(..), TraintleM, canonCurve, mkRoute, tellR)
import Type.Proxy (Proxy(..))


silent :: TraintleM ~> TraintleM
silent act = do
  { path, route } <- get
  r <- censor mempty act
  setProp @"path" path
  setProp @"route" route
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
renderCommand (TrainRoute name consist cmds) = do
  -- Enter routing mode
  local (prop (Proxy @"mode") .~ Routing name) do
    saved <- gets _.route
    modify_ _ { route = mempty }
    traverse_ renderCommand cmds
    generated <- gets _.route
    modify_ _ { route = saved }
    Proxy @"routes" @~ Map.insert name (mkRoute generated)
renderCommand v = trackBounds (renderTurtle v)

renderTurtle :: Command -> TraintleM Unit
renderTurtle cmd = do
  { radii: Pair sharp shallow } <- get
  case cmd of
    W -> pathCommand false 0
    S -> pathCommand true 0
    Z -> jog \slope -> { delta: 2 .* rotL slope, slope }
    C -> jog \slope -> { delta: 2 .* rotR slope, slope }
    X -> jog \slope -> { delta: mempty, slope: -1 .* slope }
    E -> pathCommand false shallow
    D -> pathCommand false sharp
    A -> pathCommand false (-sharp)
    Q -> pathCommand false (-shallow)
    _ -> pure unit
  where

  rotL = \(V2 x y) -> V2 y (negate x)
  rotR = \(V2 x y) -> V2 (negate y) x

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
          asks _.mode >>= case _ of
            Routing name -> do
              last <- gets $ Array.last <<< _.route
              case last, fwd of
                Just (Pair { pos: Pair _ endpoint } _), { pos: Pair startpoint _ }
                  | endpoint /= startpoint -> throwError $ "Discontinuity while routing " <> show name
                _, _ -> pure unit
              Proxy @"route" @<> [ segment ]
            Drawing -> pure unit
        Nothing -> throwError "Could not find appropriate segment"


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
    -- Nothing new drawn
    true -> setProp @"path" cancel
    -- Incorporate new segment
    false -> do
      tellR _ { bounds = Just <<< App <<< map mkBound ..$ [ before.at, after.at ] }
      setProp @"path" path { endpoint = Just (Last after), moves = Set.insert (Pair before after) path.moves }
  pure r


