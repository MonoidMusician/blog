module Train.UI where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ResourceT (ResourceM)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Filterable (separate)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Choice ((|||))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Idiolect ((<#?>))
import Math.Matrix (Vec2(..))
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones ((.$), (.$~~), (<:>))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (deletable)
import Riverdragon.River (Course(..), Lake, River, coursing, createRiver, makeLake, memoize, store')
import Riverdragon.River.Bed (freshId)
import Riverdragon.River.Beyond (instanced, withLast)
import Train.Types (Pos)

definitions :: Array (Tuple Dragon Dragon) -> Dragon
definitions entries = D.dl.$~~ entries >>= \(Tuple term def) ->
  [ D.dt.$ term, D.dd.$ def ]

definitionsies :: Array (Tuple Dragon (Array Dragon)) -> Dragon
definitionsies entries = D.dl.$~~ entries >>= \(Tuple term defs) ->
  [ D.dt.$ term ] <|> D.dd[] <$> defs

ulist :: Array Dragon -> Dragon
ulist items = D.ul.$~~ D.li[] <$> items

-- | Keep track of contiguous sequences of lefts and rights.
trackEither :: forall x y. River (Either x y) -> Lake (Either (Tuple x (Lake x)) (Tuple y (Lake y)))
trackEither = memoize >>> \input ->
  let
    { left: xs, right: ys } = separate input
  in withLast input <#?> case _ of
    { last: Just (Left _), next: Left _ } -> Nothing
    { last: Just (Right _), next: Right _ } -> Nothing
    { next: Left x } -> Just (Left (Tuple x xs))
    { next: Right y } -> Just (Right (Tuple y ys))

-- | Render lefts or rights continuously, only doing a hard switch when
-- | switching between the two.
dragonEither :: forall x y. (River x -> Dragon) -> (River y -> Dragon) -> River (Either x y) -> Dragon
dragonEither f g = trackEither >>> map (withHead f ||| withHead g) >>> D.Replacing
  where
  withHead :: forall z. (River z -> Dragon) -> Tuple z (Lake z) -> Dragon
  withHead h (Tuple z zs) = D.Egg do
    { stream: zz } <- store' z zs
    pure $ h zz

-- | Manage the `<defs>` section of an SVG.
manageDefs :: ResourceM
  { defs :: Dragon -- the current defs
  , defM :: (String -> Dragon) -> ResourceM String -- create a def as a monad action
  , defL :: (String -> Dragon) -> Lake String -- create a def as a lake
  , defineL :: (String -> Dragon) -> ResourceM (Lake String) -- create a def and track its usage
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

resourceMAsLake :: ResourceM ~> Lake
resourceMAsLake create = makeLake \cb -> do
  liftEffect <<< cb =<< create

clone :: Lake String -> Array (Lake D.AttrProp) -> Dragon
clone id attrs = D.svg_"use" ([ D.xlink_href <:> id ] <> attrs) mempty

mask :: Lake String -> Lake D.AttrProp
mask ids = D.attr "mask" <:> ids <#> \id -> "url(" <> id <> ")"


renderPos :: Pos -> Dragon
renderPos { at: V2 x y, to: V2 dx dy } = D.code[] $ D.text $ show x <> "," <> show y <> " @ " <> show dx <> "/" <> show dy

renderPosMap :: forall v. (v -> Dragon) -> Map.Map Pos v -> Dragon
renderPosMap renderItem items = definitions $ Map.toUnfoldable items
  <#> bimap renderPos renderItem

renderPosMapsies :: forall v. (v -> Array Dragon) -> Map.Map Pos v -> Dragon
renderPosMapsies renderItem items = definitionsies $ Map.toUnfoldable items
  <#> bimap renderPos renderItem
