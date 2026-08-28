module Train.Drawing where

import Prelude

import Control.Monad.ResourceT (ResourceM)
import Data.Distributive (collect)
import Data.Either (Either(..))
import Data.Filterable (separate)
import Data.Foldable (fold, foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair)
import Data.Profunctor.Choice ((|||))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Idiolect (type (@::), (<#?>), (>==))
import Math.Matrix (BBox2, Bez1(..), V2, Vec2(..), bounds2bez, normBounds, padBounds)
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones (($~~), (.$~~), (<:>), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (deletable, liveArray)
import Riverdragon.River (Course(..), Lake, River, coursing, createRiver, makeLake, mapLatest, memoize, stillRiver, store', (>>~))
import Riverdragon.River as River
import Riverdragon.River.Bed (freshId)
import Riverdragon.River.Beyond (dedup, instanced, withLast)
import Train.Types (Pos)
import Unsafe.Coerce (unsafeCoerce)


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

bounds2viewBox :: BBox2 Number -> Array Number
bounds2viewBox (V2 { min: Min xmin, max: Max xmax } { min: Min ymin, max: Max ymax }) =
  [ xmin, ymin, xmax - xmin, ymax - ymin ]




num :: Number -> String
num x = "" <> unsafeCoerce x

int :: Int -> String
int x = "" <> unsafeCoerce x

spaced :: Array (River String) -> River String
spaced = traverse River.alwaysBurstM >== joinWith " "

posIndicator :: River Pos -> River ("color" @:: String) -> Dragon
posIndicator positioning color = D.g [ D.stylish =:= D.smarts { "opacity": 0.7 } ] $ fold
  [ D.svg_"circle"
      [ D.attr "r" =:= "4px"
      , positioning <#> \{ at: V2 x y } -> D.MultiAttr
          [ D.attr "cx" $ 16*x
          , D.attr "cy" $ 16*y
          ]
      , D.stylish <:> D.smarties
        { "fill": color
        }
      ] mempty
  , D.svg_"path"
      [ D.attr "d" <:> positioning <#> \{ at: V2 x y, to: V2 dx dy } ->
          "M" <> int (16*x) <> "," <> int (16*y) <> "l" <> int (16*dx) <> "," <> int (16*dy)
      , D.stylish <:> D.smarties
        { "stroke": color
        , "stroke-width": "2px"
        }
      ] mempty
  ]

{- Targets, per layer:

- total: all tracks in one path
- disjoint: non-overlapping paths
- crossings: guard rails for crossings
- switches: guard rails for switches
- bridges: region for bridges
- tunnels: region for tunnels
-}

type RailStyle =
  { target :: String
  , shape ::
    { width ::
      { outer :: Number
      , inner :: Number
      }
    , exclusion :: Maybe
      { targets :: Array String
      , outer :: Number
      , inner :: Number
      }
    , dash :: Maybe ("pathlength" @:: Number -> { offset :: Number, dasharray :: Array Number })
    , ends :: Maybe
      { sides :: Pair Boolean
      , formula :: RailEnd
      }
    }
  , fill :: RailFill
  , effects ::
    { ids :: Array String
    , padding :: Number
    }
  -- , ends :: Maybe
  , globalOffset :: V2
  }

data RailFill
  = RailColor (River String)
  -- gradients with blur?

data RailEnd
  = RailEndBend
    { fraction :: Number
    , length :: Number
    }

type Target =
  { id :: String
  , bbox :: BBox2 Number
  , pathlength :: Number
  }

renderRails ::
  River (Map ("target" @:: String) (Array Target)) ->
  River (Array RailStyle) ->
  ResourceM { defs :: Dragon, rails :: Dragon }
renderRails targetMap styles = do
  { defs, defL } <- manageDefs
  let
    newmask curve bbox inner outer =
      maskOf (map (padBounds (outer + 12.0)) <$> bbox) $ fold
        [ clone curve
          [ D.stylish =:= D.smarts
            { "stroke": "white"
            , "stroke-width": num outer <> "px"
            }
          ]
        , guard (inner > 0.0) $ clone curve
          [ D.stylish =:= D.smarts
            { "stroke": "black"
            , "stroke-width": num inner <> "px"
            }
          ]
        ]
    maskOf bbox contents = mask $
      defL \id -> D.svg_"mask"
        [ D.id =:= id
        , bbox <#> map bounds2bez >>> \(V2 (B1 x0 x1) (B1 y0 y1)) ->
            D.MultiAttr
            [ D.attr "maskUnits" "userSpaceOnUse"
            , D.attr "x" x0
            , D.attr "y" y0
            , D.attr "width" $ x1 - x0
            , D.attr "height" $ y1 - y0
            ]
        ] contents
    applyExclusion ::
      _ ->
      { targets :: Array String
      , outer :: Number
      , inner :: Number
      } ->
      Dragon -> Dragon
    applyExclusion parent { targets, inner, outer } wrapped =
      let
        targeted = targetMap <#> \pool ->
          foldMap @Array (\t -> fromMaybe [] $ Map.lookup t pool) targets

        bbox :: River (BBox2 Number)
        bbox = targeted
          <#> collect (_.bbox >== Just)
          >== (fold >>> fromMaybe normBounds)

        maskContents = D.g.$~~
          [ clone (pure parent.id)
              [ D.stylish =:= D.smarts
                { "stroke": "white"
                , "stroke-width": num outer <> "px"
                }
              ]
          , targeted >@ foldMap \{ id: thisOne, bbox } ->
              clone (pure thisOne)
                [ D.stylish =:= D.smarts
                  { "stroke": "black"
                  , "stroke-width": "13px"
                  }
                , newmask (pure thisOne) (pure bbox) 11.0 13.0
                ]
          ]
      in D.g [ maskOf (map (padBounds 16.0) <$> bbox) maskContents ] wrapped
    renderShape target { shape } =
      maskOf (map (padBounds (shape.width.outer + 12.0)) <$> pure target.bbox) $
        maybe identity (applyExclusion target) shape.exclusion $ fold
          [ clone (pure target.id)
            [ D.stylish =:= D.smarts
              { "stroke": "white"
              , "stroke-width": num shape.width.outer <> "px"
              }
            ]
          , guard (shape.width.inner > 0.0) $ clone (pure target.id)
            [ D.stylish =:= D.smarts
              { "stroke": "black"
              , "stroke-width": num shape.width.inner <> "px"
              }
            ]
          ]

  pure $ { defs, rails: _ } $
    liveArray styles \_ styleR -> do
      let
        thisTarget = dedup ado
          ts <- stillRiver targetMap
          t <- dedup $ styleR <#> _.target
          in fromMaybe [] $ Map.lookup t ts
      liveArray thisTarget \_ targetR ->
        D.g [ mapLatest identity (renderShape <$> targetR <*> styleR) ] $~~
          [ clone (stillRiver targetR <#> _.id)
              [ D.stylish <:> D.smarties
                { "stroke": styleR >>~ case _ of
                    { fill: RailColor clrR } -> clrR
                , "stroke-width": stillRiver styleR <#> \styl -> num styl.shape.width.outer <> "px"
                }
              ]
          ]


