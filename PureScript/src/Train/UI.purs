module Train.UI where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ResourceM (track)
import Control.Monad.ResourceT (ResourceM)
import Control.Plus (empty)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Filterable (separate)
import Data.Foldable (fold, foldMap)
import Data.Functor.Compose (Compose(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number.Format as Format
import Data.Pair (Pair(..))
import Data.Profunctor.Choice ((|||))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Idiolect ((<#?>))
import Math.Matrix (Vec2(..))
import Riverdragon.Dragon (AttrProp, Dragon)
import Riverdragon.Dragon.Bones ((.$), (.$~~), (<:>), (=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (deletable)
import Riverdragon.River (Course(..), Lake, River, coursing, createRiver, makeLake, memoize, noBurst, oneStream, stillRiver, store, store')
import Riverdragon.River.Bed (freshId)
import Riverdragon.River.Beyond (instanced, withLast)
import Train.Dynamics (Traction)
import Train.Dynamics as Dyn
import Train.Types (Pos)
import Widget (Interface, mixInterface, reInterface, stillInterface, valueInterface)

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


range :: Number -> Number -> Number -> River Number -> (Number -> Effect Unit) -> Array (River AttrProp) -> Dragon
range vmin vmax step value onValue attrs = D.input $
  [ D.attr "type" =:= "range"
  , D.prop "min" =:= vmin
  , D.prop "max" =:= vmax
  , D.prop "step" =:= step
  , D.stylish =:= D.smarts
      { "width": "100%"
      , "display": "block"
      }
  , D.value <:> value
  , D.onInputNumber =:= onValue
  ] <|> attrs

number :: Number -> Number -> Number -> River Number -> (Number -> Effect Unit) -> Array (River AttrProp) -> Dragon
number vmin vmax step value onValue attrs = D.input $
  [ D.attr "type" =:= "number"
  , D.prop "min" =:= vmin
  , D.prop "max" =:= vmax
  , D.prop "step" =:= step
  , D.stylish =:= D.smarts { "width": "8em" }
  , D.value <:> value
  , D.onInputNumber =:= onValue
  ] <|> attrs


calcRail :: River Traction -> ResourceM
  { v0 :: Interface Number
  , di :: Interface Number
  , ti :: Interface Number
  , v1 :: Interface Number
  }
calcRail traction = do
  -- Independent variable
  v0Input <- track $ valueInterface 0.0
  -- One of these is chosen as the other independent variable,
  -- the others become dependent variables
  diInput <- track $ valueInterface 0.0
  tiInput <- track $ valueInterface 0.0
  v1Input <- track $ valueInterface 0.0
  let
    -- Each new input pins these as the independent variable du jour
    mode = oneStream
      [ diInput.receive <#> \di tract v0 ->
          let calc = Dyn.maxAtDistance tract v0 di in
          { di: Nothing, v1: Just calc.veloc, ti: Just calc.time }
      , noBurst tiInput.receive <#> \ti tract v0 ->
          let calc = Dyn.curve tract v0 ti in
          { ti: Nothing, v1: Just calc.veloc, di: Just calc.dist }
      , noBurst v1Input.receive <#> \v1 tract v0 ->
          let calc = Dyn.toVelocity tract (Pair v0 v1) in
          { v1: Nothing, di: Just calc.dist, ti: Just calc.time }
      ]
  let v0 = v0Input { receive = empty }
  { stream: computed } <- store $ mode <*> traction <*> v0.loopback
  di <- track $ mixInterface diInput $ computed <#?> _.di
  ti <- track $ mixInterface tiInput $ computed <#?> _.ti
  v1 <- track $ mixInterface v1Input $ computed <#?> _.v1
  pure { v0, di, ti, v1 }

fmt :: Number -> String
fmt = Format.toStringWith (Format.fixed 2)
fmting :: River Number -> Dragon
fmting = D.Text <<< stillRiver <<< map fmt

railCalc :: River Traction -> ResourceM { widget :: Dragon, outputs :: _ }
railCalc traction = do
  let
    routeDist = 10_000.0
    dms :: River Number -> Dragon
    dms speed = fold
      [ fmting speed, D.text " dm/s"
      , D.text " = "
      , fmting $ speed <#> \s -> s * 0.36
      , D.text " km/h"
      ]

  outputs@{ v0, di, ti, v1 } <- calcRail traction

  pure $ { outputs, widget: _ } $ fold
    [ mempty
    , range 0.0 300.0 0.1 (pure 0.0) v0.send []
    , dms v0.loopback
    , range 0.0 routeDist 1.0 di.receive di.send []
    , fmting di.loopback, D.text " dm"
    , range 0.0 100.0 0.1 ti.receive ti.send []
    , fmting ti.loopback, D.text " s"
    , range 0.0 300.0 0.1 v1.receive v1.send []
    , dms v1.loopback
    ]

cfgTraction :: Traction -> ResourceM { widget :: Dragon, outputs :: _ }
cfgTraction initial = do
  wheels <- track $ valueInterface initial.wheels
  motors <- track $ valueInterface initial.motors
  let
    combined = { wheels: _, motors: _ } <$> wheels.loopback <*> motors.loopback
  traction <- track $ reInterface $ stillInterface
    { send: \r -> wheels.send r.wheels <> motors.send r.motors
    , receive: combined
    , loopback: combined
    , mailbox: const empty
    , active: const empty
    , current: unwrap $ do { wheels: _, motors: _ } <$> Compose wheels.current <*> Compose motors.current
    , destroy: mempty
    }
  let outputs = { wheels, motors, traction }
  pure $ { outputs, widget: _ } $ fold
    [ number 0.0 50.0 1.0 (pure initial.wheels) wheels.send []
    , number 0.0 5000.0 25.0 (pure initial.motors) wheels.send []
    ]


table :: Array Dragon -> Array { th :: Maybe Dragon, td :: Array Dragon } -> Dragon
table headers rows =
  D.html_"table".$~~
    [ D.html_"thead".$~~ D.html_"th"[] <$> headers
    , D.html_"tbody".$~~ rows <#> \row -> D.html_"tr".$~~
      [ foldMap (D.html_"th"[]) row.th ] <>
      map (D.html_"td"[]) row.td
    ]



