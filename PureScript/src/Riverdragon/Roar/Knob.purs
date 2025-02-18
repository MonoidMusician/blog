module Riverdragon.Roar.Knob where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Writer (WriterT(..), runWriterT)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Foldable (sequence_, sum, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.RecordOverloads (class RecordOverloads, overloads)
import Data.Symbol (class IsSymbol)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Riverdragon.River (Allocar, Lake, Stream, dam)
import Riverdragon.River as River
import Riverdragon.River.Bed (cleanup)
import Riverdragon.Roar.Types (RoarO, Roar, connecting)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Audio.Node (createConstantSourceNode, intoParam)
import Web.Audio.Node as Node
import Web.Audio.Param as AudioParam
import Web.Audio.Types (AudioContext, AudioNode, AudioParam, AudioParamCmd(..), Cancel(..), Duration, Float, Ramp(..), Rate, Time, currentTime)
import Web.Audio.Types as Audio

-- sum params: Array (AudioParam into ConstantSourceNode) into AudioParam
-- so maybe Roars are lists of either audio params or audio sources? and can lazily sum and convert between them …

-- multiply signals with GainNode I guess?

-- can approximate arbitrary mapping functions with WaveShaperNode

-- instance ToAudioParam (Stream flow ) where


-- const :: Float -> Knob

-- | `Knob`s are always `NotFlowing` (despite their name?)
data Knob
  = KEmpty
  | KConst Float
  | KAdd (Array Knob)
  -- | KMul (Array Knob)
  | KCmd Float (Lake (Time -> Allocar (Array AudioParamCmd)))
  | KAudio (Lake (Array RoarO))
  | KAware (Maybe Float) (Allocar Float -> Allocar Knob)
  | KStartFrom Float Knob
  | KCtx (AudioContext -> Allocar Time -> Knob)

impulse :: forall flow. Number -> Stream flow Number -> Knob
impulse k stream = KCmd 0.0 $ dam stream <#> \target now -> pure
  [ CmdTarget { ramp: NoRamp, target, time: now }
  , CmdTarget { ramp: DecayRamp k, target: 0.0, time: now }
  ]

directValue :: forall flow. Number -> Stream flow Number -> Knob
directValue default stream = KCmd default $ dam stream <#> \target now ->
  pure [ CmdTarget { ramp: NoRamp, target, time: now } ]

linearEase :: forall flow. Number -> Number -> Stream flow Number -> Knob
linearEase default duration stream = KAware (Just default) \getValue -> do
  pure $ KCmd default $ dam stream <#> \target now -> do
    value <- getValue
    pure
      [ CmdTarget { ramp: NoRamp, target: value, time: now }
      , CmdTarget { ramp: LinRamp, target, time: now + duration }
      ]

type Envelope =
  { attack :: Duration
  , decay :: Duration
  , sustain :: Float
  , release :: Duration
  }

-- | Standard Attack-Decay-Sustain-Release envelope, where `true` events trigger
-- | the attack and `false` events trigger the release.
adsr :: forall flow. Float -> Envelope -> Stream flow Boolean -> Knob
adsr volume env events = KCmd 0.0 $ dam events <#>
  \attackOrRelease time ->
    if attackOrRelease
      then pure
        [ CmdTarget { ramp: LinRamp, target: volume, time: time + env.attack }
        , CmdTarget { ramp: LinRamp, target: volume * env.sustain, time: time + env.attack + env.decay }
        ]
      else pure
        [ CmdCancel { time, hold: Revert } -- FIXME??
        , CmdTarget { ramp: LinRamp, target: 0.0, time: time + env.release }
        ]


class ToKnob knob where toKnob :: knob -> Knob
instance ToKnob Unit where toKnob = const KEmpty
instance ToKnob Knob where toKnob = identity
instance ToKnob Number where toKnob = KConst
instance ToKnob Int where toKnob = KConst <<< Int.toNumber
instance ToKnob (Stream flow Int) where toKnob = directValue 0.0 <<< map Int.toNumber
instance ToKnob (Stream flow Number) where toKnob = directValue 0.0

instance ToKnob knob => ToKnob (Tuple Number knob) where toKnob = uncurry KStartFrom <<< map toKnob
instance ToKnob knob => ToKnob (Tuple Int knob) where toKnob = uncurry KStartFrom <<< bimap Int.toNumber toKnob

instance ToKnob knob => ToKnob (Maybe knob) where toKnob = maybe KEmpty toKnob

instance ToKnob (Stream flow AudioParamCmd) where
  toKnob = KCmd 0.0 <<< dam <<< map (pure <<< pure <<< pure)
instance ToKnob (Stream flow (Array AudioParamCmd)) where
  toKnob = KCmd 0.0 <<< dam <<< map (pure <<< pure)

instance ToKnob Roar where toKnob = KAudio <<< pure <<< pure
instance ToKnob (Array Roar) where toKnob = KAudio <<< pure
instance ToKnob (Stream flow Roar) where toKnob = KAudio <<< dam <<< map pure
instance ToKnob (Stream flow (Array Roar)) where toKnob = KAudio <<< dam
instance ToKnob (Stream flow (Maybe Roar)) where toKnob = KAudio <<< dam <<< map Array.fromFoldable

type KnobOverloads flow =
  ( impulse :: { decay :: Number, impulse :: Stream flow Number } -> Knob
  , directValue :: { default :: Number, value :: Stream flow Number } -> Knob
  , adsr :: { adsr :: Envelope, attackRelease :: Stream flow Boolean } -> Knob
  , adsrSplit :: { adsr :: Envelope, attack :: Stream flow Unit, release :: Stream flow Unit } -> Knob
  , adsrNow :: { adsr :: Envelope, release :: Stream flow Unit } -> Knob
  , adsrVol :: { adsr :: Envelope, volume :: Float, attackRelease :: Stream flow Boolean } -> Knob
  , adsrSplitVol :: { adsr :: Envelope, volume :: Float, attack :: Stream flow Unit, release :: Stream flow Unit } -> Knob
  , adsrNowVol :: { adsr :: Envelope, volume :: Float, release :: Stream flow Unit } -> Knob
  )

instance (RecordOverloads (KnobOverloads flow) tgt Knob) => ToKnob (Record tgt) where
  toKnob = overloads @(KnobOverloads flow)
    { impulse: \r -> impulse r.decay r.impulse
    , directValue: \r -> directValue r.default r.value
    , adsr: \r -> adsr 1.0 r.adsr r.attackRelease
    , adsrSplit: \r -> adsr 1.0 r.adsr (true <$ r.attack <|> false <$ r.release)
    , adsrNow: \r -> adsr 1.0 r.adsr (pure true <|> false <$ r.release)
    , adsrVol: \r -> adsr r.volume r.adsr r.attackRelease
    , adsrSplitVol: \r -> adsr r.volume r.adsr (true <$ r.attack <|> false <$ r.release)
    , adsrNowVol: \r -> adsr r.volume r.adsr (pure true <|> false <$ r.release)
    }

renderKnob :: forall rate.
  Knob ->
  AudioContext ->
  { default :: Maybe Float
  , apply :: AudioParam rate -> Allocar (Allocar Unit)
  }
renderKnob knob ctx = case knob of
  KEmpty -> { default: Nothing, apply: mempty }
  KConst v -> { default: Just v, apply: mempty }
  KCmd v cmds -> { default: Just v, apply: _ } \param -> do
    River.subscribe cmds \fire -> do
      cmdArray <- fire =<< currentTime ctx
      traverse_ (AudioParam.applyCmd param) cmdArray
  KAudio srcs -> { default: Nothing, apply: _ } \param -> do
    connecting srcs (intoParam param)
  KAdd items ->
    let knobs = renderKnob <$> items <@> ctx in
    { default: sum $ knobs <#> _.default, apply: _ } \finalParam -> do
      Tuple asAudio unsub <- knobsToAudios ctx items
      let
        wiring dis = sequence_ $
          Node.disConnect dis <$> asAudio <@> Node.intoParam finalParam
      wiring false
      cleanup $ wiring true <> unsub
  KAware default mkKnob -> { default, apply: _ } \param -> do
    nested <- mkKnob (AudioParam.currentValue param)
    (renderKnob nested ctx).apply param
  KStartFrom default next -> (renderKnob next ctx) { default = Just default }
  KCtx mkKnob -> renderKnob (mkKnob ctx (currentTime ctx)) ctx

-- | Render a `Knob` to an `AudioNode` (specifically a `ConstantSourceNode`),
-- | because an a-rate `AudioParam` can be represented as an audio signal
-- | and vice-versa.
knobToAudio :: AudioContext -> Knob -> Allocar (Tuple RoarO (Allocar Unit))
knobToAudio ctx knob = do
  let { default, apply: applyKnob } = renderKnob knob ctx
  node <- createConstantSourceNode ctx { offset: default }
  Node.startNow node
  unsub <- applyKnob (Audio.getParam node (Proxy :: Proxy "offset"))
  pure $ Tuple (Node.outOfNode node 0) unsub

knobsToAudios :: forall f. Traversable f => AudioContext -> f Knob -> Allocar (Tuple (f RoarO) (Allocar Unit))
knobsToAudios ctx knobs = runWriterT do
  traverse (WriterT <<< knobToAudio ctx) knobs

class KnobsRL
  (ir :: Row Type)
  (il :: RL.RowList Type)
  (dr :: Row Type)
  (ar :: Row Rate)
  | il -> ir dr
  where
  renderKnobsRL :: forall name source read write.
    Record ir -> AudioContext ->
    { defaults :: Record dr
    , apply :: AudioNode name source read write ar -> Allocar (Allocar Unit)
    }

instance KnobsRL () RL.Nil () ar where renderKnobsRL = mempty
instance
  ( IsSymbol s
  , ToKnob knob
  , Row.Cons s knob ir ir', Row.Lacks s ir
  , Row.Cons s (Maybe Float) dr dr', Row.Lacks s dr
  , Row.Cons s rate ar' ar
  , KnobsRL ir il dr ar
  ) => KnobsRL ir' (RL.Cons s knob il) dr' ar where
  renderKnobsRL ir ctx =
    let
      this = renderKnob (toKnob (Record.get (Proxy :: Proxy s) ir)) ctx
      more = renderKnobsRL @ir @il (unsafeCoerce ir :: Record ir) ctx
    in
    { defaults: Record.insert (Proxy :: Proxy s) this.default more.defaults
    , apply: \node -> this.apply (Audio.getParam node (Proxy :: Proxy s)) <> more.apply node
    }

class Knobs
  (ir :: Row Type)
  (dr :: Row Type)
  (ar :: Row Rate)
  | ir -> dr
  where
  renderKnobs :: forall name source read write.
    Record ir -> AudioContext ->
    { defaults :: Record dr
    , apply :: AudioNode name source read write ar -> Allocar (Allocar Unit)
    }
instance (RL.RowToList ir il, KnobsRL ir il dr ar) => Knobs ir dr ar where
  renderKnobs = renderKnobsRL @ir @il
