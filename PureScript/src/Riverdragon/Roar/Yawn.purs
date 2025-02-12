module Riverdragon.Roar.Yawn where

import Prelude

import Data.Either (either)
import Data.Identity (Identity)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Riverdragon.River (Allocar, Lake, Stream, bursting, dam, (>>~))
import Riverdragon.Roar.Types (RoarI, RoarO, subscribing)
import Uncurried.RWSET (RWSET, runRWSET)
import Web.Audio.Context (createAudioContext)
import Web.Audio.Node (destination)
import Web.Audio.Types (AudioContext, AudioParamCmd(..), Float, Ramp(..), Time)

type YawnM =
  RWSET
    { ctx :: AudioContext
    , state :: Ref
      { id :: Int }
    }
    { destroy :: Effect Unit
    , ready :: Aff Unit
    }
    Unit
    Void
    Aff

type YawnFn result =
  { ctx :: AudioContext
  , state :: Ref
    { id :: Int }
  } -> Aff
  { destroy :: Effect Unit
  , result :: result
  }

biiigYawn :: forall flow.
  YawnM (Stream flow (Array RoarO)) ->
  Aff
    { ctx :: AudioContext
    , destroy :: Effect Unit
    }
biiigYawn creator = do
  ctx <- liftEffect $ createAudioContext {}
  state <- liftEffect $ Ref.new { id: 0 }
  _ /\ orStream /\ written <- runRWSET { ctx, state } unit creator
  let outputs = either absurd identity orStream
  let { destroy: destroy1, ready } = written
  destroy2 <- liftEffect $ subscribing (dam outputs) (destination ctx)
  let destroy = destroy1 <> destroy2
  ready
  pure { ctx, destroy }



{-
  gain :: RoarO -> Knob -> YawnM RoarI
-}

-- gain' ::
--   { inputs :: Identity RoarO
--   , gain :: Knob
--   } -> YawnM
--   { node :: Unit
--   , outputs :: Identity RoarI
--   }

-- yawn :: forall a.
