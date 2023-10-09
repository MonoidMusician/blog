module FRP.Aff where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import FRP.Event (Event, makeEvent)

affToEvent :: forall a. Aff a -> Event (Maybe a)
affToEvent aff = makeEvent \cb -> do
  fiber <- Aff.runAff (cb <<< hush) aff
  pure $ Aff.launchAff_ $ Aff.killFiber (Aff.error "event unsubscribed") fiber
