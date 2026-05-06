module Control.Monad.Cell.Cells where

import Prelude

import Control.Monad.Cell.Basics (class Cellular, mintCell)
import Control.Monad.Cell.Interface (Interface)
import Control.Monad.Cell.Machines (PureMachine, mkMachine, progressMachine, syncMachine)
import Control.Monad.Cell.Operators ((<&=))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))

-- | A cell that works via incremental updates to a monoid.
mintAccumulator :: forall m w. Cellular m => Monoid w => m
  { get :: m w, put :: w -> m Unit, reset :: m w }
mintAccumulator = mintCell mempty <#> \{ get, update, swap } ->
  { get, reset: swap mempty, put: \w -> void $ update (_ <> w) }


machCounter :: PureMachine Unit Int
machCounter = mkMachine 0 \i _ -> Tuple i (i+1)

mintCounter :: forall m. Cellular m => m (m Int)
mintCounter = syncMachine machCounter <#> (#) unit


mintCleanup :: forall m. Cellular m => m Unit -> m (Interface m Unit Unit)
mintCleanup = mintCell >>> map \cell _ -> do
  callbackOnce <- cell <&= (pure unit)
  callbackOnce

mintRolling :: forall m. Cellular m =>
  m (Interface m (m Unit) Unit)
mintRolling = mintCell (pure unit) <#> \cell next -> do
  prev <- cell <&= next
  prev

mintThreshold :: forall m. Cellular m => Int -> m Unit -> m (m Unit)
mintThreshold 0 cb = pure unit <$ cb
mintThreshold n0 cb0 = progressMachine (Just (Tuple n0 cb0)) case _ of
  Just (Tuple n cb)
    | n > 1 -> pure (Just (Tuple (n-1) cb))
    | otherwise -> Nothing <$ cb
  Nothing -> pure Nothing

mintBreaker :: forall m i. Cellular m =>
  (i -> m Unit) ->
  m
    { run :: i -> m Unit
    , trip :: m Unit
    , running :: m Boolean
    }
mintBreaker act = mintCell (Just act) <#> \cell ->
  { run: \i -> do
      cell.get >>= case _ of
        Nothing -> pure unit
        Just fn -> fn i
  , trip: cell.set Nothing
  , running: isJust <$> cell.get
  }
