module Data.ArrayMachine where

import Prelude

import Control.Monad.ST as ST
import Data.Array.ST as STA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

unfoldArrayMachine ::
  forall a b exit.
  (Int -> a) ->
  b ->
  (b -> Either exit (Tuple Int (a -> Tuple a b))) ->
  { values :: Array a, exit :: exit }
unfoldArrayMachine init state0 stepFn = ST.run do
  arr <- STA.new
  let
    ensure idx len = do
      peeked <- STA.peek idx arr
      case peeked of
        Just value -> pure value
        Nothing -> do
          ensure idx =<< STA.push (init len) arr
    go len state = case stepFn state of
      Left exit -> do
        values <- STA.unsafeFreeze arr
        pure { values, exit }
      Right (Tuple idx continue) -> do
        prev <- ensure idx len
        let Tuple next state' = continue prev
        _ <- STA.poke idx next arr
        go (max len $ idx + 1) state'
  go 0 state0
