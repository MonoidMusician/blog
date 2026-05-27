module Riverdragon.River.Cytosol where

import Control.Monad.Cell.Basics
import Prelude
import Riverdragon.River

import Control.Monad.Cell.Processors (Processing)
import Control.Monad.ResourceM (class MonadResource, selfScope, subScope)
import Control.Monad.ResourceT (ResourceM, ResourceT, Scope(..), scopedRun, scopedStart_)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)

cellularStream :: forall flow i o.
  Stream flow i ->
  ResourceM (i -> ResourceM (Array o)) ->
  Lake o
cellularStream stream mintProcessor = makeLake' \runDry output -> do
  scope@(Scope { putDestructor }) <- subScope "cellularStream"
  liftEffect do putDestructor runDry
  processor <- mintProcessor
  subscribeUntil runDry stream \i ->
    void $ scopedStart_ "cellularStream" scope do
      os <- processor i
      for_ os (liftEffect <<< output)

statefulStream :: forall flow a b s.
  s -> Stream flow a ->
  (s -> a -> { state :: s, emit :: Maybe b }) ->
  Lake b
statefulStream s0 stream evolve = cellularStream stream do
  cell <- mintCell s0
  pure \i -> do
    { state: b, emit: c } <- evolve <$> cell.get <@> i
    cell.set b
    pure $ Array.fromFoldable c

processorStream :: forall flow i o.
  Stream flow i ->
  ResourceT Aff (i -> ResourceT Aff (Array o)) ->
  Lake o
processorStream stream loadProcessor = makeLake' \runDry output -> do
  scope@(Scope { putDestructor }) <- subScope "processorStream"
  liftEffect do putDestructor runDry
  liftEffect $ launchAff_ $ void $ scopedRun "processorStream" scope do
    processor <- loadProcessor
    subscribeUntil runDry stream \i ->
      launchAff_ $ void $ scopedRun "cellularStream" scope do
        os <- processor i
        for_ os (liftEffect <<< output)
