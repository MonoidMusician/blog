module Riverdragon.River.Cache where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.AVar as AVarE
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Riverdragon.River.Bed (Allocar)
import Riverdragon.River.Bed as Bed

class Cacheable t where
  estimateSize :: t -> Int

newtype LRU t = LRU
  { register :: t -> Allocar Boolean -> Allocar { bump :: Allocar Unit }
  , check :: Allocar { allowed :: Int, current :: Int }
  , reset :: Int -> Allocar Unit
  }

lru :: forall t. Cacheable t => Int -> Allocar (LRU t)
lru defaultAllowance = do
  allocated <- Bed.prealloc 0
  allowance <- Bed.prealloc defaultAllowance
  keygen <- Bed.freshId
  cacheState <- Bed.ordMap
  let
    over = (<) <$> allowance.get <*> allocated.get
    trim = trimming Map.empty
    trimming retained = do
      untouched <- cacheState.read
      over >>= case _ of
        true | Just { key, value: tgt } <- Map.findMin untouched -> do
          tgt.pleaseUnalloc >>= case _ of
            true -> do
              -- Removed
              allocated.set <<< (_ - tgt.size) =<< allocated.get
              trimming retained
            false -> do
              -- Try again next time
              trimming (Map.insert key tgt retained)
        _ -> cacheState.restore (Map.union retained untouched)
    bump lastKey = do
      was <- lastKey.get
      cacheState.remove was >>= traverse_ \st -> do
        willBe <- keygen
        lastKey.set willBe
        cacheState.set willBe st
    register = estimateSize >>> \size pleaseUnalloc -> do
      trim
      key <- keygen
      lastKey <- Bed.prealloc key
      allocated.set <<< (_ + size) =<< allocated.get
      cacheState.set key { size, pleaseUnalloc }
      pure
        { bump: bump lastKey
        }
  pure $ LRU
    { register
    , check: { allowed: _, current: _ } <$> allowance.get <*> allocated.get
    , reset: \size -> allowance.set size *> trim
    }

makeCached :: forall t. Cacheable t =>
  LRU t -> Aff t -> Allocar (Aff (Allocar t))
makeCached (LRU { register }) acquire = do
  demand <- Bed.refc
  bump <- Bed.prealloc (pure unit :: Allocar Unit)
  obtaining <- Bed.prealloc false
  cached <- AVarE.empty :: Allocar (AVar.AVar (Either Aff.Error t))
  let
    -- Increase refc while in flight, until the `Aff` fails or is canceled,
    -- or until the `Allocar` is called (signifying that its use was fulfilled)
    bracketUsage :: forall d. Aff d -> Aff (Allocar d)
    bracketUsage act = demand.bracket do
      r <- act
      liftEffect do
        demand.incr
        settled <- Bed.cleanup demand.decr
        pure $ r <$ settled

    tryDelete :: Allocar Boolean
    tryDelete = do
      demand.get >>= case _ of
        0 -> do
          void $ AVarE.tryTake cached
          bump.set (pure unit)
          pure true
        _ -> pure false

    obtainDirectly :: Aff t
    obtainDirectly = Aff.bracket
      (liftEffect $ obtaining.set true)
      (const (liftEffect $ obtaining.set false))
      \_ -> do
        -- Call whatever method we were given to acquire it
        t <- acquire
        liftEffect do
          void $ AVarE.tryTake cached -- clear it out
          void $ AVarE.tryPut (Right t) cached -- fill it (should not fail)
          entered <- register t tryDelete -- register it in cache
          bump.set entered.bump -- save the bump function
        pure t

    -- If obtaining it directly fails, we put `Left e` into the `AVar` to make
    -- sure that listeners also know it failed
    notifyFailure :: Aff ~> Aff
    notifyFailure act = Aff.catchError act \e -> do
      void $ AVar.tryPut (Left e) cached
      Aff.throwError e

    obtain :: Aff (Allocar t)
    obtain = bracketUsage do
      liftEffect obtaining.get >>= case _ of
        true ->
          -- Wait for the other thread to do the work
          AVar.read cached >>= case _ of
            Right t -> pure t
            Left e -> Aff.throwError e
        false ->
          AVar.tryRead cached >>= case _ of
            Just (Right t) -> pure t -- already cached
            _ -> do
              void $ AVar.tryTake cached -- clear out any error
              obtainDirectly # notifyFailure
  pure obtain
