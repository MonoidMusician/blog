module FRP.Helpers where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as STRef
import Control.Plus (empty)
import Data.Array ((..))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Foldable (for_, oneOfMap)
import Data.Functor.App (App(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\), type (/\))
import FRP.Event (class IsEvent, AnEvent, bang, filterMap, keepLatest, makeEvent, mapAccum, subscribe, sweep, withLast)
import FRP.Memoize (bangFold)
import Unsafe.Reference (unsafeRefEq)

counter :: forall s m a. MonadST s m => AnEvent m a → AnEvent m (a /\ Int)
counter event = mapAccum f event 0
  where
  f a b = (b + 1) /\ (a /\ b)

toggle :: forall s m a b. HeytingAlgebra b => MonadST s m => b -> AnEvent m a → AnEvent m b
toggle start event = bangFold (\_ x -> not x) event start

withLast' :: forall event a. IsEvent event => event a -> event { last :: a, now :: a }
withLast' = filterMap (\{ last, now } -> last <#> { last: _, now }) <<< withLast

dedup :: forall s m a. Eq a => Applicative m => MonadST s m => AnEvent m a -> AnEvent m a
dedup = dedupOn eq

dedupOn :: forall s m a. (a -> a -> Boolean) -> Applicative m => MonadST s m => AnEvent m a -> AnEvent m a
dedupOn feq e = compact $
  mapAccum (\a b -> let ja = Just a in ja /\ (if (feq <$> b <*> ja) == Just true then Nothing else Just a)) e Nothing



interpolate :: Int -> Int -> Array Int
interpolate i j | i > j = j .. (i - 1)
interpolate i j | i < j = i .. (j - 1)
interpolate _ _ = []

stepByStep :: forall s m r. MonadST s m => Boolean -> AnEvent m Int -> ((Int -> AnEvent m Boolean) -> r) -> AnEvent m r
stepByStep start index cb =
  let
    state = withLast' index
    swept = keepLatest $ map (oneOfMap bang) $
      state <#> \{ last, now } -> interpolate last now
  in
    sweep swept \sweeper ->
      let
        sweeper' = toggle start <<< sweeper
      in
        cb sweeper'

spotlight :: forall a s m r. Ord a => MonadST s m => Boolean -> AnEvent m a -> ((a -> AnEvent m Boolean) -> r) -> AnEvent m r
spotlight start shineAt cb =
  sweep (spotlightChange shineAt) \sweeper ->
    cb (toggle start <<< sweeper)

spotlightChange :: forall s m a. MonadST s m => Eq a => AnEvent m a -> AnEvent m a
spotlightChange shineAt =
  keepLatest $ withLast shineAt <#> \{ now, last } ->
    if last == Just now then empty
    else
      oneOfMap bang last <|> bang now

sweepSave :: forall m s r a. Ord a => MonadST s m => AnEvent m a -> ((a -> AnEvent m Unit) -> r) -> AnEvent m r
sweepSave e f = makeEvent \k1 -> do
  r <- liftST $ STRef.new Map.empty
  saved <- liftST $ STRef.new Map.empty
  let
    modifier = case _ of
      Nothing -> Just (Just 1)
      Just (Just n) -> Just (Just (n+1))
      Just Nothing -> Just Nothing
    stashed k2 (Just (Just n)) = do
      unwrap $ Monoid.power (App k2) n
    stashed _ _ = pure unit
  k1 $ f \a -> makeEvent \k2 -> do
    stashed (k2 unit) =<< (liftST $ Map.lookup a <$> STRef.read saved)
    void $ liftST $ STRef.modify (Map.insert a Nothing) saved
    void $ liftST $ STRef.modify (Map.alter (case _ of
      Nothing -> Just [k2]
      Just arr -> Just (arr <> [k2])) a)  r
    pure $ void $ liftST $ STRef.modify (Map.alter (case _ of
      Nothing -> Nothing
      Just arr -> Just (Array.deleteBy unsafeRefEq k2 arr)) a) r
  unsub <- subscribe e \a -> do
    void $ liftST $ STRef.modify (Map.alter modifier a) saved
    o <- liftST $ STRef.read r
    case Map.lookup a o of
      Nothing -> pure unit
      Just arr -> for_ arr (_ $ unit)
  pure do
    -- free references - helps gc?
    void $ liftST $ STRef.write (Map.empty) r
    unsub
