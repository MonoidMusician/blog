module FRP.Helpers where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
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
import FRP.Event (class IsEvent, Event, filterMap, keepLatest, mailboxed, makeEvent, mapAccum, subscribe, withLast)
import FRP.Memoize (pureFold)
import Unsafe.Reference (unsafeRefEq)

counter :: forall a. Event a → Event (a /\ Int)
counter event = mapAccum f event 0
  where
  f a b = (b + 1) /\ (a /\ b)

toggle :: forall a b. HeytingAlgebra b => b -> Event a → Event b
toggle start event = pureFold (\_ x -> not x) event start

withLast' :: forall event a. IsEvent event => event a -> event { last :: a, now :: a }
withLast' = filterMap (\{ last, now } -> last <#> { last: _, now }) <<< withLast

dedup :: forall a. Eq a => Event a -> Event a
dedup = dedupOn eq

dedupOn :: forall a. (a -> a -> Boolean) -> Event a -> Event a
dedupOn feq e = compact $
  mapAccum (\a b -> let ja = Just a in ja /\ (if (feq <$> b <*> ja) == Just true then Nothing else Just a)) e Nothing



interpolate :: Int -> Int -> Array Int
interpolate i j | i > j = j .. (i - 1)
interpolate i j | i < j = i .. (j - 1)
interpolate _ _ = []

stepByStep :: forall r. Boolean -> Event Int -> ((Int -> Event Boolean) -> r) -> Event r
stepByStep start index cb =
  let
    state = withLast' index
    swept = keepLatest $ map (oneOfMap pure) $
      state <#> \{ last, now } -> interpolate last now
  in
    mailboxed (map { address: _, payload: unit} swept) \sweeper ->
      let
        sweeper' = toggle start <<< sweeper
      in
        cb sweeper'

spotlight :: forall a r. Ord a => Boolean -> Event a -> ((a -> Event Boolean) -> r) -> Event r
spotlight start shineAt cb =
  mailboxed (spotlightChange (map { address: _, payload: unit} shineAt)) \sweeper ->
    cb (toggle start <<< sweeper)

spotlightChange :: forall a. Eq a => Event a -> Event a
spotlightChange shineAt =
  keepLatest $ withLast shineAt <#> \{ now, last } ->
    if last == Just now then empty
    else
      oneOfMap pure last <|> pure now

sweepSave :: forall r a. Ord a => Event a -> ((a -> Event Unit) -> r) -> Event r
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
