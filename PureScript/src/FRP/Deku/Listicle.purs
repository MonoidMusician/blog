module FRP.Deku.Listicle where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (oneOfMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Data.Variant as Variant
import Deku.Control (switcher)
import Bolson.Core (dyn, fixed)
import Deku.Core (bus, class Korok, Domable, insert, remove)
import Deku.DOM as D
import Effect (Effect)
import FRP.Deku.Component (ComponentSpec)
import FRP.Event (AnEvent, bang, keepLatest, sampleOn)
import FRP.Helpers (counter)
import FRP.Memoize (memoBangFold)
import Type.Proxy (Proxy(..))


type ListicleEvent a = Variant (add :: a, remove :: Int)

-- | Render a list of items, with begin, end, separator elements and finalize button
-- | and remove buttons on each item. (All of those are optional, except for the items.)
-- |
-- | [ begin, ...[ item, remove, separator ]..., end, finalize ]
-- |
-- | Start from an initial value, listen for external add events, internal remove events,
-- | raise messages on change, and return the current value on finalize.
listicle
  :: forall s m lock payload a
   . Korok s m
  => Show a
  => { initial :: Array a -- initial value
     , addEvent :: AnEvent m a -- external add events

     , remove :: Maybe (Effect Unit -> Domable m lock payload) -- remove button
     , finalize :: Maybe (AnEvent m (Array a) -> Domable m lock payload) -- finalize button

     , renderItem :: a -> Domable m lock payload
     , begin :: Maybe (Domable m lock payload)
     , end :: Maybe (Domable m lock payload)
     , separator :: Maybe (Domable m lock payload)
     }
  -> ComponentSpec m lock payload (Array a)
listicle desc = keepLatest $ bus \pushRemove removesEvent ->
  let
    addEvent = counter desc.addEvent <#> \(v /\ i) -> (i + Array.length desc.initial) /\ v
    initialEvent = oneOfMap bang initialValue

    initialValue :: Array (Int /\ a)
    initialValue = mapWithIndex (/\) desc.initial

    performChange :: ListicleEvent (Int /\ a) -> Array (Int /\ a) -> Array (Int /\ a)
    performChange = Variant.match
      { add: \(j /\ v) vs -> Array.snoc vs (j /\ v)
      , remove: \i -> Array.filter \(i' /\ _) -> i' /= i
      }
    changesEvent =
      Variant.inj (Proxy :: Proxy "add") <$> addEvent
        <|> Variant.inj (Proxy :: Proxy "remove") <$> removesEvent
  in
    memoBangFold performChange changesEvent initialValue \currentValue ->
      let
        intro = case desc.begin of
          Nothing -> []
          Just x -> [ x ]
        extro = case desc.end of
          Nothing -> []
          Just x -> [ x ]
        fin = case desc.finalize of
          Nothing -> []
          Just thingy ->
            [ thingy (currentValue <#> map snd) ]
        sep = case desc.separator of
          Nothing -> []
          Just v -> [ v ]

        withRemover :: Domable m lock payload -> Int -> Array (Domable m lock payload)
        withRemover item idx = case desc.remove of
          Nothing -> [ item ]
          Just remover ->
            [ item, remover (pushRemove idx) ]

        renderOne :: Int /\ a -> Array (Domable m lock payload)
        renderOne (idx /\ item) = withRemover (desc.renderItem item) idx

        dropComma :: Int -> AnEvent m Boolean
        dropComma idx = filter identity
          $
            -- `currentValue` may or may not have updated before this `sampleOn` fires,
            -- depending on the order of subscriptions to `removesEvent`, so we just
            -- detect both here.
            -- (in particular, for elements rendered in the initial view, it seems that
            -- their subscription beats that of `currentValue` somehow)
            sampleOn currentValue
          $ removesEvent <#> \rem vs ->
              -- let _ = unsafePerformEffect (logShow { idx, rem, vs }) in
              rem == idx
                || ((fst <$> (vs !! 0)) == Just rem && (fst <$> (vs !! 1)) == Just idx)
                ||
                  ((fst <$> (vs !! 0)) == Just idx)

        element = fixed $
          let
            renderItems = sampleOn (Array.length <$> currentValue) $
              (initialEvent <|> addEvent) <#> \(idx /\ item) len ->
                ( bang $ insert $ fixed $ append
                    (if len > 0 && idx /= 0 then [ switcher (fixed <<< if _ then [] else sep) (bang false <|> dropComma idx) ] else [])
                    (renderOne (idx /\ item))
                ) <|> filter (eq idx) removesEvent $> remove
          in
            intro <> [ D.span_ [ dyn renderItems ] ] <> extro <> fin
      in
        { element, value: map snd <$> currentValue }
