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
import Deku.Core (dyn, fixed)
import Deku.Core (Domable, bus, insert_, remove)
import Deku.DOM as D
import Effect (Effect)
import FRP.Deku.Component (ComponentSpec)
import FRP.Event (Event, keepLatest, sampleOn)
import FRP.Helpers (counter)
import FRP.Memoize (memopureFold)
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
  :: forall lock payload a
   . Show a
  => { initial :: Array a -- initial value
     , addEvent :: Event a -- external add events

     , remove :: Maybe (Effect Unit -> Domable lock payload) -- remove button
     , finalize :: Maybe (Event (Array a) -> Domable lock payload) -- finalize button

     , renderItem :: a -> Domable lock payload
     , begin :: Maybe (Domable lock payload)
     , end :: Maybe (Domable lock payload)
     , separator :: Maybe (Domable lock payload)
     }
  -> ComponentSpec lock payload (Array a)
listicle desc = keepLatest $ bus \pushRemove removesEvent ->
  let
    addEvent = counter desc.addEvent <#> \(v /\ i) -> (i + Array.length desc.initial) /\ v
    initialEvent = oneOfMap pure initialValue

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
    memopureFold performChange changesEvent initialValue \currentValue ->
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

        withRemover :: Domable lock payload -> Int -> Array (Domable lock payload)
        withRemover item idx = case desc.remove of
          Nothing -> [ item ]
          Just remover ->
            [ item, remover (pushRemove idx) ]

        renderOne :: Int /\ a -> Array (Domable lock payload)
        renderOne (idx /\ item) = withRemover (desc.renderItem item) idx

        dropComma :: Int -> Event Boolean
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
                ( pure $ insert_ $ fixed $ append
                    (if len > 0 && idx /= 0 then [ switcher (fixed <<< if _ then [] else sep) (pure false <|> dropComma idx) ] else [])
                    (renderOne (idx /\ item))
                ) <|> filter (eq idx) removesEvent $> remove
          in
            intro <> [  dyn renderItems  ] <> extro <> fin
      in
        { element, value: map snd <$> currentValue }
