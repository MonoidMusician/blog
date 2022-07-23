module FRP.Deku.Component where

import Prelude

import Bolson.Core (envy)
import Deku.Core (class Korok, Domable)
import FRP.Event (AnEvent, keepLatest, memoize)


-- | Abstract component
type ComponentSpec m lock payload d =
  AnEvent m (Component m lock payload d)

-- | Instantiated component
type Component m lock payload d =
  { element :: Domable m lock payload
  , value :: AnEvent m d
  }

-- | Instantiate a component spec to get an actual component, with its element
-- | and value-event.
-- |
-- | Component specs are eventful so they can maintain state, so they need to
-- | be memoized in order that the element and value refer to the same instance,
-- | otherwise the value is attached to a phantom instance that has no DOM
-- | presence, due to the way busses and subscriptions work.
withInstance
  :: forall s d m lock payload
   . Korok s m
  => ComponentSpec m lock payload d
  -> (Component m lock payload d -> Domable m lock payload)
  -> Domable m lock payload
withInstance componentSpec renderer =
  envy $ memoize componentSpec \component ->
    renderer
      { element: envy (component <#> _.element)
      , value: keepLatest (component <#> _.value)
      }
