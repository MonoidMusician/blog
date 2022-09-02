module FRP.Deku.Component where

import Prelude

import Bolson.Core (envy)
import Deku.Core (Domable)
import FRP.Event (Event, keepLatest, memoize)


-- | Abstract component
type ComponentSpec lock payload d =
  Event (Component lock payload d)

-- | Instantiated component
type Component lock payload d =
  { element :: Domable lock payload
  , value :: Event d
  }

-- | Instantiate a component spec to get an actual component, with its element
-- | and value-event.
-- |
-- | Component specs are eventful so they can maintain state, so they need to
-- | be memoized in order that the element and value refer to the same instance,
-- | otherwise the value is attached to a phantom instance that has no DOM
-- | presence, due to the way busses and subscriptions work.
withInstance
  :: forall d lock payload
   . ComponentSpec lock payload d
  -> (Component lock payload d -> Domable lock payload)
  -> Domable lock payload
withInstance componentSpec renderer =
  envy $ memoize componentSpec \component ->
    renderer
      { element: envy (component <#> _.element)
      , value: keepLatest (component <#> _.value)
      }
