module FRP.NotFrom where

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import FRP.Event (Event, filterMap, sampleOnRight_)
import Prelude (class Eq, identity, (<$), (<$>))

-- | Filter out events from one source from a larger event. That is, given
-- | events `e1` and `e2`, the following equation ought to hold:
-- |
-- | ```purescript
-- | notFrom e1 (e1 <|> e2) = e2 = notFrom e1 (e2 <|> e1)
-- | ```
-- |
-- | This relies on the events being well-behaved, mostly that they broadcast
-- | the same event to all of their subscribers in order of subscription.
notFrom :: forall a. Eq a => Event a -> Event a -> Event a
notFrom suspect downstream =
  -- Subscribe to `downstream` first, so events coming from `suspect` will
  -- come in later and take precedence on the left side of `sampleOn_`
  let instigator = Just <$> downstream <|> Nothing <$ suspect
  -- Only respond to events from downstream, of course, but with the above
  -- filtering
  in filterMap identity (sampleOnRight_ instigator downstream)
