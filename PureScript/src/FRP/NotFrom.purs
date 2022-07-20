module FRP.NotFrom where

import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST)
import Data.Maybe (Maybe(..))
import FRP.Event (AnEvent, filterMap, sampleOn_)
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
notFrom :: forall s m a. MonadST s m => Eq a => AnEvent m a -> AnEvent m a -> AnEvent m a
notFrom suspect downstream =
  -- Subscribe to `downstream` first, so events coming from `suspect` will
  -- come in later and take precedence on the left side of `sampleOn_`
  let instigator = Just <$> downstream <|> Nothing <$ suspect
  -- Only respond to events from downstream, of course, but with the above
  -- filtering
  in filterMap identity (sampleOn_ instigator downstream)
