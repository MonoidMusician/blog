module Parser.Debug where

import Prelude

import Data.Argonaut (stringifyWithIndent)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

spie :: forall a2. a2 -> a2
spie = (\v -> unsafePerformEffect (v <$ log (stringifyWithIndent 2 (unsafeCoerce v))))

spieName :: forall a2. String -> a2 -> a2
spieName name = (\v -> unsafePerformEffect (v <$ log (name <> ": " <> stringifyWithIndent 2 (unsafeCoerce v))))

foreign import thingy :: Int

perf :: forall a b. String -> (a -> b) -> a -> b
perf lbl f a = unsafePerformEffect do
  t0 <- liftEffect now
  let b = f a
  t1 <- liftEffect now
  log $ lbl <> ": " <> show (unwrap (unInstant t1) - unwrap (unInstant t0)) <> " milliseconds"
  pure b
