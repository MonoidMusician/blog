module Parser.Debug where

import Prelude

import Data.Argonaut (stringifyWithIndent)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

spie :: forall a2. a2 -> a2
spie = (\v -> unsafePerformEffect (v <$ log (stringifyWithIndent 2 (unsafeCoerce v))))

spieName :: forall a2. String -> a2 -> a2
spieName name = (\v -> unsafePerformEffect (v <$ log (name <> ": " <> stringifyWithIndent 2 (unsafeCoerce v))))

foreign import thingy :: Int
