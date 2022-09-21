module Test.Main where

import Prelude

import Data.Symbol (reflectSymbol, reifySymbol)
import Effect (Effect)
import Effect.Console (log)
import Partial (crash)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

{-
main :: Effect Unit
main = do
  let x = \_ -> unsafePartial $ crash "this is an error"
  pure unit
-}

{-
main :: Effect Unit
main = do
  reifySymbol "TESTER" \p -> do
    log (reflectSymbol p)
  pure unit
-}

class MyPartial

myCrash :: forall a. MyPartial => a
myCrash = unsafeCrashWith "myCrash"

unsafeMyPartial :: forall a. (MyPartial => a) -> a
unsafeMyPartial f = ((unsafeCoerce :: (MyPartial => a) -> (Unit -> a)) f) unit

main :: Effect Unit
main = unsafeMyPartial do
  let x = \_ -> myCrash
  pure unit
