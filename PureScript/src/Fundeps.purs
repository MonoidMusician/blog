module Fundeps where

import Prelude
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- This class can only have a single instance due to the functional dependency
class SingleInstanceFundep (r :: Row Type) | -> r where
  unified :: Proxy r

instance SingleInstanceFundep ( x :: Unit ) where
  unified = Proxy

-- This should infer `test :: Proxy ( x :: Unit )` by committing to the instance
test :: Proxy _
test = unified

main = do
  let (Proxy :: Proxy ( x :: Unit )) = test
  log "Done"
