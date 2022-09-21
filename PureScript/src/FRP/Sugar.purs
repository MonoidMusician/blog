module FRP.Sugar where

import Prelude
import Data.Tuple.Nested ((/\), type (/\))

liftTuple :: forall a b m. Applicative m => m a -> m b -> m (a /\ b)
liftTuple ma mb = (/\) <$> ma <*> mb

infixr 5 liftTuple as </\>
