module Whitespace where

import Prelude

import Data.Tuple.Nested (type (/\))

data WS
  = WSRequired
  | WSDisallowed
  | WSPreferred
  | WSPossible
  | WSFailed

class FromWS :: (Type -> Type) -> Constraint
class FromWS f where
  infixWS :: forall a b. f a -> WS -> f b -> f (a /\ b)
  circumfixWS :: forall a. WS -> f a -> WS -> f a

newtype Boundary = Bdry
  { left :: WS
  , right :: WS
  }

newtype WithBoundary :: (Type -> Type) -> Type -> Type
newtype WithBoundary f a = Bdd
  { left :: WS
  , wrapped :: f a
  , right :: WS
  }

instance semigroupWS :: Semigroup WS where
  append = case _, _ of
    WSFailed, _ -> WSFailed
    _, WSFailed -> WSFailed
    WSRequired, WSDisallowed -> WSFailed
    WSDisallowed, WSRequired -> WSFailed
    WSRequired, _ -> WSRequired
    _, WSRequired -> WSRequired
    WSDisallowed, _ -> WSDisallowed
    _, WSDisallowed -> WSDisallowed
    WSPreferred, _ -> WSPreferred
    _, WSPreferred -> WSPreferred
    WSPossible, WSPossible -> WSPossible

instance monoidWS :: Monoid WS where
  mempty = WSPossible
