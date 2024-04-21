module Whitespace where

import Prelude

import Data.Tuple.Nested (type (/\))

data WS
  = WSRequired
  | WSDisallowed
  | WSPreferred
  | WSPossible
  | WSFailed

class FromWSF :: (Type -> Type) -> Constraint
class FromWSF f where
  infixWSF :: forall a b. f a -> WS -> f b -> f (a /\ b)
  circumfixWSF :: forall a. WS -> f a -> WS -> f a

class FromWSP :: (Type -> Type -> Type) -> Constraint
class FromWSP p where
  infixWSP :: forall u v x y. p u x -> WS -> p v y -> p (u /\ v) (x /\ y)
  circumfixWSP :: forall u x. WS -> p u x -> WS -> p u x

newtype Boundary = Bdry
  { left :: WS
  , right :: WS
  }

newtype WithBoundaryF :: (Type -> Type) -> Type -> Type
newtype WithBoundaryF f a = BddF
  { left :: WS
  , wrapped :: f a
  , right :: WS
  }

newtype WithBoundaryP :: (Type -> Type -> Type) -> Type -> Type -> Type
newtype WithBoundaryP p u x = BddP
  { left :: WS
  , wrapped :: p u x
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
