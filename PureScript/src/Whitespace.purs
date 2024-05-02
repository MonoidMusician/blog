module Whitespace where

import Prelude

import Data.HeytingAlgebra (ff, tt)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Tuple.Nested (type (/\))
import Safe.Coerce (coerce)

-- | whitespace can be:
-- | 0. failed (if you try to concatenate forbidden with required, for example … it's cleanest to just include it in the semilattice)
-- | 0. forbidden
-- | 0. space allowed, no newlines
-- | 0. newline allowed, not just a space
-- | 0. whitespace allowed
-- | 0. space preferred, no newlines
-- | 0. space preferred, newlines allowed
-- | 0. soft break preferred (print nothing or newline, depending on layout)
-- | 0. space break preferred
-- | 0. prefer newline, spaces allowed
-- | 0. … I don't think prefer newline, spaces not allowed makes sense?? idk
-- | 0. require space, don't allow newline
-- | 0. require space, allow newline
-- | 0. required space break
-- | 0. print newline, require space or newline
-- | 0. require newline
data WS
  = Failed
  | Forbidden
  | AllowedSpace
  | AllowedNewline
  | Allowed
  | SpacePreferred Boolean
  | SoftBreakPreferred
  | SpaceBreakPreferred
  | BreakPreferred Boolean
  | RequireSpace Boolean
  | RequireSpaceBreak
  | RequireNewline Boolean

wsProps :: WS ->
  { allowed_newline :: Conj Boolean
  , allowed_space :: Conj Boolean
  , required :: Disj Boolean
  }
wsProps = coerce <<< case _ of
  Failed ->
    { allowed_newline: false
    , allowed_space: false
    , required: true
    }
  Forbidden -> forbidden
  AllowedSpace -> forbidden { allowed_space = true }
  AllowedNewline -> forbidden { allowed_newline = true }
  Allowed -> forbidden { allowed_newline = true, allowed_space = true }
  SpacePreferred allowed_newline -> forbidden { allowed_newline = allowed_newline, allowed_space = true }
  SoftBreakPreferred -> forbidden { allowed_newline = true, allowed_space = true }
  SpaceBreakPreferred -> forbidden { allowed_newline = true, allowed_space = true }
  BreakPreferred allowed_space -> forbidden { allowed_newline = true, allowed_space = allowed_space }
  RequireSpace allowed_newline -> required { allowed_newline = allowed_newline }
  RequireSpaceBreak -> required
  RequireNewline allowed_space -> required { allowed_space = allowed_space }
  where
  forbidden ::
    { allowed_newline :: Boolean
    , allowed_space :: Boolean
    , required :: Boolean
    }
  forbidden = ff
  required ::
    { allowed_newline :: Boolean
    , allowed_space :: Boolean
    , required :: Boolean
    }
  required = tt

data RenderWS = NoWS | Space | SoftBreak | SpaceBreak | Break

instance semigroupRenderWS :: Semigroup RenderWS where
  -- identity
  append NoWS a = a
  append a NoWS = a

  -- idempotent
  append Space Space = Space
  append SoftBreak SoftBreak = SoftBreak
  append SpaceBreak SpaceBreak = SpaceBreak
  append Break Break = Break

  append Break _ = Break
  append _ Break = Break

  append _ _ = SpaceBreak

wsRender :: WS -> RenderWS
wsRender Failed = NoWS
wsRender Forbidden = NoWS
wsRender AllowedSpace = NoWS
wsRender AllowedNewline = NoWS
wsRender Allowed = NoWS
wsRender (SpacePreferred _) = Space
wsRender SoftBreakPreferred = SoftBreak
wsRender SpaceBreakPreferred = SpaceBreak
wsRender (BreakPreferred _) = Break
wsRender (RequireSpace _) = Space
wsRender RequireSpaceBreak = SpaceBreak
wsRender (RequireNewline _) = Break

fromStuff ::
  { allowed_newline :: Conj Boolean
  , allowed_space :: Conj Boolean
  , required :: Disj Boolean
  } ->
  RenderWS ->
  WS
fromStuff = coerce >>> case _ of
  r | isFailed r -> const Failed
  { allowed_newline: false, allowed_space: false } -> const Forbidden
  { required: true, allowed_newline: false } -> const (RequireSpace false)
  { required: true, allowed_space: false } -> const (RequireNewline false)
  { required: true } -> case _ of
    NoWS -> RequireSpace true
    Space -> RequireSpace true
    SoftBreak -> RequireSpaceBreak
    SpaceBreak -> RequireSpaceBreak
    Break -> RequireNewline true
  -- required: false
  { allowed_newline: true, allowed_space: true } -> case _ of
    NoWS -> Allowed
    Space -> SpacePreferred true
    SoftBreak -> SoftBreakPreferred
    SpaceBreak -> SpaceBreakPreferred
    Break -> BreakPreferred true
  { allowed_newline: true, allowed_space: false } -> case _ of
    NoWS -> AllowedNewline
    SoftBreak -> SoftBreakPreferred
    _ -> BreakPreferred false
  { allowed_space: true, allowed_newline: false } -> case _ of
    NoWS -> AllowedSpace
    _ -> SpacePreferred false
  where
  isFailed ::
    { allowed_newline :: Boolean
    , allowed_space :: Boolean
    , required :: Boolean
    } -> Boolean
  isFailed { allowed_newline: false, required: true } = true
  isFailed { allowed_space: false, required: true } = true
  isFailed _ = false

instance semigroupWS :: Semigroup WS where
  append a b = fromStuff (wsProps a <> wsProps b) (wsRender a <> wsRender b)

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
