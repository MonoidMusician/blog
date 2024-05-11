module Whitespace where

import Prelude

import Data.HeytingAlgebra (ff, tt)
import Data.Lens as O
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Dodo as Dodo
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

instance monoidRenderWS :: Monoid RenderWS where
  mempty = NoWS
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

toDoc :: forall ann. RenderWS -> Dodo.Doc ann
toDoc = case _ of
  NoWS -> mempty
  Space -> Dodo.space
  SoftBreak -> Dodo.softBreak
  SpaceBreak -> Dodo.spaceBreak
  Break -> Dodo.break
data WSDoc ann
  = JustWS RenderWS
  | WSDoc
    { before :: RenderWS
    , doc :: Dodo.Doc ann
    , after :: RenderWS
    }
derive instance functorWSDoc :: Functor WSDoc
instance monoidWSDoc :: Monoid (WSDoc ann) where
  mempty = JustWS mempty
instance semigroupWSDoc :: Semigroup (WSDoc ann) where
  append (JustWS w1) (JustWS w2) = JustWS (w1 <> w2)
  append (JustWS w1) (WSDoc d2) = WSDoc d2 { before = w1 <> d2.before }
  append (WSDoc d1) (JustWS w2) = WSDoc d1 { after = d1.after <> w2 }
  append (WSDoc d1) (WSDoc d2) = WSDoc
    { before: d1.before
    , doc: d1.doc <> toDoc (d1.after <> d2.before) <> d2.doc
    , after: d2.after
    }

-- Not quite a lawful lens
_wsDoc :: forall ann ann'. O.Lens (WSDoc ann) (WSDoc ann') (Dodo.Doc ann) (Dodo.Doc ann')
_wsDoc = O.lens' case _ of
  JustWS ws -> Tuple mempty (const (JustWS ws))
  WSDoc d -> Tuple d.doc \doc ->
    if Dodo.isEmpty doc
      then JustWS (d.before <> d.after)
      else WSDoc d { doc = doc }

docWS :: forall ann. Dodo.Doc ann -> WSDoc ann
docWS doc | Dodo.isEmpty doc = mempty
docWS doc = WSDoc { before: mempty, doc, after: mempty }

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
