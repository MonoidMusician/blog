module Whitespace where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.HeytingAlgebra (ff, tt)
import Data.Lens as Q
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\))
import Dodo as Dodo
import Parser.Selective (class Select, select)
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

newtype ParseWS = ParseWS
  { allowed_newline :: Conj Boolean
  , allowed_space :: Conj Boolean
  , required :: Disj Boolean
  }

mkParseWS ::
  { allowed_newline :: Boolean
  , allowed_space :: Boolean
  , required :: Boolean
  } -> ParseWS
mkParseWS = coerce

unParseWS ::
  ParseWS ->
  { allowed_newline :: Boolean
  , allowed_space :: Boolean
  , required :: Boolean
  }
unParseWS = coerce

derive instance newtypeParseWS :: Newtype ParseWS _
derive newtype instance monoidParseWS :: Monoid ParseWS
derive newtype instance semigroupParseWS :: Semigroup ParseWS
derive newtype instance showParseWS :: Show ParseWS -- TODO
derive newtype instance eqParseWS :: Eq ParseWS
derive newtype instance ordParseWS :: Ord ParseWS

instance semiringParseWS :: Semiring ParseWS where
  one = mempty
  mul = append
  zero = coerce { allowed_newline: false, allowed_space: false, required: true }
  add (ParseWS l) (ParseWS r) = ParseWS
    { allowed_newline: Conj do unwrap l.allowed_newline || unwrap r.allowed_newline
    , allowed_space: Conj do unwrap l.allowed_space || unwrap r.allowed_space
    , required: Disj do unwrap l.required && unwrap r.required
    }

wsProps :: WS -> ParseWS
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

renderDoc :: forall ann. WSDoc ann -> Dodo.Doc ann
renderDoc (JustWS r) = toDoc r
renderDoc (WSDoc { before, doc, after }) = toDoc before <> doc <> toDoc after

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
_wsDoc :: forall ann ann'. Q.Lens (WSDoc ann) (WSDoc ann') (Dodo.Doc ann) (Dodo.Doc ann')
_wsDoc = Q.lens' case _ of
  JustWS ws -> Tuple mempty (const (JustWS ws))
  WSDoc d -> Tuple d.doc \doc ->
    if Dodo.isEmpty doc
      then JustWS (d.before <> d.after)
      else WSDoc d { doc = doc }

docWS :: forall ann. Dodo.Doc ann -> WSDoc ann
docWS doc | Dodo.isEmpty doc = mempty
docWS doc = WSDoc { before: mempty, doc, after: mempty }

fromStuff ::
  ParseWS ->
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
  -- FIXME
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
instance monoidWS :: Monoid WS where
  mempty = Allowed

class FromWSF :: (Type -> Type) -> Constraint
class FromWSF f where
  infixWSF :: forall a b. f a -> WS -> f b -> f (a /\ b)
  circumfixWSF :: forall a. WS -> f a -> WS -> f a
  pureWSF :: WS -> f Unit
  neverWSF :: f Void

class FromWSP :: (Type -> Type -> Type) -> Constraint
class FromWSP p where
  infixWSP :: forall u v x y. p u x -> WS -> p v y -> p (u /\ v) (x /\ y)
  circumfixWSP :: forall u x. WS -> p u x -> WS -> p u x
  pureWSP :: WS -> p Unit Unit
  neverWSP :: p Void Void

newtype Boundary = Bdry
  { left :: WS
  , right :: WS
  }

wsF :: forall f. WS -> WithBoundaryF f Unit
wsF ws = WSF ws identity identity

noBoundaryF :: forall f a. f a -> WithBoundaryF f a
noBoundaryF wrapped = BddF { left: mempty, wrapped, right: mempty }

withBoundaryF :: forall f a. FromWSF f => WithBoundaryF f a -> f a
withBoundaryF (WSF ws proof _) = proof (pureWSF ws)
withBoundaryF (EmptyF proof) = proof neverWSF
withBoundaryF (BddF r) = circumfixWSF r.left r.wrapped r.right

_bddF :: forall f a b. Functor f => (f a -> f b) -> WithBoundaryF f a -> WithBoundaryF f b
_bddF f (WSF ws proof _) = WSF ws (f <<< proof) void
_bddF _ (EmptyF _) = EmptyF (map absurd)
_bddF f (BddF r) = BddF r { wrapped = f r.wrapped }

data WithBoundaryF :: (Type -> Type) -> Type -> Type
data WithBoundaryF f a
  = WSF WS (f Unit -> f a) (f a -> f Unit)
  | EmptyF (f Void -> f a)
  | BddF
    { left :: WS
    , wrapped :: f a
    , right :: WS
    }

instance functorWithBoundaryF :: Functor f => Functor (WithBoundaryF f) where
  map f (WSF ws proof _) = WSF ws (map f <<< proof) void
  map _ (EmptyF _) = EmptyF (map absurd)
  map f (BddF r) = BddF r { wrapped = map f r.wrapped }
instance applyWithBoundaryF :: (FromWSF f, Applicative f) => Apply (WithBoundaryF f) where
  apply (EmptyF _) _ = EmptyF (map absurd)
  apply _ (EmptyF _) = EmptyF (map absurd)
  apply (WSF ws1 proof1 _) (WSF ws2 proof2 _) =
    WSF (ws1 <> ws2) ((<*>) <$> proof1 <*> proof2) void
  apply (WSF ws proof _) (BddF r) = BddF r
    { left = ws <> r.left
    , wrapped = proof (pure unit) <*> r.wrapped
    }
  apply (BddF r) (WSF ws proof _) = BddF r
    { right = r.right <> ws
    , wrapped = r.wrapped <*> proof (pure unit)
    }
  apply (BddF r1) (BddF r2) = BddF
    { left: r1.left
    , wrapped: map (uncurry ($)) $ infixWSF r1.wrapped (r1.right <> r2.left) r2.wrapped
    , right: r2.right
    }
instance applicativeWithBoundaryF :: (FromWSF f, Applicative f) => Applicative (WithBoundaryF f) where
  pure a = WSF mempty (a <$ _) void
instance altWithBoundaryF :: (FromWSF f, Alt f) => Alt (WithBoundaryF f) where
  alt (EmptyF _) r = r
  alt l (EmptyF _) = l
  alt l r = noBoundaryF $ withBoundaryF l <|> withBoundaryF r -- FIXME
instance plusWithBoundaryF :: (FromWSF f, Alt f) => Plus (WithBoundaryF f) where
  empty = EmptyF (map absurd)
instance selectWithBoundaryF :: (FromWSF f, Select f, Applicative f) => Select (WithBoundaryF f) where
  select (EmptyF _) _ = EmptyF (map absurd)
  select l (EmptyF _) = select l (noBoundaryF (map absurd neverWSF))
  select (WSF ws1 proof1 _) (WSF ws2 proof2 _) = WSF (ws1 <> ws2) (lift2 select proof1 proof2) void
  select (WSF ws proof _) (BddF r) = BddF r
    { left = ws <> r.left
    , wrapped = select (proof (pure unit)) r.wrapped
    }
  select (BddF r) (WSF ws proof _) = BddF r
    { right = r.right <> ws
    , wrapped = select r.wrapped (proof (pure unit))
    }
  select (BddF r1) (BddF r2) = BddF
    { left: r1.left
    , wrapped: select r1.wrapped r2.wrapped
    , right: r2.right -- FIXME
    }

data WithBoundaryP :: (Type -> Type -> Type) -> Type -> Type -> Type
data WithBoundaryP p u x
  = WSP WS (forall q. q Unit Unit -> q u x)
  | BddP
    { left :: WS
    , wrapped :: p u x
    , right :: WS
    }

data MaybeWS space
  = DefaultWS
  | SetWS space
  | SetOrDefaultWS space

derive instance eqMaybeWS :: Eq space => Eq (MaybeWS space)
derive instance ordMaybeWS :: Ord space => Ord (MaybeWS space)

instance showMaybeWS :: Show space => Show (MaybeWS space) where
  show DefaultWS = "DefaultWS"
  show (SetWS space) = "(SetWS " <> show space <> ")"
  show (SetOrDefaultWS space) = "(SetOrDefaultWS " <> show space <> ")"

instance semiringMaybeWS :: Semiring space => Semiring (MaybeWS space) where
  zero = SetWS zero
  one = DefaultWS

  add DefaultWS DefaultWS = DefaultWS
  add DefaultWS (SetWS space) = SetOrDefaultWS space
  add (SetWS space) DefaultWS = SetOrDefaultWS space
  add DefaultWS (SetOrDefaultWS space) = SetOrDefaultWS space
  add (SetOrDefaultWS space) DefaultWS = SetOrDefaultWS space
  add (SetWS s1) (SetWS s2) = SetWS (s1 + s2)
  add (SetOrDefaultWS s1) (SetWS s2) = SetOrDefaultWS (s1 + s2)
  add (SetWS s1) (SetOrDefaultWS s2) = SetOrDefaultWS (s1 + s2)
  add (SetOrDefaultWS s1) (SetOrDefaultWS s2) = SetOrDefaultWS (s1 + s2)

  mul DefaultWS x = x
  mul x DefaultWS = x
  mul (SetWS s1) (SetWS s2) = SetWS (s1 * s2)
  mul (SetOrDefaultWS s1) (SetWS s2) = SetOrDefaultWS (s1 * s2 + s2)
  mul (SetWS s1) (SetOrDefaultWS s2) = SetOrDefaultWS (s1 + s1 * s2)
  mul (SetOrDefaultWS s1) (SetOrDefaultWS s2) = SetOrDefaultWS (s1 + s2 + s1 * s2)

class HasDefaultWS space where
  defaultWS :: space

instance defaultWSParseWS :: HasDefaultWS ParseWS where
  defaultWS = mkParseWS ff
