{-# LANGUAGE TypeFamilies, QuantifiedConstraints, DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
module MTLite where

import Prelude hiding (id, (.))
import Data.Constraint
import Control.Category (Category(..))
import Control.Monad ((>=>))
import Data.Kind (Type)
import qualified Control.Monad.Reader.Class as MR
import qualified Control.Monad.State.Class as MS
import qualified Control.Monad.Cont.Class as MC
import Control.Applicative (Applicative(liftA2))
import Data.Profunctor (Profunctor (rmap, lmap), Strong (first', second'), Choice (right', left'))
import Data.Proxy (Proxy (Proxy))

-- | Trying to do MTL in the style of algebraic effects, for lightweight surface
-- | syntax (in tmTTmt, not Haskell [it will be verbose in Haskell]).
type K :: ((Type -> Type) -> Constraint) -> Type -> Type -> Type
newtype K c a b =
  Kleislite (forall m. c m => a -> m b)

instance (Functor <:= c) => Functor (K c i) where
  fmap :: forall x y. (x -> y) -> K c i x -> K c i y
  fmap g (Kleislite f) = Kleislite h
    where
    h :: forall m. c m => i -> m y
    h = case gatherSub :: c m :- Functor m of
      Sub Dict ->
        fmap g . f

instance (Functor <:= c) => Profunctor (K c) where
  lmap :: forall x y o. (y -> x) -> K c x o -> K c y o
  lmap g (Kleislite f) = Kleislite (f . g)

  rmap :: forall i x y. (x -> y) -> K c i x -> K c i y
  rmap = fmap

instance (Functor && Applicative <:= c) => Applicative (K c i) where
  pure :: forall x. x -> K c i x
  pure x = Kleislite f
    where
    f :: forall m. c m => i -> m x
    f = case gatherSub :: c m :- Applicative m of
      Sub Dict ->
        const (pure x)

  (<*>) :: forall x y. K c i (x -> y) -> K c i x -> K c i y
  Kleislite f <*> Kleislite g = Kleislite h
    where
    h :: forall m. c m => i -> m y
    h = case gatherSub :: c m :- Applicative m of
      Sub Dict -> \i -> f i <*> g i

instance (Functor && Applicative && Monad <:= c) => Monad (K c i) where
  (>>=) :: forall x y. K c i x -> (x -> K c i y) -> K c i y
  Kleislite f >>= g = Kleislite h
    where
    h :: forall m. c m => i -> m y
    h = case gatherSub :: c m :- Monad m of
      Sub Dict ->
        \i -> f i >>= \x ->
          case g x of
            Kleislite k ->
              k i

instance (Monad <:= c) => Category (K c) where
  id :: forall x. K c x x
  id = Kleislite f
    where
    f :: forall m. c m => x -> m x
    f = case gatherSub :: c m :- Monad m of
      Sub Dict ->
        pure

  (.) :: forall x y z. K c y z -> K c x y -> K c x z
  Kleislite g . Kleislite f = Kleislite h
    where
    h :: forall m. c m => x -> m z
    h = case gatherSub :: c m :- Monad m of
      Sub Dict ->
        f >=> g

instance (Functor <:= c) => Strong (K c) where
  first' :: forall x y z. K c x y -> K c (x, z) (y, z)
  first' (Kleislite f) = Kleislite g
    where
    g :: forall m. c m => (x, z) -> m (y, z)
    g = case gatherSub :: c m :- Functor m of
      Sub Dict -> \(x, z) -> fmap (,z) (f x)

  second' :: forall x y z. K c x y -> K c (z, x) (z, y)
  second' (Kleislite f) = Kleislite g
    where
    g :: forall m. c m => (z, x) -> m (z, y)
    g = case gatherSub :: c m :- Functor m of
      Sub Dict -> \(z, x) -> fmap (z,) (f x)

instance (Functor && Applicative <:= c) => Choice (K c) where
  left' :: forall x y z. K c x y -> K c (Either x z) (Either y z)
  left' (Kleislite f) = Kleislite g
    where
    g :: forall m. c m => Either x z -> m (Either y z)
    g = case gatherSub :: c m :- Applicative m of
      Sub Dict -> either (fmap Left . f) (pure . Right)

  right' :: forall x y z. K c x y -> K c (Either z x) (Either z y)
  right' (Kleislite f) = Kleislite g
    where
    g :: forall m. c m => Either z x -> m (Either z y)
    g = case gatherSub :: c m :- Applicative m of
      Sub Dict -> either (pure . Left) (fmap Right . f)



infixr 6 &&
class (c1 f, c2 f) => (c1 && c2) f
instance (c1 f, c2 f) => (c1 && c2) f

instance Class (c1 f, c2 f) ((c1 && c2) f) where
  cls = Sub Dict

instance Class (c1, c2) (c1 & c2) where
  cls = Sub Dict



-- API

infix 2 <:=
type (<:=) :: (k -> Constraint) -> (k -> Constraint) -> Constraint
type family (query <:= c) where
  (q1 && q2) <:= c = (q1 <:= c, q2 <:= c)
  query <:= c = Gather query c

-- `(forall m. c m => query m)` as a superclass breaks stuff above
-- because then Haskell wants to look for *every* `query m` as deriving from
-- `c m`, instead of global instances... I think this is a bug/limitation?
class Gather query c where
  gatherSub :: forall m. c m :- query m
instance (forall m. c m => query m) => Gather query c where
  gatherSub = Sub Dict


-- Examples

state :: K (MS.MonadState s && q) (s -> (a, s)) a
state = Kleislite MS.state

asks :: K (MR.MonadReader r && q) (r -> y) y
asks = Kleislite MR.asks

local :: (r -> r) -> K (MR.MonadReader r && q) i o -> K (MR.MonadReader r && q) i o
local f (Kleislite g) = Kleislite (MR.local f . g)

localStmt :: (r -> r) -> K (MR.MonadReader r && q) (K (MR.MonadReader r && q) () o) o
localStmt f = Kleislite \(Kleislite g) -> MR.local f (g ())

asksOfAsks :: K (MR.MonadReader r && q) (r -> r -> c) c
asksOfAsks = asks . asks

asksAndAsks :: K (MR.MonadReader r && q) (r -> c) (c, c)
asksAndAsks = liftA2 (,) asks asks

class Like m n where
  fwd :: forall a. n a -> m a
  bwd :: forall a. m a -> n a

like :: forall m x y a. Like m x => Like m y => Proxy m -> x a -> y a
like _ = (bwd :: m a -> y a) . (fwd :: x a -> m a)

callCC :: forall m q a b. K (Like m && MC.MonadCont && q) (K (Like m && MC.MonadCont && q) (K (Like m) a b) a) a
callCC = Kleislite \(Kleislite useCont) ->
  MC.callCC \cont ->
    useCont $ Kleislite $ like (Proxy @m) . cont
