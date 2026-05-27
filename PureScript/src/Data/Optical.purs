module Data.Optical ( module Data.Optical ) where

import Prelude

import Control.Monad.State (class MonadState, gets, modify_)
import Data.Either (Either(..))
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VariantF
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Lens (Forget, _Just, preview, view)
import Data.Lens as Optic
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row as Row
import Type.Proxy (Proxy)

class Optical :: Type -> (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class Profunctor p <= Optical path p s t a b | path s t -> a b where
  _Optic :: path -> p a b -> p s t

getAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (Forget a) s s a a =>
  path -> m a
getAt path = gets (view (_Optic @path @_ @s @s path))

findAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (Forget (First a)) s s a a =>
  path -> m (Maybe a)
findAt path = gets (preview (_Optic @path @_ @s @s path))

ofAt ::
  forall path s a m z.
    MonadState s m =>
    Optical path (Forget a) s s a a =>
  (a -> z) -> path -> m z
ofAt f path = gets (f <<< view (_Optic @path @_ @s @s path))
infix 1 ofAt as $@

fromAt ::
  forall path s a m z.
    MonadState s m =>
    Optical path (Forget a) s s a a =>
  path -> (a -> z) -> m z
fromAt = flip ofAt
infix 1 fromAt as @#

setAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (->) s s a a =>
  path -> a -> m Unit
setAt path value = modifyAt path (const value)

swapAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (Forget a) s s a a =>
    Optical path (->) s s a a =>
  path -> a -> m a
swapAt path value = getAt path <* modifyAt path (const value)

trySwapAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (Forget (First a)) s s a a =>
    Optical path (->) s s a a =>
  path -> a -> m (Maybe a)
trySwapAt path value = findAt path <* modifyAt path (const value)

modifyAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (->) s s a a =>
  path -> (a -> a) -> m Unit
modifyAt path fn = modify_ (_Optic path fn)

swapModifyAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (Forget a) s s a a =>
    Optical path (->) s s a a =>
  path -> (a -> a) -> m a
swapModifyAt path fn = getAt path <* modifyAt path fn

trySwapModifyAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (Forget (First a)) s s a a =>
    Optical path (->) s s a a =>
  path -> (a -> a) -> m (Maybe a)
trySwapModifyAt path fn = findAt path <* modifyAt path fn

appendAt ::
  forall path s a m.
    MonadState s m =>
    Optical path (->) s s a a =>
    Monoid a =>
  path -> a -> m Unit
appendAt path value = modifyAt path (_ <> value)

infix 1 setAt as @=
infix 1 swapAt as <@=
infix 1 trySwapAt as <?@=
infix 1 modifyAt as @~
infix 1 swapModifyAt as <@~
infix 1 trySwapModifyAt as <?@~
infix 1 appendAt as @<>



instance
  ( Optical path1 p s t q r
  , Optical path2 p q r a b
  ) => Optical (Tuple path1 path2) p s t a b where
    _Optic (Tuple path1 path2) = _Optic path1 <<< _Optic path2


instance (Strong p, Choice p) => Optical Int p (Array t) (Array t) t t where
  _Optic i = ix i

instance (Strong p, Choice p) => Optical Int p (List.List t) (List.List t) t t where
  _Optic i = ix i

instance Choice p => Optical Unit p (Maybe a) (Maybe b) a b where
  _Optic _ = _Just

instance Profunctor p => Optical Unit p (Identity a) (Identity b) a b where
  _Optic _ = _Newtype


newtype Key key = Key key
derive instance Newtype (Key key) _
derive instance Generic (Key key) _
derive newtype instance Eq key => Eq (Key key)
derive newtype instance Ord key => Ord (Key key)

instance (Ord key, Strong p) => Optical (Key key) p (Map.Map key t) (Map.Map key t) (Maybe t) (Maybe t) where
  _Optic (Key key) = at key


data Each = Each
derive instance Eq Each
derive instance Ord Each

instance Functor f => Optical Each (->) (f t) (f t) t t where
  _Optic Each = map



instance OpticalProxy k path p s t a b => Optical (Proxy ((path) :: k)) p s t a b where
  _Optic = _Optic_Proxy @k

class OpticalProxy :: forall k. Type -> k -> (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class Profunctor p <= OpticalProxy k path p s t a b where
  _Optic_Proxy :: Proxy path -> p a b -> p s t

instance
  ( IsSymbol key
  , Row.Cons key a inner s
  , Row.Cons key b inner t
  , Row.Lacks key inner
  , Row.Cons k b () single
  , Row.Union inner single t
  , Choice p
  ) => OpticalProxy Symbol ((key) :: Symbol) p (Variant s) (Variant t) a b where
  _Optic_Proxy key = Optic.prism (Variant.inj key) (Variant.on key Right (Left <<< Variant.expand))
else instance
  ( IsSymbol key
  , Row.Cons key f inner s
  , Row.Cons key g inner t
  , Row.Lacks key inner
  , Row.Cons k g () single
  , Row.Union inner single t
  , Functor g
  , Choice p
  ) => OpticalProxy Symbol ((key) :: Symbol) p (VariantF s x) (VariantF t x) (f x) (g x) where
  _Optic_Proxy key = Optic.prism (VariantF.inj key) (VariantF.on key Right (Left <<< VariantF.expand))
else instance
  ( IsSymbol key
  , Row.Cons key a inner s
  , Row.Cons key b inner t
  , Strong p
  ) => OpticalProxy Symbol ((key) :: Symbol) p (Record s) (Record t) a b where
  _Optic_Proxy key = prop key


