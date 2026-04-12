module Control.Monad.Conserve where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)

-- | Use to conserve referential identity when possible.
-- |
-- | Isomorphic to `Tuple (Conj Boolean)`
data Conserve t = Conserved t | Altered t

derive instance functorConserve :: Functor Conserve
derive instance foldableConserve :: Foldable Conserve
derive instance traversableConserve :: Traversable Conserve

instance applyConserve :: Apply Conserve where
  apply (Conserved f) (Conserved t) = Conserved (f t)
  apply fs ts = Altered (extract fs (extract ts))

instance applicativeConserve :: Applicative Conserve where
  pure = Conserved

instance bindConserve :: Bind Conserve where
  bind (Conserved t) f = f t
  bind (Altered t) f = case f t of
    Conserved r -> Altered r
    r@(Altered _) -> r

instance monadConserve :: Monad Conserve

instance extendConserve :: Extend Conserve where
  extend f ts@(Conserved _) = Conserved (f ts)
  extend f ts@(Altered _) = Altered (f ts)

instance comonadConserve :: Comonad Conserve where
  extract (Conserved t) = t
  extract (Altered t) = t

conserve :: forall t. t -> (t -> Maybe t) -> Conserve t
conserve t f = case f t of
  Just t' -> Altered t'
  Nothing -> Conserved t

conserve' :: forall t. t -> (t -> Conserve t) -> Conserve t
conserve' t f = case f t of
  Altered t' -> Altered t'
  Conserved _ -> Conserved t

conserve_ :: forall t. Eq t => t -> (t -> t) -> Conserve t
conserve_ t f = case f t of
  t' | t' /= t -> Altered t'
  _ -> Conserved t

conserveM :: forall t m. Monad m => t -> (t -> m (Maybe t)) -> m (Conserve t)
conserveM t f = f t <#> case _ of
  Just t' -> Altered t'
  Nothing -> Conserved t

conserveM' :: forall t m. Monad m => t -> (t -> m (Conserve t)) -> m (Conserve t)
conserveM' t f = f t <#> case _ of
  Altered t' -> Altered t'
  Conserved _ -> Conserved t

conserveM_ :: forall t m. Eq t => Monad m => t -> (t -> m t) -> m (Conserve t)
conserveM_ t f = f t <#> case _ of
  t' | t' /= t -> Altered t'
  _ -> Conserved t
