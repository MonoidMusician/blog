module Idiolect where

import Prelude

import Control.Alternative (class Alt, class Alternative, (<|>))
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Compactable (class Compactable, compact)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, intercalate, oneOfMap)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.These (These(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

type EffectArrow a b = a -> Effect b
-- | it's an arrow! that does! stuff!
infixr 1 type EffectArrow as -!>


morph :: forall f g b. Foldable f => Plus g => Applicative g => f b -> g b
morph = oneOfMap pure

intercalateMap :: forall f a m. Foldable f => Monoid m => Functor f => m -> (a -> m) -> f a -> m
intercalateMap sep f = intercalate sep <<< map f


compactMap :: forall f a b. Compactable f => Functor f => (a -> Maybe b) -> f a -> f b
compactMap = map compact <<< map
infixl 4 compactMap as <$?>

compactMapFlipped :: forall f a b. Compactable f => Functor f => f a -> (a -> Maybe b) -> f b
compactMapFlipped = flip compactMap
infixl 1 compactMapFlipped as <#?>

composeMap :: forall f a b c. Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
composeMap f g = map f <<< g
infixr 8 composeMap as ==<

composeMapFlipped :: forall f a b c. Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
composeMapFlipped f g = f >>> map g
infixr 8 composeMapFlipped as >==

composeTraverse :: forall f g a b c. Traversable f => Applicative g => (b -> g c) -> (a -> f b) -> (a -> g (f c))
composeTraverse f g = traverse f <<< g
infixr 8 composeTraverse as <==<

composeTraverseFlipped :: forall f g a b c. Traversable f => Applicative g => (a -> f b) -> (b -> g c) -> (a -> g (f c))
composeTraverseFlipped f g = f >>> traverse g
infixr 8 composeTraverseFlipped as >==>

multiplexing :: forall f a b. Alt f => f a -> f b -> f (a \/ b)
multiplexing a b = Left <$> a <|> Right <$> b
infixr 5 multiplexing as \|/

tupling :: forall f a b. Apply f => f a -> f b -> f (a /\ b)
tupling = lift2 Tuple
infixr 6 tupling as /|\

theseing :: forall f a b. Alternative f => f a -> f b -> f (a /\/ b)
theseing a b = This <$> a <|> That <$> b <|> Both <$> a <*> b
infixr 6 theseing as /\\/
infixr 6 type These as /\/

-- tripleQuoted

only :: forall a. Array a -> Maybe a
only [ a ] = Just a
only _ = Nothing

nonEmpty :: forall m. Monoid m => Eq m => m -> Maybe m
nonEmpty m | m /= mempty = Just m
nonEmpty _ = Nothing

filterFst :: forall a b. (a -> Boolean) -> Tuple a b -> Maybe b
filterFst p (a /\ b) | p a = Just b
filterFst _ _ = Nothing

filterSnd :: forall a b. (b -> Boolean) -> Tuple a b -> Maybe a
filterSnd p (a /\ b) | p b = Just a
filterSnd _ _ = Nothing

filterKey ::
  forall k t r' r k' t' r''.
    IsSymbol k =>
    Row.Cons k t r' r =>
    RL.RowToList r' (RL.Cons k' t' RL.Nil) =>
    IsSymbol k' =>
    Row.Cons k' t' r'' r =>
  Proxy k ->
  (t -> Boolean) ->
  Record r ->
  Maybe t'
filterKey _ p r
  | p (Record.get (Proxy :: Proxy k) r)
  = Just (Record.get (Proxy :: Proxy k') r)
filterKey _ _ _ = Nothing
