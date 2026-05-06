module Data.Queue where

import Prelude

import Control.Plus (class Alt, class Plus)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data Queue t = Queue (List.List t) (List.List t)

derive instance Functor Queue
instance Monoid (Queue t) where
  mempty = Queue mempty mempty
instance Semigroup (Queue t) where
  append (Queue popp pushp) (Queue popq pushq) =
    Queue (popp <> List.reverse pushp <> popq) pushq
instance Plus Queue where empty = mempty
instance Alt Queue where alt = append

pushQueue :: forall t. t -> Queue t -> Queue t
pushQueue t (Queue popq pushq) = Queue popq (List.Cons t pushq)

popQueue :: forall t. Queue t -> Maybe (Tuple t (Queue t))
popQueue (Queue (List.Cons t popq) pushq) = Just (Tuple t (Queue popq pushq))
popQueue (Queue List.Nil pushq) = case List.reverse pushq of
  List.Cons t popq -> Just (Tuple t (Queue popq List.Nil))
  List.Nil -> Nothing
