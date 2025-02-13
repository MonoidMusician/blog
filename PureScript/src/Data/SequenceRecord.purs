module Data.SequenceRecord where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as R
import Record.Builder as Builder
import Type.Equality (class TypeEquals, proof)
import Type.Proxy (Proxy(..))

class Functor m <= SequenceRecordRL (rl :: RL.RowList Type) row from to m | rl row from -> to
  where
    sequenceRecordImpl :: Proxy rl -> Record row -> m (Builder.Builder { | from } { | to })

instance sequenceRecordSingle ::
  ( IsSymbol name
  , Row.Cons name (m1 ty) trash row
  , Functor m2
  , TypeEquals m1 m2
  , Row.Lacks name from
  , Row.Cons name ty from to
  ) => SequenceRecordRL (RL.Cons name (m1 ty) RL.Nil) row from to m2 where
  sequenceRecordImpl _ a  =
      Builder.insert namep <$> valA
    where
      namep = Proxy :: _ name
      valA = to1 (R.get namep a)

else instance sequenceRecordCons ::
  ( IsSymbol name
  , Row.Cons name (m1 ty) trash row
  , Apply m2
  , TypeEquals m1 m2
  , SequenceRecordRL tail row from from' m2
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) => SequenceRecordRL (RL.Cons name (m1 ty) tail) row from to m2 where
  sequenceRecordImpl _ a  =
       fn <$> valA <*> rest
    where
      namep = Proxy :: _ name
      valA = to1 (R.get namep a)
      tailp = Proxy :: _ tail
      rest = sequenceRecordImpl tailp a
      fn valA' rest' = Builder.insert namep valA' <<< rest'

instance sequenceRecordNil :: Applicative m => SequenceRecordRL RL.Nil row from from m where
  sequenceRecordImpl _ _ = pure identity

class SequenceRecord (actions :: Row Type) (values :: Row Type) (m :: Type -> Type) where
  sequenceRecord :: Record actions -> m (Record values)

instance sequenceRecordRow ::
  ( RL.RowToList row rl
  , SequenceRecordRL rl row () row' m
  ) => SequenceRecord row row' m where
  sequenceRecord a = Builder.build <@> {} <$> builder
    where
      builder = sequenceRecordImpl (Proxy :: _ rl) a

newtype To1 :: forall k. k -> (k -> Type) -> (k -> Type) -> Type
newtype To1 c a b = To1 (a c -> b c)

to1 :: forall a b c. TypeEquals a b => a c -> b c
to1 = case proof (To1 (\a -> a)) of To1 f -> f
