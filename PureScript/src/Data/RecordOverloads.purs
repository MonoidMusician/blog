module Data.RecordOverloads where

import Prelude

import Data.Symbol (class IsSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Beside, Quote, Text)
import Record as Record
import Type.Equality (class TypeEquals, to, proof)
import Type.Proxy (Proxy(..))

data Unimportant
foreign import data Nonce :: Unimportant

class ForgetRL :: forall k. RL.RowList k -> RL.RowList Unimportant -> Constraint
class ForgetRL il (ol :: RL.RowList Unimportant) | il -> ol
instance ForgetRL RL.Nil RL.Nil
instance ForgetRL il ol => ForgetRL (RL.Cons s _t il) (RL.Cons s Nonce ol)

class FieldNames :: forall k. Row k -> RL.RowList Unimportant -> Constraint
class FieldNames r rl | r -> rl
instance (RL.RowToList r rl, ForgetRL rl ol) => FieldNames r ol

class Labels :: forall k. Row k -> Symbol -> Constraint
class Labels r s | r -> s
instance (RL.RowToList r rl, LabelsRL rl s) => Labels r s

class LabelsRL :: forall k. RL.RowList k -> Symbol -> Constraint
class LabelsRL rl s | rl -> s
instance LabelsRL RL.Nil ""
instance LabelsRL (RL.Cons s _t RL.Nil) s
instance
  ( Symbol.Append s1 "," sComma
  , Symbol.Append sComma more result
  , LabelsRL (RL.Cons s2 _t2 rl) more
  ) => LabelsRL (RL.Cons s1 _t1 (RL.Cons s2 _t2 rl)) result

-- -- | Dispatches based on the *names* of fields, so that individual fields can
-- -- | still be unified or coerced via typeclasses.
class RecordOverloadsRL (rl :: RL.RowList Type) (overloads :: Row Type) (tgt :: Row Type) (names :: RL.RowList Unimportant) o | rl names -> tgt o where
  overloadsRL :: Record overloads -> Record tgt -> o

instance
  ( FieldNames overload names2
  , TypeEquals field (Record overload -> ox)
  , IsSymbol idx
  , Row.Cons idx field _overloads overloads
  , RecordOverloadAux names1 names2 rl overloads overload tgt ox o
  ) => RecordOverloadsRL (RL.Cons idx field rl) overloads tgt names1 o where
    overloadsRL all = overloadsAux @names1 @names2 @rl @overloads @overload @tgt @ox @o
      (to (Record.get (Proxy @idx) all) :: Record overload -> ox) all
instance (Fail (Beside (Text "No overload found for ") (Quote tgt))) => RecordOverloadsRL RL.Nil overloads tgt names1 o where
  overloadsRL = unsafeCrashWith "No overload"

class RecordOverloadAux (names1 :: RL.RowList Unimportant) (names2 :: RL.RowList Unimportant) (rl :: RL.RowList Type) overloads overload tgt ox o | rl names1 names2 overload ox -> tgt o where
  overloadsAux :: (Record overload -> ox) -> Record overloads -> Record tgt -> o
instance
  ( TypeEquals ox o
  , TypeEquals tgt overload
  ) => RecordOverloadAux names names overloads rl overload tgt ox o where
    overloadsAux chosen _more matched = to $ chosen (proof matched)
else instance
  ( RecordOverloadsRL rl overloads tgt names1 o
  ) => RecordOverloadAux names1 names2 rl overloads _overload tgt _ox o where
    overloadsAux = const (overloadsRL @rl @overloads @tgt @names1)

class RecordOverloads (overloads :: Row Type) (tgt :: Row Type) o | overloads tgt -> o where
  overloads :: Record overloads -> Record tgt -> o

instance
  ( FieldNames tgt names1
  , RL.RowToList overloads rl
  , RecordOverloadsRL rl overloads tgt names1 o
  ) => RecordOverloads overloads tgt o where
    overloads = overloadsRL @rl @overloads @tgt @names1

