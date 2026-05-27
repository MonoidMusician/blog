module Control.Monad.Cell.Select where

import Prelude

import Control.Monad.Cell.Interface (KInterface(..))
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Unsafe as RU
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEquals
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

-- | Selects a particular field `k` from a row `s`: `Selected ( x :: Int ) "x" ~= Int`.
-- |
-- | Used for implementing multiple typesafe operations through interfaces:
-- |   `Selected ( x :: Int ) -> Maybe (Selected ( x :: Boolean )) ~= { x :: Int -> Maybe Boolean }`
newtype Selected (s :: Row Type) (k :: Symbol) = Selected (forall z. (forall t s'. Row.Cons k t s' s => Row.Lacks k s' => IsSymbol k => t -> z) -> z)

-- | Construct a value: `t -> Selected ( x :: t ) "x"`
select :: forall @k @t s' @s. Row.Cons k t s' s => Row.Lacks k s' => IsSymbol k => t -> Selected s k
select t = Selected \f -> f t

-- | Only require a symbol as evidence ...
unsafeSelect :: forall @k @t @s. IsSymbol k => t -> Selected s k
unsafeSelect =
  (unsafeCoerce ::
    (forall @k1 @t1 s1' @s1. Row.Cons k1 t1 s1' s1 => Row.Lacks k1 s1' => IsSymbol k1 => t1 -> Selected s1 k1) ->
    (forall @k1 @t1 @s1. {} -> {} -> IsSymbol k1 => t1 -> Selected s1 k1)
  ) (select :: (forall @k1 @t1 s1' @s1. Row.Cons k1 t1 s1' s1 => Row.Lacks k1 s1' => IsSymbol k1 => t1 -> Selected s1 k1)) {} {}

-- | Destruct a value: `Selected ( x :: t ) "x" -> t`
selected :: forall @k @t s' @s. Row.Cons k t s' s => Selected s k -> t
selected (Selected f) = f unsafeCoerce -- safe because `t` is unique under `Row.Cons k t _ s`


unselected :: forall @k @z. Selected () k -> z
unselected _ = unsafeCrashWith "No row to select"


selection :: forall @s @k. Selected s k -> String
selection (Selected use) = use \_ -> reflectSymbol (Proxy @k)

selectFrom :: forall @s @k t r' @r. Row.Cons k t r' r => Record r -> Selected s k -> t
selectFrom record (Selected use) = use \_ -> RU.unsafeGet (reflectSymbol (Proxy @k)) record


method ::
  forall @k  i ir' ir  o or' or  m.
    IsSymbol k =>
    Row.Cons k i ir' ir =>
    Row.Lacks k ir' =>
    Row.Cons k o or' or =>
    Row.Lacks k or' =>
    Functor m =>
  (Selected ir k -> m (Selected or k)) ->
  i -> m o
method = dimap (select @k) (map (selected @k))


-- | Package a keyed interface into a record.
class Functor m <= PackageInterface m i o io | io -> m i o, m i o -> io where
  package :: KInterface m (Selected i) (Selected o) -> Record io

instance
  ( Functor m
  , RL.RowToList i ir
  , RL.RowToList o or
  , RL.RowToList io ior
  , PackageInterfaceRL m i o io ir or ior
  , ListToRow ior io
  , ListToRow or o
  , ListToRow ir i
  ) => PackageInterface m i o io where
    package = packageRL (Proxy @ir) (Proxy @or) (Proxy @ior)

class PackageInterfaceRL ::
  (Type -> Type) -> Row Type -> Row Type -> Row Type ->
  RL.RowList Type -> RL.RowList Type -> RL.RowList Type ->
  Constraint
class PackageInterfaceRL m i o io ir or ior
  | ior -> m ir or, m ir or -> ior, ior -> io where
    packageRL :: Proxy ir -> Proxy or -> Proxy ior -> KInterface m (Selected i) (Selected o) -> Record io

instance PackageInterfaceRL m i o () RL.Nil RL.Nil RL.Nil where
  packageRL _ _ _ _ = {}
instance
  ( Functor m
  , IsSymbol k
  , Row.Cons k it i' i
  , Row.Lacks k i'
  , Row.Cons k ot o' o
  , Row.Cons k iot io' io
  , TypeEquals iot (it -> m ot)
  , Row.Lacks k io'
  , PackageInterfaceRL m i o io' ir or ior
  ) => PackageInterfaceRL m i o io (RL.Cons k it ir) (RL.Cons k ot or) (RL.Cons k iot ior) where
    packageRL _ _ _ iface@(KInterface process) =
      Record.insert (Proxy @k) (TypeEquals.from $ \it -> selected @k <$> process (select @k it)) $
        packageRL (Proxy @ir) (Proxy @or) (Proxy @ior) iface


-- | The converse, implemented unsafely.
class SelectingCases m i o io | io -> m i o, m i o -> io where
  selecting :: Record io -> KInterface m (Selected i) (Selected o) -- (forall @k. Selected i k -> m (Selected o k))

instance
  ( Functor m
  , RL.RowToList i ir
  , RL.RowToList o or
  , RL.RowToList io ior
  , SelectingCasesRL m i o io ir or ior
  , ListToRow ior io
  , ListToRow or o
  , ListToRow ir i
  ) => SelectingCases m i o io where
    selecting cases = KInterface inner where
      inner :: forall k. Selected i k -> m (Selected o k)
      inner (Selected use) = use \input ->
        unsafeSelect @k @_ @o <$> RU.unsafeGet (reflectSymbol (Proxy @k)) cases input

class SelectingCasesRL ::
  (Type -> Type) -> Row Type -> Row Type -> Row Type ->
  RL.RowList Type -> RL.RowList Type -> RL.RowList Type ->
  Constraint
class SelectingCasesRL m i o io ir or ior | ior -> m ir or, m ir or -> ior, ir -> i, or -> o
  -- selectingRL :: Proxy ir -> Proxy or -> Proxy ior -> Record io -> (forall @k. Selected i k -> m (Selected o k))

instance SelectingCasesRL m () () io RL.Nil RL.Nil RL.Nil

instance
  ( Functor m
  , IsSymbol k
  , Row.Cons k it i' i
  , Row.Cons k ot o' o
  , Row.Cons k iot io' io
  , TypeEquals iot (it -> m ot)
  , Row.Lacks k io'
  , SelectingCasesRL m i' o' io' ir or ior
  ) => SelectingCasesRL m i o io (RL.Cons k it ir) (RL.Cons k ot or) (RL.Cons k iot ior)
