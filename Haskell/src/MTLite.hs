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
import Data.Proxy (Proxy (Proxy))
import Control.Applicative (Applicative(liftA2))
import Data.Profunctor (Profunctor (rmap, lmap), Strong (first', second'), Choice (right', left'))

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

prj1 :: Dict ((c1 && c2) f) -> Dict (c1 f)
prj1 = mapDict (weaken1 . cls)

prj2 :: Dict ((c1 && c2) f) -> Dict (c2 f)
prj2 = mapDict (weaken2 . cls)

instance Class (c1 f, c2 f) ((c1 && c2) f) where
  cls = Sub Dict
-- This is wrong, but interesting nonetheless
-- instance (Class s1 (c1 f), Class s2 (c2 f)) => Class (s1, s2) ((c1 && c2) f) where
--   cls = Sub case (cls :: c1 f :- s1, cls :: c2 f :- s2) of
--     (Sub Dict, Sub Dict) ->
--       Dict

instance Class (c1, c2) (c1 & c2) where
  cls = Sub Dict



-- API

infix 2 <:=
type (<:=) :: (k -> Constraint) -> (k -> Constraint) -> Constraint
type family (query <:= c) where
  (q1 && q2) <:= c = (q1 <:= c, q2 <:= c)
  query <:= c = Gather query c

-- class (forall m. c m => query m) => Gather query c
-- instance (forall m. c m => query m) => Gather query c

-- `(forall m. c m => query m)` as a superclass breaks stuff above
class Gather query c where
  gatherSub :: forall m. c m :- query m
instance (forall m. c m => query m) => Gather query c where
  gatherSub = Sub Dict

-- class (forall m. Gathering query (c m) () (query m)) => Gather query c
-- instance (forall m. Gathering query (c m) () (query m)) => Gather query c

-- gatherSub :: forall query c y. Gathering query c () (query y) => c :- query y
-- gatherSub = unmapDict (flip (gather (Proxy @query) :: Dict c -> Dict () -> Dict (query y)) (Dict :: Dict ()))



-- Implementation details

-- class Gathering query c x y | query c x -> y where
--   gather :: Proxy query -> Dict c -> (Dict x -> Dict y)

-- class GatheringStep (query :: k -> Constraint) (b :: Bool) (c :: Constraint) x y | query b c x -> y where
--   gatherStep :: Proxy query -> Proxy b -> Dict c -> (Dict x -> Dict y)

-- type Satisfies :: (k -> Constraint) -> Constraint -> Bool
-- type family Satisfies query c where
--   Satisfies query (query m) = 'True
--   Satisfies _ _ = 'False

-- instance GatheringStep query (Satisfies query x) c x y => Gathering query c x y where
--   gather p = gatherStep p (Proxy @(Satisfies query x))

-- -- Short-circuit if we already derived a `Monad` constraint
-- instance GatheringStep query 'True c x x where
--   gatherStep _ _ _ = id

-- -- Multiplex queries (not useful) (why is this not overlapping?)
-- -- instance
-- --   ( Gathering q1 c () c1
-- --   , Gathering q2 c () c2
-- --   ) => GatheringStep (q1 && q2) 'False c () (c1, c2) where
-- --     gatherStep _ _ c _ =
-- --       let d0 = Dict :: Dict () in
-- --       case (gather (Proxy @q1) c d0, gather (Proxy @q2) c d0) of
-- --         (Dict, Dict) -> Dict

-- -- Chain derivations when we see a compound constraint
-- instance
--   ( Gathering query (c1 f) x y
--   , Gathering query (c2 f) y z
--   ) => GatheringStep query 'False ((c1 && c2) f) x z where
--     gatherStep p _ c = gather p (prj2 c) . gather p (prj1 c)

-- instance
--   ( Gathering query c1 x y
--   , Gathering query c2 y z
--   ) => GatheringStep query 'False (c1 & c2) x z where
--     gatherStep p _ c = gather p (mapDict (weaken2 . cls) c) . gather p (mapDict (weaken1 . cls) c)

-- -- Gather from base classes
-- instance GatheringStep Functor 'False (Functor m) x (Functor m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Functor 'False (Applicative m) x (Functor m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Functor 'False (Monad m) x (Functor m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Applicative 'False (Applicative m) x (Applicative m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Applicative 'False (Monad m) x (Applicative m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Monad 'False (Monad m) x (Monad m) where
--   gatherStep _ _ Dict _ = Dict

-- -- Gather from MTL classes
-- instance GatheringStep Functor 'False (MR.MonadReader r m) x (Functor m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Applicative 'False (MR.MonadReader r m) x (Applicative m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Monad 'False (MR.MonadReader r m) x (Monad m) where
--   gatherStep _ _ Dict _ = Dict

-- instance GatheringStep Functor 'False (MS.MonadState r m) x (Functor m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Applicative 'False (MS.MonadState r m) x (Applicative m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Monad 'False (MS.MonadState r m) x (Monad m) where
--   gatherStep _ _ Dict _ = Dict

-- instance GatheringStep Functor 'False (MC.MonadCont m) x (Functor m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Applicative 'False (MC.MonadCont m) x (Applicative m) where
--   gatherStep _ _ Dict _ = Dict
-- instance GatheringStep Monad 'False (MC.MonadCont m) x (Monad m) where
--   gatherStep _ _ Dict _ = Dict





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

callCC :: forall m q a b. K ((~) m && MC.MonadCont && q) (K ((~) m && MC.MonadCont && q) (K ((~) m) a b) a) a
callCC = Kleislite \(Kleislite useCont) ->
  MC.callCC \cont ->
    useCont $ Kleislite cont
