module Parser.Printer.Juxt where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep as G
import Data.Lens (Iso, Iso', review, view)
import Data.Lens as O
import Data.Profunctor (class Profunctor, dimap)
import Data.These (These(..), these)
import Data.These as These
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Idiolect (type (/\/))
import Type.Equality (class TypeEquals, from, to)
import Unsafe.Coerce (unsafeCoerce)

-- Binary combination classes

class Profunctor p <= Conjuxt p where
  conjuxt0 :: p Unit Unit
  conjuxt2 :: forall u v x y. p u x -> p v y -> p (u /\ v) (x /\ y)

conjuxtR :: forall p u v. Conjuxt p => p Unit Unit -> p u v -> p u v
conjuxtR h uv = dimap (Tuple unit) snd $ conjuxt2 h uv

conjuxtL :: forall p u v. Conjuxt p => p u v -> p Unit Unit -> p u v
conjuxtL uv h = dimap (Tuple <@> unit) fst $ conjuxt2 uv h

conjuxtSep2 :: forall p u v x y. Conjuxt p => p u x -> p Unit Unit -> p v y -> p (u /\ v) (x /\ y)
conjuxtSep2 ux h vy = conjuxt2 ux (conjuxtR h vy)

class Profunctor p <= Disjuxt p where
  disjuxt0 :: p Void Void
  disjuxt2 :: forall u v x y. p u x -> p v y -> p (u \/ v) (x \/ y)

class (Profunctor p, Disjuxt p, Conjuxt p) <= Awajuxt p where
  awajuxt2 :: forall u v x y. p u x -> p v y -> p (u /\/ v) (x /\/ y)
  awajuxtSep2 :: forall u v x y. p u x -> p Unit Unit -> p v y -> p (u /\/ v) (x /\/ y)

awajuxt2Default :: forall p u v x y. Disjuxt p => Conjuxt p => p u x -> p v y -> p (u /\/ v) (x /\/ y)
awajuxt2Default ux vy = _These $ conjuxt2 ux vy `disjuxt2` disjuxt2 ux vy

awajuxtSep2Default :: forall p u v x y. Disjuxt p => Conjuxt p => p u x -> p Unit Unit -> p v y -> p (u /\/ v) (x /\/ y)
awajuxtSep2Default ux h vy = _These $ conjuxt2 ux (conjuxtR h vy) `disjuxt2` disjuxt2 ux vy

-- N-ary combination classes

data Sentinel = EndChain
ing = EndChain :: Sentinel
data AndNormalize = EndChainAndNormalize
ed = EndChainAndNormalize :: AndNormalize

class ConjuxtN :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class ConjuxtN p u x i o | o -> p, p u x i -> o where
  conjuxtCons :: p u x -> i -> o
  conjuxtSepCons :: p Unit Unit -> p u x -> i -> o

instance conjuxtN0 ::
  ( TypeEquals (pux) (p u x)
  ) => ConjuxtN p u x Sentinel (pux) where
    conjuxtCons :: p u x -> Sentinel -> pux
    conjuxtCons acc EndChain = from acc
    conjuxtSepCons :: p Unit Unit -> p u x -> Sentinel -> pux
    conjuxtSepCons _sep acc EndChain = from acc

else instance conjuxtNN ::
  ( TypeEquals (pux) (p u x)
  , Profunctor p
  , Normalize u' u
  , Normalize x' x
  ) => ConjuxtN p u' x' AndNormalize (pux) where
    conjuxtCons :: p u' x' -> AndNormalize -> pux
    conjuxtCons acc EndChainAndNormalize = from (norm acc)
    conjuxtSepCons :: p Unit Unit -> p u' x' -> AndNormalize -> pux
    conjuxtSepCons _sep acc EndChainAndNormalize = from (norm acc)

else instance conjuxtNP ::
  ( TypeEquals (pvy) (p v y)
  , TypeEquals (io) (i -> o)
  , Conjuxt p
  , ConjuxtN p (u /\ v) (x /\ y) i o
  ) => ConjuxtN p u x (pvy) (io) where
    conjuxtCons :: p u x -> pvy -> io
    conjuxtCons acc new = from \i ->
      conjuxtCons (conjuxt2 acc (to new)) i
    conjuxtSepCons :: p Unit Unit -> p u x -> pvy -> io
    conjuxtSepCons sep acc new = from \i ->
      conjuxtSepCons sep (conjuxtSep2 acc sep (to new)) i

conjuxtN :: forall p i o. ConjuxtN p Unit Unit i o => Conjuxt p => i -> o
conjuxtN = conjuxtCons conjuxt0

conjuxtSepN :: forall p i o. ConjuxtN p Unit Unit i o => Conjuxt p => p Unit Unit -> i -> o
conjuxtSepN sep = conjuxtSepCons sep conjuxt0

class ConjuxtSepsN :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class ConjuxtSepsN p u x i o | o -> p, p u x i -> o where
  conjuxtSepsCons :: p u x -> i -> o

instance conjuxtSepsN0 ::
  ( TypeEquals (pux) (p u x)
  ) => ConjuxtSepsN p u x Sentinel (pux) where
    conjuxtSepsCons :: p u x -> Sentinel -> pux
    conjuxtSepsCons acc EndChain = from acc

else instance conjuxtSepsNN ::
  ( TypeEquals (pux) (p u x)
  , Profunctor p
  , Normalize u' u
  , Normalize x' x
  ) => ConjuxtSepsN p u' x' AndNormalize (pux) where
    conjuxtSepsCons :: p u' x' -> AndNormalize -> pux
    conjuxtSepsCons acc EndChainAndNormalize = from (norm acc)

else instance conjuxtSepsNP ::
  ( TypeEquals (h) (p Unit Unit)
  , TypeEquals (pvyio) (p v y -> i -> o)
  , Conjuxt p
  , ConjuxtSepsN p (u /\ v) (x /\ y) i o
  ) => ConjuxtSepsN p u x (h) (pvyio) where
    conjuxtSepsCons :: p u x -> h -> pvyio
    conjuxtSepsCons acc sep = from \new i ->
      conjuxtSepsCons (conjuxtSep2 acc (to sep) new) i

conjuxtSepsN :: forall p u x i o. Conjuxt p => ConjuxtSepsN p (Unit /\ u) (Unit /\ x) i o => p u x -> i -> o
conjuxtSepsN = conjuxtSepsCons conjuxt0 conjuxt0

class DisjuxtN :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class DisjuxtN p u x i o | o -> p, p u x i -> o where
  disjuxtCons :: p u x -> i -> o

instance disjuxtN0 ::
  ( TypeEquals (pux) (p u x)
  ) => DisjuxtN p u x Sentinel (pux) where
    disjuxtCons :: p u x -> Sentinel -> pux
    disjuxtCons acc EndChain = from acc

else instance disjuxtNN ::
  ( TypeEquals (pux) (p u x)
  , Profunctor p
  , Normalize u' u
  , Normalize x' x
  ) => DisjuxtN p u' x' AndNormalize (pux) where
    disjuxtCons :: p u' x' -> AndNormalize -> pux
    disjuxtCons acc EndChainAndNormalize = from (norm acc)

else instance disjuxtNP ::
  ( TypeEquals (pvy) (p v y)
  , TypeEquals (io) (i -> o)
  , Disjuxt p
  , DisjuxtN p (u \/ v) (x \/ y) i o
  ) => DisjuxtN p u x (pvy) (io) where
    disjuxtCons :: p u x -> pvy -> io
    disjuxtCons acc new = from \i ->
      disjuxtCons (disjuxt2 acc (to new)) i

disjuxtN :: forall p i o. DisjuxtN p Void Void i o => Disjuxt p => i -> o
disjuxtN = disjuxtCons disjuxt0

class AwajuxtN :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class AwajuxtN p u x i o | o -> p, p u x i -> o where
  awajuxtCons :: p u x -> i -> o
  awajuxtSepCons :: p Unit Unit -> p u x -> i -> o

instance awajuxtN0 ::
  ( TypeEquals (pux) (p u x)
  ) => AwajuxtN p u x Sentinel (pux) where
    awajuxtCons :: p u x -> Sentinel -> pux
    awajuxtCons acc EndChain = from acc
    awajuxtSepCons :: p Unit Unit -> p u x -> Sentinel -> pux
    awajuxtSepCons _sep acc EndChain = from acc

else instance awajuxtNN ::
  ( TypeEquals (pux) (p u x)
  , Profunctor p
  , Normalize u' u
  , Normalize x' x
  ) => AwajuxtN p u' x' AndNormalize (pux) where
    awajuxtCons :: p u' x' -> AndNormalize -> pux
    awajuxtCons acc EndChainAndNormalize = from (norm acc)
    awajuxtSepCons :: p Unit Unit -> p u' x' -> AndNormalize -> pux
    awajuxtSepCons _sep acc EndChainAndNormalize = from (norm acc)

else instance awajuxtNP ::
  ( TypeEquals (pvy) (p v y)
  , TypeEquals (io) (i -> o)
  , Awajuxt p
  , AwajuxtN p (u /\/ v) (x /\/ y) i o
  ) => AwajuxtN p u x (pvy) (io) where
    awajuxtCons :: p u x -> pvy -> io
    awajuxtCons acc new = from \i ->
      awajuxtCons (awajuxt2 acc (to new)) i
    awajuxtSepCons :: p Unit Unit -> p u x -> pvy -> io
    awajuxtSepCons sep acc new = from \i ->
      awajuxtSepCons sep (awajuxtSep2 acc sep (to new)) i

awajuxtN :: forall p i o. AwajuxtN p Void Void i o => Awajuxt p => i -> o
awajuxtN = awajuxtCons disjuxt0

awajuxtSepN :: forall p i o. AwajuxtN p Void Void i o => Awajuxt p => p Unit Unit -> i -> o
awajuxtSepN sep = awajuxtSepCons sep disjuxt0

class AwajuxtSepsN :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class AwajuxtSepsN p u x i o | o -> p, p u x i -> o where
  awajuxtSepsCons :: p u x -> i -> o

instance awajuxtSepsN0 ::
  ( TypeEquals (pux) (p u x)
  ) => AwajuxtSepsN p u x Sentinel (pux) where
    awajuxtSepsCons :: p u x -> Sentinel -> pux
    awajuxtSepsCons acc EndChain = from acc

else instance awajuxtSepsNN ::
  ( TypeEquals (pux) (p u x)
  , Profunctor p
  , Normalize u' u
  , Normalize x' x
  ) => AwajuxtSepsN p u' x' AndNormalize (pux) where
    awajuxtSepsCons :: p u' x' -> AndNormalize -> pux
    awajuxtSepsCons acc EndChainAndNormalize = from (norm acc)

else instance awajuxtSepsNP ::
  ( TypeEquals (h) (p Unit Unit)
  , TypeEquals (pvyio) (p v y -> i -> o)
  , Awajuxt p
  , AwajuxtSepsN p (u /\/ v) (x /\/ y) i o
  ) => AwajuxtSepsN p u x (h) (pvyio) where
    awajuxtSepsCons :: p u x -> h -> pvyio
    awajuxtSepsCons acc sep = from \new i ->
      awajuxtSepsCons (awajuxtSep2 acc (to sep) new) i

awajuxtSepsN :: forall p u x i o. Awajuxt p => AwajuxtSepsN p (Void /\/ u) (Void /\/ x) i o => p u x -> i -> o
awajuxtSepsN = awajuxtSepsCons disjuxt0 conjuxt0



-- Isomorphism helpers

_Generic :: forall s t a b. G.Generic s a => G.Generic t b => Iso s t a b
_Generic = dimap G.from G.to

_These :: forall a b c d. Iso (These a b) (These c d) ((a /\ b) \/ (a \/ b)) ((c /\ d) \/ (c \/ d))
_These = O.iso (these (Right <<< Left) (Right <<< Right) (map Left <<< Tuple)) (either (uncurry Both) (either This That))


-- Normalize tensors

class Normalize un norm | un -> norm where
  norm' :: Iso' norm un

class FullNormalize :: Type -> Type -> Type -> Constraint
class Normalize start step <= FullNormalize start step norm | start -> step norm where
  normalizing :: Iso' norm start

instance isNormalized :: Normalize start start => FullNormalize start start start where
  normalizing = identity
else instance stillNormalizing ::
  ( Normalize start step
  , Normalize step next
  , FullNormalize step next norm
  ) => FullNormalize start step norm where
    normalizing = norm' >>> fullNorm'

fullNorm' :: forall start step norm. Normalize start step => FullNormalize start step norm => Iso' norm start
fullNorm' = normalizing

fullNorm ::
  forall start step norm start' step' norm'.
    Normalize start step =>
    FullNormalize start step norm =>
    Normalize start' step' =>
    FullNormalize start' step' norm' =>
  Iso norm norm' start start'
fullNorm = dimap (view fullNorm' :: norm -> start) (review fullNorm' :: start' -> norm')

norm :: forall s t a b. Normalize a s => Normalize b t => Iso s t a b
norm = dimap (view norm' :: s -> a) (review norm' :: b -> t)

unnorm :: forall s t a b. Normalize a s => Normalize b t => Iso a b s t
unnorm = dimap (review norm' :: a -> s) (view norm' :: t -> b)

binormI :: forall f x' y' x y. Bifunctor f => Normalize x' x => Normalize y' y => f x y -> f x' y'
binormI = bimap (view norm') (view norm')

binormO :: forall f x' y' x y. Bifunctor f => Normalize x' x => Normalize y' y => f x' y' -> f x y
binormO = bimap (review norm') (review norm')

instance normalizeTupleLV :: Normalize (Void /\ y') Void where
  norm' = dimap absurd (fst >>> absurd)
else instance normalizeTupleRV :: Normalize (x' /\ Void) Void where
  norm' = dimap absurd (snd >>> absurd)
else instance normalizeTupleLU :: Normalize y' y => Normalize (Unit /\ y') y where
  norm' = dimap (Tuple unit) snd >>> norm'
else instance normalizeTupleRU :: Normalize x' x => Normalize (x' /\ Unit) x where
  norm' = dimap (Tuple <@> unit) fst >>> norm'
else instance normalizeTupleAssoc :: (Normalize x' x, Normalize (y' /\ z') (y /\ z)) => Normalize ((x' /\ y') /\ z') (x /\ y /\ z) where
  norm' = dimap (\(x /\ y /\ z) -> (x /\ y) /\ z) (\((x /\ y) /\ z) -> x /\ y /\ z) >>> dimap binormI binormO
else instance normalizeTuple :: (Normalize x' x, Normalize y' y) => Normalize (x' /\ y') (x /\ y) where
  norm' = dimap binormI binormO

else instance normalizeEitherL :: Normalize y' y => Normalize (Void \/ y') y where
  norm' = dimap Right (either absurd identity) >>> norm'
else instance normalizeEitherR :: Normalize x' x => Normalize (x' \/ Void) x where
  norm' = dimap Left (either identity absurd) >>> norm'
else instance normalizeEitherAssoc :: (Normalize x' x, Normalize (y' \/ z') (y \/ z)) => Normalize ((x' \/ y') \/ z') (x \/ y \/ z) where
  norm' = dimap
    (either (Left <<< Left <<< view norm') (either (Left <<< Right) Right <<< view norm'))
    (either (either Left (Right <<< Left)) (Right <<< Right) >>> binormO)
else instance normalizeEither :: (Normalize x' x, Normalize y' y) => Normalize (x' \/ y') (x \/ y) where
  norm' = dimap binormI binormO

else instance normalizeTheseL :: Normalize y' y => Normalize (Void /\/ y') y where
  norm' = dimap That (these absurd identity absurd) >>> norm'
else instance normalizeTheseR :: Normalize x' x => Normalize (x' /\/ Void) x where
  norm' = dimap This (these identity absurd (const absurd)) >>> norm'
else instance normalizeTheseAssoc :: (Normalize x' x, Normalize (y' /\/ z') (y /\/ z)) => Normalize ((x' /\/ y') /\/ z') (x /\/ y /\/ z) where
  norm' = dimap unassoc These.assoc >>> dimap binormI binormO
    where
    unassoc :: forall a b c. These a (These b c) -> These (These a b) c
    unassoc = case _ of
      This a -> This (This a)
      That (This b) -> This (That b)
      Both a (This b) -> This (Both a b)
      That (That c) -> That c
      Both a (That c) -> Both (This a) c
      That (Both b c) -> Both (That b) c
      Both a (Both b c) -> Both (Both a b) c

else instance normalizeThese :: (Normalize x' x, Normalize y' y) => Normalize (x' /\/ y') (x /\/ y) where
  norm' = dimap binormI binormO

else instance normalizeNoArguments :: Normalize G.NoArguments Unit where
  norm' = dimap (const G.NoArguments) (const unit)
else instance normalizeNoConstructors :: Normalize G.NoConstructors Void where
  norm' = unsafeCoerce
else instance normalizeSum :: (Normalize x' x, Normalize y' y) => Normalize (G.Sum x' y') (x \/ y) where
  norm' = dimap (either G.Inl G.Inr) fromGeneric >>> dimap binormI binormO
    where
    fromGeneric (G.Inl x) = Left x
    fromGeneric (G.Inr y) = Right y
else instance normalizeProduct :: (Normalize x' x, Normalize y' y) => Normalize (G.Product x' y') (x /\ y) where
  norm' = dimap (uncurry G.Product) fromGeneric >>> dimap binormI binormO
    where
    fromGeneric (G.Product x y) = Tuple x y

else instance normalized :: Normalize x x where
  norm' = identity
