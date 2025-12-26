-- @inline export fanin arity=3
-- @inline export fanout arity=3
-- @inline export conjuxtRLNil(..).conjuxtRL arity=2
-- @inline export conjuxtRLCons(..).conjuxtRL arity=2
-- @inline export conjuxtRowInst(..).conjuxtRow arity=1
-- @inline export iconjuxtRowInst(..).iconjuxtRow arity=2
-- @inline export iconjuxtRowInst(..).iconjuxtSepRow arity=3
-- @inline export disjuxtRLNil(..).disjuxtRL arity=2
-- @inline export disjuxtRLCons(..).disjuxtRL arity=2
-- @inline export disjuxtRowInst(..).disjuxtRow arity=1
-- @inline export idisjuxtRowInst(..).idisjuxtRow arity=2

-- @inline export constRecordNil always
-- @inline export constRecordCons always
-- @inline export abstractRecordNil always
-- @inline export abstractRecordCons always
-- @inline export generateRecordNil always
-- @inline export generateRecordCons always

-- -- @inline export constRecordNil(..).constRecordRL always
-- -- @inline export constRecordCons(..).constRecordRL arity=1
-- -- @inline export abstractRecordNil(..).abstractRecordRL arity=1
-- -- @inline export abstractRecordCons(..).abstractRecordRL arity=2
-- -- @inline export generateRecordNil(..).generateRecordRL always
-- -- @inline export generateRecordCons(..).generateRecordRL always

-- @inline export recjuxt arity=7
module Parser.Printer.Juxt where

import Prelude

import Control.Alternative (class Alternative, (<|>))
import Control.Apply (lift2)
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Codec (Codec(..))
import Data.Decidable (class Decidable, lose)
import Data.Decide (choose)
import Data.Divide (divide)
import Data.Divisible (class Divisible, conquer)
import Data.Either (Either(..), blush, either, hush)
import Data.Either.Nested (type (\/))
import Data.Function (applyFlipped)
import Data.Functor.Clown (Clown(..))
import Data.Functor.Costar (Costar(..))
import Data.Functor.Joker (Joker(..))
import Data.Generic.Rep as G
import Data.HeytingAlgebra (ff, tt)
import Data.Int as Int
import Data.Lens (Forget(..), Iso, Iso', Tagged(..), prism', review, view)
import Data.Lens as Q
import Data.Lens.Internal.Zipping (Zipping(..))
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Profunctor.Star (Star(..))
import Data.String.NonEmpty as NES
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.These (These(..), these)
import Data.These as These
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as Variant
import Idiolect (type (/\/), (/|\), (==<))
import Parser.Selective as Sel
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, from, to)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)




-- Binary combination classes

infixr 4 identity as !!! -- for applying isos, or with `???`
infixr 5 disjuxt2 as \!/
infixr 6 conjuxt2 as /!\
infixr 6 subjuxt2 as /!/

infix 7 dimap as >$<$>
infix 7 dimapConst as ><>
infixr 7 conjuxtR as !>
infixr 7 conjuxtL as <!

infixr 6 applyFlipped as /!
infixr 6 conjuxtSep2Flipped as !\
infixr 6 subjuxtSep2Flipped as !/

-- For annotating precedence *on rules*
-- infixr 5 disjuxt2Prec as \:
-- infixr 6 Tuple as :/
-- For annotating precedence *on tokens* (in rules)
-- pre1 /!\ pre2 /: prec :- token :\ post1 /!\ post2

dimapConst :: forall p u v x y. Profunctor p => u -> y -> p u x -> p v y
dimapConst f g = dimap (const f) (const g)


class Profunctor p <= Conjuxt p where
  _conjuxt0 :: forall u x. x -> p u x
  _conjuxt2 :: forall i o u v x y. (i -> u) -> (i -> v) -> (x -> y -> o) -> p u x -> p v y -> p i o

conjuxt0 :: forall @p. Conjuxt p => p Unit Unit
conjuxt0 = _conjuxt0 unit

conjuxt2 :: forall @p u v x y. Conjuxt p => p u x -> p v y -> p (u /\ v) (x /\ y)
conjuxt2 = _conjuxt2 fst snd Tuple

instance conjuxtFunction :: Conjuxt (->) where
  _conjuxt0 = pure
  _conjuxt2 i1 i2 o ux vy = lift2 o (i1 >>> ux) (i2 >>> vy)

instance conjuxtStar :: Applicative m => Conjuxt (Star m) where
  _conjuxt0 = pure
  _conjuxt2 i1 i2 o (Star l) (Star r) = Star (lift2 (lift2 o) (i1 >>> l) (i2 >>> r))

instance conjuxtCostar :: Functor m => Conjuxt (Costar m) where
  _conjuxt0 = pure
  _conjuxt2 i1 i2 o (Costar l) (Costar r) = Costar $
    lift2 o (map i1 >>> l) (map i2 >>> r)

instance conjuxtJoker :: Applicative m => Conjuxt (Joker m) where
  _conjuxt0 x = Joker (pure x)
  _conjuxt2 _ _ g (Joker l) (Joker r) = Joker (g <$> l <*> r)

instance conjuxtClown :: Divisible m => Conjuxt (Clown m) where
  _conjuxt0 _ = Clown conquer
  _conjuxt2 i1 i2 _ (Clown l) (Clown r) = Clown (divide (i1 &&& i2) l r)

instance conjuxtForget :: Monoid r => Conjuxt (Forget r) where
  _conjuxt0 _ = Forget mempty
  _conjuxt2 i1 i2 _ (Forget l) (Forget r) = Forget \a -> l (i1 a) <> r (i2 a)

instance conjuxtTagged :: Conjuxt Tagged where
  _conjuxt0 x = Tagged x
  _conjuxt2 _ _ g (Tagged l) (Tagged r) = Tagged (g l r)

instance conjuxtZipping :: Conjuxt Zipping where
  _conjuxt0 x = Zipping \_ _ -> x
  _conjuxt2 i1 i2 o (Zipping l) (Zipping r) = Zipping \as bs ->
    o (l (i1 as) (i1 bs)) (r (i2 as) (i2 bs))

conjuxtR :: forall p u v. Conjuxt p => p Unit Unit -> p u v -> p u v
conjuxtR h uv = dimap (Tuple unit) snd $ conjuxt2 h uv

conjuxtL :: forall p u v. Conjuxt p => p u v -> p Unit Unit -> p u v
conjuxtL uv h = dimap (Tuple <@> unit) fst $ conjuxt2 uv h

conjuxtSep2 :: forall p u v x y. Conjuxt p => p u x -> p Unit Unit -> p v y -> p (u /\ v) (x /\ y)
conjuxtSep2 ux h vy = conjuxt2 ux (conjuxtR h vy)

conjuxtSep2Flipped :: forall p u v x y. Conjuxt p => p Unit Unit -> p v y -> p u x -> p (u /\ v) (x /\ y)
conjuxtSep2Flipped h vy ux = conjuxtSep2 ux h vy

class Profunctor p <= Disjuxt p where
  _disjuxt0 :: forall u x. (forall void. u -> void) -> p u x
  _disjuxt2 :: forall i o u v x y. (forall r. (u -> r) -> (v -> r) -> i -> r) -> (x -> o) -> (y -> o) -> p u x -> p v y -> p i o

disjuxt0 :: forall @p. Disjuxt p => p Void Void
disjuxt0 = _disjuxt0 absurd

disjuxt2 :: forall @p u v x y. Disjuxt p => p u x -> p v y -> p (u \/ v) (x \/ y)
disjuxt2 = _disjuxt2 either Left Right

instance disjuxtFunction :: Disjuxt (->) where
  _disjuxt0 f = f
  _disjuxt2 i o1 o2 ux vy = i (ux >>> o1) (vy >>> o2)

instance disjuxtStar :: Applicative m => Disjuxt (Star m) where
  _disjuxt0 f = Star f
  _disjuxt2 i o1 o2 (Star l) (Star r) = Star (i (l >>> map o1) (r >>> map o2))

instance disjuxtJoker :: Plus m => Disjuxt (Joker m) where
  _disjuxt0 _ = Joker empty
  _disjuxt2 _ o1 o2 (Joker l) (Joker r) = Joker (o1 <$> l <|> o2 <$> r)

instance disjuxtClown :: Decidable m => Disjuxt (Clown m) where
  _disjuxt0 = Clown <<< lose
  _disjuxt2 f _ _ (Clown l) (Clown r) = Clown (choose (f Left Right) l r)

instance disjuxtForget :: Disjuxt (Forget r) where
  _disjuxt0 = Forget
  _disjuxt2 i _ _ (Forget l) (Forget r) = Forget (i l r)

class (Profunctor p, Disjuxt p, Conjuxt p) <= Subjuxt p where
  subjuxt2 :: forall u v x y. p u x -> p v y -> p (u /\/ v) (x /\/ y)
  subjuxtSep2 :: forall u v x y. p u x -> p Unit Unit -> p v y -> p (u /\/ v) (x /\/ y)

instance subjuxtFunction :: Subjuxt (->) where
  subjuxt2 = bimap
  subjuxtSep2 ux _ vy = bimap ux vy

instance subjuxtStar :: Applicative m => Subjuxt (Star m) where
  subjuxt2 (Star ux) (Star vy) = Star $ these
    (map This <<< ux)
    (map That <<< vy)
    \u v -> Both <$> ux u <*> vy v
  subjuxtSep2 (Star ux) (Star h) (Star vy) = Star $ these
    (map This <<< ux)
    (map That <<< vy)
    \u v -> Both <$> ux u <* h unit <*> vy v

instance subjuxtJoker :: Alternative m => Subjuxt (Joker m) where
  subjuxt2 ux vy = subjuxt2Default ux vy
  subjuxtSep2 ux h vy = subjuxtSep2Default ux h vy

instance subjuxtClown :: Decidable m => Subjuxt (Clown m) where
  subjuxt2 ux vy = subjuxt2Default ux vy
  subjuxtSep2 ux h vy = subjuxtSep2Default ux h vy

instance subjuxtForget :: Monoid r => Subjuxt (Forget r) where
  subjuxt2 ux vy = subjuxt2Default ux vy
  subjuxtSep2 ux h vy = subjuxtSep2Default ux h vy

subjuxt2Default :: forall p u v x y. Disjuxt p => Conjuxt p => p u x -> p v y -> p (u /\/ v) (x /\/ y)
subjuxt2Default ux vy = _These $ conjuxt2 ux vy `disjuxt2` disjuxt2 ux vy

subjuxtSep2Default :: forall p u v x y. Disjuxt p => Conjuxt p => p u x -> p Unit Unit -> p v y -> p (u /\/ v) (x /\/ y)
subjuxtSep2Default ux h vy = _These $ conjuxt2 ux (conjuxtR h vy) `disjuxt2` disjuxt2 ux vy

subjuxtSep2Flipped :: forall p u v x y. Subjuxt p => p Unit Unit -> p v y -> p u x -> p (u /\/ v) (x /\/ y)
subjuxtSep2Flipped h vy ux = subjuxtSep2 ux h vy

-- N-ary and row combination classes

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

class ConstRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class ConstRecord rl is os | rl -> os where
  constRecordRL :: Record is -> Record os
instance constRecordNil :: ConstRecord RL.Nil is () where
  constRecordRL _ = {}
instance constRecordCons ::
  ( IsSymbol s
  , Row.Cons s o is' is
  , Row.Cons s (ignored -> o) os' os
  , Row.Lacks s os'
  , ConstRecord rl is os'
  ) => ConstRecord (RL.Cons s t rl) is os where
    constRecordRL r = let (s :: Proxy s) = Proxy in
      Record.insert s (const (Record.get s r))
        (constRecordRL @rl r)

class AbstractRecordProf :: Type -> (Type -> Type -> Type) -> (Type -> Type -> Type) -> RL.RowList Type -> Row Type -> Row Type -> Constraint
class AbstractRecordProf info p1 p2 rl is os | rl -> os where
  abstractRecordRL :: (forall i o. info -> p1 i o -> p2 i o) -> Record is -> Record os
instance abstractRecordNil :: AbstractRecordProf info p1 p2 RL.Nil is () where
  abstractRecordRL _ _ = {}
instance abstractRecordCons ::
  ( IsSymbol s
  , TypeEquals pio (p1 i o)
  , Row.Cons s (p1 i o) is' is
  , Row.Cons s (info -> p2 i o) os' os
  , Row.Lacks s os'
  , AbstractRecordProf info p1 p2 rl is os'
  ) => AbstractRecordProf info p1 p2 (RL.Cons s pio rl) is os where
    abstractRecordRL f r = let (s :: Proxy s) = Proxy in
      Record.insert s (flip f (Record.get s r))
        (abstractRecordRL @_ @_ @_ @rl f r)

class GenerateRecord :: Type -> RL.RowList Type -> Row Type -> Constraint
class GenerateRecord const rl os | rl -> os where
  generateRecordRL :: (String -> const) -> Record os
instance generateRecordNil :: GenerateRecord const RL.Nil () where
  generateRecordRL _ = {}
instance generateRecordCons ::
  ( IsSymbol s
  , Row.Cons s const os' os
  , Row.Lacks s os'
  , GenerateRecord const rl os'
  ) => GenerateRecord const (RL.Cons s pio rl) os where
    generateRecordRL f =
      let (s :: Proxy s) = Proxy in
      Record.insert s (f (reflectSymbol s))
        (generateRecordRL @_ @rl f)

class SameLabelsRL :: forall k1 k2. RL.RowList k1 -> RL.RowList k2 -> Constraint
class SameLabelsRL rl1 rl2 | rl1 -> rl2, rl2 -> rl1 -- a white lie
instance SameLabelsRL RL.Nil RL.Nil
instance SameLabelsRL rl1 rl2 => SameLabelsRL (RL.Cons s t1 rl1) (RL.Cons s t2 rl2)
class SameLabels :: forall k1 k2. Row k1 -> Row k2 -> Constraint
class SameLabels r1 r2
instance (RL.RowToList r1 rl1, RL.RowToList r2 rl2, SameLabelsRL rl1 rl2, ListToRow rl1 r1, ListToRow rl2 r2) => SameLabels r1 r2

class ConjuxtRL :: RL.RowList Type -> Row Type -> (Type -> Type -> Type) -> Row Type -> Row Type -> Constraint
class Conjuxt p <= ConjuxtRL rl cs p is os | rl -> os where
  conjuxtRL :: Maybe (p Unit Unit) -> Record cs -> p (Record is) (Record os)

instance conjuxtRLNil :: Conjuxt p => ConjuxtRL RL.Nil cs p is () where
  conjuxtRL _ _ = rec0
instance conjuxtRLCons ::
  ( IsSymbol s
  , Conjuxt p
  , TypeEquals spio (String -> p i o)
  , Row.Cons s spio cs' cs
  , Row.Cons s i is' is
  , Row.Cons s o os' os
  , Row.Lacks s os'
  , ConjuxtRL rl cs p is os'
  ) => ConjuxtRL (RL.Cons s spio rl) cs p is os where
  conjuxtRL msep cs =
    let
      s = Proxy @s
      current = to (Record.get s cs) (reflectSymbol s)
      more = conjuxtRL @rl msep cs
      comb :: p i o -> p (Record is) (Record os') -> p (Record is) (Record os)
      comb = _conjuxt2 (Record.get s) identity (Record.insert s)
    in case msep of
      Nothing -> comb current more
      Just sep -> comb current (sep !> more)

class (Conjuxt p2, SameLabels cs is) <= IConjuxtRow cs p1 p2 is os where
  iconjuxtRow :: (forall i o. String -> p1 i o -> p2 i o) -> Record cs -> p2 (Record is) (Record os)
  iconjuxtSepRow :: (forall i o. String -> p1 i o -> p2 i o) -> p2 Unit Unit -> Record cs -> p2 (Record is) (Record os)
instance iconjuxtRowInst ::
  ( RL.RowToList cs rl
  , AbstractRecordProf String p1 p2 rl cs cs'
  , RL.RowToList cs' rl'
  , ConjuxtRL rl' cs' p2 is os
  , SameLabels cs is
  ) => IConjuxtRow cs p1 p2 is os where
    iconjuxtRow f = conjuxtRL @rl' Nothing <<< abstractRecordRL @_ @_ @_ @rl f
    iconjuxtSepRow f sep = conjuxtRL @rl' (Just sep) <<< abstractRecordRL @_ @_ @_ @rl f

class (Conjuxt p, SameLabels cs is) <= ConjuxtRow cs p is os where
  conjuxtRow :: Record cs -> p (Record is) (Record os)
  conjuxtSepRow :: p Unit Unit -> Record cs -> p (Record is) (Record os)
instance conjuxtRowInst ::
  ( RL.RowToList cs rl
  , ConstRecord rl cs cs'
  , RL.RowToList cs' rl'
  , ConjuxtRL rl' cs' p is os
  , SameLabels cs is
  ) => ConjuxtRow cs p is os where
    conjuxtRow = conjuxtRL @rl' Nothing <<< constRecordRL @rl
    conjuxtSepRow sep = conjuxtRL @rl' (Just sep) <<< constRecordRL @rl

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

class DisjuxtRL :: RL.RowList Type -> Row Type -> (Type -> Type -> Type) -> Row Type -> Row Type -> Constraint
class Disjuxt p <= DisjuxtRL rl cs p is os | rl -> is where
  disjuxtRL :: Record cs -> p (Variant is) (Variant os)

instance disjuxtRLNil :: Disjuxt p => DisjuxtRL RL.Nil cs p () os where
  disjuxtRL _ = var0
instance disjuxtRLCons ::
  ( IsSymbol s
  , Disjuxt p
  , TypeEquals spio (String -> p i o)
  , Row.Cons s (String -> p i o) cs' cs
  , Row.Cons s i is' is
  , Row.Cons s o os' os
  , DisjuxtRL rl cs p is' os
  ) => DisjuxtRL (RL.Cons s spio rl) cs p is os where
  disjuxtRL cs =
    let (s :: Proxy s) = Proxy in
    _disjuxt2 (Variant.on s) (Variant.inj s) identity
      (Record.get s cs (reflectSymbol s))
      (disjuxtRL @rl cs)

class (Disjuxt p2, SameLabels cs os) <= IDisjuxtRow cs p1 p2 is os where
  idisjuxtRow :: (forall i o. String -> p1 i o -> p2 i o) -> Record cs -> p2 (Variant is) (Variant os)
instance idisjuxtRowInst ::
  ( RL.RowToList cs rl
  , AbstractRecordProf String p1 p2 rl cs cs'
  , RL.RowToList cs' rl'
  , DisjuxtRL rl' cs' p2 is os
  , SameLabels cs os
  ) => IDisjuxtRow cs p1 p2 is os where
    idisjuxtRow f = disjuxtRL @rl' <<< abstractRecordRL @_ @_ @_ @rl f

class (Disjuxt p, SameLabels cs os) <= DisjuxtRow cs p is os where
  disjuxtRow :: Record cs -> p (Variant is) (Variant os)
instance disjuxtRowInst ::
  ( RL.RowToList cs rl
  , ConstRecord rl cs cs'
  , RL.RowToList cs' rl'
  , DisjuxtRL rl' cs' p is os
  , SameLabels cs os
  ) => DisjuxtRow cs p is os where
    disjuxtRow = disjuxtRL @rl' <<< constRecordRL @rl

class DisjuxtEnum :: (Type -> Type -> Type) -> Row Type -> Constraint
class Disjuxt p <= DisjuxtEnum p enum where
  disjuxtEnum :: (String -> p Unit Unit) -> p (Variant enum) (Variant enum)
instance disjuxtEnumInst ::
  ( RL.RowToList enum rl
  , GenerateRecord (p Unit Unit) rl cs
  , RL.RowToList cs rl'
  , ConstRecord rl' cs cs'
  , RL.RowToList cs' rl''
  , DisjuxtRL rl'' cs' p enum enum
  ) => DisjuxtEnum p enum where
    disjuxtEnum f = disjuxtRL @rl'' $ constRecordRL @rl' $ generateRecordRL @_ @rl f

-- example ::
--   forall p. Disjuxt p => (String -> p Unit Unit) ->
--   p (Variant ( x :: Unit, y :: Unit )) (Variant ( x :: Unit, y :: Unit ))
-- example = disjuxtEnum @_ @( x :: Unit, y :: Unit )

class SubjuxtN :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class SubjuxtN p u x i o | o -> p, p u x i -> o where
  subjuxtCons :: p u x -> i -> o
  subjuxtSepCons :: p Unit Unit -> p u x -> i -> o

instance subjuxtN0 ::
  ( TypeEquals (pux) (p u x)
  ) => SubjuxtN p u x Sentinel (pux) where
    subjuxtCons :: p u x -> Sentinel -> pux
    subjuxtCons acc EndChain = from acc
    subjuxtSepCons :: p Unit Unit -> p u x -> Sentinel -> pux
    subjuxtSepCons _sep acc EndChain = from acc

else instance subjuxtNN ::
  ( TypeEquals (pux) (p u x)
  , Profunctor p
  , Normalize u' u
  , Normalize x' x
  ) => SubjuxtN p u' x' AndNormalize (pux) where
    subjuxtCons :: p u' x' -> AndNormalize -> pux
    subjuxtCons acc EndChainAndNormalize = from (norm acc)
    subjuxtSepCons :: p Unit Unit -> p u' x' -> AndNormalize -> pux
    subjuxtSepCons _sep acc EndChainAndNormalize = from (norm acc)

else instance subjuxtNP ::
  ( TypeEquals (pvy) (p v y)
  , TypeEquals (io) (i -> o)
  , Subjuxt p
  , SubjuxtN p (u /\/ v) (x /\/ y) i o
  ) => SubjuxtN p u x (pvy) (io) where
    subjuxtCons :: p u x -> pvy -> io
    subjuxtCons acc new = from \i ->
      subjuxtCons (subjuxt2 acc (to new)) i
    subjuxtSepCons :: p Unit Unit -> p u x -> pvy -> io
    subjuxtSepCons sep acc new = from \i ->
      subjuxtSepCons sep (subjuxtSep2 acc sep (to new)) i

subjuxtN :: forall p i o. SubjuxtN p Void Void i o => Subjuxt p => i -> o
subjuxtN = subjuxtCons disjuxt0

subjuxtSepN :: forall p i o. SubjuxtN p Void Void i o => Subjuxt p => p Unit Unit -> i -> o
subjuxtSepN sep = subjuxtSepCons sep disjuxt0

class SubjuxtSepsN :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Constraint
class SubjuxtSepsN p u x i o | o -> p, p u x i -> o where
  subjuxtSepsCons :: p u x -> i -> o

instance subjuxtSepsN0 ::
  ( TypeEquals (pux) (p u x)
  ) => SubjuxtSepsN p u x Sentinel (pux) where
    subjuxtSepsCons :: p u x -> Sentinel -> pux
    subjuxtSepsCons acc EndChain = from acc

else instance subjuxtSepsNN ::
  ( TypeEquals (pux) (p u x)
  , Profunctor p
  , Normalize u' u
  , Normalize x' x
  ) => SubjuxtSepsN p u' x' AndNormalize (pux) where
    subjuxtSepsCons :: p u' x' -> AndNormalize -> pux
    subjuxtSepsCons acc EndChainAndNormalize = from (norm acc)

else instance subjuxtSepsNP ::
  ( TypeEquals (h) (p Unit Unit)
  , TypeEquals (pvyio) (p v y -> i -> o)
  , Subjuxt p
  , SubjuxtSepsN p (u /\/ v) (x /\/ y) i o
  ) => SubjuxtSepsN p u x (h) (pvyio) where
    subjuxtSepsCons :: p u x -> h -> pvyio
    subjuxtSepsCons acc sep = from \new i ->
      subjuxtSepsCons (subjuxtSep2 acc (to sep) new) i

subjuxtSepsN :: forall p u x i o. Subjuxt p => SubjuxtSepsN p (Void /\/ u) (Void /\/ x) i o => p u x -> i -> o
subjuxtSepsN = subjuxtSepsCons disjuxt0 conjuxt0


recjuxt ::
  forall p @s @a @b ra' ra rb' rb.
    Conjuxt p =>
    IsSymbol s =>
    Row.Cons s a ra' ra =>
    Row.Cons s b rb' rb =>
    Row.Lacks s rb' =>
  p a b -> p (Record ra) (Record rb') -> p (Record ra) (Record rb)
recjuxt =
  let s = Proxy @s in
  _conjuxt2 (Record.get s) identity (Record.insert s)

rec0 :: forall p i. Conjuxt p => p i {}
rec0 = _conjuxt0 {}

varjuxt ::
  forall p @s @a @b ra' ra rb' rb.
    Disjuxt p =>
    IsSymbol s =>
    Row.Cons s a ra' ra =>
    Row.Lacks s ra' =>
    Row.Cons s b rb' rb =>
  p a b -> p (Variant ra') (Variant rb) -> p (Variant ra) (Variant rb)
varjuxt =
  let s = Proxy @s in
  _disjuxt2 (Variant.on s) (Variant.inj s) identity

var0 :: forall p o. Disjuxt p => p (Variant ()) o
var0 = _disjuxt0 Variant.case_


-- Selective!!!

infixl 2 conduct as ???
infixr 3 twoLanes as ///

twoLanes ::
  forall p u1 u2 v1 v2 x1 x2 y1 y2.
    Functor (p Void) =>
    Profunctor p =>
  CaseTree p u1 x1 v1 y1 ->
  CaseTree p u2 x2 v2 y2 ->
  CaseTree p (u1 \/ u2) (x1 \/ x2) (v1 \/ v2) (y1 \/ y2)
twoLanes l r = twoCases identity identity (caseTreeL l) (caseTreeR r)

class Conjuxt p <= GuideFlow p where
  branchCases :: forall i j u x v y w z. p i j -> CasesSplit p i j u x v y w z -> p w z

conduct :: forall p u v x y. GuideFlow p => p u x -> CaseTree p u x v y -> p v y
conduct pux = case _ of
  ZeroCases vu x -> dimap (absurd <<< vu) (x >>> absurd) pux
  OneCase vu pvxy ->
    _conjuxt2 vu identity (#) pux pvxy
  TwoCases puxvy -> splitCases puxvy do branchCases pux

selectP ::
  forall p u v x y i j.
    Functor (p Void) =>
    GuideFlow p =>
    Conjuxt p =>
  p (u \/ j) (x \/ i) ->
  p v y ->
  p
    (u \/ (j /\ v))
    (x \/ (i /\ y))
selectP l r =
  l ??? _TupleR
    !!! oneCase conjuxt0
    /// oneCase r

data CaseTree p u x v y
  = ZeroCases (v -> Void) (x -> Void)
  | OneCase (v -> u) (p v (x -> y))
  | TwoCases (SplitCases p u x v y)

-- `dimap` the profunctor of profunctors, `CaseTree p`
iddimap ::
  forall p u x v y u' x' v' y'.
    Profunctor p =>
  (u -> u') ->
  (x' -> x) ->
  (v' -> v) ->
  (y -> y') ->
  CaseTree p u x v y ->
  CaseTree p u' x' v' y'
iddimap u x v y = case _ of
  ZeroCases vu x_ -> ZeroCases (dimap v absurd vu) (dimap x identity x_)
  OneCase vu vxy -> OneCase (dimap v u vu) (dimap v (dimap x y) vxy)
  TwoCases uxvy -> TwoCases $ splitCases uxvy \cases -> casesSplit case cases of
    CasesSplit wuv jxy pixuz piyvz ->
      CasesSplit
        (dimap v identity wuv)
        (dimap x identity jxy)
        (iddimap u identity identity y pixuz)
        (iddimap u identity identity y piyvz)

oneCase ::
  forall p v y.
    Profunctor p =>
  p v y -> forall u x. CaseTree p u x (Tuple u v) (Tuple x y)
oneCase pvy = OneCase fst (dimap snd (flip Tuple) pvy)

caseTreeL ::
  forall p u1 u2 x v y1 y2.
    Profunctor p =>
  CaseTree p u1 x v y1 ->
  CaseTree p (u1 \/ u2) x v (y1 \/ y2)
caseTreeL = iddimap Left identity identity Left
caseTreeR ::
  forall p u1 u2 x v y1 y2.
    Profunctor p =>
  CaseTree p u2 x v y2 ->
  CaseTree p (u1 \/ u2) x v (y1 \/ y2)
caseTreeR = iddimap Right identity identity Right

twoCases ::
  forall p i j u x v y w z.
  (w -> Either u v) ->
  (j -> Either x y) ->
  CaseTree p i x u z ->
  CaseTree p i y v z ->
  CaseTree p i j w z
twoCases f g l r = TwoCases $ casesSplit $ CasesSplit f g l r

cleaveCases ::
  forall f p u x v y.
  (forall void. p void ~> f) ->
  CaseTree p u x v y ->
  Tuple (v -> u) (Sel.CaseTree x f y)
cleaveCases p2f = case _ of
  ZeroCases vu x -> Tuple (absurd <<< vu) $ Sel.ZeroCases x
  OneCase vu pxy -> Tuple vu $ Sel.OneCase (p2f pxy)
  TwoCases cases -> splitCases cases
    \(CasesSplit wuv jxy pixuz piyvz) ->
      let
        Tuple ui xz = cleaveCases p2f pixuz
        Tuple vi yz = cleaveCases p2f piyvz
      in Tuple (wuv >>> either ui vi) $ Sel.twoCases jxy xz yz

lstarCases ::
  forall f r p u x v y.
  (forall i unit. p i unit -> Star f i r) ->
  CaseTree p u x v y ->
  Tuple (v -> u) (Star f v r)
lstarCases p2s = case _ of
  ZeroCases vu _x -> Tuple (absurd <<< vu) $ Star $ vu >>> absurd
  OneCase vu pxy -> Tuple vu $ p2s pxy
  TwoCases cases -> splitCases cases
    \(CasesSplit wuv _jxy pixuz piyvz) ->
      let
        Tuple ui (Star sui) = lstarCases p2s pixuz
        Tuple vi (Star svi) = lstarCases p2s piyvz
      in Tuple (wuv >>> either ui vi) $ Star $ wuv >>> either sui svi

derive instance profunctorCaseTree :: Profunctor p => Profunctor (CaseTree p u x)

summarizeCaseTree :: forall p u v x y m. Monoid m => (forall i j. p i j -> m) -> CaseTree p u x v y -> m
summarizeCaseTree f = case _ of
  ZeroCases _ _ -> mempty
  OneCase _ pxy -> f pxy
  TwoCases cases -> splitCases cases
    \(CasesSplit _ _ pixuz piyvz) ->
      summarizeCaseTree f pixuz <> summarizeCaseTree f piyvz

-- | `SplitCases p i j w z` is an existential of `CasesSplit i u v w p j x y z`
-- | over `u`, `v`, `x` and `y`.
foreign import data SplitCases :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Type

-- SplitCases p (Either u v) (Either x y) (Either s t) (Either a b)

instance profunctorSplitCases :: Profunctor p => Profunctor (SplitCases p i j) where
  dimap f g uxvy = splitCases uxvy \cases -> casesSplit (dimap f g cases)

-- Note that even though we are inserting an `fmap` here, we still know that
-- the left and right cases are exclusive, because of how the types line up!
-- The key is that we do not have to (and are not able to) map the output `r`
-- of each computation and feed it into another. It all is branched from the
-- input type and is up to the consumer to handle \~somehow\~.
data CasesSplit p i j u x v y w z
  = CasesSplit (w -> Either u v) (j -> Either x y) (CaseTree p i x u z) (CaseTree p i y v z)

derive instance profunctorCasesSplit :: Profunctor p => Profunctor (CasesSplit p i j u x v y)

casesSplit :: forall p i j u x v y w z. CasesSplit p i j u x v y w z -> SplitCases p i j w z
casesSplit = unsafeCoerce

splitCases ::
  forall i w p j z.
  SplitCases p i j w z ->
  forall r. (forall u v x y. CasesSplit p i j u x v y w z -> r) -> r
splitCases cases f = f (unsafeCoerce cases)


selectCase ::
  forall @s @p @i @o is os is' os' enumi enumo enumi' enumo'.
    IsSymbol s =>
    Profunctor p =>
    Row.Cons s Unit enumi' enumi =>
    Row.Cons s Unit enumo' enumo =>
    Row.Cons s i is' is =>
    Row.Cons s o os' os =>
  p i o ->
  CaseTree p (Variant enumi) (Variant enumo') (Variant is') (Variant os) ->
  CaseTree p (Variant enumi) (Variant enumo) (Variant is) (Variant os)
selectCase here more | s <- Proxy @s =
  TwoCases $ casesSplit $ CasesSplit
    do Variant.on s Left Right
    do Variant.on s (const $ Left unit) Right
    do OneCase (const $ Variant.inj s unit) $ rmap (\v _ -> Variant.inj s v) $ to here
    do more

class SelectVariantRL :: (Type -> Type -> Type) -> RL.RowList Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class Profunctor p <= SelectVariantRL p rl cases enumi enumo is os | rl -> enumo is where
  selectVariantRL :: Record cases -> CaseTree p (Variant enumi) (Variant enumo) (Variant is) (Variant os)
instance Profunctor p => SelectVariantRL p RL.Nil cases enumi () () os where
  selectVariantRL _ = ZeroCases Variant.case_ Variant.case_
instance
  ( IsSymbol s
  , Profunctor p
  , TypeEquals pio (p i o)
  , Row.Cons s pio cases' cases
  , Row.Cons s Unit enumi' enumi
  , Row.Cons s Unit enumo' enumo
  , Row.Cons s i is' is
  , Row.Cons s o os' os
  , SelectVariantRL p rl cases enumi enumo' is' os
  ) => SelectVariantRL p (RL.Cons s pio rl) cases enumi enumo is os where
    selectVariantRL cases =
      selectCase @s (to (Record.get (Proxy @s) cases)) do
        selectVariantRL @_ @rl cases

class Profunctor p <= SelectVariantRow p cases enum is os where
  selectVariant :: Record cases -> CaseTree p (Variant enum) (Variant enum) (Variant is) (Variant os)
instance
  ( RL.RowToList cases rl
  , SameLabels cases enum
  , SameLabels cases os
  , SameLabels os enum
  , SameLabels is enum
  , SelectVariantRL p rl cases enum enum is os
  ) => SelectVariantRow p cases enum is os where
    selectVariant = selectVariantRL @_ @rl

variantCase ::
  forall @s @p @ci @co @i @o is os is' os' casei caseo casei' caseo'.
    IsSymbol s =>
    Profunctor p =>
    Row.Cons s ci casei' casei =>
    Row.Cons s co caseo' caseo =>
    Row.Cons s (Tuple ci i) is' is =>
    Row.Cons s (Tuple co o) os' os =>
  p i o ->
  CaseTree p (Variant casei) (Variant caseo') (Variant is') (Variant os) ->
  CaseTree p (Variant casei) (Variant caseo) (Variant is) (Variant os)
variantCase here more | s <- Proxy @s =
  TwoCases $ casesSplit $ CasesSplit
    do Variant.on s Left Right
    do Variant.on s Left Right
    do OneCase (Variant.inj s <<< fst) $ dimap snd (\o co -> Variant.inj s (Tuple co o)) $ to here
    do more

class CaseVariantRL :: (Type -> Type -> Type) -> RL.RowList Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class Profunctor p <= CaseVariantRL p rl cases casei caseo is os | rl -> caseo is where
  caseVariantRL :: Record cases -> CaseTree p (Variant casei) (Variant caseo) (Variant is) (Variant os)
instance Profunctor p => CaseVariantRL p RL.Nil cases casei () () os where
  caseVariantRL _ = ZeroCases Variant.case_ Variant.case_
instance
  ( IsSymbol s
  , Profunctor p
  , TypeEquals pio (p i o)
  , Row.Cons s pio cases' cases
  , Row.Cons s ci casei' casei
  , Row.Cons s co caseo' caseo
  , Row.Cons s (Tuple ci i) is' is
  , Row.Cons s (Tuple co o) os' os
  , CaseVariantRL p rl cases casei caseo' is' os
  ) => CaseVariantRL p (RL.Cons s pio rl) cases casei caseo is os where
    caseVariantRL cases =
      variantCase @s (to (Record.get (Proxy @s) cases)) do
        caseVariantRL @_ @rl cases

class Profunctor p <= CaseVariantRow p cases casei caseo is os where
  caseVariant :: Record cases -> CaseTree p (Variant casei) (Variant caseo) (Variant is) (Variant os)
instance
  ( RL.RowToList cases rl
  , SameLabels cases os
  , SameLabels cases casei
  , SameLabels cases caseo
  , CaseVariantRL p rl cases casei caseo is os
  ) => CaseVariantRow p cases casei caseo is os where
    caseVariant = caseVariantRL @_ @rl

-- Isomorphism helpers

_Generic :: forall s t a b. G.Generic s a => G.Generic t b => Iso s t a b
_Generic = dimap G.from G.to

_coerce :: forall s t a b. Coercible s a => Coercible t b => Iso s t a b
_coerce = dimap coerce coerce

_coerce' :: forall s a. Coercible s a => Iso' s a
_coerce' = dimap coerce coerce

-- Tensors

_These :: forall a b c d. Iso (These a b) (These c d) ((a /\ b) \/ (a \/ b)) ((c /\ d) \/ (c \/ d))
_These = Q.iso (these (Right <<< Left) (Right <<< Right) (map Left <<< Tuple)) (either (uncurry Both) (either This That))

type TheseRow a b = Variant ( "This" :: a, "That" :: b, "Both" :: { left :: a, right :: b } )
_TheseRow :: forall a b c d. Iso (These a b) (These c d) (TheseRow a b) (TheseRow c d)
_TheseRow =
  these (Variant.inj (Proxy @"This")) (Variant.inj (Proxy @"That")) (\left right -> Variant.inj (Proxy @"Both") { left, right })
  >$<$> Variant.match { "This": This, "That": That, "Both": \{ left, right } -> Both left right }

_TheseR :: forall a b. Iso a b (These a Void) (These b Void)
_TheseR = dimap This (these identity absurd (const absurd))

_TheseL :: forall a b. Iso a b (These Void a) (These Void b)
_TheseL = dimap That (these absurd identity absurd)


type TupleRow a b = { fst :: a, snd :: b }
_TupleRow :: forall a b c d. Iso (Tuple a b) (Tuple c d) (TupleRow a b) (TupleRow c d)
_TupleRow = uncurry { fst: _, snd: _ } >$<$> do _.fst /|\ _.snd

_TupleR :: forall a b. Iso a b (Tuple a Unit) (Tuple b Unit)
_TupleR = dimap (Tuple <@> unit) fst

_TupleL :: forall a b. Iso a b (Tuple Unit a) (Tuple Unit b)
_TupleL = dimap (Tuple unit) snd

_TupleVL :: forall a b. Iso Void Void (Tuple Void a) (Tuple Void b)
_TupleVL = dimap absurd fst

_TupleVR :: forall a b. Iso Void Void (Tuple a Void) (Tuple b Void)
_TupleVR = dimap absurd snd


type EitherRow a b = Variant ( "Left" :: a, "Right" :: b )
_EitherRow :: forall a b c d. Iso (Either a b) (Either c d) (EitherRow a b) (EitherRow c d)
_EitherRow =
  (Variant.inj (Proxy @"Left") ||| Variant.inj (Proxy @"Right"))
  >$<$> Variant.match { "Left": Left, "Right": Right }

_EitherR :: forall a b. Iso a b (Either a Void) (Either b Void)
_EitherR = dimap Left (either identity absurd)

_EitherL :: forall a b. Iso a b (Either Void a) (Either Void b)
_EitherL = dimap Right (either absurd identity)


type MaybeRow a = Variant ( "Nothing" :: Unit, "Just" :: a )
_MaybeRow :: forall a b. Iso (Maybe a) (Maybe b) (MaybeRow a) (MaybeRow b)
_MaybeRow =
  (Variant.inj (Proxy @"Nothing") ?|| Variant.inj (Proxy @"Just"))
  >$<$> Variant.match { "Nothing": const Nothing, "Just": Just }

_MaybeR :: forall a b. Iso (Maybe a) (Maybe b) (Either a Unit) (Either b Unit)
_MaybeR = dimap (maybe (Right unit) Left) blush

_MaybeL :: forall a b. Iso (Maybe a) (Maybe b) (Either Unit a) (Either Unit b)
_MaybeL = dimap (maybe (Left unit) Right) hush


splitMaybeL :: forall p i o. Disjuxt p => p Unit Unit -> p i o -> p (Maybe i) (Maybe o)
splitMaybeL l r = _MaybeL !!! l \!/ r
infixr 2 splitMaybeL as ?!!

splitMaybeR :: forall p i o. Disjuxt p => p i o -> p Unit Unit -> p (Maybe i) (Maybe o)
splitMaybeR l r = _MaybeR !!! l \!/ r
infixl 2 splitMaybeR as !!?

faninMaybeL :: forall p i o. Disjuxt p => p Unit o -> p i o -> p (Maybe i) o
faninMaybeL = _disjuxt2 maybe' identity identity
infixr 2 faninMaybeL as ?||

faninMaybeR :: forall p i o. Disjuxt p => p i o -> p Unit o -> p (Maybe i) o
faninMaybeR = _disjuxt2 (flip maybe') identity identity
infixl 2 faninMaybeR as ||?

twoCasesMaybeL :: forall p u x v y. Profunctor p =>
  p Unit Unit ->
  CaseTree p u x v y ->
  CaseTree p (Maybe u) (Maybe x) (Maybe v) (Maybe y)
twoCasesMaybeL l r = twoCases (view _MaybeL) (view _MaybeL)
  (OneCase (const Nothing) (rmap (\_ _ -> Nothing) l))
  (iddimap Just identity identity Just r)
infixr 2 twoCasesMaybeL as ?//

twoCasesMaybeR :: forall p u x v y. Profunctor p =>
  CaseTree p u x v y ->
  p Unit Unit ->
  CaseTree p (Maybe u) (Maybe x) (Maybe v) (Maybe y)
twoCasesMaybeR l r = twoCases (view _MaybeR) (view _MaybeR)
  (iddimap Just identity identity Just l)
  (OneCase (const Nothing) (rmap (\_ _ -> Nothing) r))
infixr 2 twoCasesMaybeR as //?

fanin :: forall p i1 i2 o. Disjuxt p => p i1 o -> p i2 o -> p (Either i1 i2) o
fanin = _disjuxt2 either identity identity
infixr 2 fanin as |||

fanout :: forall p i o1 o2. Conjuxt p => p i o1 -> p i o2 -> p i (Tuple o1 o2)
fanout = _conjuxt2 identity identity Tuple
infixr 2 fanout as &&&

-- Arrays and Lists

_Array :: forall i o. Q.Iso (Array i) (Array o) (Unit \/ (i /\ Array i)) (Unit \/ (o /\ Array o))
_Array = dimap
  do Array.uncons >>> maybe (Left unit) (Right <<< (_.head /|\ _.tail))
  do either (const []) (uncurry Array.cons)

_Array' :: forall i o. Q.Iso (Array i) (Array o) (Unit \/ NonEmptyArray i) (Unit \/ NonEmptyArray o)
_Array' = dimap
  do NEA.fromArray >>> maybe (Left unit) Right
  do either (const []) NEA.toArray

_NEA :: forall i o. Q.Iso (NonEmptyArray i) (NonEmptyArray o) (i /\ Array i) (o /\ Array o)
_NEA = dimap
  do NEA.uncons >>> (_.head /|\ _.tail)
  do uncurry NEA.cons'

_List :: forall i o. Q.Iso (List i) (List o) (Unit \/ (i /\ List i)) (Unit \/ (o /\ List o))
_List = dimap
  do List.uncons >>> maybe (Left unit) (Right <<< (_.head /|\ _.tail))
  do either (const List.Nil) (uncurry List.Cons)

_NEL :: forall i o. Q.Iso (NonEmptyList i) (NonEmptyList o) (i /\ List i) (o /\ List o)
_NEL = dimap
  do NEL.uncons >>> (_.head /|\ _.tail)
  do uncurry NEL.cons'

_NES :: Q.Prism' String NES.NonEmptyString
_NES = prism' NES.toString NES.fromString

-- Other

_IntNumber :: Q.Iso' Number (Either Int Number)
_IntNumber = dimap (\n -> maybe (Right n) Left $ Int.fromNumber n) (either Int.toNumber identity)

_Boolean :: Q.Iso' Boolean (Either Unit Unit)
_Boolean = dimap (if _ then Right unit else Left unit) (either ff tt)


-- Generic

class GenericRepToTensor rep tensor | rep -> tensor where
  _GenericRepTensor' :: Iso' rep tensor

instance constructorToTensor :: GenericRepToTensor g t => GenericRepToTensor (G.Constructor name g) (Proxy name /\ t) where
  _GenericRepTensor' = dimap coerce G.Constructor <<< _GenericRepTensor' <<< dimap (Tuple Proxy) snd
instance argumentToTensor :: GenericRepToTensor (G.Argument t) t where
  _GenericRepTensor' = _coerce'
instance noArgumentsToTensor :: GenericRepToTensor G.NoArguments Unit where
  _GenericRepTensor' = dimap (const unit) (const G.NoArguments)
instance noConstructorsToTensor :: GenericRepToTensor G.NoConstructors Void where
  _GenericRepTensor' = dimap unsafeCoerce absurd
instance sumToTensor :: (GenericRepToTensor g1 t1, GenericRepToTensor g2 t2) => GenericRepToTensor (G.Sum g1 g2) (t1 \/ t2) where
  _GenericRepTensor' = dimap
    (fromGeneric >>> bimap (view _GenericRepTensor') (view _GenericRepTensor'))
    (bimap (review _GenericRepTensor') (review _GenericRepTensor') >>> either G.Inl G.Inr)
    where
    fromGeneric (G.Inl x) = Left x
    fromGeneric (G.Inr y) = Right y
instance productToTensor :: (GenericRepToTensor g1 t1, GenericRepToTensor g2 t2) => GenericRepToTensor (G.Product g1 g2) (t1 /\ t2) where
  _GenericRepTensor' = dimap
    (fromGeneric >>> bimap (view _GenericRepTensor') (view _GenericRepTensor'))
    (bimap (review _GenericRepTensor') (review _GenericRepTensor') >>> uncurry G.Product)
    where
    fromGeneric (G.Product x y) = Tuple x y

_GenericTensor' :: forall ty rep tensor. G.Generic ty rep => GenericRepToTensor rep tensor => Iso' ty tensor
_GenericTensor' = _Generic <<< _GenericRepTensor'

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
  norm' = _TupleVL
else instance normalizeTupleRV :: Normalize (x' /\ Void) Void where
  norm' = _TupleVR
else instance normalizeTupleLU :: Normalize y' y => Normalize (Unit /\ y') y where
  norm' = _TupleL >>> norm'
else instance normalizeTupleRU :: Normalize x' x => Normalize (x' /\ Unit) x where
  norm' = _TupleR >>> norm'
else instance normalizeTupleLP :: Normalize y' y => Normalize (Proxy x /\ y') y where
  norm' = dimap (Tuple Proxy) snd >>> norm'
else instance normalizeTupleRP :: Normalize x' x => Normalize (x' /\ Proxy y) x where
  norm' = dimap (Tuple <@> Proxy) fst >>> norm'
else instance normalizeTupleAssoc :: (Normalize x' x, Normalize (y' /\ z') (y /\ z)) => Normalize ((x' /\ y') /\ z') (x /\ y /\ z) where
  norm' = dimap (\(x /\ y /\ z) -> (x /\ y) /\ z) (\((x /\ y) /\ z) -> x /\ y /\ z) >>> dimap binormI binormO
else instance normalizeTuple :: (Normalize x' x, Normalize y' y) => Normalize (x' /\ y') (x /\ y) where
  norm' = dimap binormI binormO

else instance normalizeEitherL :: Normalize y' y => Normalize (Void \/ y') y where
  norm' = _EitherL >>> norm'
else instance normalizeEitherR :: Normalize x' x => Normalize (x' \/ Void) x where
  norm' = _EitherR >>> norm'
else instance normalizeEitherAssoc :: (Normalize x' x, Normalize (y' \/ z') (y \/ z)) => Normalize ((x' \/ y') \/ z') (x \/ y \/ z) where
  norm' = dimap
    (either (Left <<< Left <<< view norm') (either (Left <<< Right) Right <<< view norm'))
    (either (either Left (Right <<< Left)) (Right <<< Right) >>> binormO)
else instance normalizeEither :: (Normalize x' x, Normalize y' y) => Normalize (x' \/ y') (x \/ y) where
  norm' = dimap binormI binormO

else instance normalizeTheseL :: Normalize y' y => Normalize (Void /\/ y') y where
  norm' = _TheseL >>> norm'
else instance normalizeTheseR :: Normalize x' x => Normalize (x' /\/ Void) x where
  norm' = _TheseR >>> norm'
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

-- Juxt instances for Codec

instance (Applicative m, Monoid b) => Conjuxt (Codec m a b) where
  _conjuxt0 x = Codec (pure $ pure x) (pure $ Tuple mempty x)
  _conjuxt2 i1 i2 o (Codec l1 r1) (Codec l2 r2) =
    Codec (lift2 (lift2 o) l1 l2) (lift2 (lift2 o) (i1 >>> r1) (i2 >>> r2))
instance (Plus m) => Disjuxt (Codec m a b) where
  _disjuxt0 f = Codec (pure empty) f
  _disjuxt2 i o1 o2 (Codec l1 r1) (Codec l2 r2) = Codec
    (lift2 (\i1 i2 -> o1 <$> i1 <|> o2 <$> i2) l1 l2) (i (o1 ==< r1) (o2 ==< r2))
instance (Alternative m, Monoid b) => Subjuxt (Codec m a b) where
  subjuxt2 l r = subjuxt2Default l r
  subjuxtSep2 l h r = subjuxtSep2Default l h r

-- GuideFlow is not possible for Codec, gets stuck needing to produce an
-- error for encoding, but it is producing `Tuple b` ...
-- maybe just needs a different order of parameters?

-- https://blog.poisson.chat/posts/2016-10-12-bidirectional-serialization.html
-- https://blog.poisson.chat/posts/2016-10-17-monadic-example.html

