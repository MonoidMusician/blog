module Parser.Printer.Juxt where

import Prelude

import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bitraversable (bifoldMap, bitraverse)
import Data.Codec (Codec(..))
import Data.Decidable (class Decidable, lost)
import Data.Decide (chosen)
import Data.Divide (divided)
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
import Data.Lens (Forget(..), Iso, Iso', Tagged(..), review, view)
import Data.Lens as Q
import Data.Lens.Internal.Zipping (Zipping(..))
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice ((+++))
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong ((&&&), (***))
import Data.These (These(..), these)
import Data.These as These
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Idiolect (type (/\/), multiplexing, tupling, (==<))
import Parser.Selective as Sel
import Prim.Coerce (class Coercible)
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, from, to)
import Unsafe.Coerce (unsafeCoerce)




-- Binary combination classes

infixr 4 identity as !!! -- for applying isos
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
  conjuxt0 :: p Unit Unit
  conjuxt2 :: forall u v x y. p u x -> p v y -> p (u /\ v) (x /\ y)

instance conjuxtFunction :: Conjuxt (->) where
  conjuxt0 = identity
  conjuxt2 = (***)

instance conjuxtStar :: Applicative m => Conjuxt (Star m) where
  conjuxt0 = Star pure
  conjuxt2 (Star l) (Star r) = Star (bitraverse l r)

instance conjuxtCostar :: Functor m => Conjuxt (Costar m) where
  conjuxt0 = Costar mempty
  conjuxt2 (Costar l) (Costar r) = Costar ((l <<< map fst) &&& (r <<< map snd))

instance conjuxtJoker :: Applicative m => Conjuxt (Joker m) where
  conjuxt0 = Joker (pure unit)
  conjuxt2 (Joker l) (Joker r) = Joker (tupling l r)

instance conjuxtClown :: Divisible m => Conjuxt (Clown m) where
  conjuxt0 = Clown conquer
  conjuxt2 (Clown l) (Clown r) = Clown (divided l r)

instance conjuxtForget :: Monoid r => Conjuxt (Forget r) where
  conjuxt0 = Forget mempty
  conjuxt2 (Forget l) (Forget r) = Forget (bifoldMap l r)

instance conjuxtTagged :: Conjuxt Tagged where
  conjuxt0 = Tagged unit
  conjuxt2 (Tagged l) (Tagged r) = Tagged (l /\ r)

instance conjuxtZipping :: Conjuxt Zipping where
  conjuxt0 = Zipping \_ _ -> unit
  conjuxt2 (Zipping l) (Zipping r) = Zipping \(aL /\ aR) (bL /\ bR) ->
    l aL bL /\ r aR bR

conjuxtR :: forall p u v. Conjuxt p => p Unit Unit -> p u v -> p u v
conjuxtR h uv = dimap (Tuple unit) snd $ conjuxt2 h uv

conjuxtL :: forall p u v. Conjuxt p => p u v -> p Unit Unit -> p u v
conjuxtL uv h = dimap (Tuple <@> unit) fst $ conjuxt2 uv h

conjuxtSep2 :: forall p u v x y. Conjuxt p => p u x -> p Unit Unit -> p v y -> p (u /\ v) (x /\ y)
conjuxtSep2 ux h vy = conjuxt2 ux (conjuxtR h vy)

conjuxtSep2Flipped :: forall p u v x y. Conjuxt p => p Unit Unit -> p v y -> p u x -> p (u /\ v) (x /\ y)
conjuxtSep2Flipped h vy ux = conjuxtSep2 ux h vy

class Profunctor p <= Disjuxt p where
  disjuxt0 :: p Void Void
  disjuxt2 :: forall u v x y. p u x -> p v y -> p (u \/ v) (x \/ y)

instance disjuxtFunction :: Disjuxt (->) where
  disjuxt0 = identity
  disjuxt2 = (+++)

instance disjuxtStar :: Applicative m => Disjuxt (Star m) where
  disjuxt0 = Star pure
  disjuxt2 (Star l) (Star r) = Star (bitraverse l r)

instance disjuxtJoker :: Plus m => Disjuxt (Joker m) where
  disjuxt0 = Joker empty
  disjuxt2 (Joker l) (Joker r) = Joker (multiplexing l r)

instance disjuxtClown :: Decidable m => Disjuxt (Clown m) where
  disjuxt0 = Clown lost
  disjuxt2 (Clown l) (Clown r) = Clown (chosen l r)

instance disjuxtForget :: Disjuxt (Forget r) where
  disjuxt0 = Forget absurd
  disjuxt2 (Forget l) (Forget r) = Forget (either l r)

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
  ZeroCases vu x -> dimap vu (x >>> absurd) pux
  OneCase vu pvxy ->
    (vu &&& identity) >$<$> uncurry (#)
    !!! pux /!\ pvxy
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
  l ??? _TupleL !!! oneCase conjuxt0
    /// oneCase r

data CaseTree p u x v y
  = ZeroCases (v -> u) (x -> Void)
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
  ZeroCases vu x_ -> ZeroCases (dimap v u vu) (dimap x identity x_)
  OneCase vu vxy -> OneCase (dimap v u vu) (dimap v (dimap x y) vxy)
  TwoCases uxvy -> TwoCases $ splitCases uxvy \cases -> casesSplit case cases of
    CasesSplit wuv jxy pixuz piyvz ->
      CasesSplit
        (dimap v identity wuv)
        (dimap x identity jxy)
        (iddimap u identity identity y pixuz)
        (iddimap u identity identity y piyvz)

oneCase ::
  forall p u x v y.
    Profunctor p =>
  p v y -> CaseTree p u x (Tuple u v) (Tuple x y)
oneCase = OneCase fst <<< dimap snd (flip Tuple)

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
  forall p u x v y.
    Profunctor p =>
  CaseTree p u x v y ->
  Tuple (v -> u) (Sel.CaseTree x (p Void) y)
cleaveCases = case _ of
  ZeroCases vu x -> Tuple vu $ Sel.ZeroCases x
  OneCase vu pxy -> Tuple vu $ Sel.OneCase (lcmap absurd pxy)
  TwoCases cases -> splitCases cases
    \(CasesSplit wuv jxy pixuz piyvz) ->
      let
        Tuple ui xz = cleaveCases pixuz
        Tuple vi yz = cleaveCases piyvz
      in Tuple (wuv >>> either ui vi) $ Sel.twoCases jxy xz yz

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

_TheseL :: forall a b. Iso a b (These a Void) (These b Void)
_TheseL = dimap This (these identity absurd (const absurd))

_TheseR :: forall a b. Iso a b (These Void a) (These Void b)
_TheseR = dimap That (these absurd identity absurd)


_TupleL :: forall a b. Iso a b (Tuple a Unit) (Tuple b Unit)
_TupleL = dimap (Tuple <@> unit) fst

_TupleR :: forall a b. Iso a b (Tuple Unit a) (Tuple Unit b)
_TupleR = dimap (Tuple unit) snd

_TupleVL :: forall a b. Iso Void Void (Tuple Void a) (Tuple Void b)
_TupleVL = dimap absurd fst

_TupleVR :: forall a b. Iso Void Void (Tuple a Void) (Tuple b Void)
_TupleVR = dimap absurd snd


_EitherL :: forall a b. Iso a b (Either a Void) (Either b Void)
_EitherL = dimap Left (either identity absurd)

_EitherR :: forall a b. Iso a b (Either Void a) (Either Void b)
_EitherR = dimap Right (either absurd identity)

_MaybeL :: forall a b. Iso (Maybe a) (Maybe b) (Either a Unit) (Either b Unit)
_MaybeL = dimap (maybe (Right unit) Left) blush

_MaybeR :: forall a b. Iso (Maybe a) (Maybe b) (Either Unit a) (Either Unit b)
_MaybeR = dimap (maybe (Left unit) Right) hush


-- Arrays and Lists

_Array :: forall i o. Q.Iso (Array i) (Array o) (Unit \/ (i /\ Array i)) (Unit \/ (o /\ Array o))
_Array = dimap
  do Array.uncons >>> maybe (Left unit) (Right <<< (Tuple <$> _.head <*> _.tail))
  do either (const []) (uncurry Array.cons)

_Array' :: forall i o. Q.Iso (Array i) (Array o) (Unit \/ NonEmptyArray i) (Unit \/ NonEmptyArray o)
_Array' = dimap
  do NEA.fromArray >>> maybe (Left unit) Right
  do either (const []) NEA.toArray

_NEA :: forall i o. Q.Iso (NonEmptyArray i) (NonEmptyArray o) (i /\ Array i) (o /\ Array o)
_NEA = dimap
  do NEA.uncons >>> (Tuple <$> _.head <*> _.tail)
  do uncurry NEA.cons'

_List :: forall i o. Q.Iso (List i) (List o) (Unit \/ (i /\ List i)) (Unit \/ (o /\ List o))
_List = dimap
  do List.uncons >>> maybe (Left unit) (Right <<< (Tuple <$> _.head <*> _.tail))
  do either (const List.Nil) (uncurry List.Cons)

_NEL :: forall i o. Q.Iso (NonEmptyList i) (NonEmptyList o) (i /\ List i) (o /\ List o)
_NEL = dimap
  do NEL.uncons >>> (Tuple <$> _.head <*> _.tail)
  do uncurry NEL.cons'

-- Other

_IntNumber :: Q.Iso' Number (Either Int Number)
_IntNumber = dimap (\n -> maybe (Right n) Left $ Int.fromNumber n) (either Int.toNumber identity)

_Boolean :: Q.Iso' Boolean (Either Unit Unit)
_Boolean = dimap (if _ then Right unit else Left unit) (either ff tt)


-- Generic

class GenericRepToTensor rep tensor | rep -> tensor where
  _GenericRepTensor' :: Iso' rep tensor

instance constructorToTensor :: GenericRepToTensor g t => GenericRepToTensor (G.Constructor name g) t where
  _GenericRepTensor' = dimap coerce G.Constructor <<< _GenericRepTensor'
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
  norm' = _TupleR >>> norm'
else instance normalizeTupleRU :: Normalize x' x => Normalize (x' /\ Unit) x where
  norm' = _TupleL >>> norm'
else instance normalizeTupleAssoc :: (Normalize x' x, Normalize (y' /\ z') (y /\ z)) => Normalize ((x' /\ y') /\ z') (x /\ y /\ z) where
  norm' = dimap (\(x /\ y /\ z) -> (x /\ y) /\ z) (\((x /\ y) /\ z) -> x /\ y /\ z) >>> dimap binormI binormO
else instance normalizeTuple :: (Normalize x' x, Normalize y' y) => Normalize (x' /\ y') (x /\ y) where
  norm' = dimap binormI binormO

else instance normalizeEitherL :: Normalize y' y => Normalize (Void \/ y') y where
  norm' = _EitherR >>> norm'
else instance normalizeEitherR :: Normalize x' x => Normalize (x' \/ Void) x where
  norm' = _EitherL >>> norm'
else instance normalizeEitherAssoc :: (Normalize x' x, Normalize (y' \/ z') (y \/ z)) => Normalize ((x' \/ y') \/ z') (x \/ y \/ z) where
  norm' = dimap
    (either (Left <<< Left <<< view norm') (either (Left <<< Right) Right <<< view norm'))
    (either (either Left (Right <<< Left)) (Right <<< Right) >>> binormO)
else instance normalizeEither :: (Normalize x' x, Normalize y' y) => Normalize (x' \/ y') (x \/ y) where
  norm' = dimap binormI binormO

else instance normalizeTheseL :: Normalize y' y => Normalize (Void /\/ y') y where
  norm' = _TheseR >>> norm'
else instance normalizeTheseR :: Normalize x' x => Normalize (x' /\/ Void) x where
  norm' = _TheseL >>> norm'
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
  conjuxt0 = Codec (pure $ pure unit) mempty
  conjuxt2 (Codec l1 r1) (Codec l2 r2) =
    Codec (lift2 tupling l1 l2) (bitraverse r1 r2)
instance (Plus m) => Disjuxt (Codec m a b) where
  disjuxt0 = Codec (pure empty) absurd
  disjuxt2 (Codec l1 r1) (Codec l2 r2) = Codec
    (lift2 multiplexing l1 l2) (either (Left ==< r1) (Right ==< r2))
instance (Alternative m, Monoid b) => Subjuxt (Codec m a b) where
  subjuxt2 l r = subjuxt2Default l r
  subjuxtSep2 l h r = subjuxtSep2Default l h r

-- GuideFlow is not possible for Codec, gets stuck needing to produce an
-- error for encoding, but it is producing `Tuple b` ...
-- maybe just needs a different order of parameters?
