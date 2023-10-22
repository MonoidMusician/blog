module Parser.Selective where

import Prelude hiding (zero, one, ap)

import Control.Apply (lift2)
import Control.Plus (class Plus, empty)
import Data.Array (foldr)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Enum (class BoundedEnum, enumFromTo)
import Data.Profunctor (lcmap)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Intro                                                                      --
--------------------------------------------------------------------------------

-- The main goal here is to explain *exclusive branching* in terms of tensors.
-- The punchline is `Interpret (Two x y) (Either i j) f r`. See how it works
-- by following along below:


--------------------------------------------------------------------------------
-- Library Types                                                              --
--------------------------------------------------------------------------------


-- The basic `select` operation. It enables some static analysis and dynamic
-- runtime, but it does not capture exclusive choice.
--
-- You can recover `apply` as `select <<< map Left`.
class (Functor f, Apply f) <= Select f where
  select :: forall a r. f (Either a r) -> f (a -> r) -> f r

-- Another way to express selective applicatives, with binary branching.
-- The left and right branches are clearly exclusive, only one needs to be taken
-- (though implementations are free to execute everything, as per `Select`).
-- The problem is that it loses exclusivity when stacked beyond the 2 cases
-- represented here. In fact, it is no better than `select` in general.
--
-- You can recover `select` with `branch`+`pure` (in two equivalent ways).
-- That is why I put `Applicative` as a superclass here: `Branching` is not
-- incredibly useful without `pure`.
class (Select f, Applicative f) <= Branching f where
  branch :: forall a b r. f (Either a b) -> f (a -> r) -> f (b -> r) -> f r


-- A type that represents combinators for case-matching like `branch` but for
-- arbitrary shapes.
--
-- Type parameters:
-- - `x f r`: shape of branches that will handle the data of type `i` on a
--   functor `f`, returning a result of type `r`.
-- - `i` for input: type of input data to these branches.
-- - `f` for functor: the selective functor we are looking at.
-- - `r` for return: the type of output from the branching computation
-- - `q` (universally quantified): arbitrary data, corresponding to other cases,
--   that are not handled here, that must be preserved.
newtype Interpret x i f r = Interpret
  (forall q. f (Either i q) -> x f r -> f (Either q r))

-- `Interpret` is contravariant over `x` and `i`.
adapt ::
  forall x y i j f r.
    Functor f =>
  (y f r -> x f r) ->
  (j -> i) ->
  Interpret x i f r -> Interpret y j f r
adapt yx ji (Interpret f) = Interpret \jq x -> f (lmap ji <$> jq) (yx x)

-- Magic up the right `Interpret` for `x` shape of cases on input `i`.
class Interpreters x i | x -> i where
  interpreters :: forall f r. Select f => Interpret x i f r
  caseTree :: forall f r. Functor f => x f r -> CaseTree i f r

-- Static analysis via a monoid. Really it should be a semigroup, but we allowed
-- `Zero` into the picture.
class Analyze (x :: (Type -> Type) -> Type -> Type) where
  analyze :: forall f r m. Monoid m => (forall a. f a -> m) -> x f r -> m

-- Choose `q = Void`, since we have no more cases to handle
ap ::
  forall x i f r.
    Functor f =>
  Interpret x i f r ->
  f i -> x f r -> f r
ap (Interpret f) i x = f (Left <$> i) x <#> either absurd identity

-- Automatically build and apply the interpreters for the shape of `x`.
casingOn ::
  forall x i f r.
    Select f =>
    Interpreters x i =>
  f i -> x f r -> f r
casingOn = ap interpreters


-- We have a canonical choice for `x` -- a case tree in the following sense:
data CaseTree i f r
  = ZeroCases (i -> Void)
  | OneCase (f (i -> r))
  | TwoCases (SplitCases i f r)

instance functorCaseTree :: Functor f => Functor (CaseTree i f) where
  map _ (ZeroCases toVoid) = ZeroCases toVoid
  map f (OneCase fir) = OneCase (compose f <$> fir)
  map f (TwoCases cases) = TwoCases (map f cases)

-- `SplitCases i f r` is an existential of `CasesSplit a b i f r` over `a` and `b`
foreign import data SplitCases :: Type -> (Type -> Type) -> Type -> Type

instance functorSplitCases :: Functor f => Functor (SplitCases i f) where
  map f xy = splitCases xy \cases -> casesSplit (map f cases)

-- Note that even though we are inserting an `fmap` here, we still know that
-- the left and right cases are exclusive, because of how the types line up!
-- The key is that we do not have to (and are not able to) map the output `r`
-- of each computation and feed it into another. It all is branched from the
-- input type and is up to the consumer to handle \~somehow\~.
data CasesSplit a b i f r
  = CasesSplit (i -> Either a b) (CaseTree a f r) (CaseTree b f r)

derive instance functorCasesSplit :: Functor f => Functor (CasesSplit a b i f)

casesSplit :: forall a b i f r. CasesSplit a b i f r -> SplitCases i f r
casesSplit = unsafeCoerce

splitCases :: forall i f r. SplitCases i f r -> forall z. (forall a b. CasesSplit a b i f r -> z) -> z
splitCases cases f = f (unsafeCoerce cases)

twoCases ::
  forall a b i f r.
    (i -> Either a b) ->
    CaseTree a f r ->
    CaseTree b f r ->
    CaseTree i f r
twoCases f a b = TwoCases $ casesSplit $ CasesSplit f a b

-- One thing you can do is apply a `CaseTree` to a specific value of `i` to see
-- what branch it chooses. This lets you apply it via `>>=`.
applyCaseTree :: forall i f r. Functor f => CaseTree i f r -> i -> f r
applyCaseTree (ZeroCases toVoid) i = absurd (toVoid i)
applyCaseTree (OneCase fir) i = fir <@> i
applyCaseTree (TwoCases xy) ij =
  splitCases xy \(CasesSplit fg x y) ->
    case fg ij of
      Left i -> applyCaseTree x i
      Right j -> applyCaseTree y j

-- The other way to apply it is via `<*>`, which means we do not get to skip
-- executing any branches.
mergeCaseTree :: forall i f r. Applicative f => CaseTree i f r -> f (i -> r)
mergeCaseTree (ZeroCases toVoid) = pure (absurd <<< toVoid)
mergeCaseTree (OneCase fir) = fir
mergeCaseTree (TwoCases xy) = splitCases xy \(CasesSplit fg x y) ->
  lift2 (\f g ij -> either f g (fg ij)) (mergeCaseTree x) (mergeCaseTree y)

hoistCaseTree :: forall i f g r. (f ~> g) -> CaseTree i f r -> CaseTree i g r
hoistCaseTree _ (ZeroCases toVoid) = ZeroCases toVoid
hoistCaseTree h (OneCase fir) = OneCase (h fir)
hoistCaseTree h (TwoCases xy) = splitCases xy \(CasesSplit fg x y) ->
  twoCases fg (hoistCaseTree h x) (hoistCaseTree h y)

cmapCaseTree :: forall i j f r. Functor f => (j -> i) -> CaseTree i f r -> CaseTree j f r
cmapCaseTree h (ZeroCases toVoid) = ZeroCases (toVoid <<< h)
cmapCaseTree h (OneCase fir) = OneCase (lcmap h <$> fir)
cmapCaseTree h (TwoCases xy) = splitCases xy \(CasesSplit fg x y) ->
  twoCases (fg <<< h) x y

-- Hmm I don't think theres a satisfying way to implement this without
-- collapsing it all via `mergeCaseTree` ... it would need to be another
-- constructor. And it would just correspond to repeated applications of
-- `caseTreeOn` at a higher level.

-- nestCaseTree :: forall i j f r. Applicative f => CaseTree i f j -> CaseTree j f r -> CaseTree i f r
-- nestCaseTree (ZeroCases toVoid) = const (ZeroCases toVoid)
-- nestCaseTree (OneCase fij) = case _ of
--   ZeroCases toVoid -> OneCase (map (absurd <<< toVoid) <$> fir)
--   OneCase fjr -> OneCase (lift2 (>>>) fij fjr)
--   TwoCases xy -> splitCases xy \(CasesSplit fg x y) ->
--     OneCase $ ?help
-- nestCaseTree (TwoCases xy) = case _ of
--   ZeroCases toVoid -> ?help
--   OneCase fjr -> ?help
--   TwoCases uv -> ?help

-- Instead, it makes sense to implement this: push down applicative sequencing
-- through each branch of the tree.
afterCaseTree :: forall i j f r. Applicative f => CaseTree i f j -> f (j -> r) -> CaseTree i f r
afterCaseTree (ZeroCases toVoid) _ = ZeroCases toVoid
afterCaseTree (OneCase fij) fjr = OneCase (lift2 (>>>) fij fjr)
afterCaseTree (TwoCases xy) fjr = splitCases xy \(CasesSplit fg x y) ->
  twoCases fg (afterCaseTree x fjr) (afterCaseTree y fjr)

instance interpretersCaseTree :: Interpreters (CaseTree i) i where
  caseTree = identity
  interpreters = Interpret go
    where
    -- See specific implementations below for explanations of this code
    go :: forall f i' q r. Select f => f (Either i' q) -> CaseTree i' f r -> f (Either q r)
    go iq (ZeroCases toVoid) = either (absurd <<< toVoid) Left <$> iq
    go iq (OneCase fir) =
      select (map Left <$> iq)
        (compose Right <$> fir)
    go ijq (TwoCases xy) = splitCases xy \(CasesSplit f x y) ->
      let
        iq = either (either Left (Right <<< Left)) (Right <<< Right) <<< lmap f <$> ijq
        xi = go iq x
        jq = either (either Left (Right <<< Left)) (Right <<< Right) <$> xi
        yj = go jq y
      in either identity Right <$> yj

instance analyzeCaseTree :: Analyze (CaseTree i) where
  analyze _ (ZeroCases _) = mempty
  analyze f (OneCase fir) = f fir
  analyze f (TwoCases xy) = splitCases xy \(CasesSplit _ x y) ->
    analyze f x <> analyze f y

instance interpretersSplitCases :: Interpreters (SplitCases i) i where
  caseTree = TwoCases
  interpreters = adapt caseTree identity interpreters

instance analyzeSplitCases :: Analyze (SplitCases i) where
  analyze f xy = splitCases xy \(CasesSplit _ x y) ->
    analyze f x <> analyze f y

instance interpretersCasesSplit :: Interpreters (CasesSplit a b i) i where
  caseTree = TwoCases <<< casesSplit
  interpreters = adapt caseTree identity interpreters

instance analyzeCasesSplit :: Analyze (CasesSplit a b i) where
  analyze f (CasesSplit _ x y) =
    analyze f x <> analyze f y

-- Might as well make this a thing. Not sure if it can be usefully split into
-- separate methods?
class Casing f where
  caseTreeOn :: forall i r. f i -> CaseTree i f r -> f r


--------------------------------------------------------------------------------
-- Define some specific shapes of cases:                                        --
--------------------------------------------------------------------------------

data Zero :: (Type -> Type) -> Type -> Type
data Zero f r = Zero

-- It is easy to case on `Void`, it requires `Zero` information.
zero :: forall f r. Functor f => Interpret Zero Void f r
zero = Interpret \iq Zero -> either absurd Left <$> iq

instance interpretZero :: Interpreters Zero Void where
  interpreters = zero
  caseTree Zero = ZeroCases identity

instance analyzeZero :: Analyze Zero where
  analyze _ Zero = mempty


-- This is the fundamental building block of case matching: once we have
-- narrowed down the case to be of type `i`, we can run this to produce a
-- result of type `r`.
--
-- We can take finite products of this (see `Two`), and that's about it.
data One i f r = One (f (i -> r))

-- We can handle `One` case using the power of `branch`.
--
-- Note that this already requires `Select`, due to how `Interpret` wants us to
-- plumb around unhandled cases too. If we did not have to do this, it would
-- only require `apply`.
one :: forall i f r. Select f => Interpret (One i) i f r
one = Interpret \iq (One x) ->
  -- Pass through the extra data `q` from the right to the left
  select (map Left <$> iq)
    -- Apply the case branch, and redirect its output
    -- (since it is not a `q` on the left)
    (compose Right <$> x)

instance interpretOne :: Interpreters (One i) i where
  interpreters = one
  caseTree (One fir) = OneCase fir

instance analyzeOne :: Analyze (One i) where
  analyze f (One x) = f x


data Plain :: (Type -> Type) -> Type -> Type
data Plain f r = Plain (f r)

-- `Plain` is like `One`, but it is not awaiting any specific data.
plain :: forall f r. Select f => Interpret Plain Unit f r
plain = Interpret \iq (Plain x) ->
  -- Pass through the extra data `q` from the right to the left
  select (map Left <$> iq)
    -- Apply the case branch, and redirect its output
    -- (since it is not a `q` on the left)
    (const <<< Right <$> x)

instance interpretPlain :: Interpreters Plain Unit where
  interpreters = plain
  caseTree (Plain fr) = OneCase (const <$> fr)

instance analyzePlain :: Analyze Plain where
  analyze f (Plain x) = f x


data Two ::
  ((Type -> Type) -> Type -> Type) ->
  ((Type -> Type) -> Type -> Type) ->
  ((Type -> Type) -> Type -> Type)
data Two x y f r = Two (x f r) (y f r)

-- We can handle `Two` cases by juggling our choice of `q`.
-- This is the basic tensoring: `Two` and `Either`.
--
-- It is kind of funny that this just requires `Functor`, since all of the
-- casing is handled at the leaves and we get free plumbing from that.
two ::
  forall x y i j f r.
    Functor f =>
  Interpret x i f r ->
  Interpret y j f r ->
  Interpret (Two x y) (Either i j) f r
two _fi _fj = Interpret fij
  where
  fij :: forall q. f (Either (Either i j) q) -> Two x y f r -> f (Either q r)
  fij ijq (Two x y) =
    let
      -- Destruct ifL, ifR once the arbitrary `q` is in scope
      Interpret fi = _fi
      Interpret fj = _fj
      -- First we expose `i` and pass through `q` and `j`
      iq :: f (Either i (Either j q))
      iq = either (either Left (Right <<< Left)) (Right <<< Right) <$> ijq
      -- We handle `i` and it becomes a `r`
      xi :: f (Either (Either j q) r)
      xi = fi iq x
      -- Then we expose `j` and pass through both `q` and the `i` that
      -- became an `r`
      jq :: f (Either j (Either q r))
      jq = either (either Left (Right <<< Left)) (Right <<< Right) <$> xi
      -- We handle `j` and it becomes a `r` as well
      yj :: f (Either (Either q r) r)
      yj = fj jq y
    -- Now we collapse the two cases of `r` that we handled,
    -- keeping `q` on the left
    in either identity Right <$> yj

instance interpretTwo :: (Interpreters x i, Interpreters y j) => Interpreters (Two x y) (Either i j) where
  interpreters = two interpreters interpreters
  caseTree (Two fir fjr) = twoCases identity (caseTree fir) (caseTree fjr)

instance analyzeTwo :: (Analyze x, Analyze y) => Analyze (Two x y) where
  analyze f (Two x y) = analyze f x <> analyze f y


-- Nicer form of `Two (One i) y`.
data Succ i y f r = Succ (f (i -> r)) (y f r)

succ ::
  forall y j i f r.
    Select f =>
  Interpret y j f r ->
  Interpret (Succ i y) (Either i j) f r
succ ifs = adapt (\(Succ fi fj) -> Two (One fi) fj) identity (two one ifs)

instance interpretSucc :: Interpreters y j => Interpreters (Succ i y) (Either i j) where
  interpreters = succ interpreters
  caseTree (Succ fi fj) = caseTree (Two (One fi) fj)

instance analyzeSucc :: Analyze y => Analyze (Succ i y) where
  analyze f (Succ i y) = f i <> analyze f y


-- Nicer form of `Two x (One j)`, the opposite of `Succ`.
data Cuss x j f r = Cuss (x f r) (f (j -> r))

cuss ::
  forall x i j f r.
    Select f =>
  Interpret x i f r ->
  Interpret (Cuss x j) (Either i j) f r
cuss ifs = adapt (\(Cuss fi fj) -> Two fi (One fj)) identity (two ifs one)

instance interpretCuss :: Interpreters x i => Interpreters (Cuss x j) (Either i j) where
  interpreters = cuss interpreters
  caseTree (Cuss fi fj) = caseTree (Two fi (One fj))

instance analyzeCuss :: Analyze x => Analyze (Cuss x j) where
  analyze f (Cuss x j) = analyze f x <> f j


-- Branch on a boolean (true then false).
data ThenElse :: (Type -> Type) -> Type -> Type
data ThenElse f r = ThenElse (f r) (f r)

thenElse :: forall f r. Select f => Interpret ThenElse Boolean f r
thenElse = adapt
  (\(ThenElse bT bF) -> Two (Plain bT) (Plain bF))
  (if _ then Left unit else Right unit)
  interpreters

instance interpretThenElse :: Interpreters ThenElse Boolean where
  interpreters = thenElse
  caseTree (ThenElse bT bF) = twoCases
    (if _ then Left unit else Right unit)
    (OneCase (const <$> bT))
    (OneCase (const <$> bF))

instance analyzeThenElse :: Analyze ThenElse where
  analyze f (ThenElse i j) = f i <> f j


-- Branch on a type with a finite number of values, with a fallback error case.
-- (We need the fallback because we cannot guarantee that `enumFromTo` produces
-- everything, there's just no sensible way to represent that in the PureScript
-- type system.)
data FiniteBase :: Type -> (Type -> Type) -> Type -> Type
data FiniteBase i f r = FiniteBase (i -> f r) (f Void)

finiteBase ::
  forall i f r.
    BoundedEnum i =>
    Eq i =>
    Select f =>
  Interpret (FiniteBase i) i f r
finiteBase = adapt listCases identity finiteCases

listCases ::
  forall i f r.
    BoundedEnum i =>
    Eq i =>
    Functor f =>
  FiniteBase i f r -> FiniteCases i f r
listCases (FiniteBase f baseCase) =
  foldr
    (\i more -> FiniteCase (eq i) (const <$> f i) more)
    (EndFinite baseCase)
    (enumFromTo bottom top)

instance interpretFiniteBase :: BoundedEnum i => Interpreters (FiniteBase i) i where
  interpreters = finiteBase
  caseTree = caseTree <<< listCases

instance analyzeFiniteBase :: BoundedEnum i => Analyze (FiniteBase i) where
  analyze f (FiniteBase cases empt) = Array.fold
    [ enumFromTo bottom top
      # Array.foldMap \i ->
        f (cases i)
    , f empt
    ]


-- Use `empty :: Plus f => f Void` for the base case.
data Finite :: Type -> (Type -> Type) -> Type -> Type
data Finite i f r = Finite (i -> f r)

finite ::
  forall i f r.
    BoundedEnum i =>
    Eq i =>
    Plus f =>
    Select f =>
  Interpret (Finite i) i f r
finite = adapt (\(Finite cases) -> FiniteBase cases empty) identity finiteBase

-- No interpret instance possible, since `Plus f` is not allowed

instance analyzeFinite :: BoundedEnum i => Analyze (Finite i) where
  analyze f (Finite cases) =
    enumFromTo bottom top
      # Array.foldMap \i ->
        f (cases i)

-- Branch on a finite number of enumerated cases, with a fallback error case.
data FiniteCases i f r
  = EndFinite (f Void)
  | FiniteCase (i -> Boolean) (f (i -> r)) (FiniteCases i f r)

finiteCases :: forall i f r. Select f => Interpret (FiniteCases i) i f r
finiteCases = Interpret \iq x -> unfoldCases (iq <#> map Left) x
  where
  unfoldCases :: forall q.
    f (Either i (Either q r)) ->
    FiniteCases i f r ->
    f (Either q r)
  unfoldCases iq (EndFinite end) = select iq (absurd <$> end)
  unfoldCases iq (FiniteCase matches return more) =
    let
      pullOutMatch (Left i) | matches i = Left i
      pullOutMatch (Left i) = Right (Left i)
      pullOutMatch (Right q) = Right (Right q)
    in flip unfoldCases more $ select
      (pullOutMatch <$> iq)
      ((\f i -> Right (Right (f i))) <$> return)

instance interpretFiniteCases :: Interpreters (FiniteCases i) i where
  interpreters = finiteCases
  caseTree (EndFinite empt) = OneCase (absurd <$> empt)
  caseTree (FiniteCase matches return more) =
    let
      pullOutMatch i | matches i = Left i
      pullOutMatch i = Right i
    in twoCases pullOutMatch (OneCase return) (caseTree more)

instance analyzeFiniteCases :: Analyze (FiniteCases i) where
  analyze f = go
    where
    go (EndFinite end) = f end
    go (FiniteCase _ return more) = f return <> go more


--------------------------------------------------------------------------------
-- Scratch work:                                                              --
--------------------------------------------------------------------------------


-- What I had to write as a test case to come up with the above:
-- A demonstration that `branch` does not encode N-ary exclusive choice.
branch3' ::
  forall f a b c d.
    Applicative f =>
    Branching f =>
  f (Either a (Either b c)) ->
  f (a -> d) ->
  f (b -> d) ->
  f (c -> d) ->
  f d
branch3' i a b c =
  branch (
    branch (
      branch i
        -- `map (Right <<< Right)` is an abstract function here: static analysis
        -- cannot peek inside it to see how it is mapping cases around, from
        -- `Either` to `Either`, so for all it knows the output of `a` here
        -- gets routed to `b` and `c` later. But you and I know that is
        -- impossible!
        (map (Right <<< Right) <$> a)
        -- This case also does routing around of cases, but in contrast we do
        -- expect this to go into another case ...
        (pure (map Left))
      :: f (Either b (Either c d))
    )
      (map Right <$> b)
      (pure identity)
    :: f (Either c d)
  )
    (c)
    (pure identity)
  -- Note that all of this `branch`+`pure` business means that we are really
  -- using `select` here: `branch` itself is not even a good building block for
  -- N-ary branches, since one branch has to be dedicated to re-routing the
  -- existing data that does not belong to the branch.

-- Now it can be written with the above machinery:
branch3 ::
  forall f a b c d.
    Select f =>
  f (Either a (Either b c)) ->
  f (a -> d) ->
  f (b -> d) ->
  f (c -> d) ->
  f d
branch3 i a b c = casingOn i (Succ a (Succ b (One c)))


-- (This was before I wrote CaseTree.)
--
-- Maybe this is enough to encode exclusive choice?? Would be complicated ...
--
-- I guess the problem is that it still has `Functor`, and interleaving these
-- things with `Functor` is no good.
--
-- I guess you could encode all the `either` shuffling as methods here??
-- Ugh, but then you need to be able to keep track of all the switching somehow
-- 0.o.
-- class Functor f <= Casing f where
--   oneCase :: forall i q r. f (Either i q) -> f (i -> r) -> f (Either q r)
--   zeroCases :: forall i. f (Either Void i) -> f (Either i Void)
