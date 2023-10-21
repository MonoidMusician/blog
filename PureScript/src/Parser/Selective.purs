module Parser.Selective where

import Prelude hiding (zero, one, ap)

import Control.Plus (class Plus, empty)
import Data.Array (foldr)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Enum (class BoundedEnum, enumFromTo)

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
class (Functor f) <= Select f where
  select :: forall a r. f (Either a r) -> f (a -> r) -> f r

-- Another way to express selective applicatives, with binary branching.
-- The left and right branches are clearly exclusive, only one needs to be taken
-- (though implementations are free to execute everything, as per `Select`).
-- The problem is that it loses exclusivity when stacked beyond the 2 cases
-- represented here. In fact, it is no better than `select` in general.
--
-- You can recover `select` with `branch`+`pure` (in two equivalent ways).
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

--------------------------------------------------------------------------------
-- Define some shapes of cases:                                               --
--------------------------------------------------------------------------------

data Zero :: (Type -> Type) -> Type -> Type
data Zero f r = Zero

-- It is easy to case on `Void`, it requires `Zero` information.
zero :: forall f r. Functor f => Interpret Zero Void f r
zero = Interpret \iq Zero -> either absurd Left <$> iq

instance interpretZero :: Interpreters Zero Void where
  interpreters = zero

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
  where
  listCases :: FiniteBase i f r -> FiniteCases i f r
  listCases (FiniteBase f baseCase) =
    foldr
      (\i more -> FiniteCase (eq i) (const <$> f i) more)
      (EndFinite baseCase)
      (enumFromTo bottom top)

instance interpretFiniteBase :: BoundedEnum i => Interpreters (FiniteBase i) i where
  interpreters = finiteBase

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


-- Maybe this is enough to encode exclusive choice?? Would be complicated ...
--
-- I guess the problem is that it still has `Functor`, and interleaving these
-- things with `Functor` is no good.
--
-- I guess you could encode all the `either` shuffling as methods here??
-- Ugh, but then you need to be able to keep track of all the switching somehow
-- 0.o.
class Functor f <= Casing f where
  oneCase :: forall i q r. f (Either i q) -> f (i -> r) -> f (Either q r)
  zeroCases :: forall i. f (Either Void i) -> f (Either i Void)
