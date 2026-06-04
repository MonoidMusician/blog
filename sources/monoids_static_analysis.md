---
title: Monoids for Static Analysis
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

_this is part of the series on selective applicative functors_

Before we dive into monoids for static analysis (that is, the non-functor part of the picture), I want to emphasize that the strength of selective applicative functors/arrows is that not only do you get static analysis for a computation overall, but because it can be interleaved with the functor/arrow structure, it lets you refine that static analysis *as the computation runs*.

For example, for the `Haxl`{.haskell} functor, the computation of the set of blocking requests is interleaved with actually running those requests, so when a particular branch is selected based on known information, it actually knows what requests to use.

It is a bit difficult to fit static analysis for control flow for static applicatives into existing typeclasses.

What we will do is let _branching analysis_ be carried by the ordinary monoid typeclass.
It seems to be more natural to specify this.
Then _sequential analysis_ is specified by monoids on a newtype wrapper `Sequence m`{.haskell}, which has no implementation of its own, but is just a tag for other types to implement their own instances.

Then static analysis on a type `m`{.haskell} is the conjunction of typeclasses `(Monoid m, Monoid (Sequence m))`{.haskell}.
This allows fine-grained `Semigroup`{.haskell}s for nonempty collections for free.


```haskell
-- A really useful pattern synonym! why have i not seen it before?
pattern Coerce :: Coercible outer inner => inner -> outer
pattern Coerce c <- (coerce -> c) where
  Coerce c = coerce c

-- | Newtype specifically for *sequential* combining, since disjunctive
-- combining is the natural one to specify.
newtype Sequence m = Sequence m
  deriving newtype (Eq, Ord, NFData)
pattern Branch :: Sequence m -> m
pattern Branch c = Coerce c


type StaticAnalysis m = (Monoid m, Monoid (Sequence m))

(->-) :: forall m. Monoid (Sequence m) => m -> m -> m
m1 ->- m2 = Branch (Sequence m1 <> Sequence m2)

(~~) :: forall m. Monoid m => Sequence m -> Sequence m -> Sequence m
m1 ~~ m2 = Sequence (Branch m1 <> Branch m2)


analyzeBranches :: forall m. StaticAnalysis m => [m] -> m
analyzeBranches = fold @m

analyzeSequence :: forall m. StaticAnalysis m => [m] -> m
analyzeSequence = coerce (fold @(Sequence m))
```

Now it can integrate with the selective applicative typeclass:

```haskell
-- | A constant functor, for `Casing`
newtype AnalyzeBy m t = Analysis m

instance StaticAnalysis m => Casing (AnalyzeBy m) where ...

-- | Or we can bolt it onto an existing `Casing` functor
-- | (this is isomorphic to `ProductF (AnalyzeBy m)`).
newtype WithAnalysis m f t = WithAnalysis m (f t)

instance (StaticAnalysis m, Casing f) => Casing (AnalyzeBy m) where ...
```

You *can* do static analysis with any old monoid, having the sequencing and branching structures be the same – that is what the paucity of laws for “raw control flow” allows.
But letʼs talk about more interesting, better-behaved analyses.

One of the best ways to tabulate information is with sets.
For Haxl, this would be the set of dependencies to fetch.
Another example would be the set of nonterminals used in a parser combinator expression.

We can use sets in two ways: as a lattice, where sequencing is union and branching is intersection, or as a semilattice, where both operations are union.

For `UnderSet s`{.haskell}, we need a constructor for the empty intersection \(x \cap \top = x\), which will function as an absorptive element \(\top \cup x = \top\) to follow the near-semiring law `0 * x = 0`{.haskell}.^[Technically we could use `BoundedEnum s`{.haskell}, but that is less efficient.]

However the lattice structure isnʼt quite right: the commutativity of the lattice would force `x * 0 = 0`{.haskell}, or \(x \cup \top = \top\), but we do not want to discard requirements *before* absurdity, only after absurdity.
So we use a non-commutative multiplication where `x * 0 = x`{.haskell} instead.
However, *this* is in conflict with the multiplicative identity `y = 1 * y`{.haskell}, since ` 0 = 1 * 0 = 1`{.haskell} then.
So we need a new multiplicative identity, to denote totally pure computations (directly lifted from functions), as opposed to computations with no dependencies (but possibly other effects).

### Shaping Monoids for Control Flow

A bit of scaffolding to shape monoids for control flow.

#### Unreachable Control Flow

First we need to handle unreachable control flow.

```haskell
-- After an error, analysis does not apply (unreachable control flow).
data Flowy m = EndsInDisaster m | CanEndCleanly m | EndsCleanly m

instance Monoid m => Monoid (Flowy m) where mempty = EndsCleanly mempty
instance Semigroup (Sequence m) => Semigroup (Sequence (Flowy m)) where
  -- Cut off analysis after a guaranteed error
  Sequence (EndsInDisaster m) <> _ = Sequence (EndsInDisaster m)
  -- Still ends in error
  Sequence (flowed -> m1) <> Sequence (EndsInDisaster m2) = coerce (EndsInDisaster @m) $ m1 ->- m2
  -- Ends cleanly
  Sequence (EndsCleanly m1) <> Sequence (EndsCleanly m2) = coerce (EndsCleanly @m) $ m1 ->- m2
  -- Mixed cases
  Sequence (flowed -> m1) <> Sequence (flowed -> m2) = coerce (CanEndCleanly @m) $ m1 ->- m2
instance Semigroup m => Semigroup (Flowy m) where
  -- All branches end in disaster still
  EndsInDisaster m1 <> EndsInDisaster m2 = EndsInDisaster $ m1 <> m2
  -- All branches end cleanly still
  EndsCleanly m1 <> EndsCleanly m2 = EndsCleanly $ m1 <> m2
  -- Mixed cases
  (flowed -> m1) <> (flowed -> m2) = CanEndCleanly $ m1 <> m2
instance Sample s m => Sample s (Flowy m) where
  sample = EndsCleanly . sample

-- Extract the inner value
flowed :: Flowy m -> m
flowed (EndsInDisaster m) = m
flowed (CanEndCleanly m) = m
flowed (EndsCleanly m) = m
```

#### Identities

At this point we might as well pull the identities out into their own structure, which will normalize the control flow before it reaches two semigroups for sequencing and branching.
However there is one last choice that the implementation cannot make, which is what happens when one or more branches of control flow are pure.
For `OverSet`{.haskell}, `withPureBranch`{.haskell} is just the identity function, but for `UnderSet`{.haskell}, it needs to produce `Set.empty`{.haskell} to indicate a computation with no requirements (no guaranteed dependencies) but that may be impure nontheless.

```haskell
class (Semigroup m, Semigroup (Sequence m)) => StaticAnalysis1 m where
  -- Should be idempotent and commute with `(<>) @m`
  withPureBranch :: m -> m

-- Add identities
data MaybeAnalysis m = PureAnalysis | AbsurdAnalysis | SomeAnalysis (Flowy m)

instance StaticAnalysis1 m => Semigroup (MaybeAnalysis m) where
  AbsurdAnalysis <> m = m
  m <> AbsurdAnalysis = m
  PureAnalysis <> PureAnalysis = PureAnalysis
  SomeAnalysis m <> PureAnalysis = SomeAnalysis (withPureBranch m)
  PureAnalysis <> SomeAnalysis m = SomeAnalysis (withPureBranch m)
  SomeAnalysis m1 <> SomeAnalysis m2 = SomeAnalysis (m1 <> m2)
instance StaticAnalysis1 m => Monoid (MaybeAnalysis m) where
  mempty = AbsurdAnalysis

instance Semigroup (Sequence m) => Monoid (Sequence (MaybeAnalysis m)) where
  mempty = Sequence PureAnalysis
instance Semigroup (Sequence m) => Semigroup (Sequence (MaybeAnalysis m)) where
  Sequence PureAnalysis <> m = m
  m <> Sequence PureAnalysis = m
  Sequence AbsurdAnalysis <> _ = Sequence AbsurdAnalysis
  Sequence (SomeAnalysis (flowed -> m)) <> Sequence AbsurdAnalysis =
    Sequence (SomeAnalysis (EndsInDisaster m))
  Sequence (SomeAnalysis m1) <> Sequence (SomeAnalysis m2) = Sequence (SomeAnalysis (m1 ->- m2))

unMaybeAnalysis :: forall m. StaticAnalysis m => MaybeAnalysis m -> m
unMaybeAnalysis (SomeAnalysis m) = flowed m
unMaybeAnalysis PureAnalysis = analyzeSequence []
unMaybeAnalysis AbsurdAnalysis = analyzeBranches []
```

### Sets

Now we can get back to sets for static analysis.

```haskell
-- | For under-approximation of requirements,
-- | sequencing is union and branching is intersection.
newtype UnderSet o = UnderSet (Set o)

instance Ord o => Semigroup (UnderSet o) where
  (<>) = coerce (Set.intersection @o)
deriving via Set o instance Ord o => Semigroup (Sequence (UnderSet o))
deriving via Set o instance Ord o => Monoid (Sequence (UnderSet o))

-- | Sequencing and branching are both union, for
-- | over-approximation/tallying what occurs.
newtype OverSet o = OverSet (Set o)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance Ord o => Semigroup (Sequence (OverSet o))
deriving newtype instance Ord o => Monoid (Sequence (OverSet o))
```

The converse is also useful sometimes.
Imagine you may have proofs in scope that narrow what constructors a particular value may be: `AgreeSet Name`{.haskell} will keep track of the constructors it can be for a particular course of execution.
If you can obtain different proofs in different branches, then overall, you have to expect the union of those cases.
But if you get various proofs in sequence, you know it has to belong to the intersection of them all, since the value itself did not change.

```haskell
-- | The converse of `UnderSet`, for agreement under sequencing.
-- | Maybe useful for tracking what constructors a value may have,
-- | for example.
newtype AgreeSet s = AgreeSet (Set s)
  deriving newtype (Semigroup, Monoid)

instance Ord o => Semigroup (Sequence (AgreeSet o)) where
  (<>) = coerce (Set.intersection @o)
```



### Bounds and Proportion

#### Bounds

Hereʼs one: letʼs tally occurrences, finding upper and lower bounds across the possible paths of control flow.
This means that for sequencing we __sum__ the minimums with each other, and the maximums with themselves, over the course of the flow.
This is distinct from just choosing min and max for both sequencing and branching.

:::Details
Across branches, take the respective max and min:

\[\mathbb{B}(n_\downarrow, n_\uparrow) \bigcirc \mathbb{B}(m_\downarrow, m_\uparrow) = \mathbb{B}(n_\downarrow \land m_\downarrow, n_\uparrow \lor m_\uparrow).\]

Sequentially, add componentwise:

\[\mathbb{B}(n_\downarrow, n_\uparrow) \rhd \mathbb{B}(m_\downarrow, m_\uparrow) = \mathbb{B}(n_\downarrow + m_\downarrow, n_\uparrow + m_\uparrow).\]
:::

```haskell
data Bounds o = Bounds { bndMin :: !o, bndMax :: !o }

bound :: o -> Bounds o
bound = join Bounds
instance Ord o => Semigroup (Bounds o) where
  Bounds minL maxL <> Bounds minR maxR = Bounds (min minL minR) (max maxL maxR)
instance Num o => Semigroup (CaseAware (Bounds o)) where
  CaseAware (Bounds minL maxL) <> CaseAware (Bounds minR maxR) =
    CaseAware $ Bounds (minL + minR) (maxL + maxR)
instance Ord o => Sample o (Bounds o) where
  sample = bound
```

#### Proportion

We can also calculate a rough proportion of branches where it occurs.

:::Details
Across branches, add componentwise:

\[\mathbb{B}(n_\bullet, n_\circ) \bigcirc \mathbb{B}(m_\bullet, m_\circ) = \mathbb{B}(n_\bullet + m_\bullet, n_\circ + m_\circ).\]

Sequentially, combine them like probabilities:

\[\mathbb{B}(n_\bullet, n_\circ) \rhd \mathbb{B}(m_\bullet, m_\circ) = \mathbb{B}(n + m - n_\circ * m_\circ, n_\circ * m_\circ),\]

where \(n = n_\bullet + n_\circ\) and \(m = m_\bullet + m_\circ\) are the respective totals, and \(n_\circ * m_\circ\) is interpreted as the probability that both \(n\) and \(m\) do _not_ occur.
:::

```haskell
-- Track the proportion of cases where something occurs or does not.
data Proportion n = Proportion { pWith :: !n, pWithout :: !n }

instance Num n => Semigroup (Proportion n) where
  Proportion wiL woL <> Proportion wiR woR = Proportion (wiL + wiR) (woL + woR)
instance Num n => Semigroup (CaseAware (Proportion n)) where
  -- This assumes that cases are independent
  CaseAware (Proportion sL1 sL2) <> CaseAware (Proportion sR1 sR2) =
    CaseAware $ Proportion (sL * sR - s2) s2
    where
    s2 = sL2 * sR2
    sR = sR1 + sR2
    sL = sL1 + sL2
```

#### Demand

We can combine these ingredients to automatically tally the demand for a variable.
It is important to know whether it is used once or more for some optimizations, and .

```haskell
-- Overall usage of something, including the bounds on the number of times
-- it will be used and the proportion of cases where it is used.
-- This automatically handles control flow (cutting off analysis after errors)
-- and empty cases (via `Maybe`).
newtype Demand = Demand (Flowy (Maybe (Bounds Int, Proportion Int)))

instance Sample Bool Demand where sample = mkDemand

mkDemand :: Bool -> Demand
mkDemand True = demand
mkDemand False = noDemand

demand, noDemand :: Demand
demand = Demand (CanEndCleanly (Just (bound 1, Proportion 1 0)))
noDemand = Demand (CanEndCleanly (Just (bound 0, Proportion 0 1)))

ddBounds :: Demand -> Bounds Int
ddBounds = Demanded >>> flowed >>> maybe (bound 0) fst
ddProportion :: Demand -> Proportion Int
ddProportion = Demanded >>> flowed >>> maybe (Proportion 0 0) snd

ddAlways :: Demand -> Bool
ddAlways = (== 0) . pWithout . ddProportion

ddNever :: Demand -> Bool
ddNever = (== 0) . pWith . ddProportion
```

### Maps

Maps are nuanced too.
Like the above, they have different patterns for uses.
Notably, we may want intersection behavior.
But more commonly we will want `unionWith`{.haskell} behavior.



