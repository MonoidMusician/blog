---
title: Efficient Solver for Poset Constraints
subtitle: Using a Lattice of Rationals
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

## Motivation

I have long believed that universe polymorphism should be a standard feature of proof assistants.

As [“An Order-Theoretic Analysis of Universe Polymorphism”](https://favonia.org/files/mugen.pdf) rightly points out, “The key to consistent syntax is simply irreflexivity, that is, preventing \(\mathfrak{U} : \mathfrak{U}\)”.

> In this section we prove that any \(\mathcal{H}\)-polymorphic type theory is as consistent as standard (monomorphic) type theory with \(\mathbb{N}\)-indexed universes, essentially because any given proof can only mention finitely many universes, allowing us to interpret those universe levels as natural numbers while preserving the strict order. We therefore argue that type theorists should break the longstanding tradition of equating syntactic universe levels with the induction order used in building semantic models: the former need not even be well-founded (as in Examples 2.3 and 2.4). The key to consistent syntax is simply irreflexivity, that is, preventing \(\mathfrak{U} : \mathfrak{U}\).

## Problem Statement

Instead of following their approach of defining poset models and morphisms between them (used to lift universe levels), I opt to just make a solver that generates arbitrary posets incrementally, from constraints on fresh variables.

A poset solver is going to be an object that accepts constraints of the form: \(v_1 \le v_2\), \(v_1 < v_2\), or \(v_1 = v_2\).

We need this solver to be *online* in a sense: we will constantly be adding constraints, and at most we may add a couple constraints before stopping to ask if it is still consistent, so we may as well check consistency every step.
This is because we need to know that the universe levels are consistent before evaluation is guaranteed to terminate, and typechecking is interleaved with typechecking in a proof assistant.

In Haskell terms, weʼre looking for a `Solver`{.haskell} type that supports `relate`{.haskell}:

```haskell
data Relation = LessThanEqual | LessThan | Equal

relate :: (Fresh, Relation, Fresh) -> Solver -> Maybe Solver
```

where `Fresh`{.haskell} is the type used for universe variables.^[It is a newtype wrapper around `Int`{.haskell}, allowing us to use the efficient `IntMap`{.haskell} data structure.]

The type of posets we create this way are called finitely generated posets: they have a set of variables (unrelated by default), and a set of relations between them.
The only extra relations that hold in that finitely generated poset are the ones that you can deduce by the laws: of most concern is transitivity.

### Posets

A poset is an algebraic/logical structure with just a couple simple laws:

- Transitivity: if \(v_1 \le v_2\) and \(v_2 \le v_3\), then \(v_1 \le v_3\).
- Reflexivity: \(v \le v\).
- Antisymmetry: if \(v_2 \le v_1\) and \(v_1 \le v_2\), then \(v_1 = v_2\).

Importantly, elements do not have to be comparable in the poset, unlike a total order.
I guess this is commonly(?) denoted \(v_1 \parallel v_2\), though it often goes without notation.

The strict order \(v_1 < v_2\) stands for the conjunction of the weak order \(v_1 \le v_2\) and an apartness relation \(v_1 \ne v_2\).

In fact, we exploit this, storing the apartness relations \(v_1 \ne v_2\) separately from the weakly ordered poset structure.
Thus the challenge of writing a poset solver is mostly in the weak order: apartnesses are the only source of inconsistency in a poset, and they can be checked at any time.

A poset is inconsistent if its relations include a loop \(v_1 < v_2 \le v_3 \le \cdots \le v_1\) where at least one relation is strict.
If they are all weak inequalities, one merely learns that all of the variables in that loop are equal, and there is no immediate inconsistency from it.

### Lattices

The most important cases of posets are often semilattices and lattices.

Least upper bound and greatest lower bound.

## Theory and Solution

We want our solver to be as fast as possible, so to do this, we want to choose *concrete positions* for each variable.
These positions do not have to be deterministic, and they do not have to be final: we will be updating the concrete positions of variables as we go, but semantically, we will only be adding constraints.

Concrete positions may change, but the relationships of variables can only maintain their order or collapse due to an equality.

This means that we can look up and compare the current positions of variables very very quickly.

```haskell
data PartialComparison
  = PosetUN -- uncomparable
  | PosetEQ -- equal
  | PosetLE -- less-than-or-equal (but not known to be equal (yet))
  | PosetGE -- greather-than-or-equal

lattice :: Lattice -> Lattice -> PartialComparison

compareIn :: (Fresh, Fresh) -> Solver -> Maybe (Lattice, PartialComparison, Lattice)
compareIn (v1, v2) solver = do
  x <- lookup v1 solver
  y <- lookup v2 solver
  pure (x, lattice x y, y)

lookup :: Fresh -> Solver -> Maybe Lattice
lookup (Fresh v) = IntMap.lookup v . vars

instance Monoid PartialComparison where
  mempty = PosetEQ
instance Semigroup PartialComparison where
  (<>) :: PartialComparison -> PartialComparison -> PartialComparison
  PosetEQ <> r = r
  r <> PosetEQ = r
  PosetLE <> PosetLE = PosetLE
  PosetGE <> PosetGE = PosetGE
  _ <> _ = PosetUN
```

The key is choosing the correct `Lattice`{.haskell} type, implementing the partial comparison `lattice :: Lattice -> Lattice -> PartialComparison`{.haskell}, and figuring out how to update concrete `Lattice`{.haskell} positions in a solver.

### Background

#### The Total Order of Rational Numbers

One of my inspirations was the fact that the rational numbers \(\Q\) play the role of `Lattice`{.haskell} for *total orders* (not partial orders like we want).

Just at a very concrete level, you know that you can incrementally update a total order if you store each variable as a rational number: you can always^[up to numeric limits] find a position to insert a new variable into the order, at, say, \(\frac{x+z}{2}\).

You better be modeling a total order, though, otherwise you end up with excess constraints between variables that should actually be unrelated. ^[This also means that it is difficult to actually model a linear order by iterating individual relations as we want.]

#### Fraïsse Limits

There is a theoretical framework we can apply here: [Fraïssé Limits](https://golem.ph.utexas.edu/category/2009/11/fraisse_limits.html) (this is the blog post that introduced me to them).

In fact, this blog post could solve our problem for us: the Rado graph is a universal countable graph.
Besides its construction via random relations, it can be modeled in more determinstic ways: notably via the Ackermann relation.
Posets can be modeled as graphs, so could this be used as a poset solver?

This does not turn out to be a practical solution though: my language [HatStack](hatstack.html) models Hereditarily Finite Sets (HFSes) via the Ackermann coding: set membership in HatStack is the digraph relation in the Rado digraph.
However, the natural numbers used become too large to fit in memory very quickly, so HatStack also uses ordered finite sets to model HFSes.

Also it is not clear that this encoding would help us with transitivity very much.

But can we come up with our own Fraïssé limit?

#### 2D rationals

Before we get there, letʼs look at an *almost* good-enough lattice: \(\Q\x\Q\), the product of rationals as *posets*.

The comparison goes through component-wise.
That is, \((p_1, p_2) \le (q_1, q_2)\) iff \(p_1 \le q_1\) and \(p_2 \le q_2\).

In fact, not only do we keep the poset structure, but we also keep the lattice structure.
Essentially, componentwise `min`{.haskell} and `max`{.haskell} work well.

Repeating some relevant definitions from above for clarity,

```haskell
rationalProduct :: (Ratio Int, Ratio Int) -> (Ratio Int, Ratio Int) -> PartialComparison
rationalProduct (p1, p2) (q1, q2) =
  partialOfTotal (p1 `compare` q1) <> partialOfTotal (p2 `compare` q2)

data PartialComparison
  = PosetUN -- uncomparable
  | PosetEQ -- equal
  | PosetLE -- less-than-or-equal (but not known to be equal (yet))
  | PosetGE -- greather-than-or-equal

instance Monoid PartialComparison where
  mempty = PosetEQ
instance Semigroup PartialComparison where
  (<>) :: PartialComparison -> PartialComparison -> PartialComparison
  PosetEQ <> r = r
  r <> PosetEQ = r
  PosetLE <> PosetLE = PosetLE
  PosetGE <> PosetGE = PosetGE
  _ <> _ = PosetUN

partialOfTotal :: Ordering -> PartialComparison
partialOfTotal = \case
  EQ -> PosetEQ
  LT -> PosetLE
  GT -> PosetGE
```

We can embed any finite total order, represented as numbers (rational or integer or natural), either as \((r, 0)\) or \((0, r)\), or any number of ways.
This is called a chain (of comparable elements).

Importantly, we also have a way of representing uncomparable elements now: \((0, 1)\) is uncomparable with \((1, 0)\), since one component is smaller whilst the other component is larger.

We can even represent arbitrarily many uncomparable elements: \((i, -i)\), each element is larger in one component and smaller in the other.
This is called an anti-chain (of uncomparable elements), and it is significant progress for representing posets that we can represent arbitrarily long anti-chains.

However, the structure is still too constrained to represent arbitrary posets.
Letʼs look at an example.

Letʼs say we start with an antichain \(v_1 \parallel v_2 \parallel v_3\).
We can represent them as \(v_1 = (-1, 1), v_2 = (0, 0), v_3 = (1, -1)\).

Now we want to insert a new point \(v_4 \le v_1, v_2\) below \(v_1\) and \(v_2\).^[Ignore the fact that we only insert one relation at a time in our model.]
There are infinite places it can go, but a good place to chose is at the infimum \(v_4 := v_1 \land v_2 = (-1, 0)\).
Notice how we have \(v_4 \le v_1\) and \(v_4 \le v_2\), but \(v_4 \parallel v_3\): we have not inserted any excess relations.

Now we do this for \(v_5 \le v_2, v_3\): choose \(v_5 := v_2 \land v_3 = (0, -1)\).
Again this introduces those constraints, but \(v_5\) is uncomparable to \(v_1\) and \(v_4\) as we want.

However, we just used up the two degrees of freedom available in our lattice \(\Q\x\Q\)!
If we go to insert \(v_6 \le v_1, v_3\) we are stuck: *any* point we pick has to be below the infimum \(v_1 \land v_3 = (-1, -1)\), but that is less than \(v_2\), which is something we did not ask for!^[In fact, it is less than \(v_4\) and \(v_5\) as well, though that is fixable.]

Hopefully you can see that this situation is generic: we started with an antichain and asked for three pairwise lower bounds, and we only could get two distinct ones back.
A clever re-assignment of lattice points would not solve this.

You can also think about it visually: think about a zig-zag of several elements, wrapped into a circle
(the elements of the antichain arranged in a circle, and then the lower bounds of adjacent pairs below them, like an inverted crown shape).
You can represent this as a planar digraph, but it is not a planar *Hasse* diagram: some relations will be going the wrong way on the page.
Thus we cannot represent it in \(\Q\x\Q\).

However, that is about the only problem.
We just need *more* dimensions.

#### An Infinite-Dimensional Lattice

I heard you liked dimensions.

We move from \(\Q\x\Q\) to `type Lattice = IntMap Rational`{.haskell} to fix this problem.
And with it, we move from primarily math to primarily Haskell.

One could technically justify this Haskell data type in terms of math language.
But what makes for an efficient data type makes for a weird presentation in the maths.

:::Details
It is a countably infinite product \(\N \to \Q \cup \{\top\}\) of the lattice \(\Q\) with a top element \(\top\) adjoined, but with *finite support*: all but a finite set of indices map to \(\top\).

(This is because `IntMap`{.haskell} is a finite data type, and missing indices are interpreted as being greater than any `Rational`{.haskell}.)
:::

The lattice structure on this type is given by the functions `infimum = unionWith min`{.haskell} and `supremum = intersectionWith max`{.haskell}.
The partial comparison `l1 <=? l2`{.haskell} requires that for all values in `l2`{.haskell}, the corresponding value in `l1`{.haskell} is less than or equal to it.
Thatʼs the cleanest way to describe it, the partial comparison function is a bit less clear:

```haskell
lattice :: Lattice -> Lattice -> PartialComparison
lattice l1 l2 = fold $ mergeValues l1 l2
  (const PosetLE)
  (const PosetGE)
  \x y -> partialOfTotal (compare x y)
```

Hopefully you can see that this lattice is at least large enough to fit any finitely generated poset.
An inefficient way to do it would be to give each variable its own dimension, and then the position of a variable is its set of upper bounds: not even using the rational structure, just mapping each dimension to `0`{.haskell} for example.

In fact, the rational structure is not too useful either: universe constraints are not often *insertions*.
However, I did not see a speedup with using integers, so I kept the rationals.
(I think there is a kind of monad-like insertion operation that would work better with the rationals.)

But more than being universal, we want to know that we can update it efficiently enough.
And that takes some figuring out.

### The Solver

```haskell
data Chain = Chain
  { numerator :: {-# UNPACK #-} !Int32
  , denominator :: {-# UNPACK #-} !Int32
  }

type Var = Int
type VarMap = IntMap
type Dim = Int
type DimMap = IntMap

type Lattice = DimMap Chain

data Solver = Solver
  { ctr :: !Int -- ^ next free dimension to use
  , vars :: !(VarMap Lattice) -- ^ current position of each point
  , points :: !(DimMap (Set.Set Chain)) -- ^ active points along each dimension
  , aparts :: !(VarMap IntSet.IntSet) -- ^ smaller var index to larger var index
  }
```

The semantic state consists of `vars`{.haskell} and `aparts`{.haskell}, while `ctr`{.haskell} and `points`{.haskell} are technically optimizations: you can compute `points`{.haskell} from `vars`{.haskell} and `ctr`{.haskell} from `points`{.haskell}.

`points`{.haskell} is used for quickly inserting at rationals.
By seeing which points are used along each dimension, we can quickly insert above or below a point at the end of a dimension.
In some cases, this saves doing a linear scan of `vars`.

- 
