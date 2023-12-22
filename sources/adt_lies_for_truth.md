---
title: Lies Told in Defense of the Truth
subtitle: "Subtypes and Quotients: The theory behind safe interfaces for ADTs in FP"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Hm can we just take a second to appreciate how beautiful the long stroke on the “Q” in “Quotients” is?

Ah …

Anyways. Hi!

Weʼre here to cut through the _lies_ and talk about the *truth* behind Haskell/PureScript datatypes!

If you arenʼt familiar with Algebraic Data Types (ADTs), theyʼre great and I highly recommend learning about them.

ADTs are a huge upgrade in expressivity compared to C- or Java-like datatypes, but they still have their limitations.
They do a great job at representing the _structure_ of data but fail to capture all of the _invariants_ that apply.
These limitations can be seen in informal assumptions that code is expected to maintain when working with particular ADTs.^[By “informal” I just mean “not written into the programming language one is using” ([e.g.]{t=} Haskell/PureScript).]

Weʼll be exploring how safe interfaces with ADTs actually work (for example, the common approach of “smart constructors”) as well as how more sophisticated type systems (ahem, dependent type systems) can precisely specify the invariants.

As usual, I will be giving most code snippets in Haskell/PureScript-ish syntax, and talking about the more theoretical side in Agda-ish syntax.

## Background

### Properties

### Relations

### Proofs

### Cardinality

## Better types

ADTs are great as base types, but we need to.

Subtypes and quotients have a nice duality.
For example, quotients are easy to _construct_ while subtypes are easy to _destruct_.

### Subtypes

A subtype refines a base type by whittling it down to only values that satisfy some property.

They do this by pairing the value of the base type with the proof that that value satisfies the subtypeʼs property.
If proofs are optimized away, this means that a subtype can have the same representation as its underlying type, but that is tricky to guarantee for type theories.

What this means is that subtypes are _difficult to construct_ (since one needs to provide the proof that the value actually has the property!) but _easy to destruct_.

### Quotients

Quotients are a bit stranger to the programmerʼs sensibility, and yet we will see that they do come up in practice.
Quotients allow you to take values of the base type and selectively erase differences between them.

You essentially get to manipulate what equality looks like for a type, within reason.
You do this by giving a relation that specifies which values from the base type to identify.
(Equality still needs to be reflexive, symmetric, and transitive! An equivalence relation, that is.)

Quotients are super easy to construct, since they will accept any value of the base type.
Metaphorically it will then erase the distinctions between different values that were identified by the relation.
The catch is that this makes them difficult to destruct.
How so?

Well, how does the type theory maintain the property that these distinctions are erased?
It needs to ensure that every function that uses a value from a quotient returns the same result for values that were identified under the quotient.

That is, destructing a quotient type with a function \(f\) requires a proof that for all \(x\) and \(y\) related by the quotientʼs relation, the function returns the same thing for both inputs.
\[x R y \to f(x) = f(y)\]

```purescript
data Quotient (t :: Type) (r :: t -> t -> Prop)

-- Property: `r x y` implies `mk x = mk y :: Quotient t r`
mk :: forall t r. t -> Quotient t r
```

```agda
quot : (t : Type) (r : t -> t -> Prop) -> Type

mk :
  (t : Type) (r : t -> t -> Prop) ->
  t -> quot t r

mk_eq :
  (t : Type) (r : t -> t -> Prop) (x y : t) ->
  r x y -> mk t r x = mk t r y

elim :
  (t : Type) (r : t -> t -> Prop) (z : Type)
  (f : t -> z)
  (forall (x y : t) -> r x y -> f x = f y) ->
  quot t r -> z
```

#### Cubical type theory

Cubical type theory lets you pattern match on quotients by making equalities constructors with arguments in the interval.

#### History of quotients

Why are they called quotients?
Group theory, category theory, type theory.

## Case studies

### Squashing the fun out of boolean

As our first very basic example, consider quotienting `Boolean`{.haskell} by a relation `Squish`{.haskell} that identifies `True`{.haskell} with `False`{.haskell}.
With some sleight of hand, weʼll also consider a similarly named (but different!) relation `Squash`{.haskell} that still generates the same quotiented type.

```haskell
data Boolean = True | False

data Squish (a :: Boolean) (b :: Boolean) where
  Squish :: Squish 'True 'False

type Squished = Quotient Boolean Squish

data Squash (a :: Boolean) (b :: Boolean) where
  Squash :: forall a b. Squash a b

type Squashed = Quotient Boolean Squash
```




### Sets and (hash)maps

Have you ever peeked into the source behind `Data.Set` in Haskell or PureScript?

Hereʼs Haskellʼs [`Data.Set.Set`](https://hackage.haskell.org/package/containers-0.6.6/docs/src/Data.Set.Internal.html#Set):

```haskell
data Set a    = Bin {-# UNPACK #-} !Size !a !(Set a) !(Set a)
              | Tip

type Size     = Int
```

And PureScriptʼs [`Data.Map.Map`](https://github.com/purescript/purescript-ordered-collections/blob/v3.0.0/src/Data/Map/Internal.purs#L70-L73) (since `Data.Set.Set k`{.haskell} is just `Data.Map.Map k Unit`{.haskell}):

```purescript
data Map k v
  = Leaf
  | Two (Map k v) k v (Map k v)
  | Three (Map k v) k v (Map k v) k v (Map k v)
```

Which we can specialize to `v = Unit`{.haskell}:

```purescript
data Set k
  = Leaf
  | Two (Set k) k (Set k)
  | Three (Set k) k (Set k) k (Set k)
```

They look different: Haskell implements balanced _binary_ trees, while PureScript implements 2–3 trees I believe?

Haskell also throws in a `Size`{.haskell} field for optimizing that operation.


They look like ordinary ADTs … but theyʼre not!!

Letʼs consider the purpose of these collections:
they seek to give lookups in \(O(\log n)\) time-complexity.^[There are lots of other concerns when optimizing functional data structures, like sharing, but this is enough for now.]

Obviously they cannot scan every element, that would be \(O(n)\), so they cut down on the search space by using binary search to hone in on the element with repeated comparisons:

```haskell
-- | \(O(\log n)\). Is the element in the set?
member :: forall a. Ord a => a -> Set a -> Bool
member = go
  where
    go !_ Tip = False
    go x (Bin _ y l r) = case compare x y of
      LT -> go x l
      GT -> go x r
      EQ -> True
```

::: {.Bonus box-name="Aside"}
If you havenʼt seen Haskell and PureScriptʼs `compare :: forall a. Ord a => a -> a -> Ordering`{.haskell}, check it out: `Ordering`{.haskell} is the simplest ADT that makes APIs so much nicer.
:::

However, that requires maintaining an **invariant**: the elements must come _in order_ in the set.
Otherwise an element that _is_ in the set could be missed.
