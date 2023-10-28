---
title: A Semiring From Any Semilattice
subtitle: A mathematical pun?
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Iʼve been working on some theoretical aspects of programming recently.
Writing new compiler optimization passes.
Thinking about [parsers](https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Languages/Talk.purs) through the lens of [selective applicative functors](https://hackage.haskell.org/package/selective-0.7/docs/Control-Selective.html#t:Selective), and tweaking them to encode [exclusive choice](https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Selective.purs).

If you go far enough down the rabbit hole, it turns out that you want semirings for static analysis.
This is not unheard of in compilers!
Itʼs a really good technique for analyzing control flow, for example: information about exclusive branches are combined additively, and information from sequential operations are combined multiplicatively.
It is especially appropriate because, semantically speaking, you want those sequential operations to distribute over the branching.

(You can already see this in more typical typeclasses like `Alternative`{.haskell}, which is naturally analyzed by taking `<*>`{.haskell} to `*`{.haskell} and `<|>`{.haskell} to `+`{.haskell}.
Itʼs just that Iʼm interested in augmenting `Selective`{.haskell} to encode exclusive choice too.)

This led me to come up with this construction: how to make a semiring out of a semilattice.

This construction answers the question, “if you need a semiring for static analysis, how do you also keep other data around that does not care about the branching structure?” (like, say, a monoid).

:::Details
Specifically in selective applicative parsers, I need it to answer the question of why aggregating information about the grammar is a valid thing to do across parser combinator segments, no matter how they are combined.

And in the compiler pass I was doing, I was implementing demand analysis via semirings (especially the [min tropical semiring](https://en.wikipedia.org/wiki/Tropical_semiring)).
I actually donʼt have specific information I was considering aggregating as a semilattice, but it was a possibility that might come up, especially if I want to fuse some passes together.
Right now my one pass is really three traversals of the tree, with various monad stacks of reader, writer, and state.
(Yes I used all three.)
:::

I don't know if this construction is well-known!
Let me know if you have a reference for it.

:::{.Key_Idea box-name="tl;dr"}
You can make a semiring out of a semilattice by adjoining a new zero element. Lifting the semilattice operation in the two obvious ways gives you `+`{.haskell} and `*`{.haskell}. Idempotence gives distributivity(!).
:::

## Background

(Bounded) semilattices are monoids whose operation is also idempotent: \(x \diamond x = x\) for all \(x\).

I will write the monoid operation as `x <> y`{.haskell} and as \(x \diamond y\),
and the empty element as `mempty`{.haskell} or \(e\).
<!-- okay but what about schwa -->

:::Bonus
Semilattices have deep connections to order theory: they induce a really nice preorder given by \(x \leq y\) when \(x \diamond y = x\) (or vice-versa, depending on whether you are talking about meet or join semilattices – and no, I cannot keep them straight 🏳️‍🌈).
But we donʼt need the order theory here.
:::

Semirings are rings without subtraction: just addition and multiplication and their identities, zero and one, respectively.
And [distributivity] and [annihilation] laws to intertwine these two monoids.

The funny part of this is that “semi-” means different things: semirings are just missing subtraction (kind of a weird use of semi, which is why some call them [rigs](https://ncatlab.org/nlab/show/rig)), but semilattices are literally half of a lattice (one idempotent monoid instead of two interlinked).

(Lattices are actually closely related to semirings: they have the same shape of operations, and you can turn every bounded _distributive_ lattice into a semiring – in two ways, in fact, since you can make a lattice with the opposite order.)

So itʼs like a mathematical joke that they can be related to each other at all!

How do we get two monoids out of one??

## Description

The key idea is to adjoin a zero.
Thatʼs it.

The rest of the moves can be determined from that premise, so letʼs see how it works:

```{.haskell data-lang="PureScript"}
data WithZero t = Zero | NonZero t

-- Imagine that `t` is really a `Semilattice`
-- (this does not exist as a class in PureScript)
instance Monoid t => Semiring (WithZero t) where
  zero = Zero
  one = NonZero mempty

  add Zero Zero = Zero
  add Zero r = r
  add l Zero = l
  add (NonZero l) (NonZero r) = NonZero (l <> r)

  mul (NonZero l) (NonZero r) = NonZero (l <> r)
  mul _ _ = Zero
```

The two operations are the semilattice operation lifted through `Maybe`{.haskell} in the two possible ways:

- `add`{.haskell} follows the pattern of the default `Semigroup a => Monoid (Maybe a)`{.haskell} instance, that uses `Nothing`{.haskell} as its identity.
  This makes sense since weʼre adding `Zero`{.haskell}, the identity for `add`{.haskell}.
  Indeed, it is forced by the laws, and the fact that we only have one binary operation to use.
- `mul`{.haskell} is like the other possible instance, `Monoid a => Monoid (App Maybe a)`{.haskell}, formed by `(<>) = lift2 (<>)`{.haskell}.
  This is likewise forced by the annihilation law and the fact that we only have one binary operation to use.

## Laws

Iʼm going to be lazy and use math notation for the laws, with the understanding that when I say \(x \neq 0\) for example, it means in Haskell/PureScript that `x = NonZero x' :: WithZero t`{.haskell} for some unique `x' :: t`{.haskell}, and if \(x, y \neq 0\) then \(x \diamond y\) means `NonZero (x' <> y')`{.haskell}.

### Distributivity

The fun part is the left and right distributivity laws:

To prove left distributivity, \(x * (y + z) = x * y + x * z\), we look at some cases:

- If \(x = 0\), then we have \(0 * (y + z) = 0 = 0 * y + 0 * x\).
- If \(y = 0\), then we have \(x * (0 + z) = x * z = x * 0 + x * z\).
- If \(z = 0\), then we have \(x * (y + 0) = x * y = x * y + x * 0\) similarly.
- So now we can assume that all three variables are nonzero.
  But that means we fall back to the underlying semilattice operation:
  \[x \diamond (y \diamond z) = (x \diamond y) \diamond (x \diamond z).\]
  But by commutativity and associativity, \[(x \diamond y) \diamond (x \diamond z) = (x \diamond x) \diamond (y \diamond z).\]
  And finally we finish off with idempotence: \[x \diamond (y \diamond z).\]


We prove right distributivity in the same way \((x * y) + z = x * z + y * z\), based on the calculation \[(x \diamond z) \diamond (y \diamond z) = (x \diamond y) \diamond (z \diamond z).\]

The takeaway is that **idempotence of the semilattice gives us distributivity of the semiring**.
This is why having a semilattice and not merely a monoid is essential.

This does make some sense: if weʼre aggregating information that does not care about branching structure at all, well, semilattices are great models for accumulating knowledge.
Idempotence says you only learn a fact once.

:::{.Bonus box-name="Generalizing"}
We donʼt require multiplication to be commutative, so if you drop the left-distributivity law, you could get away with a [right-regular band](https://en.m.wikipedia.org/wiki/Band_(algebra)#Right-regular_bands) with the law \(x \diamond y \diamond x = y \diamond x\).

I think left-distributivity is a bit weirder than right-distributivity, in the context of control flow.
Right distributivity just says you can copy any training code into each case branch.

However, in general Iʼm a fan of left-regular bands, since they intuitively preserve order.

Also, to be fair, you could absolutely disregard some semiring laws for the sake of static analysis of programs:
you donʼt always want to treat programs as purely algebraic structures, and often want to dig into the details of how they were constructed.

Like, if youʼve factored out common control flow, thatʼs almost always for a reason!
So your static analysis should reflect that.
:::

### Annihilation

We made this true by definition of `mul`{.haskell}: \(0 * x = 0 = x * 0\).

### Additive monoid

#### Identity

We also made \(0 + x = x = x + 0\) true by definition of `add`{.haskell}.

#### Associativity

So we just need to prove that \(x + (y + z) = (x + y) + z\) for \(x, y, z \neq 0\).
But that follows from the semilatticeʼs associativity: \(x \diamond (y \diamond z) = (x \diamond y) \diamond z\).

### Multiplicative monoid

#### Identity

For \(1 * x = x = x * 1\), we need two cases: \(x = 0\) and \(x \neq 0\).
But if \(x = 0\), it is trivial still.
(This is the nice way the identities and annihilator elements interact.
They donʼt add any proof burden to the other.)

So for \(x \neq 0\) (and since \(1 \neq 0\) and \(1\) is given by the semilatticeʼs identity \(e\)), we look at the underlying semilattice and find that \(e \diamond x = x = x \diamond e\) as we want.

#### Associativity

Same case analysis as usual: if \(x, y, z \neq 0\) then we get associativity from the semilattice, otherwise both sides equal \(0\) by the power of the annihilator.

### Absorption laws fail

Note that we cannot make a lattice out of the semilattice – thatʼs a step too far.
Intuitively from the order theory point of view, thereʼs no reason why would would be able to, since the meet and join operations of a lattice have opposite views of the preorder of the lattice.

And algebraically, the two absorption laws would fail in general:
\(x * (x + y) = x\) and \(x + (x * y) = x\) (even stating them like that looks weird).
For \(x \neq 0\), by idempotence of the semilattice we would see \(x \diamond (x \diamond y) = x \diamond y\), which only equals \(x\) if \(y = e\).
Thereʼs just no way to get rid of the extra \(y\) there if we are sticking to one operation.

## Iterating

You could technically iterate this construction, since `add`{.haskell} and `mul`{.haskell} are both idempotent, commutative, associative operations now.
However itʼs not terribly interesting.

You end up adjoining some number of identities and annihilators to the underlying semilattice.
(New top/bottom elements, depending on which way you look at it.)
The order that you do this in does not matter, only how many times you choose to do each way.
