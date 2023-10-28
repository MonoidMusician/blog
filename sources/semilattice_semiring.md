---
title: A Semiring From Any Semilattice
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Iʼve been working on some stuff.
Like parsers and selective applicative functors.
And selective applicative parsers.

If you go far enough down the rabbit hole, it turns out that you want semirings for static analysis.
This is not unheard of in compilers!
This led me to come up with this construction.

This construction answers the question, “if you need a semiring for static analysis, how do you also keep other data around that does not care about the branching structure (like, say, a monoid)?”

I don't know if this construction is well-known!
Let me know if you have a reference for it.

## Description

The idea is to adjoin a zero.
Thatʼs it.

```{.haskell data-lang="PureScript"}
data WithZero a = Zero | NonZero a

-- Imagine that `a` is a `Semilattice`, which does not exist in PureScript
instance Monoid a => Semiring (WithZero a) where
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

- `add`{.haskell} is the default `Semigroup a => Monoid (Maybe a)`{.haskell} instance, that uses `Nothing`{.haskell} as its identity.
  This makes sense since weʼre adding `Zero`{.haskell}, the identity for `add`{.haskell}.
  Indeed, it is forced by the laws, and the fact that we only have one binary operation to use.
- `mul`{.haskell} is the other instance, `Monoid a => Monoid (App Maybe a)`{.haskell}, formed by `(<>) = lift2 (<>)`{.haskell}.
  This also makes sense and is forced by the annihilation law and the fact that we only have one binary operation to use.

## Laws

The fun part is the distributivity law:

\(x * (y + z) = x * y + x * z\)

- If \(x = 0\), then we have \(0 * (y + z) = 0 = 0 * y + 0 * x\), which is true.
- If \(y = 0\), then we have \(x * (0 + z) = x * z = x * 0 + x * z\), which is also true.
- Symmetrically, \(z = 0\) is also handled.
- So now we can assume that all three variables are nonzero.
  But that means we fall back to the underlying semilattice operation:
  \[x \diamond (y \diamond z) = (x \diamond y) \diamond (x \diamond z).\]
  But by commutativity and associativity, \[(x \diamond y) \diamond (x \diamond z) = (x \diamond x) \diamond (y \diamond z).\]
  And finally we finish off with idempotence: \[x \diamond y \diamond z.\]

This is why having a semilattice and not merely a monoid is essential.

This does make some sense: if weʼre aggregating information that does not care about branching structure at all, well, semilattices are great models for accumulating knowledge.
Idempotence says you only learn a fact once.

We donʼt require multiplication to be commutative, so if you drop the right-distributivity law, you could get away with a [left-regular band](https://en.m.wikipedia.org/wiki/Band_(algebra)#Left-regular_bands) with the law \(x \diamond y \diamond x = x \diamond y\).
(Iʼm too tired to think if this is the right choice for static analysis of programs or not.)

## Iterating

You could iterate this construction, since `add`{.haskell} and `mul`{.haskell} are both idempotent, commutative, associative operations now.

Thus you end up adjoining some number of identities and annihilators to the underlying semilattice.
(New top/bottom elements, depending on which way you look at it.)
