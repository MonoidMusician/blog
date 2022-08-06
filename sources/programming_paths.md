---
title: Why You Should Believe in HoTT Path Induction as a (Haskell) Programmer
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

## Outline

There are two tasks:

1. Accepting that isomorphism is the right notion of equality (!) for types.
  Yes I do mean equality -- nothing less.
2. That programs you write with `subst` obey the equations specified by \(J\).
  In particular, the version you wrote for one type is the same as the version wrote for the other type with coercions inserted.
  Or maybe itʼs the other way, the one you wrote with explicit coercions could have all just been identities in some parallel universe.

## The Situation

`List`{.haskell}, `Tsil`{.haskell}, `Array`{.haskell}

## Definitions of Equality
Weʼre not going to go in depth here (thatʼs what the rest of this article is for!), but we need to set some ground rules.
We start with two Haskell definitions of type-equality:

```haskell
newtype Leibniz a b = EquiConvertable (forall f. f a -> f b)

data x :~: y where
  Refl :: z :~: z


```

Unfortunately, we really need dependent types to explain HoTT, so we canʼt just stick with Haskell and I will invoke Agda as necessary.
Mostly for syntactic convenience, since it is _very_ helpful to have a coherent syntax to talk about the concepts and types we are discussing!
Note that the main difference is the roles of `:`{.ot} and `::`{.ot} are flipped in Haskell vs Agda, as type annotation versus list constructor.^[I personally like the Agda convention better, but of _course_ I say that as a type theorist 😉.]
The substantial programs will still be in Haskell.

Here are two equivalent definitions in Agda:
```agda
data _≡_ (x : Type) : Type -> Type where
  refl : x ≡ x

data _≡_ : Type -> Type -> Type where
  refl : (x : Type) -> x ≡ x


```

### Coercible
Note that coercions in the sense of Haskellʼs `Coercible`{.haskell} are a fascinating topic, but largely orthogonal to what we are talking about here!
We are going to ignore them for the rest of this article.

::: Bonus
One could think of coercions as paths that compile down to the identity, but that requires referencing some underlying (runtime) model which we donʼt talk about in HoTT.
:::

Instead we will talk about coercing along a _path_.
This means that given some path `(p :: x :~: y)`{.haskell} we can write `(coe p :: x -> y)`{.haskell} for the coercion along the path `p`{.haskell}.

### Axiom \(J\)

Our goal is to explain path induction for types, which is also referred to as Axiom \(J\) for historical reasons.
Hereʼs the type of Axiom \(J\) (specialized to `Type`{.haskell}):
```agda
J :
  (x : Type) ->
  (P : (y : Type) -> x ≡ y -> Type) ->
  P x refl ->
  (y : Type) ->
  (p : x ≡ y) ->
  P y p


```

This says:

- Given our first type `(x : Type)`{.agda},
- And given a family `(P : (y : Type) -> x ≡ y -> Type)`{.agda} ranging over possible second types `(y : Type)`{.agda} and equality proofs `x ≡ y`{.agda},
- If we know that `P x refl`{.agda} holds,
- Taking any other second type `(y : Type)`{.agda}
- And proof of their equality `(p : x ≡ y)`{.agda},
- We get a proof that the family holds at `p` too: `P y p`{.agda}.
  This is just from the fact that it held at `refl`{.agda}, which is what everyone finds surprising!

Letʼs pick this apart a bit more and put it into a programming context.

- The type family `P` is going to be like a schema for programs varying along an abstract interface.
  For example, we could take `P y `
- The type `x` is the one that our programs _expect_ to use, while `y` will be the type they are _given_.
- Of course you canʼt just swap out types like that, so we ask for the assurance that they are equivalent types: the proof that `x ≡ y`.

Original monomorphic program.
Type family that specifies what we want to target.


## Functors
There are a few related meanings of functors that we need to disentangle before we go further.

1. The simplest meaning: anything of kind `Type -> Type`{.haskell}.
  More broadly: any type-level function.

2. The next simplest is our beloved `Functor`{.haskell} typeclass:

    ```haskell
    class Functor f where
      fmap :: forall a b. (a -> b) -> f a -> f b


    ```

3. There are also related classes: `Contravariant`{.haskell}, `Invariant`{.haskell}, `Phantom`{.haskell}.
  Thereʼs a fifth I want to include for completeness: `EquiFunctor`{.haskell}, which I will explain later.
  Itʼs a folklore result that any standard functor (in the first sense) will satisfy one of these classes.

    ```haskell
    class Contravariant f where
      contramap :: forall a b. (a -> b) -> f b -> f a

    class Invariant f where
      invmap :: forall a b. (a -> b) -> (b -> a) -> f a -> f b

    class Phantom (f :: Type -> Type) where
      coerce :: forall a b. f a -> f b

    class EquiFunctor (f :: Type -> Type) where
      equimap :: forall a b. Leibniz a b -> f a -> f b


    ```

4. Finally we have the category-theoretic notion of Functor, which goes much much further.
  We will not try to model the full scope of this in Haskell, but it is good to know that there is an underlying concept that unifies all of the above.

## Path induction with functors
Letʼs explore.

## Equality of programs

## Path induction for programs

## Parametricity
