---
title: Introduction to PolyTT
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

## Polynomial Functors

Polynomials are formal functions
  on number-y stuff, (semi)rings, etc.

- \(x^2 + 2\)
- \(-2(x^3 + 5x)(x - 1) = -2x^4 + 2x^3 - 10x^2 + 10x\)

Polynomial functors are formal functors
  on the category of types, and similar categories

- \(y^2 + 2\)
  - ```haskell
  data P y
    = Fn (Bool -> y)
    | Const Bool
  ```
  - ```haskell
  data Pair y = MkPair y y
  data P y
    = Term1 (Pair y)
    | Term2
    | Term3
  ```
- \(5 y^\mathbb{N} + y\)
  - ```haskell
  data P y
    = Term1 Five (Natural -> y)
    | Term2 y
  ```
- \(\sum(\ell : \mathbb{N}), y^{\text{Fin}\ \ell}\)
  - ```haskell
  data List y = Nil | Cons y (List y)
  ```
  - ```agda
  record List (a : Type) : Type where
    len : ℕ
    items : Vec len a
  ```



:::Note
What does formal mean in math?

- A syntactic object with an intended semantic interpretation (my own definition)
  - Typically involves a “formal parameter” like \(x\) or \(y\).
- Hint: this means they have data underlying them
- (Aside:) This data does not have to correspond to the interpretation!
  Distinct polynomials over finite fields can have identical interpretations, and Galois theory cares about this distinction.
:::

## Category of polynomial functors

Polynomials are “just” coproducts of representables.

- Representable functors \(y^S\) are functions from an indexing type to the parameter.
  They are potentially infinite products of the parameter.
  - ```haskell
  class Representable k f where
    iso :: forall a. (k -> a) <-> f a
  instance Representable () Identity where
    ...
  data Pair y = MkPair y y
  instance Representable Bool Pair where
    ...
  ```
    - [Hackage](https://hackage.haskell.org/package/representable-functors-3.2.0.2/docs/Data-Functor-Representable.html)
    - [Pursuit](https://pursuit.purescript.org/packages/purescript-functor-vector/0.1.0/docs/Data.Functor.Representable#t:Representable)

- Coproducts in the category of functors are computed pointwise:
  - ```haskell
  data BinaryCoproduct f g a
    = Left (f a)
    | Right (g a)
  ```

- But we want infinite coproducts too, and this is where we need dependent types!
  - ```agda
  record BinaryCoproduct (f g : Type -> Type) (a : Type) : Type where
    side : Bool
    value : (if side then g else f) a
  record InfiniteCoproduct (I : Type) (fs : I -> (Type -> Type)) (a : Type) : Type where
    i : I
    value : (fs i) a
  ```

- Tensor structures:
  - Coproduct `+`
  - Product `×`
  - Day convolution `⊗`
  - Composition `◁`


## Polynomials in PolyTT
_… skipping some steps …_

The essential data underlying a polynomial functor `P`{.agda data-lang=PolyTT} is:

- a type `base P : Type`{.agda data-lang=PolyTT}
  - think of as the type of constructors to choose from
- a function `fib P : base P -> Type`{.agda data-lang=PolyTT} of fibers over the base
  - think of as giving the arity of each constructor

We write a polynomial as a summation:

```{.agda data-lang=PolyTT}
Σ (i : base P), y^(fib P i)

-- A polynomial can be written in standard form
-- via `base` and `fib`
def Poly-eta (P : Poly)
  : P = (Σ (i : base P), y^(fib P i))
  := refl
```

From this essential data, we can construct the corresponding functor:

```{.agda data-lang=PolyTT}
-- Interpret a Poly as a functor Type → Type
def ofP (P : Poly): Type → Type :=
  λ Y → Σ (p : base P), fib P p → Y
```

## Morphisms of polynomials

_The cool stuff!_

Morphisms from `P`{.agda data-lang=PolyTT} to `Q`{.agda data-lang=PolyTT}:

- a mapping `base f : base P -> base Q`{.agda data-lang=PolyTT}
  - pick a constructor for the output based on the input constructor
- a mapping `fib f : Π (p : base P), fib Q (base f p) -> fib P p`{.agda data-lang=PolyTT}
  - populate the fields of the output constructor, by choosing which input fields to copy from
  - note the contravariance!! `fib Q`{.agda data-lang=PolyTT} before `fib P`{.agda data-lang=PolyTT}

But we bundle this as a single mapping `f : Π (p : base P), Σ (q : base q), (fib Q q -> fib P p)`{.agda data-lang=PolyTT}.

Again, from this data we really want to get a natural transformation out of it:

```{.agda data-lang=PolyTT}
-- Interpret a morphism as a natural transformation
def ofM (P Q : Poly) : P ⇒ Q → Π (Y : Type), ofP P Y → ofP Q Y :=
  λ m Y (p , v) →
    let (q , f) := m p in
    ( q , λ y → v (f y) )
```

## Special Syntax for Morphisms

Values of objects are not interesting from a type theoretic point of view.
But morphisms are special.
(They're painful to write outside of PolyTT!)

There is the positive fragment of the theory, which is just standard type theory.
We add a negative fragment to the theory, which operates linearly.
It does not have special types, but its values are thought of as obligations to discharge – exactly once.
Or as inputs in wiring diagrams.

We introduce negative “sinks” paired with a positive component, as a “box”^[Terminology subject to revision].
The positive component comes first, since the type of the sink depends on it!
But when we talk about wiring diagrams, the positive component is the output, and the negative is the input.
This is why we use a leftwards arrow to write it: `(p+ <~ p-)`{.agda data-lang=PolyTT}.

If we have a polynomial `P`{.agda data-lang=PolyTT}, then introducing the box `(p+ <~ p-)`{.agda data-lang=PolyTT} will give us two new variables in scope:
```{.agda data-lang=PolyTT}
+ p+ : base P
- p- : fib P p+
```

A morphism of polynomials is written as a function from one box to another:

```{.agda data-lang=PolyTT}
λ (p+ <~ p-) =>
  return (p+ <~ p-)
```

You can fulfill an obligation by writing to a sink.

You can split sinks of (dependent) pairs into individual sinks:
```{.agda data-lang=PolyTT}
λ (p+ <~ pq-) =>
  -- Split
  let (p- , q-) := pq-;
  -- Write to one
  q- <- (some positive value whose type matches up);
  return p+ <~ p-
```

There's a few other things, but these are the fundamentals.

### Id

Map base to base.
Do the sink.

### Maybe to List

### Swap

Three ways to write it.

### Eval

Follow the types? :D

# Type Theory behind it

_Imperative program? linear logic? you decide._

Negative sinks are into a formal parameter that you never get to access directly.
Enforces parametricity over that formal parameter, even in DTT where we could add classical axioms.

Continuation-y.
(Call exactly once continuations.)


Cells in memory.
Borrow their value.

No normal form for programs this way, just the base/fib data.

## Unpair

Given a sink whose type is a sigma type:

```{.agda data-lang=PolyTT}
A : Type
B : A → Type
pq⁻ : (Σ (a : A), B a)⁻
```

If you unpair that sink:

```{.agda data-lang=PolyTT}
let⁻ (p⁻ , q⁻) := pq⁻;
......
```

For the rest of the program, you end up with two sinks and write to the original sink:

```{.agda data-lang=PolyTT}
p⁻ : (A)⁻
q⁻ : (B⁻ (borrow p⁻))⁻
pq⁻ ← (borrow p⁻ , borrow q⁻)
```

Note that you have to borrow the value of `p⁻`{.agda data-lang=PolyTT} to give the type of `q⁻`{.agda data-lang=PolyTT}.
And then their values are immediately borrowed and written as a pair (now a positive value) to `pq⁻`{.agda data-lang=PolyTT}, such that `borrow pq⁻ = (borrow p⁻ , borrow q⁻)`{.agda data-lang=PolyTT}.


## Macros
