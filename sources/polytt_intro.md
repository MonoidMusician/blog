---
title: Introduction to PolyTT
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

## Polynomial Functors

Polynomials are formal functions
  on number-y stuff, (semi)rings, etc.

Polynomial functors are formal functors
  on the category of types, and similar categories

What does formal mean in math?

- A syntactic object with an indended semantic interpretation (my own definition)
  - Typically involves a “formal parameter” like \(x\).
- Hint: this means they have data underlying them
- (Aside:) This data does not have to correspond to the interpretation!
  Distinct polynomials over finite fields can have indentical interpretations, and Galois theory cares about this distinction.

Coproduct of representables

- Pointwise coproduct

… skip some steps …

The data underlying a polynomial functor `P`{.agda data-lang=PolyTT} is:

- a type `base P : Type`{.agda data-lang=PolyTT}
- a function `fib P : base P -> Type`{.agda data-lang=PolyTT} of fibers over the base

```{.agda data-lang=PolyTT}
-- Interpret a Poly as a functor Type → Type
def ofP : Poly → Type → Type :=
  λ P Y → Σ (p : base P), fib P p → Y
```

Morphisms from `P`{.agda data-lang=PolyTT} to `Q`{.agda data-lang=PolyTT}:

- a mapping `base f : base P -> base Q`{.agda data-lang=PolyTT}
- a mapping `fib f : Π (p : base P), fib Q (base f p) -> fib P p`{.agda data-lang=PolyTT}

But we bundle this as a single mapping `f : Π (p : base P), Σ (q : base q), (fib Q q -> fib P p)`

```{.agda data-lang=PolyTT}
-- Interpret a morphism as a natural transformation
def ofM (P Q : Poly) : P ⇒ Q → Π (Y : Type), ofP P Y → ofP Q Y :=
  λ m Y (p , v) →
    let (q , f) := m p in
    ( q , λ y → v (f y) )
```

## Special Syntax for Morphisms

Values of objects, we don't care about.
But morphisms, we do.
(Also they're painful to write.)

Imperative program? linear logic? you decide.

Negative sinks are into a formal parameter that you never get to access directly.
Enforces parametricity over that formal parameter, even in DTT where we could add classical axioms.

Continuation-y.
(Call exactly once continuations.)

### Id

Map base to base.
Do the sink.

### Swap

Three ways to write it.

### Eval

Follow the types? :D

## Typing

Cells in memory.
Borrow their value.

No normal form for programs this way, just the base/fib data.

### Unpair

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
