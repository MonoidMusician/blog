---
title: Day Convolution and Polynomial Functors
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026/10/20
---

The Day convolution^[Named after Brian Day, who first described it] of functors is one of my favorite constructions.
It can look complicated, but the most approachable direction is through polynomial functors, also known as containers.

:::{.Warning box-name="Background"}
This post covers the topic from several different angles, so hopefully you get something out of it.
To get the most of out it, ideally you are comfortable with dependent sums (sigma types / dependent pairs) and dependent products (pi types / dependent functions), some Haskell and/or Mikan (née Agda), some category theory (functors, natural transformations, closed categories and/or adjunctions).
For the more advanced category theory stuff, co-ends and monoidal categories will be used to define the Day convolution in not-quite-full generality.

You may be interested in the closely related posts here:

- [3+ Views of Applicative Functors: A Rondo on Monoids and Functors](applicatives.html).
- [(Extra)Natural Transformations](extranatural-transformations.html).

:::

---

To avoid burying the lede, here is a Haskell definition to puzzle over, if you want:

```haskell
data Day f g r where
  Day :: f u -> g v -> (u -> v -> r) -> Day f g r
```

With the natural choice of `(u -> v -> r)`{.haskell} being the tuple constructor `(,)`{.haskell}, you get a constructor `f u -> g v -> Day f g (u, v)`{.haskell}, which kind of shows how it is closely related to lax monoidal functors / applicative functors, which come with an operation `liftA2 (,) :: m u -> m v -> m (u, v)`{.haskell}, which can be rephrased as `Day m m (u, v) -> (u, v)`{.haskell}.

---

And my favorite example of the Day convolution, which I havenʼt seen talked about anywhere:

:::Key_Idea
If you want to represent a matrix or table, you use the Day convolution of rows and columns.

In particular, if you want to represent an arbitrary-sized rectangular matrix, you use `Day List List`{.haskell}!

With fixed-size functors, you can make use of the fact that, [e.g.]{t=}, `Day List Vec3`{.haskell} is isomorphic to `Compose List Vec3`{.haskell}: you have one unified list of rows, and you know that each column inside always has three elements.

But when both rows and columns are variable-length, you usually are stuck working with the lossy format `Compose List List`{.haskell} and just hoping that all of the inner lists are magically the same length.

Instead, `Day List List`{.haskell} says precisely that there is a number of rows and a number of columns, and then there are elements filling out the table with those dimensions: exactly `rows * cols` elements, as you would want.

\[(\mathrm{List} \ox \mathrm{List})(y) \cong \sum_{r \as \N} \sum_{c \as \N} y^{r \x c}\]
:::

## Pre-Understanding

Letʼs put the Day convolution in context before diving into details.

The Day convolution takes two functors \(F, G\) and describes a new functor \(F \ox G\) of the same type.
(The binary operator \(\ox\) is kind of a generic notation for a “monoidal tensor” like this. In this article, it will stand for the Day convolution until it needs to be disambiguated as \(\ox_{Day}\) much later.)

There are restrictions on when this makes sense and exists, but for our purposes, we will be talking about endofunctors on \(\mathrm{Set}\), the category of sets (ahem, types) with ordinary functions.

Key properties of the Day convolution:

- It is symmetric, meaning it has an isomorphism \(\sigma_{F,G} : F \ox G \cong G \ox F\) that is its own inverse.^[\({\sigma_{F,G}}^{-1} = \sigma_{G,F}.\)]

- It is associative, with isomorphism \(\alpha_{F,G,H} : (F \ox G) \ox H \cong F \ox (G \ox H)\).

- The identity functor \(\mathrm{Id} : \mathcal C \to \mathcal D\) serves as its identity, with left and right unitors \(\lambda_{H} : \mathrm{Id} \ox H \cong H \cong H \ox \mathrm{Id} : \rho_{H}\).

This makes it a symmetric monoidal tensor, forming the symmetric monoidal category of functors under Day convolution.


### Endofunctor Composition

Day convolution of endofunctors is related to functor composition, \(F \lhd G\), defined by \((F \lhd G)(X) = F(G(X))\).

For endofunctors, Day convolution is basically the symmetric version of composition,
and one of the most common things to do to a Day convolution of endofunctors is to turn it into the corresponding composition, \(F \ox G \to F \lhd G\).
And because Day convolution is symmetric, \(F \ox G \to G \lhd F\) also works.

This is one explanation for why applicatives and monads are closely linked: via this map, you can turn the monad operation \(F \lhd F \to F\) into the applicative operation \(F \ox F \to F\).


### Enriched

If you [like monoids like I do](applicatives.html#background), Day convolution in quite a lot of generality can be defined for functors \(\mathcal C \to \mathcal V\):

- \(\mathcal V\) is a _symmetric_ monoidal category that is *cocomplete*.
- \(\mathcal C\) is a \(\mathcal V\)-enriched monoidal category.

You might call these copresheaves, because it is a covariant functor to the base of enrichment, \(\mathcal V\).
A presheaf would usually be from \(\mathcal C^{op} \to \mathcal V\),
where the typical case is \(\mathrm{Set}\)-enriched, [i.e.]{t=} conventional categories, to \(\mathrm{Set}\).


I guess there is a story for promonoidal categories, which may generalize to functors \(\mathcal C \to \mathcal D\)?
But I still need to look into that.



## Polynomial Endofunctors

One thing I love about polynomial functors is that they make the Day convolution look _easier_ than functor composition.

Normally we think about polynomials on the real numbers.
These polynomials look like \(\pi x^2 + 2x - e + 1\): a finite sum of terms, each one being \(x\) to some power (a natural number) multiplied by a real-number coefficient.
A little more explicitly: \(\pi x^2 + 2x^1 - ex^0 + 1x^0\).

There is nothing special about the real numbers: polynomials make sense for any field, ring, or even semiring (no division or subtraction required!).
It also works in some stranger situations too: \(x\) can be a square matrix (multiplication is not commutative, but powers still make sense), and coefficients can be the scalars for that matrix.

Now, think about this: sets also feature operations that look like addition (=&nbsp;coproduct / sum / disjoint union), multiplication (=&nbsp;product / Cartesian product), and exponentiation (=&nbsp;functions / \(\mathrm{Set}\) morphisms / representables).
Do polynomials work there too?
Sure!

[The “arithmetic” of sets](https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types) operates over nontrivial data, so properties like identities and associativity are categorified into isomorphisms, which are not trivial equations like they are for numbers.^[Sets are isomorphic [iff]{t=} they have the same cardinality (= number of elements), but isomorphisms of sets are nontrivial.]
But this only enhances the story of polynomial endofunctors; it does not detract from it, since everything works so beautifully with preserving this data.

Letʼs consider polynomials in \(\mathrm{Set}\), to be interpreted as endofunctors (that is, functors from \(\mathrm{Set}\) to \(\mathrm{Set}\)).

There are a few different ways to do it.

Maybe we sum them up by power.
\[\sum_{n \as \N} C(n) \x y^{\mathrm{Fin}(n)}.\]

But restricting powers to be finite and represented by \(\mathrm{Fin}\) in particular is not ideal ….

So maybe they could look like this:
\[\sum_{s \as S} C(s) \x y^{P(s)},\]
with a coefficient and a power for each summand.

But the coefficients are actually not necessary!<!-- ^[Not for discrete sets, at least.] -->
This is a simpler representation that has nice properties:

\[R(S \Join P)(y) = \sum_{s \as S} y^{P(s)}\]

It can even be written more suggestively as
\[\sum_{s \as S} \prod_{p \as P(s)} y\]
which will come in handy later.

Breaking this down:

- \(S\) is the set of shapes (or summands).
  In type theory, \(S : \mathrm{Type}\).

- \(P(s)\) is the set of positions (or powers) for shape \(s\).

  :::Note
  In type theory, \(P(s) : \mathrm{Type}\), so \(P\) itself has the type \(S \to \mathrm{Type}\).
  You can call it a **type family** (or set family, or just family) on \(S\), but that is just a name for a function from \(S\) to the type universe \(\mathrm{Type}\).
  :::

- \(\sum\) denotes a dependent sum (“dependent pair”): in terms of data, an element \(s : S\), and then an element of type \(y^{P(s)}\) dependent on the value of \(s\).
- \(\prod\) denotes a dependent product (“dependent function”): it is a function that takes any \(p : P(s)\) and returns an output.
  In this case, the output type is the functor parameter \(y\) which, by definition, is not dependent on the shape or position.
  This is why it is equivalent to the non-dependent function \(P(s) \to y\), which can also be written as an exponential in sets: \(y^{P(s)}\) – notice the flip of which gets written on the left and right.

  :::Note
  To see why non-dependent functions are called exponentials, count the number of possible functions between small types.
  For each input, the output can be independently chosen, leading to repeated multiplication [a.k.a.]{t=} exponentiation.
  :::

Together, the shapes \(S\) and positions \(P\) describe the data of the polynomial, which I will denote as \(S \Join P\).
Then \(R(S \Join P)\) realizes it as a functor in parameter \(y\).

<!-- Outside of the realization, we can think of \(y\) as a “formal” parameter: a placeholder for whatever \(y\) is chosen later. -->

Polynomial endofunctors are also called **containers**.
(This is where the “shapes” and “positions” terminology I prefer comes from.)

```agda
-- A container has a set of shapes and,
-- for each shape, a set of positions.
record Container where
  constructor _⋈_
  field
    Shape : Type
    Position : Shape → Type

-- Realize a container as a functor: for a given parameter
-- type `X`, the container is a dependent sum that
-- chooses a shape `s : S` and has `P s`-many positions
-- to fill with `X`s.
realize : Container → (Type → Type)
realize (S ⋈ P) = λ X → Σ[ s ∈ S ] (P s → X)
```

:::{.Example box-name="Exercise"}
It is worth writing out the mapping function to see that it is a functor.

```agda
realize :
  (C : Container) →
  (X Y : Type) →
  (f : X → Y) →
  realize C X → realize C Y
```
:::

:::Note
The bowtie \(\Join\) / `⋈`{.agda} is not standard notation; I am just using it for this article.
:::

A surprising amount of functors are polynomial functors.
One important example that is *not* included is the continuation monad:
it is not strictly positive, instead nesting the functor parameter in two layers of contravariance: `(y -> r) -> r`{.haskell}.
Another example that is not included is [bags](https://mathoverflow.net/questions/302631/what-is-the-polynomial-functor-for-the-bag-monad) (finite sets with multiplicity).

However, most of your favorite data types are probably included, and even functors you have not thought of before.

### Coproduct of Representables

The catchphrase of polynomial functors is that they are “coproducts of representables”.

Polynomial functors also seem to be all about quantifier alternation, sums of products of sums of products of …, though that is less widely recognized.^[Say \(\prod \sum \prod \sum \prod \sum\) and see how many catgirls show up, though.]
This is part of why they are exceptional models for interaction/communication.


#### Representables

A **representable functor** \(F\) comes with a **representing object** \(i\) that makes it naturally isomorphic to \(i \to y\): a natural isomorphism of functors \(F \cong (i \to {-}) = {-}^i\), implying the isomorphism \(F(y) \cong (i \to y) = y^i\) for each \(y\).

Representable functors have a lot of nice properties because morphisms are convenient to work with and are preserved by every construction.
From the perspective of containers, representable functors are **fixed-shape containers**.

For a finite type \(N\) isomorphic to \(\mathrm{Fin}(n)\), the representable functor \((N \to {-}) \cong (\mathrm{Fin}(n) \to {-})\) is isomorphic to a **vector** with \(n\) elements (foci).

- For \(\mathbf{0} \cong \mathrm{Fin}(0)\), you get an empty container: one shape, no elements.
- For \(\mathbf{1} \cong \mathrm{Fin}(1)\), you get the identity container: always one focus.
- For \(\mathbf{2} \cong \mathrm{Fin}(2)\), you get the functor of pairs: tuples where both elements have the same type.

  That is, \((\mathbf{2} \to X) \cong X \x X\).
- For \(\mathbf{3} \cong \mathrm{Fin}(3)\), you get a functor of triples, \(X \x X \x X\), like you might use to represent 3D vectors.


:::Bonus
There is an optic (in the sense of lenses/prisms/traversals) that captures being a fixed-shape container: [`Grate`{.haskell}](https://r6research.livejournal.com/28050.html).
:::

```agda
data Void : Type where

data Unit : Type where
  unit : Unit

data Fin : ℕ → Type where
  zero : Fin (succ n)
  succ : Fin n → Fin (succ n)

Representable : Type → Container
Representable T = Unit ⋈ λ _ → T

Finite : ℕ → Container
Finite n = Representable (Fin n)
```

#### Coproducts

From this base, we can take coproducts of these representables.

For example, `Maybe`{.haskell}, the container that has zero or one foci, could be represented with \(\mathbf{2}\), like so:

```agda
data Bool : Type where
  false : Bool
  true : Bool

Maybe : Container
Maybe = Bool ⋈ bool Void Unit
```

```haskell
data Maybe t = Nothing | Just t
```

This is a coproduct in the most classic sense: a binary operation.

We can also take a nullary coproduct, for the container with no shapes: the constant functor to the empty type.

```agda
Never : Container
Never = Void ⋈ absurd
```

```haskell
data Never t
```

:::Warning
Do not confuse `Never`{.agda}, the container that can never be constructed, with `Finite 0`{.agda}, the container that can always be constructed, but has no elements inside.
They are “empty” in different ways.
:::

### Examples

#### Lists

:::Key_Idea
For `List`{.haskell} (one of the classical examples of containers/polynomial functors!), we need an infinite coproduct.
The shapes are the length of list, and then we need that many foci.

```agda
List : Container
List = ℕ ⋈ Fin
```

That is,
\[\mathrm{List}(y) = \sum_{n \as \N} y^{\mathrm{Fin}(n)}\]

A length of list paired with a vector of the corresponding length (represented as a function).
:::

What is remarkable is that this is much closer to a binary view of how languages like Rust work (a length and a flat vector).

As opposed to constructing lists as biased linked lists: right-associative cons lists or left-associative snoc lists.

```haskell
data List a = Nil | Cons a (List a)
data Tsil a = Lin | Snoc (Tsil a) a
```

:::Warning
In this example, the family of positions \(\mathrm{Fin}\) is injective, meaning that each length of list comes from a specific shape.

Donʼt overgeneralize and think that each shape of a container must have a distinct number of elements: that is usually not the case.

For example, if you take the coproduct \(\mathrm{List} + \mathrm{Maybe}\), there are two distinct shapes for lengths \(0\) and \(1\): the shape coming from \(\mathrm{List}\) and the shape coming from \(\mathrm{Maybe}\).^[If you consider it merely up to isomorphism, as cardinalities instead of types, these get lost in the soup together – you would only know that they have multiplicity two; but from the perspective of concrete data, it is pretty clear where they are coming from, \(\mathrm{List}\) on the left or \(\mathrm{Maybe}\) on the right.]
:::

But what I like most is that we can make a functor for rectangular tables.
The shape chooses how many rows and columns there will be, and the amount of positions will be their multiplication.
This ensures that there is a position for every row and column: no more dealing with a potentially-ragged number of columns across rows.

```agda
Table : Container
Table = (ℕ × ℕ) ⋈ λ (rows , cols) → Fin rows × Fin cols
```

We will see later that this is the Day convolution of `List`{.agda} with itself: `Table = Day List List`{.agda}.
(This is generally how you would have to define it, without dependent types and the perspective of containers.)


#### Other Examples

Tack on data to an existing container.
This stretches the polynomial view of it.
But in the data view, it is pretty straightforward:
just tack on the data to the shape, and ignore it for the positions.

```agda
EnvT : Type → Container → Container
EnvT E (S ⋈ P) =
  (E × S) ⋈ λ (_e , s) → P s
```

```haskell
data EnvT e w t = EnvT e (w t)
```



### Tensors

There are a bunch of useful tensors between polynomial functors, which helps put the Day convolution into context.

#### Coproduct

```haskell
data Coproduct f g y = InL (f y) | InR (f y)
```

The shape of a coproduct is either a shape from the left summand, or a shape from the right summand.
The set of positions is taken from the side that was chosen.

\[(S_0 \Join P_0) + (S_1 \Join P_1) = (S_0 + S_1) \Join (P_0 \sqcup P_1)\]

Where \(P_0 \sqcup P_1\) is the function that joins \(P_0\) and \(P_1\), defined by
\[
\begin{alignat*}{2}
  &(P_0 \sqcup P_1)(inl&(s_0)) &= P_0(s_0), \text{\ and}\\
  &(P_0 \sqcup P_1)(inr&(s_1)) &= P_1(s_1).
\end{alignat*}
\]

```agda
Coproduct (S0 ⋈ P0) (S1 ⋈ P1) =
  (Either S0 S1) ⋈ (either P0 P1)
```

#### Product

```haskell
data Product f g y = Product (f y) (g y)
```

The shape of a product is a shape for the left factor and a shape for the right factor.
The positions is the sum of positions for each factor: think of it as concatenating the vectors, so their lengths add together.

\[(S_0 \Join P_0) \x (S_1 \Join P_1) = (S_0 \x S_1) \Join \lambda (s_0, s_1).\ (P_0(s_0) + P_1(s_1))\]

Maybe you notate the positions like \(P_0 \mathop{{\small\lceil}{+}{\small\rceil}} P_1 : S_0 \x S_1 \to \mathrm{Type}\).

```agda
Product (S0 ⋈ P0) (S1 ⋈ P1) =
  (Tuple S0 S1) ⋈ λ (s0 , s1) → Either (P0 s0) (P1 s1)
```

#### Day Convolution

```haskell
-- Mysteriously missing from the mission log ...
```

The shape of a Day convolution is also product of shapes for the factors.
This time it uses the *product* of positions, not their sum.

\[(S_0 \Join P_0) \ox (S_1 \Join P_1) = (S_0 \x S_1) \Join \lambda (s_0, s_1).\ (P_0(s_0) \x P_1(s_1))\]

Maybe you notate the positions like \(P_0 \mathop{{\small\lceil}{\x}{\small\rceil}} P_1 : S_0 \x S_1 \to \mathrm{Type}\).

```agda
Day (S0 ⋈ P0) (S1 ⋈ P1) =
  (Tuple S0 S1) ⋈ λ (s0 , s1) → Tuple (P0 s0) (P1 s1)
```

:::Bonus
The Day convolution is defined relative to a tensor.
This Day convolution – the default notion – is relative to the product of sets.
The product of functors is actually a Day convolution relative to the coproduct of sets.
:::

#### Functor Composition

```haskell
data Compose f g y = Compose (f (g y))
```

Somewhat ironically, functor composition is trickier to describe with polynomial functors than the Day convolution is.
The Haskell datatype is super simple: functor composition and much more is built in to the type system.

Recall that the realization of a polynomial (shapes and positions) as a functor is
\[R(C \coloneqq S \Join P)(y) = \sum_{s \as S} \prod_{p \as P(s)} y.\]

So we want something that just looks like the nesting
\[R(C_0 \lhd C_1)(y) = \sum_{s_0 \as S_0} \prod_{p_0 \as P_0(s_0)} \sum_{s_1 \as S_1} \prod_{p_1 \as P_1(s_1)} y\]
with \(C_0 \lhd C_1\) being notation for the composition of containers.

It turns out that there are quantifiers \(\sum^\Join\) and \(\prod^\Join\) that accomplish this.
They will be covered in the next section, allowing us to write
\[(S_0 \Join P_0) \lhd C_1 = \sum^{\normalsize\Join}_{s_0 \as S_0} \prod^{\normalsize\Join}_{p_0 \as P_0(s_0)} C_1.\]
This really is the simplest way to describe composition of polynomial functors.

But the elementary description as a polynomial – what those quantifiers will desugar into – goes like

#.  Decide on a shape \(s_0 : S_0\) for the outer container.
#.  For each position \(p_0 : P_0(s_0)\) in the outer container, also decide on a shape \(S_1\) for the inner container.
    This is a mapping \(f : P_0(s_0) \to S_1\).
#.  A position in the composition starts at a position in the outer container \(p_0 : P_0(s_0)\),
#.  And a position in the inner container \(p_1 : P_1(f(p_0))\).^[Cuz \(p_0\) is the only term of type \(P_0(s_0)\) to apply to \(f\) to get a term of type \(S_1\) for \(P_1\).]

As a formula,

\[(S_0 \Join P_0) \lhd (S_1 \Join P_1) = \left(\left(\sum_{s_0 \as S_0} (P_0(s_0) \to S_1)\right) \mathrel{\large\Join} \lambda (s_0, f) \mapsto \sum_{p_0 \as P_0(s_0)} P_1(f(p_0))\right).\]

Admittedly a terrifying formula, but I already broke it down so you got it!

Mostly it is just annoying how details of the quantification (\(s_0 : S_0\) and \(p_0 : P_0(s_0)\)) need to be repeated on both the shapes and position side; this is what the dedicated quantifiers clean up.

```agda
Compose (S0 ⋈ P0) C1 =
  Coproducts S0 λ s0 →
    Products (P0 s0) λ p0 →
      C1
```

### Quantifiers

```haskell
-- Error: need dependent types
```

As mentioned, we can get some quantifiers to create a container from a family of containers.
These are dependent or infinitary versions of the existing tensors.

---

Infinitary/dependent coproduct.

The goal is that computing the realization results in an extra sum.
\[R(\sum^{\normalsize\Join}_{j \as J} (S(j) \Join P(j)))(y) \cong \sum_{j \as J} \sum_{s \as S(j)} \prod_{p \as P(j)(s)} y.\]

This just uses associativity to condense the two sums into one, deriving this container.

```agda
Coproducts : (J : Type) → (J → Container) → Container
Coproducts J C =
  (Σ[ j ∈ J ] (C j).Shape) ⋈ λ (j , s) → (C j).Position s
```

:::Details
Taking it back to the coproduct of representables idea:
If all of the `C`{.agda} are representable, that means the `(C j).Shape`{.agda} is trivial, so the dependent sum `(Σ[ j ∈ J ] (C j).Shape)`{.agda} can just become `J`{.agda}, and the position is `λ j → (C j).Position unit`{.agda}: `C`{.agda} boils down to a function `J → Type`{.agda}.
That is, `⋈`{.agda} is the constructor for the coproduct-of-representables representation, like I promised.
:::

\[\sum^{\normalsize\Join}_{j \as J} (S(j) \Join P(j)) = \left(\sum_{j \as J} S(j)\right) \mathrel{\large\Join} \lambda (j, s_0) \mapsto P(j)(s_0)\]

---

Infinitary/dependent product.

Again, it should show up as extra quantification in the realization.
\[R(\prod^{\normalsize\Join}_{j \as J} (S(j) \Join P(j)))(y) \cong \prod_{j \as J} \sum_{s \as S(j)} \prod_{p \as P(j)(s)} y.\]

```agda
Products : (J : Type) → (J → Container) → Container
Products J C =
  ((j : J) → (C j).Shape) ⋈ λ f → Σ[ j ∈ J ] (C j).Position (f j)
```

\[\prod^{\normalsize\Join}_{j \as J} (S(j) \Join P(j)) = \left(\prod_{j \as J} S(j)\right) \mathrel{\large\Join} \lambda f \mapsto \sum_{j \as J} P(j)(f(j)).\]

---

Exponentiation by a fixed type, as the infinitary product on a constant family.

```agda
Exp : Type → Container → Container
Exp J C = Products J λ _ → C
```

\[(S \Join P)^J = (J \to (S \Join P)) = (J \to S) \mathrel{\large\Join} \lambda f \mapsto \sum_{j \as J} P(f(j)).\]

---

Infinitary Day convolution.

```agda
Days : (J : Type) → (J → Container) → Container
Days J C =
  ((j : J) → (C j).Shape) ⋈ λ f → (j : J) → (C j).Position (f j)
```

\[\bigotimes_{j \as J} (S(j) \Join P(j)) = \left(\prod_{j \as J} S(j)\right) \mathrel{\large\Join} \lambda f \mapsto \prod_{j \as J} P(j)(f(j)).\]

---

Dependent composition??
I have not seen this one talked about.
It is a little strange but it seems to fall out pretty naturally from the definitions and patterns.


\[
\mathop{\Huge\lhd}\limits_{(s_0,\, p_0) \as S_0 \Join P_0} (S_1(s_0, p_0) \Join P_1(s_0, p_0)) = \sum^{\normalsize\Join}_{s_0 \as S_0} \left(
  \prod^{\normalsize\Join}_{p_0 \as P_0(s_0)} (S_1(s_0, p_0) \Join P_1(s_0, p_0))
\right)
\]

\[
\mathop{\Huge\lhd}\limits_{(s_0,\, p_0) \as S_0 \Join P_0} C_1(s_0, p_0) =
  \sum^{\normalsize\Join}_{s_0 \as S_0} \prod^{\normalsize\Join}_{p_0 \as P_0(s_0)} C_1(s_0, p_0)
\]


### Morphisms and Homs

I havenʼt said anything about the morphisms between polynomial functors yet, which basically means I have failed as a category theorist haha.^[One attitude sometimes talked about in category theory is that the morphisms matter most, the objects hardly at all.]

The morphisms between polynomial functors should coïncide with the usual notion of morphism for functors: natural transformations.

:::{.Details box-name="Quick Recap"}
A natural transformation \(\varphi\) from \(F : \mathcal C \to \mathcal D\) to \(G : \mathcal C \to \mathcal D\) has components \(\varphi_O : F(O) \to G(O)\), for an object \(O : \mathcal C\).
The naturality condition says that the choice of object \(O : \mathcal C\) does not matter: for a morphism \(m : x \to y\) in \(\mathcal C\), \(\varphi_y \circ F(m) = G(m) \circ \varphi_x\), meaning that the natural transformation commutes with mapping the morphism (over \(F\) and \(G\) respectively).^[The naturality condition comes for free in Haskell, as a result of relational parametricity, which is a strong version of naturality.]

For more, Iʼve started a post about [(Extra)natural transformations](extranatural-transformations.html).
:::

<!--
:::{.Bonus box-name="Aside"}
I kind of like thinking about a natural transformation as being indexed by a morphism instead of an object:

For a morphism \(m : X \to Y\), the natural transformation at that morphism \(\varphi_m : F(X) \to G(Y)\) performs the action of \(\varphi\) as well as the action of \(m\), together in one go.
Then the naturality condition says that any mapping of \(F\) (prior) or \(G\) (afterwards) can be subsumed into \(m\):
\[G(g) \circ \varphi_m \circ F(f) = \varphi_{g \circ m \circ f}\]
where it makes sense (for \(g : Y \to Z\) and \(f : W \to X\)).

To me this captures the idea better than the separate mapping \(\varphi_y \circ F(m)\) or \(G(m) \circ \varphi_x\), and it makes more sense at a formal level than mapping objects to morphisms.

Just like a functor can lift a whole diagram at once, not just a single morphism, this presentation of a natural transformation shows that it can also apply to diagrams (with objects in the image of \(F\)).
:::
-->

At first glance this looks like a lot of information packed into the definition of naturality.
One morphism for each object in the (source) category??
However, the situation is much more practical (in part due to the naturality condition and the Yoneda lemma, among other things).

Just like the data of a polynomial functor was given by shapes and positions, the data underlying this natural transformation is given by a function for shapes and a function for positions.

```agda
record ContainerMorphism ((SI ⋈ PI) : Container) ((SO ⋈ PO) : Container) where
  constructor _-⋈>_
  field
    -- The shape of the output is determined
    -- by the shape of the input
    OnShapes : SI → SO
    -- To fill each slot in the output, the function says
    -- what position of the input to copy the data from
    OnPositions : (s : SI) → PO (OnShapes s) → PI s

realizeMorphism :
  (CI CO : Container) →
  ContainerMorphism CI CO →
  ∀ (X : Type) → realize CI X → realize CO X
realizeMorphism (SI ⋈ PI) (SO ⋈ PO) (fs -⋈> fp) X (s0 , p0) =
  (fs s0 , fp s0 p0)
```

The function for shapes operates forward: the shape of the output is determined by the shape of the input, as you would expect.
The function for positions maps backwards: it looks at each output position (for the given shape) and says what position _of the input_ to copy it _from_.

That is, this `ContainerMorphism`{.agda} structure does not talk about the elements of the functor parameter \(X\) at all: it just talks about a correspondence of shapes (forwards) and positions (backwards).
That gives us the naturality condition by construction!

There is a way to write it more elegantly as a single function, returning a dependent sum, instead of two functions dependent on each other:

\[\prod_{s_0 \as S_0} \sum_{s_1 \as S_1} \left(P_1(s_1) \to P_0(s_0)\right)\]

Or, with our best non-dependent dependent function impression,

\[\prod_{s_0 \as S_0} \sum_{s_1 \as S_1} \prod_{p_1 \as P_1(s_1)} P_0(s_0)\]

Again, we see quantifier alternation show up!

Notice how \(\sum_{s_1} \prod_{p_1}\) is straight out of the formula for the realization of the container \(S_1 \Join P_1\) as a functor.
This will come in handy later, writing it like

\[\prod_{s_0 \as S_0} R(S_1 \Join P_1)(P_0(s_0)).\]

:::{.Example box-name="Exercise"}
Write out the map \(F \ox G \to F \lhd G\) that I have referenced so many times.
(This is the map that relates monads and applicatives.)

As a reminder of the definitions of these containers,

\[
\begin{align*}
  (S_0 \Join P_0) \ox (S_1 \Join P_1) &= (S_0 \x S_1) &&\mathrel{\large\Join} \lambda (s_0, s_1).\ (P_0(s_0) \x P_1(s_1)) \\
  (S_0 \Join P_0) \lhd (S_1 \Join P_1) &= \left(\sum_{s_0 \as S_0} (P_0(s_0) \to S_1)\right) &&\mathrel{\large\Join} \lambda (s_0, f) \mapsto \sum_{p_0 \as P_0(s_0)} P_1(f(p_0)).
\end{align*}
\]

Maybe you can visually see how this works already.

:::

---

This is the notion of natural transformation, or morphism in the category of functors.
Category theorists call this the “Hom-set” of the category (derived from Homomorphism).

What about morphisms inside the category itself?
These will be “internal homs” or “Hom-objects” or maybe just Homs.
These are interesting, and closely related to natural transformations, but they will be _polynomial functors_, not sets.

Of course you can plop the natural transformation in a constant functor, but I am not sure that has any nice properties.
Maybe?

We will talk about the Hom that is right adjoint to the product functor shortly.
That is the one that makes it into a closed category.^[A closed category, unqualified, means a category that is monoidal closed with respect to the categorical product, as this is the typical way to be closed and generally most impactful.]

But the one that is right adjoint to Day convolution is actually a little more straightforward and more interesting.

#### Day Hom

```haskell
data DayHom f g y = DayHom (forall x. f x -> g (x, y))
data DayHom f g y = DayHom (forall i o. (y -> i -> o) -> f i -> g o)
```

This Hom-object comes with the property \(((F \ox G) \to H) \cong (F \to (G \overset{\ox}{\rArr} H))\).

The main component of it is a natural transformation `forall x. f x -> g x`{.haskell}.
But entangled with it – just like the Day convolution does – every position of the resulting `g`{.haskell} is tagged with the functor parameter `y`{.haskell} too.
That is the part that turns it into an interesting functor, a hom-object inside \(\mathrm{Poly}\), not just a hom-set.

This is possible to represent this as a polynomial functor \(C_0 \overset{\ox}{\rArr} C_1\), but it requires some thought.

First, the set of shapes.
It turns out that this is the hom-set, actually.
A shape is a natural transformation \(f\) from \(C_0\) to \(C_1\), which is pretty remarkable!

The set of positions for a shape \(f\) is going to allow identifying all of those places that a \(y\) could live.
We need a position in \(C_1\), and the shape of that is determined by the natural transformation based on the shape of the input \(C_0\).
Let \(f^S\) represent that mapping of shapes \(S_0 \to S_1\).
Then we will be satisfied with \(\sum_{s_0 \as S_0} P_1(f^S(s_0))\).
That is, the caller gets to choose what shape of \(C_0\) it is inputting and what position in the resulting \(C_1\) it wants to query, then it will get the \(y\) at that position back.

\[(C_0 \coloneqq S_1 \Join P_1) \overset{\ox}{\rArr} (C_1 \coloneqq S_1 \Join P_1) = (C_0 \to C_1) \Join \lambda f \mapsto \sum_{s_0 \as S_0} P_1(f^S(s_0))\]

```agda
DayHom C0@(S0 ⋈ P0) C1@(S1 ⋈ P1) =
  ContainerMorphism C0 C1
    ⋈ λ (fs -⋈> fp) →
      Σ[ s0 ∈ S0 ] P1(fs(s0))
```


#### Product Hom

```haskell
data ProdHom f g y = ProdHom (forall x. f x -> g (Either x y))
data ProdHom f g y = ProdHom (forall x. (y -> x) -> f x -> g x)
```

This Hom-object comes with the property \(((F \x G) \to H) \cong (F \to (G \overset{\x}{\rArr} H))\).


<!--
forall x. h x -> ProdHom f g x
forall x. h x -> forall y. (x -> y) -> f y -> g y
forall y. forall x. (x -> y) -> h x -> f y -> g y
forall y. (exists x. (x -> y, h x)) -> f y -> g y
forall y. (Coyoneda h y) -> f y -> g y
forall y. h y -> f y -> g y
forall y. Product h f y -> g y
-->

\[
  \prod^{\normalsize\Join}_{s_0 \as S_0}
  \sum^{\normalsize\Join}_{s_1 \as S_1}
  \prod^{\normalsize\Join}_{p_1 \as P_1(s_1)}
  \left(
  \left(P_0(s_0) + 1\right) \mathrel{\large\Join}
    (0 \sqcup 1)
  \right)
\]

Notice how the \(s_0, s_1, p_1\) quantifier alternation is just like the formula for a morphism, which we already noticed was like applying the functor.

With a bit of cute shortening,
\[\prod^{\normalsize\Join}_{s_0 \as S_0} C_1 \lhd (P_0(s_0) + \mathrm{Id}),\]
where \(\mathrm{Id}\) is the identity functor and \(P_0(s_0)\) is that set interpreted as a constant functor.


```agda
ProdHom : Container → Container → Container
ProdHom (S0 ⋈ P0) C1 =
  Products S0 λ s0 →
    Compose C1
      (Coproduct
        (P0 s0 ⋈ λ _ → Void)
        (Unit ⋈ λ _ → Unit)
      )
```

For more, see https://topos.institute/blog/2023-09-21-powers-of-polynomial-monads/


## Without Polynomials

We can build up its description in terms of a [GADT]{t=} from its description as a polynomial endofunctor.

Recall that description: the polynomial \(F \ox G\) has shapes from the product of shapes of \(F\) and of \(G\), and positions that are the product of the respective positions.

So, to handle the product of shapes part, we know that we want something `f _`{.haskell} shaped and something `g _`{.haskell} shaped.
Letʼs sketch that:

```haskell
data Day f g r where
  Day :: f _ -> g _ -> ... -> Day f g r
```

Now we want to say that a position in `Day f g r`{.haskell} – which corresponds to an element of type `r`{.haskell}! – comes from a position in `f _`{.haskell} and a position in `g _`{.haskell}.

So if, hypothetically, `f`{.haskell} and `g`{.haskell} had positions represented by `u` and `v` respectively, a function `(u -> v -> r)`{.haskell} could mean constructing a position in `Day f g r`{.haskell} by picking one from `f`{.haskell} and one from `g`{.haskell}.

It turns out, if we write just that, we get the correct definition of Day convolution in Haskell:

```haskell
data Day f g r where
  Day :: f u -> g v -> (u -> v -> r) -> Day f g r

-- The constructor
mkDay :: forall f g r u v. f u -> g v -> (u -> v -> r) -> Day f g r
mkDay = Day

-- Deconstructor (eliminate the existential)
unDay :: forall f g r out. (forall u v. f u -> g v -> (u -> v -> r) -> out) -> Day f g r -> out
unDay mk (Day fu gv comb) = mk fu gv comb
```

Important: `u`{.haskell} and `v`{.haskell} are existentially quantified here, they are not visible from outside.
This is why the continuation passed to the deconstructor needs to work for any choice of `u`{.haskell} and `v`{.haskell}.

This means that `f u`{.haskell} is there strictly in the role of providing shapes and providing positions represented by `u`{.haskell}, and the same for `g v`{.haskell} with positions represented by `v`{.haskell}.

And `(u -> v -> r)`{.haskell} is a function from both of those existential position types to the known result type `r`{.haskell}.

:::Key_Idea
They only thing you can do with this function `(u -> v -> r)`{.haskell} is to apply it to a pair of positions, one from `f`{.haskell} and one from `g`{.haskell}.
Thus it represents the pairing of positions that we saw in the polynomial functor explanation.
:::




For completeness, besides this symmetric definition, you can have two biased definitions that apply the function to either side:

```haskell
data FnDay f g r where
  FnDay :: f (i -> r) -> g i -> FnDay f g r
data DayFn f g r where
  DayFn :: f i -> g (i -> r) -> DayFn f g r
```

By the Yoneda lemma, all three definitions are equivalent when `f`{.haskell} and `g`{.haskell} are lawful `Functor`{.haskell}s, as is assumed.
You get between them by using partial application and `fmap`{.haskell}/`<$>`{.haskell}.
Again, you can think of it as only having one choice for how to apply these functions and combine them.

:::{.Example box-name="Exercise"}
Can you write down the natural transformation `forall f g r. Functor f => Functor g => Day f g r -> Compose f g r`{.haskell}?

Maybe write down the isomorphisms between the different definitions of Day convolution here.
:::



### Category Theory

Roll in the abstract nonsense!

First, as something that could be a universal property.

For every morphism \(u \x v \to r\) we want to be able to construct \((F \ox_{Day} G)(r)\) from \(F(u) \x G(v)\).

We can generalize this to tensors other than \(\x\).

Let \(\mathcal V\) be a symmetric monoidal closed category, with a tensor \({\ox_\mathcal{V}} : \mathcal V \x \mathcal V \to \mathcal V\).
Another condition is that \(\mathcal V\) should also be ... cocomplete? And complete? Not exactly sure.
Note: since \(\mathcal V\) is closed, it can be considered as a \(\mathcal V\)-enriched (self-enriched) category.


Let \(F, G : \mathcal C \to \mathcal V\) be \(\mathcal V\)-enriched functors ((co)presheaves) from a \(\mathcal V\)-enriched monoidal category \(\mathcal C\) to the self-enriched \(\mathcal V\).
Then \({\ox_{Day}}\) is also a \(\mathcal V\)-enriched functor from \(\mathcal C\) to \(\mathcal V\).

That is, \(\mathcal V\), the base of enrichment, is a monoidal category with tensor \({\ox_\mathcal{V}}\).
The category \(\mathcal C\) also has its own tensor \({\ox_\mathcal{C}}\): the Day convolution \({\ox_{Day}}\) is relative to this choice of \({\ox_\mathcal{C}}\).


Then the universal property would say something about:
For every morphism \(u \ox_\mathcal{C} v \to r\), construct \((F \ox_{Day} G)(r)\) from \(F(u) \ox_\mathcal{V} G(v)\).

Note that the morphism from \(\mathcal C\) is an object in \(\mathcal V\), so we naturally tensor it with the \(F(u)\) and \(G(v)\): \(\mathcal{C}(u \ox_\mathcal{C} v, r) \ox_\mathcal{V} F(u) \ox_\mathcal{V} G(v)\).
Finally, a co-end (written with an integral sign) is the way to quantify over \(u : \mathcal C\) and \(v : \mathcal C\) within \(\mathcal V\).^[In fact, within the category of functors \(\mathcal C \to \mathcal V\).]

The total comes out to:

\[
  F \ox_{Day} G = \overset{u, v}{\int} \mathcal{C}(u \otimes_\mathcal{C} v, {-}) \otimes_\mathcal{V} F(u) \otimes_\mathcal{V} G(v)
\]

Slightly less formally, introduce the parameter \(r : \mathcal C\):^[this gives the object mapping of the functor, technically it does not quite say how to map morphisms: you have to trace how \(r\) shows up in the equation to track the morphisms, but this is straightfoward.]

\[
  (F \ox_{Day} G)(r) = \overset{u, v}{\int} \mathcal{C}(u \otimes_\mathcal{C} v, r) \otimes_\mathcal{V} F(u) \otimes_\mathcal{V} G(v)
\]



Picking this apart:

- The functor \(F \ox_{Day} G : \mathcal C \to \mathcal D\) is defined on an parameter \(r \in \mathcal C\).
- It is a co-end over (under?) \(u : \mathcal C\) and \(v : \mathcal C\), which corresponds to existential quantification in type theory.
- Inside the co-end is an object in \(\mathcal V\),^[really, a functor \(\mathcal C \to \mathcal V\) when abstracting over \(r\)] a tensor of three components:

  #.  The morphism \(\mathcal{C}(u \otimes_\mathcal{C} v, r)\) from the existential objects, tensored with \(\ox_\mathcal{C}\), to the parameter \(r\);
  #.  The functor \(F(u)\), holding the existential object as its parameter; and
  #.  The functor \(G(v)\), holding the other existential object.



## Properties

This is not a categorical product: you cannot project out `f r`{.haskell} or `g r`{.haskell} in general.

You can project out their shape, though: `Day f g r -> f ()`{.haskell} and `Day f g r -> g ()`{.haskell}.

The identity for this tensor is, of course, the `Identity`{.haskell} functor: `data Identity t = Identity t`{.haskell}, which has one shape and one position in that shape.
So there are natural isomorphisms `Day f Identity <~> f <~> Day Identity f`{.haskell}.

Applicatives can be defined with `applyDay :: Day f f ~> f`{.haskell} [i.e.]{t=} `forall r. Day f f r -> f r`{.haskell}, so for any other definition of applicative ([e.g.]{t=} in terms of `(<*>) :: f (i -> o) -> f i -> f o`{.haskell}), you can also define `applyDay`{.haskell}.

I already mentioned that the Day convolution is symmetric.
The cute thing is that you define this with `flip`{.haskell} on the function `(u -> v -> r)`{.haskell}.

```haskell
symmDay :: Day f g ~> Day g f
symmDay (Day f g comb) = Day g f (flip comb)
```

Since we are dealing with endofunctors, we can map the Day convolution to a directed composition: `day2Compose :: Day f g ~> Compose f g`{.haskell}.


## Applications

### Applicative Functors

An applicative functor comes with a natural transformation \(F \ox F \to F\) that is associative and comes with an identity \(\mathrm{Id} \to F\).
Another way of thinking about it is as an arbitrary-arity operation \(F \ox F \ox \cdots \ox F \to F\).

Just like \(\mathrm{List}\) is the free monoid (and monoids come with an arbitrary-arity operation \(M \x M \x \cdots \x M \to M\) [a.k.a.]{t=} \(\mathrm{List}(M) \to M\)), we can construct a free applicative functor.

The free applicative functor could be written as a polynomial functor like this:
\[\sum^{\normalsize\Join}_{n \as \N} \bigotimes_{i \as \mathrm{Fin}(n)} F\]
which becomes, for \(F = S \Join P\),
\[
\left(\sum_{n \as \N} \prod_{i \as \mathrm{Fin}(n)} S\right)
  \mathrel{\large\Join} \lambda (n, f) \mapsto
\left(\prod_{i \as \mathrm{Fin}(n)} P(f(i))\right)
\]

Outside of polynomial functors, as I said in [my dedicated post about applicatives](applicatives.html), I like what Phil Freeman does to [construct `FreeApplicative`{.haskell} and `FreeApply`{.haskell} together](https://blog.functorial.com/posts/2017-07-01-FreeAp-Is-A-Comonad.html):
```haskell
newtype FreeApplicative f a = FreeApplicative (Coproduct Identity (FreeApply f) a)

newtype FreeApply f a = FreeApply (Day f (FreeApplicative f) a)
```

(In PureScript, `Apply f`{.purescript} is a superclass of `Applicative f`{.purescript}: `Apply f`{.purescript} contains `<*>`{.purescript} while `Applicative f`{.purescript} contains `pure`{.purescript}.)

Applicatives have static analysis and parallelism.
But that is not a topic to go into here.

### Comonads

Phil was actually interested in `FreeApply`{.haskell} as a [comonad transformer](https://blog.functorial.com/posts/2017-07-01-FreeAp-Is-A-Comonad.html), which is pretty unique to think about, related to his notion of [Comonads as Spaces](https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html), for building purely functional UI paradigms.
See those links to his blog for more.

Extensible Day convolution via row types:

https://pursuit.purescript.org/packages/purescript-smash/3.0.0

Just like the builtin `Record :: Row Type -> Type`{.purescript} uses rows to represent labeled types joined using product (`Tuple`{.purescript}), and the library-implemented [`Variant :: Row Type -> Type`{.purescript}](https://pursuit.purescript.org/packages/purescript-variant/8.0.0) does it for its dual, the coproduct (`Either`{.purescript}), `Smash :: Row (Type -> Type) -> (Type -> Type)`{.purescript} does it for Day convolution.

:::Warning
This package predates polykinds in PureScript.

The kind of `Smash`{.purescript} would be written in modern terms as

```purescript
data Smash (r :: Row (Type -> Type)) (a :: Type)
```

That is, it takes a row of functors and makes a functor.

But because it was written before useful machinery was polykinded, it was written as `r :: Row Type`{.purescript} using `Proxy f`{purescript} to represent each functor `f`{.purescript} as something of kind `Type`{.purescript}.
:::

Seriously, row types are really cool and more languages should implement them.


### Applicative Phases

Worth reading!!

https://www.cs.ox.ac.uk/jeremy.gibbons/publications/phases.pdf

Basically you can combine different applicatives with the Day convolution and choose to handle them in any order.

You can do something similar to `FreeApplicative`{.haskell}: you can take a ziplist of applicatives, sorted by a kind of depth.
This is \(\N\)-indexed phases.

That allows for breadth-first traversals, for one.
Each traversal can defer its children by one level, then they get stitched together in the right order in the end.

You could also have the components for the Day convolution tracked in a row type, where they get string labels instead of natural numbers.
This is something Phil did; see the previous section.
Just using it for applicative functors instead of comonads.



### Tables / Matrices

As I spoiled in the introduction, \(\mathrm{Table} = \mathrm{List} \ox \mathrm{List}\).

Here is one way of thinking about this:

- The applicative instance on lists constructs a Cartesian product.
- We know this Cartesian product has `rows * columns` elements, but it is all flattened together.
- The applicative instance on lists comes from its monad instance, so it morally uses the map \(\mathrm{List} \ox \mathrm{List} \to \mathrm{List} \lhd \mathrm{List}\) before flattening it with \(\mathrm{List} \lhd \mathrm{List} \to \mathrm{List}\).
- So you could *not* flatten it, and keep it.
  This is what everyone does!
- But why not keep the actual input \(\mathrm{List} \ox \mathrm{List}\), in its Day convolution form?
  Now you have something that can always be converted into a Cartesian product, and is guaranteed not to have holes in its rectangular table.

Working with the curried definition
```haskell
data Day f g r where
  Day :: f u -> g v -> (u -> v -> r) -> Day f g r
```
yields a nice property: if you consider `f u`{.haskell} to be rows, you can partially apply `(u -> v -> r)`{.haskell} to each row, the use the resulting `(v -> r)`{.haskell} to potentially share work across the columns of that row.
(Okay, this is just the same as the biased definition `f (v -> r) -> g v -> Day f g r`{.haskell}.)

If you are worried about the function-ness of this definition and would prefer if the elements `r`{.haskell} in `Table r = Day List List r`{.haskell} were stored explicitly, you can memoize by index in the table (`Tuple Nat Nat`{.haskell}) to cache all of the results of `(u -> v -> r)`{.haskell} across the table.
Then the new `(u -> v -> r)`{.haskell}, which is chosen to be `(Nat -> Nat -> r)`{.haskell}, is just a lookup in the memoized table.

This seems like it will be useful for tables for UI reasons, too.
But with the caveat that those tables often want to merge adjacent cells sometimes, which is … not something this representation helps with.


### Polynomial functors

Polynomial functors can be used to model a lot of topics more abstractly, not just datatypes.
Their quantifier alternation makes them a model of interaction and communication: call \(\prod\) and response \(\sum\).

Day convolution is used for juxtaposition in diagrams, and the categorical product serves a much smaller story.

https://topos.institute/blog/2023-09-21-powers-of-polynomial-monads/



## Why is it a Convolution?

Maybe it is a little deceptive leaving this question for last.
But maybe you just skipped to the end of the article anyways.

We know what makes the Day convolution Day (it is named after Brain Day, who first described it), but what makes it a convolution?

One of the classic(ally confusing) convolutions – used in signal processing, among other things – is defined on real functions as the integral
\[(f * g)(y) = \int_{-\infty}^{\infty} f(x)g(y-x)dx.\]

The subtraction in there always throws me for a loop!
(The integral symbol resembles the symbol used for co-ends, and there are good reasons for that, but letʼs not get ahead of ourselves.)

If it were a finite sum, a more symmetric way to think about it would be by summing over all \(u\) and \(v\) that sum to \(y\)
\[\sum_{u + v = y} f(u) g(v).\]

This can be used with more monoids than addition.
Addition of real numbers has unique subtraction, so you can find the single correct \(v\) for every \(u\).
But not all monoids are groups like this!

In general, take two functions \(f, g : M \to N\) from a monoid \((M, \diamond)\) to a set \((N, +, \x)\) equipped with addition and multiplication.
A convolution will probably look like this:
\[(f * g)(y) = \sum_{u \diamond v = y} f(u) \x g(v)\]
— where you either hope that the sum is finite, or hope you have some way to deal with the infinite summation.

This conditional sum can be turned into a traditional sum with the Dirac delta function \(\delta(x, y)\), which is 0 when the arguments are disequal and 1 when they are equal.

\[(f * g)(y) = \sum_{u, v \in M} f(u) g(v) \delta(u \diamond v, y)\]

Finally, by adding more than a sprinkle of category theory, and generalizing the Dirac delta to a morphism in a category^[A set, regarded as a discrete category enriched in numbers, has the Dirac delta as its morphism family], we get the Day convolution as above:

\[(F \ox_{Day} G)(y) = \int^{u,v} F(u) \ox_\mathcal{V} G(v) \ox_\mathcal{V} \mathcal{C}(u \ox_\mathcal{C} v, y).\]

Tada!

This isnʼt a complete explanation by any means, but hopefully it provides you hints where to look further to explore this pattern.
