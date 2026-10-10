---
title: 3+ Views of Applicative Functors
subtitle: "A [Rondo](https://en.wikipedia.org/wiki/Rondo) on Monoids and Functors"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2023/11/08, 2026/10/09
---

:::{.Key_Idea box-name="Background"}
To get the most out of this post diving into the theory of [applicative functors](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Applicative.html#t:Applicative) and [monads](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Monad.html#t:Monad), you should be familiar with some category theory, monoids, isomorphisms, existential quantification, maybe the Yoneda lemma.
:::

Choose your fighter!

- `liftA2 ($) :: f (i -> o) -> f i -> f o`{.haskell} (the monoid under Day convolution)
- `liftA2 (,) :: f u -> f v -> f (u, v)`{.haskell} (the lax monoidal functor)
- `liftA2 (.) :: f (y -> z) -> f (x -> y) -> f (x -> z)`{.haskell} (the lifted category)

Applicative functors are very important.
The first two ways of constructing them are often talked about, [e.g.]{t=} see [“The monoidal presentation”](https://en.wikibooks.org/wiki/Haskell/Applicative_functors#The_monoidal_presentation) on Haskellʼs WikiBook.

But thereʼs a third way that I find really interesting!

Just like monads give rise to Kleisli arrows `x -> f y`{.haskell}, applicative functors give rise to a category by taking arrows of shape `f (x -> y)`{.haskell}:

```haskell
-- A category from an applicative functor
newtype Arrf f x y = Arrf (f (x -> y))

instance Applicative f => Category (Arrf f) where
  id = Arrf (pure id)
  Arrf f . Arrf g = Arrf (liftA2 (.) f g)

-- A category from a monad
newtype Kleisli f x y = Kleisli (x -> f y)

instance Monad f => Category (Kleisli f) where
  id = Kleisli return
  Kleisli f . Kleisli g = Kleisli (\x -> f =<< g x)
```

Their stories are similar: associativity of the monad or applicative is exactly what you need for associativity of composition of these arrows, and `pure`{.haskell}/`return`{.haskell} are used for the identity morphism.
<!-- This form of associativity is particularly nice and familiar; the associativity for monads in terms of `join :: f (f a) -> f a`{.haskell} is also not too complicated, but it is obscured by the time it is translated to `>>=`{.haskell}/`bind`{.haskell}, which . -->

Note the key difference here: in `Kleisli`{.haskell}, the `f`{.haskell} only appears inside, after the argument is provided; in `Arrf`{.haskell}, the `f`{.haskell} appears on the outside, so it governs over the functions.

:::{.Details box-name="cf."}
Iʼm not sure there is a standard name for this construction of applicative arrows.

In Haskell packages I have seen it twice (generalizing to an arbitrary profunctor `p`{.haskell} instead of functions `(->)`{.haskell}):

#. [`newtype Cayley f p x y = Cayley (f (p x y))`{.haskell}](https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor-Cayley.html)

#. [`newtype StaticArrow f p x y = StaticArrow (f (p x y))`{.haskell}](https://hackage.haskell.org/package/arrows-0.4.4.2/docs/Control-Arrow-Transformer-Static.html)

… but I am not sure where the name Cayley comes from.

It is also the construction given by [change of enriching category](https://ncatlab.org/nlab/show/change+of+enriching+category) (or “change of base”).
But this seems a bit obscure too; it does not have a catchy name, and is defined in terms of lax monoidal functors anyways.
:::

We can recover applicatives from this category by setting `x = ()`{.haskell}, thus `f y <-> Arrf f () y`{.haskell}.
This is called taking a [global element](https://ncatlab.org/nlab/show/global+element), since `()`{.haskell} is the [terminal object](https://ncatlab.org/nlab/show/terminal+object).

```haskell
newtype Global arr y = Global (arr () y)

local :: Functor f => Arrf f () (x -> y) -> Arrf f x y
local (Arrf f) = Arrf (($ ()) <$> f)

instance Applicative f => Applicative (Global (Arrf f)) where
  pure x = Global (x <$ id)
  Global f <*> Global x = Global (local f . x)
```

This is just like we do with Kleisli arrows, to recover monads.

```haskell
raise :: Functor f => (x -> Kleisli f () y) -> Kleisli f x y
raise f = Kleisli \x -> f x ()

instance Monad f => Monad (Global (Kleisli f)) where
  return x = Global (x <$ id)
  Global x >>= Global f = Global (raise f . x)
```

:::Bonus
Stick around for [another blog post](selective-applicatives-theoretical-basis.html) describing selective applicative functors through this perspective too!
That will motivate how I stumbled across this one.

https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Selective.purs
:::

:::{.Bonus box-name="Relative monads"}
Monads `f`{.haskell} relative to a functor `h`{.haskell} have a Kleisli category of morphisms `h x -> f y`{.haskell}.
:::

:::{.Warning .full-width box-name="Disclaimer"}
This post mostly discusses category theory in the context of \(\mathrm{Set}\), as applied to Haskell and other programming languages.
It is important to note that these concepts can and should be considered in greater generality: applicative functors (unlike monads) are not restricted to endofunctors; they can map between different categories.
And, well, people take issue with applying concepts from \(\mathrm{Set}\) to Haskell, since it is not a total language and has some other semantic knots that make desired laws not work out outside of a [meaningful total fragment](https://dl.acm.org/doi/abs/10.1145/1111320.1111056).
:::

## Free constructions

The free construction for the first two forms rapidly converge on the Day Convolution.
The Day Convolution ([Hackage](https://hackage.haskell.org/package/kan-extensions-5.2.5/docs/Data-Functor-Day.html#t:Day), [Pursuit](https://pursuit.purescript.org/packages/purescript-day/10.0.1/docs/Data.Functor.Day#t:Day)) of functors is one of the most important tensors out there!

Thereʼs a couple ways to present it, one that is symmetric (which is æsthetically satisfying!), and two that resemble the type signatures of `<*>`{.haskell} and `<**>`{.haskell} very closely:

```haskell
data Day f g r = forall u v. Day (f u) (g v) (u -> v -> r)

data FnDay f g r = forall i. FnDay (f (i -> r)) (g i)
data DayFn f g r = forall i. DayFn (f i) (g (i -> r))
```

:::Bonus
How are these related?
By the Yoneda lemma!
:::

I rather like what Phil Freeman does to [construct `FreeApplicative`{.haskell} and `FreeApply`{.haskell} together](https://blog.functorial.com/posts/2017-07-01-FreeAp-Is-A-Comonad.html):
```haskell
newtype FreeApplicative f a = FreeApplicative (Coproduct Identity (FreeApply f) a)

newtype FreeApply f a = FreeApply (Day f (FreeApplicative f) a)
```

We can do a similar thing to fashion a free category and free semigroupoid.
(A semigroupoid does not require an identity arrow like a category does.)

```haskell
-- A pair of compatible arrows: from `x` to `y` to `z`, with `y` hidden existentially.
data Snuggle arr1 arr2 x z = forall y. Snuggle (arr1 x y) (arr2 y z)

newtype FreeCategory arr x y = FreeCategory (Coproduct2 (->) (FreeSemigroupoid arr) x y)

newtype FreeSemigroupoid arr x y = FreeSemigroupoid (Snuggle arr (FreeCategory arr) x y)
```

:::{.Details box-name="cf."}
https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor-Composition.html
:::

:::{.Key_Idea box-name="Important Technicality!"}
Please note that there is a little dishonesty here: this constructs the free category that is also a profunctor.
This means that we can lift arbitrary functions into our arrows.

The alternative would be to define a GADT that requires its arguments are the same type, to define identity arrows specifically.

```haskell
data JustIdentity x z where
  JustIdentity :: JustIdentity y y
```

However, for our purposes of modelling Applicative Functors, using `(->)`{.haskell} works much better.

Obligatory note that `Arrow`{.haskell} is not the intersection of `Strong`{.haskell} (a subclass of `Profunctor`{.haskell}) and `Category`{.haskell} – [it adds some laws](https://www.eyrie.org/~zednenem/2017/07/twist).
:::

So now we can circle around and show that the categorical building blocks are enough to get back to free applicatives:

```haskell
applicativeCategory :: FreeApplicative f r <-> FreeCategory (Arrf f) () r

applySemigroupoid :: FreeApply f r <-> FreeSemigroupoid (Arrf f) () r
```

<details class="Details">

<summary>Long Proof</summary>

… two hours of pain later&nbsp;…

(yes I did just make up my own syntax for isomorphisms)

```haskell
-- We are going to show that `Arrrf f ()` is naturally isomorphic to
-- `FreeApplicative f`.
type Arrrf f = FreeCategory (Arrf f)

-- We need two small isomorphisms off the bat, since we will be dealing with
-- functions out of the unit type
trivial1 :: Identity r <-> (->) () r
Identity r <=> \() -> r

trivial2 :: f r <-> Arrf f () r
fr <=> Arrf fur
  where
  fr =(isomap (r <=> \() -> r))= fur

-- The isomorphism we want, via case analysis
applicativeCategory :: FreeApplicative f r <-> Arrrf f () r
-- The first case is trivial
Inl ir <=> Inl2 ur
  where
  ir =(trivial1)= ur
-- The second case we will defer to the next isomorphism
Inr anApply <=> Inr2 aCompose
  where
  anApply =(applySemigroupoid)= aCompose

-- The isomorphism for the corresponding non-empty structures
applySemigroupoid :: FreeApply f r <-> FreeSemigroupoid (Arrf f) () r
-- We use DayFn to make it slightly easier for ourselves (since the proof that
-- DayFn and Day are equal is tricky)
DayFn f g <=> Snuggles arr brr
  where
  -- Both DayFn and Snuggles introduce an existential type variable; I call it χ
  -- and in this case we can take the same value for it on both sides
  (f :: f χ) <-> (arr :: Arrf f () χ)
  f =(trivial2)= arr
  (g :: FreeApplicative f (χ -> r)) <-> (brr :: Arrrf f χ r)
  g =(applicativeCategoryFn)= brr

-- This is a very important (and stronger) isomorphism we need to show by
-- composition of the main isomorphism with another helper
applicativeCategoryFn :: FreeApplicative f (i -> r) <-> Arrrf f i r
applicativeCategoryFn = applicativeCategory <<< categoryFn

-- The isomorphism we need to finish it off: the fact that we can take out
-- a function from the output side of the morphism.
categoryFn :: Arrrf f () (i -> r) <-> Arrrf f i r
-- It is very hard to write it down as a single isomorphism, so we have to
-- write down each direction and prove that they are inverses
categoryFn-> arrrp = lmap (\i -> (i, ())) (second' arrrp)
categoryFn<- arrrp = lmap (\() -> id) (closed arrrp)

-- Proof that categoryFn-> (categoryFn<- arrrp) = arrrp:
  lmap (\i -> (i, ())) (second' (lmap (\() -> id) (closed arrrp)))
  -- second' over lmap
= lmap (\i -> (i, ())) (lmap (second' \() -> id) (second' (closed arrrp)))
  -- second' for functions
= lmap (\i -> (i, ())) (lmap (\(i, ()) -> (i, id)) second' (closed arrrp))
  -- lmap composition
= lmap ((\i -> (i, ())) >>> (\(i, ()) -> (i, id))) >>> second' (closed arrrp)
  -- >>> for functions
= lmap (\i -> (i, id)) (second' (closed arrrp))
  -- trust me :3
= arrrp

-- Proof that categoryFn<- (categoryFn-> arrrp) = arrrp:
  lmap (\() -> id) (closed (lmap (\i -> (i, ())) (second' arrrp)))
  -- closed over lmap
= lmap (\() -> id) (lmap (closed \i -> (i, ())) (closed (second' arrrp)))
  -- closed for functions
= lmap (\() -> id) (lmap (\g -> \j -> (g j, ())) (closed (second' arrrp)))
  -- lmap composition
= lmap ((\() -> id) >>> (\g -> \j -> (g j, ()))) (closed (second' arrrp)))
  -- >>> for functions
= lmap ((\() -> \j -> (j, ()))) (closed (second' arrrp))
  -- trust me :3
= arrrp

instance Strong arr => Strong (FreeCategory arr) where
  first' (Inl2 fn) = Inl2 (first' fn)
  first' (Inr2 (Snuggles arr continue)) =
    Inr2 (Snuggles (first' arr) (first' continue))
  second' (Inl2 fn) = Inl2 (second' fn)
  second' (Inr2 (Snuggles arr continue)) =
    Inr2 (Snuggles (second' arr) (second' continue))

instance Closed arr => Closed (FreeCategory arr) where
  closed (Inl2 fn) = Inl2 (closed fn)
  closed (Inr2 (Snuggles arr continue)) =
    Inr2 (Snuggles (closed arr) (closed continue))

instance Functor f => Strong (Arrf f) where
  first' (Arrf f) = Arrf (first' <$> f)
  second' (Arrf f) = Arrf (second' <$> f)

instance Functor f => Closed (Arrf f) where
  first' (Arrf f) = Arrf (closed <$> f)
```

<!--

-- Here's a way of writing out the details, though:
-- The first case is literally trivial
Inl2 uχr <=> Inl2 χr
  where
  uχr <=> \() -> χr
-- Here's where we have to stop relying on isomorphisms: the problem is that
-- we need to choose different existential variables going each direction
Inr2 (Snuggles urr vrr) <=> Inr2 (Snuggles arr brr)
  where
  -- In this obligation we need to tuple in the input, to preserve it until
  -- we can apply it at the output
  (urr :: Arrrf f () χ) -> (arr :: Arrrf f i (i, χ))
  keep urr |=> arr
  (vrr :: Arrrf f χ (i -> r)) -> (brr :: Arrrf f (i, χ) r)
  delayedApply vrr |=> brr

  -- In this obligation we
  (urr :: Arrrf f () (i -> χ)) <- (arr :: Arrrf f i χ)
  urr <=| defer arr
  (vrr :: Arrrf f (i -> χ) (i -> r)) <- (brr :: Arrrf f χ r)
  vrr <=| closed brr

keep :: Arrrf f () r -> Arrrf f i (i, r)
keep arrrp = Inl2 (\i -> (i, ())) >>> second' arrrp

delayedApply :: Arrrf f s (i -> r) -> Arrrf f (i, s) r
delayedApply arrrp = second' arrrp >>> Inl2 (\(i, ir) -> ir i)

defer :: Arrrf f i r -> Arrrf f () (i -> r)
defer arrrp = Inl2 (\() -> \i -> i) >>> closed arrrp

-->

</details>

### Interprets

These are free constructions exactly like `List`{.haskell} is the free monoid:
They are right associative, so they satisfy identity and associativity directly, and do not need to be quotiented by laws.

The defining characteristic of a free construction is that it can be interpreted into any other instance of that structure.
For the free monoiad, this is `foldMap`{.haskell} (one of my favorite functions … I like monoids, so that should be no surprise):

```haskell
foldMap :: forall t m. Monoid m => (t -> m) -> List t -> m
foldMap f (Cons head tail) =
  -- head :: t
  -- tail :: List t
  f head <> foldMap f tail
foldMap _ Nil = mempty
```

This can be done for the new free constructions.

```haskell
interpretApplicative ::
  forall f m.
    Applicative m =>
  (forall r. f r -> m r) ->
  (forall r. FreeApplicative f r -> m r)
interpretApplicative f2m (FreeApplicative (Inr (FreeApply (Day head tail combine)))) =
  -- head :: f x
  -- tail :: FreeApplicative f y
  -- combine :: x -> y -> r
  liftA2 combine (f2m head) (interpretApplicative f2m tail)
interpretApplicative  _  (FreeApplicative (Inl r)) =
  -- r :: r
  pure r

interpretCategory ::
  forall p c.
    Category c =>
    Profunctor c =>
  (forall x y. p x y -> c x y) ->
  (forall x y. FreeCategory p x y -> c x y)
interpretCategory p2c (FreeCategory (Inr2 (FreeSemigroupoid (Snuggle head tail)))) =
  -- head :: p x y
  -- tail :: FreeCategory p y z
  p2c head >>> interpretCategory p2c tail
interpretCategory  _  (FreeCategory (Inl2 fn)) =
  -- fn :: x -> y
  rmap fn id
```


### Folds

We can also define some folds on these types for static analysis.

Itʼs easy enough to define them directly, but conceptually they are interpretations into constant functors.

```haskell
foldApplicative ::
  Monoid m =>
  (forall a. f a -> m) ->
  forall a. FreeApplicative a -> m
foldApply ::
  Semigroup m =>
  (forall a. f a -> m) ->
  forall a. FreeApply a -> m

foldCategory ::
  Monoid m =>
  (forall x y. arr x y -> m) ->
  forall x y. FreeCategory x y -> m
foldSemigroupoid ::
  Semigroup m =>
  (forall x y. arr x y -> m) ->
  forall x y. FreeSemigroupoid x y -> m
```

## Mathematically

### Background

These are some of the most beautiful concepts for me.
The simple pleasures of category theory.

#### Ordinary monoids

To recap the ordinary definition of monoids that you see in Haskell or learn in an Abstract Algebra class (okay, you probably learn about groups in Abstract Algebra, not monoids):

A monoid is a type \(M\) (called the carrier type) equipped with a binary operation \(\diamond : M \times M \to M\) and identity \(i : M\) and satisfying three laws:

1. Associativity: for all \(x, y, z : M\), \((x \diamond y) \diamond z = x \diamond (y \diamond z)\)
2. Left identity: for all \(x : M\), \(i \diamond x = x\)
3. Right identity: for all \(x : M\), \(x \diamond i = x\)

Often the monoid is written as a tuple \((M, \diamond, i)\) in math notation.
In Haskell, it is expressed as a typeclass
```haskell
class Monoid m where
  (<>) :: m -> m -> m
  mempty :: m
```
so the operation is inferred from the carrier type, and newtypes are used to give other instances ([e.g.]{t=} [`Additive`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Monoid.Additive#t:Additive) and [`Multiplicative`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Monoid.Multiplicative)).
In dependent type theories, the laws can be provided as fields like the operations:
```agda{data-lang=""}
record MonoidOn (M : Type) : Type where
  (<>) : M -> M -> M
  mempty : M
  assoc : ∀ x y z. (x <> y) <> z = x <> (y <> z)
  idL : ∀ x. mempty <> x = x
  idR : ∀ x. x <> mempty = x

record MonoidOf {M : Type} ((<>) : M -> M -> M) (mempty : M) : Type where
  assoc : ∀ x y z. (x <> y) <> z = x <> (y <> z)
  idL : ∀ x. mempty <> x = x
  idR : ∀ x. x <> mempty = x
```

Once the binary operation \(\diamond\) is chosen, the identity, if it exists, is uniquely determined.^[This is called [“property-like structure”](https://ncatlab.org/nlab/show/stuff,+structure,+property).]

<details class="Details">

<summary>Uniqueness of identity</summary>

Imagine there were two identities, \(i_1\) and \(i_2\).
Consider the expression \(i_1 \diamond i_2\):

- By the left identity of \(i_1\), \(i_1 \diamond i_2 = i_2\).
- But on the flipside, by the right identity of \(i_2\), \(i_1 \diamond i_2 = i_1\).
- Therefore \(i_2 = i_1\).

Both of the identities defer to each other, collapsing into the same value.

So if \((M, \diamond, i_1)\) and \((M, \diamond, i_2)\) are two monoids with the same carrier type and operation, they are the same monoid.

</details>

Between two monoids \((M, \diamond, i)\) and \((N, \hearts, e)\) there is a type of **monoid homomorphisms**, which consists of:

- A function \(f : M \to N\) between the carrier types,
- which preserves the monoid operation: \(f(x \diamond y) = f(x) \hearts f(y)\),
- and the identity: \(f(i) = e\).^[In a group, preserving the inverse means that homomorphisms automatically preserve the identity. In a monoid, it needs to be imposed explicitly.]

This defines the object (monoids) and morphisms (monoid homomorphisms) of the category \(\mathrm{Mon}\) of monoids.

Having a binary operation, a uniquely determined identity (both preserved by morphisms), plus associativity and identity laws, is a theme throughout everything that is discussed here:

- Monoids
- Categories
- Monoidal tensors
- Monoid objects
- Enriched categories
- Applicative functors
- Monads

Each manifestation looks a bit different, but they have the same idea at their core.

#### Monoidal category

Before we can define lax monoidal functors, we need to define monoidal categories.

A monoidal category is a category \(\mathcal C\) with a bifunctor \(\ox : \mathcal C \x \mathcal C \to \mathcal C\) (called a “monoidal product” or “monoidal tensor” or “tensor” or “tensor product” or …) that is associative and an object \(\mathrm I : \mathcal C\) that serves as an identity for the tensor: \(\mathrm I \ox X \cong X \cong X \ox \mathrm I\).

There are some laws that the structure morphisms, the associator and unitors, have to satisfy: these are named the triangle and pentagon identities.^[In higher category theory, they themselves become morphisms, pentagonator!]

Monoidal categories may be symmetric, or merely “braided”.
But this is not necessary: all that matters is associativity and even asymmetric bifunctors can be tensors.

Categories usually have multiple suitable tensors: this means they can be seen as monoidal categories in several ways.

Of course one first thinks of the categorical product \(\times\), which is characterized by being the best solution to the pair of projections \(X \times Y \to X\) and \(X \times Y \to Y\).
But its categorical dual, the coproduct, is also a monoidal “tensor product”: characterized by injections \(X \to X + Y\) and \(Y \to X + Y\).
(Some categories have a biproduct: the product and coproduct coïncide and is called the biproduct.)

Categories of algebras often have interesting tensors.
<!-- The category of vector spaces (with linear functions/matrices as morphisms). -->

Categories of topological spaces or pointed topological spaces have some interesting tensors too: [e.g.]{t=} the [wedge sum](https://en.wikipedia.org/wiki/Wedge_sum).

Categories for linear logic / linear type theory usually come with four fundamental tensors: multiplicative and additive conjunction and disjunction.

Functor categories have some interesting tensors: they can inherit the product and coproduct.
The Day convolution is another important tensor; it requires a bit more structure to define (the functor must land in a cocomplete symmetric monoidal category).

A category of endofunctors (functors from a category to itself = where the source and destination categories are identical) has another interesting tensor: _composition_.
Thatʼs right: functor composition is decidedly not symmetrical in any way, so it defies our intuition, but it is associative, and the identity functor serves as its identity on both sides, so it is a perfectly fine monoidal tensor.

#### Monoid object

A monoid object \(M\) in a monoidal category is an object with a monoid operation, as a morphism in \(\mathcal C\): \(\mu : M \ox M \to M\), plus an identity, also as a morphism: \(\eta : \mathrm I \to M\).

As youʼve heard innumerable times by now, this monoid object needs to satisfy associativity and identity axioms.
Interestingly, to _state_ these axioms requires the use of the associator and unitors from the monoidal category (plus other categorical structure).

<details class="Details">

<summary>Stating the laws</summary>

Using the usual names for these isomorphisms, the associator \(\alpha_{X,Y,Z} : (X \ox Y) \ox Z \cong X \ox (Y \ox Z)\), the left unitor \(\lambda_X : \mathrm I \ox X \cong X\), and the right unitor \(\rho_X : X \ox \mathrm I \cong X\), the laws are:

(A) Associativity: \(\alpha_{M,M,M}^{-1} \then (\mu \ox id_M) \then \mu = (id_M \ox \mu) \then \mu : M \ox (M \ox M) \to M\)

    To take a right-associated triple \(M \ox (M \ox M)\) and evaluate it down to a single \(M\), the two options are:

    i.  Evaluate it left-associatively,
        1. Left-associate it with \(\alpha_{M,M,M}^{-1} : M \ox (M \ox M) \to (M \ox M) \ox M\),
        2. then evaluate the left side with \((\mu \ox id_M) : (M \ox M) \ox M \to (M) \ox M\),
        3. and evaluate what is left with \(\mu : M \ox M \to M\);
    ii. Or, evaluate it right-associatively,
        1. Evaluate the right side with \((id_M \ox \mu) : M \ox (M \ox M) \to M \ox (M)\),
        3. and evaluate what is left with \(\mu : M \ox M \to M\).
(B) Left identity: \((\eta \ox id_M) \then \mu = \lambda_M : \mathrm I \ox M \to M\)

    i.  Turn the tensor identity \(\mathrm I\) into the identity element in \(M\) with \(\eta\), while preserving the value on the right; then compose them,
    ii. Or just drop the tensor identity with the left unitor \(\lambda_M\); the result is the same.
(C) Right identity: \((id_M \ox \eta) \then \mu = \rho_M : M \ox \mathrm I \to M\)

    i.  Turn the tensor identity \(\mathrm I\) into the identity element in \(M\) with \(\eta\), while preserving the value on the left; then compose them,
    ii. Or just drop the tensor identity with the right unitor \(\rho_M\); the result is the same.

</details>

The end result is that a monoid object has \(n\)-ary composition for any \(n \ge 0\): \(M \ox M \ox \cdots \ox M \to M\), where for \(n > 1\) the associativity of the tensors does not matter for \(n > 1\), and for \(n = 0\) the nullary tensor is the identity \(\mathrm I\) of course.

#### Enriched category

Finally, it is worth noting that monoidal categories are a great setting to define *enriched categories*, which are worth studying but I will not go into much detail here.

Similar to monoid objects, composition and identity is defined as morphisms in the category of enrichment, by using its monoidal tensor.

For all objects \(x\), \(y\), and \(z\), composition is given by a morphism \(\mathrm{hom}(x, y) \ox \mathrm{hom}(y, z) \to \mathrm{hom}(x, z)\) and the identity is given by a morphism \(\mathrm I \to \mathrm{hom}(x, x)\).

Ordinary categories can be thought of as categories enriched in \(\mathrm{Set}\).
This is, in fact, all we need for the purposes of this article.

A closed category can be thought of as a category enriched in itself.
That is, a closed category has morphisms who are themselves representable as objects in the category: this is how function types work, for example, and why curried functions `x -> y -> z`{.haskell} make sense while category morphisms are not nestable or associative like that.
\(\mathrm{Set}\) is a closed category, so it makes sense that it is both.

Sometimes you enrich in posets, categories with less structure.
The poset of truth values is a key example, as are posets of numbers, especially the unit interval \([0, 1]\) of real numbers for probabilities.

:::Bonus
Sometimes you enrich in categories with even more structure: enriched category theory can inform higher category theory.
These enriched morphisms look globular, if that is a term that makes sense to you.
:::

### Applicative functors as:

Okay, now that you have something to look at for background on the maths, letʼs get into the first definition.

#### Lax monoidal functors

Applicatives are most often explained as lax monoidal functors.

A lax monoidal functor \(F : (\mathcal C, \ox, \mathrm I) \to (\mathcal D, \oplus, \mathrm J)\) says that the functor, which already respects the category structure, also plays nicely with the additional monoidal category structure.

In particular, it asks the functor to respect the monoidal identity with a function \(\mathrm J \to F \mathrm I\) and respect the monoidal tensor with \(F X \oplus F Y \to F (X \ox Y)\).
These functions themselves have to satisfy identity and associativity axioms: this is where the laws of the applicative functor come from.

:::Warning
Notice how the structure of the output category \(\mathcal D\), \(\oplus\) and \(J\), appear on the _left_ side of the morphisms, while that of the input category \(\mathcal C\), \(\ox\) and \(I\) appears on the output side (_inside_ of \(F\)).
:::

:::Bonus
A oplax monoidal functor flips these arrows, and a monoidal functor requires them to be isomorphisms.
:::

For conventional applicative functors, the categories are both \(\mathrm{Set}\) with the product as tensor.
This translates into getting functions `() -> f ()`{.haskell} (`pure`{.haskell} specialized to `()`{.haskell}) and `(f x, f y) -> f (x, y)`{.haskell} (`liftA2 (,)`{.haskell}).
Using `fmap`{.haskell} you can derive the usual operations, `pure`{.haskell} and `(<*>)`{.haskell}.

You can look at other monoidal structures on \(\mathrm{Set}\) to get some other useful lax monoidal functors.

:::Warning
A lot of (op)lax monoidal functors are trivial in \(\mathrm{Set}\), in part because comonoids are trivial in \(\mathrm{Set}\), or weird to the point of being not useful.

- Oplax monoidal functor with product is trivial: \(F(X \times Y) \to F(X) \times F(Y)\) is just two projections lifted through \(F\).
- Lax monoidal functor with coproduct is also trivial: \(F(X) + F(Y) \to F(X + Y)\) does the dual, lift the appropriate injection through \(F\) for whichever case comes through.
- Oplax between coproducts is weird: \(F(X + Y) \to F(X) + F(Y)\) has obvious implementations for `Identity`{.haskell} and `Maybe`{.haskell}; I think you can give a lawful implementation for lists that chooses the side that occurs first; but a lot of useful functors do not have instances.
:::


#### Monoid objects

Next we have the definition that I find a little cleaner, æsthetically.

Applicative functors are monoid objects in the category of endofunctors *with the Day convolution tensor*.

:::Warning
This is not to be confused with [monoids in the category of endofunctors *with the composition tensor*](https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem) – those are monads, not applicatives.
:::

Unpacking this: the monoid object \(M : \mathrm{End}(\mathcal C)\) is an endofunctor \(\mathcal C \to \mathcal C\).
It comes with two morphisms, which are natural transformations: composition \(M \ox M \to M\), and the identity \(\mathrm I \to M\), where \(\mathrm I\) is the identity functor.

The natural transformation for the monoid identity gives `pure`{.haskell}, the identity for the applicative functor: we would write the natural transformation as `forall t. Identity t -> M t`{.haskell}, and use just `t`{.haskell} instead of the newtype `Identity t`{.haskell}.

The natural transformation for composition would be written as `forall t. Day M M t -> M t`{.haskell}.
But since `Day`{.haskell} is on the left of an arrow, we can unpack its existential and come up with
```haskell
forall t x y. (x -> y -> t, M x, M y) -> M t
```
which is the curried version of `liftA2`{.haskell}!
Whereupon you recover `(<*>) = liftA2 id`{.haskell} as usual.

One reason I like this definition is that it is just a monoid object: there is no lax or oplax modifier that somewhat arbitrarily sets the direction of the morphisms.
But it also highlights the Day convolution beautifully, which is incredibly useful when defining not only the free structure but also other constructions related to applicative functors, and is cool in its own right.

#### Change of enrichment category

For our last magic trick, we will change from a \(\mathrm{Set}\)-enriched category to a … \(\mathrm{Set}\)-enriched category.

As I mentioned before, `f (x -> y)`{.haskell} is the carrier type for this change of basis.
Take a morphism and wrap it with the functor.
The structure of an applicative is exactly what we need to transfer the category structure, à la `liftA2 (.)`{.haskell}.
By “exactly”, I mean that you can either start with a lax monoidal functor and show that it makes a viable category structure, or equivalently, assume that `f (x -> y)`{.haskell} has a category structure and rebuild the structure of the applicative functor.

I think thereʼs a nice simplicity to this version of the story.
Itʼs a straightforward category, and a category provides a nice jumping-off point for other structure, which is why it was so nice for my [exploration of selective applicative functors](selective-applicatives-theoretical-basis.html).



### Equivalence

I went through some of the steps of some of the isomorphisms above.

But just notice that all of the definitions come with some form of left and right identities and some form of associativity.

The data is all the same, the shapes are all the same, it is just a matter of checking some details and proving it.
