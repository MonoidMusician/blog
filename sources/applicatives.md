---
title: Views on Applicative Functors
subtitle: 3+ ways to think about applicative functors
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

Choose your fighter!

- `liftA2 ($) :: f (a -> b) -> f a -> f b`{.haskell}
- `liftA2 (,) :: f u -> f v -> f (u, v)`{.haskell}
- `liftA2 (.) :: f (y -> z) -> f (x -> y) -> f (x -> z)`{.haskell}

Applicative functors are very important.
The first two ways of constructing them are often talked about, [e.g.]{t=} see [“The monoidal presentation”](https://en.wikibooks.org/wiki/Haskell/Applicative_functors#The_monoidal_presentation) on Haskellʼs WikiBook.

But thereʼs a third way that I find really interesting!

Just like monads give rise to Kleisli arrows, applicative give rise to a category by taking arrows of this shape:

```haskell
newtype Arrp f a b = Arrp (f (a -> b))

instance Applicative f => Category (Arrp f) where
  id = Arrp (pure id)
  Arrp f . Arrp g = Arrp (liftA2 (.) f g)

newtype Kleisli f a b = Kleisli (a -> f b)

instance Applicative f => Category (Kleisli f) where
  id = Kleisli return
  Kleisli f . Kleisli g = Kleisli (\a -> f =<< g a)
```

:::{.Details box-name="cf."}
https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor-Cayley.html
:::

Note the key difference here: in `Kleisli`{.haskell}, the `f`{.haskell} only appears inside, after the argument is given; in `Arrp`{.haskell}, the `f`{.haskell} appears on the outside, so it governs over the functions.

We can recover applicatives from this category by setting `a = ()`{.haskell}, thus `f b <-> Arrp f () b`{.haskell}.^[This is called taking a [global element](https://ncatlab.org/nlab/show/global+element), since `()`{.haskell} is the [terminal object](https://ncatlab.org/nlab/show/terminal+object).]

```haskell
newtype Global arr b = Global (arr () b)

local :: Functor f => Arrp f () (a -> b) -> Arrp f a b
local (Arrp f) = Arrp (($ ()) <$> f)

instance Applicative f => Applicative (Global (Arrp f)) where
  pure a = Global (a <$ id)
  Global f <*> Global a = Global (local f . a)
```

This is just like we do with Kleisli arrows, to recover monads.

```haskell
raise :: Functor f => (a -> Kleisli f () b) -> Kleisli f a b
raise f = Kleisli \a -> f a ()

instance Monad f => Monad (Global (Kleisli f)) where
  return a = Global (a <$ id)
  Global a >>= Global f = Global (raise f . a)
```

:::Bonus
Stick around for another blog post describing selective applicative functors through this perspective too!
That will motivate how I stumbled across this one.

https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Selective.purs
:::

:::{.Bonus box-name="Question"}
Can we explain relative monads somehow?
:::

## Free constructions

The free construction for the first two forms rapidly converge on the Day Convolution.
The Day Convolution ([Hackage](https://hackage.haskell.org/package/kan-extensions-5.2.5/docs/Data-Functor-Day.html#t:Day), [Pursuit](https://pursuit.purescript.org/packages/purescript-day/10.0.1/docs/Data.Functor.Day#t:Day)) of functors is one of the most important tensors out there!

Thereʼs a couple ways to present it, one that is symmetric (which is æsthetically satisfying!), and two that resemble the type signatures of `<*>`{.haskell} and `<**>`{.haskell} very closely:

```haskell
data Day f g r = forall u v. Day (f u) (g v) (u -> v -> r)

data FnDay f g r = forall a. FnDay (f (a -> r)) (g a)
data DayFn f g r = forall a. DayFn (f a) (g (a -> r))
```

:::Details
How are these related?
By the Yoneda lemma!
:::

I rather like what Phil Freeman does to [construct `FreeApplicative`{.haskell} and `FreeApply`{.haskell} together](https://blog.functorial.com/posts/2017-07-01-FreeAp-Is-A-Comonad.html):
```haskell
newtype FreeApplicative f a = FreeApplicative (Coproduct Identity (FreeApply f) a)

newtype FreeApply f a = FreeApply (Day f (FreeApplicative f) a)
```

We can do a similar thing to fashion a free category and free groupoid.
(A groupoid does not require an identity arrow like a category does.)

```haskell
data Snuggles arr1 arr2 x z = forall y. Snuggle (arr1 x y) (arr2 y z)

newtype FreeCategory arr x y = FreeCategory (Coproduct2 (->) (FreeGroupoid arr) x y)

newtype FreeGroupoid arr x y = FreeGroupoid (Snuggles arr (FreeCategory arr) x y)
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
:::

So now we can circle around and show that the categorical building blocks are enough to get back to free applicatives:

```haskell
applicativeCategory :: FreeApplicative f r <-> FreeCategory (Arrp f) () r

applyGroupoid :: FreeApply f r <-> FreeGroupoid (Arrp f) () r
```

<details class="Details">

<summary>Long Proof</summary>

… two hours of pain later …

(yes I did just make up my own syntax for isomorphisms)

```haskell
-- We are going to show that `Arrrp f ()` is naturally isomorphic to
-- `FreeApplicative f`.
type Arrrp f = FreeCategory (Arrp f)

-- We need two small isomorphisms off the bat, since we will be dealing with
-- functions out of the unit type
trivial1 :: Identity r <-> (->) () r
Identity r <=> \() -> r

trivial2 :: f r <-> Arrp f () r
fr <=> Arrp fur
  where
  fr =(isomap (r <=> \() -> r))= fur

-- The isomorphism we want, via case analysis
applicativeCategory :: FreeApplicative f r <-> Arrrp f () r
-- The first case is trivial
Inl ir <=> Inl2 ur
  where
  ir =(trivial1)= ur
-- The second case we will defer to the next isomorphism
Inr anApply <=> Inr2 aCompose
  where
  anApply =(applyGroupoid)= aCompose

-- The isomorphism for the corresponding non-empty structures
applyGroupoid :: FreeApply f r <-> FreeGroupoid (Arrp f) () r
-- We use DayFn to make it slightly easier for ourselves (since the proof that
-- DayFn and Day are equal is tricky)
DayFn f g <=> Snuggles arr brr
  where
  -- Both DayFn and Snuggles introduce an exitential type variable; I call it χ
  -- and in this case we can take the same value for it on both sides
  (f :: f χ) <-> (arr :: Arrp f () χ)
  f =(trivial2)= arr
  (g :: FreeApplicative f (χ -> r)) <-> (brr :: Arrrp f χ r)
  g =(applicativeCategoryFn)= brr

-- This is a very important (and stronger) isomorphism we need to show by
-- composition of the main isomorphism with another helper
applicativeCategoryFn :: FreeApplicative f (i -> r) <-> Arrrp f i r
applicativeCategoryFn = applicativeCategory <<< categoryFn

-- The isomorphism we need to finish it off: the fact that we can take out
-- a function from the output side of the morphism.
categoryFn :: Arrrp f () (i -> r) <-> Arrrp f i r
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

instance Functor f => Strong (Arrp f) where
  first' (Arrp f) = Arrp (first' <$> f)
  second' (Arrp f) = Arrp (second' <$> f)

instance Functor f => Closed (Arrp f) where
  first' (Arrp f) = Arrp (closed <$> f)
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
  (urr :: Arrrp f () χ) -> (arr :: Arrrp f i (i, χ))
  keep urr |=> arr
  (vrr :: Arrrp f χ (i -> r)) -> (brr :: Arrrp f (i, χ) r)
  delayedApply vrr |=> brr

  -- In this obligation we
  (urr :: Arrrp f () (i -> χ)) <- (arr :: Arrrp f i χ)
  urr <=| defer arr
  (vrr :: Arrrp f (i -> χ) (i -> r)) <- (brr :: Arrrp f χ r)
  vrr <=| closed brr

keep :: Arrrp f () r -> Arrrp f i (i, r)
keep arrrp = Inl2 (\i -> (i, ())) >>> second' arrrp

delayedApply :: Arrrp f s (i -> r) -> Arrrp f (i, s) r
delayedApply arrrp = second' arrrp >>> Inl2 (\(i, ir) -> ir i)

defer :: Arrrp f i r -> Arrrp f () (i -> r)
defer arrrp = Inl2 (\() -> \i -> i) >>> closed arrrp

-->

</details>

### Interprets

### Folds

We can define some folds on these types for static analysis.

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
foldGroupoid ::
  Semigroup m =>
  (forall x y. arr x y -> m) ->
  forall x y. FreeGroupoid x y -> m
```

Obligatory https://www.eyrie.org/~zednenem/2017/07/twist

### Mathematically

Lax monoidal functors.

Monoids in the category of endofunctors *with the Day convolution tensor*.
(Not to be confused with [monoids in the category of endofunctors *with the composition tensor*](https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem).)

Enriched category? idk.
