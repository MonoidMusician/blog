---
title: Selective Applicative Functors
subtitle: The Missing Theoretical Basis for *Exclusive Determined Choice*
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

I havenʼt seen a good accounting of the essence of selective applicative functors.

Theyʼve been longing for a better description, to help explain what should be allowed and what should be disallowed, beyond “hey, here is a function `select`{.haskell} that seems to do useful things and enable us to write interesting code”.

Selective applicative functors were originally proposed in 2019 in the paper [Selective Applicative Functors](https://dl.acm.org/doi/pdf/10.1145/3341694), by Andrey Mokhov, Georgy Lukyanov, Simon Marlow, and Jeremie Dimino, with this typeclass definition:

```haskell
class Applicative f => Selective f where
  select :: f (Either a b) -> f (a -> b) -> f b
```

The `branch`{.haskell} combinator was unfortunately not integral to the story, 

```haskell
branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch x l r = fmap (fmap Left) x <*? fmap (fmap Right) l <*? r
```

But the story has stopped short after `select`{.haskell} because the familiar tools of theoretical analysis failed to apply to `select`{.haskell} specifically: there was no account for `select`{.haskell} in terms of monoidal tensors, and even `branch`{.haskell} did not meaningfully compose with itself, which means that there was no obvious way that the laws related to more familiar algebraic and categorical structures like monoids (and, it turns out, near-semirings).

Howevver, the final answer for what selective applicative functors want to be is really cool.
We just have to work a bit harder to see it: we have to consider arrows (composable profunctors) instead of plain functors.

Selective applicative functors want to encode *exclusive determined choice*.
They can choose between a finite number of predetermined case branches based upon previous results during evaluation.^[`select`{.haskell} itself has an implicit `pure`{.haskell} branch with no side-effects.]

Once you see it, it makes so much sense from a programming language perspective: it has the shape of an AST for a programming language.
We can even go farther and relate it to the other typeclass for control flow, `Alternative`{.haskell}, which provides `<|>`{.haskell} for *nondeterministic* choice.

## Overview

Here it is, here is the essence of selective applicatives!

Letʼs start from the familiar territory of monads.
Monads encode the essence of dynamic control flow: because `>>=`{.haskell} allows binding any function as a continuation, an action in a monad can construct an arbitrary action to execute next, *dynamically*.
This also forces a clear direction to evaluation: the left effects have to happen before the right effects, because the result of the left action is used to determine the whole action on the right.
This also disallows static analysis: the constant functor [`Const`{.haskell}]() is not an interesting monad.^[The reason it does not have a `Monad`{.haskell} instance at all is because it would not be compatible with the much more compelling `Applicative`{.haskell} instance.]

This is in contrast to applicative functors, which have no “arrow of time”: their structure can be dualized to run effects in reverse because it has no control flow required by the interface.^[Of course particular applicative functors can have interesting control flow themselves, via combinators other than `<*>`{.haskell}.]
And their static analysis, given by `Monoid m => Applicative (Const m)`{.haskell}, uses monoids to accumulate information about each action that was sequenced by `<*>`{.haskell}.

:::Key_Idea
Selective applicative functors sit in the sweet spot of expressing *finite* control flow: they allow (but do not require) that an implementation choose between a finite number of branches of otherwise static control flow.

This means that we need something that restricts `>>=`{.haskell} to *finite-case functions* to encode *exclusive determined choice*.

This will be defined as a `CaseTree`{.haskell} data type below.
:::

It turns out that the most natural setting is to consider arrows instead of functors.
The functor can be recovered as an arrow out of the unit type (spelled `()`{.haskell} in Haskell and `Unit`{.purescript} in PureScript).

Any approach that does not consider arrows seems doomed to failure, mainly for the reason that we want to work with coproducts (`either :: (x -> r) -> (y -> r) -> (Either x y -> r)`{.haskell}) which involves the domain in an essential manner, while products (`tuple :: (i -> x) -> (i -> y) -> (i -> Tuple x y)`{.haskell}) stay in the codomain and thus are more amenable to restricting to actions on functors.

The concept that we need, of “finite-case functions”, is a bit tricky to formulate (especially in programming data types: it needs existential types), which I believe is part of why it has been missed.

However, once we focus on the arrows instead of the applicative actions in isolation, it can all pop into place and it pays off in revealing details of the structure we were really after.

:::Key_Idea
For example, we learn that selective applicative functors allow static analysis via near-semirings by using a constant functor.
(That is, we define static analysis to be an interpretation into a constant functor, and we learn that near-semirings are the algebraic structure we need to make it work.)
:::

:::Warning
The common formulation of `select :: f (Either u v) -> f (u -> v) -> f v`{.haskell} **does not capture** this story.
This `select`{.haskell} is a specialization to where one branch is a constant identity branch, and the general formulation cannot be recovered perfectly.
That is, `select`{.haskell} allows for finite choice / optional effects, but it cannot encode which branches of control flow are exclusive.
:::

:::Details
We can squeeze it all into a single data type for a free arrow (a strong profunctor and a category) like this, subject to some laws.

```haskell
data ControlFlow f i r where
  Action :: (f (i -> r)) -> ControlFlow f i r
  Pure :: (i -> r) -> ControlFlow f i r
  Sequencing :: ControlFlow f i x -> ControlFlow f x r -> ControlFlow f i r
  CaseBranch ::
    (i -> Either x y) ->
    ControlFlow f x r ->
    ControlFlow f y r ->
    ControlFlow f i r
```

We then recover the free selective applicative as `FreeSelective f = ControlFlow f ()`{.haskell}.
:::

## In search of a monoidal tensor

<!-- By “tensor” I mean the generalization of tensor products in category theory.
Specifically they are formulated as monoidal products, part of the definition of a [monoidal category](https://en.wikipedia.org/wiki/Monoidal_category).

The word “product” here is legacy: a *product* in a category is a particular kind of *monoidal product* that comes with projections: `Product x y -> x`{.haskell} and `Product x y -> y`{.haskell} (satisfying laws).
But monoidal products may not have these projections.
For example, coproducts are also monoidal products. -->

Monoidal tensors are associative up to isomorphism: \(x \otimes (y \otimes z) \cong (x \otimes y) \otimes z\).
Making them monoidal is an identity object: \(I \otimes x \cong x \cong x \otimes I\).
They may also satisfy symmetry^[in two ways, see the difference between braided monoidal categories and symmetric monoidal categories]: \(x \otimes y \cong y \otimes x\).

Monoids are a really nice setting for computation, and their symmetries (associativity, and left and right identities) ensure that we can focus on structure that matters while systematically ignoring the structure that does not matter.

Having the categorical structure also enables us to separate data that the program manipulates from the structure of the program itself.
Finally, we consider static analysis as the analogous laws on plain monoids (not functors) via the constant functor (decategorification).

So we want an explanation of selective applicative functors in terms of monoidal tensors.

### Tensorfail

Most explanations are missing the point of what *should* be possible with selective applicatives:
they search for a tensor that can explain something about `select :: f (Either u v) -> f (u -> v) -> f v`{.haskell}.

[One suggestion](https://www.pls-lab.org/en/Selective_functors)^[Which I have seen a couple times] is that it should come from a lax monoidal action on coproducts, of the shape `f (Either u v) -> Either (f u) (f v)`{.haskell}, but that is just wrong:
that is the *wrong* kind of static analysis for selective applicatives to support, as it would require evaluating the control flow purely statically, meaning that `f`{.haskell} cannot contain any interesting effects after all.

That is: Lax actions over coproducts (`f (Either u v) -> Either (f u) (f v)`{.haskell}) are incredibly rare in programming (or Set-like categories, more formally).
It is clear that motivating examples of selective applicative functors do not satisfy this.
We will in fact see that selective applicative structures are much more *plentiful* than monads or applicatives.

Another approach I looked at is viewing it through the lens of possibly-constant functions, like `Either (u -> v) v`{.haskell}.
These are not nicely behaved either: they do not form a category^[at least not with nice properties], and even computationally, since we cannot detect constant functions in general, it is not clear why the computationally distinguished constant functions should be treated specially.
Which is silly, because the point of these functors is all about computation, and I believe there is a nice story for it in category theory too!

So we need to change our approach and recognize that `select`{.haskell} is not a primitive that composes nicely: it does not have a monoidal tensor, so it is not the operation we should be looking at.

We *need* monoids if we are to determine what “composes nicely” even means.
The failure of `select`{.haskell} to faithfully reproduce `branch`{.haskell} (or at least the failure of `branch`{.haskell} to faithfully reproduce a ternary `branch`{.haskell}) is exactly the failure of finding a tensorial explanation for what is happening.

We need to work harder to find the tensor in selective applicative functors, if we want to see a compositional story.

### Finite continuations

Letʼs revisit the idea of adapting `(>>=) :: f i -> (i -> f r) -> f r`{.haskell} into a function with a finite number of cases.
Can we find a data type that encodes this idea?

#### Not quite good enough

An idea that is a non-starter is just requiring that `i`{.haskell} is a finite type here: that would allow enumerating the cases `f r`{.haskell}, but there is no way to pass non-finite data from `i`{.haskell} to `r`{.haskell} (like `branch :: f (Either x y) -> f (x -> r) -> f (y -> r) -> f r`{.haskell} clearly does).

Slightly better would be something like `f (idx, dat) -> (idx -> f (dat -> r)) -> f r`{.haskell}, where `idx`{.haskell} is finite, and `dat`{.haskell} is non-finite data that can be processed to obtain `r`{.haskell}.
But again this is unsatisfactory, since `dat`{.haskell} cannot depend on the chosen index, which again is something that `branch`{.haskell} can do (and this really matters if one case results in `Void`{.haskell}, for example, and you need to use `absurd`{.haskell}).

If we had dependent types, we could do `IsFinite idx -> f (Σ (i : idx), dat i) -> (Π (i : idx), f (dat i -> r)) -> f r`{.haskell}, which *would* adequately describe what we want.
But we do not have dependent types in Haskell, and it does not feel like dependent types should be *necessary*, even if they are convenient.
And it does not particularly get us closer to a categorical formulation.

#### Branching

So … letʼs revisit the type of `branch`{.haskell} again.

We want `branch :: f (Either x y) -> f (x -> r) -> f (y -> r) -> f r`{.haskell} to look more like `(>>=) :: f i -> (i -> f r) -> f r`{.haskell}.
Is there a type `Branch f i r`{.haskell} that can replace the arrow `(i -> f r)`{.haskell} to make it happen?

We can mechanically write a datatype that expresses this: when `i`{.haskell} is of the form `Either x y`{.haskell}, then we should have the data `f (x -> r)`{.haskell} and `f (y -> r)`{.haskell}:

```haskell
data Branch f i r where
  Branch ::
    f (x -> r) ->
    f (y -> r) ->
    Branch f (Either x y) r
```

:::Key_Idea
Matching on `Branch`{.haskell} gives the type equality `i ~ (Either x y)`{.haskell}, but this is not essential, since we can always map an arbitrary `f i`{.haskell} on the left into a type that is of the form `f (Either _ _)`{.haskell} using `(<$>)`{.haskell}.
So it suffices to just have a *function* `i -> Either x y`{.haskell} instead of an equality.

Similarly, we will use `i -> Void`{.haskell} later, where we could have used a type equality.

This generalization is not crucial, per se. But it is convenient to work with ordinary profunctors down the line, instead of type equalities.
:::

Finally, to generalize this, we want to be able to **recurse**: instead of stopping at *singleton* branches `f (x -> r)`{.haskell} and `f (y -> r)`{.haskell}, what if we continued on to finitely more branches before getting to continuations of the form `f (_ -> r)`?
Can we form an actual _case tree_?

### Tensorful

Here is the final result, an arrow `CaseTree f i r`{.haskell} to replace the monadic arrow `(i -> f r)`{.haskell}, making use of those observations.

```haskell
class Casing f where
  caseTreeOn :: forall i r. f i -> CaseTree f i r -> f r

data CaseTree f i r where
  TwoCases ::
    -- How to split the input into data for each case
    -- (Note that `x` and `y` are existential here!)
    (i -> Either x y) ->
    -- Control flow for the `Left` case
    (CaseTree f x r) ->
    -- Control flow for the `Right` case
    (CaseTree f y r) ->
    CaseTree f i r
  -- One static effect
  OneCase :: (f (i -> r)) -> CaseTree f i r
  -- Represent an empty case branching
  ZeroCases :: (i -> Void) -> CaseTree f i r

-- We can recover `select` and `branch`
branch :: Casing f => f (Either x y) -> f (x -> z) -> f (y -> z) -> f z
branch input left right = caseTreeOn input $
  TwoCases id (OneCase left) (OneCase right)

select :: Applicative f => Casing f => f (Either u v) -> f (u -> v) -> f v
select input continue = caseTreeOn input $
  TwoCases id (OneCase $ pure id) (OneCase continue)

-- But the general form of `TwoCases` recursing into further `TwoCases`
-- is necessary to really express the structure of exclusive branching
```

You should think of `CaseTree f i r`{.haskell} as a “finite-case” function from datatype `i`{.haskell} to datatype `r`{.haskell}, with effects in the functor `f`{.haskell}.
It is a restricted form of `i -> f r`{.haskell}, as this function demonstrates we can get back to the monadic arrow:

```haskell
-- | One thing you can do is apply a `CaseTree` to a specific value of `i` to see
-- | what branch it chooses. This lets you apply it via `>>=`.
applyCaseTree :: forall i f r. Functor f => CaseTree i f r -> i -> f r
applyCaseTree (ZeroCases toVoid) i = absurd (toVoid i)
applyCaseTree (OneCase fir) i = fir <&> ($ i)
applyCaseTree (TwoCases fg x y) ij =
  case fg ij of
    Left i -> applyCaseTree x i
    Right j -> applyCaseTree y j
```

*Or*, we can execute all effects from the `f (_ -> r)`{.haskell}s and get `f (i -> r)`{.haskell} simply by doing the case analysis at the level of the inner functions.

```haskell
-- | The other way to apply it is via `<*>`, which means we do not get to skip
-- | executing any branches.
mergeCaseTree :: forall i f r. Applicative f => CaseTree i f r -> f (i -> r)
mergeCaseTree (ZeroCases toVoid) = pure (absurd . toVoid)
mergeCaseTree (OneCase fir) = fir
mergeCaseTree (TwoCases fg x y) =
  liftA2 (\f g ij -> either f g (fg ij)) (mergeCaseTree x) (mergeCaseTree y)
```

This data type has a *ton* of structure.
As a start, note that it is a profunctor: we can map the output covariantly, with `map :: (r -> r') -> CaseTree f i r -> CaseTree f i r'`{.haskell}, and the input contravariantly, with a function of type `(i' -> i) -> CaseTree f i r -> CaseTree i' f r`{.haskell}.

:::Details
This is the full mapping function: to transform from `CaseTree f i r`{.haskell} to `CaseTree f' j r'`{.haskell}, you need a function `j -> i`{.haskell}, a natural transformation `f ~> f'`{.haskell}, and a function `r -> r'`{.haskell}.

```haskell
mapCaseTree :: forall f f' i j r r'. Functor f' =>
  (forall d. f d -> f' d) -> (j -> i) -> (r -> r') -> CaseTree f i r -> CaseTree f' j r'
mapCaseTree _ g _ (ZeroCases toVoid) = ZeroCases (toVoid . g)
mapCaseTree f g h (OneCase fir) = OneCase (dimap g h <$> f fir)
mapCaseTree f g h (TwoCases split cx cy) =
  TwoCases (split . g) (mapCaseTree id g h cx) (mapCaseTree id g h cy)
```

Note that the recursion in `TwoCases`{.haskell} handles `f`{.haskell} up front and recurses with `f = id`{.haskell} on the existential types `x`{.haskell} and `y`{.haskell}.

Having the Yoneda lemma for this functor is important for the laws below, to ensure that no generality is lost by using `id :: Either x y -> Either x y`{.haskell} to fill in `split :: i -> Either x y`{.haskell}.

Importantly, note that the return `r`{.haskell} *and even the input* `i`{.haskell} are ordinary data: we can just map over them with arbitrary functions, they donʼt need to be special finite functions at all.
The finite structure is all contained in `CaseTree`{.haskell}.
:::

This exposes the monoidal structure between tensors we were looking for, enabling us to talk about laws and algebraic structure.

We can express associativity with `TwoCases`{.haskell}, and the identity between `TwoCases`{.haskell} and `ZeroCases`{.haskell}.
These are not equalities in the data type `CaseTree`{.haskell} itself: these are laws that we expect to apply to implementations of `Casing f`{.haskell} (selective applicative functors).

:::{.Details box-name="Laws"}
Using the associativity of the tensor `Either`{.haskell}, we can express associativity of `CaseTree (Either x (Either y z)) f r`{.haskell}.
(By the Yoneda lemma, any other choice of `xyz` that maps into `Either x (Either y z)` works just as well.)

```haskell
assoc :: Either x (Either y z) -> Either (Either x y) z
assoc (Left x) = Left (Left x)
assoc (Right (Left y)) = Left (Right y)
assoc (Right (Right z)) = Right z

cx :: CaseTree f x r
cy :: CaseTree f y r
cz :: CaseTree f z r

TwoCases id cx (TwoCases id cy cz) ~=
  TwoCases assoc (TwoCases id cx cy) cz
```

We know that `Either`{.haskell} has the identity `Void`{.haskell}.

```haskell
absurd :: Void -> x

idL :: Either Void x -> x
idL = either absurd id
idR :: Either x Void -> x
idR = either id absurd

cx :: CaseTree f x r

TwoCases id cx (ZeroCases absurd) ~= imap idL cx
TwoCases id (ZeroCases absurd) cx ~= imap idR cx
```

`Either` is also a symmetric tensor.

```haskell
sym :: Either x y -> Either y x
sym (Left x) = Right x
sym (Right y) = Left y

TwoCases id cx cy ~= TwoCases sym cy cx
```

Optionally, we can consider a kind of idempotence:

```haskell
collapse :: Either x x -> x
collapse (Left x) = x
collapse (Right x) = x

TwoCases id (OneCase cx) (OneCase cx) ~=? OneCase (dimap collapse id <$> cx)
```

This idempotence is generally bad from a computational point of view: the fact that `cx`{.haskell} appears in both cases means it is not decidable when/how this law would apply, it would require deciding whether two effects in `f`{.haskell} are equal.
But for running the program, and for some forms of static analysis (with semilattices), it should hold.
:::

---

Note that this is just a data structure.
It is a data structure with existential types (`x`{.haskell} and `y`{.haskell}) and non-uniform recursion over those existential types (`CaseTree f i r`{.haskell} recurses into `CaseTree f x r`{.haskell} and `CaseTree f y r`{.haskell}).
But it is still a finite data structure that you can recurse over.

---

You can think of `CaseTree`{.haskell} as performing two functions: it first splits the input `i`{.haskell} into a n-ary tensor product `Either x (Either y (...))`{.haskell} (where the details of associativity and empty cases should not matter in the end), and then it provides an action specifically for each case it split into: `(f (x -> r), f (y -> r), ...)`{.haskell}.

You could split these functions into their own data types, with a common index to make sure that they agree.
That indexing type would have to be a binary tree of types, which is unpleasant without dependent types.
Then you would existentially quantify over it to recover `CaseTree`{.haskell}.
But `CaseTree`{.haskell} is a nice direct formulation of their composite.

---

the `i`{.haskell} has *migrated inside* the `f`{.haskell}, so now it has passed from the “static analysis” boundary over to the runtime side of the data
and we no longer care about its branching structure, it has become a blob of arbitrary data


## Free constructions and arrows

To return to the theoretical considerations.

The free monad gets a lot of attention.

```haskell
data FreeMonad f r = Return r | Join (f (FreeMonad f r))

-- More suggestively
data FreeMonad f r
  = Return r
  | Join (Compose f (FreeMonad f r))
```

But the free applicative is much less common.
Why is this?

Well, one reason is that it requires the `Day`{.haskell} convolution, which requires existential types.
And we have already established that programmers avoid thinking about existential types.

```haskell
data FreeApplicative f r
  = Pure r
  | Apply (Day f (FreeApplicative f) r)

-- This is the style that directly relates to `(<*>)`
data Day f g r where
  -- Existential `i`
  Day :: f (i -> r) -> g i -> Day f g r

-- This is the obviously-symmetric formulation
data Day f g r where
  -- Existential `x` and `y`
  Day :: f x -> g y -> (x -> y -> r) -> Day f g r
```

### Monoid objects in monoidal categories

However, those are specifically the formulation of monads and applicatives as *monoid objects* (in a monoidal category).
Notably, that is the source of the “a monad is just a monoid in the [monoidal] category of endofunctors [under composition]” meme, where an applicative functor is “just” a monoid in the [monoidal] category of endofunctors *under Day convolution*.

:::{.Note box-name="Aside"}
I argue that it is important to include “under composition” in the description of the monad, because there are many monoidal products available on functors, and functor composition is a surprising choice, since it is not symmetric at all!

In fact, you can think of the Day convolution as a symmetrization of the composition tensor: there is a natural map `Day f g ~> Compose f g`{.haskell}, and since `Day`{.haskell} is symmetric, it also has a map `Day f g ~> Compose g f`{.haskell} via `Day g f ~> Day f g`{.haskell}.
:::

The monoid operation is an arrow
\(\operatorname{merge} : M \otimes M \to M\)
in the functor category: that is, a natural transformation.

So for monads it is `join :: Compose f f ~> f`{.haskell}, which is just `forall r. f (f r) -> f r`{.haskell}, and for applicatives it is `liftA2 :: Day f f ~> f`{.haskell}, which expands to `forall r x y. (x -> y -> r) -> f x -> f y -> f r`{.haskell}, at which point we can either take `x = (y -> r)`{.haskell} and apply `id`{.haskell} to get back `(<*>)`{.haskell}, or we can take `r = (x, y)`{.haskell} and apply `(,)`{.haskell} to get a `tupling`{.haskell} function below.

:::Note
Applicative functors can also be thought of as *lax monoidal functors* (between two monoidal categories), which come with an operation
\(\operatorname{tupling} : f(x) \otimes f(y) \to f (x \otimes y)\).
(In the case of applicative functors, both \({\otimes}\) are the product \({\times}\).)

As we just said, it comes from the fact that a universal choice for the `(x -> y -> r)`{.haskell} function is the tuple constructor: because that preserves information, any other choice factors through it.
:::

### Arrows

We can formulate them differently.
Instead of thinking of monads and applicatives as lax monoidal functors, we can think of them as *arrows*.

As a warmup, a free category can be expressed as

```haskell
data FreeCategory p i o where
  Id :: FreeCategory p y y
  -- Existential `y`
  Compose :: p x y -> FreeCategory p y z -> FreeCategory p x z
```

(This is an analogous structure to the humble cons list, since it has the non-recursive structure `p _ _`{.haskell} on the left and the recursion `FreeCategory p _ _`{.haskell} on the right.)

(Note that there's some trickery around categories that are also profunctors here [indeed, profunctors with strength], that we will sidestep by only working in categories based on functions.)

```haskell
newtype MonadArrow f i r = MonadArrow (i -> f r)
newtype FreeMonad f r = FreeMonad
  (FreeCategory (MonadArrow f) Unit r)

newtype ApplicativeArrow f i r = ApplicativeArrow (f (i -> r))
newtype FreeApplicative f r = FreeApplicative
  (FreeCategory (ApplicativeArrow f) Unit r)
```

The “monad arrow” here is also known as the Kleisli category of the monad `f`{.haskell}.

It turns out that the “applicative arrow” is explained as a [change of enriching category](https://ncatlab.org/nlab/show/change+of+enriching+category) (via `f`{.haskell} being a lax monoidal functor), even though we are only going from Set (which is itself a Set-enriched category) back to a different Set-enriched category (whose arrows are `f (i -> r)`{.haskell}).

So the free selective applicative?

```haskell
data ControlFlow f i r where
  Action :: (f (i -> r)) -> ControlFlow f i r
  Pure :: (i -> r) -> ControlFlow f i r
  CaseFlow :: CaseTree (ControlFlow f Unit) i r -> ControlFlow f i r
  Sequencing :: ControlFlow f i x -> ControlFlow f x r -> ControlFlow f i r
```

Notice that we can essentially inline `CaseTree`{.haskell} into this data type too:

```haskell
data ControlFlow f i r where
  Action :: (f (i -> r)) -> ControlFlow f i r
  Pure :: (i -> r) -> ControlFlow f i r
  Sequencing :: ControlFlow f i x -> ControlFlow f x r -> ControlFlow f i r
  Absurd :: (i -> Void) -> ControlFlow f i r
  CaseBranch ::
    (i -> Either x y) ->
    ControlFlow f x r ->
    ControlFlow f y r ->
    ControlFlow f i r
```

However, this is not incredibly useful: a lot of forms of static analysis really want to deal with case branches at once, so it would need to detect `CaseBranch`{.haskell} and gather all the branches up.

Note that even if `f`{.haskell} is the type of functor where you could statically analyze the values in it (e.g. `List`{.haskell} as opposed to `IO`{.haskell}), we are instantiating it with an unknown function type.

:::Details
I say *unknown* function type here because even if you could technically analyze the first action in `ControlFlow f i r`{.haskell} when `i`{.haskell} is finite, the fact that `Sequencing (Pure id) _`{.haskell} constructs another equivalent (equal?) `ControlFlow f i r`{.haskell} where you can no longer analyze the first action on that basis (since it is now hidden behind an existential) means that you *should not*.
:::

This hints to us that we can construct another type that consists of all the static information of `ControlFlow`{.haskell}, forgetting all of the functions (as they are unanalyzable) and thus all of the types:

```haskell
data FlowInfo f where
  Info :: f () -> FlowInfo f
  Pure :: FlowInfo f
  Sequencing :: FlowInfo f -> FlowInfo f -> FlowInfo f
  Absurd :: FlowInfo f
  CaseBranch :: FlowInfo f -> FlowInfo f -> FlowInfo f

data FlowInfo f
  = Info (f ())
  | Pure
  | Sequencing (FlowInfo f) (FlowInfo f)
  | Absurd
  | CaseBranch (FlowInfo f) (FlowInfo f)
```

Notice how this looks like an untyped AST for a program now!
We removed the existentials and the polymorphic recursion: it is the plainest of plain data types now.

In fact, you might spy what it has turned into: it is a free semiring, or something close to it.
(Specifically it should be a near semiring.)

```haskell
summarize :: NearSemiring m => (f () -> m) -> FlowInfo f -> m
summarize f2m (Info f) = f2m f
summarize _ Pure = one
summarize f2m (Sequencing l r) = summarize f2m l * summarize f2m r
summarize _ Absurd = zero
summarize f2m (CaseBranch l r) = summarize f2m l + summarize f2m r
```

:::Details
From the laws for a category, we know that `Sequencing`{.haskell} should be associative.
We also have that `Sequencing (Pure id) f ~ f ~ Sequencing f (Pure id)`{.haskell} should all be equivalent.
So once we forget the functions (by going from `ControlFlow f i r`{.haskell} to `FlowInfo f`{.haskell}), we need to treat _all_ `Pure :: FlowInfo f`{.haskell} as equivalent.^[From a computational point of view, this is because we allow arbitrary type shuffling, while still treating them as trivial _programs_ from the point of view of the computation that `f`{.haskell} encodes.]
So `Pure`{.haskell} is the identity for whatever operation we map `Sequencing`{.haskell} to.

Similarly we have that `CaseBranch`{.haskell} is associative, and its identity is `Absurd`{.haskell}.

We *also* should have that `CaseBranch`{.haskell} is commutative.
The order of cases should not matter, since only one will be taken – in the monadic interpretation, at least.^[We should be careful about making such strong determinations about what should and should not count, since that is what has led previous lines of reasoning about selective applicative functors astray.]

Finally, we have the matter of distributivity.
It turns out that we should only have one sided distributivity: `(x + y) * z = x * z + y * z`{.haskell}, which implies `0 * z = 0`{.haskell}.
(This is what makes it a near semiring instead of a semiring.)

The other side of distributivity is not easy to satisfy for programs, in particular.
The distributivity laws above work well for programs, but the other direction does not apply so well:

- `0 * z = 0`{.haskell} means that anything *after* absurdity does not matter (it is not going to get run!), while `z * 0 = 0`{.haskell} is suspect only because `z`{.haskell} may have side effects that run before it gets to absurdity, which could be important to keep track of (especially if error recovery is a possibility).^[The functor result does not matter, since it is coming from absurdity!]
- `(x + y) * z = x * z + y * z`{.haskell} says that control flow proceeds with the shared control flow after case branches, while `z * (x + y) = z * x + z * y`{.haskell} would require unbounded backtracking.

This makes sense, especially because we are encoding *determined* choice, where earlier results determine later control flow.
We should not be surprised if an arrow of time occurs in our equations!
:::

#### Arrow transformer

You might note that weʼve barely been using `f`{.haskell} here, just as a placeholder for a convenient arrow type.
This suggests that we could formulate `ControlFlow`{.haskell} as a arrow transformer, and require the right kind of arrow `p`{.haskell} (a strong profunctor and category, at least) to make `ControlFlowT p`{.haskell} behave as we want.

```haskell
data ControlFlowT p i r where
  Action :: p i r -> ControlFlow f i r
  Pure :: (i -> r) -> ControlFlow f i r
  Sequencing :: ControlFlow f i x -> ControlFlow f x r -> ControlFlow f i r
  Absurd :: (i -> Void) -> ControlFlow f i r
  CaseBranch ::
    (i -> Either x y) ->
    ControlFlow f x r ->
    ControlFlow f y r ->
    ControlFlow f i r
```

I have not thought about this much.

### Alternative

Finally it is time to address the elephant in the room: what does this have to do with `<|>`{.haskell} from `Alternative`{.haskell}?

As I mentioned in the overview, `(<|>) :: f x -> f x -> f x`{.haskell} (alternatively: `f x -> f y -> f (Either x y)`{.haskell}) is a combinator for nondeterministic choice, where the combinator itself does not give any information about which branch to take, it is completely up to the implementation.

The main examples of this are `LogicT`{.haskell} (a monad transformer that encodes nondeterminism, layering `List`{.haskell} semantics on top of another monad), and parser combinator transformers, the simplest of which would be a hypothetical `ParserT`{.haskell}.
You can think of the basic monads `Maybe`{.haskell}, `Either`{.haskell}, `List`{.haskell} as simplifications of these ideas.

```haskell
newtype ParserT m a =
  ParserT { unParserT :: String -> m (Either Bool (String, a)) }

instance Functor m => Alt (ParserT m) where
  ParserT l <|> ParserT r = ParserT \input -> do
    parsedL <- l input
    case parsedL of
      Right parsed ->
        pure parsed
      Nothing ->
        -- This encodes backtracking: restarting on the same input
        -- But usually parser combinators check that the input was
        -- not consumed at all, and use a combinator to enable
        -- backtracking in those cases.
        r input

--------------------

newtype LogicT m a =
  LogicT { unLogicT :: forall r. (a -> m r -> m r) -> m r -> m r }

instance Functor m => Alt (LogicT m) where
  LogicT l <|> LogicT r = LogicT \cons nil ->
    l cons (r cons nil)

observeAllT :: Applicative m => LogicT m a -> m [a]

observeManyT :: Monad m => Int -> LogicT m a -> m [a]
```

`ParserT`{.haskell} is the most important example of the usefulness of `<|>`{.haskell}, where even something as simple as taking different actions based on different characters of input is encoded using `<|>`{.haskell}.

`LogicT`{.haskell} is a monad transformer with pervasive backtracking, modeling nondeterminism of the kind “run every possible computation to find every possible result”, essentially.
It can also be lazily explored with [`observeManyT`{.haskell}](https://hackage-content.haskell.org/package/logict-0.8.2.0/docs/Control-Monad-Logic.html#v:observeManyT), to only obtain the first few results.

Still, the common theme is that to obtain even one successful result, backtracking deeply through the computation may be necessary.

Because they are both monads, they support: `f (Maybe a) -> f a`{.haskell}, via `(_ >>= maybe empty pure)`{.haskell}.
This operation makes sense in some applicatives, too, but it needs a specialized implementation.

## Syntax

To build intuition, lets talk about what the syntax for selective applicatives would look like.

Haskell and PureScript already have notation for monads: `do`{.haskell} notation, dubbed the “programmable semicolon”.
Haskell reuses `do`{.haskell} notation for applicatives, but PureScript uses dedicated `ado ... in`{.purescript} notation.
So I will use that for clarity.

The difference between the two notations is that each action in `do`{.haskell} notation gets access to the previous result, while `ado`{.purescript} notation restricts access to the variables bound by the actions, until the final result (denoted with `in`{.purescript}).
This is the difference between `f i -> (i -> f r) -> f r`{.haskell} and `f i -> f (i -> r) -> f r`{.haskell}.

```purescript
doexample ijk = do
  x <- action1 ijk
  y <- action2 ijk x
  z <- action3 ijk x y
  pure $ summarize ijk x y z

adoexample ijk = ado
  x <- action1 ijk
  y <- action2 ijk
  z <- action2 ijk
  in summarize ijk x y z
```

So how would it look like for selective applicatives?
It should allow a finite branching structure, while still respecting the boundary between actions and data.

```purescript
sdoexample ijk = sdo
  x <- action1 ijk
  y <- action2 ijk
  z <- case decision ijk x y of
    Case1 u ->
      m <- action3 ijk
      n <- action4 ijk
      in accessible ijk x y u m n
    Case2 v ->
      _ <- action3 ijk
      in v
    Case3 p q ->
      in p + q
  r <- action4 ijk
  in summarize ijk x y z r
```

The bound variables appear in the `case`{.purescript} scrutinee and in the `in`{.purescript} result, but still not in the actions.
And each branch of the case picks up in the same `sdo`{.kw} scope, so actions still do not get to see the action-bound variables `x`{.purescript}, `y`{.purescript}, or the case-bound variables `u`{.purescript}, `v`{.purescript}, `p`{.purescript}, or `q`{.purescript}.

The main issues with it are:

- The syntax looks bad. It can probably be refined, but it does not look great as an adaptation of `do`{.purescript} notation, especially since the branches of the `case`{.purescript} are not expressions but further statements in the `sdo`{.kw} block.
- Actually compiling it requires exposing the encoding of the case tree to the program, as we will see below once we can make that concrete.


## Conclusion


