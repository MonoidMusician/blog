---
title: Monoids in Public
subtitle: Useful monoid structures in programming
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2023/11/15 – 2025/12/03
---

:::Note
It is a weird historical quirk that Haskell/PureScript call the symmetric operation `(<>) :: m -> m -> m`{.purescript} by the name of “append”.
Most of the world reserves append and prepend for the asymmetric operations shaped like `List t -> t -> List t`{.purescript} and `t -> List t -> t`{.purescript}, respectively, and call `(<>)`{.purescript} by the name of “concatenation” (literally meaning “[chaining together](https://en.wiktionary.org/wiki/concatenate)”).
That is pretty clearly a better convention, but I will still use the Haskell convention in this article.
:::

## VDOM trees as monoids
<p class="dated">2023/11/15</p>

Imagine you have your favorite VDOM model:

```purescript
data VDOM
  = Text String
  | Node
      ElementType
      (Array Attributes)
      (Array VDOM)
```

Now, weʼve done this before, itʼs pretty inconvenient to use.
It forces use to reify component boundaries in the DOM itself.
If you need to return something of type `VDOM`{.haskell}, but you produced several things, you need to wrap it in a `<div>`{.html} or `<span>`{.html} – but now you need to know which to choose!
Itʼs not great.

What if we made `VDOM`{.haskell} into a monoid?
That would solve lots of problems!

This is basically what React Fragments are, so we add a new constructor:

```purescript
data VDOM
  = Text String
  | Node
      ElementType
      (Array Attributes)
      VDOM
  | Fragment
      (Array VDOM)
```

The advantage of adding a new constructor is that we donʼt have to change so many types, particularly when constructing things.
We just need a new way to consume `Fragment`{.haskell} at the end of the process.

:::{.Note box-name="Side-Effect"}
The children of a `VDOM`{.haskell} do not have to be an array anymore, which makes constructing them slightly more convenient sometimes.
(And in other circumstances, you can just use `Fragment`{.haskell} or `fold :: Array VDOM -> VDOM`{.haskell}.)
:::

Now we can write an interesting monoid instance for this, which builds up a fragment but tries to remove nesting as it does so.

```purescript
instance Monoid VDOM where
  mempty = Fragment []
instance Semigroup VDOM where
  append (Fragment []) vdom = vdom
  append vdom (Fragment []) = vdom
  append (Fragment v1s) (Fragment v2s) = Fragment (v1s <> v2s)

  -- Keep flattening, to keep it associative
  append v1 (Fragment v2s) = Fragment ([v1] <> v2s)
  append (Fragment v1s) v2 = Fragment (v1s <> [v2])

  -- Finally, if none of the above cases apply,
  -- we wrap it up in a two-element array:
  append v1 v2 = Fragment [v1, v2]
```

<details class="Bonus">

<summary>Bonus</summary>

You can also add special behavior if you want to collapse `Text`{.haskell} nodes, but this starts to get a little ugly since it needs to look inside `Fragment`{.haskell} too:

```purescript
instance Semigroup VDOM where
  append (Fragment []) vdom = vdom
  append vdom (Fragment []) = vdom
  append (Fragment v1s) (Fragment v2s) = Fragment (v1s <> v2s)

  -- Handle text specially
  append (Text "") vdom = vdom
  append vdom (Text "") = vdom
  append (Text t1) (Text t2) = Text (t1 <> t2)

  -- We need to handle text at the edges of fragments too
  -- for associativity (I did not check all of the details)
  append (Fragment vs) (Text t2)
    | Just (Tuple vs' t1) <- stripTextEnd vs =
      Fragment (vs' <> [Text (t1 <> t2)])
  append (Text t1) (Fragment vs)
    | Just (Tuple t2 vs') <- stripTextStart vs =
      Fragment ([Text (t1 <> t2)] <> vs')
  append (Fragment v1s) (Fragment v2s)
    | Just (Tuple v1s' t1) <- stripTextEnd v1s
    , Just (Tuple t2 v2s') <- stripTextStart v2s =
      Fragment (v1s' <> [Text (t1 <> t2)] <> v2s')

  -- Keep flattening, to keep it associative
  append v1 (Fragment v2s) = Fragment ([v1] <> v2s)
  append (Fragment v1s) v2 = Fragment (v1s <> [v2])

  -- Finally, if none of the above cases apply,
  -- we wrap it up in a two-element array:
  append v1 v2 = Fragment [v1, v2]

stripTextStart :: Array VDOM -> Maybe (Tuple String (Array VDOM))
stripTextStart = Array.uncons >=> case _ of
  { head: Text t1, tail: vs } -> Just (Tuple t1 vs)
  _ -> Nothing

stripTextEnd :: Array VDOM -> Maybe (Tuple (Array VDOM) String)
stripTextEnd = Array.unsnoc >=> case _ of
  { init: vs, last: Text t2 } -> Just (Tuple vs t2)
  _ -> Nothing
```

:::Note
With these additional cases, the empty text node `Text ""`{.haskell} is _almost_ an identity, but the empty fragment `Fragment []`{.haskell} is still the identity of the monoid: [you cannot have two identities in a monoid](https://proofwiki.org/wiki/Identity_is_Unique).
And I think thereʼs a specific reason why you want `Fragment []`{.haskell} to be the identity, but I havenʼt actually worked through the details to fully justify it.
:::

</details>

## Join with separator monoid
<p class="dated">2023/11/15</p>

I confess that this is also motivated by webdev: for the class attribute, which is a space-separated list of class names.

We do a similar trick: we detect the identity and handle it specially.^[Pedagogically I probably should have started with this example, but I donʼt really feel like re-working this post right now.]

```purescript
newtype ClassName = ClassName String

instance Monoid ClassName where
  mempty = ClassName ""

instance Semigroup ClassName where
  append (ClassName "") e = e
  append e (ClassName "") = e
  -- Join two non-empty class names with a space
  append (ClassName c1) (ClassName c2) =
    ClassName (c1 <> " " <> c2)
```

Unfortunately, itʼs a bit of work to generalize this to any separator.
Youʼd want to track the separator at the type level to ensure it is the same across the whole monoid.
Without dependent types, you have to tie it to some typeclass, like some kind of singleton reflection.

For the case of strings in PureScript, we can use symbols (type-level strings):

```purescript
newtype JoinWith (s :: Symbol) = JoinedWith String

instance IsSymbol s => Monoid (JoinWith s) where
  mempty = JoinedWith ""

instance IsSymbol s => Semigroup (JoinWith s) where
  append (JoinedWith "") e = e
  append e (JoinedWith "") = e
  append (JoinedWith s1) (JoinedWith s2) =
    JoinedWith
      (s1 <> reflectSymbol (Proxy :: Proxy s) <> s2)
```

You could also easily adapt this for any type that you can construct with strings, like a [pretty printer](https://pursuit.purescript.org/packages/purescript-dodo-printer/2.2.1/docs/Dodo.Internal#t:Doc).
(You also need to be able to check whether it [is empty](https://pursuit.purescript.org/packages/purescript-dodo-printer/2.2.1/docs/Dodo.Internal#v:isEmpty), of course.)

Now you can do such fun stuff as:
```purescript
joinWithComma :: Array String -> String
joinWithComma = unwrap <<< foldMap
  (JoinedWith :: String -> JoinWith ", " String)
```

And the real benefit of making a proper monoid structure is that it is now compositional.
You can just chuck it in a record and it does the right thing \^.\^

## Precedence in pretty printers
<p class="dated">2024/05/01</p>

:::{.centered}
*Originally posted at <https://tech.lgbt/@monoidmusician/112368939004740994>*
:::

This semiring is an amalgamation of a few concepts: a unification monoid, the [`Last`{.purescript}](https://pursuit.purescript.org/packages/purescript-maybe/6.0.0/docs/Data.Maybe.Last#t:Last) monoid, and some other stuff.

My goal is to capture precedence during the pretty-printing phase of my printer–parser–pretty-printer framework.
It really *really* needs to be compositional and to handle arbitrary structures, not just the usual binary operators.

The idea is that the applicative `<*>`{.purescript} combinators of the parser will correspond to multiplication `*`{.purescript} of precedence, and the alternative `<|>`{.purescript} combinators of the parser will correspond to addition of precedence.

During printing, when the precedence combines in certain ways, it will automatically insert parentheses (or some other method of delineating precedence, obviously i am not going to hardcode it).

The

```purescript
Prec 1 + Prec 1 * Prec 2
  = Prec 1 + Prec 2
  = NoPrec (since 1 /= 2)

Prec 1 * (NoPrec + Prec 2)
  = ??? no good answer …
```

------------------------------------------------------------------------

it is a similar problem as follow sets: what do you do with empty
alternatives? ... just not recursive (no need to peek inside
nonterminals)

------------------------------------------------------------------------

well, this is algebra, so we can just invent an answer `MTPrec`{.purescript} ("empty
or [known] precedence") and see if it still works out:

```purescript
NoPrec + Prec 2
  = MTPrec 2

Prec 1 * EmptyOrPrec 2
  = NoPrec (since 1 /= 2)
```

I guess I also need a `DiffPrec`{.purescript}  ...

hmm I wrote an impl, best to throw it into QuickCheck at this point, I
think

------------------------------------------------------------------------

QuickCheck is happy! I will just have to consider tomorrow whether this
actually buys me anything…

specifically I want it to help me delineate precedence for pretty
printing&nbsp;… *and* parsing? idk yet

the specific idea is that it will explain why reduction rules can be
assigned precedence in Happy (this is necessary for the implicit
adjuxtaposition operator for function application), and how it defaults
to that of the last token

------------------------------------------------------------------------

```purescript
data Prec v
  = NoPrec
  | AnyPrec
  | DiffPrec
  | Prec v
  | MTPrec v

instance semiringPrec :: Eq v => Semiring (Prec v) where
  zero = AnyPrec
  one = NoPrec

  add DiffPrec _ = DiffPrec
  add _ DiffPrec = DiffPrec
  add AnyPrec v = v
  add v AnyPrec = v
  add NoPrec (Prec v) = MTPrec v
  add (Prec v) NoPrec = MTPrec v
  add NoPrec (MTPrec v) = MTPrec v
  add (MTPrec v) NoPrec = MTPrec v
  add (Prec v1) (Prec v2)
    | v1 == v2 = Prec v1
    | otherwise = DiffPrec
  add (Prec v1) (MTPrec v2)
    | v1 == v2 = MTPrec v1
    | otherwise = DiffPrec
  add (MTPrec v1) (Prec v2)
    | v1 == v2 = MTPrec v1
    | otherwise = DiffPrec

  mul AnyPrec _ = AnyPrec
  mul _ AnyPrec = AnyPrec
  mul NoPrec v = v
  mul v NoPrec = v
  mul _ DiffPrec = DiffPrec
  mul DiffPrec (MTPrec _) = DiffPrec
  mul (Prec v1) (MTPrec v2)
    | v1 == v2 = Prec v2
    | otherwise = DiffPrec
  mul (MTPrec v1) (MTPrec v2)
    | v1 == v2 = MTPrec v1
    | otherwise = DiffPrec
  mul _ (Prec v) = Prec v
```

------------------------------------------------------------------------

shout-out to quickcheck-laws

<https://github.com/purescript-contrib/purescript-quickcheck-laws/blob/v7.0.0/src/Test/QuickCheck/Laws/Data/Semiring.purs>

------------------------------------------------------------------------

I think the upshot is that parsing rules have a simple precedence
behavior: just use the Last monoid for each rule, since the alternative
rules are already in a list structure

pretty printing will require this new semiring, since it does not have
alternatives in the same structure, it is just a function

so when you transition from “known precedence” to “unknown precedence”,
is where it adds the opportunity to parenthesize

ughhh how does this interact with CST nowwww

------------------------------------------------------------------------

CST shouldnʼt need precedence, right, thatʼs kind of the point of it,
that it just preserves parentheses that existed in the input


## Top-Down Traversals
<p class="dated">2024/09/18</p>

Recently I faced a problem: how do I get more control out of a traversal in PureScript?

There are two problems here:

1. Collapsing a tree to a flat, one-dimensional monoid structure is too simplified
2. PureScript is strict, so short-circuiting is more complicated

In particular, I want to do a single top-down traversal and estimate the complexity of evaluating an Erlang expression.
The hardest part is the complexity of literals: a statically-allocated literal is trivial to evaluate, but all children of a literal must be literals.
Additionally, we need to not count the complexity of the bodies of lambdas: they are just a single allocation too.

As a refresher on the kind of traversal I am talking about, I had the existing signature

```purescript
visit ::
  forall m.
    Monoid m =>
  -- A callback to value a single node
  (ErlExpr -> m) ->
  -- The aggregate of the node and its children
  (ErlExpr -> m)
```

My first iteration was returning `Tuple Boolean m`{.purescript}, but this suffered from being confusing (does `true`{.purescript} mean to keep the childrenʼs result or skip it?), and it did not solve the first point.

Eventually I landed on this type:

```purescript
data Visit m
  -- Skip evaluating the children
  = ShortCircuit m
  -- Evaluate the children and append this value
  | Append m
  -- Customize the result of evaluating the children
  | Continue (m -> m)
```

Note that `Append`{.purescript} is just for convenience, it is equivalent to `Continue <<< append`{.purescript} (where [`append :: m -> m -> m`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Data.Semigroup#v:append) is the monoid operation, also written `(<>)`{.purescript}).

`ShortCircuit`{.purescript} also would not be necessary, if the monoid operation was lazy.
It would be equivalent to `Continue <<< const`{.purescript}.
But alas, PureScript is strict and its Prelude only includes strict append, and itʼs not really worth having a lazy append.

No matter, it is pretty simple to case on `Visit`{.purescript}.

```purescript
-- | Traverse with the ability to short circuit
-- | and alter results of child traversals
visit' ::
  forall m.
    Monoid m =>
  (ErlExpr -> Visit m) ->
  (ErlExpr -> m)
visit' f = go
  where
  go e = case f e of
    -- Avoid computing `children e`
    ShortCircuit m -> m
    -- Just append, like normal `visit`
    Append m -> m <> children e
    -- Allow it to manipulate `children e`
    -- however it wants to based on `e`
    Continue mm -> mm (children e)

  -- Recurse into the children
  children = case _ of
    -- ^ `case _ of` is an anonymous        --
    -- argument, equal to `\e -> case e of` --
    Literal _ -> mempty
    Var _ _ -> mempty
    BinOp _ e1 e2 -> go e1 <> go e2
    UnaryOp _ e1 -> go e1
    BinaryAppend e1 e2 -> go e1 <> go e2
    List es -> foldMap go es
    ...
```

This structure of `Monoid m => m -> m`{.purescript} is great for making the most out of the tree traversal: it allows you to delimit the children and operate on them as a group, which was not possible before.

Itʼs also related to the [Endomorphism Semiring](https://en.wikipedia.org/wiki/Endomorphism_ring), which is [very cool](Eudoxus.html).
One of the algebraic structures of all time.

Anyways, now we can do what I came here to do: estimate the runtime complexity of an Erlang expression.

We start by crafting a monoid `Complexity`{.purescript} to keep track of the information about the cumulative complexity of an expression, including whether it is a group of literals.
It is based on `Additive Int`{.purescript} (the integers as a monoid under addition), with the twist of the Boolean distinction between `Lit`{.purescript} and `Complex`{.purescript} (preferring `Complex`{.purescript} of course, which makes `Lit 0`{.purescript} the identity).

Note that literals still have the `Int`{.purescript} cost because multiple literals side-by-side have a cumulative cost (this is what is modeled in `Semigroup Complexity`{.purescript}), which is canceled out in `groupOfLiterals`{.purescript} when it forms part of a larger literal (only if all children are literals).

That is, `{ 3 => 4, 5 => [6,7,8] }`{.erl} is a literal, but `{ x => fun f/1, y => 2 }`{.erl} is not a literal (only the keys `x`{.erl} and `y`{.erl} and the value `2`{.erl} is a literal there).

```purescript
data Complexity
  = Lit Int
  | Complex Int
instance semigroupComplexity :: Semigroup Complexity where
  append (Complex i) (Complex j) = Complex (i + j)
  append (Lit i) (Complex j) = Complex (i + j)
  append (Complex i) (Lit j) = Complex (i + j)
  append (Lit i) (Lit j) = Lit (i + j)
instance monoidComplexity :: Monoid Complexity where
  mempty = Lit 0

unComplexity :: Complexity -> Int
unComplexity (Lit _) = 1
unComplexity (Complex i) = i

-- | Estimate the runtime complexity of an expression.
-- | Used for determining when to memoize a function.
estimatedComplexity :: ErlExpr -> Int
estimatedComplexity = unComplexity <<< visit' case _ of
  -- Do not recurse into closures
  Fun _ _ -> ShortCircuit (Complex 1)
  -- Yeah, `fun f/1` is an allocation ...
  FunName _ _ _ -> ShortCircuit (Complex 1)
  -- A curried call costs one
  FunCall Nothing (FunCall _ _ _) _ -> Append (Complex 1)
  -- Base calls cost more since they might do more work
  -- (Note: atoms count during recursion, so unqualitifed >= 3,
  -- and qualified calls >= 4, plus arguments of course)
  FunCall _ _ _ -> Append (Complex 2)
  -- Literals are cheap
  Literal _ -> ShortCircuit (Lit 1)
  -- Literal constructors are cheap if
  -- all of their children are literals
  List _ -> groupOfLiterals
  Tupled _ -> groupOfLiterals
  Map _ -> groupOfLiterals
  Record _ -> groupOfLiterals
  -- Everything else costs 1 plus its children
  _ -> Append (Complex 1)
  where
  groupOfLiterals :: Visit Complexity
  groupOfLiterals = Continue case _ of
    Lit _ -> Lit 1
    Complex i -> Complex (i + 1)
```

- https://github.com/id3as/purescript-backend-erl/blob/703361c61b5848de1b8e4cb894cc8f2307618d4e/src/PureScript/Backend/Erl/Syntax.purs#L253-L261
- https://github.com/id3as/purescript-backend-erl/blob/703361c61b5848de1b8e4cb894cc8f2307618d4e/src/PureScript/Backend/Erl/Syntax.purs#L310-L352

## Monoids (and Applicatives) on Lists
<p class="dated">2025/12/03</p>

I think it is good to recognize the diversity of useful instances we can have for even the familiar [`List`{.purescript}](https://pursuit.purescript.org/packages/purescript-lists/7.0.0/docs/Data.List.Types#t:List) functor (and its isomorphic types: arrays, vectors, whatever you want to call things).
Here I have five instances that I have come across – weʼll be talking about [`Semigroup`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/6.0.2/docs/Data.Semigroup#t:Semigroup)s and [`Monoid`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/6.0.2/docs/Data.Monoid#t:Monoid)s, but also the analogous [`Apply`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/6.0.2/docs/Control.Apply#t:Apply) and [`Applicative`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/6.0.2/docs/Control.Applicative#t:Applicative) structures for functors, and [`Align`{.purescript}](https://pursuit.purescript.org/packages/purescript-these/6.0.0/docs/Data.Align#t:Align) too.

### Free Monoid

The free monoid instance is *good*, it is the default instance for lists and arrays for good reason, you want to be able to concatenate them easily…

```purescript
data List t
  = Nil
  | Cons t (List t)
instance Monoid (List t) where
  mempty = Nil
instance Semigroup (List t) where
  append Nil rs = rs
  append (Cons l ls) rs = Cons l (append ls rs)

derive instance Functor List
derive instance Foldable List

type FreeMonoid = List
```

:::Bonus
The `Foldable`{.purescript} instance is interesting: [`foldMap`{.purescript}](https://pursuit.purescript.org/packages/purescript-foldable-traversable/6.0.0/docs/Data.Foldable#v:foldMap) gives the universal action of `List`{.purescript} as a free monoid: you can coherently interpret it as any other monoid, in a way that is compatible with `Monoid (List t)`{.purescript}.
The other structures weʼll talk about can also have `Foldable`{.purescript} instances, but they wonʼt be compatible with the `Monoid`{.purescript} instance.
:::

… but I think it makes us overlook lots of other monoid instances!

### (Short) Zip List

So, okay, the next instance that comes up is the _zip list_ instance.
The structure we should be talking about here is `Applicative`{.purescript}/`Apply`{.purescript} (which is `Applicative`{.purescript} without the identity `pure :: forall t. t -> f t`{.purescript}, only having `(<*>)`{.purescript}).
We can lift semigroups through apply, and monoids through applicatives, via `append = lift2 append`{.purescript} and `mempty = pure mempty`{.purescript}.

The zip list is *not* a monoid when presented as a strict datatype (least fixed point):
only when presented as a lazy datatype does it become a greatest fixed point, allowing you to implement the identity, which needs to be an infinite ziplist of `mempty`{.purescript}.

```purescript
-- A finite / strict zip list ...
data FiniteZipList t
  = FiniteZipNil
  | FiniteZipCons t (FiniteZipList t)
derive instance Functor FiniteZipList

-- ... is only a semigroup
instance Semigroup t => Semigroup (FiniteZipList t) where
  append = lift2 (append :: t -> t -> t)
instance Apply FiniteZipList where
  -- Nil is now an annihilator, not the identity
  apply FiniteZipNil _ = FiniteZipNil
  apply _ FiniteZipNil = FiniteZipNil
  apply (FiniteZipCons l ls) (FiniteZipCons r rs) =
    FiniteZipCons (l $ r) (apply ls rs)

-- *Potentially* infinite zip list, thanks to laziness
data LazyZipList t
  = LazyZipNil
  | LazyZipCons t (Lazy (LazyZipCons t))
derive instance Functor LazyZipList

instance Semigroup t => Semigroup (LazyZipList t) where
  append LazyZipNil _ = LazyZipNil
  append _ LazyZipNil = LazyZipNil
  append (LazyZipCons l ls) (LazyZipCons r rs) =
    LazyZipCons (l <> r) (append ls rs)
      -- this uses `Monoid m => Monoid (Lazy m)`,
      -- which forces both sides when forced
      -- (since `Monoid` is strict, we cannot do any
      -- better in PureScript)
instance Apply (LazyZipList t) where
  apply LazyZipNil _ = LazyZipNil
  apply _ LazyZipNil = LazyZipNil
  apply (LazyZipCons l ls) (LazyZipCons r rs) =
    LazyZipCons (l $ r) (lift2 apply ls rs)
      -- here we have to lift it through `Lazy` manually

-- Produce an infinite list of the same value
instance Applicative (LazyZipList t) where
  pure t = FiniteZipCons t
    -- We can recurse only under `defer`, due to strictness
    $ defer $ \_ -> pure t
-- Giving us our monoid identity too
instance Monoid t => Monoid (LazyZipList t) where
  mempty = pure (mempty :: t)
```

This is intimately related to the fact that `Map k`{.purescript} only comes with an `Apply`{.purescript} instance, no `Applicative`{.purescript}: `Apply (Map Natural)`{.purescript} could be reimplemented via `Apply (Compose FiniteZipList Maybe)`{.purescript} and also suffers from the need for an infinite identity.
The `Monoid (Map Natural)`{.purescript} instance uses a different combination pattern which _does_ have an identity in the empty `Map`{.purescript}, which we will talk about shortly.

### Cartesian Product

While we are talking about `Applicative`{.purescript}s, `List`{.purescript} already comes with an `Applicative`{.purescript} instance: the cartesian product!
(This `Applicative`{.purescript} instance extends to a `Monad`{.purescript}, unlike the `ZipList`{.purescript} one.)

We already have an instance for this: it is [`App List m`{.purescript}](https://pursuit.purescript.org/packages/purescript-functors/5.0.0/docs/Data.Functor.App), which has the semigroup and monoid instances given by lifting:

```purescript
type CartesianListMonoid = App List

instance semigroupApp :: (Apply f, Semigroup a) => Semigroup (App f a) where
  append (App fa1) (App fa2) = App (lift2 append fa1 fa2)

instance monoidApp :: (Applicative f, Monoid a) => Monoid (App f a) where
  mempty = App (pure mempty)
```

To be honest though, this specific monoid instance `Monoid m => CartesianListMonoid m`{.purescript} **rarely** comes up in practice.
It is vaguely related to nondeterminism / probability monads, you could use it to tabulate tables I guess, but most of the time you either want to work with actual data using `<*>`{.purescript}, or you want to preserve the structure of the indices (separate rows/columns) that is lost when taking the Cartesian product.

### _Long_ Zip List

Alright, we had the (short) zip list, but what about a looong zip list??

The previous zip list operated by truncating to the shorter list, but if we are just doing a monoid combine, why not take the other available side like `Maybe`{.purescript} does?
And in a twist of duality, the _long_ zip list has an identity of the empty list again, unlike the infinite list we needed for the identity of the short zip list.

This is related to the analogy between `Apply (Map Natural)`{.purescript} and `Apply (Compose FiniteZipList Maybe)`{.purescript} that we talked about above: itʼs kind of the other side of it, seeing `Monoid (Map Natural m)`{.purescript} as operating on contiguous elements in `Monoid (LongZip m)`{.purescript} (i.e. some of the `Maybe`{.purescript}ness has been removed).

```purescript
data LongZip t
  = ShortLong
  | LongGoing t (LongZip t)

instance Semigroup t => Semigroup (LongZip t) where
  append ShortLong rs = rs
  append ls ShortLong = ls
  -- Notice that this case is the same as `ZipList`!
  append (LongGoing l ls) (LongGoing r rs) =
    LongGoing (l <> r) (ls <> rs)
instance Semigroup t => Monoid (LongZip t) where
  mempty = ShortLong
```

The tragedy here is that there is not an `Apply`{.purescript} instance to induce this `Semigroup`{.purescript}: since the operands are of different types, you cannot produce `LongGoing`{.purescript} once you see `ShortLong`{.purescript} on either side of `<*>`{.purescript}.

:::Bonus
The resolution here is to consider it not as a lax monoidal functor from monoidal categories of `Tuple`{.purescript} to `Tuple`{.purescript}, but from [`These`{.purescript}](https://pursuit.purescript.org/packages/purescript-these/6.0.0/docs/Data.These#t:These) to `Tuple`{.purescript}.

This is given by the [`Align`{.purescript}](https://pursuit.purescript.org/packages/purescript-these/6.0.0/docs/Data.Align#t:Align) class, which gives you `curry aligned :: Tuple (f x) (f y) -> f (These x y)`{.purescript} instead of `curry (lift2 Tuple) :: Tuple (f x) (f y) -> f (Tuple x y)`{.purescript}.

(Yes, the source tensor `These`{.purescript} appears in the function *output*, cuz it is inside the `f`{.purescript} there.)

Once you get to `These m m`{.purescript} for a `Monoid m`{.purescript}, the natural way to collapse it into `m`{.purescript} is given by
```purescript
collapse :: These m m -> m
collapse (This l) = l
collapse (That r) = r
collapse (Both l r) = l <> r
```

So we have re-explained how `Semigroup (LongZip t)`{.purescript} arises out of a lax monoidal functor, just for a different shape of lax monoidal functor.
:::

*Ironically*, I came across this instance in a paper about applicative functors!

Jeremy Gibbons [et al.]{t=} used this shape of appending operation in [Phases in Software Architecture](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/phases.pdf) to allow for arbitrarily many phases of applicatives, where phases are added as needed.
Their `Phases`{.haskell} is a lifted version of `LongZip`{.purescript}, operating via the Day convolution, but using the same shape of appending.

```haskell
data Phases m a where
  Pure :: a → Phases m a
  Link :: (a → b → c) → m a → Phases m b → Phases m c

instance Applicative m ⇒ Applicative (Phases m) where
  pure x = Pure x
  Pure f <*> xs = fmap f xs
  fs <*> Pure x = fmap (λf → f x) fs
  Link f xs ys <*> Link g zs ws = Link h (xs ⊗ zs) (ys ⊗ ws)
  where h (x, z) (y, w) = (f x y) (g z w)
```

They could have used something shaped like `Map k`{.haskell} with any orderable key type, but encoding that via the Day convolution sounds like a pain.
Phil Freeman used row types to make a type safe tensoring of arbitrary functors in [`purescript-smash`](https://pursuit.purescript.org/packages/purescript-smash/3.0.0), though the implementation of the safe API uses unsafe functions.
(This was intended for [Comonads as Spaces](https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html), not for applicatives, though the Day convolution is a useful construction for both.)

### Overlapping Append

Finally, we have a monoid that popped up in something I was coding.
I will present the monoid first (it is actually self-contained), and then explain how it came about later.

This monoid acts like our standard free-monoid concatenation, except for the fact that the tail of the first list gets merged with the head of the second list.
That is, they are concatenated with one element of overlap, while the remaining elements still spill out to the left and right as normal.

```purescript
-- A monoid (with suggestive names) that overlaps elements by one when appending
data MatchTrack m
  = NoMatch
  | MatchTrack m (MatchTrack m)

-- First we implement `Monoid (MatchTrack m)`
instance Monoid m => Monoid (MatchTrack m) where
  mempty = NoMatch
-- This appends the lists together with *one* element of overlap
instance Semigroup m => Semigroup (MatchTrack m) where
  append NoMatch r = r
  append l NoMatch = l
  append (MatchTrack trySome NoMatch) (MatchTrack more remaining) =
    MatchTrack (trySome <> more :: m) remaining
  append (MatchTrack priority trailing) r =
    MatchTrack priority (trailing <> r :: MatchTrack m)
```

We can actually factor the identity `NoMatch`{.purescript} out of this, recovering `MatchTrack m`{.purescript} as `Maybe (Overlapping m)`{.purescript}.

```purescript
data Overlapping m
  = Overtail m
  | Overlap m (Overlapping m)

instance Semigroup m => Semigroup (Overlapping m) where
  -- Two singletons append to a singleton
  append (Overtail l) (Overtail r) = Overtail (l <> r)
  -- Overlap the tail of one with the head of the next
  append (Overtail l) (Overlap r rs) = Overlap (l <> r) rs
  -- Plop the head of the first in front of appending the rest
  append (Overlap l ls) rs = Overlap l (ls <> rs)

-- This _inherits_ an identity
instance Monoid m => Monoid (Overlapping m) where
  mempty = Overtail mempty
```

Iʼm sure that many more patterns of overlapping are possible: choosing any fixed amount of overlap can work as well (“overlap up to two elements of lists”), but maybe the lists themselves can specify how much overlap they want?

<details class="Details">

<summary>More overlap</summary>

This seems to work out alright?
Passes QuickCheck

I think it only generalizes the overlap of one element, not the overlap of always two elements, though.

```purescript
type Overlap = List
data CustomOverlap t
  = CustomOverlap
    { overlapFront :: Overlap t
    , inner :: List t
    , overlapRear :: Overlap t
    }
  | JustOverlap (List t)

forget :: forall t. CustomOverlap t -> List t
forget (JustOverlap items) = items
forget (CustomOverlap sections) =
  sections.overlapFront <> sections.inner <> sections.overlapRear

instance Semigroup t => Semigroup (CustomOverlap t) where
  -- We need to handle empty lists explicitly,
  -- otherwise they do not form the identity
  append (JustOverlap Nil) rs = rs
  append ls (JustOverlap Nil) = ls
  append (JustOverlap ls) (JustOverlap rs) =
    case compare (length ls) (length rs) of
      -- If they line up exactly, we can merge them exactly
      EQ -> JustOverlap (zipWith (<>) ls rs)
      -- Otherwise we merge their overlap and the longer one hangs out
      LT -> CustomOverlap
        { overlapFront: zipWith (<>) ls (take (length ls) rs)
        , inner: Nil
        , overlapRear: drop (length ls) rs
        }
      GT -> CustomOverlap
        { overlapFront: dropEnd (length rs) ls
        , inner: Nil
        , overlapRear: zipWith (<>) (takeEnd (length rs) ls) rs
        }
  append (JustOverlap overlapRear) rs =
    append (CustomOverlap { overlapFront: Nil, inner: Nil, overlapRear }) rs
  append ls (JustOverlap overlapFront) =
    append ls (CustomOverlap { overlapFront, inner: Nil, overlapRear: Nil })
  append (CustomOverlap ls) (CustomOverlap rs) =
    CustomOverlap
      { overlapFront: ls.overlapFront
      , inner: ls.inner <> forget (
          JustOverlap ls.overlapRear <> JustOverlap rs.overlapFront
        ) <> rs.inner
      , overlapRear: rs.overlapRear
      }
instance Semigroup t => Monoid (CustomOverlap t) where
  mempty = JustOverlap mempty
```

</details>

#### How it came about

I had an idea to capture Parsec-style parser combinators (“monadic” or “recursive descent” style) in a data structure reminiscent of the [Comonads as Spaces](https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html) method.
Where the (almost-)comonad will encode the parser as an infinite lazy data structure, and the method of “walking around the space” is given by feeding in tokens to process and looking them up in the structure, one layer at a time.

You can view the full code here, with the monoid of interest highlighted: https://gist.github.com/MonoidMusician/b8ce75f012a4008f7904cde941a7fcba#file-parsemachine-hs-L235-L241

One of the essential ingredients is also a kind of phasing, like the previous section on phases of programs via applicative functors.
We want to capture all of the parsing of a token in a single layer, but the parser is allowed to backtrack, and the parser is allowed to have fallback behavior other than failure, too.

So we have a `Map tok _path`{.haskell} of the tokens we know we can handle, along with possibly a plain fallback `_path`{.haskell}, and then we have a *list* of all of this:
a data type like `[(Map tok _path, Maybe _path)]`{.haskell} but with a funky funky monoid instance (used when you call `<|>`{.haskell} to combine two parsers).

*Technically* we could get by without `MatchTrack`{.haskell}, but that would mean that we are always backtracking, which changes the semantics of the parser, hurts efficiency, and means that the `Map tok`{.haskell} is less accurate account of the tokens available *right then and there*.
(Indeed, there are combinators like `tryAlt`{.haskell} and `tryBind`{.haskell} that avoid an `Ord`{.haskell} constraint for the `Map`{.haskell} index: they simply do not merge maps at all.)

Like, the cool thing about having `Map tok`{.haskell} living in our parser is that that statically resolves conflicts *while the parser is being built* (lazily), in the cases where no backtracking is allowed.
So we want to keep that as much as possible.

So the overlap comes from the fact that in `(ls <> rs) :: MatchTrack m`{.haskell}, the *tail element* of `ls`{.haskell} (namely, `l`{.haskell} in `MatchTrack l NoMatch`{.haskell}) is the element that does not have any backtracking (yet), so it gets merged with the head of `rs`{.haskell}, where `MatchTrack l ls@(MatchTrack _ _)`{.haskell} marks an instance of backtracking between `l`{.haskell} and `ls`{.haskell}.

:::{.Details box-name="Digression"}
This is a sign that `Maybe (Overlapping m)`{.haskell} is a better datatype for explaining this behavior after all, but `NoMatch`{.haskell} has some semantic significance (it marks an “expected EOF”), and the datatypes were already wildly complicated, so I kept it as-is.

In terms of `Overlapping m`{.haskell}, the explanation is that `Overtail l`{.haskell} marks the tail that has no backtracking applied, and `Overlap l ls`{.haskell} is the constructor that introduces backtracking from `l`{.haskell} onwards to `ls`{.haskell}.
:::

Interestingly, a similar idea of staging as the applicative paper shows up in the parser too:
the parser *monad* is staged using `Compose`{.haskell} (that is, with a fixed order, instead of the commutative order of `Day`{.haskell}), since the result of parsing a token is of type `ParseMachine ix tok (ParseMachine ix tok r)`{.haskell}, to allow for committing to a parse locally.
Controlling backtracking like this helps constrain memory usage during parsing.

## Your Requests Here

thatʼs all I have for now!
know any other cute liʼl monoids I should include?
