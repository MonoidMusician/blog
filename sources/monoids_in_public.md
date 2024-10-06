---
title: Monoids in Public
subtitle: Useful monoid structures in programming
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2023/11/15 – 2024/09/18
---

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
printing … *and* parsing? idk yet

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

## Your Requests Here

know any other cute liʼl monoids I should include?
