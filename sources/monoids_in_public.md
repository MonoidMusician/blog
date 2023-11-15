---
title: Monoids in Public
subtitle: Useful monoid structures in programming
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

## (V)DOM trees as monoids

Imagine you have your favorite VDOM model:

```{.haskell data-lang="PureScript"}
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

```{.haskell data-lang="PureScript"}
data VDOM
  = Text String
  | Node
    ElementType
    (Array Attributes)
    VDOM
  | Fragment
    (Array VDOM)
```

Bonus: the children of a `VDOM`{.haskell} do not have to be an array anymore, which makes constructing them slightly easier sometimes.
(An in other circumstances, you can just use `fold :: Array VDOM -> VDOM`{.haskell}.)

```{.haskell data-lang="PureScript"}
instance Monoid VDOM where
  mempty = Fragment []
instance Semigroup VDOM where
  append (Fragment []) vdom = vdom
  append vdom (Fragment []) = vdom
```

You can also add special behavior if you want to collapse `Text`{.haskell} nodes:

```{.haskell data-lang="PureScript"}
instance Semigroup VDOM where
  append (Fragment []) vdom = vdom
  append vdom (Fragment []) = vdom
  append (Fragment v1s) (Fragment v2s) = Fragment (v1s <> v2s)

  -- Handle text specially
  append (Text "") vdom = vdom
  append vdom (Text "") = vdom
  append (Text t1) (Text t2) = Text (t1 <> t2)

  -- Keep flattening, to keep it associative
  append v1 (Fragment v2s) = Fragment ([v1] <> v2s)
  append (Fragment v1s) v2 = Fragment (v1s <> [v2])

  -- Finally, if none of the above cases apply,
  -- we wrap it up in a two-element array:
  append v1 v2 = Fragment [v1, v2]
```

:::Note
With these new cases, the empty text node `Text ""` is _almost_ an identity, but the empty fragment `Fragment []` is still the identity of the monoid: you cannot have two identities to a monoid.
:::


## Join with separator monoid

I confess that this is also motivated by webdev: for the class attribute.

We do a similar trick: we detect the identity and handle it specially.^[Pedagogically I probably should have started with this example, but I donʼt really feel like re-working this post right now.]

```{.haskell data-lang="PureScript"}
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

```{.haskell data-lang="PureScript"}
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

Now you can do such fun stuff as:
```{.haskell data-lang="PureScript"}
joinWithComma :: Array String -> String
joinWithComma = unwrap <<< foldMap
  (JoinedWith :: String -> JoinWith ", " String)
```

And the real benefit of making a proper monoid structure is that it is now compositional.
You can just chuck it in a record and it does the right thing ^.^
