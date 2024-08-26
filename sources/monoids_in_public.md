---
title: Monoids in Public
subtitle: Useful monoid structures in programming
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

## VDOM trees as monoids

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

## Your Requests Here

know any other cute liʼl monoids I should include?
