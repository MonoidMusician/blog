---
title: Redesigning `Coercible`{.purescript}
subtitle: Just use quantified constraints
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
date: 2021/12/12
---

[`Coercible`{.purescript}](https://pursuit.purescript.org/builtins/docs/Prim.Coerce#t:Coercible) redesign

- I want to get rid of “roles” as a hardcoded feature.
  I donʼt know of anyone proposing a solid theory of higher-order roles yet.
  But itʼs not necessary, in any case: itʼs much better to use quantified constraints.

  [tl;dr]{t=}:
  - roles now establish the canonical instance for a type constructor; the translation should be pretty obvious
  - all of these are symmetric, so that doesnʼt need to be baked into the solver (except for new abstract instances in scope)
  - `Coercible`{.purescript} then is also generated using transitivity and reflexivity and application: `Coercible f g => forall a. Coercible (f a) (g a)`{.purescript}
- Some notes on what I was trying in Haskell: https://gist.github.com/MonoidMusician/30813fb645abe0cef9cc5ec6efe16b10

-----

so I was thinking about what evidence you actually get from a role, within the system, and I think it's literally just quantified constraints, and this will basically give us higher order roles when we need it (especially for things like monad transformers)
if you declare a single-parameter functor to have a role, this is the evidence you get:

```haskell
type Phantom f = forall a b. Coercible (f a) (f b)
type Representational f = forall a b. Coercible a b => Coercible (f a) (f b)
type Nominal f = forall a b. a ~ b => Coercible (f a) (f b)
```

roles are used for setting up what coercible instances there are, but within the system, you cannot actually distinguish between a particular role and these quantified constraints, I believe

then the idea is that for representational types, the instances are like … the simplest thing

you can read it right off the datatype

for data `CoEnvT x m a = Single x | Higher (m a)`{.haskell} the instance is `(Coercible x y, Coercible (m a) (n b)) => Coercible (CoEnvT x m a) (CoEnvT y n b)`{.haskell}

you just coerce all the corresponding fields, and you don't have to reduce the Coercible constraints at all: you don't have to guess if `m`{.haskell} and `n`{.haskell} have to have representational or not

this might require the other thing I was saying where instances need to be bi-implications: it would be great if `Coercible (CoEnvT x m a) (CoEnvT y n b)`{.haskell} would also imply `Coercible (m a) (n b)`{.haskell} for example

things are a little trickier for recursive things: `data Free f a = Pure a | Wrap (f (Free f a))`{.haskell} mechanically would have instance `(Coercible a b, Coercible (f (Free f a)) (g (Free g b))) => Coercible (Free f a) (Free g b)`{.haskell}, but I think for representational `f`{.haskell} and `g`{.haskell} there might be problems with infinite instances … but note that you can reduce it to `(Coercible a b, (forall x y. Coercible x y => Coercible (f x) (g y))) => Coercible (Free f a) (Free g b)`{.haskell} to break the cycle

(this might not be a problem in Haskell, but I would definitely worry about it in PureScript)

this eliminates a weird thing where newtypes and data worked differently wrt coercibility: it was always the case that `newtype App f a = App (f a)`{.haskell} effectively has instance `Coercible (f a) (g b) => Coercible (App f a) (App g b)`{.haskell} by transitivity of Coercible; namely, unwrapping and rewrapping

so I would like that for all datatypes

and also helps out with the whole discussion around monad/functor coercing, yeah

- https://gitlab.haskell.org/ghc/ghc/-/issues/8177#note_257671
- http://oleg.fi/gists/posts/2019-07-31-fmap-coerce-coerce.html
- https://gitlab.haskell.org/ghc/ghc/-/wikis/roles2#join
- https://ryanglscott.github.io/2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad/
- https://github.com/purescript/documentation/blob/master/language/Roles.md

----

Coercible and polymorphic subsumption donʼt play well together?

```haskell
lmap :: forall p a b. (FunctorOf Op ((->) ~> (->)) p) => (a -> b) -> forall x. p b x -> p a x
lmap f = coerce (map @_ @_ @p (Op f))
```

```
    • Couldn't match representation of type: forall x1.
                                             p b x1 -> p a x1
                               with that of: p b x -> p a x
        arising from a use of ‘coerce’
```
