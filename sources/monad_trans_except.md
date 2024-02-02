---
title: "`ExceptT` vis-a-vis `StateT`"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

When dealing with monad transformers, how do you want to stack `ExceptT`{.haskell} and `StateT`{.haskell}?

:::{.Key_Idea box-name="tl;dr"}
The [tl;dr]{t=} is that you probably want `StateT`{.haskell} on the outside and `ExceptT`{.haskell} on the inside.
This is the one that has good backtracking!
:::

I assume you know enough about monad transformers for this question to make sense.
This discussion can also be transferred to other ways of combining monads, like algebraic effects, where the order of handlers is analogous to the order of monad transformers.

Quick refresher on `ExceptT`{.haskell} and `StateT`{.haskell}:

```haskell
ExceptT e m a = m (Either e a)

StateT s m a = s -> m (a, s)

-- ExceptT e (StateT s) m a
EST e s m a = s -> m (Either e a, s)
-- StateT s (ExceptT e) m a
SET s e m a = s -> m (Either e (a, s))
```

As you can see, these two monad stacks differ in what happens on failure: in `EST`{.haskell} you still get a new state `s`{.haskell} out of the computation if you get a `Left`{.haskell}, but in `SET`{.haskell} you only get a new state if it succeeds with `Right`{.haskell}.

What does this mean for the `Monad`{.haskell} instances?
Well the second one, `SET`{.haskell}, enforces rolling back state when you are backtracking.
The first one, `EST`{.haskell}, keeps the state around after failure.

Some observations:

1. The behavior of `EST`{.haskell} is actually strictly more powerful: around any specific error recovery, you can always capture the state with [`get`{.haskell}](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Class.html#v:get) and then restore it with [`put`{.haskell}](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Class.html#v:put) when you need to backtrack.
  And any failed computations have access to the default unmodified state to return; thatʼs not a mystery.
1. This kind of “defensive” approach also works for `SET`{.haskell}, so it doesnʼt lock you into one specific option actually.
1. However, it doesnʼt work for generic APIs phrased in terms of [`MonadError`{.haskell}](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Except.html#t:MonadError), which wonʼt be able to call `get`{.haskell} and `put`{.haskell} for backtracking since thereʼs no [`MonadState`{.haskell}](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Class.html#t:MonadState) in scope.
  As one common example, if you want to just try a bunch of options via `<|>`, it would be annoying to have to add explicit backtracking to each of those.
1. So in general, `SET`{.haskell} is much more convenient when you want to backtrack, which is what you want to do most of the time!

There are a lot of common algorithms that involve rolling back state when backtracking.^[Can I call this rollbacktracking? No? Awh…]
So itʼs really cool that you get this for free, by committing to having your state be immutable and picking the right monad stack!

## When `SET`{.haskell} makes more sense

Most of the time!

If youʼre doing

## When `EST`{.haskell} makes more sense

Itʼs probably more common to want to use both directions of `ExceptT`{.haskell} vis-a-vis `Writer`{.haskell}, but I could imagine some scenarios where you want to maintain state too, either as an optimization for something that should really use Writer, or state that represents information you accumulate regardless of errors, like in SAT solving if you keep a list of possibilities you have ruled out or something (thatʼs kind of like `Writer`{.haskell} but you donʼt want to actually use `Writer`{.haskell}).

Or if youʼre running network requests and want fine-grained retries, you would keep a map of `RequestID`{.haskell} to `Either FailureCount SuccessData`{.haskell} in state and persist that across retries

The failure at the atomic (single request) level is relevant, and the failure at the global (monadic computation) level is also relevant, but the failure of an individual request does not invalidate the state (in fact, it needs to contribute to it)

The plot twist is that the SAT solving model is not so different from the request retry model – theyʼre just nondeterministic in different ways.

