---
title: "`ExceptT` vis-a-vis `StateT`"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

When dealing with monad transformers, how do you want to stack `ExceptT`{.haskell} and `StateT`{.haskell}?

:::{.Key_Idea box-name="tl;dr"}
The tl;dr is that you probably want `StateT`{.haskell} on the outside and `ExceptT`{.haskell} on the inside.
This is the one that has good backtracking.
:::

```haskell
ExceptT e m a = m (Either e a)

StateT s m a = s -> m (a, s)

EST e s m a = s -> m (Either e a, s)
SET s e m a = s -> m (Either e (a, s))
```

it’s probably more common to want to use both directions of Except vis-a-vis Writer, but I could imagine some scenarios where you want to maintain state too, either as an optimization for something that should really use Writer, or state that represents information you accumulate regardless of errors, like in SAT solving if you keep a list of possibilities you have ruled out or something (that’s kind of like Writer but you don’t want to actually use Writer)

or if you’re running network requests and want fine-grained retries, you would keep a map of RequestID to Either FailureCount SuccessData in state and persist that across retries

the failure at the atomic (single request) level is relevant, and the failure at the global (monadic computation) level is also relevant, but the failure of an individual request does not invalidate the state (in fact, it needs to contribute to it)

the plot twist is that the SAT solving model is not so different from the request retry model

(they’re just nondeterministic in different ways)

