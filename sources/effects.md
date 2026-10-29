---
title: Effects, Side-Effects, and Functors
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026
---

<!-- ʼ -->

This is a post about “effects”, and what imperative programmers are missing out on, swimming in the matrix of only those effects blessed by their runtime.

This is a love letter to different kinds of effects we can think about; different ways we can phrase our computations to get the most for free.

Yes, we will be talking about monads! And applicative functors. And selective applicative functors.
They are models for explicit effects, ranging in power on the expressivity vs static analysis tradeoff.

You will want some background in these topics, some familiarity with Haskell, and some knowledge of [purity/equational reasoning/referential transparency/denotational semantics] to get the most out of this.
Feel free to skim (and ask questions) regardless!

:::Details
In Haskell and other pure [FP]{t=} languages, we talk about effects in terms of a functor (applicative/monad/...) `f :: Type -> Type`{.haskell}, distinguished from pure functions `i -> o`{.haskell}.

“Effectful functions” have shape `i -> f o`{.haskell}, for some effects functor `f`{.haskell}.
This is a [Kleisli arrow](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Arrow.html#t:Kleisli); it is one of the fundamental ways to make a category out of monads.^[A category, in category theory, is a formal system.]

You can also have functions inside effects, `f (i -> o)`{.haskell}, and this is kind of the analagous arrow for applicative effects.
I wrote a [post about that](applicatives.html).
:::

## What are effects?

I want to expand your conception of what an effect is.

The most visible, most talked about effects are those that occur ambiently in imperative languages.
They are the water you swim in, the air you breathe, the matrix you are trapped in when you write imperative code.
But still they are the effects you use the most often, the ones you have easy access to.

The top ones are: mutability, (psuedo-)randomness, exceptions, asynchrony^[Fun fact: in Erlang you can simulate mutable state by communicating between two immutable processes.], I/O, FFI, and so on.
When you execute a line of code, it can do any or all of these things.

In some sense they are supposed to represent “how computers run”, and provide efficiency for that.
However, their prevalence, the ease in which you are allowed to reach for them, can stunt your ideas of what effects you should have access to.

If only you worked in a language that allowed this expansive notion to not be an uphill battle.
If only programming language designers were interested in empowering their users to be at their best and put their best effects forward.

### Runtime-blessed effects

In languages based on pure functional programming, like Haskell and PureScript, this corresponds to monads like [`IO`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/System-IO.html#t:IO), [`ST`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Monad-ST.html#t:ST), [`Effect`{.purescript}](https://pursuit.purescript.org/packages/purescript-effect/4.0.0/docs/Effect#t:Effect), [`Aff`{.purescript}](https://pursuit.purescript.org/packages/purescript-aff/8.0.1/docs/Effect.Aff#t:Aff), and so on.

Because they are based on pure functional programming and are not totally pure^[For totally pure languages I would count Dhall and Mikan (née Agda), for example.], they both have escape mechanisms to smuggle in effects into thunks or functions, respectively: [`unsafePerformIO`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/GHC-IO-Unsafe.html#v:unsafePerformIO) and [`unsafePerformEffect`{.purescript}](https://pursuit.purescript.org/packages/purescript-effect/4.0.0/docs/Effect.Unsafe#v:unsafePerformEffect).
Interestingly, Haskellʼs ambient effects are in `IO`{.haskell}, its runtime is totally async, while PureScript compiles to JavaScript by default, which has a strong distinction between synchronous and asynchronous effects: functions, being ordinary JavaScript functions, only have access to synchronous effects.^[In web browser runtimes, this distinction is rigorously enforced; in NodeJS, it is a little more fuzzy.]

Even without dragging in the whole implications of `IO`{.haskell}/`Effect`{.purescript}, being able to throw errors in otherwise pure functions is often viewed as normal: see `error`{.haskell} and `unsafeCrashWith`{.purescript}.
Finally, non-termination (infinite loops) is a more subtle effect; one which is a lot more work to excise, with diminishing returns.

So why are these effects considered effects?
They represent impure functions: code that cannot be reasoned about directly as mathematical functions, but whose evaluation needs to be controlled through some model of execution.
These ones specifically hook into the capabilities provided by the runtime.

:::Note
You might think that performance/speed could be an effect too.
This is a reasonable thought: performance *does* matter; sometimes profoundly so.
But virtually all approaches so far treat performance as hidden; something that is not reasoned about at the level of semantics we like to talk about.
Part of the reason is that it is just genuinely difficult to talk about in a way that is rigorous and useful.
But also it is more useful for reasoning to be able to substitute different functions that calculate the same result.

Similarly, memory management is also not part of the story here.
:::

### Pure … effects?

Those things we can agree are effects.

But what about the `Maybe`{.haskell} monad?
The list monad?
Writer monad?
State monad?
Parser combinators?
Is there some … line here?
Or are they all effects?

Like, `Maybe`{.haskell} is the simplest nontrivial monad.

```haskell
data Maybe t = Nothing | Just t
```

It encodes plain failure or success.
But thereʼs nothing mysterious about it: it is a simple data type that can be understood through case analysis.
Does *this* count as a side effect?

Nah, no side-effects occur if you construct a `Maybe`{.haskell} value and pass it to a function that destructs it with case analysis.
That is only local reasoning with pure functions, pure data.
Equational reasoning, just as math intended.

So why is failure a side effect?

### What the notation leaves implicit

:::: Key_Idea
My position is that side-effects are about what the notation leaves implicit.

If your “statement” does not return useful data, what does it *do*? Side-effects!

`Maybe ()`{.haskell} has the side-effect of possibly failing; `Maybe Void`{.haskell}^[`Void`{.haskell} is what is called `never`{.rust} in some languages: the type with no inhabitants], in fact, has the side-effect of *definitely* failing.^[Or not terminating. See? That is another point for non-termination being an effect.]
::::

Side-effects for a type `f r`{.haskell} are what occurs in the course of computing the result(s) `r`{.haskell} for you.
When you are focusing on the result `r`{.haskell}, what the expression “returns”, then `f`{.haskell} is the means to the end: some side-effects of your computing `r`{.haskell}.
When you care about the whole type `f r`{.haskell}, you probably wonʼt talk about side-effects anymore.

:::Note
Functors like `Maybe`{.haskell} are typically talked about as _returning_ data, even if they are not a function, or do not return the value (like [`ContT`{.haskell}](https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-Cont.html#t:ContT), which provides the “return” value to a callback, before returning a value of a different type in the inner monad).
:::


`do`{.haskell} notation is, of course, one of the obvious ways that notation leaves effects implicit.
It desugars to the “bind”/`flatMap`{.haskell} operator `>>=`{.haskell}, with a lambda binding the result of the previous computation to expose to the remaining computations.
So I would argue that even using those explicit combinators: `>>=`{.haskell}, `pure`{.haskell}, `<*>`{.haskell}, `<*`{.haskell}, `*>`{.haskell}, and yes, maybe even `<$>`{.haskell},— using those combinators counts as performing effects; they are just syntactic noise for calling functions with the correct calling convention for combining their effects.


### Compromise

#### Mathematical modeling

Theoretically, every computation can be modeled mathematically.
You may have to make some compromises: complicate the mathematical model to capture more details.

In the presence of randomness, nondeterminism, concurrency, and other effects, you may not be able to pin down a single value for each function, but you could model the set of values it could return, maybe even assign them probabilities.

However, this has limits.
Just like conservation of energy and momentum only apply to a closed physics system, mathematical modeling of a computer only applies to a closed system of computation – one that is not interacting with unknown entities.

If you are not willing to model the whole world to fashion your closed system, you need some mechanism for interacting with the outside: accepting input and expressing output.

This is why the main effect monad in Haskell is called `IO`{.haskell}: it is responsible for handling input and output at the boundary of the open system of the Haskell runtime, even if you do not conventionally consider C [FFI]{t=} calls to be I/O.

#### `ST`{.haskell .nowrap}

The job of `ST`{.haskell} is to tame the effects from `IO`{.haskell}/`Effect`{.purescript}, so that the efficiency of mutability can be used within a pure interface.
Local mutability is fine, as long as it does not leak out.

:::Details
The usage of a type variable `s`{.haskell} for the region ensures that mutable data is keyed to its monad, and the rank-2 signature of `runST :: (forall s. ST s v) -> v`{.haskell} ensures that mutation does not occur elsewhere, since the type variable will not unify outside of its restricted scope.
:::

You could implement `ST`{.haskell} entirely without mutation, using a `State`{.haskell} monad and a little bit of justified unsafety.
Your state would be something like `IntMap Dynamic`{.haskell} modeling a “heap”, where the [`IntMap`{.haskell}](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-IntMap-Strict.html) is just used to allocate “mutable” cells, and [`Dynamic`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Dynamic.html#t:Dynamic) stores typechecked dynamic values.

However, accessing a dynamic cell has the potential to fail to cast, and `Dynamic`{.haskell} cannot store polymorphic types (it requires [`Typeable`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Typeable.html#t:Typeable)).
So you might as well use `unsafeCoerce`{.haskell}, since the allocator and getters/setters are responsible for the safety of access.

:::Note
This is a good example why functions like `newSTRef`{.haskell} need to be effectful.

Sometimes newcomers ask why it is in an effect monad, when it does not have side effects: it is not like other code cares whether you ran `newSTRef`{.haskell}.
But it would not be referentially transparent to have it be a plain function: if it was, then every allocation would in fact be the same.
That is, it is not safe to *duplicate* the effect of `newSTRef`{.haskell} and treat those results as interchangeable, even though it is safe to delete it if its result is not used.

But with the model of `State heap`{.haskell}, the act of allocating both references and changes the heap directly: there is no way that `newSTRef`{.haskell} could be a pure function in this model.
:::

This would be a mathematical modeling of the mutability effects of `ST`{.haskell}.
But it would be slow, expensive, and ultimately not worth it.
It is just to say that mutability is not magic: it saves you from having to drag around the whole state – every allocated cell – explicitly in an immutable map.

However, it would have one really cool upside: since the state became immutable, you could save and restore it easily, rolling back changes to some or all variables whenever you want.

## Simulating monads in runtime-blessed effects

A lot of effects can at least be simulated in runtime-blessed effects.
This may be good for computation efficiency... but is ultimately responsible for limiting imagination, I think, and making certain things more difficult.

For example, the reader monad.
Explicitly you could pass the value around, but implicitly?
Youʼre stuck by making a global variable, setting it to the value you want before calling a function, and clearing it when that function is done.
This interacts really badly with any form of concurrency or asynchrony.
You could have a value active for two computations interleaved, with no way to telepathically communicate to them which one to choose.
And you might not have a good way to clear it when the computation finishes.

The situation for the writer monad is a bit better.
You can just maintain the “current output” in a mutable variable, write to it as necessary.
Some operations will need to checkpoint it by saving the mutable variable, swapping in `mempty`{.haskell}, then adding the saved state to the (post-processed) output of a previous computation.

However, all of these patterns are a little … annoying.
A little error-prone, when you end up writing them out yourself instead of using a real monad transformer.
A little bit wrong semantically, which causes problems when composing with other effects: especially nondeterminism or asynchrony.

Of course, early return and exceptions can be used to simulate `Either`{.haskell}/`ExceptT`{.haskell}.
To be honest, I find this an area in which Haskell and PureScript libraries are lacking.


## What you are missing out on

Now that we talked about what effects can be simulated in imperative languages, letʼs dive into those that cannot be – or cannot be simulated without a lot of coöperation.

### Classes of effect functors

There are three main power levels of effect that we consider:

- Applicative, characterized by `(<*>) :: f (i -> o) -> f i -> f o`{.haskell} (lifted function application)
- Selective applicative, characterized by [uhh … itʼs complicated](selective-applicative-theoretical-basis.html), but at least should have `branch :: f (Either x y) -> f (x -> r) -> f (y -> r) -> f r`{.haskell}
- Monadic, characterized by `(>>=) :: f i -> (i -> f o) -> f o`{.haskell} (sequencing / `flatMap`{.haskell} / bind operator)

Applicative functors exhibit static effects: the program is known ahead of time, the side effects are all run/combined, then the results (placed in the functor parameter: [e.g.]{t=} `i`{.haskell} and `o`{.haskell} above) are combined.

Monadic functors are totally dynamic: the next step is not even computed until the previous one has run.
In fact, this sequencing was the main reason they were chosen for effects in Haskell, which otherwise did not have means to force computation to be sequenced.

Unfortunately, the monad interface totally destroys static analysis, and their full power is rarely needed, I argue.
This is why selective applicatives sit in between their power levels.
Ideally, they represent a static program with fixed parts (delimited by branching and/or looping) that can be chosen between based on data at runtime – but no obligation is made here that branches are discarded/ignored, so static analysis is totally possible.^[And if you do need the monadic power, it is easy to add it back in while retaining limited static analysis.]

So thatʼs the first thing that imperative languages lose by runtime-blessed effects sticking to a monadic paradigm: static analysis.

### Parsers

I donʼt think you really need me to go over monadic parser combinators as they are traditionally done, basically in the recursive-descent / LL style.
In the Haskell tradition, these are “Parsec” parsers.

Instead, I want to talk about selective applicative parsers: parsers with static analysis built in.

This has been my little research project, on and off, for a handful of years now.

You can make a parser combinator library that captures the grammar, does static analysis, then maybe even runs on an optimized parsing table.
It isnʼt even that hard.
(I just wanted to add other features, so it is indeed quite difficult now. Shush.)

One benefit of going to [LR(1)]{t=} preprocessing, is that you get a table+action-based parser that handles left recursion.
Then it assembles a parse tree, which gets decoded into the parse result for the functor.

:::Warning
One key downside is that, even though the computation of the parse table is specified with pure functions and independent of the functor-y bits, existing compilers will not stage the computation to happen at compile time.
So it needs to be computed independently, stored, and spliced in.

This is something I hope to address in [Pudding](pudding.html), a dependent type theory with staging.
:::

The preprocessing engine can be swapped out, without changing the functor type or parsers at all, just their interpretation.

The idea of adding selective applicative combinators into the mix is that we can model binary formats better: length-prefixed data, letting intermediate parse results choose how the parser flows.



### Nondeterminism

Nondeterminism kind of has two different meanings.

The one that springs to mind for you is probably randomness: maybe true randomness (with some source of entropy) augmented with pseudo-randomness, or even just pseudo-randomness on its own.
You ask the machine to give you a number and it gets to spit out any one it likes, with no discernable pattern, and no predisposed bias, ideally.

But there is another sense of non-determinism: the sense of a [Nondeterministic Turing machine](https://en.wikipedia.org/wiki/Nondeterministic_Turing_machine).

You see, a deterministic Turing machine is familiar: running a predetermined set of actions in response to data and instructions, deterministically.
These algorithms are often used to solve yes/no questions, like SAT (Boolean satisfaction).

A nondeterministic Turing machine allows nondeterministic steps: steps that are not dictated by the machine (either its instructions or its data) but by the outside.
You can think of this as an “oracle” that mysteriously outputs the correct answer to move towards the right yes/no answer at the end of the process.
But you can *also* model this by [simulating each possibility along the way](https://en.wikipedia.org/wiki/Nondeterministic_Turing_machine#DTM_simulation_of_NTM).

This is the sense in which the list monad and the [`LogicT`{.haskell}](https://hackage-content.haskell.org/package/logict-0.8.2.0) monad transformer model nondeterminism: simulate all possibilities.^[Ideally you would deduplicate this in some way, but that is not really possible while remaining an ordinary `Functor`{.haskell}.]

It is also the sense in which parser combinators are nondeterministic: one computation may fail, so it falls over to the next.^[Some parser combinator frameworks capture all parse results.]

:::Key_Idea
Nondeterminism is the idea that something from outside the computation influences how the computation evolves: what path is taken, what value is produced, and so on.

`(<|>) :: f r -> f r -> f r`{.haskell} or `f x -> f y -> f (Either x y)`{.haskell} is a key combinator for nondeterminism.
:::


### Software Transactional Memory (STM)

Software Transactional Memory (STM) is one of the few ways to make concurrency actually compose.
It relies on detailed management of effects: only pure or idempotent functions should be used inside the [`STM`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/GHC-Conc.html#t:STM) monad, and [`atomically :: STM r -> IO r`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/GHC-Conc.html#v:atomically) is the combinator that springs it into action: retrying the computation until it sees consistent state and can commit it.

If you do not commit to having a culture of providing pure functions when possible, STM is markedly less useful.


### Resource management

https://blog.veritates.love/resource-acquisition

Racket implements Custodians, which can guard execution and resources, like OS processes, but within a single Racket process.
Files, sockets, threads – even active allocations, for OOM killing.
(That is integrated with the GC.)

https://docs.racket-lang.org/reference/eval-model.html#(part._custodian-model)

https://www-old.cs.utah.edu/plt/publications/ismm04-wf.pdf



### Continuation monad

https://www.schoolofhaskell.com/user/dpiponi/the-mother-of-all-monads

### Applicative Validation

`Either`{.haskell}/`ExceptT`{.haskell} are for monadic errors.

The downside of the monadic approach is that you only get a single error out of it.
By the monad laws, `throwError err >>= f`{.haskell} has to equal `throwError err`{.haskell}: computation stops after the error, and nothing else matters.

What if we could capture multiple errors?

Applicative validation is this idea.

It comes in two flavors: applicative and alternative.

Applicative validation accumulates errors in a semigroup, having the behavior `err e1 <*> err e2 = err (e1 <> e2)`{.haskell}.

Alternative validation uses ... a distribute pair of semigroups structure?
The most bare-bones notion of semiring you can imagine.
With `err e1 <*> err e2 = err (e1 * e2)`{.haskell} and `err e1 <|> err e2 = err (e1 + e2)`{.haskell}.


### Reader+Writer, instead of State

In the same vein, it is good to think about splitting effects up into reader+writer effects instead of one state effect.
Make things applicative.

Unification conventionally uses state effects: the current set of solutions.

Semilattice.

### Applicative Phases

[Phases in Software Architecture](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/phases.pdf)

### Reactive data / [FRP]{t=}

Like STM, FRP is another thing that benefits from pure functions.
If you use `<$>`{.haskell} over a event stream, you can memoize the function in some situations, sharing the result between multiple subscribers – but only if you know the function is pure.

<!-- ### Indexed monads -->


## Moving forward




