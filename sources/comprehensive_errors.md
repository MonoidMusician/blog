---
title: A Framework for Comprehensive Errors In a (Hypothetical?) Typechecker
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

I am on a quest for the best errors out there. Will I ever realize it? Nope, I clearly don’t have the time. But here’s my blueprint for what I believe are my most significant contributions to it so far.

Despite thinking these are my most significant contributions, I also believe that what I am doing is obvious and not at all surprising. I am not making creative choices of my own, I am eliminating choices from the implementation. Just takes the right lens to look at it, in theory!

## Intro: Point of View

Just to let you know where I am coming from. I am working on implementing the Dhall language in PureScript, and extending it with certain features. These features necessitate a more sophisticated typechecker with state management that was not necessary before, and I want to push it in novel directions.

Dhall is essentially a dependently-typed lambda calculus with extra features like imports, which, while interesting to programmers, are not so interesting from the perspective of language design. The semantics take the form of typechecking and evaluation judgments in the usual way. And since it is dependently typed, evaluation is required during typechecking, although that doesn't figure too deeply into our story yet.

One key feature of Dhall, though, is that it has no implicit arguments! Type applications are all explicit. This is important because it allows us to avoid subsumption checks and just do unification. Much simpler, especially for the extensions I am working on, although I am optimistic that my ideas could be extended to cover at least simple forms of subsumption as well. Like `Natural <: Int`, not necessarily implicit function arguments.

Obligatory note on how typechecking doesn't exist it's all type inference …………………………………………………

PureScript is a typed functional programming language, much like Haskell, that compiles to JavaScript, for execution in web browsers and NodeJS. Code examples will be in PureScript, since that is the language I am implementing in.

### My Argument

Despite more and more compilers being written in functional languages like Haskell and PureScript, or in other languages with functional concepts in mind, I argue that they are still wedded to a notion of typechecking state that acts a lot more like the imperative world, with continuous access to an everchanging global state.

This document, then, is an exploration of what the alternatives might look like and what benefits can be gained from them. Key benefits are:
1. For users: Able to generate more errors at once, which can give users more context and insight into what went wrong in the global picture, as opposed to a single localized error that doesn't reveal which side of the picture was really incorrect.
2. In implementation: Able to “roll back” or avoid committing pending state changes.
3. Theory: Looking at the algorithm from a fresh new perspective, increasing our ability to reason about it abstractly without worrying about implementation details.

In particular, I have begun to suspect that the effectful structure of typechecking is actually quite straightforward (once you embrace the mixture of sequential effects with opportunities for parallel effects). And once that skeleton is established, what is left is just algebraic problems that can be isolated and solved to come up with more general, more polymorphic type theories that were all but written into the structure of the original type theory. In particular I am aiming to add universe and row polymorphism to Dhall and general type unification.

## Part 1: Monads? Applicatives! Both?!

This part is ostensibly a dive about the nitty gritty details of the practical implementation of a typechecker. More specifically, it’s about how I believe they should be implemented (at least for simple type theories!) in a purely functional way, particularly in languages like PureScript and Haskell.

However, it is also the story of how effects are useful for thinking about type theory and how the effectful structure of type theory can begin to be isolated from its algebraic structure.

### The Story

To step back a bit, let’s revisit the question of how do we write down/communicate/learn the rules of a particular type theory?

There’s two main ways: the presentation of the rules in terms of logic with fake syntax, and the implementation of the rules in an actual programming language.

Despite the many axes I have to grind with the fake syntax of logic-style presentations of type theory, they are quite effective at communicating rules concisely. At least to humans who are familiar with the white lies of type theory, certainly they are not detailed enough for computers to understand. However, one big downside is that they do not encode errors at all: the error state is just “no derivation found within this system of rules”.

So with compilers, we want to give more specific errors. Like, “Expected this to be a type but it was a number.” These errors really represent the fact that the system of rules forced us to commit to a derivation, but one – or more! – of the assumptions wasn’t satisfied.

This “or more!” part here is what is most important to me: due to how typecheckers are implemented, they usually bail on the first error. I want to avoid that. For example, when typechecking a list, in theory all the expressions could be typechecked independently and the results combined, and this includes combining all the errors that might have occurred. (It also includes combining the state changes – ponder that for later.)

A similarly suboptimal behavior is that when some knowledge is gained by the typechecker, it commits to that and judges all further knowledge against what it has committed to. (Again, this gets particularly tricky with state, but exists before it.) Returning to the list example, if the first item in the list has a particular type, all others are judged against that type – and even if the first is wrong while _all_ the others agree with each other, you’ll get many errors despite only having one problem!

So my idea is basically to step away from the nitty gritty details of the sequential algorithm implementing the rules, and take a bird’s eye view of what’s going on at each step, in order to get better errors.

In particular, this will mean **performing as many pieces in parallel at once as make sense**. If we get errors, then we can report all errors that occurred, not just the first. Or if no errors occur, then we should continue on the sequence of actions with knowledge of all of those results. And, **if those results were on equal footing (like the elements of a list), we should consider them on equal footing, and compare them not just to the first element but holistically as a group**.

Finally the conclusion of this perspective is that the actual algebraic checks that need to occur can be separated from the computational structure of how typechecking proceeds. To realize this goal, though, we need to dive into the details of sequential and parallel effects, since I envision that a hybrid viewpoint where effects (particularly errors) are incorporated into the description of the rules will be fruitful.

### Monads and Applicatives in Parallel

Functional implementations use monads to sequence effects in the compiler. We’ll talk about the details of what these effects are later, since it matters more in the applicative version, but for now assume it has errors (via `ExceptT`), local context (via `ReaderT`), information reporting (via `WriterT`), and eventually state (via `StateT` … but actually not).

The original motivation for the parallel class is asynchronous operations. Strictly speaking, we do not need to talk about asynchronous effects here – static language semantics (for Dhall in particular) should not need to talk about disk or network access, for typechecking or evaluation. Still, it is worth it to provide context on parallel effects for one paragraph.

How can asynchronous operations run in parallel? Of course the JavaScript execution environment is dreadfully single-threaded, so the _synchronous steps_ making up an async operation cannot compete. However, up to that granularity, async operations _can_ operate in parallel: it makes sense to start, say, network-bound operations as soon as possible and wait for them to come in – listening in parallel for their completion. Failed networks requests may potentially need timeouts before retrying, which would be another case where asynchronicity wins by keeping work happening while waiting on other tasks. If just sequential actions were allowed, each network would only be initiated only after the previous one finished, including timeouts for retries.

The rest of the effects will not be parallel in this sense of executing concurrently in real-time, but will be parallel in a related sense which will be clarified later.

#### Background on Applicatives vs Monads

Monads are a general abstraction for sequential effects. Every monad is also an applicative, however, and applicatives provide for possibly-parallel effects.

Being a monad or an applicative is a structure on functors: there can be different instances of these structures for a given functor. We'll denote monadic functors with variables like `m` and `n` and applicative functors with variables like `f` and `g`.

For example, the monad for asynchronous effects in PureScript is `Aff` and its corresponding parallel applicative is `ParAff`. If you have an asynchronous function to get files, it might like look `readFile :: FileName -> Aff String`, for example, and converting this to `ParAff` will allow combining several of these in parallel. We read `Aff String` as “an asynchronous, effectful computation which produces a string if it succeeds”. In general, we read monadic and applicative functors `f a` as “producing a value of type `a` with some effects allowed by `f`”. Some categories of effects include reading/writing mutable variables, I/O, failure and errors, nondeterminism, and state, and functors give us tools to separate them out into distinct layers.

Monads and applicatives share a common operation `pure :: forall a. a -> f a`. This operation creates an effectful computation that in fact has no effects (hence the name “pure”), and only returns the value passed to `pure`. Abstractly, this is an identity for the monoidal operations for monads and applicatives, and thus uniquely determined.

Applicatives have an additional operation called `apply :: forall a b. f (a -> b) -> f a -> f b` (also written infix as `<*>`). One of the laws is that `pure f <*> pure a = pure (f a)` – applying a pure function to a pure value is the same as the pure computation whose result is the function applied to the value. For non-pure computations, the intent is obvious to see: effects from both are run and their results are applied to obtain the compound result.

Except, it is not quite so obvious. Is the first argument run first, or the second argument run first? Are they run in parallel? Does it matter??

The beauty in applicatives is that there is the freedom to choose. In fact, it is a simple matter to swap effects around and run them in reverse order, for any applicative. For some applicatives (e.g. `Maybe`), this doesn't make a difference. But for our story here, the ability to have them run in “parallel” is crucial, and we will gradually clarify what this means.

Back to monads now. Monads have a further operation called `bind :: forall a b. m a -> (a -> m b) -> m b` (written infix as `>>=`). This is the key that allows for sequencing effects. The intuition is is that the effects that need to be run to obtain the `b` are not known until the effects from the `m a` are run to obtain the `a` needed to pass to `a -> m b`. (This is not to suggest that there _must_ be a result `a` or _only one_ – indeed, that's part of the magic of monads: they can handle errors and nondeterminacy gracefully, meaning zero or multiple results respectively.)

To give some intuition for how `bind` works, one of the monad laws is that `pure a >>= f = f a`. If the first argument is a pure computation resulting in some value `a`, the result of the bind just applies the continuation `f` to that value.

In fact, it is possible to implement `apply` from `bind`: `apply mf ma = mf >>= \f -> (ma >>= \a -> f a)`. The `Monad` typeclass in PureScript requires that if apply is not given by that implementation, it is at least equivalent to that.

But there's nothing stopping us from considering additional applicative structures on the same type. So, to exploit the tension between parallel and sequential effects, we can relate a monad with a parallel counterpart, with trivial coercion functions each way. Now we have the choice which way we want to combine effects. There is still only one `bind` operation, but now there are two `apply` operations: sequential (via the `bind`) and parallel.

Sometimes the choice is forced for us: the dependency of later effects on previous values will force us to use `bind`. But in a surprising number of cases, `apply` is sufficient. In fact, for traversing over a datatype (like a list of values), `apply` is all that is necessary, so we just need a combinator to choose to do it in parallel.

One caveat here is that sometimes the dependency is implicit, due to inexact types being used. In particular, in order for an expression to be safe to evaluate, it must be the case that the expression typechecks. So there is a logical dependency on typechecking an expression before evaluating it – but this isn't represented in the types. One could model this by having a new type solely for expressions that are known to typecheck, and then having normalization operate on that type instead of any and all expressions. But the PureScript type system isn't strong enough to express this internally or get any benefit from it. There are workarounds (so-called “smart constructors”) which provide slightly more safety, but they are still imperfect and I do not personally choose to do that. And there are cases where the evidence is indirect: if a list typechecks then so do its elements (in fact, any sub-expression).

The main problem with monads is that they do not compose, in general. Instead, monad stacks must be carefully designed so that they consist of composable parts. Luckily it is not a problem for the examples we have to discuss. The obvious things we need to do are indeed possible. However, it results in a weird discrepancy in libraries where monads are defined with composable versions (monad transformers) and applicatives only have uncomposed versions.

_(Sidebar: There is an additional abstraction of `alt :: forall a. f a -> f a -> f a` and `empty :: forall a. f a` (this is even more obviously a monoid operation and identity!) which is also useful, but a little beyond scope for now.)_

#### Examples

Now we will go through some examples of common monads and how they are already parallel or can be given counterparts of parallel applicatives.

##### Local Context from `ReaderT`

In some sense, this is nicest monad to work in. It encodes local context – some form of read-only data that is kept around and accessible during all parts of the program, and nested computations can even receive modified context. Note that this isn't quite state: the local context cannot be changed for sequential computations like state would do, only nested ones. Context flows top-down through the computational tree instead of linearly through execution.

The reason why it is the most well-behaved is that it is literally just an extra function argument being carried around. And since we are working in languages derived from lambda calculi, function arguments (i.e. variables) are freely accessible everywhere in scope anywhere, so implementing local context is trivial.

It is for this reason that the parallel counterpart of this monad is literally itself, as an applicative. There is no notion of sequencing effects versus running them independently if the only effect is accessing read-only data, which is oblivious to whether other reads have occurred.

##### Information Accumulation in `WriterT`

On the flip side of the coin, we have write-only data accumulation. How this works is that we can accumulate data in a monoid. In practice, a common monoid to use is often just lists of data – for good reason, since lists form the free monoid.

Similarly to the above monad, since the only effect here is _accumulating_ data, there isn't a separate parallel version that needs to be provided, it is just `WriterT` itself.

(My argument is that this is sufficient for the state of a typechecker, but more on that later.)

##### Errors through `ExceptT`

Errors start to get a little tricky. If you assume every action is run in a well-defined sequence, and each depends on the previous ones (as is the assumption when using monads), then a sequence of actions can only fail with one error. The first one crashes the whole thing. However, when you start asking for things to be run in parallel, when possible, then it is desirable to keep track of however many errors occurred at once. We will use the same trick as above and store them in an arbitrary monoid, but often just as lists of errors.

In fact, things are worse: if we add the ability to try alternatives, one actually needs a semiring of errors, not just a monoid! Luckily I do not have to deal with that in the context of my projects so far. Alternation is very limited, and is easy enough to encode via error-recovery.

Now is a great time to clarify a bit what parallel means. As I said, PureScript is very much single-threaded, so the underlying evaluation is sequential. (Strictness is sort of relevant but not incredibly so.) So the parallel applicative instance cannot avoid actually evaluating them sequentially. But what it does do is give the opportunity for considering all the errors that occurred in the computations, and preserving all that information.

#### State via `StateT`

State is kind of the beast of monad transformers. By state we finally mean readable _and_ writable state, where the data that is written most recently should be read by upcoming computations, until the next write.

So … how in the world do we make this readwritable state parallel? … well, we don't.

In fact, it's worse: if we stack errors and state in the useful way (i.e. `StateT (ExceptT e m) s`)., we only get state information when no error has occurred. Thus the sequential nature of state infects the potentially-parallel errors portion of the stack, and we're back to only getting a single error at a time.

<!-- TODO: applicative state composing wrong -->

Instead I argue that we need to **forgo readwritable state** and restrict ourselves to **separate readable context flowing top-down with writable information bubbling bottom-up during typechecking**.

This discipline, I argue, will result in clarity about how typechecking works as well as clarity for the user about how errors occur. In particular, the structure of the typechecking follows closely the structure of the AST, so if the user has a good mental model of abstract syntax (or at least how expressions nest), then they gain a closer insight into the typechecker. More transparent and predictable operation of the typechecker, disentangled from pseudo-mutable state, is a win for everyone. Hopefully.

(Time will tell what performance is like. Hehe.)

### The Monad Stack in Practice

For an implementation of Dhall as it stands today, it really only needs local context and errors. I also added a writer to the stack (just in case!). And for import resolution, the stack needs a base monad of `Aff` for asynchronous effects, instead of a pure monad like `Identity`. But the point is: it all supports parallelism nicely. (There's a tiny bit of state tied into import resolution, but it is unimportant since it isn't implemented with `StateT` but instead as part of effects already in `Aff`, plus its actual order of evaluation should not matter.)

For the new features I am adding, I need to add some notion of state. Part 2 will get into what actually goes into this (**not** `StateT`!), but the trick is ensuring that it works with parallelism. Once it does, the actual structure of typechecking is the same! Just with a few special function invocations sprinkled around the leaves to make use of the new state.

### Implementing a Parallel Typechecker

I will begin by reiterating the examples I've talked about before. Lists are “obviously” parallel, and function application is “obviously” not …– or is it?

#### Typechecking List Literals

In Dhall, empty lists need to be annotated with their type, while nonempty lists have their type inferred from their elements. These elements must then all have the same type!

When typechecking a nonempty list literal, typechecking the elements should be done in parallel. This situates all the errors that occur on equal footing. We should further consider all the resulting types on equal footing.

In particular, I formulated a helper function called `checkConsistency` that takes in some non-empty foldable of values and sorts them into clusters of equal elements. If they're all equal, then great, return that. Otherwise throw an error showing exactly what the clusters are.

This allows the user to see precisely what types were all present in the list when they do not match, no matter if all elements but one have a type or if they each have different types or whatnot. This is clearly the optimal solution, in terms of preserving all information, and we didn't have to sacrifice that much to get it. It is also superior because it doesn't leak information about how the typechecker is going about its job. Is it proceeding left to right? Did it pick the first type to compare against? Does it shuffle it and proceed in a random order, taking a random one to be a representative? By guaranteeing that the user sees all the information from doing it in parallel, none of this matters.

This `checkConsistency` function was sufficient for Dhall as currently standardized. The next thing I will need to add is unification. It should not be a huge deal, but it will make the function effectful (since unification information will need to be produced). And there's some open questions about if naïve methods of grouping will work. This is where the ability to avoid committing state changes will be important. See Part 2 for more.

#### Typechecking Function Application

Simplified [function application rule](https://github.com/dhall-lang/dhall-lang/blob/master/standard/type-inference.md#functions):

```{.agda data-lang=TT}
f : forall (x : A) -> B
a : A
_______________________
f a : B[x := a]
```

This is arguably the single most important judgment in type theories, since they're all about functions and function application.

It is also where what I said about “no subsumption” becomes important: there's nothing fancy we need to do at function application, just ensure the type of the argument and the function domain match.

Notice how I phrased it like “ensure that X and Y match” (i.e. “unify X and Y”). This is because there’s no bias in the rules: it's not “ensure X matches Y” or “ensure Y matches X”.

In a lot of systems it would be the case that one wants to infer either the function first, and then check the argument against its domain, or check the argument first and then check the function type so it satisfies that. This is because the additional information from the first piece guides the typechecker to make various choices to end up with the right type for the second piece of the equation.

Dhall's type system has no room for choices like this. It is simple and straightforward. This allows us to infer the function and the argument in parallel! They do not influence each other, although their types do need to be checked for compatibility. And then state also needs to be combined, but that's simply part of the framework we will build, not something unique to the function rule.

So yes, it turns out that function application, which looks like a very asymmetric rule where one side could/should dominate over the other, can in fact operate using the same principle of parallelism as any other binary operator. Typecheck both sides and then check for compatibility, returning the appropriate return type if all goes well.

### Summary and Conclusions

What have we learned?

In pursuit of better errors, I am seeking not to make good choices, but to force the _best_ choice. Hopefully my example with the lists is enough to convince you that considering the elements in parallel and their types on equal footing is uniquely the best choice, and we came to that conclusion by seeing that it was the choice that **preserved all the information we could have available to us:** all the errors that occurred, or all the ways the types agreed and disagreed with each other.

Actually, it would be even better, a heroic bestest effort to show the user all the errors that occurred _as well as_ the result of unifying all the elements that didn't result in errors … but that is probably too much work for little gain, what I proposed already fits nicer into the abstraction boundaries as they stand, and may be correspondingly more predictable for a user. But anyways.

This discipline of preserving information will take us far, I think.

In the next part, we will see how the discipline of supporting parallel information flow pushes us in a unique direction to consider what typechecker state really means and how to solve it algebraically.

## Part 2: The Data and Algebra of Semantics, State, and Errors

The previous part gave us an outline of the framework we'll be working in: as much parallel computation as possible. Not necessarily parallel as in multi-threaded, but parallel as in executing independently and considering the resulting information altogether.

However, there are a ton of details to figure out.

### Implementation/Framework

Okay, so what is this mysterious `StateT` replacement I've been hinting at?

Well, there's a couple choices. But the simplest choice is literally just `MaybeT (WriterT w)`. Tada! QED. We're all done here.

Just kidding.

We will see why it is possible in a short bit, but there are two problems with using `MaybeT (WriterT w)`: it obscures a bit of what's really happening, and it allows the state where no error has occurred but no result was produced either. In practical terms this is not a big deal, just generate a “no error occurred” error, but it seems suboptimal.

The general idea is sound, though: we want to accumulate some state, and if there's some error state then we can forgo returning a result.

The next option is to split the state into “good” state `m` and error state `o`. The main problem is that error state may come mixed with good state.

#### Monoid-ish State

How it's work out is that the overall state `w` is a monoid, and then we have two subsets of “good” and “error” states (`m` and `o`) which are complementary in some sense.

Since the overall state is a monoid, it means we **always know how to combine two states**. Which might be a surprise – don't we expect combining states to fail? And the answer is yes, totally – but we'll **still keep track of the error state wrapped up in the monoid**! But we expect that the good state contains the identity element (always want to start off on good footing), and complementarily we expect that error states are closed under the semigroup operation: combining error states shouldn't take us back to the land of goodness. However, they aren't completely separated: an error state with a good state may be a mixed state, where some of the good state survives and some of the error state infects the good state still. Nevertheless, adding good state to an error state shouldn't make the error state disappear.

Some of these are just intuitions and are not quite formalized into laws yet. One further law, though, is that good state and error state should commute with each other. Oh whoops, that definitely doesn't happen. In the squashed model where we forget order, it will though, but then everything commutes. Hmm …

#### The Stack

```purescript
data SE w e a
  = Success (StateOf w) a
  | Error (These (ErrorOf w) (NonEmptyArray e)) (StateOf w) (Maybe a)
```

### Theory/Algebras

There are three different kinds of “algebra” we need to keep track of when actually implementing the ideas I have for Dhall. To get us started thinking in this framework, though, I will present a trivial algebra which only has equalities.

#### Warm-Up: Consistency

Let's imagine we have a very simple AST of plain trees (rose trees) with simple equality constraints of the form `variable = value` at the leaves.

Our goal will be to check that these constraints all fit together nicely, as a way of easing into this way of thinking about constraints and information flow.

Strictly speaking, this is really at the wrong level to model what we're working towards: the constraints we want to talk about don't occur in the source code, the constraints arise one step removed – during typechecking!

However, you may simply think of this AST as being either an abstract model of the constraints arising during typechecking, or as literally formed via some steps of processing that resemble typechecking. Or simply as the toy example it is.

Let's begin by asking what a good state should look like? Well, if all the constraints `variable = value` are consistent, then we'll assign each variable to a single value. Something like `Map Name Value`.

What's a bad state? Well, we've had at least two inconsistent assignments to the same variable. Is that all? No, potentially many variables could have conflicts! So, following our principle of keeping _all_ information around, the error state is really a non-empty map of inconsistencies: `NonEmptyMap Name (TwoOrMore Value)`.

Now's where the magic happens. What's the least common denominator? Can we find a happy monoid that snugly encompasses both? Certainly it's going to be some sort of map with names as keys – that much is clear. It can definitely be empty – the trivial state, which is a good state, is empty! For each key, if it's consistent it will have one value, if it's inconsistent it will have two or more values … so it's really one or more values. As a type, then, it is `Map Name (OneOrMore Value)`.

Watch this: the combining of two states now is just the obvious monoid operation on this type!!!! And then we sift out the consistent and inconsistent states based on whether they have a single value or multiple values.

That's it. We don't have to agonize over how to add error states, or how to add good states. It all just falls out of combining it all into one large state.

And notice how it just does the obvious thing: “record all the unique values associated with the variable via constraints”. It preserves information.

Note that we don't allow _no_ values with a name. There are slightly deep reasons why, which I might as well explain here, but feel free to skip. The obvious reason is that if there's no constraint mentioning a variable, we shouldn't record it, and if there is, there definitely is a value associated with that constraint. So we always have a value for each key. But there's deeper reasons too. One is that `Map k v` has a monoid structure whenever `v` has a semigroup structure – the empty map provides the identity, regardless of whether `v` has one! Looking further into the math, we might say that `Map k v` is really modelled by a function `k -> Maybe v` with finite support, where `Maybe v` this time shoulders the responsibility of adding the identity `Nothing`, which is lifted to the function `const Nothing` (with trivial support). This “semigroup-to-monoid” property pervades the theory of states built up in my implementation.

Backing up slightly, let's focus on what `OneOrMore` and `TwoOrMore` are.

Open question: what would it look like if we added constraints of the form `variable1 = variable2`? Doesn't have to be a formal definition in types like the above. I know there's a good answer for what good states look like, but I'm not so sure about error states: there might be several options. It might be that a particular representation of error states is forced by the laws I want to hold – which would be interesting because it actually would violate the principle of “most information” and potentially be confusing, but if it is the option that makes sense I would still go for it. Or maybe the representation isn't forced, but it starts leaking the choices in the typechecker which I suggested should be hidden. Hm.

#### Universe Levels

As I explain in more detail in my actual draft of the paper, there are three algebras we are dealing with here. The first is the language of predicative universe levels, including just zero, successor, and maximum. Next we want to extend it with an impredicative universe, so the impredicative-maximum operation is added. However, for normal forms, we reach for a slightly more powerful operator.

##### Predicative Universe Levels

The idea is we saturate all information gained from the axioms. There are a few tricks to get us the information we need to do this, without literally applying all of the axioms individually.

The first is that we want to know how expressions relate across models. Is one expression always strictly less than, or less than or equal to the other? I came up with a system to capture the relevant relationships which is described in the paper.

The second part is shifting to force an inequality. That is, two expressions may not be related as is, but one can be shifted by a constant amount to always be less than the other.

Finally there's some additional stuff to reduce the state. I won't claim that it's enough to reach an actual normal form of the whole solver state, but it's closer.

Now, to exhibit a concrete solution, we follow a simple algorithm. We pick the least key at each step, and set the variables in it to the constant factor known on the right. Then we reduce the constraint set by this new knowledge and repeat until the constraints are empty.

The informal justification for this relies heavily on the normalization algorithm above. In particular, we say that the choice is valid because if there was any constraint to force the LHS to always be greater than the stated constant, it would have been found and bumped up. Since it is the least choice we can make, it should maintain consistency.

##### Impredicative Universe Levels

It is clear that there is a naïve, brute-force method to checking impredicative universe levels. Simply case-split every assignment of variables to “zero” or “positive”, and check for consistency based on the predicative universe solver. This is an exponential blow-up over the complexity of predicative checking (which I suspect is polynomial, perhaps cubic), but probably mangeable in practical cases, where impredicativity is either obvious or not relevant.

I've been working towards method that avoids this, but I'm not 100% sure it works out. The problem is that it seems to require adding a minimum operation!

##### Errors

It's not quite clear what comprehensive, associative errors should look like. Right now the errors that are returned simplify identify _some_ chain of axioms that result in a contradiction. Actually, it is more indirect than that: it identifies two expressions in the `GESolver` with such that `k !>= v` is requested to hold but in fact `k < v` across all models. Of course, this `k !>= v` constraint is just a combination of source constraints with axioms, and that derivation could be tracked. Plus, raw axioms are not really a suitable format for intelligibility either.

It seems like an uphill battle to provide comprehensible errors, much less global errors that collect all the conflicting information. The one bright ray of hope is that the problem is secretly geometric! For predicative universes, in fact, each constraint carves out a (simply?) connected subset of <i>c<sub>00</sub></i>, and the boundaries are all of slope 0 or 1. For impredicative universes, it is a little worse, but not too bad. (I don't have a precise characterization yet.)

So for a global picture, one could collect information about which regions are compatible and incompatible. Back to the logical/axiomatic perspective, one could identify loops like `a <= b < c <= a`, and then other errors like `0 > 1` and such. But eh, doesn't fully seem worth it.

The next problem would be excising the error state from the good state.

What would make it easiest for users of the language to understand what went wrong with such a technical aspect of type theory that's about mysterious numbers?

#### Row Types

Uhh work in progress? I am hopeful a similar story will hold for row types as for universe levels. That is, we accumulate constraints and check their consistency. However, it will look much different.

##### Partially Solved Rows

Unlike universe levels, where we literally do not solve a variable ever, rows are going to operate via partially inferred rows.

Hmm I was thinking like `data Row a = Closed (Map String a) | Open (Map String (Maybe a))`.

<!-- But maybe we want `data Row a = Closed (Map String a) | Open (Map String { constraintPresent :: Constraint a, constraintAbsent :: Constraint Unit })` with the constraint information localized to where it occurs? But I think that won't really be compatible with (1) complex casing in constraints and (2) structural unification. -->

##### Descent To Type Constraints

The unfortunate part about row constraints is that they necessitate type constraints.

Consider the `//` operator: `((rL : { r1… }) // (rR : { r2… })) : { r1 || r2… }`.

What if it becomes known that `r1` has a label `a`? Well, then we know that `r0 = r1 || r2` has the label `a` whether or not `r2` has it, but the type isn't known yet. That is `r1 = ( a : T1, r3-a… )` then `r0 = r1 || r2 = ( a : T0, r3-a || r4-a… )` with either `r2 = r4-a` not having the label `a` thus forcing `T0 = T1` or `r2 = ( a : T2, r4-a… )` having the label `a` forcing `T0 = T2`.

So really we have to consider type constraints when considering row constraints – at least if we want it to be complete! Luckily we shouldn't have to get into full structural unification yet, because the constraints are just the obvious unification constraints.

#### Structural Unification

On the one hand, this part of it is more obvious because it is something that a lot of typecheckers do. On the other hand, it is less obvious because is incomplete – there is no perfect solution. Organizing the data is tricky as well. I hope that I will have enough experience from the other parts to pull this off.

##### Framework

Unification state is a mapping from metavariables to expressions (which may contain metavariables).

Metavariables are named after their location. When they get partially solved, they generate more metavariables which are named after their location in turn, so it all works out nicely. (Hopefully.)

Not clear whether its best to leave those derived metavariables separately indexed or to always consolidate them into the original metavariable.

The tricky part is that repeated unifications generate unifications.

##### Universes

Recall that universes are the types we give to types. So everytime we have something we know is a type, we need to ask what universe it is in – and in the general setting, we hardly know the answer right away. So if we have a bare lambda `\x -> b` without a type on the argument, we know `x` has some unknown type `T` and `T` must live in some universe `u`. So already we have to expand it to `\(x : T : Universe u) -> b`, and so on.

Thus we need universe level solving. Luckily we have that!

##### Rows

Similarly, all the record/union operators will start causing row constraints to appear.

##### Field Access Operator

The field access operator is pretty tricky. What does `value.field` mean in Dhall? Well it depends on whether `value` is a record value or a union type.

I personally think this is the most ugly overloading ever for several reasons. Ugh, bring back good old union literals please. But on the flip side, it provides a nice challenge to show that I can accommodate even the seemingly unprincipled “rules”.

What we have to do is introduce a RecordValueOrUnionType …………………… wait. No that's right. RecordValueOrUnionType.

This will sort of behave like a RecordValue by default. Its fields will have values, and those values will have corresponding types, since we get that for free in this setup. Those types are really important.

The magic is that it will unify with either record values *or* union types! This is where we really start introducing relations into the unification lattice.

Unfortunately, we have to hijack the occurs check on the type of the fields to instead – in certain special cases – force it to collapse into the UnionType case. (I told you it was only marginally principled.) If we have `u.x : u` or `u.x : t -> u` then it must be `u` is a UnionType. (*Is* that the occurs check, if it occurs on the _type_ of a field? I hope so. Ugh.)

The next step is that we have to close it under the operations we need. Luckily normalization is trivial.   Typechecking means we need to introduce a RecordTypeOrUniverse – again, normalizing that is trivial, and of course the type of a type has to be a universe so it stops there.

One issue is that information has to flow backwards from RecordTypeOrUniverse – if it unifies with a RecordType or with a Universe – back to the corresponding RecordValueOrUnionType.

##### Higher-Order

Higher-order unification is certainly, definitely, absolutely undecidable.

## Part 3: Provenance

Provenance is my way of keeping track of where expressions come from. The idea seems to be floating about already (I have had discussions with Nate Faubion about it, for example), but this is my take on it.

### Source Spans Are Broken

Normal compilers operate with “source spans” that tell users where the error occurred in their code.

There are a few issues with this. The first is that the concept is not well defined: what source span should be reported for this particular error? Who knows! It's usually fairly clear what source span should be given to a node in a concrete syntax tree, but the compiler natively operates on AST. This leads to difficulty when implementing, since sometimes ambiguous decisions have to be made, best guesses as to what would be useful to the user in the absence of clear guiding principles. There are further implementation issues since many ASTs are not meant to keep source spans around, so it's easy to lose track of them. Source spans also do not work well with derived data: if one derives some AST from a source AST (for example, as a desugaring step), the source spans can be copied over somewhat, but it often isn't granular enough – especially since desugarings often _add_ nodes!

For these reasons, source spans fall short of being a precise enough concept for my needs.

In fact, the situation is even more tricky in dependently-typed languages: arbitrary source can be present at the type level, and in fact it must be typechecked and (partially) evaluated before it can be compared. So not only is there derived data being passed around, but it can be evaluated and sliced/focused in on in arbitrary ways.

### Provenance to the Rescue

This is where my notion of provenance comes in.

**Provenance should be good enough that you can determine what exact expression it refers to.** The caveat, of course, is that it may not be an expression literally found in the source, but a derived expression!

Each operation that runs not only **preserves the provenance** from the expression it was given, it **also** tags its result with the new provenance that **stamps it as the result of that particular operation** being applied to whatever the input was.

Thus we see that it is common for nodes to have multiple provenances documenting all the ways that their expression arose. In fact, unification does not have a special type of provenance for itself, it just combines the provenance of its (normalized) inputs!

There are several types of provenance that I make use of:
- Location, that is, picking out a sub-expression
- Type-of, returning the result of typechecking the node
- Forms of normalization, including alpha-normalization, and beta-normalization (evaluation), also substituting all variables in scope.
- Resolving imports

So given a particular source expression, there is a **function from provenances to their corresponding expressions**. The contract of the compiler is that this function is **secretly total**: every provenance output by the compiler should refer to an actual known expression. But in general, the function will be written with effects:
- For location provenance (drilling down into an AST), the function is partial (in the sense of `Maybe`) because the type of node may not be the type of node denoted by the location.
- For type-of provenance, the function is partial because an expression may not typecheck, producing the errors we have been talking about.
- Beta normalization provenance is also partial in the sense that it may not terminate, but again, a well-written typechecker implementation will make sure that only expressions that are known to typecheck are normalized, used, and could appear in the output.
- Alpha-normalization is well-behaved and does not require.
- Substitution might require a reader context to know what values to substitute for what variables? Haven't quite figured it out yet TODO
- Resolving imports generally requires an async context as I've mentioned above. However, since all of the necessary imports were already resolving, we can get away with the import cache that was maintained during the import process. Lookups in this cache will still be partial, though.

### Multiple Source Files

The one problem with provenance is that it doesn't tell us _which_ source file it came from, only how to get from a source file to an expression. So we should pair an origin with provenance. The origin is really an index into some collection of source files. It could be actual file paths or URIs, or keys in a database, or numbered expressions in a REPL – whatever.

In types, this looks like `type Location = Tuple origin provenance`, and it is this type that actually gets threaded around the typechecker and evaluator.

### Provenance in Practice

Efficiency is probably going to be a nightmare. There should be ways to optimize it, though, by storing provenance in a tree form for better sharing (as opposed to collapsing it into a flat list, which might be exponential), and also by lazily computing it (just let the runtime traverse the graph!). I think it's best to represent the operations that actually occur. Sifting through the data is best left for later. Most of the time it won't be inspected!

### Time Traveling

One of the exciting parts about keeping around the deluge of information is that we can travel back in time and ask questions like, “Where did this node originate? What did it look like before it was normalized?” – however, caveat usor, there will be many answers to these questions, so this is why I think an interactive interface for sifting through this information is all but essential.

### Algebraic Errors

The trickiest part of provenance in my situation is integrating location information into the algebraic errors, which don't necessarily follow the same structural boundaries as the rest of typechecking.

The first issue is merely a technical one: how do you know where you are? The answer is simply a reader context, being careful about threading it around, possibly using recursion schemes, and so on.

The second issue is more pernicious: how do you incorporate this extra provenance into the algebraic structure that we said errors fit into? The main problem is that the associativity of the monoid structure is at odds with the reality of how errors are detected through a tree-based process of combining constraints.

One option is to **give up on the associativity of the monoid structure** in the algebra of errors. Certainly we want good state to still be associative, but once we start reaching error state maybe we want to say “Hey, stop! pause! halt! Something bad happened and it doesn't make sense to continue to pretend it's all worth combining anymore”. This does make sense regarding intent.

However, while this does a great job of pinpointing where the sparks of conflict ignited, it means that we stop short of a global picture that is exactly the strength of this algebraic approach.

A middle ground would be to **snapshot errors with the provenance**, thus recording where the sparks ignited, and additionally keep the monoidal errors going without provenance.

A more clever option would be to **reverse engineer the provenance of conflicts after the fact**: certainly the provenance of each constraint is recorded, so they would spark conflict at their nearest common ancestor.

Finally we could just lean into the global nature of the constraint algebras and say **maybe there is no particular place where conflicts occur**; it is merely a result of constraints from (leaf and branch) nodes conflicting.

## Part 4: Visualization and Interactivity
