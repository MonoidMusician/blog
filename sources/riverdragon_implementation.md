---
title: Implementing FRP and Why
subtitle: Riverdragon FRP Documentation (1)
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/02/22
---

[🐲 _whatʼs upstream?_ 🌊]{style="display: block;text-align: center;font-size:1.2em"}

To explain *why* I wrote Riverdragon, we need to need to talk about what makes “[FRP]{t=}” *Functional* Reactive Programming, and the challenges that being purely functional imposes.

Of course, it is not all challenges: the sequencing of effects is a boon for maintainable code, it just means that you have to be more careful and deliberate about it.

::::{.Key_Idea box-name="tl;dr"}
The punchline of this article is in [Consequences of `IsFlowing`{.ps}](#consequences-of-isflowing), which I will copy here:


:::Key_Idea
Whatʼs special about `River`{.ps}?
Well, like a `Fiber`{.ps} represents a running `Aff`{.ps} computation, a `River`{.ps} represents a flowing event stream: besides its burst behavior (which is completely separate and we will get to later), a `River`{.ps} maintains a list of subscribers and **broadcasts the same value to all active subscribers**.

This is important for two main reasons: it ensures that multiple subscriptions to the same `River`{.ps} are seeing the same events, which is critical for maintaining a coherent display of DOM.

And it allows memoizing various functions, especially `<$>`{.ps} and `filterMap`{.ps}, so that the work they do can be shared between multiple subscribers automatically.
:::

:::Key_Idea
On the other hand, `Lake`{.ps} is a **description of how to create an event stream**, not a flowing event stream itself.

It can be instantiated on demand, and depending on when exactly that happens, you might get different event streams: either due to minor timing differences, or just due to fundamentally different circumstances.

For example, one *very* common aspect that makes a `Stream`{.ps} a `Lake`{.ps} is that it maintains internal state between callbacks: even something as simple as counting a stream with `counter :: forall flow a. Stream flow a -> Lake (a /\ Int)`{.ps} introduces internal state that depends on exactly *when* you started subscribing to upstream.
:::

::::

This article starts by discussing `Aff`{.ps} (similar to `IO`{.hs} in Haskell) as a familiar example of a functional asynchronous monad with a result type `Fiber`{.ps}, and then talks about how the distinction between flowing and not flowing streams is solidified in the design of `Riverdragon.River`{.ps} (my [FRP]{t=} event implementation) and even in `Riverdragon.Roar`{.ps} (a [FRP]{t=} front-end for Web Audio).

--------------------------------------------------------------------------------------------------------------
System                       Not flowing                          Flowing
---------------------------  -----------------------------------  --------------------------------------------
aff (asynchronous effects)   `Aff`{.ps}, `ParAff`{.ps}            `Fiber`{.ps}, of a running `Aff`{.ps} action

River (event stream)         `Lake = Stream NotFlowing`{.ps}      `River = Stream Flowing`{.ps}

Roar (audio graphs)          `Knob`{.ps} à la `AudioParam`{.js}   `Roar`{.ps}, output from an `AudioNode`{.js}

GHC (Haskell runtime)        `IO`{.hs}                            `Async`{.hs}, of a running `IO`{.hs} action
--------------------------------------------------------------------------------------------------------------

<!-- Table: Comparing “not flowing” and “flowing” variants of various functors in PureScript (and Haskell). -->

I want others to understand it as deeply as I do 💜

## A point of comparison: [`Aff`{.ps}](https://pursuit.purescript.org/packages/purescript-aff/8.0.0/docs/Effect.Aff#t:Aff) vs [`Fiber`{.ps}](https://pursuit.purescript.org/packages/purescript-aff/8.0.0/docs/Effect.Aff#t:Fiber)

Letʼs start at a familiar point in PureScriptʼs design space: with the [`Aff`{.ps}](https://pursuit.purescript.org/packages/purescript-aff/) library, and regarding the distinction between [`Aff`{.ps}](https://pursuit.purescript.org/packages/purescript-aff/8.0.0/docs/Effect.Aff#t:Aff) and [`Fiber`{.ps}](https://pursuit.purescript.org/packages/purescript-aff/8.0.0/docs/Effect.Aff#t:Fiber).

[`Aff`{.ps}](https://pursuit.purescript.org/packages/purescript-aff/8.0.0/docs/Effect.Aff#t:Aff) is very common in PureScript: it is the main way that we carry out asynchronous, effectful computations.
Of course there is [`Effect`{.ps}](https://pursuit.purescript.org/packages/purescript-effect/4.0.0/docs/Effect#t:Effect), which represents synchronous, effectful computations, and in JavaScript this is a particularly rigid restriction: there is no way^[at least in the browser – technically in node.js there are ways] to convert an asynchronous computation to a synchronous one.
And `Effect`{.ps}s cannot be interrupted by JavaScript code, so they are much simpler to deal with.
Computations running in `Aff`{.ps} may be interleaved with other things, they may be canceled while they are running, and other complexities.

But throughout this all lies the part that makes functional programming pure: `Effect`{.ps} and `Aff`{.ps} themselves are *not* running computations: instead, you should think of them as *descriptions* (from the programmer to the runtime) of *how to run* a particular computation.

Being values in a functional programming language, they may depend on values of variables in scope (since they are closures).
And since they are effectful, their result may depend on other state: any and all state that JavaScript has access to.
This includes local `Ref`{.ps}s, `localStorage`{.js}, other Web APIs like `fetch()`{.js}, and so on.

:::Bonus
You could make this a little more explicit by using a Free Monad over a base functor.
The base functor describes all the actions you are allowed to take.

However, there are limitless actions you are allowed to take, and free monads are slow to interpreted.
So [`Effect`{.ps}](https://pursuit.purescript.org/packages/purescript-effect/4.0.0/docs/Effect#t:Effect) is represented as “any JavaScript function that accepts no arguments, and is allowed to have effects” (whereas functions `->`{.ps} are any JavaScript function that accepts a single argument and is not allowed to have effects).

`Aff`{.ps} has its own representation in JavaScript (essentially an ADT), since it is actually managed by a [lightweight runtime](https://github.com/purescript-contrib/purescript-aff/blob/main/src/Effect/Aff.js) that schedules the synchronous steps of the asynchronous computations to run interleaved and handles errors and such.
:::

Of course, `Aff`{.ps} is commonly used for representing async APIs in JavaScript, which mostly use `Promise`{.js}s to represent their result.
However, `Aff`{.ps} is not analogous to `Promise`{.js}!
`Aff`{.ps} is a *description* of a computation *to run*, `Promise`{.js} is a way to await the result of an *already running*^[or completed] computation.

In fact, [`Fiber`{.ps}](https://pursuit.purescript.org/packages/purescript-aff/8.0.0/docs/Effect.Aff#t:Fiber) in the `aff` library is the type most analogous to `Promise`{.js}: it represents a way to await the result of an already running^[or completed, or suspended] `Aff`{.ps} computation.

Whereas `Promise`{.js} is a bare bones API that barely scraped by the JavaScript standardization process, `Aff`{.ps} is more fully featured, and `Fiber`{.ps}s allow more control over the `Aff`{.ps} they are spawned from:

- a `Fiber`{.ps} can be started immediately, with `forkAff`{.ps} or `launchAff`{.ps} or `runAff`{.ps} and so on
- or a `Fiber`{.ps} can be suspended until its result is demanded, with `suspendAff`{.ps} or `launchSuspendedAff`{.ps} or `runSuspendedAff`{.ps}
- importantly, `Fiber`{.ps} also allows *cancelling* the running computation with `killFiber`{.ps} and waiting for the cancelation to complete

  :::Note
  JS promises do not have any provisions for cancelling their computations, and thatʼs a major drawback of them in my opinion. Especially since it means that each individual API needs dedicated workarounds like [`AbortController`{.js}](https://developer.mozilla.org/en-US/docs/Web/API/AbortController/AbortController) and [`AbortSignal`{.js}](https://developer.mozilla.org/en-US/docs/Web/API/AbortSignal) and ugh, why is it so overcomplicated.
  :::
- finally, the aff API has a notion of supervising contexts

However, besides these bonus details, `Aff`{.ps} still runs on the callback model of JavaScript: just look at `makeAff`{.ps} and `runAff`{.ps}.
(Even `Fiber`{.ps} is [a bunch of callbacks in a `newtype`](https://github.com/purescript-contrib/purescript-aff/blob/v8.0.0/src/Effect/Aff.purs#L165-L173).)

More-than-incidentally, `Aff`{.ps} guarantees that its callback is only run once.
This (and error handling) is the main difference between `Aff`{.ps} and `Stream`{.ps}s in [FRP]{t=}.

## In Riverdragon [FRP]{t=}: `Lake`{.ps} versus `River`{.ps} (they are both `Stream _`{.ps})

:::Note
This article assumes you are familiar with monads and applicatives and such.
:::

The [classic [FRP]{t=} library in PureScript](https://pursuit.purescript.org/packages/purescript-event/1.2.4/docs/FRP.Event) is a port of [Haskellʼs reactive bananas](https://hackage.haskell.org/package/reactive-banana-1.3.2.0/docs/Reactive-Banana-Combinators.html).
`Event`{.ps} vs `Behavior`{.ps}, yeah.
`Event`{.ps} is a bad name, especially in JavaScript, because that already refers to a specific event that happened.
I also donʼt like the idea that it is “just a list” (bleh).
Itʼs clearly not! it is interactive and reactive and freshly evolving and yeah!

:::Note
I do not have an analogue of `Behavior`{.ps} yet. Maybe soon, but it is not so necessary.
Mostly because `Stream`{.ps}s (even `River`{.ps}s!) are given full reign to push their current value to subscribers immediately.
:::

Anyways, I wanted a different name, so I chose `Stream`{.ps}.
This is the river in Riverdragon (and the DOM is a dragon to deal with, rawr).

[The simplest encoding of an event stream](https://github.com/paf31/purescript-event/blob/v1.2.4/src/FRP/Event.purs#L39-L39) is the way it has always been done in PureScript:

```purescript
newtype Event a = Event
  ( (a -> Effect Unit) ->
    Effect (Effect Unit)
  )
```

- It takes a callback `(a -> Effect Unit)`{.purescript} to call for each value that occurs in the stream, as it occurs.
  (Note that these are typically synchronous callbacks. This is unlike `Promise`{.js}, which queues callbacks as [microtasks](https://developer.mozilla.org/en-US/docs/Web/API/HTML_DOM_API/Microtask_guide)!)
- It performs some `Effect`{.purescript}s to set up (install event listeners, register timers, create mutation observers, start any other number of asynchronous computations)
  - It may also call the callback synchronously! Like `pure` does, for example.
- And it returns a latent `Effect Unit`{.purescript}, which the caller can run later to cancel the subscription (remove event listeners, cancel timers, and so on)

It is conceptually simple and remarkably effective.

But something that always bothered me was the two ways to create an event:

- `create :: forall a. Effect { event :: Event a, push :: a -> Effect Unit }`{.purescript}, which is run from the outside, if you will: events are pushed in from the outside and forwarded to all current subscribers
- and `makeEvent :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Event a`{.purescript}, which sets up things for each subscriber

It turns out that weʼre working with a similar distinction: `makeEvent`{.ps} is like `Aff`{.ps} (a description of how to start an event stream) and `create`{.ps} is more like `Fiber`{.ps}.

So I decided to reify this distinction in the type system: in Riverdragon, `Stream :: IsFlowing -> Type -> Type`{.purescript} takes a type parameter to distinguish flowing streams (`type River = Stream Flowing`{.ps}, like `Fiber`{.ps}) from latent streams (`type Lake = Stream NotFlowing`{.ps}, like `Aff`{.ps}).

:::Note
You could make `create`{.ps} for `Aff`{.ps} too if you wanted.
It would operate with the condition that only the first call of `push`{.ps} would have any effect: later calls would be ignored.
(And in fact, if the `Fiber`{.ps} was canceled, even the first `push`{.ps} would be ignored.)
:::

It turns out that, just like Typed, Pure Functional Programming offers a lot of clarity over procedural/imperative programming, this style of Functional Reactive Programming offers a lot more clarity even over normal “[FRP]{t=}”!

*However*, before we get into that: we need to talk about initial values in streams.

### Bursts: Streams that start with an initial value (sometimes) (multiple?) (effectfully!)

One of the funny things about this tradition of [FRP]{t=} is that a big deal is made out of `Behavior`{.ps} *always* having a value, like `Time -> a`{.ps} would have.
But this simply isnʼt true in practice either: for example, a JavaScript `Behavior`{.ps} for mouse position doesn't actually know where the mouse cursor is when the page loads (and this is not unique to browsers or JavaScript: a lot of APIs for peripherals rely on events in OSes).

And conversely, it is often *very very* useful for events to start with a default value: if a subscriber joins late (say, because an event happens which means you need to rerender some stuff in the DOM, and that stuff is itself dynamic), it is incredibly convenient if not outright necessary sometimes for that subscriber to be immediately notified of a “current” or “default” value of the event it is subscribing to, so it never runs dry and isnʼt left dangling while waiting for the next event.

Like, this was the whole promise of [FRP]{t=}: your values should be encapsulated in one place, and anything that depends on it should be able to rely on its value and be able to update in reaction to those events!
If you need to plumb along default and current values outside of the `Stream`{.ps}, youʼve lost the plot.
It isnʼt about how well you can encode event callbacks, itʼs about the reactive values they represent.

So I allow an initial *burst* of events as a subscriber connects.
And in fact I encode it explicitly in the model.

(There are even a bunch of combinators for controlling the behavior of bursts.)

#### Justifying bursts in streams

How do I justify this special exception for initial “burst” events?

Well, itʼs not too difficult.

Our normal stream interface^[You could call it `BareStream` I guess] would have the familiar methods: `Functor.<$>`{.ps} for sure, `Apply.<*>`{.ps} to join latest values from two events, and `Alt.<|>`{.ps} to take events from whenever either of two streams fire.
It just wouldnʼt have `Applicative.pure`{.ps} to start with an initial value.

However, it is perfectly fine and cromulent to take the `Product`{.ps} of two functors: this preserves `Functor`{.ps}, `Apply`{.ps}, and `Alt`{.ps}.
However it also gives us `Applicative.pure`{.ps} now too.^[Okay, maybe this means that they are sort of twisted together? Or maybe it is more like `Coproduct Identity (Product _ BareStream)`. Idk, not actually important!]

The specific other functor we are taking the product with is going to be a “burst” functor, like

```purescript
-- | Not allowed to call its callback immediately!
type BareStream = ...

-- | A burst effect that returns some values
type Burst a = Effect (Array a)
type Burst = Compose Effect Array

-- | Now we can combine them
type Stream = Product Burst BareStream
```

And `Array`{.ps} already has well-defined `Apply.<*>`{.ps} behavior (take the Cartesian product of entries!), and it gives us `Applicative.pure = singleton`{.ps}.
And these both lift fine through `Compose Effect`{.ps}, since `Effect` also has `Applicative`{.ps} and this `Applicative`{.ps} can be used to lift `Alt Array`{.ps} through.^[Okay so for those keeping score at home: we are up to something like `Coproduct Identity (Product (Compose Effect ArrayButNotSingleton) BareStream)`, if we were to want to derive the construction through algebraic newtypes, essentially.]

So yeah, we should be justified in having all of the nice interfaces we want on our actual `Stream` type, accounting for burst.

### Consequences of `IsFlowing`{.ps}

Okay, now we can get back to the distinction between `River`{.ps} and `Lake`{.ps} and why it is so important!

As a refresher, `FRP.Event`{.ps} had two ways to create events, `create`{.ps} and `makeEvent`{.ps}.
Do you remember which was which?&nbsp;… yeaahh, thought so.

In Riverdragon now, we have two similar methods which are clearer about their roles:

- `createRiver :: forall a. Effect { destroy :: Effect Unit, send :: a -> Effect Unit, stream :: River a }`{.purescript} which is just like `FRP.Event.create`{.ps} but with the addition of a `destroy`{.ps} method which informs subscribers that no more values will be sent (End of Stream, or EOS).
  - `createRiverStore`{.ps} adds a `Maybe a`{.ps} parameter (used to configure an initial value, optional) and creates a river that stores its last value: when a new subscriber joins, it receives this most recent value immediately.
    There are other behaviors you can envision ([e.g.]{t=} you could track the whole history and replay it), but this is by far the most common.
- and `makeLake :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Lake a`{.purescript}, which is exactly like `FRP.Event.makeEvent`{.ps}.
  - following the tradition of using primes to denote altered versions of functions (usually just a little more or less complex), `makeLake'`{.ps} also allows gives a callback to indicate EOS (End of Stream)

These form the prototypical `River`{.ps} and `Lake`{.ps}.

:::Key_Idea
Whatʼs special about `River`{.ps}?
Well, like a `Fiber`{.ps} represents a running `Aff`{.ps} computation, a `River`{.ps} represents a flowing event stream: besides its burst behavior (which is completely separate and we will get to later), a `River`{.ps} maintains a list of subscribers and **broadcasts the same value to all active subscribers**.

This is important for two main reasons: it ensures that multiple subscriptions to the same `River`{.ps} are seeing the same events, which is critical for maintaining a coherent display of DOM.

And it allows memoizing various functions, especially `<$>`{.ps} and `filterMap`{.ps}, so that the work they do can be shared between multiple subscribers automatically.^[Of course this is a tradeoff, and for lots of simple functions there is more overhead to setting up a new `River`{.ps} than for calculating it for multiple subscribers.]
:::

:::Key_Idea
On the other hand, `Lake`{.ps} is a **description of how to create an event stream**, not a flowing event stream itself.

It can be instantiated on demand, and depending on when exactly that happens, you might get different event streams: either due to minor timing differences, or just due to fundamentally different circumstances.

For example, one *very* common aspect that makes a `Stream`{.ps} a `Lake`{.ps} is that it maintains internal state between callbacks: even something as simple as counting a stream with `counter :: forall flow a. Stream flow a -> Lake (a /\ Int)`{.ps} introduces internal state that depends on exactly *when* you started subscribing to upstream.
:::

### Improved Representation

The internal representation got more complicated.
The good news is that the surface API does not really need to reflect this complexity: it is more about internal bookkeeping and maintaining low friction inside the machinery, which can all be smoothed over at the interface to the outside world.

```purescript
data Stream (flow :: IsFlowing) a = Stream IsFlowing
  ( { receive :: a -!> Unit, commit :: Id -!> Unit, destroyed :: Allocar Unit } -&>
    { burst :: Array a, sources :: Array Id, unsubscribe :: Allocar Unit }
  )
```

We track `IsFlowing`{.ps} at the value level explicitly for two reasons: (1) it is good for keeping the types unconstrained by typeclasses, and (2) it is sometimes convenient/necessary to lie about it.

As mentioned above, explicitly representing burst events is good.
Particularly because it eliminates some nasty edge cases (“what if the subscriber unsubscribes during the burst?”).
And yeah, if you use `makeLake`{.ps} in a way that calls the callback during subscription, it gets collated into the burst of the `Stream`{.ps}, and vice-versa, if you `subscribe`{.ps}, you receive the burst events immediately.

And there is the `destroyed`{.ps} callback for End of Stream.

The `commit`{.ps} callback and `sources`{.ps} return are the interesting part for [the next section](#eliminating-events-resulting-from-transient-internal-states)!
Again, they are not something that consumers care about, they are just internally tracked.

### Eliminating events resulting from transient internal states

:::{.Example box-name="Thought Experiment"}
Consider this: `output = (+) <$> input <*> input`{.purescript}, where `input :: River Int`{.purescript}.

First you push the value `1`{.ps} through the `input`{.ps} stream.
Initially the applicative has no value for `input`{.ps}, so when the left subscription is called it just stores the value `1`{.ps}, and only when the right subscription is called is it able to output the value `1 + 1 = 2`{.ps}.

So far so good.

What happens when `input`{.ps} changes from `1`{.ps} to `2`{.ps}?

Well, in naïve implementations of [FRP]{t=}, you end up with a problem:

when the left subscription is called with `2`{.ps} now, it already has a value for the right subscription: `1`{.ps}! so it happily outputs `3`{.ps}. whoa?!

but wait, we still have to call the right subscription: okay now it sees `2`{.ps}, and the stored value on the left is also `2`{.ps}, so we output `4`{.ps} finally.

Pushing the events `1`{.ps} and `2`{.ps} through the `input`{.ps} event stream produced an event stream with events `2`{.ps}, `3`{style="color: #ff004d"}, and `4`{.ps}.

So apparently `i + i`{.ps} does not equal `(pure) 2 * i`{.ps} in naïve [FRP]{t=} land!
:::

This contrived example is the kernel of a lot of bugs that can manifest, especially in interactive UIs.
Say you want to record changes to various parameters derived from user input: under this model, each user event may send multiple updates, it would be a mess!

You might argue: why not introduce a new operator that ignores events from the left and only takes events from the right?
This is possible, and in fact a good idea: it exists as `(+) <$> input <?*> input`{.ps} in `Riverdragon.River`{.ps}.

But it doesnʼt actually solve the problem!
In general, it is basically impossible for [FRP]{t=} programmers to track exactly where values came from, and to partition out interactions of the upstreams that contributed to each event in order to do this kind of deduplication.
And again, we came here based on the promise that we would be working with *reactive values*, not to deal with the mechanics of event subscriptions.

#### Non-solutions

A very simple approach would be to delay events, by say a frame, and only take the latest value of events.
This is probably okay for some solutions, maybe even desirable, but it is far from a systematic solution.

Max/MSP has a convention that events always move [right to left](https://docs.cycling74.com/legacy/max8/tutorials/basicchapter05), and it is the leftmost inlet that triggers an output event in most cases.
Again, this is a workable solution, but a non-starter for us.

However, I knew it was possible to *just do it* because I had **done** it before:

```purescript
-- | Filter out events from one source from a larger event. That is, given
-- | events `e1` and `e2`, the following equation ought to hold:
-- |
-- | ```purescript
-- | notFrom e1 (e1 <|> e2) = e2 = notFrom e1 (e2 <|> e1)
-- | ```
-- |
-- | This relies on the events being well-behaved, mostly that they broadcast
-- | the same event to all of their subscribers in order of subscription.
notFrom :: forall a. Eq a => Event a -> Event a -> Event a
notFrom suspect downstream =
  -- Subscribe to `downstream` first, so events coming from `suspect` will
  -- come in later and take precedence on the left side of `sampleOn_`
  let instigator = Just <$> downstream <|> Nothing <$ suspect
  -- Only respond to events from downstream, of course, but with the above
  -- filtering
  in filterMap identity (sampleOnRight_ instigator downstream)
```

It turns out that this comment, almost 3 years ago, contained the kernel of the idea of a `River`{.ps}: “events [that] broadcast the same event to all of their subscribers (in order of subscription)”.
It just took refining the idea and codifying it thoroughly into the [FRP]{t=} system.

Another suggestion that the idea should be possible: if you had a centralized view of the entire [FRP]{t=} graph (say, like a graphical editor, like Max/MSP), of what source events there are and how they are connected to other nodes, you could perform the deduplication in a very systematic way: either moving values through it as a wave front, or ensuring that events are pushed synchronously but only accounted for when their downstreams have caught up.

This is the idea we want to mimic, but in a decentralized way, where streams and their subscriptions are still just anarchic functions that are called whenever.

#### Solving transient events

So how do we pull it off in Riverdragon?

Well, you could read the source of `combineStreams`{.ps} (the helper that is used for all of `<*>`{.ps}, `<*?>`{.ps}, and friends).

<details class="Details">

<summary>Source of `combineStreams`{.ps}</summary>

```purescript
-- | Combine streams according to selection logic, where each side can reject
-- | having its update result in a downstream event (note that it still updates
-- | the latest value internally!).
combineStreams ::
  forall flow a b c.
  These (a -> Boolean) (b -> Boolean) ->
  (a -> b -> c) ->
  Stream flow a -> Stream flow b -> Stream flow c
combineStreams logic comb (Stream t1 e1) (Stream t2 e2) = Stream (t1 <> t2) \cbs -> do
  -- only run the upstream destroyer once both have been destroyed
  destroyed <- threshold 2 cbs.destroyed
  lastValues <- prealloc2 Nothing Nothing
  needsPush <- prealloc false
  sourcesR <- prealloc Set.empty
  let
    cbL a = do
      lastValues.setL (Just a)
      needsPush.set true
      pure unit
    commitL id = do
      n <- needsPush.get
      l <- if n then fst <$> lastValues.get else pure Nothing
      Tuple shouldCommit shouldPush <- case logic of
        -- It is always our responsibility to commit
        This p -> pure $ Tuple true $ map p l == Just true
        -- Never commit
        That _ -> pure $ Tuple false false
        -- Only commit if this source is unique to us
        Both p _ -> do
          shouldCommit <- not Set.member id <$> sourcesR.get
          pure $ Tuple shouldCommit $ map p l == Just true
      when shouldCommit do
        commit id shouldPush
    cbR b = do
      lastValues.setR (Just b)
      needsPush.set true
      pure unit
    commitR id = do
      n <- needsPush.get
      r <- if n then snd <$> lastValues.get else pure Nothing
      Tuple shouldCommit shouldPush <- case logic of
        -- Never commit
        This _ -> pure $ Tuple false false
        -- Always commit
        That p -> pure $ Tuple true $ map p r == Just true
        Both _ p -> pure $ Tuple true $ map p r == Just true
      when shouldCommit do
        commit id shouldPush

    commit id shouldPush = do
      when shouldPush do
        Tuple a b <- lastValues.get
        case lift2 comb a b of
          Just c -> cbs.receive c
          -- Have not received a value on both sides
          _ -> pure unit
      cbs.commit id
  -- and only count each destructor once (just in case)
  cbs1 <- cbs { receive = cbL, commit = commitL, destroyed = _ } <$> cleanup destroyed
  cbs2 <- cbs { receive = cbR, commit = commitR, destroyed = _ } <$> cleanup destroyed
  -- subscribe to upstream
  r1 <- e1 cbs1
  r2 <- e2 cbs2
  sourcesR.set (Set.fromFoldable r2.sources)
  -- initialize from the burst
  lastValues.setL (Array.last r1.burst)
  lastValues.setR (Array.last r2.burst)
  pure
    -- TODO: burst logic?
    { burst: lift2 comb r1.burst r2.burst
    , sources: bifoldMap (const r1.sources) (const r2.sources) logic
    , unsubscribe: r1.unsubscribe <> r2.unsubscribe
    }
```

</details>

But donʼt read that.
The implementation got much much longer than I wanted&nbsp;…

Anyways, the main idea is that each event declares a list of “source IDs” that it will emit events from.
Each event is sent first via `receive :: a -!> Unit`{.purescript} then via `commit :: Id -!> Unit`{.purescript}.

Importantly, the upstream gets to call them immediately, in order: `cbs.receive a <* cbs.commit id`{.purescript}.
Because all of the callbacks happen synchronously, between `cbs.receive a`{.purescript} and `cbs.commit id`{.purescript}, all of the subscribers have been called and notified: thus we already know the right time to push the *real* events through the [FRP]{t=} graph, with `cbs.commit id`{.purescript}.

Then the job of `combineStreams`{.ps}/`<*>`{.ps} is to maintain the most recent values and listen for the correct `commit` event to send its own updated value out (a `receive`{.ps} call), and then pass on the `commit` event too.
(Technically `<*>`{.ps} just needs to listen to `commit` on one or the other side, for IDs that are shared, but `combineStreams`{.ps} needs more logic due to how generic it is.)

The mechanism reveals the invariant: a source ID denotes an event stream that may push its own events into the [FRP]{t=} graph, synchronously.
So if you have one stream that delays events from another stream, it needs its own source ID for those delayed events, even though they have the same values as events from an existing source ID.

It is also very important that streams that may filter out events still must pass their commit events through, since they still count as sources.
This includes `<*>`{.ps}, since it is not guaranteed to have an initial value from either side!
These are equivalent streams: `empty <*> upstream`{.ps} and `filter (const false) upstream`{.ps} and they both push `commit`{.ps} events through from `upstream`{.ps}, but to outside observers, they act like `empty`{.ps} since they never send a value.

Another important fussy little detail is that source IDs cannot be declared up-front, only after subscription.
This is maybe the main divergence from the idealized “[FRP]{t=} graph is just a data structure” view, which would have static source IDs up front.

#### Case Studies

##### Global reactive values like device pixel ratio

Alba pointed me to this recipe on MDN for [watching changes to `devicePixelRatio`{.js}](https://developer.mozilla.org/en-US/docs/Web/API/Window/devicePixelRatio#monitoring_screen_resolution_or_zoom_level_changes), so I quickly implemented it in `Riverdragon.River.Beyond`{.ps}.

```javascript
export const _devicePixelRatio = {
  now: () => window.devicePixelRatio,
  subscribe: cb => () => {
    let active = true;
    let rolling = () => {};
    const untilNext = cb => {
      const mediaQueried = window.matchMedia(`(resolution: ${window.devicePixelRatio}dppx)`);
      mediaQueried.addEventListener('change', cb);
      rolling = () => mediaQueried.removeEventListener('change', cb);
    };
    const onChange = () => {
      rolling();
      if (!active) return;
      cb(window.devicePixelRatio)();
      untilNext(onChange);
    }
    untilNext(onChange);
    return () => { active=false; rolling() };
  },
};
```

```purescript
foreign import _devicePixelRatio ::
  { now :: Allocar Number
  , subscribe :: (Number -> Allocar Unit) -> Allocar (Allocar Unit)
  }

devicePixelRatio :: River Number
devicePixelRatio = River.mayMemoize $ River.unsafeRiver $ makeLake \cb -> do
  cb =<< _devicePixelRatio.now
  _devicePixelRatio.subscribe cb
```

The reason it is a river is because it is a single global value that we are listening to: all subscribers see the same events. But the call to `River.mayMemoize`{.ps} is the real magic there, it

- ensures that we only subscribe to one media query at a time^[possibly better for efficiency?],
- allowing the events to take place as a single source within the FRP graph, so we can remove the transient events

Without `River.mayMemoize`{.ps} there, this snippet would produce transient events showing nonsense like “1.5789473684210527 = 1.3333333333333333” (as the user zooms in from `1.33` to `1.57`):

```purescript
D.ol[] $ D.Appending $ dam ado
  l <- devicePixelRatio
  r <- devicePixelRatio
  in D.li[] $ D.show l <> D.text " = " <> D.show r
```

<!-- (TODO: add a run button to run this directly here??) -->

You can paste that expression at [Live FRP DOM Coding](live_frp.html).^[Yeah, thatʼs all the code you need to try it out! it gets templated into a full module with a header, imports, declarations, and so on]

### Benefits of `IsFlowing`{.ps}

The main benefit of `IsFlowing`{.ps} is that multiple subscribers know they are seeing the same values at, well, similar points in time (not exactly the same, but synchronously close together).
(And again, the instantiation of a `Lake`{.ps} to a `River`{.ps} produces a burst which it is valuable to capture explicitly.)

One of the things that this elucidated for me was the signature of `fix`{.ps}, which has always been mystifying.

```purescript
fix :: forall o i. (Event i -> { input :: Event i, output :: Event o }) -> Event o

fix :: forall o i. (River i -> { loopback :: Stream _ i, output :: Stream _ o }) -> Lake o
```

Now the types almost tell the story: the output is a `Lake`{.ps} since it is created on demand (and indeed, its state will depend on when you subscribe to it).
You then receive a loopback stream which is already running: you get to mix in your own events that youʼre bringing it to produce the loopback stream you want to see and the stream of output events that downstream subscribers will see, and these two streams can be `Lake`{.ps}s since they are only subscribed to once (and then forwarded to other subscribers).

`fix`{.ps} is actually a really important primitive and could be used to implement such staples as `foldStream`{.ps} and `statefulStream`{.ps}, though they are implemented on their own^[for efficiency? idk].

`singleShot`{.ps} is a funny one: it is conceputally very simple as it is just a stream that destroys itself after a single event&nbsp;… but even it turns any stream into a `Lake`{.ps}, for it matters when the observer starts watching for events.

Outside of the typeclass-provided combinators (`Applicative`{.ps}, `Alt`{.ps}, and `Filterable`{.ps}), just about the only combinator that preserves `Flowing`{.ps} streams is `delay`{.ps}, and itʼs on thin ice.
Notably, `delay`{.ps} has a different weird aspect that it generates a new stream ID, because its events are not synchronous with upstream.

### Benign (allocation-like) effects `Allocar`{.ps}

`Allocar`{.ps} really exists as a result of `instantiate`{.ps}, a function which turns a `Lake`{.ps} into a `River`{.ps}, and its twin `withInstantiated`{.ps}.
In truth, it is executing effects: not only does it need to set up a new `River`{.ps} to proxy events from upstream to all downstream subscribers, but it needs to subscribe to the upstream `Lake`{.ps} which is technically effectful: it *shouldnʼt* do anything weird, but it technically could.

In fact, if you think about the analogy of a pure [FRP]{t=} graph: the graph can exist on its own as a pure data structure, like `Aff`{.ps} is a pure description of how to theoretically run a computation.
Itʼs only in the process of actually *subscribing* to nodes in the [FRP]{t=} graph that it becomes a monster capable of producing effects on the outside world.
So in theory, all `instantiate`{.ps} should be doing is allocating a reference to a new node in the graph and wiring it up.
(In fact: consider how it works exactly this way for Web Audio Nodes! Nothing *really* happens until a node producing audio is connected to an [`AudioDestinationNode`{.js}](https://developer.mozilla.org/en-US/docs/Web/API/AudioDestinationNode): everything else would just be internal state ticking away.)

`Allocar`{.ps}, then, is for the “glue” effects that take place *inside* the [FRP]{t=} graph and donʼt affect the outside world themselves.
Lightly scoped effects of allocations, bookkeeping, and other preparations.

This is important in transforming `instantiate`{.ps} (the direct representation as an effect) to `withInstantiated`{.ps}, which performs the effect as a `Lake`{.ps}.
Because a `Lake`{.ps} is already allocating stuff for its subscriber, it makes sense that it should be able to easily allocate similar resources, and bring subscriptions to other streams along for the ride.

So how would I define `Allocar`{.ps}?

The prototypical example of a benign effect would be `Ref.new`{.ps}.
It allocates a mutable cell (really just a JavaScript object) which is effectful only because it has a distinct referential identity, not because any other code in the world cares about this new `Ref`{.ps}.

But even things like `addEventListener`{.ps} are (for the most part^[There are some weird exceptions where having particular event listeners on DOM elements makes them be clickable on iOS, or `MessagePort`{.js}s are buffered until `.onmessage`{.js} is set *or* `.start()`{.js} is called]) benign: their effects are scoped to the caller and not visible to outside observers, inasmuch as the callback behaves that way.

If it was just about things like `Ref`{.ps}s, in theory you could use `ST`{.ps} to scope the effects in a safe way.
But how would you scope `ST`{.ps} to a region, when the region is&nbsp;… the entire [FRP]{t=} graph?
(Technically you could, but it is very much not worth it.)

Even doing as much as making `Allocar`{.ps} a newtype instead of a type synonym would cause so much friction to make it unbearable.

Note that in the first case, the `Ref`{.ps} has no need for explicit resource management: being garbage collected by the JavaScript VM is enough.
But `addEventListener`{.ps} needs to be paired with` removeEventListener`{.ps} explicitly.
So the lite rule is that anything that runs in `Allocar`{.ps} should be (able to be) paired with a destructor, but formalizing this in a proper monad (like `ScoreM`{.ps}) is also an uphill battle with diminishing returns.

:::Bonus
Why the name “allocar”?

Well I wanted something softer than “allocate”, and I could have gone with like an Italian “[allogare]{lang=it}” but then Iʼm not actually saving any characters, and it turns out that “[alocar]{lang=es}” does not exist in Spanish either? they seem to use “[asignar]{lang=es}”. so idk. Itʼs just a made up word!

Also I used the symbol `&`{.ps} for some of the operators as a nod to the address pointer symbol.
:::

#### [OOP]{t=} but done better

`Riverdragon.River.Bed`{.ps} is my attempt at creating a library to make working with `Allocar`{.ps} (and `Effect`{.ps}!) pretty pleasant. From the module header:

> This module is all about helpers for managing the lifecycles of variables with various semantics (replacement, accumulation, thresholds, and so on).
>
> It is sort of “[OOP]{t=} but done better”: each allocation of a variable runs in `Allocar`{.ps} and returns a bunch of instantiated methods encapsulated in a record. It offers great abstraction, no manual handling of refs and such.
>
> It actually does a decent job of being inlined by the backend-optimizer, with some help from inlining directives. It does not quite *look* like something a dev would write (there are spurious constant declarations), but it should perform similarly or identically.
>
> It is just really cute and nice and convenient!

One of the main ideas is that I donʼt *want* to use mutable variables: you can do anything to mutable variables, who knows whatʼs happening!
Instead I want to create self-contained APIs that capture total behaviors on their own.

(The worst part of it is just the naming. I was unhappy with the inconsistent naming of `STRef`{.ps} vs `Ref`{.ps} versus other things. But Iʼm not sure I did much of a better job of it myself, yet.)

Some highlights:

- `accumulator`{.ps} is a staple that just keeps accumulating values from a monoid with `.put`{.ps}, until you call `.reset`{.ps} to pop out what you have and start anew, or nondestructively `.get`{.ps} what you have.
  So you get the nice guarantee that you are always mutating it by accumulation, you canʼt accidentally ignore the previous value of the variable.
  (Unless you purposefully ignore it with `.reset`{.ps} and then `.put`{.ps}.)
- `postHocDestructors`{.ps} lets you `.track`{.ps} destructors from other resources to call later, or, if it was already destroyed, call them immediately. This is important when allocating resources asynchronously, like `AudioWorklet`{.js}s
  - `cleanup`{.ps} it its little sibling, who just makes sure that a destructor effect is only called once.
    Again, it is effectful to make the reference to the “new” destructor, but it is super benign (and in some cases, may act like `pure`{.ps}!).
    - `threshold`{.ps} if you donʼt want it to happen the first time, but on the \(n\)th time.
- `rolling`{.ps} is the opposite idea: instead of gathering destructors, it rotates them, calling the last destructor when a new destructor arrives for next time.
  Again, just a little object that encapsulates the right behavior, instead of having a mutable variable floating around that can be set to whatever new thing without any protocol!
  (Unfortunately, you often want a sort of bracketing behavior, where you tear-down before creating the new thing.)
- `diffingArraySet`{.ps} lets you `.swap`{.ps} in a new array and returns what was `added`{.ps}, `removed`{.ps}, or `still`{.ps} present from the previous state (and can also be `destroy`ed to refuse further updates).
  This is used to keep track of audio connections! So an audio node input can subscribe to `Lake (Array Roar)`{.ps} (a changing stream of simultaneous audio signals).
- `subscriptions`{.ps} is a heavy-hitter that handles keeping track of subscribers and sending events to the current list of them (rather, it lets you traverse them) in order.
- `eventListener`{.ps} is one that interfaces with a web API, just registers an event listener and returns the unsubscribe function: again, I consider it an `Allocar`{.ps}-style effect, not a full-fledged `Effect`{.ps}, because it hardly ever has outside effects.
- `runningAff`{.ps} is like `launchAff >== joinFiber`{.ps} but more lightweight (in particular, it tries to only keep the settled value around, not any closures).

The ultimate goal might be to make a monad (transformer) that manages effects, particularly for users of the [FRP]{t=} libraries, more than for implementing the [FRP]{t=} itself.
But for now it has been more convenient to manually track destructors, and just provide the most convenient way to make sure they are called during teardown.
It would just be annoying to have both styles side-by-side…
`ScoreM`{.ps}, for building audio graphs, is a step in the right direction: it maintains `destroy :: Dual (Effect Unit)`{.ps} for calling destructors in reverse order, and `ready :: ParAff Unit`{.ps} for running effects when the rest of the audio graph is ready and waiting for everything to be up and running.

## Web Audio in [FRP]{t=}: `Roar`{.ps} and `Knob`{.ps}

So I started working on adding Web Audio support to Riverdragon, naming it `Riverdragon.Roar`{.ps}, and I realized that this same distinction pops up!

`Knob`{.ps}^[(Suggestions for cuter dragon names for `Knob`{.ps}?)] is a *slightly* nicer interface for `AudioParam`{.ps}.
Unfortunately, there is just no great interface for them (partly for understandable reasons, partly for [really really unfortunate reasons](https://bugzilla.mozilla.org/show_bug.cgi?id=1308431)).

So the deal is that an `AudioParam`{.ps} has two main things that can contribute to it: besides a default value, there are various audio parameter commands that create a timeline, and this is *summed* with contribution from connected audio nodes.

- Parameters of `defaultValue`, `minValue`, and `maxValue`, set by the parameter node.
- Audio timeline, controlled by the various commands.
  - This is hidden state! You may command the timeline, but you canʼt directly access it. Grrr.
  - This means that you kind of have to wrap the API, track the commands, simulate them... it is just not worth it.
- Connected audio inputs.

:::{.Details box-name="Side note"}
The A-Rate (Audio Rate?) versus K-Rate (Kontrol Rate?) is mostly irrelevant, except for performance and algorithmic limitations.
:::

Connecting audio inputs to audio parameters is both very important, because it allows precise and arbitrary control of audio parameters at audio rate, and also very annoying: it means you cannot predict the value of an audio parameter at a point in time, in general, because that would require simulating audio arbitrarily far out into the future, and for stateful audio like reverb or even just lowpass filters, that would be a nightmare.

Anyways, this means that you can convert an audio signal into an audio parameter, using `AudioNode.prototype.connect(audioParam, outputIndex)`{.js} (you canʼt use the three-argument method – ask me how I know!), and `ConstantSourceNode`{.js} is the perfect tool for converting an audio parameter into an audio signal.

Hopefully you can see where this is going:

:::Key_Idea
A `Knob`{.ps} is a *description* of how to control an audio parameter or audio signal.

A `Roar`{.ps} is an actual running audio signal, which is particularly important for being sample-accurate now.

And also because `Knob`{.ps} can incorporate feedback&nbsp;… havenʼt quite figured out how that is supposed to work exactly.
Partly because `AudioParam`{.js} is just an unpleasant interface to work around.
But the feedback isnʼt “real” until it is reified to a particular `AudioParam`{.js} at a point in time.
:::

TODO: bake the audio graph into the [FRP]{t=} graph? idk, maybe stuff to do around destroying streams and such.

### Rant on the deficiencies of `AudioParam`{.js}s

Really one of the biggest problems of AudioParam is that it is a “do anything” class with ~7 different ways to schedule values on the timeline – one of which is [not implemented in Firefox](https://bugzilla.mozilla.org/show_bug.cgi?id=1308431)! and it is not clear if the others are properly implemented, because there just isn't a good spec for how these things should all cohere (they donʼt.)

And I guess that the `.value`{.js} of an `AudioParam`{.js} does not depend on the audio signals feeding into it? That seems wrong... but Iʼm not sure if either answer is _good_, per se.

The solution, as always, is to express coherent thoughts first, and then compose those atomic behaviors into a more nuanced picture.

For example, an ADSR is a coherent thought. Sure, there is still behavior and edge cases to specify. but it is driven by a series of rising and falling events, and it responds to those with preset attack, decay, sustain, and release curves. Or rather, not preset curves, but preset *behaviors* at those intervals (the exact curve may depend on the value at a point in time).

For example, if you wanted an ADSR in cents (hundreths of a semitone of pitch), you could later map it to frequency with an exponential function. (Fortunately the Web API doesn't require this, and it allows you to specify cents beside frequency in the relevant places)

So the goal of the front-end of `Riverdragon.Roar.Knob`{.ps} is to express these coherent behaviors in single functions, and then figure out how to implement them.
(Right now it is limited to very simple behaviors like ADSR, linear interpolation, exponential decay&nbsp;… and they arenʼt even correct yet, there are glitches in the linear stuff.)

## Differences

First of all it is worth explaining why `Effect`{.ps} does not fit into this flowing-and-not-flowing model.

At runtime, `Effect`{.ps} is a synchronous (and effectful) JavaScript function.
As such, control flow proceeds by calling it, and then it either exits with a return value, or throws an error (which may or may not be handled by a `catch`{.js} in some caller).
This means that it cannot be interrupted by means within JavaScript: the JavaScript execution context itself may pause and resume later, it may crash due to out of memory or other OS limits or signals, but there is no JavaScript API to interrupt it, no way for other code to interact with a pending result that is not already async (through web workers or worklets or so on). So that is why there is no `Fiber`{.ps} equivalent for `Effect`{.ps}.

Next, the main difference between `Aff`{.ps}/`Fiber`{.ps} and `Stream`{.ps} (`River`{.ps}/`Lake`{.ps}) (both based on the callback model) is of course that `Stream`{.ps} allows calling the callback multiple times and a `Fiber`{.ps} only resolves one.
`Aff`{.ps} is also more tailored to *being a computation*, instead of representing interfaces between independent running things: it has supervision contexts and more detailed error handling (although no polymorphic errors yet, darn).
`Stream`{.ps} only has the concession that it allows an End of Stream notification: if you really wanted to represent an error you could use `Stream _ (Either Error yourData)`{.ps} and send a `Left`{.ps} error right before EOS.

`Roar`{.ps} and `Knob`{.ps} are obviously distinct in representing audio data, which is far too much data to process in this model of JavaScript, which is particularly expensive to compute because of all of the callbacks and context switching (and general indirection of PureScript as compiled to JavaScript).
So instead, it just represents audio nodes and high level commands, and delegates to Web Audio to handle the computation.
Internally, the Web Audio framework uses an audio quantum of 128 samples (each sample is a 32-bit float) and passes those quanta around within the audio graph in an optimized and synchronized manner.
Itʼs very cool actually, just a bit frustrating that the API isnʼt nicer to work with, and the necessity of using AudioWorklets.
