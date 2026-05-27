---
title: All the subtypes of streams in Riverdragon
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026/05/05
---

The `Stream`{.purescript} functor walks a fine line between “sending events” and “having values, reactively”.
And thatʼs on purpose: you often need the two concepts to interact closely.

How does state change?
In response to events!

Sometimes those events include a new value, replacing the old one wholesale.
Other times they alter the state in a different way.
And sometimes there is no coherent notion of state, it is just transient events passing through a channel, directed to a particular subscriber or broadcast to all.

Sometimes subscribing is silent, just an acknowledgement that when new events come through, someone wants to know about it.
Often subscribing means joining into a stream of reactive state, so the joining includes the latest state, if it is available.
Sometimes the whole history ~~flashes in front of your eyes~~— *ahem,* is replayed at the moment of subscription.

Accounting for all the possible behaviors makes it a bit difficult to optimize.
Different interfaces rely on those behaviors, so they all must fit together between point A and B, from where the event originates to wherever it is acted upon.
It is also difficult to conceptualize the nuances.

But at the end of the day, `Stream`{.purescript} is an interface with expectations for sensible behavior:
the programmer is still responsible for knowing the details of how to apply it, what matters when.
It just helps to be functional, knowing what functions are stateless and which need to be instantiated effectfully.

## Categories

### Burst events

First off, burst events

- There are injections `Array t -> River t`{.purescript} and `Effect (Array t) -> River t`{.purescript}.
- These are event streams that output a burst to new subscribers, but do not give any new events.
- While the events within a single stream are ordered, there is no ordering in time between separate streams, thus only a partial order on their cartesian product.
  (`x1,y1` weakly occurs before `x2,y2` if `x1` weakly occurs before `x2` and `y1` weakly occurs before `y2`.)
- In particular, the caller cannot call an unsubscribe function while handling burst events: the unsubscriber isnʼt available yet, it hasnʼt been returned!
- The effects to generate the burst should be benign, though.
  Ideally not globally observable, just getting some mutable state.
- Note that `Effect (Array t)`{.purescript} still qualifies as a `River t`{.purescript} even if it does not return the same array to all subscribers!
  That is the main restriction for a `River`{.purescript}, but it does not apply to the initial burst of events.
  (I mean, how could it? Two subscriptions donʼt occur at the same moment in time.)

### Lakes

`Lake t`{.purescript} is the next simplest conceptually:

- It is modeled by the type `Effect Unit -> (t -> Effect Unit) -> Effect (Effect Unit)`{.purescript}
- It receives two callbacks, **`receive`{.purescript}** to receive the values of type `t`{.purescript}, and **`runDry`{.purescript}** to indicate end-of-stream (EOS)
- The function then sets up any state it needs and runs some effects, then it returns a _destructor_ that unsubscribes from further events
- It can call the callback immediately (synchronously) with values, to give “burst” events (like `Array t`{.purescript} / `Effect (Array t)`{.purescript} from earlier)
- It can establish any other methods to provide values to the callback: timers, event listeners, remote connections, and so on
- It generally _should not have other noticeable effects_ from just subscribing
  - `join $ subscribe stream mempty`{.purescript} (which immediately unsubscribes after doing nothing with the burst events) should have no observable effects, ideally

    :::Warning
    One exception to this is if the underlying API is insufficiently reactive and, while processing a new subscriber, it polls a new value or finds missed events, it is okay to notify the other subscribers too.
    That is, if the event was morally supposed to happen “soon” (if caught), it can happen during subscription.
    :::
- After it calls the end-of-stream callback, or after the caller calls the returned unsubscribe function, it should never call the callbacks
- It should unregister whatever callbacks it gave to timers and so on
- Indeed, it should do its best to forget those callbacks completely, to allow garbage collecting those closures and data referenced by them

```purescript
-- | Make a lake whose event stream gets allocated per subscriber, e.g. for
-- | timeouts or the like.
makeLake :: forall a. ((a -!> Unit) -> ResourceM Unit) -> Stream NotFlowing a
makeLake = makeLake' <<< const

makeLake' :: forall a. (Effect Unit -> (a -!> Unit) -> ResourceM Unit) -> Stream NotFlowing a
makeLake' streamTemplate = Stream NotFlowing \cbs -> do
  id <- globalId
  loadingBurst \whenLoaded -> do
    unsubscribe <- map _.destroy $ start_ "makeLake'" $ streamTemplate cbs.destroyed do
      -- Grab `a` for `burst :: Array a`, or if loading is done
      -- then give it to the callback
      \a -> whenLoaded a do
        cbs.receive a
        cbs.commit id
    pure { burst: _, sources: [id], unsubscribe }
```

A ton of manipulations of streams result in `Lake`{.purescript}s.
Even otherwise benign effects like `dedup`{.purescript} result in streams that are dependent on when subscription occurred.
If `upstream`{.purescript} looks like `1 1 3 4 4 5`{.boo}, `dedup upstream`{.purescript} from the start produces `1 _ 3 4 _ 5`{.boo}, but if someone subscribes to it after the first event, they see `1 3 4 _ 5`{.boo} instead of `_ 3 4 _ 5`{.boo}.

You often want to instantiate a `Lake`{.purescript} into a `River`{.purescript}, perform some manipulations that end up turning it back into a `Lake`{.purescript}, then instantiate the result into a `River`{.purescript} again so downstream consumers see consistent events/state.

This is especially important for `statefulStream`{.purescript}, so all subscribers see a unified state, instead of one that depends on when they subscribed to it and thus what events they happened to catch (in the case of a `River`{.purescript} – or, for `Lake`{.purescript}s, what events might have been generated for them).

```purescript
-- | A stateful stream that can update its own state and emit values.
statefulStream :: forall flow a b s.
  s -> Stream flow a ->
  (s -> a -> { state :: s, emit :: Maybe b }) ->
  Lake b
statefulStream b0 (Stream t stream) folder = Stream t \cbs -> do
  current <- mintCell b0
  stream $ cbs { receive = _ } \a -> do
    { state: b, emit: c } <- folder <$> current.get <@> a
    current.set b
    case c of
      Just v -> cbs.receive v
      _ -> pure unit
```

However, if there is only one consumer, it does not matter: consuming one `Lake`{.purescript} is equivalent to turning it into a `River`{.purescript} and consuming that.

And it is important to be careful to instantiate the `Lake`{.purescript} in the correct way: `foldStream`{.purescript} will not have the correct behavior when instantiated as a river ordinarily (that is, in the way that copies the burst behavior from the original stream), it needs to be instantiated as a store specifically, to see the latest folded value instead of the `b0`{.purescript} burst that kicked off the `Lake`{.purescript}.

```purescript
foldStream :: forall flow a b. b -> Stream flow a -> (b -> a -> b) -> Lake b
foldStream b0 upstream folder = pure b0 <|> statefulStream b0 upstream
  \b a -> folder b a # emitState

-- | Helper: always emit the state.
emitState :: forall state. state -> { state :: state, emit :: Maybe state }
emitState state = { state, emit: Just state }
```

### Rivers

`River t`{.purescript} is the other basic stream type

- It is exemplified by the type `{ send :: t -> Effect Unit, stream :: River t }`{.purescript}, which must be created effectfully
- Internally it maintains a list of all current subscribers and (key word) __broadcasts__ the same events to those subscribers
- Especially for this type of river, all subscriptions share the same source ID and broadcast synchronously through the whole network, so
for rivers, `r + r`{.purescript} and `(2 * _) <$> r`{.purescript} are equivalent, for example

More generally, a `River`{.purescript}ʼs contract is that after the initial burst, they broadcast the same event to all subscribers.
(Ideally with the same source id? not clear. Maybe the property is that it is okay to merge source ids: if it didnʼt matter, it shouldnʼt matter.)

Besides the plain `createRiver`{.purescript}, there is also `instantiate`{.purescript}.

Some operations like `map`{.purescript} and `filter`{.purescript} (indeed, `filterMap`{.purescript} and `mapArray`{.purescript}) are stateless, just transforming events individually, so they are safe to resubscribe to the upstream directly.

Furthermore, it is even safe (and sometimes desirable) to `memoize`{.purescript} these `River`{.purescript}s: using `createRiver`{.purescript}^[actually `_createProxy`{.purescript}] to create an output that broadcasts events from a single subscription upstream to any number of subscribers downstream, even removing the upstream subscription when there are no downstream subscriptions (this is one thing that `River`{.purescript}s provide).
This avoids recomputing expensive mapping and filtering functions for multiple subscribers.

### (River) Stores

I distinguish two main types of stores: river stores and stores.

This comes from the naming scheme that evolved:

```purescript
createStore :: t -> Effect { send :: t -> Effect Unit, stream :: River t, current :: Effect t }
createRiverStore :: Maybe t -> Effect { send, stream, current :: Effect (Maybe t) }
createRiver :: Effect { send, stream }
```

Their semantics are pretty simple: new subscribers see the more recent value immediately.

The difference is just that stores are guaranteed to come with an initial value, while river stores may not have one.
This matters at the type level and also for ergonomics downstream.
That is, if youʼre combining a lot of streams with `<*>`{.purescript}, you probably want to know that they all come with initial values so the output populates immediately, or at least so events from other branches are not ignored.
Before stores, Riverdragon, bursting behavior, my experiments with FRP front-ends suffered from that bug a lot more.

`dedup`{.purescript} is pure for stores and river stores, but not for rivers in general
and it specifically needs to dedup by `Eq`{.purescript}: approximate equality or congruence will not do



### Historyful Rivers

A more exotic type of river would be one that remembers all of the events that passed through it and bursts it to new subscribers.

Possibly you want to limit the size of the buffer and discard or summarize old events.
But the issue is that you donʼt know how to summarize the information that each subscriber might want to glean from it.
You could use monoids and semigroups, but knowing the right monoids and semigroups to cover all the subscribers?
And there isnʼt a dedicated type for first-event, sometimes the subscriber does need all the events anyways.

Maybe you want to remember and replay the first and last event.

Or maybe you want to drop events that have been “fully processed” in some way: fully negotiated/transmitted remotely/flushed to storage or something.
And possibly just replay the first and last event there too.

### Only-Once

Only-once events are a little interesting too.

- You can have `Promise`{.javascript}-like Rivers, that resolve once during their lifetime, and then resolve immediately or near-immediately for later subscribers.^[Promises always resolve as microtasks in JavaScript, in contrast to how `Aff`{.purescript} in PureScript resolves sequential `Effect`{.purescript}s synchronously.]
  This would count as an only-once river store, because it remembers the value but did not start with one.
- Or you can have a `River t`{.purescript} that only resolves once during its lifetime, to its then-active subscribers, before forgetting the value so all later subscribers see an empty stream.
- You can have a `Lake t`{.purescript} that delays some benign `Effect t`{.purescript} by a timeout or something.
- And of course you can `limitTo 1`{.purescript} any stream to produce an only-once `Lake t`{.purescript}, though you may want to delete the burst behavior before you do that

### Non examples

- Subscribing should not send events to other subscribers, nor should unsubscribing
  - This also means that subscribing and immediately unsubscribing should not have noticeable side effects
  - If you need to keep track of what things are subscribed, use an interface within `ResourceM`{.purescript} to track subscription and unsubscription while creating a normal `River`{.purescript} to return.

## Purity

- `memoize :: River ~> River`{.purescript}, basically a pure version of `instantiate`{.purescript}
- `mailbox :: River { key, value } -> (key -> River value)`{.purescript} similarly
- `dedup :: RiverStore ~> RiverStore`{.purescript} only if the last event of the burst is the last event that has been broadcast, so for stores and river stores and historic rivers
  - this might be the only viable example?

In general, the reason that `Flowing`{.purescript} is tracked at the type level is to aid in designing robust APIs, and the reason that it is tracked at the value level is so that `memoize`{.purescript} can be automatically applied around `filterMap`{.purescript} and the like.
