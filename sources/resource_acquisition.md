---
title: Forms of resource acquisition
subtitle: A monad and case study
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/09/13, 2025/12/29
---

The main takeaway from this has been that instead of thinking about individual resources and their creation conditions and destructors, itʼs been more fruitful to think about it as consisting of _scopes_ of resources, which can be destroyed when they are no longer needed.
Resources only exist as destructors and wait-functions to call in the scope.
This works much better since we are operating at the level of a monad abstraction.

Thinking of gestalt scopes also allows waiting for the entire scope to be ready, in asynchronous ways: forwards and backwards and in parallel as necessary.

## Case Studies

### WebAudio

Web Audio is a little funny:

You create audio nodes, hook up subscriptions... and then start and stop sources.

This is all synchronous.
However, *loading* audio *worklets* (which are required for lots of advanced audio processing techniques) is asynchronous.
You ask the audio context to load some modules (at any time you choose), and they can each register some audio processors by name (at any time they choose: most often synchronous with the module loading), which you then reference in the synchronous `AudioWorkletNode`{.js} constructor.^[I donʼt think there is a way to list which processors have been registered? Ugh I donʼt like the API…]

So when creating a worklet node, you have to wait for the module to load first.

(You could preload, but thatʼs no fun.)

There is a slight workaround, however: you can synchronously create an output node (say, a gain node used as a passthrough), and when the worklet has loaded, then you connect the new worklet to the output.
(In my system, you donʼt need a virtual input node, because downstream nodes are responsible for their subscriptions, but in other systems you might need input available like output.)

So the creation of an output node is synchronous, so everything downstream has something to subscribe to.
Then you wait for audio worklets (and whatever else) to finish loading, hooking them up internally to everything.
*Finally* you start the sources in the graph, once you know at this time that everything has been initialized.

So thereʼs kind of three phases:

- Create output nodes, sync
- Wait for everything to come online, async (and in parallel!)
- Start everything, sync

(I think my current implementation conflates the last two … whoops.)

Finally, you can destroy everything.
Thereʼs no way to cancel module loading but thatʼs usually fine…
But you can stop nodes, remove all subscriptions, and so on, so they become inert and forgettable.

(You donʼt actually destroy nodes, just ensure that they are forgotten so the [GC]{t=} cleans them up.)

More common than doing a hard cut, is doing some sort of fadeout and then running the destructor logic after the fade is done.
The audio graph needs to be set up for this (you need a gain that you can control), but otherwise it is fairly straightforward.

So yeah, it is a bit peculiar, but it works well.
This is something that I would like a general resource acquisition framework to support.

### Streaming media nodes: creation and subscription

There is a bit less to say about this, but think about creating nodes, and then subscribing them.

If file sources start automatically, there is a bit of a problem: if nothing subscribes to them, their first (few) frames are likely dropped.
The biggest issue here is that dropping the first IDR frame (keyframe) of a video means that nothing can be decoded until the next IDR frame (or possibly I frame, though that does not guarantee absence of decoding errors going forward).

So workflow creation needs to create downstream nodes first, then create the file source and synchronously subscribe to it once it is available.

So itʼs a case where the creation of workflow nodes needs to flow back to front (in the simplest form), opposite of the flow of media.

### Enter, transition, and exit animations

These are tricky and I havenʼt really worked with them in depth.

Put bluntly: CSS and HTML arenʼt a great way to do enter and exit animations, stuff that requires or would require reflows or overlapping content and such.
I guess some frameworks have tried to address this, but idk.
VDOM approaches are weird: “please update the DOM to remove this … but leave a zombie copy of it lingering while it animates”.

Particularly weird is when you need to transition from one thing to another thing, and they need to occupy the same space.
Yeah thatʼs awkward.

Well I think that being up front about what elements can enter, transition, and exit, and how they are allowed to do that, would help a little bit.
If you need to create a containing element ahead of time to allow them to overlap…

Anyways I just think it is important to note the difference between “this [element/resource] is exiting soon” versus “please delete, now!”.

### RPC

Web APIs are nice because they are synchronous, destruction is synchronous, and worst case it happens on garbage collection.

Remote Process Communication is trickier because everything is async: destruction is async^[You can always fire-and-forget if you want/need to.], creation is async, and you might have operations pending whose results may still come in when you try to destroy something.

What makes it *even* trickier is that the remote process canʼt communicate with your [GC]{t=}.
You canʼt use a `WeakMap`{.js} across RPC!

Now, apparently there are ways to hook into the [GC]{t=}, even in Javascript.
But normally you have to be pretty careful about tracking resource usage.
Basically everything that the remote might care about now has to become a tracked resource, instead of the nontracked “let the [GC]{t=} take care of it” resources that we are used to in JavaScript.

### Event streams

Riverdragon has pretty simple resource behaviors and such.

One thing worth noting here is that a stream can destroy itself: declare that it will not output anything more and stuff downstream to clean up if they want.

(This was my compromise instead of adding errors to streams like RxJS has.)

Keeping things from leaking with functional-style JavaScript is tricky…

The other interesting part of a streamʼs lifecycle is the startup burst: synchronous events emitted while a subscription is first being set up.
This needs to be handled explicitly in many places in the implementation, but because it is synchronous, it doesnʼt necessitate any overarching coordination between streams or other resources and doesnʼt leak out of the API.

## Implementation

### Inspirations

`Aff`{.purescript} (hey at least it has cancelers! supervisors, bracket, try-finally, synchronous and asynchronous scheduling … lots of good stuff actually)

`ScoreM`{.purescript} (for WebAudio)

... something new soon? `ResourceM`{.purescript}?

basically it should track a context for acquiring resources: running the monad creates the context and returns whatever was acquired.

two features: create async dependent contexts, and create independent contexts.
I guess one could maybe couple and decouple contexts at will…

async dependent contexts is what `MonadUnlift`{.haskell} is about.
maybe we can hook into `Aff`{.purescript} lifecycle too

```
acquire enough :: base monad (sync or async)
wait initialized :: parallel async
all good to go :: some phases that convert to base monad
readyState :: Rational

destroy :: op sync
wasDestroyed :: Boolean
destroyed :: async
```

constructor errors are fatal, but still call destructors to unwind.
destructors are not fatal, but store the first error at least.

`goodToGo` is called once `wait` is done: or rather, `wait` is a phase zero of `goodToGo`:

  - e.g. could sweep front-to-back then back-to-front
  - sync, or could be async, or parallel async
  - and so on
  - should be consistent
  - but could be lifted to a universal form:
  - `Map Rational { seq :: Aff Unit, qes :: Dual (Aff Unit), par :: Par (Aff Unit) }`
  - (runs each layer, one by one, `seq` before `qes` before `par`)
  - ((but really you should use the rational index to sequence different types))

API:

```purescript
MonadEffect m =>
MonadError e m =>

-- Track some resource creation which produces a destructor
-- (This unlocks `Riverdragon.River.Bed` and `Riverdragon.River`)
track :: m { r | destroy :: Effect Unit } -> ResourceT m { r | destroy :: Effect Unit }

putDestroy :: Effect Unit -> ResourceT m Unit
data Dir = Seq | Qes | Par
putGoodToGo :: Rational -> Dir -> Aff Unit -> ResourceT m Unit
putWait :: Aff Unit -> ResourceT m Unit
putWait = putGoodToGo zero Par

-- Basic form of run, where `wait` is returned separately
-- (including waiting on `goodToGo`, which is automatically run after the base `wait`)
start :: ResourceT m r -> m { result :: r, wait :: Aff Unit, destroy :: Effect Unit }
-- Run but ensure `wait` is finished too
run :: (Aff Unit -> m Unit) -> ResourceT m r -> m { result :: r, destroy :: Effect Unit }
-- Get a localized `start`/`run`
getStart :: ResourceT m (typeof start)
getRun :: ResourceT m (typeof run)

-- Localized destroy: still called upon global destroy too
scope :: (Effect Unit -> ResourceT m { r }) -> ResourceT m { r | destroy :: Effect Unit }
-- Destroy when scope exits (like `with` in Python)
scopedDestroy :: (Effect Unit -> ResourceT m r) -> ResourceT m r

mk ::
  m { result :: result
    , wait :: Aff Unit
    , destroy :: Effect Unit
    } -> ResourceT m result

-- some kind of rotating?
-- some kind of ref counting?
```

questions:

- what happens if new resources pop up during `wait`/`goodToGo`?
  - maybe you restart from their layers, wait for everything to stabilize...
- i guess if you want to queue destruction in order and make a `waitForDestroyed` you can?

### `ResourceT`{.purescript}/`ResourceM`{.purescript}/`MonadResource`{.purescript} Implementation

I have implemented this as a monad transformer and a typeclass.
The main workhorses are actually plain data structures – the monad is just a wrapping around `ReaderT Scope`{.purescript}, basically, and the typeclass an interface to it.

```purescript
-- | The public interface to a scope, consisting of destructors and waiters.
newtype Scope = Scope
  { putDestructor :: Effect Unit -> Effect Unit
  , destroy :: Effect Unit
  , destroyed :: Effect (Disj Boolean)
  , waitDestroyed :: App Fiber Unit
  , putWaiters :: Waiters -> Effect Unit
  , wait :: App Fiber Unit
  }

mkSubscope :: forall m. MonadEffect m => String -> Scope -> m Scope

-- | Running the effect creates a new subscope and destroys the previous one.
oneSubScopeAtATime :: forall m. MonadEffect m => String -> Scope -> m (Effect Scope)

-- | A monoid keeping track of the state of resources in a (sub)scope
type ResourceState =
  { destructors :: Dual (Effect Unit)
  , destroyed :: Disj Boolean
  , waiters :: Waiters
  , waiting :: App Fiber Unit
  , waited :: Disj Boolean
  }

-- | A priority map of asynchronous functions to run and wait on when the
-- | resource is coming online: first-to-last, last-to-first, and parallel.
type Waiters =
  SemigroupMap Rational
    { seq :: Aff Unit, qes :: Dual (Aff Unit), par :: ParAff Unit }
data Dir = Seq | Qes | Par
```

Adopting it for `ScoreM`{.purescript} and Riverdragon more generally has gone well, though there are still a few bugs to shake out.
(The `String`{.purescript} arguments are names for debugging which scopes are prematurely destroyed.)

Not having to manually keep track of destructors is such a relief.

The typeclass interface exposes functions for working with destructors and waiters as you would expect.
It also exposes these combinators for scopes:

```purescript
-- | Caution: should not neglect inner destroy
inSubScope :: forall m. MonadResource m => String -> m ~> m

-- | Kind of like `with` in Python: destroys resources on exit
scoped :: forall e m. MonadResource m => MonadError e m => String -> m ~> m

-- | Actually like `async with` in Python (although it does not wait for destructors)
with ::
  forall e m resource return.
    MonadResource m => MonadAff m => MonadError e m =>
  String -> m resource -> (resource -> m return) -> m return

-- | Use around `inSubScope` (or `scoped` or `with`)
noWait :: forall m. MonadResource m => m ~> m
```

Finally, the monad transformer provides some more specific functions, for running and dealing with monads:

```purescript
newtype ResourceT m a = ResourceT (Scope -> m a)
type ResourceM = ResourceT Effect

-- | Create a new base scope and run the `ResourceT` in it.
-- TODO: catch error? cancel self if Aff? supervise?
start :: forall m r. MonadEffect m => String ->
  ResourceT m r ->
  m { result :: r, wait :: Fiber Unit, destroy :: Effect Unit, scope :: Scope }

-- | Starts and waits for the resources to be ready.
run :: forall m r. MonadAff m => String ->
  ResourceT m r ->
  m { result :: r, destroy :: Effect Unit, scope :: Scope }


-- | Run in an existing scope.
scopedStart :: forall m r. MonadEffect m => String ->
  Scope ->
  ResourceT m r ->
  m { result :: r, wait :: Fiber Unit, destroy :: Effect Unit }

-- | Run in an existing scope and start the waiters (but do not wait for them).
scopedStart_ :: forall m r. MonadEffect m => String ->
  Scope ->
  ResourceT m r ->
  m { result :: r, destroy :: Effect Unit }

-- | Run in an existing scope and wait for the waiters.
scopedRun :: forall m r. MonadAff m => String ->
  Scope ->
  ResourceT m r ->
  m { result :: r, destroy :: Effect Unit }

-- | The `Aff` is killed when the scope is destroyed
monitor :: String -> Scope -> (Aff ~> Aff)
-- | Destroy the scope when the `Aff` exits (does not `monitor`)
link :: Scope -> (Aff ~> Aff)
-- | Destroy the scope when no more `Aff`s from this are running
linkN :: Scope -> Effect (Aff ~> Aff)
```

### Usage

`ScoreM`{.purescript} is now just a stack on top of `ResourceT`{.purescript}:

```purescript
type ScoreM = MutStateT ScoreSt (ReaderT ScoreRd (ResourceT Effect))
type ScoreAsync = MutStateT ScoreSt (ReaderT ScoreRd (ResourceT Aff))
type ScoreSt = Record (ScoreS + ())
type ScoreRd = Record (ScoreR + ())

-- Reader state
type ScoreR r =
  ( ctx :: AudioContext
  , iface :: ScoreLive
  | r
  )
-- Mutable state
-- TODO: caching of knobs?
type ScoreS r =
  ( id :: Int
  , worklets :: Map String (Aff Unit) -- unused at the moment
  | r
  )
-- Live variables that notify when they change
type ScoreLive =
  { temperament :: Interface (Array Number)
  , pitch :: Interface Frequency
  }
```

`Stream`{.purescript} (`River`{.purescript}/`Lake`{.purescript}) is still a plain effect over records (… for now?), but it integrates with `MonadResource`{.purescript} and `ResourceM`{.purescript} for some [API]{t=}s.
The nicest of these is `subscribeM1`{.purescript} which only keeps one resource active at a time, so you donʼt have to track cleanup state manually across time.

```purescript
-- | Helper to run callbacks in the same scope as the subscribe is done in.
subscribeM :: forall flow a m. MonadResource m => Stream flow a -> (a -> ResourceM Unit) -> m Unit

-- | With `oneSubScopeAtATime`, so the previous resource is destroyed upon each new event.
subscribeM1 :: forall flow a m. MonadResource m => Stream flow a -> (a -> ResourceM Unit) -> m Unit
```

The `Dragon`{.purescript}ʼs `Egg :: ResourceM Dragon -> Dragon`{.purescript} now uses `ResourceM`{.purescript} to create live resources while rendering, which simplifies things so much: the egg scope does not need to be passed around manually, and it does not need to contain impredicative fields for each operation to track.
Instead, every API (ideally) runs in `ResourceM`{.purescript} already and is tracked automatically.
And, in the worst case, the new explicit `Scope`{.purescript} type can be exposed and passed to wherever needs it.
