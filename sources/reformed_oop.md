---
title: OOP Without the Goop
subtitle: No Objects, Just Closures in Records
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026/05/05
---

[OOP]{t=} gets a lot of things wrong.
Objects themselves are not interesting, they are usually beside the point.
It is their behaviors, their interfaces, that are interesting.
[Even lifecycle/RAII can be decoupled from an “object” and a “class”.](resource_acquisition.html)

Iʼve been experimenting with a different approach that simply seems cleaner.

[OOP]{t=} sort of developed from the perspective of C turning into C++:
it started with passing mutable struct pointers to individual global functions, and then C++ organized these “members” and “methods” into a class.
The class also provided constructors and a destructor to manage the lifecycle of its data.^[Java took it further and said there is no function, only class, which was an ugly mistake.]

Things were a bit different with JavaScript and Python, for example, but they still sort of ended up in the same place.

While organization is nice, this is too much living under one roof.

Remember how we used to do private members in JavaScript?
Before JavaScript had `class`{.javascript} syntax and `#privateField`{.javascript}s, the constructor had to use its local scope to store private variables, and bind closures that can access them.

Why not push this to its limit?
A constructor doesnʼt even need `this`{.javascript}, it can just return a record (Plain Old JavaScript Object) of closures linked together by their shared scope.

In fact, this is what Iʼve been doing in PureScript.

Since PureScript has row types (enabling anonymous records), I donʼt need to foreign import types for objects: I just declare their interface and import a single function that creates that interface.

While pure functional programming is a good fit for immutable data, I think this barebones-OOP style is an easier approach for mutable data, resources that change under effects.

## Records of Closures

We still need some place to keep all the behaviors on an object together, and I propose that it should be an immutable record of closures (effectful functions that will manipulate the data/resource).

Before:

```purescript
module Effect.Ref

-- Declare the foreign type
-- (in PureScript there is nothing to import, per se)
foreign import data Ref :: Type -> Type

-- Import a constructor
foreign import new :: forall v. v -> Effect (Ref v)

-- And import all of the methods
foreign import read :: forall v. Ref v -> Effect v

foreign import write :: forall v. v -> Ref v -> Effect Unit


import Effect.Ref as Ref

example :: Effect Int
example = do
  ref <- Ref.new 0
  -- You have to import the file
  -- and remember where the object goes
  Ref.write 1 ref
  Ref.read ref
  -- 1
```

After:

```purescript
type Cell v =
  { get :: Effect v
  , set :: v -> Effect Unit
  }

-- Import the constructor, which gives all of the methods
foreign import mintCell :: forall v. v -> Effect (Cell v)

example :: Effect Int
example = do
  cell <- mintCell 0
  -- It is just a record so it is easy to call
  cell.set 1
  cell.get
  -- 1
```

No opaque foreign type.

*Much* fewer things to import across the FFI boundary.

And no module import to remember: methods are always available on the record with simple dot syntax, and you donʼt have to remember whether the object comes first or last in the arguments.

:::Details
A function has access to variables in its local namespace (instantiated for its current call) plus the namespace where it was created, all the way on up to the global namespace usually.
Pairing the underlying code with a chain of namespaces is known as a __closure__.
As an optimization, only the required variables may be packed in a closure, as opposed to whole namespaces.

Closures pack variables of any type into an interface accessible through the function type: this is why they can be used to [encode existential quantification](https://stackoverflow.com/a/13653533), and also why they do *not* support things like [NFData](https://github.com/haskell/deepseq/issues/16) and serialization, except [when the runtime could actually support it](https://blog.veritates.love/pickling.html#functions-builtins-bootstrapping).

For our purposes, they provide abstraction (implementing the same interface by any means necessary) and the construction of closures provides the coherence that normally is associated with an “object”: `cell.set`{.purescript} and `cell.get`{.purescript} both contain the same mutable reference in their closure.
:::


### Extra methods

For convenience and completeness, it is nice to have more methods.
Iʼm particularly fond of a `swap`{.purescript} method that returns the old value.

We also might as well abstract the monad from `Effect`{.purescript} to `m`{.purescript} while we are here.

```purescript
-- | This is an abstract interface to a simple mutable cell.
type Cell m v =
  { get :: m v
  , set :: v -> m Unit
  , swap :: v -> m v -- obtain the old value
  , update :: (v -> v) -> m (Pair v) -- return both the old and new values
  , modify :: Modify m v
  }
-- keep additional information `r` during modification
newtype Modify m v = Modify (forall r. (v -> Tuple v r) -> m { prev :: v, next :: v, info :: r })
```

(`Modify`{.purescript} is wrapped because the impredicative type causes issues for typeclass deriving, among other things.)

```purescript
-- | Make a mutable cell, appropriate for the effect type (Ref or STRef)
class Monad m <= Cellular m where
  mintCell :: forall @v. v -> m (Cell m v)


-- | Use a cell for a mutable state transformer.
newtype Cellfie v m a = Cellfie (ReaderT (Cell m v) m a)
```

### Operators

Now that we are duck typing this, we can provide operators to access the methods:

```purescript
setCell :: forall ops m v r. Functor m => { set :: v -> m r | ops } -> v -> m Unit
setCell = map void <<< _.set

swapCell :: forall ops m v r. { swap :: v -> m r | ops } -> v -> m r
swapCell = _.swap

infix 1 setCell as &=      -- v -> m Unit
infix 1 swapCell as <&=    -- v -> m v
infix 1 updateCell as &~   -- (v -> v) -> m Unit
infix 1 swapifyCell as <&~ -- (v -> v) -> m v
```

PureScriptʼs row types make this seamless by enabling extensible records of operations `{ | ops }`{.purescript}.




## Philosophy

These are the factors that have led me here.

Some of it is just convenience, but I think this convenience also points to it being a good choice.

### Abstraction

I think the extra abstraction is *really* nice.

It has annoyed me so many times that one writes a program to an otherwise abstract interface, but it isnʼt actually adaptable to a new interface, because it relied on particular object types, not their interface.

With `Cell m v`{.purescript}, it doesnʼt actually matter what is backing the `Cell`{.purescript}:
it should support reading and writing.

An asynchronous cell in [`Aff`{.purescript}](https://pursuit.purescript.org/packages/purescript-aff) could even maintain its state remotely, via network requests. (… not that I would recommend that.)

More interestingly, interfaces that are already asynchronous could be virtualized across the network.
Layer on caching, depending on the semantics.


### Mutable variables have no contract

— no intentionality, either

The bit of extra friction in using mutable values in PureScript has made me realize that not having mutable variables can actually be a good thing for clarity.

Mutable variables, even including `Cell m v`{.purescript}, are bad because they have no principle to their use: they can be overwritten with any value at any time.

```purescript
mintAccumulator :: forall m w. Cellular m => Monoid w => m
  { get :: m w, put :: w -> m Unit, reset :: m w }
```

This accumulator, on the other hand, is principled.
Its state always accumulates, until an explicit `reset`{.purescript} is called:
and that could be removed if monotonicity is worth guaranteeing.

What about a counter?

```purescript
mintCounter :: forall m. Cellular m => m (m Int)
```

You store it in an immutable variable, and each time it is called in an effectful context, it returns the next higher integer.

Contrast this to bare imperative programming, where you declare plain variables with *no* contract for how they are used, no description of how they evolve over the program.

This is especially frustrating in the small scale and large:

- in individual loops, a mutable variable can represent all sorts of monoids ([`Max Number`{.purescript}](https://pursuit.purescript.org/packages/purescript-orders/6.0.0/docs/Data.Ord.Min#t:Min), [`Additive Int`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/6.0.2/docs/Data.Monoid.Additive#t:Additive), [`Conj Boolean`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/6.0.2/docs/Data.Monoid.Conj#t:Conj), and so many more), and it increases cognitive load trying to figure out what behavior it has
  - not every behavior is modeled by a monoid, but I think you will find that most are! and it is easier if the basic behavior is able to be described like this, and you just have to pay attention to the cases of exceptional behavior
- invariants over the course of a whole program run or object lifetime are also difficult to track: you have to chase down their use sites, not the definition site


### Scopes

This style of object pairs really well with my idea of scopes from [Resource Acquisition](resource_acquisition.html#implementation).

In this conception of scope, a scope is simply a way of accumulating destructors and telling us if a scope was destroyed already.^[This is sufficient in a garbage-collected language, at least.]
Trust me, tracking individual `_.destroy`{.purescript} methods on objects is *not* fun.

```purescript
newtype Scope = Scope
  { putDestructor :: Effect Unit -> Effect Unit
  , destroy :: Effect Unit
  , destroyed :: Effect (Disj Boolean)
  }
```

My implementation also contains functions to wait for full initialization to complete, since this was convenienent for my WebAudio nodes.

```purescript
newtype Scope = Scope
  { putDestructor :: Effect Unit -> Effect Unit
  , destroy :: Effect Unit
  , destroyed :: Effect (Disj Boolean)
  , waitDestroyed :: App Fiber Unit
  , putWaiters :: Waiters -> Effect Unit
  , wait :: App Fiber Unit
  }
derive newtype instance Monoid Scope
derive newtype instance Semigroup Scope
```

This forms a monoid, stating that the way to inherit from multiple scopes is simply to register your destructors with all of them.

You could possibly envision scopes containing locks in multithreaded languages.
But I am not sure how well that would work with a monoid structure…

(Basically my complaint about multithreaded data structures is that if everything is mutable, everything needs a lock. But these locks are just floating about in space, it is not clear how they fit together into a coherent constellation. If they instead live externally in a `Scope`{.purescript} that governs a group of resources, it may be slightly clearer.)


### Inheritance/extensibility

I think the story for extensibility is actually okay.
You can merge records together to add more methods.
Inheritance through composition: you can instantiate other objects and manage methods however you see fit.


### Erlang-style processes

Think of how a mutable cell evolves over time.

One explanation for this behavior is as a [Mealy machine](https://hackage-content.haskell.org/package/machines-0.7.4/docs/Data-Machine-Mealy.html).
It takes successive inputs that inform its internal state as it produces an output each time.

```purescript
newtype Machine m i o = Machine (i -> m (Tuple (Machine m i o) o))

monadicMachine :: forall m s i o. Applicative m =>
  s ->
  (s -> i -> m (Tuple s o)) ->
  Machine m i o
monadicMachine initial evolve = encapsulate initial
  where
  encapsulate state = Machine \input -> ado
    Tuple updated output <- evolve state input
    in Tuple (encapsulate updated) output
```

Notice how it encapsulates the state type `s`{.purescript} and does not reveal it, capturing it in the closure instead.^[This encapsulation even allows for machines that switch their internal state type, by handing off to a new machine: `Either (Machine m i o) s`{.purescript}.]

We can spawn a machine, backing it with a mutable variable: `Machine m i o -> m (i -> m o)`{.purescript}.

This has a problem when `m`{.purescript} is asynchronous, though: requests can race, meaning that state is overwritten based on stale information.

This basically led me down a rabbit hole of reinventing Erlang-style [`gen_server`](https://www.erlang.org/docs/24/man/gen_server)s that process a queue of inputs in order, checking state out and in as it works on a request.

This eventually ended up as a `SwitchProcessor`{.purescript}, with an internal state `state`{.purescript} it can handle a keyed input `input key`{.purescript} to produce a keyed output `output key`{.purescript} via the monad `m`{.purescript}, with some extra nuances in behavior along the way.

```purescript
mkSwitchProcessor :: forall m state input output.
  state -> (forall key. state -> input key -> m (SwitchProcessing m (SwitchProcessor m input output) state (output key))) ->
  SwitchProcessor m input output

data SwitchProcess m switch stay
  = ProcessStay stay
  | ProcessTrySwitch (m switch) (m stay)
  | SwitchLater (m (SwitchProcess m switch stay))

data SwitchProcessing m switch stay o
  = SwitchProcessed (SwitchProcess m switch stay) (m o)
  | SwitchProcessing stay
    -- this `m` can block for a while and then return,
    -- either returning the output immediately or
    -- asking for a new state to continue processing
    (m (Either o (stay -> m (SwitchProcessing m switch stay o))))
```

> An async processor that queues inputs until the state is available, and
> also allows switching out the whole processor for one with an interface
> of the same type.
>
> It maintains a queue of input-output pairs or threads waiting on state.
> It will transfer inputs over, but it can only switch if there are no
> threads waiting on the state.
>
> Switching allows adding or discarding items from state without dealing
> with optionality: they can be always present (or captured in the closure),
> no need to check if they are present on each operation and deal with errors.
>
> It also can help with directional state machines, where previous states
> become unreachable. Although there is nothing that prevents it from
> switching back to another instance of the previous machine later.


Again, this is just an abstract interface to a mutable cell.

Here it is implemented on top of JavaScript object mutability, but its interface could also be implemented across JavaScript workers with serializable objects, or in Erlang with its ability to send any Erlang object across processes or even servers (since they are immutable! all mutability is done via FFI or by emergent interactions between processes).




## Downsides

Alright, there are some downsides to this approach.
But I am not convinced that they are blockers, especially in a language like PureScript that makes it more ergonomic than the alternatives, and has the capacity to be optimizable (plus, JavaScript runtimes are impressively optimized already).

#.  It requires a sufficiently high level language to make sense of records and closures in the first place.
    Low-level FFI and bootstrapping may still require interfacing with the old style of classes-and-methods or pointers-and-procedures, leaving it to the high level language to wrap them in records-and-closures.

#.  Additionally, it might be much harder to swing without garbage-collected languages, not sure on that.

#.  It still has the same downside of [OOP]{t=} that everything is object-centric, being methods *on* an object, and so on.^[Iʼve always complained that [why should things like addition and concatenation be methods on one object instead of the other?](oop.html), and I hope you also see why it is shortsighted that [OOP]{t=} forces that.]
    But this is okay, I think that is reasonable when dealing with mutation/communication-centric things.
    And the closures can be projected out into plain functions, without having to worry about if method access produces the right `this`{.javascript} reference.

#.  It probably makes optimization difficult, since everything is virtual, based on closures not methods.

#.  There is no explicit referential identity: you cannot test whether two `Cell`{.purescript}s are the same because they are abstract interfaces.
    You may still have some use cases where you want explicit referential identity, but … I think those are rare and deserve to be special, not the default.

#.  Itʼs more difficult to ascribe contracts to the behavior of these objects, since they are basically duck typed now.
    But again, I think it is a reasonable tradeoff here, and there are ways of being more explicit if you want to be.



## Examples

to demonstrate the concept^[I think my best examples are yet to be written … I am still in the fundamentals and velocity-building stage of exploration here]

### Riverdragon FRP

Creating a river (a flowing stream) is a resource action, so it returns a record of methods: the created `stream`{.purescript} and a way to send events through it.

```purescript
createRiver :: forall flow a m. MonadResource m =>
  m { send :: a -!> Unit, stream :: Stream flow a, destroy :: Effect Unit }

createStore ::
  forall flow a m. MonadResource m =>
  a ->
  m
    { send :: a -!> Unit
    , stream :: Stream flow a
    , destroy :: Effect Unit
    , current :: Effect a
    }

instantiate ::
  forall flowIn flowOut a m.
    MonadResource m =>
  Stream flowIn a ->
  m
    { burst :: Array a
    , stream :: Stream flowOut a
    , destroy :: Effect Unit
    }
```

`_.destroy`{.purescript} is just a convenience method, since it is already registered with the `Scope`{.purescript} from `MonadResource`{.purescript}.

### Interface to a mutable value

I use this for the widgets on my blog.
It is pretty similar to the above (in fact, it was inspired by and in turn inspired it).

```purescript
-- | An interface to a mutable value, stored in one location. It may not have
-- | a value at first, or ever.
type Interface a =
  { send :: a -> Effect Unit
  , receive :: River a
  , loopback :: River a
  , mailbox :: a -> River Unit
  , current :: Effect (Maybe a)
  , destroy :: Effect Unit
  }
```

The benefits over a `Ref`{.purescript} are:

#.  You can receive notifications of the events via the `River`{.purescript}s.
#.  Input and output are separated (`send`{.purescript} vs `current`{.purescript}), so you can map over them with a pair of functions, one for each direction.
    This allows widgets on my blog to communicate uniformly via JSON, while themselves seeing `Interface Int`{.purescript} or whatever type they de/encode.

#.  The key nicety is `loopback`{.purescript} vs `receive`{.purescript}, which can filter out events emitted out from a particular interface versus the events coming in to it.
    This is incredibly useful for UI events, where naïvely updating a text box could cause it to lose its selection.^[Of course, this is possible to avoid with [the correct web APIs](https://pursuit.purescript.org/packages/purescript-textcursor/2.0.0/docs/Web.Util.TextCursor), but few people put in that effort, sigh…]

    (Of course, any particular implementation of `Interface`{.purescript} is free to ignore this and implement `receive = loopback`{.purescript}, but it is still nice.)

#.  Finally, `mailbox`{.purescript} allows efficiently notifying for particular values.
    You can imagine a selection index maintained as an `Interface Int`{.purescript} and 1000 DOM elements listening to see if they are selected.
    (This form isnʼt quite correct for that, but you get the idea.)

### Remote resources

Here is a helper for requesting remote resources ([e.g.]{t=} over HTTP), that includes status via the [`RemoteData`{.purescript} pattern](https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/RemoteData#RemoteData), and automatically cancels itself when the scope is destroyed.

Note that it has a temporary canceler and also a permanent destructor that prevents further requests.

```purescript
data Status a
  = Unloaded
  | Loading (Effect Unit) -- canceler
  | Failed String
  | Loaded a

remoteResource :: forall a m. MonadResource m =>
  { acquire :: Aff a
  , reloadable :: Boolean
  , retryable :: Boolean
  } -> m
  { start :: Effect Unit
  , load :: Aff a
  , cancel :: Effect Unit
  , loaded :: River a
  , status :: River (Status a)
  , destroy :: Effect Unit
  }
```

This is just *so* much nicer than what JavaScript would have to offer: canceling requests is done with the awkward [`AbortController`{.javascript}](https://developer.mozilla.org/en-US/docs/Web/API/AbortController)/[`AbortSignal`{.javascript}](https://developer.mozilla.org/en-US/docs/Web/API/AbortSignal) combo (why oh why are those full fledged objects), and it just has promises and the `.addEventListener`{.javascript}/`.removeEventListener`{.javascript} API instead of the proper asynchronous monad `Aff`{.purescript} and FRP event stream `River`{.purescript}.


### Helpers for destructors

Two methods that are indispensible for tracking cleanup of objects:

```purescript
mintCleanup :: forall m. Cellular m => m Unit -> m (m Unit)
mintCleanup = mintCell >>> map \cell -> do
  callbackOnce <- cell <&= (pure unit)
  callbackOnce

mintRolling :: forall m. Cellular m =>
  m (Interface m (m Unit) Unit)
mintRolling = mintCell (pure unit) <#> \cell next -> do
  prev <- cell <&= next
  prev
```

The first ensures that its cleanup effect only runs once, prevent double counting.
Additionally, it makes sure to free its closure to prevent resources from leaking via a retained destructor.

Rolling accepts a new cleanup function each time it is called, running the old one.
This is incredibly useful when subscribing to streams: sometimes you want each new event to teardown the previous actions.

When specialized to `Effect`{.purescript}, these both basically optimize away with [`purescript-backend-optimizer`](https://github.com/aristanetworks/purescript-backend-optimizer/), using a `let`{.javascript} binding:

```javascript
var mintRollingE = () => {
  let a$p = () => {
  };
  return (next) => () => {
    const a$p$1 = a$p;
    a$p = next;
    return a$p$1();
  };
};
var mintCleanupE = (x) => () => {
  let a$p = x;
  return () => {
    const a$p$1 = a$p;
    a$p = () => {
    };
    return a$p$1();
  };
};
```

This is a breaker that can only be tripped, not reset.
It also frees its closure.

```purescript
mintBreaker :: forall m i. Cellular m =>
  (i -> m Unit) ->
  m
    { run :: i -> m Unit
    , trip :: m Unit
    , running :: m Boolean
    }
mintBreaker act = mintCell (Just act) <#> \cell ->
  { run: \i -> do
      cell.get >>= case _ of
        Nothing -> pure unit
        Just fn -> fn i
  , trip: cell.set Nothing
  , running: isJust <$> cell.get
  }
```



## Future

I think what I have outlined here is a much simpler approach that cuts to the heart of what we need from interfaces to objects and mutability.

Iʼve been using this to develop [Riverdragon](riverdragon_implementation.html), my take on [FRP]{t=}.^[Riverdragon needs surprisingly few object behaviors for its helpers: about 10.]
Iʼve even been using it to port Riverdragon from PureScript to other languages: I think [everything should be compatible with FRP](https://tech.lgbt/@monoidmusician/116449166586150538) and I wanna use Riverdragon everywhere.
It doesnʼt need to be done in functional languages or with monads, it works well in JavaScript with plain effectful functions!^[Even `Scope`{t=} works, using the usual tricks for local monads via a global variable, plus some care around closures/callbacks/asynchrony.]

I will keep using it, in my own work.
Perhaps it can make its way to a published library.
It would be even nicer to reimagine a whole ecosystem around it.

- https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Riverdragon/River.purs
- https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Riverdragon/River/Bed.purs
- https://github.com/MonoidMusician/blog/tree/main/PureScript/src/Control/Monad/Cell

Maybe you will join me too.
Maybe we can make simpler new languages based on these concepts.
