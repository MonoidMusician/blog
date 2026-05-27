---
title: Everything Should Be FRP Compatible
subtitle: Functional Reactive Programming Everywhere
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026/04/22
---

I think FRP (Functional Reactive Programming) is great. I don’t think everything should be written in an FRP style, it has its warts, but I think all types of dynamic objects should be compatible with the philosophy, concepts, and interfaces of FRP.

the basic idea is reactive values that may come with a “current” value or other form of history, and notify you when they change (if they aren’t [continuously variable](https://stackoverflow.com/questions/5875929/specification-for-a-functional-reactive-programming-language/5878525#5878525))

this feels like [OOP]{t=} still, just with prioritizing the event stream/notifications as an integral part of the design, not something that needs to be bolted back on in every interface in ad-hoc ways

the Functional part adds the nuance that we want to be able to describe not just instantiated values/streams, but also descriptions of what streams are before any effects are run: before instantiation, before [API]{t=}s are called, before referential identity

every time one adds state (like deduplication, debouncing, or even just a counter) to a stream, it now can be observed from different perspectives: different instances of the same stream-description can behave differently

this instantiation either happens implicitly at the consumer, or explicitly with a effect that instantiates one subscriber that broadcasts to all subscribers of a new stream

this is the distinction I make between `River`{.purescript} (an already-instantiated `Stream Flowing`{.purescript} that broadcasts to all of its subscribers) versus `Lake`{.purescript} (a `Stream NotFlowing`{.purescript} that may have different events per subscriber, as it is instantiated for each)

[Riverdragon](riverdragon_implementation.html)

------

I recently had a bug in some [UI]{t=} code I wrote where I was explicitly instantiating a stream before rendering, to share some work across nodes in the [DOM]{t=}

(specifically: I was instantiating an [SVG]{t=} node into `<defs>` so it could be `<use>`ʼd multiple times later)

because this instantiation effect happened *before* rendering, it caused a minor bug where the stream I had instantiated to gather the `<defs>` would miss it or be in a wonky state when the renderer got around to subscribing and adding it to the DOM

the proper fix here was to instantiate the stateful `<defs>` node in a way that each new subscriber got the *full history* of events through it

(the other behaviors available were: each new subscriber gets the latest event through it, or each new subscriber looks upstream just for the “burst” events that the stream would naturally generate – which is generally not the right behavior for a stateful stream)

------

as an example of something that *should* be [FRP]{t=}-compatible but usually isnʼt:

filesystems!

now, thereʼs Annoying Details here,

but youʼd kind of like an individual file to be something like an event stream that holds both the fileʼs contents and its metadata and can notify you when it changes

(obviously for large files you donʼt want to actually read it into memory each time … but that is the simplest and most correct interface for small files, at least)

then thereʼs annoying issues of what if the file goes away, or is renamed, … plus debouncing and stuff

so file watching isnʼt trivial. but it is something that ought to be FRP-compatible.

------

most Web APIs are vaguely FRP-compatible, theyʼll give you ways to invoke callbacks when things change

but the whole Event / `addEventListener` design has gotten increasingly weird, especially with `AbortSignal` and stuff

and itʼs a lot of work to package stuff into gestalt streams instead of disparate events on objects with mutable fields

and itʼs interesting that plain objects *donʼt* give you ways to observe events on their fields: you have to proxy them or make your own setters or something

------

the other interesting bit is that thereʼs no way to get the current mouse cursor position, both on the web and in some UI frameworks

you instead have to listen for a mouse event

(with asterisks around “the” and “mouse cursor”: there can be zero or multiple cursor locations, especially if they are touchpoints …

… but generally there is still an API for plain mouse cursor events, that youʼd like to turn into a “current mouse position” reactive stream, sending its last known position to new subscribers)
