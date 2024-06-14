---
title: "chat: ideas for FRP DOM library"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

btw are you familiar with event streams in FRP?\
stuff like RxJS Observable

> I know practically nothing about FRP\
> I’ve never really looked into it

okay, gotcha :)\
the key idea is: “what if i have a value that changes over time?”\
“i want to work with it as if it is a single value/object, but i need to know how to query it, or how to be notified when it changes, …” stuff like that\
and it's a bit more complicated in pure FP (for good reason), because there's a difference between “i have a [global, or locally instantiated] timer that is running at a fixed interval” versus “i know how to make a new timer running at a fixed interval wherever it needs to appear” (it becomes instantiated for each subscriber separately)\

> Mmm yeah okay, so it’s another way of addressing state in functional programming

yup!\
I've actually found it to be one of the worst ways 😅 it's very very easy to let it turn into spaghetti code again … with implicit state everywhere

> That sounds like a bit of a nightmare, but also most software techniques now that I think about it

but it makes sense as an idea, and it's very appealing\
and so many people have worked on the problem of “how do you integrate this with the web browser's DOM”:\
on the surface it seems pretty simple:

- you consume an event stream to dynamically update attributes in the DOM, without making and diffing a VDOM (virtual DOM … aka a separate data structure that represents what you want the DOM to look like – that's what React made popular)
- on the other side, listeners in the DOM will generate event streams that you can then listen to … and these event streams feed back into other internal state, and output events to set attributes again, and you can already see how it turns into spaghetti quickly 😅

> Yeah that sounds like a messy situation lmao

the advantage of avoiding the VDOM is obvious: you don't have to diff or traverse data structures, you can just target the right part of the DOM directly\
unfortunately the flip side is … how do you deal with nodes actually changing? even a simple TODO list demo app becomes difficult to structure\
and if you figure out the TODO list, where only individual nodes are changing … what if you really do what to swap out whole subtrees? and do you write your own diffing logic at some point??

> Ohhh god okay yeah that’s messed

(related problem that I find fascinating but have much less experience tackling: incremental computation! can you compute what changes in the DOM directly from how your state changes?)

one of the other tricky parts of FRP DOM library: you end up with very little guarantees that what your application thinks its state is, is what is reflected in the DOM, and vice-versa … those things are tricky to debug

that's the basic lay of the land, next i'll figure out what contributions i want to make haha

- first up: i don't really like the existing Event/Behavior abstraction in purescript-event … I think there probably needs to be hot/cold events, and I think there needs to be a kind of Ref-integrated abstraction, and uh,, yeah\
- also I think event sources should be tracked separately … there are some weird edge cases around things double-updating if they listen to the same event, so I want to eliminate that … kind of by building a DAG of the event flow
- something I really want is “fragment” support (React fragments), where you can work with basically a slice of HTML/DOM (multiple elements and text nodes) as if it were a single element (that is: monoids!)
- I also want better types (and monoids!) to make working with the DOM interface easier … working with strings is no fun!\
- and I want better interfaces in general … it would be great to have a high-level interface to “what is the size of the DOM element”, as opposed to working out the APIs and the math for that from scratch every time 🫠
- better state management, better APIs / easier patterns for abstraction\
- hopefully SSR won't be too complicated (importantly: I know the trick for serializing fragments)
