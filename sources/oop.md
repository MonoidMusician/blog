---
title: OOP
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2023/10/28
---

Modern Java(Script)-style OOP is terrible because it focuses on something that really fucking does not matter* (*most of the time): a certain narrow, prescribed relationship of objects with their “methods”

“Method” means nothing, it basically just means “an object was involved in this function and I arbitrarily decided which based on where it was convenient to put the code / what source code I happened to be in control of”

FP teaches us that the only things that matter are: the shape of data we choose to work with, [i.e.]{t=} (G)ADTs and a few effectful and/or mutable constructs as a treat, and the structure of plain old functions

thatʼs it

Attaching these functions to particular objects is at most a convenient calling convention, or maybe a nice hack for dynamic overloading, which can quickly become unpredictable; at worst, methods being tied to objects is an utter distraction from choices that actually matter

(Smalltalk-style OOP / the actor model is more defensible but only if you make actors interesting!)

Erlang on the other hand actually does cool things with OOP from two perspectives:

- Erlang processes are, as I understand them, an implementation of the actor–message passing model, and you actually get nice properties here:
  - processes are the unit of abstraction around which concurrency guarantees are given (messages from P1 to P2 always are received in order)
  - having an explicit mailbox lets you flush message queues, important for [e.g.]{t=} if you need to deregister a supervisor on another process that may have already failed in the meantime
- Erlang also uses modules as overloading, but again there is actual meaning here:
  - It guarantees that abstractions like gen_server (the basic worker process implementation) have to pass you your process state manually – and that process state is immutable because Erlang data is immutable
  - This also enables hot reloading: you can reload the module with a code patch to seamlessly start calling the new functions, and if youʼre careful/lucky it will keep state


