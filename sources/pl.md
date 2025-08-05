---
title: Thoughts on programming languages
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

Functional Programming has it right: the default stance of data should be *immutable*.
(And this requires a garbage collector: canʼt really pass around deep immutable structs with sharing and callbacks and stuff without garbage collection.)

Why should data be immutable?

- because it reduces the primitives you need in your language
  - fundamentally: variables, constructors, cases, functions
  - of course, sugar: lambdas with cases built in, and so on
  - and some more features to make some things nicer, more feasible: record accessors, record update expressions
- it allows for *accurate* types for all of that
  - there's no viable way to typecheck discriminated unions of mutable data, for example. (TypeScript makes tons of unsafe assumptions that values don't change – doubly unsafe when they can be implicit function calls via getter properties or proxies.)
  - everyone wants to at least *fake* having dependent types, but you canʼt have that if things are mutable
- the primitives that imperative languages have for mutability are really bad
  - notify on changes? what about transactions, batched changes? undoability? modifying vs getting/setting?
  - really we want reactive values, “objects”/“actors” that encapsulate some resource-idea specifically, as opposed to a `Point` class that just happens to be mutable
- increases *safe* assumptions the compiler can make to optimize things
  - no more worries about `virtual`{.c}
  - atomicity is always tricky
- mutation is an effect, and we want to model that explicitly with monads, especially to capture different types of mutation
  - STM, synchronous vs asynchronous, capabilities (JavaScript in an isolated browser vs NodeJS with access to a filesystem), and so on
