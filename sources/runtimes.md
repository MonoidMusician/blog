What is a runtime?

For a lot of programming needs, data/objects in a programming language can be treated as mathematical objects.
What distinguishes mathematical/logical objects is that they are deterministic^[Arguably [choice operators](https://en.wikipedia.org/wiki/Choice_function) are a source of nondeterminism in mathematical logic, but it is different than programming languages.], immutable, eternal, time-invariant: they exist without reference to time, without referential identity, without reference to anything but their logical relationships to each other.
They just exist (in theory) and they will never change – which means you do not need to keep track of time, sequencing, concurrency, any of those things.

Not all _programming_ objects will behave like this, but it is a useful perspective to take.
It is the functional programming perspective.

A runtime, then, is what you add to this conception to produce a runnable, usable, interactive, rich programming language.

A runtime is responsible for implementing programming language semantics (whether they are specified elsewhere or just by implementation and convention), for managing memory (through a garbage collector ([GC]{t=}) or reference counting or some combination).

A runtime also includes:

- external APIs you can hook into (often communicating with the [OS]{t=} or with other runtimes or programs on the same runtime)
- observable details like referential identity and exceptions that do not correspond to pure functions and may not even be guaranteed
- concurrency, be it a single-threaded event loop or access to processor-level multithreading (usually mediated by the [OS]{t=})
- performance guarantees and oddities^[Everyone complains about laziness in Haskell being tricky to reason about, but I think this is true about runtime performance of all runtimes: Erlang has its own details that you have to get to know when you run into issues or need greater performance. Programming languages are complex and none are free from surprises and deep dark lore.]

These are the interesting bits!

Every^[citation needed] language is Turing-complete, but this is a list of the things you can do that arenʼt about computing numbers or reading and writing bytes: the capabilities of different runtime systems for different languages.

## Runtimes

### JavaScript (V8/Node.js/Electron, SpiderMonkey)

- Structured clone
  - Used for communication with workers, also exposed in [`v8.serialize`](https://nodejs.org/api/v8.html#serialization-api) and used in a few disk formats
  - Only data, no prototypes, which is more than a bit dicey in JavaScript...
  - V8 and SpiderMonkey both donʼt do string sharing??
- Node.js and Electron support [startup snapshots](https://nodejs.org/api/v8.html#startup-snapshot-api), which is a full restartable heap snapshot, including all functions and all data and such
  - Not all builtin modules are supported, though: https://github.com/nodejs/node/blob/main/lib/internal/main/mksnapshot.js
- Different execution contexts: main browser context, main Node.js context, worker threads (web workers, service workers, audio worklets, [etc.]{t=}), startup snapshot, ...
- [`WeakMap`{.js}](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap), [`WeakSet`{.js}](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakSet), [`FinalizationRegistry`{.js}](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/FinalizationRegistry): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Memory_management#data_structures_aiding_memory_management
- [WASM]{t=}, a low-level runtime integrated into the high-level JavaScript runtime!

### Haskell GHC

- Laziness, of course.
- [`GHC.Compact`{.haskell}](https://hackage.haskell.org/package/ghc-compact), a compact region which acts as a unit for garbage collection, needs to be fully evaluated data (not functions).
  - Can be compacted with and without maintaining sharing
- [`GHC.StableName`{.haskell}](https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.21.0.0-8bb5/GHC-StableName.html), a kind of pointer identity that is explicitly preserved by the (copying) garbage collector
- Weak Refs
- Mutability vs immutability is also integrated into the GC
- Stable pointers, static pointers, pinned memory, [etc.]{t=}
- [Software Transactional Memory (STM)](https://hackage.haskell.org/package/stm) for concurrency, [`MVar`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Concurrent-MVar.html), `ST`
- SIMD (now available without LLVM codegen)

### Erlang BEAM/OTP

- Immutable data, lightweight processes and inter-process communication, [per-process GC](https://www.erlang.org/doc/apps/erts/garbagecollection.html)
  > per process generational semi-space copying collector using Cheney's copy collection algorithm together with a global large object space
- Erlang is actually stricter about immutability than Haskell, in some ways, despite having no effect system
  - Every function call *could* have side-effects (except some BIFs (built-in functions) that are guaranteed to be side-effect-free, and thus safe to use in guards), but the only way that non-BIFs can have side-effects is via process communication or by calling NIFs
- [Erlang Term Storage](https://www.erlang.org/doc/apps/stdlib/ets.html)
- Reload/update modules (qualified references, as opposed to same-module unqualified references)
  - A bit difficult to actually plan around in a robust way ([e.g.]{t=} doesnʼt make sense to rely on it from PureScript)
- [Distributed processes/applications](https://www.erlang.org/doc/system/distributed.html)

### OS/POSIX

### Unix/Linux/systemd

- Transfer file descriptors over Unix sockets
- Sandboxing-type stuff, chroot, bind mounts, overlayfs, ...
  - cgroups via [`systemd.resource-control`](https://www.freedesktop.org/software/systemd/man/latest/systemd.resource-control.html), can set RAM limit (`MemoryLimit`), total memory limit, niceness (scheduling priority), etc.

### CPU

- CPU cache is a huge issue: a lot of effort goes into designing cache-friendly algorithms and data structures (and cache is a source of errors, vulnerabilities, weaknesses)
- Atomic operations, SIMD operations
- Thread-local storage that gets migrated with the thread, or [not](https://mcyoung.xyz/2023/03/29/rseq-checkout/#building-the-checkout-desk)


## Features

### Threading

Threading is something that every aspect of the runtime needs to pay attention to, especially wrt mutability (both explicit mutability like `IORef`{.haskell} and implicit mutability like evaluation of lazy thunks)

### Weak Refs

A lot of runtimes need to provide dedicated support for weak refs, integrating them with the garbage collector in some way (and even providing callbacks for when objects are GCed).^[Even in languages without GC, it is often necessary to specially support weak refs in some way(s).]

### HTTPS CAs

Languages tend to have to provide defaults and configuration for security, [e.g.]{t=} Node.js has `--use-bundled-ca` and other options for certificate authority configuration.

### JIT

JITs have a difficult tradeoff: JIT loading times versus code speed while actually running.
A lot of code (especially in scripting languages) is run during startup only and is not worth JITing.
However, really powerful JITs like for JavaScript and WASM, can do amazing things with using runtime information to rewrite more optimizations during runtime for “hot” code that is used in predictable ways, even with JavaScript dynamic typing.

### FFI/Native Code

Node.js addons, Erlang NIFs, Haskell FFI

### Debugging, profiling, coverage
