Every^[citation needed] language is Turing-complete, but this is a list of the things you can do that arenʼt about computing numbers or reading and writing bytes: the capabilities of different runtime systems for languages.

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

### Haskell GHC

- Laziness, of course.
- [`GHC.Compact`{.haskell}](https://hackage.haskell.org/package/ghc-compact), a compact region which acts as a unit for garbage collection, needs to be fully evaluated data (not functions).
  - Can be compacted with and without maintaining sharing
- [`GHC.StableName`{.haskell}](https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.21.0.0-8bb5/GHC-StableName.html), a kind of pointer identity that is explicitly preserved by the (copying) garbage collector
- Weak Refs
- Mutability vs immutability is also integrated into the GC
- Stable pointers, static pointers, pinned memory, [etc.]{t=}
- [STM](https://hackage.haskell.org/package/stm), [`MVar`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Concurrent-MVar.html), `ST`
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
A lot of code is run during startup only and is not worth JITing.
However, really powerful JITs like for JavaScript and WASM, can do amazing things with using runtime information to rewrite more optimizations during runtime for “hot” code that is used in predictable ways, even with JavaScript dynamic typing.

### FFI/Native Code

Node.js addons, Erlang NIFs, Haskell FFI

### Debugging, profiling, coverage
