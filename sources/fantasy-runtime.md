---
title: Fantasy runtime
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2021/08/07
---

The idea is that all data will be easily serializable/transferable/sharable – particularly nonmutable data – so it will be clear the identity of data, the extent of what the data is and anything it references, the (current) value, its hash, etc.

The word, pointer, and synchronization are the only really primitive notions to implement. Memory management and other things can be built on top.

<!-- Maybe also pigworkerʼs notion of like reference pointers? for like skip lists? -->

Immutability and reference semantics will be built into the language.

There will be some simple language of conditions/guarantees on the data, that represents both the language-internal invariants as well as the user-code invariants/knowledge. In particular, if enough is guaranteed about the type of data, it can be unboxed. On the other side, if data is known to be (im)mutable, that part can also be left out.

Note: maybe data can be locally mutable à la ST in Haskell. In that case it won’t have synchronization primitives, but it also can’t escape scope.

Not sure if synchronization primitives can/should be shared across different segments of data, if they are “really” the same object (e.g. a linked list).

Invariants can be freely added to immutable data (STM style maybe). For example, sets and hash maps will be implemented as arrays with verified order or something. Invariants are fixed on mutable data and must hold true after each atomic operation.

Memoized data is similar to invariants – similar problems with caching/invalidation, verification, just with actually storing data alongside. And whilst you want invariants to always hold (see above), memoized data should be computed lazily.

Incremental functions can propagate invariants and memoized data.

Maybe this is actually the start of a FRP library.

Higher level type system on top

This is where objects acquire behaviors and meaning.

---

lightweight types

data layout:

- total length in bytes
- type
- additional header based on data type (more type information/metadata)
- (user type information/metadata?)
- [actual data]
- nullable pointer to memory management and synchronization metadata

data types:

- concrete/unboxable:
    - n<8: n-word (incl. 0)
    - data pointer (strong and weak ref)
    - transparent reference to avoid copying immutable data?
    - hash
    - foreign (ghost) pointer
    - wrapped data with verified invariants (subtype) and memoized data
    - raw buffer (shortcut for 1-word array)
    - function?
    - double?
- high order bits:
    - compound (struct/record) with n fields
    - tagged (union) with n options
    - array with bytesize of element

<!-- https://nickav.co/posts/0003_wasm_from_scratch -->
<!-- https://wingolog.org/archives/2023/03/20/a-world-to-win-webassembly-for-the-rest-of-us -->
