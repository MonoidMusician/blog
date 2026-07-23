---
title: Waterfall Model of Types
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026/07/22
toc: false
---

This is my waterfall model of types, for thinking about compilers and programming language design.
It is clearly biased towards Haskell but useful more generally, I think.
A lot of compilers may only include a few stages from this model; none of them include stage 0.

------------

<br/>

**Waterfall Stage 0** is what the user intends for the types to express.
This is a problem because there is no way to convey this.
But it is all-important:
Everything flows from user intent.
***Everything***.

The holy grail of programming is how to express user intent with just the right amount of verbosity to make it clear and rigorous but not full of boilerplate.

Types are a key part of this, and one of the most important ideas is that types can be the same in every respect *except* for how the user intends to use them.
They can be isomorphic, they can be represented the same computationally, but be intended differently.
(However, on the other hand, intention may alter usage patterns so that similar types lean towards different representations for efficiency.)

<br/>

**Waterfall Stage 0.5** is where the user gets to make their type design known in concrete terms, but before the logic of the programming language is in full force.
This stage is concrete in the sense that it is executable code, but it is not concrete enough to actually understand what the types *are*: they are not be enumerated or their relationships may not be solidified yet.
Think of metaprogramming and code generation: whether it is a parser generator or [Template Haskell](https://wiki.haskell.org/Template_Haskell) or a Python script outputting types from an OpenAPI schema.^[Of course you can have fully dynamic types and programs, but the lack of static analysis becomes a hinderance.]
But those are pedestrian examples: this area deserves a lot more love and research for sure.

<br/>

**Waterfall Stage 1** is The Concrete User-Facing Programming Language: the logic of what the compiler deals with, after parsing, templating, and basic desugaring – but still pretty close to the literal source code.

This stage is all about nominal types, ever so important for capturing user intent for overloading (with typeclasses or other mechanisms) and also for clarity (newtypes and checked constructors, for example).
Its goal is to be comprehensive yet cohesive: to make sure that the user intent is coming through and that the program works as a logical whole.
Restrictions are placed, like disallowing [Orphan Instances](https://wiki.haskell.org/index.php?title=Orphan_instance), for the Good of the Logic (and ease and predictability of implementation).
Users can write elaborate programs and have guarantees about how it will run and how it will fit into the larger picture.
But, importantly, you do not need an implementation of this layer: it makes sense on its own.

<br/>

**Waterfall Stage 1.5** is the next layer down from that.
It is a step towards implementation *and* it conveys information back to stage 1.
Typeclasses are quickly [scrapped](https://haskellforall.com/2012/05/scrap-your-type-classes), completing desugaring/elaboration by compiling dictionaries into ordinary datatypes and regular argument passing.
Other forms of subsumption are also elaborated into core terms.

The `Coercible`{.haskell} typeclass and `coerce`{.haskell} function are all about this layer: the Haskell compiler can guarantee that, by looking through newtypes, these types are zero-cost coercible, and in fact, those coercions could be elided altogether.
We have not committed to a data representation, just grouped certain types together in their representation.
This is not going to let you coerce from an enum type to an integer type, for example, even if they are both stored in memory the same way.

<br/>

**Waterfall Stage 2** is How The Compiler Thinks The Types Should Be Represented.
Nominal types are no longer relevant: everything is structural now.
This is where the compiler gets to make decisions: data layout, boxing, float and integer sizes, tagged unions, all that fun stuff.
Each type now comes with a description of how it is represented, how it interacts with the garbage collector, how you construct and destruct it, and other important details.
Elaborate pattern matching is gone, replaced with simple case analysis on a single datatype at a time^[So it can be compiled down to jumps in assembly.].

We are not down to raw pointers and bytes yet, though.
These types have been washed clean of their trappings of user intent, but they still come with strict contracts for how the compiler and runtime must work with them.

Few user-accessible facilities will see through this stage, mostly those marked as unsafe like [`unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (a :: TYPE r1) (b :: TYPE r2). a -> b`{.haskell}](https://hackage-content.haskell.org/package/ghc-internal-9.1401.0/docs/GHC-Internal-Unsafe-Coerce.html#v:unsafeCoerce-35-), as opposed to proper systematic interfaces.

<br/>

**Waterfall Stage 3** is the memory model.
The data has now been unrolled into word and byte arrays, but pointers are still present as a distinct concept.
You want some notion of pointer provenance and typing, saying that pointers are allocated, tracked, dereferenced, and deallocated correctly.

This is the stage that the garbage collector cares about.
You can envision a simplistic stage 3 where each object comes with an allocation length and a bitmask saying which words in its data are pointers that the garbage collector needs to chase.
Maybe the garbage collector can even be implemented as a program in a language designed for this stage.

You can also do fun things like serializing shared and cyclic data by tracking which pointers have been written already and generating backrefs as needed.

Some memory optimizations may be unlocked by treating data as bytes and words.

<br/>

**Waterfall Stage 4** is where it ends.
Pointers are now just bytes like everything else, maybe you add pointer tagging or encode them with [NaN boxing](https://docs.rs/boxing/latest/boxing/nan/index.html) or [NuN boxing](https://wingolog.org/archives/2011/05/18/value-representation-in-javascript-implementations).
You hope that you have done everything correctly for the machine to interpret data according to the semantics of the program and hopefully user intent too.
Everything is a cascade of tiny side-effects and unchecked memory access speeding along at a few GHz.

<br/>

------------

What is the waterfall model good for?

It is good for talking about how user intent gets encoded and eventually discarded.
The key takeaway is that nominal types are for the user and typechecker, and structural types are in the implementation.

The waterfall is great for talking about optimizations.
Aspects of user intent are lost along the way, but details about implementation are added and refined.
Different optimizations naturally apply at different points of the waterfall.
Sometimes user intent needs to be clawed back: static analysis recovers information about the range and shape (bit pattern) of numeric values possible in the program to enable certain optimizations.

A lot of meaningful optimizations apply best at a high level: list/stream fusion, the idea that what you get out of a map should be what you just put into it^[This depends on lawfulness of `Ord`{.haskell} and correctness of `insert`{.haskell} and `lookup`{.haskell}.].
Other optimizations need to wait until decisions have been made about data layout and representation: bithacking, coercing a data tag, and so on.
Sometimes happy coincidences turn into opportunistic optimizations; rarely the compiler may shuffle its decisions around to make things coincide.

Key to the story of optimizations is the accounting of side-effects, and even benign allocations eventually become impenetrable side-effects that the compiler is not sure whether it can elide.

<br/>

------------

<br/>

The waterfall model highlights lots of opportunities for improvement in how we do things.

**Stage 0.5** is awkward in every implementation thus far.
Being able to describe what you want types to be and do without having to spell out all of the details is complicated, even in high-level languages with typeclasses and other type-directed codegen options.^[One particular annoyance in Haskell is that there is no option to “derive all the appropriate typeclasses”, you must still list them out. Mostly I wish for this with very lightweight newtype wrappers and for new monad transformers or classes.]
To date, there seems to be no good balance of ease-of-use, rigor, and efficiency for most metaprogramming options.

Staged compilation with dependent types *may* be one solution here, so I am trying to implement it in [Pudding](github.com/MonoidMusician/Pudding) based on the outline in [Staged Compilation with Two-Level Type Theory](https://andraskovacs.github.io/pdfs/2ltt.pdf).

Similarly, **Stage 1** is overly restrictive: Haskell specifies exact datatypes and pattern matching here, but we often want more flexibility.
For example, ideally the compiler would be able to freely optimize programs to use list and text builders locally, or even ropes.
(It can do so with fusion rewrite rules, but that is too local.)
Laziness helps with abstraction here, it allows more flexibility in interleaving and skipping computations, but when you pattern match a datatype in Haskell, Haskell says you are bound to be matching on *that* datatype.

And sometimes we write simple code first but then want to associate additional metadata on top of the data.
[AST]{t=} nodes that cache what variables they refer to, stuff like that.
Now you have to modify your datatype and every consumer and producer of it to retain that information, even places that should be oblivious to it.
This is something I want to fix in [tmTTmt](tmttmt.html#motivation).

On the flipside, sometimes language semantics are a bit too loose for user intent.
Sometimes you want to tell Haskell that pointer identity does matter, and not to elide allocations, because you are using finalizers or [stable names](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/GHC-StableName.html) on otherwise pure data without using [`IO`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/System-IO.html#t:IO) to sequence it.

Or sometimes the language semantics are fine, but you just want different semantics at/beyond Stage 2.
Maybe you need to match the layout of a struct in C or GLSL.
Maybe you want to know that your tuple of booleans fits into an atomic word more than that it is coercible with other tuples.

My [WASMFP](https://github.com/MonoidMusician/blog/blob/main/Haskell/src/WASMFP.hs) experiment, making a simple functional programming language that compiles to [WASM]{t=} with [GC]{t=} types, is unique in that each type can specify how it is treated by the surface syntax (like an [ADT]{t=} or like an array or opaquely) and how it is compiled to WASM on the backend.^[This uses [existential types](functions_as_data.html) to be easily extensible.]
This really cleanly illustrates Stage 1 nominal types versus Stage 2 structural types, and the WASM implementations handle the later stages: allocating GC types and tagging them so that `i31`{.wasm} can live on the stack with references.

Even more extreme than this, I think it would be really cool if types were allowed to specify their data layout exactly with a bidirectional parser–serializer, and all of the high-level data accesses were compiled to operate on that efficiently, somehow.
Imagine if your implementation of a media codec like H264 or AAC was literally a series of datatypes that fit the bitstream exactly but also pulled out the important semantics into the user types you care about.
How much tedious code would that eliminate, right there, all on its own?
