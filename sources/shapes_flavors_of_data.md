---
title: Shapes and Flavors of Data
subtitle: The essence of runtime, and what graphs have to do with it
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
date: 2024/01/28
---

I want to talk about data today.
In particular, I want to talk about runtime representations of data, **real** data – that is, potentially mutable and referentially opaque data – and demystify what they actually are in terms of more familiar notions of data.

You shouldnʼt just throw up your hands once you have cyclic references!
Itʼs possible and worthwhile to design tools to work with the raw graph of runtime data, no matter its shape.

(Well, okay, you might give up at functions and sockets, for example – thatʼs okay!)

The main thing we will work up to (and go beyond) is Pythonʼs [`pickle`{.python} module](https://docs.python.org/3/library/pickle.html), including my own implementation of it for the [Nasal scripting language](https://wiki.flightgear.org/Nasal_scripting_language) (which I believe is cleaner in some respects, although obviously less industrial).

## Background

This post has spent a long time marinating in my mind, so Iʼm happy to finally get it on the page!

Iʼve identified some basic flavor profiles of data:

#. Pure (immutable) data
    - Either acyclic …
    - … or allowed to be cyclic
#. Mutable data (say, in-memory)
    - Youʼre basically forced to allow cyclic references!^[Except in Rust, I think?]
    - Sharing matters!
#. Data defined in reference to external things (like file or socket handles)
    - External is relative – as it should be!
    - Your need to think about capturing intent
      - [e.g.]{t=} file paths are probably your best indicator of intent – but if it was a temporary file, you could probably create a new temporary file instead!
        but oh now what if a different string is storing the path of the file, now you donʼt know how those pieces of data relate anymore and cannot update that string …
#. Functions? *Are* functions data?? (Iʼm including closures and procedures and routines and all that stuff in my definition of functions, btw.)
#. Quotiented data (Agda, Lean, Rocq)
    - see [Subtypes/Quotients: Lies Told in Defense of the Truth](adt_lies_for_truth.html) ([WIP]{t=})

It will be important to think about what notions of equality are useful and adequate for these kinds of data!

We will talk about runtimes, including what garbage collectors do.
Garbage collection is pretty special, since it gets into all the nitty gritty implementation details of a runtime: it essentially pins down the data model of most languages that have garbage collection.

It is also important to talk about what capabilities the language provides for working with and thus distinguishing data.
If the difference cannot be observed, it does not exist!
(And we may be “morally” justified in ignoring it anyways.)

### Serialization, deserialization

Serializing in its basic form – for pure data – means writing it out to a string such that it can be recovered from that string.
(Bitstring, bytestring, Unicode string, even a bignum – the exact details do not matter.
It also does not matter if it is a well packed encoding or a sparse encoding with lots of invalid strings that do not deserialize.)

Pickling is more general: it means serializing non-pure data by taking a snapshot of mutable data, preserving referential identity in the process (this is hard!), and doing your best with external references, and maybe giving up on functions.

Part of my argument is that serializing and pickling is very tied up in what data means!

Like, What is the essence of data?
I hope it is something you can grasp fully, explain fully, and write out to a file and fully reconstruct in a new runtime context.
(Mumble mumble: decidable equality, computation, countability.)

### Equivalent vs equal

I think I have definitions of equal and equivalent that are useful.

Equality in the classical sense^[Leibniz equality, “Identity of indiscernibles”] is going to be too strict, I argue, for the notion of data I want to consider.
This happens as soon as you have references with mutable identity.
First of all, it is hard to compare values across runtimes, which is silly!
Data does not only exist for an instant in time!
And two mutable objects can be _interchangeable_ even if they are not _identical_ references.

:::Key_Idea
So I will use “equal” to mean “values that live within the same run of the runtime and will definitely act the same, thus equal in all ways the language could distinguish^[besides performance – we always disregard performance here]”.

And I will use “equivalent” to mean “values that, if they exist in the same run of the runtime, would cause no difference if all references to them were swapped and then their children were shallowly swapped; otherwise, values that would act as similar as possible across different runs of the runtime”.
:::

:::Example
We definitely need to walk through an example of what “equivalent” means, unpack my definition.
Hereʼs one in JavaScript, although the particulars of JavaScript do not matter:

```javascript
// Set up some data
let shared = [];
let v0 = { 0: shared, 1: [], 2: 2 };
v0[3] = v0;
let v1 = { 0: shared, 1: [], 2: 2 };
v1[3] = v1;
shared.push(v0, v1);

// I hope you agree that this characterizes
// the state of the data, most or less
for (let v of [v0, v1]) {
  // shared reference
  assert(v[0] === shared);
  // equivalent empty arrays,
  // though not equal
  assert(v[1].length === 0);
  // index 2 is 2
  assert(v[2] === 2);
  // self reference
  assert(v[3] === v);
}
// and they are referenced by shared in order
assert(shared[0] === v0);
assert(shared[1] === v1);

// they are not equal:
assert(v0 !== v1);

// but we can swap them, ...
[v0, v1] = [v1, v0];
// ... all references to them, ...
[v0[3], v1[3]] = [v1[3], v0[3]]; // yes, self references count
[shared[0], shared[1]] = [shared[1], shared[0]];
// ... and the immediate references they hold,
// since they have the same shallow structure
[v0[0], v1[0]] = [v1[0], v0[0]];
[v0[1], v1[1]] = [v1[1], v0[1]];
[v0[2], v1[2]] = [v1[2], v0[2]];
[v0[3], v1[3]] = [v1[3], v0[3]];
// this last one fixes up their self references again!

// Now we have the same observable state of the world!
for (let v of [v0, v1]) {
  // shared reference
  assert(v[0] === shared);
  // equivalent empty arrays,
  // though not equal
  assert(v[1].length === 0);
  // index 2 is 2
  assert(v[2] === 2);
  // self reference
  assert(v[3] === v);
}
// and they are still referenced by shared, in the
// order corresponding to the same variable names
assert(shared[0] === v0);
assert(shared[1] === v1);

// so we conclude that `v0` and `v1` are equivalent!
```
:::

:::Note
Note that “equal” and “equivalent” are, strictly speaking, external notions.
However, “equal” is often testable internally, such as by `===`{.javascript}.
(Except for the case of `NaN`{.javascript} – you technically have to use `a === b || (a !== a && b !== b)`{.javascript} to detect `NaN ==== NaN`{.javascript}, and I believe that different `NaN`{.javascript}s are indistinguishable.)
:::

In Dhall, equal and equivalent happen to be the same!^[Citation needed.]
Unfortunately this is more a reflection of the rather stagnant notion of data in Dhall – no mutability, no references, no cyclic structures.

<!-- Iʼm trying to think about whether In some hypothetical dynamically typed, imperative runtime they could also be the same – if you outlawed referential equality comparisons.
(Thatʼs kind of the whole point of this article!) -->

In general, as soon as you have references, equality and equivalence will not be the same.

(You can always^[] use test mutations to tell when two references are identical or not, so you cannot just outlaw referential equality and hope that they become indiscernable.)

And practically speaking, there are always wrinkles.

:::{.Details box-name=Rambling}
Maybe there are bignums who can be observed to have different encoding by esoteric functions, but both encode the same number – you would be justified in wanting to consider them equal.
(Especially if this is a distinction that can be observed by those esoteric functions but not reflected by a pickling function, for example.
That is, if you could know that one number has an overlong encoding by virtue of being observably _different_ from the other number, but not know which number is overlong, or, indeed, how overlong it is.
Well, I suppose pickling would mean you have a canonical, short form, so it would just be a question of not knowing how overlong it is – maybe even being able to reconstruct an overlong form without trying a bunch of weird math operations to force decanonicalization, which would potentially get undecidable.)
:::

## By Concept

Letʼs talk about concepts of data that exist across specific languages and runtimes!

### Strings

As a warm-up to thinking about references and pointers:
Letʼs observe that immutable strings are treated specially by a lot of runtimes!

V8 has a lot of different representations of strings, and it is free to change representation between them, [e.g.]{t=} while garbage collecting.
It is free to make these changes since they arenʼt observable from JavaScript (outside of performance characteristics).

Erlang likewise has immutable binaries (bytestrings) which serve similar purposes although they only support fast slicing (no fast concatenation).
To get fast concatenation, users are expected to build up nested lists and flatten it out into one binary at the end of all the concatenation operations.
(Luckily it is rare to want to index back into a binary as your are building it up, so this is acceptable for most use-cases.)

The thing that the Erlang garbage collector is specifically allowed to do is to delete old binaries whose contents are only partially referenced.
Then it has the task of fixing up the references to those slices to refer to the new binaries with adjusted indices.
This is definitely mutating data at some level ([e.g.]{t=} the level of raw memory), but it is not visible from Erlang at all.

Anyways, this is just a nicer example of stuff the runtime can do behind the scenes that has a very precise semantic description.
As we start to think about references and managed pointers and external references and functions, it gets less clear!

It is also a good warning to be careful about what “immutable” means: since every runtime uses mutation at some level, we only care about immutability as viewed from the language itself.

### Mutability or purity

Mutability should be one of the first things you think of when you think about data, especially if you have had some exposure to ideas from Functional Programming (FP).

The really nice thing about pure languages like Haskell and PureScript is that they separate out [mutable](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-IORef.html#t:IORef) [references](https://pursuit.purescript.org/packages/purescript-refs/6.0.0/docs/Effect.Ref#t:Ref) from pure data, and dispense with mutable variables altogether.
It affords us a much nicer toolbox for expressing these concepts, in theory and in code.

#### Referential identity vs referential transparency

One way to say it is that pure data is characterized by its serialization.
If it makes no reasonable difference whether you keep using the original value versus the value after it has been serialized and deserialized, then that is pure data.
(This is a reformulation of referential transparency.)

Mutable data cares about its referential **identity**, on the other hand.
(Note that “reference” most often means something like “pointer”, but it doesnʼt have to be restricted to that.)



#### Sharing references

The thing about pure data is that it essentially doesnʼt really care if it is a tree or a DAG.
I mean, it matters in terms of efficiency and storage space!
(Though this is rarely exposed at a useful level.^[Sighs in [tmTTmt](tmttmt.html) …])
But the results of an algorithm wonʼt be changed just by sharing more or less internal structure among the immutable data.

Mutable data pretty obviously cares about sharing.

### Functions

Functions are tough.
When viewed from above (mathematically) and below (most runtime implementations), they cannot be sensibly serialized.

One thing that is always true^[Even in non-Turing-complete languages!] is that you **cannot** expect to know when two functions are equivalent.
They may happen to be equivalent in a obvious way, but if they arenʼt, you cannot necessarily find out that they actually have distinct behavior.
(In other words: equivalence is semidecidable [reference needed].)

If you have the original syntax defining the functions, you *do* have a snowballʼs chance in hell of deciding that two functions are equivalent, by doing some procedures to normalize the syntactic definitions of the functions.
(One difficulty is that you will also have to keep track of closures, including captured variables and open variables.)

But once the syntax is forgotten ([viz.]{t=} at runtime), the best you can do is a pointer comparison, which is a very poor decider of equivalence.

The funny thing is that, although all functions start out as syntax, syntactic normalization is very rare for languages.

Only a few term-based languages like Dhall will always keep functions as syntax^[This is slightly unfair, considering that Dhall now uses [NbE]{t=}, but it amounts to the same thing.], and only theorem provers like Agda/Lean/Rocq will keep the function syntax around for their core operation (whereas code extraction and runtime is almost entirely a separate issue).

(Aside: In theory you could defunctionalize a whole closed-system program right? At least for pure data? Maybe for mutable too?)

However, in languages like Agda, each function definition is considered distinct, so it might as well be a pointer check.
(This is true for almost all theorem prover languages, certainly for recursively-defined functions – Dhall just avoids the issue by not having recursive functions!)



## By Language

To really talk about data though, we have to talk about specific runtimes.
Weʼll work our way up from simplest to more thorny.

### Fantasy

Languages that nobody actually runs.
Theyʼre just too academic or something.
(Joking.)

#### JSON

JSON is a pretty great place to start.

JSON is pure, immutable data.
It is acyclic and unshared.
If you want to represent sharing, you need to represent it at a different level!
By encoding some notion of identities and references, that is.

Many other things (like binary data/bitstrings) are mathematically equivalent to JSON in this regard.
They are “just” pure data, in which you can encode any other pure data, with more or less help from existing structure like arrays and objects.

There are some wrinkles, like the fact that numeric precision is not specified, and the order of keys may matter to some runtimes and not others (okay, practically all runtimes [mostly](https://stackoverflow.com/a/5525820) preserve order by now).

Note that JSON is often deserialized to _mutable_ data.
This means that _mutable_ data may not roundtrip through JSON: shared values will be unshared.
But Iʼm getting ahead of myself.

#### [Dhall](https://dhall-lang.org/)

From an abstract perspective, Dhall is pretty much like JSON, with a couple key differences:

- The data is strictly typed.
- You can serialize functions!

Dhall comes with a good notion of equality – judgmental equality – which applies normalization to open terms (expressions with free variables – in particular, functions).
By applying algebraic reasoning, judgmental equality can sometimes show that two functions are equal, but not always!

#### [Agda](https://agda.readthedocs.io/en/latest/)

(I use this mostly as a catch-all for theorem provers.)

Agda is kind of like Dhall, but its emphasis is different, and it has more features.
As mentioned somewhere here, recursive functions mean that it gives up even earlier on detecting equality of functions.
The other important new feature is quotients.

Quotients are interesting: they are – at some level – represented by the same (runtime) data as everything else, but some of the underlying data has to be treated as equal by _everything_ (in particular, by equality, and thus by the rest of the theory, since everything has to preserve equality).

Specifically, Higher Inductive Types and Observational Type Theory are interesting topics here, but way too deep to get into at the moment (and well covered by others – not something I feel much need to opine on in this context).

### Real World

Okay maybe some people actually run these languages and interact with their runtimes.

#### Erlang/Elixir/BEAM

Erlang is pretty interesting, for an untyped language.
Data is fundamentally immutable^[BEAM uses mutation to construct data locally, but this is only an implementation detail that cannot be observed from Erlang.] and acyclic and this has influenced many aspects of its design.

This enables somewhat exotic features like non-linear pattern matching and deep equality, which are unheard of in untyped, mutable languages.
(These are the same feature, actually. Non-linear pattern matching is implemented via deep equality.)

In fact, Erlang specifies a very specific serialization format for data, for seamlessly communicating across distributed systems.

<!-- As far as I know, -->


##### What are processes?

It is not the case that Erlang processes are data.
That is, processes themselves are not just a kind of data.
Erlang does not claim to let you save the full state of a process off to disk and restart it from the saved data.
That would be a little silly – what would happen to all the other processes it was bound to?
No, processes are part of a dynamic runtime system.

However, a _reference_ to a process *is* “just” data still, and can be compared for equality like any other data, and it can be serialized.

(The same for BEAM functions and closures, btw. Their references are opaque in some sense, but still just data.)

And interestingly, the dynamic, asynchronous nature of processes means that they *must* expose mutation.

Indeed, one way to implement a mutable reference is to spin up a process that holds the “current” value^[inasmuch as a concurrent system can have a current state …] and returns it when queried and purely updates it in its state.
That is, its state is immutable, but external callers can see fresh data whenever they query the process.


#### Haskell

Haskell is actually one of the most complex ones here, since there are many levels you can talk about data at.

One the one hand, you can talk about pure data all day long.
You can pretend that it operates like Agda or Dhall – totally pure!
(Except you cannot compare functions for equality [except you can].)

You can even add mutable references (again with equality [again with the exception]).
Itʼs actually really beneficial to have this separation between mutable references and immutable data that they contain, but I didnʼt get into it here.

Mutable references form a possibly cyclic directed graph, just like the imperative languages we will be talking about.
But Haskell can also form cyclic data references via laziness: [“tying the knot”](https://wiki.haskell.org/Tying_the_Knot), as it is called.

However, Haskell is a bit weird in that you can also peek inside the machine, and compare pointers and do other “naughty” stuff.
This can be used to short-circuit deep equality comparisons via referential equality, memoize functions on uncomparable types, and other [legitimate](https://acatalepsie.fr/posts/overloading-lambda) [uses](https://dl.acm.org/doi/pdf/10.1145/3409002)^[See §3.2 “Stable Names”.].
Except it isnʼt so naughty (if you do it right), it is just a different layer of abstraction.

In fact, if you drill down, Haskell has some kind of data model that its runtime operates on.
This will tell you when it is okay to use `unsafeCoerce`{.haskell}, for example.
Itʼs maybe worth talking about the way Haskell evaluates, what its thunks represent, how mutable and immutable data work, STM, FFI,— but it just goes on for ages.

I think itʼs really worth thinking about this deeply, taking seriously the kinds of data that Haskell uses at runtime, and how references to them interact, how the garbage collector makes sense of it all.

But as we will see, an awful lot of runtimes seem to be about managing the graph of references of data, and itʼs useful to be able to work with those graphs at some point, even if the language is hesitant to give it up so easily.
(Clearly allowing a program unmanaged access to its own heap would be a disaster.)

#### JavaScript

JavaScript will be our stand-in for a scripting language with a simple, dynamically typed, mutable data model (and garbage collector).

Everyoneʼs familiar with the JSON side of JavaScript: it stands for JavaScript Object Notation, after all.
But once you embed in the larger language, you can get things like cyclic references via mutation.

As a silly example,
```javascript
// this is the “weird” JS `const`
// where the variable reference is
// constant, but the data is mutable!
const selfRef = [];
selfRef.push(selfRef);
console.log(selfRef[0] === selfRef);

console.log(JSON.stringify(selfRef));
// Uncaught TypeError: cyclic object value
```

This is no longer JSON serializable!

This one is still JSON serializable, but it no longer behaves the same after deserialization:
```javascript
const twin = {name: "Lo"};
const twins = [twin, twin];
const gemini = JSON.parse(JSON.stringify(twins));
twins[1].name = "Hi";
gemini[1].name = "Hi";
console.log(JSON.stringify(twins)); // [{name: "Hi"}, {name: "Hi"}]
console.log(JSON.stringify(gemini)); // [{name: "Lo"}, {name: "Hi"}]
```

So JavaScript, by allowing mutation, can observe cyclic and shared references that JSON simply does not have.

Most people donʼt reckon with this aspect of the runtime at a deep level!
Obviously they throw mutable references around all the time and understand that they are shared, and will design some kind of serialization format that uses IDs or something and then reconstruct the right shared references on top of that.
But they donʼt build debugging tools for JavaScript itself that would work with arbitrary data.

But what if you didnʼt have to?

Thereʼs actually a way to make a serialization so that `gemini`{.javascript} behaves like `twins`{.javascript}.
I call this pickling, after the Python library.
More on this later.

##### Functions

Not much worth saying about functions.
You can get their source code in JavaScript (why??), but you cannot observe their closure, so you cannot pickle them up thoroughly.

##### Globals, such as DOM references

As will be the theme, references to foreign data (not strings, numbers, objects, arrays) are tough.

Global object types are worth thinking about.
They are opaque to code, but in theory they live in predictable places in the global namespace each time, so the proper reference to them can be reconstructed.
However, thereʼs still complications, such as that different runtimes will expose different ones (Chrome, Firefox, Node, Deno, …).

By far the most common types of foreign objects will be from the DOM.
Some can be serialized pretty directly – at least snapshotted if they arenʼt immutable (like all the little attribute list or node list types, or bounding box type).

If you have a reference to an element with an `id`{.html:Attribute}, you might expect that you could serialize it, and then reload the page, and have it still refer to “that” element.
But “that” element doesnʼt exist anymore.
Maybe thereʼs a new one with the same `id`{.html:Attribute} – maybe the `id`{.html:Attribute} doesnʼt exist anymore!
Well, it is sort of the best marker of intent there is.

And so we come to the conclusion that we will always be struggling with the API boundaries of runtimes, of data that isnʼt constructed from within the language itself.
Once you have data inside your system that references things outside, how do you deal with it?
What kind of guarantees could you still get when persisting it?

##### Debugging cyclic structures

It is worth noting that NodeJS debugging facilities finally implemented support for detecting cyclic references when printing out structures and letting you know what the reference is.

Itʼs a simple thing, but itʼs a barrier that most people throw up their hands when encountering.

Itʼs also funny that when debugging things interactively, via point and click stuff, lazily expanding, that you donʼt care too much whether the data structure is cyclic or shared: nothing will blow up since it does not recurse automatically.

##### Pickling JavaScript runtime values

You can imagine an algorithm (well, I could write it if I wasnʼt sleepy) that pickles JSON-like objects, but in a way that respects mutability and sharing.
It would write out a function definition to reconstruct a new mutable object equivalent to the one it was given.

In JavaScript you would have to do this the slow way.
You would maintain a list of **all** mutable objects you have seen, along with where you saw them as a path down from the root object.
You would then output code that reconstructs an object incrementally, by adding properties in order, and grabbing shared references from other parts of the object as necessary (or caching them in variables).

The cool thing is that you can do it all with `const`{.javascript}!
You donʼt need mutability at the variable level, you can (and should) do it all at the mutable value level.

The algorithm I decscribed can do this for data that is simple in structure, but complicated in terms of references, and with extensions it could handle more things (like regular expressions would be easy to add, `undefined`{.javascript} would be trivial).
Actually, it is funny – it would also need to be extended to handle sparse arrays, and all of these little details tell you about how simplified JSON is from the actual data model of JavaScript.

This would give you your own faithful, accurate slice of the runtime heap as viewed from the perspective of one objectʼs watershed of references.
The resulting reconstructed value would behave the same with regards to mutability of its children.
It just would not compare equal with `===`{.javascript}, since it is a newly allocated value (and all of its children are too).

However, if nobody else remembered the old object, and you substituted in the new object very sneakily … nobody would know 🤫

#### Python

Python is pretty similar to JavaScript, in the rough kinds of mutable data it supports, but worth talking about separately.

It has more explicit boundaries around mutability and immutability in its data types.
(Although still not as nice as Haskell. And I suppose JavaScript has been getting a little more in the way of immutability and actual data types.)

Python also provides the pickling library that is one of the main subjects of this article.
More on this later.

Some wrinkles:

- The fact that [the hashing function rotates each run](https://docs.python.org/3.3/using/cmdline.html#envvar-PYTHONHASHSEED) is really interesting!
  It technically is an observable difference between runs, but it isnʼt some essential semantic feature of the data.
  And you would have the same sort of thing if you are allowed to see an ordering on pointers.

- External references, like files and sockets and other stuff – talked about elsewhere.

- Regexes are really interesting.
  Theyʼre pointers to foreign object (compiled regexes in some library implementation).
  But they can be reconsistuted into equivalent objects very easily.

##### Pickling Python runtime values

Itʼs worth getting to know the [`pickle`{.python} module](https://docs.python.org/3/library/pickle.html)ʼs capabilities and limitations, so I will just copy and paste the juicy bits here:

> The `pickle` module keeps track of the objects it has already serialized, so that later references to the same object won’t be serialized again. […]
>
> This has implications both for recursive objects and object sharing. Recursive objects are objects that contain references to themselves. […] Object sharing happens when there are multiple references to the same object in different places in the object hierarchy being serialized. `pickle` stores such objects only once, and ensures that all other references point to the master copy. Shared objects remain shared, which can be very important for mutable objects.

> `pickle` can save and restore class instances transparently, however the class definition must be importable and live in the same module as when the object was stored.

> Note that functions (built-in and user-defined) are pickled by fully [qualified name](https://docs.python.org/3/glossary.html#term-qualified-name), not by value. [[2]](https://docs.python.org/3/library/pickle.html#id8) This means that only the function name is pickled, along with the name of the containing module and classes. Neither the function’s code, nor any of its function attributes are pickled. Thus the defining module must be importable in the unpickling environment, and the module must contain the named object, otherwise an exception will be raised. [[3]](https://docs.python.org/3/library/pickle.html#id9)

> Similarly, when class instances are pickled, their class’s code and data are not pickled along with them. Only the instance data are pickled. This is done on purpose, so you can fix bugs in a class or add methods to the class and still load objects that were created with an earlier version of the class. If you plan to have long-lived objects that will see many versions of a class, it may be worthwhile to put a version number in the objects so that suitable conversions can be made by the class’s `__setstate__()` method.

This last quote raises an important point: there is some aspect of intent when restoring data to a new runtime.
Just because you named the class or function the same, does not mean it is the same class or function!
But it is a good marker of intent and worth preserving.

### Others

#### Go, Java, …

I actually donʼt know a whole lot about Go or Java.

But they need some structure, at least for garbage collection purposes!

“Heap layout”.

Basically every runtime *at least* needs to keep track of which piece of data is a pointer or not.

#### Rust

dunno?

#### C/C++

Uh, yeah.
Good luck.

Pointers “are” numbers?
What the fuck?!

Clearly thereʼs nothing much we can say about coherent semantics …
without getting really deep into the weeds of what is and isnʼt undefined behavior and why.

However, it does reinforce the point: at a very very basic level, OSes and memory management and stuff are about managing the graph of live pointers – it is just very very hard to determine what bytes are actually live pointers at any given point in a C program, and what bytes are other kinds of data.

## Nasal Scripting Language

Yeah, it gets its own section and backstory!!

[Nasal](https://wiki.flightgear.org/Nasal_scripting_language) is a small embedded scripting language.
Its name stands for “Not another scripting language”^[[“Nasl” was already taken](https://github.com/andyross/nasal/blob/088be4d3642f696ad99bad3c79d15b692b368934/www/index.html#L182-L186)].
Its only notable use is in the [FlightGear open-source flight simulator](https://wiki.flightgear.org/Main_Page), although [AlgoScore](https://kymatica.com/apps/algoscore) and a tiny handful of other projects use it.

Its data model is basically JavaScript, but simpler and better.
(Arrays are their own data type, and you can actually iterate over collections in a sensible manner. Good riddance to Lua and JavaScript. Ugh.)

It has some metaprogramming facilities by default, plus I prototyped some more of my own, including full bytecode decompilation.

Finally it has this one special function: `id(obj)`{.javascript}.
It returns a string representation of the pointer for any object!
```{.javascript data-lang=Nasal}
>>> id([])
'vec:0x7fea11014c40'
```
I mean, I guess it is like the `id()`{.python} function in Python …
Yeah, both use mark/sweep GCs, so pointers are stable.

Anyways, the other great thing about Nasal is that objects donʼt have constructors!
It is so liberating.

### Pickling Nasal runtime values

Pickling consists of writing out a file that, when executed, returns an equivalent object.
(The body of a file is simply a function. Plus all statements are expressions – they have a return value, although sometimes it is a pretty useless return value.)

- You initialize a hashmap of object ids that have been seen.
- For each object you see, you look at the hashmap:
  - If it exists, you reference the existing variable and stop walking the structure.
  - If not, you add it to the map, and add a variable to the file to save the reference.
- For non-recursive data, you just set it directly.
- For recursive data (objects and arrays), to handle cyclic references, you initialize it to an empty value and add items via mutation.
  Thus if those items mention cyclic references, that reference exists, and will continue to be built as necessary.

In lieu of a hashmap, you could even use a list of objects, and traverse it in linear time to compare referential identity.
This is possible in most dynamic languages, just really slow.

### Functions, builtins, bootstrapping

I also tried to work on a bootstrapping system for Nasal.
I never completed it.

Anyways, this is relevant because I did add bytecode decompilation for functions.
You could already inspect bound scopes of closures, and callers and local scopes of the call stack.
All that was left was builtins (which donʼt have bytecode).

If you have a contract with the bootstrapping system, you could look up the globally accessible names for the builtin functions that you could not decompile, and hopefully assume that equivalent builtin functions would live in those same spots on the next bootstrapping too.

The problem, still, is builtin external references, like files and such.
Some could be supported on a case-by-case basis, but not everything.

Also, ideally you would airgap^[Sorry, wrong word.] the builtins, since some builtins are fake builtins.
That is, they are wrappers over actual builtin functions, but you could only access those builtins through the closure of the wrapped function – so you might as well stop bytecode decompilation for the wrapped functions (by wrapping the decompiler!) and treat them as builtins.

Anyways, in theory you would almost be able to save the entire state of a running Nasal system and fully reconstitute it under an equivalent bootstrapped environment.
At the very least, you would expect to be able to save complicated data with simple functions.

### Equivalence

The pickling process points towards a method of determining equivalence.
Obviously you should sort the file in some semantic way, and rename variables to less arbitrary things.
Maybe normalize the bytecode for functions.

After that, you should just be able to compare your resulting files and use that as a notion of equivalence!

Alternatively, you could write out a direct algorithm: take two objects at runtime and walk them recursively with the same kind of hashmap trick, comparing at which paths you see the objects, and then just make sure the shared references appear at the same minimal-paths from the root across their sharings.
(You want to avoid cyclic recursions, of course, which does mean you will only look at minimal paths.)

Iʼll call this … “graph equality”?
“Stable equality”?
It is what Iʼve meant by “equivalence” all along.

### Ghost objects (foreigns)

Nasal has a concept of “ghost” objects which are pointers to foreign objects, literally just C pointers with some associated data to help the garbage collector.

These are constructed by C APIs, and the only way to reconstruct them would be if you can call those APIs again to produce equivalent objects – which may not always be possible.

### Strings

Nasal strings were actually a fun challenge.
It has mutable strings!

It has immutable interned strings, which are cached in a global lookup table.
This is used for identifiers (including object keys), to speed up comparisons.

It also has mutable strings (and I believe they can be mutated to be immutable? it is a little weird).

The referential identity of strings is not exposed – the equality operator ignores it.
However, you can still determine whether two mutable strings are the same reference, by using test mutation: if you mutate one and the other stays the same, they are different references.

(You can even use missed fast comparisons to determine if a string is interned or not.)


## Broader Thoughts

The main thing I want you to take away is that dynamic runtimes donʼt have to be scary places filled with spaghetti data flying around all over the place.
Itʼs actually possible to tame mutable references in many ways!

The concrete lesson is that there are three useful notions of equivalence, that are used for characterizing what levle of abstraction of “data” we want to be looking at, and I think the middle one is much more important than we give it credit for:

- Referential equality, which treats everything as live, mutable references, but is too fine-grained and doesnʼt make sense across restarting the program.
  This is the notion of equality that your runtime (and particularly its garbage collector) is tasked with preserving for your code.
- My new notion of equivalence, which I will call “graph equality of mutable data”, which keeps track of shared mutable references and so on.
- The notion of [“deep equality”](https://lodash.com/docs/#isEqual) of objects, which treats them mostly as if they are immutable data.
  (I didnʼt talk about it at all, whoops, but I assume you are familiar with it.)
  It can be very useful, but it acts a lot like traditional serialization, and isnʼt comprehensive enough to actually probe the whole of a running system.

So while referential equality forms your basic data model of a language, I encourage thinking about equivalence.
**If you could swap out two objects completely** (including _existing_ references to them), **would you be able to notice?**

And then you need to keep abstracting away what you care about.
Do you care about exact hash values?
Do you store those hash values in a way that would make reconstruction fail to mean the same thing with a different random seed?
And so on.

So thereʼs still domain-specific work to be done, as there always is.

:::Key_Idea
But if we can expose the underlying graph of referential relationships, we have a much much MUCH larger toolbox for working with data and data serialization.
:::


<!-- The main take-away is that there is *some* notion of equality that your runtime respects.

One of the main ways you see this is in the functionality afforded to you by the code you are allowed to write in the language.
The flip side of that is that the runtime does its best to preserve these semantics, particularly the garbage collector, which has to shuffle objects around without you noticing!

Thereʼs always some layer of abstraction between you and the raw data that the garbage collector sees, otherwise each garbage collection would have the potential to break your world if you are not careful.

This -->

### Graph equality of mutable data

:::Key_Idea
Is (shallow) referential equality the best we can do?
What about deep (explicitly non-referential) equality?

Emphatically no – but, it is probably not worth it.

See, we could have the notion of equality that pickling and unpickling preserve.
Graph equality, where the sharing of mutable references is tracked and tabulated by their role, instead of exact referential identity^[think: pointer].

If you have two data graphs where pointers are shared in equivalent ways, sure, they could totally be considered parallel universes and interchangeable amongst themselves.
(Obviously if something external holds references to them and you donʼt have a way to swap them out, this can break.)
:::

The only problem is that it is pretty expensive, it requires a _lot_ of bookkeeping, and most people generally donʼt care – they are fine either writing the equality comparison they need, or settling for the standard deep equality.

However, it is very useful!

Like, as one example, this is literally what [stable names](https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-StableName.html) are used for.

:::Key_Idea
Runtimes are literally built on graphs!

We want to be able to touch this.
To expose it, to hold it in our hands.
To work with it, to meld it to our own needs.
To chart our own course through the graph, traversing references and recording where weʼve already visited.

We can have very nice things if we give up the dichotomy of referential identity versus deep equality, and embrace the graph nature of runtimes.
:::

<!-- ### Notions of references

Erlang????
Stable, bookkeeped to actual pointers by [GC]{t=}?
Idek.

Like, it is kind of fucked up that some languages would let you serialize a pointer, and then lose it. -->
<!--
### Semantic identity of _external_ references…?

Well, thereʼs what I talked about above in graph equality.
If you have two data graphs where pointers are shared in equivalent ways, sure, they could totally be considered parallel universes and interchangeable amongst themselves.
(Obviously if something external holds references to them and you donʼt have a way to swap them out, this can break.)

But more generally, you cannot know if two references are supposed to be “the same” – [e.g.]{t=} across runs of a program (where you cannot even compare pointers, or, well, you at least would know it is a foolsʼ errand to compare pointers).
It becomes about _intention_.
Are the references created by the same code, during calls in different runs that are meant to be equivalent?
Itʼs really hard to know!

The main difficulty is in dealing with external APIs and external resources, as we have talked about.

Think about external file references: technically they are just numbers, at like the POSIX level!
Itʼs pointers again!
But maybe you want to consider.
But that doesnʼt actually mean that they were intended to be the same.
Or on the flip side, what if they are pointers to tmp files?
Symlinks?
Bind mounts?
It gets really complicated.

The best you can do when serializing it would be to write out the `open()`{.python} call, basically.
The file name.
(What if you run as a different user and donʼt have the necessary permissions anymore? lol) -->

### Constructors

As part of these musings on data, I subscribe to the idea that the only objects that should have constructors (at the level of data – obviously client code will want different abstractions) are objects that are constructed from external references, FFI.

Idk, constructors in the sense of mainstream OOP are mostly a distraction for this view of data I want to talk about.
They just arenʼt good, arenʼt necessary, they get in the way – especially since the arguments to the constructors donʼt have to correspond to the runtime data of the object at all.

## Appendix

Miscellaneous thoughts that donʼt belong in the conclusion but do belong at the end.

### Uses of references

You really should not be able to ask for two references to be ordered against each other: it doesnʼt mean anything with regards to the _meaning_ of the program (although it may record some historical data about how the program _did_ happen to run).
But you kind of should be able to put them into a map efficiently, and ordering/hashing is potentially important for that, but only if it can be stable.

Mark/sweep GC is good for stability of pointers (and thus comparisons).
I think mark/compact still can preserve comparisons.

Weak references are interesting.
Every time Iʼve wanted weak references, Iʼve always actually wanted to do them as reverse references: data stored on the key object, instead of in a map indexed by the key object.
(Of course this may leak memory if the map you want to store is not long-term/global.)

### Abstract model for [GCable]{t=} data

I think itʼs instructive to pin down a model of garbage collectable data in Haskell/PureScript, where we can talk about references separately from pure data structures.

This is enough to model the fragment of JavaScript values I said should be covered by the pickling function I sketched.
(Well, you could easily add `undefined`{.javascript}.)

```haskell
-- The managed heap for the runtime data.
-- I believe this is what rustaceans
-- call an arena?
newtype Heap s metadata shape = Heap
  (MVector s (RuntimeData shape, metadata))

data RuntimeData shape
  -- Runtime data is given by a pure shape
  -- (which needs to be `Traversable`!)
  -- which contains runtime references
  -- in a known way
  = RuntimeData (shape RuntimeRef)
  -- It can also be an external “ghost”
  -- reference that we have a function
  -- to destruct (or dereference, if
  -- it is shared data)
  | ExternalGhost Ptr (IO ())

-- A managed reference we control,
-- thus it is an opaque pointer
-- into the opaque memory heap
data RuntimeRef
  = ManagedOpaque Int
  deriving (Eq, Ord)
  -- ^ the user is allowed `Eq`
  --   but not `Ord`

-- An example shape for mutable data
-- in the spirit of JSON (the only
-- reason it is not JSON is that JSON
-- is immutable, being a serialization
-- format, strictly speaking)

-- A JSON value, either a plain value
-- or a reference to a mutable value
data JSONValue ref
  = Null
  | Number Scientific
  | String Text
  | ByRef ref
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- What data lies behind a mutable value?
data JSONShape ref
  = Array [JSONValue ref]
  | Object [(Text, JSONValue ref)]
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- If we take the immediate fixpoint,
-- without mutable references in the
-- loop, we get plain immutable JSON data,
-- except that it is lazy, so it is
-- potentially infinite
newtype JSON = JSON (JSONValue JSON)

data Idx
  = ArrayIdx Int
  | ObjectIdx Text

-- A machine for creating a graph in
-- the mutable JSON structure
data Machine ref
  = SetKey ref Text ref (Machine ref)
  | Push ref ref (Machine ref)
  | Get ref Idx (ref -> Machine ref)
  | NewArray (ref -> Machine ref)
  | NewObject (ref -> Machine ref)
```

Now we can talk about the concepts from above.

```haskell
equal :: Eq ref =>
  JSONValue ref ->
  JSONValue ref ->
  Boolean
equal = (==)

-- Since Haskell is lazy,
-- JSON is a greatest fixpoint,
-- so, with some care, I believe
-- you could even reify recursive
-- data into the JSON type
-- (but `Eq` would not terminate)
snapshot ::
  (ref -> m (JSONShape ref)) ->
  JSONValue ref -> m JSON

deepEq ::
  (ref -> m (JSONShape ref)) ->
  JSONValue ref ->
  JSONValue ref ->
  m Boolean
deepEq read x y = do
  m <- snapshot read x
  n <- snapshot read y
  -- compare as JSON
  pure (m == n)

equivalent :: Ord ref =>
  (ref -> m (JSONShape ref)) ->
  JSONValue ref ->
  JSONValue ref ->
  m Boolean
equivalent read = comparing Map.empty
  where
  comparing seen =
```

However, we donʼt even really need this arena/managed heap.
We can use the Haskell runtime itself!

I havenʼt worked out the details ([stable names](https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-StableName.html)?), but we should be able to reify the graph of a cyclic `JSON`{.haskell} value too.

:::Warning
The main difference is that, in Haskell, the infinite `JSON`{.haskell} could be truly infinite (like, procedurally generated) – it does not need to be backed by a finite amount of data like it would be in JavaScript.
:::
