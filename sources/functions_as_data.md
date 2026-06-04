---
title: Sticking Functions Where They Donʼt Belong
subtitle: "Extensible Defunctionalization with Typeclasses.\n\nOr, Functions in Pure Data in Haskell."
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026/06/04
---

What does pure data mean? and how the heck do we stick functions in there?

For me, _pure data_ means a type that does or can implement:

- `Show`{.haskell}, to serialize the whole internal structure.
- [`NFData`{.haskell}](https://hackage-content.haskell.org/package/deepseq-1.5.2.0/docs/Control-DeepSeq.html#t:NFData), to make sure the structure is fully evaluated (forces all the lazy thunks).

  <details class="Bonus">
  <summary>Aside about NFData for functions</summary>
  There were a [few](https://github.com/haskell/deepseq/issues/16) [debates](https://github.com/haskell/deepseq/issues/111) around whether the shallow [WHNF]{t=} instance for functions should be allowed to stay, as a cheat to work around otherwise awkward code.
  Well this blog post provides a potential way around that!
  Like, semantically you would want `NFData`{.haskell} to normalize the closure, all of the data that stuck around to help the function implementation do its job.
  But part of the job of closures is to hide that data – you have no idea what types it has!
  That data probably cannot be normalized, unless everything can be normalized, and … you may not like what it drags in.
  </details>

- `Eq`{.haskell}, to compare values.
- `Ord`{.haskell}, to order values – not necessarily in any semantic way.
  Semantics usually prefer partial orders, anyway.
  Just think of this as “the typeclass that `Set`{.haskell} and `Map`{.haskell} use to keep track of keys”.
  Things that implement `Eq`{.haskell} and _cannot_ implement `Ord`{.haskell} are really rare: existentials, or things implemented by the runtime (like pointer equality / referential identity – see `Eq (IORef t)`{.haskell} for an example).
- Maybe `Read`{.haskell}, although we will see later why that does not figure into our story much.

A type that implements these interfaces lawfully has no surprises hidden inside.
It is fully legible, plain, flat data.
An [ADT]{t=}, a bytestring, an arbitrarily large natural number,— its representation shouldnʼt matter.
Just data.

Unfortunately this also means that it isnʼt _fun_.
You can add extensibility to your data, but extensible _behavior_ is more difficult.
You can teach all the code that consumes it how to handle these new shapes of data, maybe provide them with a registry to look up behaviors ­– if they exist at runtime.
But you canʼt embed functions in data.
Not theoretically, not practically.^[Okay, some runtimes can [serialize functions as bytecode or syntax](pickling.html#functions-builtins-bootstrapping), but that is not an option in Haskell, and is probably best to avoid.]

Or can you?

### Origin Story: Toy WASM Backend

I have been experimenting with making a silly little functional programming language that would compile to [WASM]{t=}, using [GC]{t=} types.
(WASM GC provides structs and arrays with garbage collected references, integrated with the host runtime for references across the boundary: `externref`{.wasm}.
Not that I wouldnʼt want to make my own GC sometimeee, but it is worth trying WASM GC out especially for prototyping.)

I really _really_ want types in the language to have great freedom to choose how they are encoded, in types and in code.
For example, the surface language provides basic syntax for arrays, text, and [ADT]{t=}s, but we should be able to encode enums as `i8`{.wasm} or `i31`{.wasm}^[`i31`{.wasm} shaves off a bit from `i32`{.wasm} so it is colocatable with GC pointers] or something. And maybe we want to have bitarrays encoded intelligently as bytearrays `(array i8)`{.wasm}.

I donʼt want to maintain a registry of the custom types I add.
I donʼt want to infect codegen with knowledge of specialized types and compilation.
The basic codegen is so simple, it should stay that way.
Keep the essence pure.

But I also donʼt want to give up on my expressions being pure data, an AST that I can inspect and compare and feed to property tests or something.

What am I to do?

### The Existential Pattern

Letʼs smuggle functions into otherwise pure data, with a form of defunctionalization!

The magic of typeclasses is that they provide global, _coherent_ dictionaries, keyed to types.
Because they are global and coherent, they donʼt contain data themselves:
all that you need to know is what type they have.
Theoretically.

And the best part?
Typeclass dictionaries contains functions, for free.

`NFData`{.haskell} isnʼt a problem for typeclass dictionaries: there are no unexpected thunks or bottoms waiting in global typeclass dictionaries.
`Eq`{.haskell} and even `Ord`{.haskell} are already provided on [`TypeRep`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Typeable.html#t:TypeRep), the runtime representation of `Typeable`{.haskell} types (which is automatically derived).
And `Show`{.haskell} isnʼt a problem (but `Read`{.haskell} will be).

First we make a typeclass synonym that bundles the nice things we want.

```haskell
-- Nice properties for existentially hidden data
type Existentiable hidden = (Typeable hidden, NFData hidden, Ord hidden, Show hidden)
```

Next we make a typeclass that contains the functions we want to smuggle through.
How about this one for injecting arbitrary foreign semantics into code.

```haskell
class Existentiable sem => Semantics sem where
  semTyp :: sem -> STyp
  semCode :: sem -> WASM
  semEval :: sem -> [Expr] -> Map Name Expr -> Maybe Expr
  semExprs :: sem -> [Expr]
  semEffects :: sem -> Effects
```

The semantics type can be any data type that captures a behavior we want to see.
Then once we have that data, we can project out the behaviors we care about: the type it has, some way to generate code, maybe simulate its evaluation, and an accounting of the effects it has, for static analysis purposes.

These are basically hooks into our compiler, places where we can extensibly inject arbitrary new behaviors.

Then we make an existential type that wraps any such data with its corresponding typeclass dictionary, including `Typeable`{.haskell}, `Eq`{.haskell}, and so on, up to the actual `Semantics`{.haskell} we want.

```haskell
data ForeignSemantics = forall sem. Semantics sem => ForeignSemantics sem
```

Note that all of the typeclass methods are projective (they receive a `sem`{.haskell} and do not produce it), so we can implement `Semantics`{.haskell} for this abstract `ForeignSemantics`{.haskell} existential type too:

```haskell
instance Semantics ForeignSemantics where
  semTyp (ForeignSemantics sem) = semTyp sem
  semCode (ForeignSemantics sem) = semCode sem
  semEval (ForeignSemantics sem) = semEval sem
  semExprs (ForeignSemantics sem) = semExprs sem
  semEffects (ForeignSemantics sem) = semEffects sem
```

To do this, we have to implement the `Existentiable`{.haskell} constituents.

`Show`{.haskell} is straightforward, just unwrap it and apply `show`{.haskell}.
`NFData`{.haskell} also just forwards to the wrapped `NFData`{.haskell} – the dictionary, although it contains functions, does not need to be normalized for the reasons I discussed earlier: it is just a global typeclass dictionary.

`Eq`{.haskell} and `Ord`{.haskell} require comparing the runtime type reflection that is bundled in `Typeable sem`{.haskell} between the two values it gets.
And of course `Typeable ForeignSemantics`{.haskell} comes for free – magic fairy dust courtesy of [GHC]{t=}.

```haskell
instance Show ForeignSemantics where
  show (ForeignSemantics spec) = show spec
instance NFData ForeignSemantics where
  rnf (ForeignSemantics spec) = rnf spec

instance Eq ForeignSemantics where
  ForeignSemantics spec0 == ForeignSemantics spec2
    | Just spec1 <- Typeable.cast spec0 = spec1 == spec2
    | otherwise = False
instance Ord ForeignSemantics where
  ForeignSemantics spec0 `compare` ForeignSemantics spec2
    | Just spec1 <- Typeable.cast spec0 = spec1 `compare` spec2
    | otherwise = Typeable.typeOf spec0 `compare` Typeable.typeOf spec2
```

Now we get to use `ForeignSemantics`{.haskell} in the main AST for expressions, or wherever else it is useful, without spoiling deriving for `Show`{.haskell} or `NFData`{.haskell} or `Eq`{.haskell} or `Ord`{.haskell}.

```haskell
-- The AST for expressions
data Expr
  -- Literals
  = EF64 !Double
  | EF32 !Float
  | EU64 !Word64
  | EU32 !Word32
  | ES64 !Int64
  | ES32 !Int32
  | ETxt !Text

  -- A let expression: multiple names to bind in order (not recursive let)
  | ELet [(Expr, Bind)] Expr
  -- A variable reference
  | EVar STyp !Name
  -- An n-ary lambda expression
  | EFun STyp [Bind] Expr
  -- Function application (n-ary)
  | EApp Expr [Expr]

  .
  .
  .

  -- Foreign code
  | EForeign ForeignSemantics
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData)
```

Finally we can provide some helpful pattern synonyms.
Since we bundled `Typeable`{.haskell} with this existential type, we can compare it against a specific type and see if it was that type, using [`Typeable.cast`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Typeable.html#v:cast).
(This requires a view pattern, which I am not really fond of, but is incredibly convenient in this situation.)
The pattern synonym now works for destructing concrete types.

For convenience, we might as well repeat it for the AST type too.
Call it `ESem`{.haskell}, since every `Expr`{.haskell} constructor starts with `E`{.haskell}.

```haskell
pattern Sem :: Semantics sem => Semantics sem => sem -> ForeignSemantics
pattern Sem sem <- ForeignSemantics (Typeable.cast -> Just (sem :: sem)) where
  Sem sem = ForeignSemantics sem

pattern ESem :: Semantics sem => Semantics sem => sem -> Expr
pattern ESem sem = EForeign (Sem sem)
```

This is nice so that the library itself can provide instances and it still gets to match on them concretely when it needs to.
For instance, `STyp`{.haskell} is another existential for synthetic types.

Now we can create instances and they just magically work.
A minimal data type for a foreign call consists of its type, side effects that optimizations may need to know about, and the name of the WASM function or import to call.

```haskell
data ForeignCall = ForeignCall STyp Effects Name
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Semantics ForeignCall where
  semTyp (ForeignCall t _ _) = t
  semCode (ForeignCall _ _ name) = tell $ SEXP "call" [ wasmID name ]
  semEval _ _ _ = Nothing
  semExprs _ = []
  semEffects (ForeignCall _ ef _) = ef
```

Et voilà, it is automatically hooked into the compiler and we can summon any function up with the magic expression `ESem (ForeignCall (itsType) mempty "functionToCall")`{.haskell}.

:::Note
It is really important to note that this is one of the few times where we really do need typeclass-constrained existentials.
Normally you would say that `data Showable = forall t. Show t => Showable t`{.haskell} is equivalent to just `String`{.haskell} (or whatever), because converting it to a `String`{.haskell} via `show`{.haskell} is all you can do with the existential `t`{.haskell} value that you received.
But because we want functions as data in there, we need to keep the typeclass dictionary abstract, and hang onto the underlying data type.

Plus, you are doing real work here.
You have to decide what is the essence of the data behind the behavior you want for each instance of the typeclass.
You are designing the closure of the functions you want and calling it the `sem`{.haskell} type.
That is a big chunk of the work of defunctionalization anyway.

But you donʼt have to worry about the extensibility of the defunctionality;
about being a library versus being a consumer of that library.
That is all taken care of by Haskell.
Itʼs just another typeclass, and typeclasses are amazing.

You just donʼt get a pretty enumeration of the instances at the end, that you would need for implementing deserialization like `Read`{.haskell}.
You are restricted to methods that are compatible with this extensible open-world view of what data it is seeing.
But this is perfect for hooks into a compiler or such.
:::

### Conclusion

Defunctionalization is usually either a conscious, excruciatingly explicit design choice, or something that is performed externally on a closed codebase, where all the functions are known.

This form of defunctionalization is extensible: you can write a Haskell library that contains some instances, and users can bring their own instances to the table, without any coordination.

You get to add extensible behaviors to any part of your code or data, without compromising much on what it means to be data, or tracking a registry of extensions, or trying to predict what your users (or you in the future) might want to do with it.

It requires some effort to determine what data is essential to the functions you want for an instance, but hopefully it is not too onerous and provides some clarity.
It is better than the alternatives.

:::Bonus
Unfortunately, there is no way to automatically serialize and deserialize these existential dictionaries.
I was hoping that maybe [GHC.Compact](https://hackage-content.haskell.org/package/ghc-compact-0.1.0.0/docs/GHC-Compact.html) could do it, but it does not have a carve-out for typeclass dictionaries.
It does say:

> The serialized data can only be deserialized by the exact binary that created it, but it can be stored indefinitely before deserialization.

Which – in theory – could even include functions and closures if you really want to play with fire.
Same binary, same functions.
Less dangerously, the binary could embed a canonical `TypeRep -> Dict`{.haskell} table (well, dictionary assembler) to look instances back up when deserializing.
But the GHC runtime does not support that.
:::
